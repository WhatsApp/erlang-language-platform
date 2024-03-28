/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Cursor;
use std::io::Read;
use std::io::Write;
use std::mem;
use std::os::unix::net::UnixListener;
use std::os::unix::net::UnixStream;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
use std::time::Duration;

use anyhow::anyhow;
use anyhow::bail;
use anyhow::Context;
use anyhow::Result;
use byteorder::BigEndian;
use byteorder::ReadBytesExt;
use byteorder::WriteBytesExt;
use common_test::ConversionError;
use common_test::GroupDef;
pub use common_test::TestDef;
use crossbeam_channel::bounded;
use crossbeam_channel::Receiver;
use crossbeam_channel::Sender;
use eetf::pattern;
use eetf::Term;
use elp_syntax::SmolStr;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use jod_thread::JoinHandle;
use lazy_static::lazy_static;
use parking_lot::Mutex;
use regex::Regex;
use stdx::JodChild;
use tempfile::Builder;
use tempfile::TempPath;
use text_size::TextRange;

pub mod common_test;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum DiagnosticLocation {
    Normal(TextRange),
    Included {
        directive_location: TextRange, // Location of include directive in the file compiled
        error_location: TextRange,     // Location of the error in the included file
    },
}

/// This struct ensures proper shutdown sequence
///
/// Struct fields are dropped in definition order, meaning we:
///   * first stop sending requests
///   * then stop accepting responses
///   * then shutdown the child process
///   * then delete the socket
///   * finally delete the executable
#[derive(Debug)]
struct SharedState {
    _writer_for_drop: JoinHandle,
    _reader_for_drop: JoinHandle,
    _child_for_drop: JodChild,
    _socket_for_drop: TempPath,
    _escript_for_drop: TempPath,
}

#[derive(Clone, Debug)]
pub struct Connection {
    sender: Sender<Request>,
    _for_drop: Arc<SharedState>,
}

/// Until such time as we have a fully re-entrant erlang service,
/// we must guard access to it during tests to prevent race
/// conditions leading to flaky tests. T182801661
pub static ERLANG_SERVICE_GLOBAL_LOCK: Mutex<()> = Mutex::new(());

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum CompileOption {
    Includes(Vec<PathBuf>),
    Macros(Vec<eetf::Term>),
    ParseTransforms(Vec<eetf::Term>),
    ElpMetadata(eetf::Term),
    ForceWarnMissingSpecAll,
}

impl From<CompileOption> for eetf::Term {
    fn from(val: CompileOption) -> Self {
        match val {
            CompileOption::Includes(includes) => {
                let paths = eetf::List::from(
                    includes
                        .into_iter()
                        .map(|path| path_into_list(path).into())
                        .collect::<Vec<_>>(),
                );
                eetf::Tuple::from(vec![eetf::Atom::from("includes").into(), paths.into()]).into()
            }
            CompileOption::Macros(macros) => {
                let macros = eetf::List::from(macros);
                eetf::Tuple::from(vec![eetf::Atom::from("macros").into(), macros.into()]).into()
            }
            CompileOption::ParseTransforms(transforms) => {
                let transforms = eetf::List::from(transforms);
                let parse_transforms = eetf::Atom::from("parse_transforms");
                eetf::Tuple::from(vec![parse_transforms.into(), transforms.into()]).into()
            }
            CompileOption::ElpMetadata(elp_metadata) => {
                let label = eetf::Atom::from("elp_metadata");
                eetf::Tuple::from(vec![label.into(), elp_metadata]).into()
            }
            CompileOption::ForceWarnMissingSpecAll => {
                eetf::Atom::from("warn_missing_spec_all").into()
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Format {
    OffsetEtf,
    Text,
}

#[derive(Debug, Clone)]
pub struct ParseRequest {
    pub options: Vec<CompileOption>,
    pub path: PathBuf,
    pub format: Format,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DocOrigin {
    /// Get docs by running edoc on the Erlang source file.
    /// Preferable, since it doesn't require an out-of-band build step
    /// to get up-to-date BEAM files with embedded docs or `.chunk` files
    /// beside them.
    Edoc,
    /// Get docs via the EEP-48 standardised method from BEAM files.
    /// Required for some dependencies which don't use standard edocs in
    /// comments, but who store docs in the relevant chunk of beam files
    /// which we can assume are up-to-date, e.g. OTP.
    Eep48,
}

#[derive(Debug, Clone)]
pub struct DocRequest {
    pub doc_origin: DocOrigin,
    /// No matter the doc origin, **we give the path for the source file
    /// here**. If the origin is EEP-48, erlang_service will resolve it to
    /// the appropriate BEAM file itself.
    pub src_path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct CTInfoRequest {
    pub module: eetf::Atom,
    pub src_path: PathBuf,
    pub compile_options: Vec<CompileOption>,
    pub should_request_groups: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError {
    pub path: PathBuf,
    pub location: Option<DiagnosticLocation>,
    pub msg: String,
    pub code: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DocDiagnostic {
    pub code: String,
    pub severity: String,
    pub line: u32,
    pub message: String,
}

type RawNameArity = (String, u32);
type RawMarkdown = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CTInfoResult {
    pub all: Term,
    pub groups: Term,
}

impl CTInfoResult {
    pub fn all(&self) -> Result<FxHashSet<TestDef>, ConversionError> {
        common_test::all(&self.all)
    }
    pub fn groups(&self) -> Result<FxHashMap<SmolStr, GroupDef>, ConversionError> {
        common_test::groups(&self.groups)
    }
}

#[derive(Debug)]
pub struct DocResult {
    pub module_doc: RawMarkdown,
    pub function_docs: FxHashMap<RawNameArity, RawMarkdown>,
    pub diagnostics: Vec<DocDiagnostic>,
}

#[derive(Debug)]
struct Response(Cursor<Vec<u8>>);
type RequestTag = &'static [u8; 3];
type Request = (RequestTag, Vec<u8>, Option<Sender<Response>>);

impl Response {
    fn decode_segments(self, mut f: impl FnMut(&[u8; 3], Vec<u8>) -> Result<()>) -> Result<()> {
        let mut cursor = self.0;
        if cursor.read_u8().expect("no status for message") != 0 {
            let mut buf = String::new();
            cursor
                .read_to_string(&mut buf)
                .expect("malformed error message");
            bail!("parse service failed with: {}", buf);
        };

        let mut tag = [0; 3];
        while let Ok(()) = cursor.read_exact(&mut tag) {
            let size = cursor.read_u32::<BigEndian>().expect("malformed segment");
            let mut buf = vec![0; size as usize];
            cursor.read_exact(&mut buf).expect("malformed segment");
            f(&tag, buf)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseResult {
    pub ast: Arc<Vec<u8>>,
    pub stub: Arc<Vec<u8>>,
    pub files: Arc<Vec<u8>>,
    pub errors: Vec<ParseError>,
    pub warnings: Vec<ParseError>,
}

impl ParseResult {
    pub fn error(error: ParseError) -> Self {
        Self {
            ast: Arc::default(),
            stub: Arc::default(),
            files: Arc::default(),
            errors: vec![error],
            warnings: Vec::default(),
        }
    }

    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }
}

impl Connection {
    pub fn start() -> Result<Connection> {
        let escript_src =
            include_bytes!(concat!(env!("OUT_DIR"), "/erlang_service/erlang_service"));
        let mut escript = Builder::new().prefix("erlang_service").tempfile()?;
        escript.write_all(escript_src)?;

        let socket = Builder::new()
            .prefix("erlang_service_socket")
            .make(|path| UnixListener::bind(path))?;

        let mut cmd = Command::new("escript");
        cmd.arg(escript.path());
        cmd.arg(socket.path());

        let proc = cmd.spawn()?;
        let escript = escript.into_temp_path();

        let (sender, writer, reader) = unix_transport(socket.as_file())
            .context("establishing unix socket erlang_service transport")?;

        Ok(Connection {
            sender,
            _for_drop: Arc::new(SharedState {
                _escript_for_drop: escript,
                _socket_for_drop: socket.into_temp_path(),
                _child_for_drop: JodChild(proc),
                _writer_for_drop: writer,
                _reader_for_drop: reader,
            }),
        })
    }

    fn request_reply(&self, tag: RequestTag, request: Vec<u8>, unwind: impl Fn()) -> Response {
        let (sender, receiver) = bounded::<Response>(0);
        self.sender.send((tag, request, Some(sender))).unwrap();
        loop {
            match receiver.recv_timeout(Duration::from_millis(100)) {
                Ok(result) => return result,
                Err(_) => unwind(),
            }
        }
    }

    pub fn request_parse(&self, request: ParseRequest, unwind: impl Fn()) -> ParseResult {
        let path = request.path.clone();
        let tag = request.tag();
        let request = request.encode();
        let reply = self.request_reply(tag, request, unwind);

        let mut ast = vec![];
        let mut stub = vec![];
        let mut files = vec![];
        let mut warnings = vec![];
        let mut errors = vec![];

        reply
            .decode_segments(|segment, data| {
                match segment {
                    b"AST" => ast = data,
                    b"STU" => stub = data,
                    b"FIL" => files = data,
                    b"WAR" => warnings = data,
                    b"ERR" => errors = data,
                    _ => log::error!("Unrecognised segment: {:?}", segment),
                };
                Ok(())
            })
            .and_then(|()| {
                Ok(ParseResult {
                    ast: Arc::new(ast),
                    stub: Arc::new(stub),
                    files: Arc::new(files),
                    warnings: decode_errors(&warnings).context("decoding warnings")?,
                    errors: decode_errors(&errors).context("decoding errors")?,
                })
            })
            .unwrap_or_else(|error| {
                ParseResult::error(ParseError {
                    path,
                    location: None,
                    msg: format!("Could not parse, error: {}", error),
                    code: "L0002".to_string(),
                })
            })
    }

    pub fn request_doc(&self, request: DocRequest, unwind: impl Fn()) -> Result<DocResult, String> {
        lazy_static! {
            static ref FUNCTION_DOC_REGEX: Regex =
                Regex::new(r"^(?P<name>\S+) (?P<arity>\d+) (?P<doc>(?s).*)$").unwrap();
            static ref DOC_DIAGNOSTIC_REGEX: Regex =
                Regex::new(r"^(?P<code>\S+) (?P<severity>\S+) (?P<line>\d+) (?P<message>(.|\n)*)$")
                    .unwrap();
        }

        let tag = request.tag();
        let encoded = request.clone().encode();
        let reply = self.request_reply(tag, encoded, unwind);

        let mut function_docs = FxHashMap::default();
        let mut module_doc = String::new();
        let mut diagnostics = Vec::new();

        reply
            .decode_segments(|segment, data| {
                match segment {
                    b"MDC" => module_doc = String::from_utf8(data)?,
                    b"FDC" => {
                        let text = String::from_utf8(data).unwrap_or_else(|err| {
                            log::warn!("Failed UTF-8 conversion in FUNCTION_DOC: {err}");
                            // Fall back to lossy latin1 loading of files.
                            // This should only affect files from yaws, and
                            // possibly OTP that are latin1 encoded.
                            let contents = err.into_bytes();
                            contents.into_iter().map(|byte| byte as char).collect()
                        });
                        if let Some(caps) = FUNCTION_DOC_REGEX.captures(&text) {
                            let name = caps.name("name").unwrap().as_str().to_string();
                            let arity = caps.name("arity").unwrap().as_str().parse::<u32>()?;
                            let doc = caps.name("doc").unwrap().as_str().to_string();
                            function_docs.insert((name, arity), doc);
                        } else {
                            log::error!("Could not capture in FUNCTION_DOC: {text}");
                        }
                    }
                    b"EDC" => {
                        let text = String::from_utf8(data).unwrap_or_else(|err| {
                            log::warn!("Failed UTF-8 conversion in EDOC_DIAGNOSTIC: {err}");
                            // Fall back to lossy latin1 loading of files.
                            // This should only affect files from yaws, and
                            // possibly OTP that are latin1 encoded.
                            let contents = err.into_bytes();
                            contents.into_iter().map(|byte| byte as char).collect()
                        });
                        if let Some(caps) = DOC_DIAGNOSTIC_REGEX.captures(&text) {
                            let code = caps.name("code").unwrap().as_str().to_string();
                            let severity = caps.name("severity").unwrap().as_str().to_string();
                            let line = caps.name("line").unwrap().as_str().parse::<u32>()?;
                            let message = caps.name("message").unwrap().as_str().to_string();
                            diagnostics.push(DocDiagnostic {
                                code,
                                severity,
                                line,
                                message,
                            });
                        } else {
                            log::error!("Could not capture in EDOC_DIAGNOSTICS: {text}");
                        }
                    }
                    _ => log::error!("Unrecognised segment: {:?}", segment),
                };
                Ok(())
            })
            .map(|()| DocResult {
                module_doc,
                function_docs,
                diagnostics,
            })
            .map_err(|error| {
                log::error!(
                    "Erlang service crashed for: {:?}, error: {:?}",
                    request,
                    error
                );
                format!(
                    "Erlang service crash when trying to load docs: {:?}",
                    request
                )
            })
    }

    pub fn ct_info(
        &self,
        request: CTInfoRequest,
        unwind: impl Fn(),
    ) -> Result<CTInfoResult, String> {
        let module = request.module.clone();
        let tag = request.tag();
        let request = request.encode();
        let reply = self.request_reply(tag, request, unwind);

        let mut all = vec![];
        let mut groups = vec![];

        reply
            .decode_segments(|segment, data| {
                match segment {
                    b"ALL" => all = data,
                    b"GRP" => groups = data,
                    _ => log::error!("Unrecognised segment: {:?}", segment),
                };
                Ok(())
            })
            .and_then(|()| {
                Ok(CTInfoResult {
                    all: Term::decode(&*all)?,
                    groups: Term::decode(&*groups)?,
                })
            })
            .map_err(|error| {
                log::info!("Failed to fetch CT Info for {}: {:?}", module.name, error);
                format!("Failed to fetch CT Info for {:?}", module.name)
            })
    }

    pub fn add_code_path(&self, paths: Vec<PathBuf>) {
        let mut buf = Vec::new();
        for path in paths {
            let path = path.to_str().expect("non UTF8 path encountered");
            buf.write_u32::<BigEndian>(path.len() as u32)
                .expect("buf write failed");
            buf.write_all(path.as_bytes()).expect("buf write failed");
        }
        let request = (b"ACP", buf, None);
        self.sender.send(request).unwrap();
    }
}

fn unix_transport(listner: &UnixListener) -> Result<(Sender<Request>, JoinHandle, JoinHandle)> {
    let (socket, _addr) = listner.accept()?;

    let inflight = Arc::new(Mutex::new(FxHashMap::default()));
    let (writer_sender, writer_receiver) = bounded::<Request>(0);

    let writer = jod_thread::spawn({
        let inflight = inflight.clone();
        let socket = socket.try_clone()?;
        move || match writer_run(writer_receiver, socket, inflight) {
            Ok(()) => {}
            Err(err) => log::error!("writer failed with {}", err),
        }
    });

    let reader = jod_thread::spawn({
        move || match reader_run(socket, inflight) {
            Ok(()) => {}
            Err(err) => log::error!("reader failed with {}", err),
        }
    });

    Ok((writer_sender, writer, reader))
}

fn reader_run(
    mut socket: UnixStream,
    inflight: Arc<Mutex<FxHashMap<u64, Sender<Response>>>>,
) -> Result<()> {
    loop {
        let size = socket.read_u32::<BigEndian>()? as usize;
        let mut buf = vec![0; size];
        socket.read_exact(&mut buf)?;

        let mut cursor = Cursor::new(buf);
        let id = cursor.read_u64::<BigEndian>()?;
        let sender = inflight.lock().remove(&id).expect("unexpected response id");
        if let Err(err) = sender.send(Response(cursor)) {
            log::info!("Got response {}, but request was canceled: {}", id, err);
        };
    }
}

fn writer_run(
    receiver: Receiver<Request>,
    mut socket: UnixStream,
    inflight: Arc<Mutex<FxHashMap<u64, Sender<Response>>>>,
) -> Result<()> {
    let mut counter = 0;
    receiver
        .into_iter()
        .try_for_each(|(tag, request, sender)| {
            counter += 1;
            if let Some(sender) = sender {
                inflight.lock().insert(counter, sender);
            }
            let len = tag.len() + mem::size_of::<u64>() + request.len();
            socket.write_u32::<BigEndian>(len.try_into().expect("message too large"))?;
            socket.write_all(tag)?;
            socket.write_u64::<BigEndian>(counter)?;
            socket.write_all(&request)?;
            socket.flush()
        })?;
    Ok(socket.shutdown(std::net::Shutdown::Write)?)
}

fn decode_errors(buf: &[u8]) -> Result<Vec<ParseError>> {
    if buf.is_empty() {
        return Ok(vec![]);
    }

    // the upstream pattern::Str does not match Term::ByteList which is what we get
    #[derive(Debug, Clone)]
    pub struct Str;
    impl<'a> pattern::Pattern<'a> for Str {
        type Output = String;
        fn try_match(&self, input: &'a eetf::Term) -> pattern::Result<'a, Self::Output> {
            match input {
                eetf::Term::ByteList(bytes) => std::str::from_utf8(&bytes.bytes)
                    .map(ToString::to_string)
                    .map_err(|_| self.unmatched(input)),
                _ => Err(self.unmatched(input)),
            }
        }
    }

    eetf::Term::decode(buf)?
        .as_match(pattern::VarList((
            Str,
            pattern::Or((
                (pattern::U32, pattern::U32), // Normal location
                pattern::FixList(((pattern::U32, pattern::U32), (pattern::U32, pattern::U32))), // Location in include file
                "none",
            )),
            Str, // message
            Str, // code
        )))
        .map_err(|err| anyhow!("Failed to decode errors: {:?}", err))
        .map(|res| {
            res.into_iter()
                .map(|(path, position, msg, code)| ParseError {
                    path: path.into(),
                    location: match position {
                        pattern::Union3::A((a, b)) => Some(DiagnosticLocation::Normal(
                            TextRange::new(a.into(), b.into()),
                        )),
                        pattern::Union3::B(((a, b), (c, d))) => {
                            Some(DiagnosticLocation::Included {
                                directive_location: TextRange::new(a.into(), b.into()),
                                error_location: TextRange::new(c.into(), d.into()),
                            })
                        }
                        pattern::Union3::C(_) => None,
                    },
                    msg,
                    code,
                })
                .collect()
        })
}

impl ParseRequest {
    fn tag(&self) -> RequestTag {
        match self.format {
            Format::OffsetEtf { .. } => b"COM",
            Format::Text => b"TXT",
        }
    }

    fn encode(self) -> Vec<u8> {
        let mut buf = Vec::new();
        let options = self
            .options
            .into_iter()
            .map(|option| option.into())
            .collect::<Vec<eetf::Term>>();
        let list = eetf::List::from(vec![
            path_into_list(self.path).into(),
            eetf::List::from(options).into(),
        ]);
        eetf::Term::from(list).encode(&mut buf).unwrap();
        buf
    }
}

impl DocRequest {
    fn tag(&self) -> RequestTag {
        match self.doc_origin {
            DocOrigin::Edoc => b"DCE",
            DocOrigin::Eep48 => b"DCP",
        }
    }

    fn encode(self) -> Vec<u8> {
        let mut buf = Vec::new();
        let list = eetf::List::from(vec![path_into_list(self.src_path).into()]);
        eetf::Term::from(list).encode(&mut buf).unwrap();
        buf
    }
}

impl CTInfoRequest {
    fn tag(&self) -> RequestTag {
        b"CTI"
    }

    fn encode(self) -> Vec<u8> {
        let mut buf = Vec::new();
        let compile_options = self
            .compile_options
            .into_iter()
            .map(|option| option.into())
            .collect::<Vec<eetf::Term>>();
        let should_request_groups = match self.should_request_groups {
            true => eetf::Atom::from("true").into(),
            false => eetf::Atom::from("false").into(),
        };
        let list = eetf::List::from(vec![
            self.module.into(),
            path_into_list(self.src_path).into(),
            eetf::List::from(compile_options).into(),
            should_request_groups,
        ]);
        eetf::Term::from(list).encode(&mut buf).unwrap();
        buf
    }
}

#[cfg(unix)]
fn path_into_list(path: PathBuf) -> eetf::ByteList {
    use std::os::unix::prelude::OsStringExt;
    eetf::ByteList {
        bytes: path.into_os_string().into_vec(),
    }
}

#[cfg(test)]
mod tests {
    use std::str;

    use expect_test::expect_file;
    use expect_test::ExpectFile;
    use lazy_static::lazy_static;

    use super::*;

    #[test]
    fn regular_module() {
        expect_module(
            "fixtures/regular.erl".into(),
            expect_file!["../fixtures/regular.expected"],
            vec![],
        );
    }

    #[test]
    fn unsupported_extension() {
        expect_module_filtered_error(
            "fixtures/unsupported_extension.yrl".into(),
            expect_file!["../fixtures/unsupported_extension.expected"],
            vec![],
        );
    }

    #[test]
    fn structured_comment() {
        expect_module(
            "fixtures/structured_comment.erl".into(),
            expect_file!["../fixtures/structured_comment.expected"],
            vec![],
        );
    }

    #[test]
    fn errors() {
        expect_module(
            "fixtures/error.erl".into(),
            expect_file!["../fixtures/error.expected"],
            vec![],
        );

        expect_module(
            "fixtures/misplaced_comment_error.erl".into(),
            expect_file!["../fixtures/misplaced_comment_error.expected"],
            vec![],
        );
    }

    #[test]
    fn warnings() {
        expect_module(
            "fixtures/error_attr.erl".into(),
            expect_file!["../fixtures/error_attr.expected"],
            vec![],
        );
    }

    #[test]
    fn unused_record() {
        expect_module(
            "fixtures/unused_record.erl".into(),
            expect_file!["../fixtures/unused_record.expected"],
            vec![],
        );
    }

    #[test]
    fn unused_record_in_header() {
        expect_module(
            "fixtures/unused_record_in_header.hrl".into(),
            expect_file!["../fixtures/unused_record_in_header.expected"],
            vec![],
        );
    }

    #[test]
    fn override_warn_missing_spec_all() {
        expect_module(
            "fixtures/override_warn_missing_spec_all.erl".into(),
            expect_file!["../fixtures/override_warn_missing_spec_all.expected"],
            vec![CompileOption::ForceWarnMissingSpecAll],
        );
    }

    #[test]
    fn edoc_scan_errors() {
        expect_docs(
            "fixtures/edoc_scan_errors.erl".into(),
            expect_file!["../fixtures/edoc_scan_errors.expected"],
        );
    }

    #[test]
    fn edoc_warnings() {
        expect_docs(
            "fixtures/edoc_warnings.erl".into(),
            expect_file!["../fixtures/edoc_warnings.expected"],
        );
    }

    #[test]
    fn edoc_errors() {
        expect_docs(
            "fixtures/edoc_errors.erl".into(),
            expect_file!["../fixtures/edoc_errors.expected"],
        );
    }

    #[test]
    fn ct_info() {
        expect_ct_info(
            "ct_info_SUITE".into(),
            "fixtures/ct_info_SUITE.erl".into(),
            expect_file!["../fixtures/ct_info_SUITE.expected"],
        );
    }

    #[test]
    fn ct_info_exception() {
        expect_ct_info(
            "ct_info_exception_SUITE".into(),
            "fixtures/ct_info_exception_SUITE.erl".into(),
            expect_file!["../fixtures/ct_info_exception_SUITE.expected"],
        );
    }

    #[test]
    fn ct_info_incomplete() {
        expect_ct_info(
            "ct_info_incomplete_SUITE".into(),
            "fixtures/ct_info_incomplete_SUITE.erl".into(),
            expect_file!["../fixtures/ct_info_incomplete_SUITE.expected"],
        );
    }

    #[test]
    fn ct_info_dynamic_all() {
        expect_ct_info(
            "ct_info_dynamic_SUITE".into(),
            "fixtures/ct_info_dynamic_SUITE.erl".into(),
            expect_file!["../fixtures/ct_info_dynamic_SUITE.expected"],
        );
    }

    #[test]
    fn ct_info_infinite_loop() {
        expect_ct_info(
            "ct_info_infinite_loop_SUITE".into(),
            "fixtures/ct_info_infinite_loop_SUITE.erl".into(),
            expect_file!["../fixtures/ct_info_infinite_loop_SUITE.expected"],
        );
    }

    fn expect_module(path: PathBuf, expected: ExpectFile, options: Vec<CompileOption>) {
        lazy_static! {
            static ref CONN: Connection = Connection::start().unwrap();
        }
        let request = ParseRequest {
            options,
            path,
            format: Format::Text,
        };
        let response = CONN.request_parse(request, || ());
        let ast = str::from_utf8(&response.ast).unwrap();
        let stub = str::from_utf8(&response.stub).unwrap();
        let actual = format!(
            "AST\n{}\n\nSTUB\n{}\n\nWARNINGS\n{:#?}\n\nERRORS\n{:#?}\n",
            ast, stub, &response.warnings, &response.errors
        );
        expected.assert_eq(&actual);
    }

    fn expect_module_filtered_error(
        path: PathBuf,
        expected: ExpectFile,
        options: Vec<CompileOption>,
    ) {
        lazy_static! {
            static ref CONN: Connection = Connection::start().unwrap();
        }
        let request = ParseRequest {
            options,
            path,
            format: Format::Text,
        };
        let response = CONN.request_parse(request, || ());
        let ast = str::from_utf8(&response.ast).unwrap();
        let stub = str::from_utf8(&response.stub).unwrap();
        let errors = &response
            .errors
            .iter()
            .map(|e| {
                let mut e = e.clone();
                e.msg = "*removed*".to_string();
                e
            })
            .collect::<Vec<ParseError>>();
        let actual = format!(
            "AST\n{}\n\nSTUB\n{}\n\nWARNINGS\n{:#?}\n\nERRORS\n{:#?}\n",
            ast, stub, &response.warnings, &errors
        );
        expected.assert_eq(&actual);
    }

    fn expect_docs(path: PathBuf, expected: ExpectFile) {
        lazy_static! {
            static ref CONN: Connection = Connection::start().unwrap();
        }
        let request = DocRequest {
            doc_origin: DocOrigin::Edoc,
            src_path: path,
        };
        let response = CONN.request_doc(request, || ()).unwrap();
        let actual = format!(
            "MODULE_DOC\n{}\n\nFUNCTION_DOCS\n{:#?}\n\nEDOC_DIAGNOSTICS\n{:#?}\n\n\n",
            &response.module_doc, &response.function_docs, &response.diagnostics
        );
        expected.assert_eq(&actual);
    }

    fn expect_ct_info(module: String, path: PathBuf, expected: ExpectFile) {
        lazy_static! {
            static ref CONN: Connection = Connection::start().unwrap();
        }
        let request = CTInfoRequest {
            module: eetf::Atom::from(module),
            src_path: path,
            compile_options: vec![],
            should_request_groups: true,
        };
        let actual = match CONN.ct_info(request, || ()) {
            Ok(response) => {
                format!(
                    "CT_INFO_ALL\n{}\n\nCT_INFO_GROUPS\n{}",
                    &response.all, &response.groups
                )
            }
            Err(err) => err.to_string(),
        };
        expected.assert_eq(&actual);
    }
}
