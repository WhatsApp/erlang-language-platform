/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::BufReader;
use std::io::BufWriter;
use std::io::Cursor;
use std::io::Read;
use std::io::Write;
use std::mem;
use std::path::PathBuf;
use std::process::Child;
use std::process::ChildStdin;
use std::process::ChildStdout;
use std::process::Command;
use std::process::Stdio;
use std::sync::Arc;
use std::sync::RwLock;
use std::time::Duration;

use anyhow::anyhow;
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

lazy_static! {
    pub static ref ESCRIPT: RwLock<String> = RwLock::new("escript".to_string());
}

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
///   * finally delete the executable
#[derive(Debug)]
struct SharedState {
    _writer_for_drop: JoinHandle,
    _reader_for_drop: JoinHandle,
    _child_for_drop: JodChild,
    _file_for_drop: TempPath,
}

#[derive(Clone, Debug)]
pub struct Connection {
    sender: Sender<Request>,
    _for_drop: Arc<SharedState>,
}

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
    pub override_options: Vec<CompileOption>,
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

type Request = (Tag, Vec<u8>, RequestType);

enum RequestType {
    Sender(Sender<Response>),
    CallbackReply(u64), // Original Id
    NoReply,
}

type Tag = &'static [u8; 3];

#[derive(Debug)]
// Second element is the inflight id for responding to callback requests.
struct Response {
    payload: Cursor<Vec<u8>>,
    id: u64,
    status: ResponseStatus,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ResponseStatus {
    Ok,       // 0
    Error,    // 1
    CallBack, // 2
}

impl From<u8> for ResponseStatus {
    fn from(value: u8) -> Self {
        match value {
            0 => ResponseStatus::Ok,
            1 => ResponseStatus::Error,
            2 => ResponseStatus::CallBack,
            n => {
                log::warn!("Got an unexpected ResponseStatus: {n}");
                ResponseStatus::Error
            }
        }
    }
}

impl Response {
    fn decode_segments(self, mut f: impl FnMut(&[u8; 3], Vec<u8>) -> Result<()>) -> Result<()> {
        let mut cursor = self.payload;
        if self.status == ResponseStatus::Error {
            let mut buf = String::new();
            cursor
                .read_to_string(&mut buf)
                .expect("malformed error message");
            Err(anyhow!("erlang service failed with: {}", buf))
        } else {
            let mut tag = [0; 3];
            while let Ok(()) = cursor.read_exact(&mut tag) {
                let size = cursor.read_u32::<BigEndian>().expect("malformed segment");
                let mut buf = vec![0; size as usize];
                cursor.read_exact(&mut buf).expect("malformed segment");
                f(&tag, buf)?
            }
            Ok(())
        }
    }

    fn is_callback(&self) -> bool {
        self.status == ResponseStatus::CallBack
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseResult {
    pub ast: Arc<Vec<u8>>,
    pub stub: Arc<Vec<u8>>,
    pub errors: Vec<ParseError>,
    pub warnings: Vec<ParseError>,
}

impl ParseResult {
    pub fn error(error: ParseError) -> Self {
        Self {
            ast: Arc::default(),
            stub: Arc::default(),
            errors: vec![error],
            warnings: Vec::default(),
        }
    }

    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IncludeType {
    Normal,
    Lib,
    Doc, // Search in same dir as file only
}
impl From<&str> for IncludeType {
    fn from(value: &str) -> Self {
        match value {
            "L" => IncludeType::Lib,
            "D" => IncludeType::Doc,
            _ => IncludeType::Normal,
        }
    }
}
pub type ResolveInclude<'a> = dyn Fn(IncludeType, &str) -> Option<String>;

impl Connection {
    pub fn start() -> Result<Connection> {
        let escript_src =
            include_bytes!(concat!(env!("OUT_DIR"), "/erlang_service/erlang_service"));
        let mut escript = Builder::new().prefix("erlang_service").tempfile()?;
        escript.write_all(escript_src)?;

        let escript_bin = ESCRIPT.read().unwrap();

        let mut cmd = Command::new(&*escript_bin);
        cmd.arg(escript.path());

        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());

        let mut proc = cmd.spawn()?;
        let escript = escript.into_temp_path();

        let (sender, writer, reader) = stdio_transport(&mut proc);

        Ok(Connection {
            sender,
            _for_drop: Arc::new(SharedState {
                _file_for_drop: escript,
                _child_for_drop: JodChild(proc),
                _writer_for_drop: writer,
                _reader_for_drop: reader,
            }),
        })
    }

    fn request_reply(&self, tag: Tag, request: Vec<u8>, unwind: impl Fn()) -> Response {
        self.request_reply_handle(tag, request, unwind, |callback| {
            panic!(
                "Got unexpected callback processing {:?}: {:?}",
                tag, callback
            );
        })
    }

    fn request_reply_handle(
        &self,
        tag: Tag,
        request: Vec<u8>,
        unwind: impl Fn(),
        handle_callback: impl Fn(Response) -> Result<Vec<u8>>,
    ) -> Response {
        let (sender, receiver) = bounded::<Response>(0);
        self.sender
            .send((tag, request, RequestType::Sender(sender)))
            .expect("failed sending request to parse server");

        // Every 100ms check if the db was cancelled by calling back to db.
        // If the query was cancelled the `unwind` callback will panic and
        // we'll abandon the computation. This is important to avoid blocking
        // the main loop for too long - cancellation is sent when updating the
        // db on user edits
        loop {
            match receiver.recv_timeout(Duration::from_millis(100)) {
                Ok(result) => {
                    let id = result.id;
                    if result.is_callback() {
                        match handle_callback(result) {
                            Ok(buf) => {
                                // Sender None means it won't update the inflight store
                                let request = (b"REP", buf, RequestType::CallbackReply(id));
                                self.sender.send(request).unwrap();
                            }
                            Err(err) => {
                                log::warn!("handle_callback gave err: {}, for {}", err, id);
                                // We must always reply, else the erlang side hangs
                                self.sender
                                    .send((b"REP", Vec::new(), RequestType::CallbackReply(id)))
                                    .unwrap();
                            }
                        }
                    } else {
                        return result;
                    }
                }
                Err(_) => unwind(),
            }
        }
    }

    pub fn request_parse(
        &self,
        request: ParseRequest,
        unwind: impl Fn(),
        resolve_include: &impl Fn(IncludeType, &str) -> Option<String>,
    ) -> ParseResult {
        let path = request.path.clone();
        let tag = request.tag();
        let request = request.encode();
        let reply = self.request_reply_handle(tag, request, unwind, |request| {
            self.handle_request_parse_callback(request, resolve_include)
        });

        let mut ast = vec![];
        let mut stub = vec![];
        let mut warnings = vec![];
        let mut errors = vec![];

        reply
            .decode_segments(|tag, data| {
                match tag {
                    b"AST" => ast = data,
                    b"STU" => stub = data,
                    b"WAR" => warnings = data,
                    b"ERR" => errors = data,
                    _ => log::error!("unrecognised segment {:?}", tag),
                };
                Ok(())
            })
            .and_then(|()| {
                Ok(ParseResult {
                    ast: Arc::new(ast),
                    stub: Arc::new(stub),
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

    fn handle_request_parse_callback(
        &self,
        reply: Response,
        resolve_include: impl Fn(IncludeType, &str) -> Option<String>,
    ) -> Result<Vec<u8>> {
        let payload_buf = &reply.payload.get_ref();
        let opens = &payload_buf[(reply.payload.position() as usize)..];
        let mut buf = Vec::new();
        if !opens.is_empty() {
            let string_val = decode_utf8_or_latin1(opens.to_vec());
            if let Some(resolved) = Self::do_resolve(&string_val, resolve_include) {
                buf.write_u32::<BigEndian>(resolved.len() as u32)
                    .expect("buf write failed");
                buf.write_all(resolved.as_bytes())
                    .expect("buf write failed");
            }
        } else {
            log::warn!("handle_request_parse_callback: did not get OPN segment");
        }
        Ok(buf)
    }

    pub fn do_resolve(
        string_val: &str,
        resolve_include: impl Fn(IncludeType, &str) -> Option<String>,
    ) -> Option<String> {
        let (normal_or_lib, path) = string_val.split_once(":")?;
        resolve_include(normal_or_lib.into(), path)
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
                        let text = decode_utf8_or_latin1(data);
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
                        let text = decode_utf8_or_latin1(data);
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
        let request = (b"ACP", buf, RequestType::NoReply);
        self.sender.send(request).unwrap();
    }
}

fn stdio_transport(proc: &mut Child) -> (Sender<Request>, JoinHandle, JoinHandle) {
    let instream = BufWriter::new(proc.stdin.take().unwrap());
    let mut outstream = BufReader::new(proc.stdout.take().unwrap());

    let inflight = Arc::new(Mutex::new(FxHashMap::default()));

    let (writer_sender, writer_receiver) = bounded::<Request>(0);
    let writer = jod_thread::spawn({
        let inflight = inflight.clone();
        move || match writer_run(writer_receiver, instream, inflight) {
            Result::Ok(()) => {}
            Err(err) => log::error!("writer failed with {}", err),
        }
    });

    let reader = jod_thread::spawn({
        move || match reader_run(&mut outstream, inflight) {
            Result::Ok(()) => {}
            Err(err) => {
                let mut buf = vec![0; 512];
                let _ = outstream.read(&mut buf);
                let remaining = String::from_utf8_lossy(&buf);
                log::error!(
                    "reader failed with {:?}\nremaining data:\n\n{}",
                    err,
                    remaining
                );
            }
        }
    });

    (writer_sender, writer, reader)
}

fn reader_run(
    outstream: &mut BufReader<ChildStdout>,
    inflight: Arc<Mutex<FxHashMap<u64, Sender<Response>>>>,
) -> Result<()> {
    loop {
        let size = outstream.read_u32::<BigEndian>()? as usize;
        let mut buf = vec![0; size];
        outstream.read_exact(&mut buf)?;
        if buf == b"EXT" {
            log::info!("Reader terminating");
            return Ok(());
        }
        let mut cursor = Cursor::new(buf);
        let id = cursor.read_u64::<BigEndian>()?;
        let status = cursor.read_u8()?.into();
        if status == ResponseStatus::CallBack {
            let inflight = inflight.lock();
            // Do not remove entry from inflight db
            let sender = inflight.get(&id).expect("unexpected response id");
            if let Err(err) = sender.send(Response {
                payload: cursor,
                id,
                status,
            }) {
                log::info!("Got response {}, but request was canceled: {}", id, err);
            };
        } else {
            // Finished, remove entry from inflight
            let sender = inflight.lock().remove(&id).expect("unexpected response id");
            if let Err(err) = sender.send(Response {
                payload: cursor,
                id,
                status,
            }) {
                log::info!("Got response {}, but request was canceled: {}", id, err);
            };
        };
    }
}

fn writer_run(
    receiver: Receiver<Request>,
    mut instream: BufWriter<ChildStdin>,
    inflight: Arc<Mutex<FxHashMap<u64, Sender<Response>>>>,
) -> Result<()> {
    let mut counter = 0;
    receiver
        .into_iter()
        .try_for_each(|(tag, request, sender)| {
            let id = match sender {
                RequestType::Sender(sender) => {
                    counter += 1;
                    inflight.lock().insert(counter, sender);
                    counter
                }
                RequestType::CallbackReply(original_id) => original_id,
                RequestType::NoReply => counter,
            };
            let len = tag.len() + mem::size_of::<u64>() + request.len();
            instream.write_u32::<BigEndian>(len.try_into().expect("message too large"))?;
            instream.write_all(tag)?;
            instream.write_u64::<BigEndian>(id)?;
            instream.write_all(&request)?;
            instream.flush()
        })?;
    Ok(())
}

fn decode_utf8_or_latin1(data: Vec<u8>) -> String {
    String::from_utf8(data).unwrap_or_else(|err| {
        log::warn!("Failed UTF-8 conversion in FUNCTION_DOC: {err}");
        // Fall back to lossy latin1 loading of files.
        // This should only affect files from yaws, and
        // possibly OTP that are latin1 encoded.
        let contents = err.into_bytes();
        contents.into_iter().map(|byte| byte as char).collect()
    })
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
    fn tag(&self) -> Tag {
        match self.format {
            Format::OffsetEtf { .. } => b"COM",
            Format::Text => b"TXT",
        }
    }

    fn encode(self) -> Vec<u8> {
        let options = self
            .options
            .into_iter()
            .map(|option| option.into())
            .collect::<Vec<eetf::Term>>();
        let override_options = self
            .override_options
            .into_iter()
            .map(|option| option.into())
            .collect::<Vec<eetf::Term>>();
        let list = eetf::List::from(vec![
            path_into_list(self.path).into(),
            eetf::List::from(options).into(),
            eetf::List::from(override_options).into(),
        ]);
        let mut buf = Vec::new();
        eetf::Term::from(list).encode(&mut buf).unwrap();
        buf
    }
}

impl DocRequest {
    fn tag(&self) -> Tag {
        match self.doc_origin {
            DocOrigin::Edoc => b"DCE",
            DocOrigin::Eep48 => b"DCP",
        }
    }

    fn encode(self) -> Vec<u8> {
        let list = eetf::List::from(vec![path_into_list(self.src_path).into()]);
        let mut buf = Vec::new();
        eetf::Term::from(list).encode(&mut buf).unwrap();
        buf
    }
}

impl CTInfoRequest {
    fn tag(&self) -> Tag {
        b"CTI"
    }

    fn encode(self) -> Vec<u8> {
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
        let mut buf = Vec::new();
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

    fn expect_module(path: PathBuf, expected: ExpectFile, override_options: Vec<CompileOption>) {
        lazy_static! {
            static ref CONN: Connection = Connection::start().unwrap();
        }
        let request = ParseRequest {
            options: vec![],
            override_options,
            path,
            format: Format::Text,
        };
        let response = CONN.request_parse(request, || (), &|_, _| None);
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
        override_options: Vec<CompileOption>,
    ) {
        lazy_static! {
            static ref CONN: Connection = Connection::start().unwrap();
        }
        let request = ParseRequest {
            options: vec![],
            override_options,
            path,
            format: Format::Text,
        };
        let response = CONN.request_parse(request, || (), &|_, _| None);
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
