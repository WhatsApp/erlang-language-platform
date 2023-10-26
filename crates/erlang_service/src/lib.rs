/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Read;
use std::io::Write;
use std::iter;
use std::path::PathBuf;
use std::process::Child;
use std::process::ChildStdin;
use std::process::ChildStdout;
use std::process::Command;
use std::process::Stdio;
use std::sync::Arc;

use anyhow::anyhow;
use anyhow::Context;
use anyhow::Result;
use crossbeam_channel::bounded;
use crossbeam_channel::Receiver;
use crossbeam_channel::Sender;
use eetf::pattern;
use fxhash::FxHashMap;
use jod_thread::JoinHandle;
use parking_lot::Mutex;
use regex::Regex;
use stdx::JodChild;
use tempfile::Builder;
use tempfile::TempPath;
use text_size::TextRange;

// Location information of a warning/error may come in different flavors:
// * Eqwalizer: byte offset for range.
// * Default parser: line, column for start.
// Note: Using byte offset everywhere would complicate the code,
//       since conversion requires access to source file and its encoding.

/// Start location, as returned by erl parser (see erl_anno:location()).
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct StartLocation {
    pub line: u32,
    pub column: u32,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Location {
    TextRange(TextRange),
    StartLocation(StartLocation),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum DiagnosticLocation {
    Normal(Location),
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

#[derive(Debug, Clone)]
pub enum CompileOption {
    Includes(Vec<PathBuf>),
    Macros(Vec<eetf::Term>),
    ParseTransforms(Vec<eetf::Term>),
    ElpMetadata(eetf::Term),
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
enum Request {
    ParseRequest(ParseRequest, Sender<Result<UndecodedParseResult>>),
    AddCodePath(Vec<PathBuf>),
    DocRequest(DocRequest, Sender<Result<DocResult>>),
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

pub type ParseResult = RawParseResult<ParseError>;
type UndecodedParseResult = RawParseResult<u8>;
type RawNameArity = (String, u32);
type RawMarkdown = String;

pub type DocResult = RawModuleDoc<DocDiagnostic>;

#[derive(Debug)]
pub struct RawModuleDoc<Error> {
    pub module_doc: RawMarkdown,
    pub function_docs: FxHashMap<RawNameArity, RawMarkdown>,
    pub diagnostics: Vec<Error>,
}

#[derive(Debug)]
enum Reply {
    ParseReply(Result<UndecodedParseResult>),
    DocReply(Result<DocResult>),
}

enum ResponseSender {
    ParseResponseSender(Sender<Result<UndecodedParseResult>>),
    DocResponseSender(Sender<Result<DocResult>>),
}

impl ResponseSender {
    fn send_exn(&self, e: anyhow::Error) {
        match self {
            ResponseSender::ParseResponseSender(r) => r.send(Result::Err(e)).unwrap(),
            ResponseSender::DocResponseSender(r) => r.send(Result::Err(e)).unwrap(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RawParseResult<Error> {
    pub ast: Arc<Vec<u8>>,
    pub stub: Arc<Vec<u8>>,
    pub errors: Vec<Error>,
    pub warnings: Vec<Error>,
}

impl UndecodedParseResult {
    pub fn decode(self) -> Result<ParseResult> {
        let errors = decode_errors(&self.errors).with_context(|| "when decoding errors")?;
        let warnings = decode_errors(&self.warnings).with_context(|| "when decoding warnings")?;

        Ok(ParseResult {
            ast: self.ast,
            stub: self.stub,
            errors,
            warnings,
        })
    }
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

impl Connection {
    pub fn start() -> Result<Connection> {
        let escript_src =
            include_bytes!(concat!(env!("OUT_DIR"), "/erlang_service/erlang_service"));
        let mut escript = Builder::new().prefix("erlang_service").tempfile()?;
        escript.write_all(escript_src)?;

        let mut cmd = Command::new("escript");
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

    pub fn request_parse(&self, request_in: ParseRequest) -> ParseResult {
        let (sender, receiver) = bounded::<Result<UndecodedParseResult>>(0);
        let path = request_in.path.clone();
        let request = Request::ParseRequest(request_in.clone(), sender);
        self.sender.send(request).unwrap();
        match receiver.recv().unwrap() {
            Result::Ok(result) => match result.decode() {
                Result::Ok(result) => result,
                Err(error) => {
                    log::error!("Decoding parse result failed: {:?}", error);
                    ParseResult::error(ParseError {
                        path,
                        location: None,
                        msg: format!("Could not parse, error: {}", error),
                        code: "L0001".to_string(),
                    })
                }
            },
            Err(error) => {
                log::error!(
                    "Erlang service crashed for: {:?}, error: {:?}",
                    request_in,
                    error
                );
                ParseResult::error(ParseError {
                    path,
                    location: None,
                    msg: format!("Could not parse, error: {}", error),
                    code: "L0002".to_string(),
                })
            }
        }
    }

    pub fn request_doc(&self, request: DocRequest) -> Result<DocResult, String> {
        let (sender, receiver) = bounded::<Result<DocResult>>(0);
        let request = Request::DocRequest(request, sender);
        self.sender.send(request.clone()).unwrap();
        match receiver.recv().unwrap() {
            Result::Ok(result) => Result::Ok(result),
            Err(error) => {
                log::error!(
                    "Erlang service crashed for: {:?}, error: {:?}",
                    request,
                    error
                );
                Err(format!(
                    "Erlang service crash when trying to load docs: {:?}",
                    request
                ))
            }
        }
    }

    pub fn add_code_path(&self, paths: Vec<PathBuf>) {
        let request = Request::AddCodePath(paths);
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
                    "reader failed with {}\nremaining data:\n\n{}",
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
    inflight: Arc<Mutex<FxHashMap<usize, ResponseSender>>>,
) -> Result<()> {
    let mut line_buf = String::new();
    loop {
        line_buf.clear();
        outstream.read_line(&mut line_buf)?;
        let parts = line_buf.split_ascii_whitespace().collect::<Vec<_>>();
        if parts.is_empty() {
            break;
        }

        let id: usize = parts[1].parse()?;
        let size: usize = parts[2].parse()?;

        let sender = inflight.lock().remove(&id).unwrap();

        match parts[0] {
            "REPLY" => {
                let reply = decode_segments(outstream, &mut line_buf, size)?;
                send_reply(sender, reply)?;
            }
            "EXCEPTION" => {
                let mut buf = vec![0; size];
                outstream.read_exact(&mut buf)?;
                let resp = String::from_utf8(buf).unwrap();
                let error = anyhow!("{}", resp);
                sender.send_exn(error);
            }
            _ => {
                log::error!("Unrecognised message: {}", line_buf);
                break;
            }
        }
    }

    fn decode_segments(
        outstream: &mut BufReader<ChildStdout>,
        line_buf: &mut String,
        num: usize,
    ) -> Result<Reply> {
        let mut ast = Vec::new();
        let mut stub = Vec::new();
        let mut warnings = Vec::new();
        let mut errors = Vec::new();

        let mut function_docs = FxHashMap::default();
        let mut module_doc = Vec::new();
        let mut edoc_diagnostics = Vec::new();

        let mut is_doc = false;

        let function_doc_regex =
            Regex::new(r"^(?P<name>\S+) (?P<arity>\d+) (?P<doc>(?s).*)$").unwrap();

        let doc_diagnostic_regex =
            Regex::new(r"^(?P<code>\S+) (?P<severity>\S+) (?P<line>\d+) (?P<message>.*)$").unwrap();

        for _ in 0..num {
            line_buf.clear();
            outstream.read_line(line_buf)?;
            let parts = line_buf.split_ascii_whitespace().collect::<Vec<_>>();
            if parts.is_empty() {
                break;
            }
            let size: usize = parts[1].parse()?;
            let mut buf = vec![0; size];

            outstream.read_exact(&mut buf)?;

            match parts[0] {
                "AST" => ast = buf,
                "STUB" => stub = buf,
                "WARNINGS" => warnings = buf,
                "ERRORS" => errors = buf,
                "MODULE_DOC" => {
                    is_doc = true;
                    module_doc = buf
                }
                "FUNCTION_DOC" => {
                    is_doc = true;
                    let text = match String::from_utf8(buf) {
                        Ok(text) => text,
                        Err(err) => {
                            log::warn!("Failed UTF-8 conversion in FUNCTION_DOC: {err}");
                            // Fall back to lossy latin1 loading of files.
                            // This should only affect files from yaws, and
                            // possibly OTP that are latin1 encoded.
                            let contents = err.into_bytes();
                            contents.into_iter().map(|byte| byte as char).collect()
                        }
                    };
                    if let Some(caps) = function_doc_regex.captures(&text) {
                        let name = caps.name("name").unwrap().as_str().to_string();
                        let arity = caps.name("arity").unwrap().as_str().parse::<u32>()?;
                        let doc = caps.name("doc").unwrap().as_str().to_string();
                        function_docs.insert((name, arity), doc);
                    } else {
                        log::error!("Could not capture in FUNCTION_DOC: {text}");
                    }
                }
                "EDOC_DIAGNOSTIC" => {
                    let text = match String::from_utf8(buf) {
                        Ok(text) => text,
                        Err(err) => {
                            log::warn!("Failed UTF-8 conversion in EDOC_DIAGNOSTIC: {err}");
                            // Fall back to lossy latin1 loading of files.
                            // This should only affect files from yaws, and
                            // possibly OTP that are latin1 encoded.
                            let contents = err.into_bytes();
                            contents.into_iter().map(|byte| byte as char).collect()
                        }
                    };
                    if let Some(caps) = doc_diagnostic_regex.captures(&text) {
                        let code = caps.name("code").unwrap().as_str().to_string();
                        let severity = caps.name("severity").unwrap().as_str().to_string();
                        let line = caps.name("line").unwrap().as_str().parse::<u32>()?;
                        let message = caps.name("message").unwrap().as_str().to_string();
                        edoc_diagnostics.push(DocDiagnostic {
                            code,
                            severity,
                            line,
                            message,
                        });
                    } else {
                        log::error!("Could not capture in EDOC_DIAGNOSTICS: {text}");
                    }
                }
                _ => {
                    log::error!("Unrecognised segment: {}", line_buf);
                    break;
                }
            }
        }
        if is_doc {
            let module_doc_str = String::from_utf8(module_doc).unwrap();
            Ok(Reply::DocReply(Ok(RawModuleDoc {
                module_doc: module_doc_str,
                function_docs,
                diagnostics: edoc_diagnostics,
            })))
        } else {
            Ok(Reply::ParseReply(Ok(UndecodedParseResult {
                ast: Arc::new(ast),
                stub: Arc::new(stub),
                warnings,
                errors,
            })))
        }
    }

    Ok(())
}

fn send_reply(sender: ResponseSender, reply: Reply) -> Result<()> {
    match (sender, reply) {
        (ResponseSender::ParseResponseSender(s), Reply::ParseReply(r)) => {
            s.send(r).unwrap();
            Result::Ok(())
        }
        (ResponseSender::DocResponseSender(s), Reply::DocReply(r)) => {
            s.send(r).unwrap();
            Result::Ok(())
        }
        (ResponseSender::ParseResponseSender(_), Reply::DocReply(_)) => Result::Err(anyhow!(
            "erlang_service response mismatch: Got a doc reply when expecting a parse reply"
        )),
        (ResponseSender::DocResponseSender(_), Reply::ParseReply(_)) => Result::Err(anyhow!(
            "erlang_service response mismatch: Got a parse reply when expecting a doc reply"
        )),
    }
}

fn writer_run(
    receiver: Receiver<Request>,
    mut instream: BufWriter<ChildStdin>,
    inflight: Arc<Mutex<FxHashMap<usize, ResponseSender>>>,
) -> Result<()> {
    let mut counter = 0;
    receiver.into_iter().try_for_each(|request| match request {
        Request::ParseRequest(request, sender) => {
            counter += 1;
            inflight
                .lock()
                .insert(counter, ResponseSender::ParseResponseSender(sender));
            let tag = request.tag();
            let bytes = request.encode(counter);
            writeln!(instream, "{} {}", tag, bytes.len())?;
            instream.write_all(&bytes)?;
            instream.flush()
        }
        Request::AddCodePath(paths) => {
            writeln!(instream, "ADD_PATHS {}", paths.len())?;
            for path in paths {
                writeln!(instream, "{}", path.display())?;
            }
            Result::Ok(())
        }
        Request::DocRequest(request, sender) => {
            counter += 1;
            inflight
                .lock()
                .insert(counter, ResponseSender::DocResponseSender(sender));
            let tag = request.tag();
            let bytes = request.encode(counter);
            writeln!(instream, "{} {}", tag, bytes.len())?;
            instream.write_all(&bytes)?;
            instream.flush()
        }
    })?;
    instream.write_all(b"EXIT\n")?;
    Ok(())
}

fn decode_errors(buf: &[u8]) -> Result<Vec<ParseError>> {
    if buf.is_empty() {
        return Ok(vec![]);
    }

    eetf::Term::decode(buf)?
        .as_match(pattern::VarList((
            pattern::Str(pattern::Unicode),
            pattern::Or((
                (pattern::U32, pattern::U32), // Normal location
                pattern::FixList(((pattern::U32, pattern::U32), (pattern::U32, pattern::U32))), // Location in include file
                "none",
            )),
            pattern::Str(pattern::Unicode), // message
            pattern::Str(pattern::Unicode), // code
        )))
        .map_err(|err| anyhow!("Failed to decode errors: {:?}", err))
        .map(|res| {
            res.into_iter()
                .map(|(path, position, msg, code)| ParseError {
                    path: path.into(),
                    location: match position {
                        pattern::Union3::A((a, b)) => {
                            // Temporary for  T148094436
                            let _pctx = stdx::panic_context::enter("\ndecode_errors1".to_string());
                            Some(DiagnosticLocation::Normal(Location::TextRange(
                                TextRange::new(a.into(), b.into()),
                            )))
                        }
                        pattern::Union3::B(((a, b), (c, d))) => {
                            // Temporary for  T148094436
                            let _pctx = stdx::panic_context::enter("\ndecode_errors2".to_string());
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
    fn tag(&self) -> &'static str {
        match self.format {
            Format::OffsetEtf { .. } => "COMPILE",
            Format::Text => "TEXT",
        }
    }

    fn encode(self, id: usize) -> Vec<u8> {
        let location = eetf::Atom::from("offset").into();
        let location_tuple =
            eetf::Tuple::from(vec![eetf::Atom::from("location").into(), location]).into();
        let options = self
            .options
            .into_iter()
            .map(|option| option.into())
            .chain(iter::once(location_tuple))
            .collect::<Vec<eetf::Term>>();
        let tuple = eetf::Tuple::from(vec![
            eetf::BigInteger::from(id).into(),
            path_into_list(self.path).into(),
            eetf::List::from(options).into(),
        ]);
        let mut buf = Vec::new();
        eetf::Term::from(tuple).encode(&mut buf).unwrap();
        buf
    }
}

impl DocRequest {
    fn tag(&self) -> &'static str {
        match self.doc_origin {
            DocOrigin::Edoc => "DOC_EDOC",
            DocOrigin::Eep48 => "DOC_EEP48",
        }
    }

    fn encode(self, id: usize) -> Vec<u8> {
        let tuple = eetf::Tuple::from(vec![
            eetf::BigInteger::from(id).into(),
            path_into_list(self.src_path).into(),
        ]);
        let mut buf = Vec::new();
        eetf::Term::from(tuple).encode(&mut buf).unwrap();
        buf
    }
}

#[cfg(unix)]
fn path_into_list(path: PathBuf) -> eetf::List {
    use std::os::unix::prelude::OsStringExt;
    path.into_os_string()
        .into_vec()
        .into_iter()
        .map(|byte| eetf::FixInteger::from(byte).into())
        .collect::<Vec<eetf::Term>>()
        .into()
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

    fn expect_module(path: PathBuf, expected: ExpectFile, options: Vec<CompileOption>) {
        lazy_static! {
            static ref CONN: Connection = Connection::start().unwrap();
        }
        let request = ParseRequest {
            options,
            path,
            format: Format::Text,
        };
        let response = CONN.request_parse(request);
        let ast = str::from_utf8(&response.ast).unwrap();
        let stub = str::from_utf8(&response.stub).unwrap();
        let actual = format!(
            "AST\n{}\n\nSTUB\n{}\n\nWARNINGS\n{:#?}\n\nERRORS\n{:#?}\n",
            ast, stub, &response.warnings, &response.errors
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
        let response = CONN.request_doc(request).unwrap();
        let actual = format!(
            "MODULE_DOC\n{}\n\nFUNCTION_DOCS\n{:#?}\n\nEDOC_DIAGNOSTICS\n{:#?}\n\n\n",
            &response.module_doc, &response.function_docs, &response.diagnostics
        );
        expected.assert_eq(&actual);
    }
}
