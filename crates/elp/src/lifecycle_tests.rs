/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! How the language server and `elp daemon` see files being opened, edited,
//! created and deleted, driven through their real entry points.

use std::fs;
use std::thread;
use std::time::Duration;
use std::time::Instant;

use elp_eqwalizer::Mode;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::ModuleName;
use elp_ide::elp_ide_db::elp_base_db::RootQueryDb;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;
use elp_log::Logger;
use elp_project_model::DiscoverConfig;
use elp_project_model::buck::BuckQueryConfig;
use lsp_server::Connection;
use lsp_server::ErrorCode;
use lsp_server::Message;
use lsp_server::Notification;
use lsp_server::Request;
use lsp_server::Response;
use lsp_server::ResponseError;
use lsp_types::DidChangeWatchedFilesParams;
use lsp_types::DidCloseTextDocumentParams;
use lsp_types::DidOpenTextDocumentParams;
use lsp_types::DocumentSymbolParams;
use lsp_types::DocumentSymbolResponse;
use lsp_types::FileChangeType;
use lsp_types::FileEvent;
use lsp_types::GotoDefinitionParams;
use lsp_types::GotoDefinitionResponse;
use lsp_types::InitializeParams;
use lsp_types::Position;
use lsp_types::TextDocumentIdentifier;
use lsp_types::TextDocumentItem;
use lsp_types::TextDocumentPositionParams;
use lsp_types::notification;
use lsp_types::notification::Notification as _;
use lsp_types::request;
use lsp_types::request::Request as _;
use serde_json::Value;
use tempfile::TempDir;

use crate::ServerSetup;
use crate::build::load;
use crate::build::load::LoadConfig;
use crate::build::types::LoadResult;
use crate::cli::Fake;
use crate::convert;
use crate::watchman;
use crate::watchman::WatchmanFile;

/// Project loading and file events reach the server asynchronously, so
/// checks are retried for up to this long.
const TIMEOUT: Duration = Duration::from_secs(60);

/// A project in a temporary directory.
struct TestProject {
    _dir: TempDir,
    root: AbsPathBuf,
}

impl TestProject {
    fn new(files: &[(&str, &str)]) -> Self {
        let dir = TempDir::new().unwrap();
        let root = AbsPathBuf::assert_utf8(dunce::canonicalize(dir.path()).unwrap());
        let project = TestProject { _dir: dir, root };
        for (name, text) in files {
            project.write(name, text);
        }
        project
    }

    fn path(&self, name: &str) -> AbsPathBuf {
        self.root.join(name)
    }

    fn write(&self, name: &str, text: &str) {
        let path = self.path(name);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, text).unwrap();
    }

    fn remove(&self, name: &str) {
        fs::remove_file(self.path(name)).unwrap();
    }
}

/// A language server running its real main loop, connected in memory.
struct TestServer {
    client: Connection,
    server: Option<thread::JoinHandle<()>>,
    next_id: i32,
}

impl TestServer {
    fn start(project: &TestProject) -> Self {
        crate::ensure_rayon_pool();
        let (connection, client) = Connection::memory();
        let server = thread::spawn(|| {
            ServerSetup::new(connection, Logger::default())
                .to_server()
                .unwrap()
                .main_loop()
                .unwrap()
        });
        let mut server = TestServer {
            client,
            server: Some(server),
            next_id: 0,
        };
        #[expect(deprecated, reason = "the server reads `root_uri`")]
        let params = InitializeParams {
            root_uri: Some(convert::uri_from_abs_path(&project.root)),
            ..Default::default()
        };
        server.request::<request::Initialize>(params).unwrap();
        server.notify::<notification::Initialized>(lsp_types::InitializedParams {});
        server
    }

    fn notify<N: notification::Notification>(&self, params: N::Params) {
        let notif = Notification::new(N::METHOD.to_string(), params);
        self.client.sender.send(notif.into()).unwrap();
    }

    fn request<R: request::Request>(
        &mut self,
        params: R::Params,
    ) -> Result<R::Result, ResponseError> {
        self.next_id += 1;
        let id = self.next_id.into();
        let req = Request::new(id, R::METHOD.to_string(), params);
        self.client.sender.send(req.into()).unwrap();
        let deadline = Instant::now() + TIMEOUT;
        loop {
            let message = self
                .client
                .receiver
                .recv_timeout(deadline.saturating_duration_since(Instant::now()))
                .unwrap_or_else(|err| panic!("no answer to {}: {err}", R::METHOD));
            match message {
                Message::Response(resp) if resp.id == self.next_id.into() => {
                    return resp
                        .response_result
                        .map(|result| serde_json::from_value(result).unwrap());
                }
                Message::Request(req) => {
                    // The server fetches its configuration and registers
                    // capabilities; a client with no settings will do.
                    let result = if req.method == request::WorkspaceConfiguration::METHOD {
                        serde_json::json!([null])
                    } else {
                        Value::Null
                    };
                    let resp = Response::new_ok(req.id, result);
                    self.client.sender.send(resp.into()).unwrap();
                }
                _ => {}
            }
        }
    }

    /// `None` while the server is still loading.
    fn answer<T>(result: Result<T, ResponseError>) -> Option<T> {
        match result {
            Ok(answer) => Some(answer),
            Err(err) if err.code == ErrorCode::ContentModified as i32 => None,
            Err(err) => panic!("request failed: {err:?}"),
        }
    }

    fn wait_for<T>(&mut self, what: &str, mut check: impl FnMut(&mut Self) -> Option<T>) -> T {
        let deadline = Instant::now() + TIMEOUT;
        loop {
            if let Some(result) = check(self) {
                return result;
            }
            assert!(Instant::now() < deadline, "timed out waiting for {what}");
            thread::sleep(Duration::from_millis(50));
        }
    }

    fn open(&self, path: &AbsPath, text: &str) {
        self.notify::<notification::DidOpenTextDocument>(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: convert::uri_from_abs_path(path),
                language_id: "erlang".to_string(),
                version: 0,
                text: text.to_string(),
            },
        });
    }

    fn close(&self, path: &AbsPath) {
        self.notify::<notification::DidCloseTextDocument>(DidCloseTextDocumentParams {
            text_document: TextDocumentIdentifier::new(convert::uri_from_abs_path(path)),
        });
    }

    fn files_changed(&self, changes: &[(&AbsPath, FileChangeType)]) {
        let changes = changes
            .iter()
            .map(|(path, typ)| FileEvent::new(convert::uri_from_abs_path(path), *typ))
            .collect();
        self.notify::<notification::DidChangeWatchedFiles>(DidChangeWatchedFilesParams { changes });
    }

    /// The names of the symbols in `path`, or `None` while loading.
    fn symbols(&mut self, path: &AbsPath) -> Option<Vec<String>> {
        let params = DocumentSymbolParams {
            text_document: TextDocumentIdentifier::new(convert::uri_from_abs_path(path)),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        match Self::answer(self.request::<request::DocumentSymbolRequest>(params))? {
            Some(DocumentSymbolResponse::Nested(symbols)) => {
                Some(symbols.into_iter().map(|symbol| symbol.name).collect())
            }
            other => panic!("unexpected document symbols: {other:?}"),
        }
    }

    /// The files the definition at `position` in `path` is in, or `None`
    /// while loading.
    fn definitions(&mut self, path: &AbsPath, position: Position) -> Option<Vec<AbsPathBuf>> {
        let params = GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams::new(
                TextDocumentIdentifier::new(convert::uri_from_abs_path(path)),
                position,
            ),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        let locations = match Self::answer(self.request::<request::GotoDefinition>(params))? {
            None => vec![],
            Some(GotoDefinitionResponse::Scalar(location)) => vec![location],
            Some(GotoDefinitionResponse::Array(locations)) => locations,
            other => panic!("unexpected definition: {other:?}"),
        };
        Some(
            locations
                .iter()
                .map(|location| convert::abs_path(&location.uri).unwrap())
                .collect(),
        )
    }
}

impl Drop for TestServer {
    fn drop(&mut self) {
        let _ = self
            .client
            .sender
            .send(Notification::new(notification::Exit::METHOD.to_string(), ()).into());
        let Some(server) = self.server.take() else {
            return;
        };
        // The test has already failed, and joining a server that is stuck
        // would hang it.
        if thread::panicking() {
            return;
        }
        let deadline = Instant::now() + TIMEOUT;
        while !server.is_finished() {
            assert!(Instant::now() < deadline, "the server did not exit");
            thread::sleep(Duration::from_millis(10));
        }
        if let Err(panic) = server.join() {
            std::panic::resume_unwind(panic);
        }
    }
}

const CALLER: &str = "-module(a).\nf() -> b:g().\n";
/// The `g` in `b:g()` in [`CALLER`].
const CALL: Position = Position {
    line: 1,
    character: 9,
};
const CALLEE: &str = "-module(b).\n-export([g/0]).\ng() -> ok.\n";

#[test]
fn an_open_document_wins_over_disk() {
    let project = TestProject::new(&[("src/a.erl", "-module(a).\ndisk() -> ok.\n")]);
    let mut server = TestServer::start(&project);
    let a = project.path("src/a.erl");

    server.open(&a, "-module(a).\nopened() -> ok.\n");

    let symbols = server.wait_for("loading", |server| server.symbols(&a));
    assert_eq_expected!(vec!["opened/0"], symbols);
}

#[test]
fn a_disk_change_does_not_replace_an_open_document() {
    let project = TestProject::new(&[
        ("src/a.erl", "-module(a).\ndisk() -> ok.\n"),
        ("src/b.erl", "-module(b).\ndisk() -> ok.\n"),
    ]);
    let mut server = TestServer::start(&project);
    let (a, b) = (project.path("src/a.erl"), project.path("src/b.erl"));
    server.open(&a, "-module(a).\nopened() -> ok.\n");
    server.wait_for("loading", |server| server.symbols(&a));

    project.write("src/a.erl", "-module(a).\nchanged() -> ok.\n");
    project.write("src/b.erl", "-module(b).\nchanged() -> ok.\n");
    server.files_changed(&[(&a, FileChangeType::CHANGED), (&b, FileChangeType::CHANGED)]);

    // `b` is reloaded after `a` would be, so once `b` has changed, `a` has had
    // its chance to.
    server.wait_for("b to change", |server| {
        server
            .symbols(&b)
            .filter(|symbols| symbols == &["changed/0"])
    });
    assert_eq_expected!(Some(vec!["opened/0".to_string()]), server.symbols(&a));
}

#[test]
fn closing_a_document_falls_back_to_disk() {
    let project = TestProject::new(&[("src/a.erl", "-module(a).\ndisk() -> ok.\n")]);
    let mut server = TestServer::start(&project);
    let a = project.path("src/a.erl");
    server.open(&a, "-module(a).\nopened() -> ok.\n");
    server.wait_for("loading", |server| server.symbols(&a));

    server.close(&a);

    server.wait_for("the disk contents", |server| {
        server.symbols(&a).filter(|symbols| symbols == &["disk/0"])
    });
}

#[test]
fn a_created_module_becomes_visible() {
    let project = TestProject::new(&[("src/a.erl", CALLER)]);
    let mut server = TestServer::start(&project);
    let a = project.path("src/a.erl");
    server.open(&a, CALLER);
    let definitions = server.wait_for("loading", |server| server.definitions(&a, CALL));
    assert_eq_expected!(Vec::<AbsPathBuf>::new(), definitions);

    project.write("src/b.erl", CALLEE);
    let b = project.path("src/b.erl");
    server.files_changed(&[(&b, FileChangeType::CREATED)]);

    server.wait_for("b to be visible", |server| {
        server
            .definitions(&a, CALL)
            .filter(|files| files == std::slice::from_ref(&b))
    });
}

#[test]
fn a_deleted_module_disappears() {
    let project = TestProject::new(&[("src/a.erl", CALLER), ("src/b.erl", CALLEE)]);
    let mut server = TestServer::start(&project);
    let (a, b) = (project.path("src/a.erl"), project.path("src/b.erl"));
    server.open(&a, CALLER);
    server.wait_for("b to be visible", |server| {
        server
            .definitions(&a, CALL)
            .filter(|files| files == std::slice::from_ref(&b))
    });

    project.remove("src/b.erl");
    server.files_changed(&[(&b, FileChangeType::DELETED)]);

    server.wait_for("b to disappear", |server| {
        server
            .definitions(&a, CALL)
            .filter(|files| files.is_empty())
    });
}

/// Loads `project` the way `elp daemon` does.
fn load_for_daemon(project: &TestProject) -> LoadResult {
    let (elp_config, manifest) =
        load::discover_manifest(project.root.as_ref(), &DiscoverConfig::new(false, "test"))
            .unwrap();
    load::load_project_from_manifest(
        &Fake::default(),
        &manifest,
        &elp_config,
        LoadConfig::new(Mode::Shell, BuckQueryConfig::BuildGeneratedCode),
    )
    .unwrap()
}

fn watchman_event(name: &str, exists: bool, new: bool) -> WatchmanFile {
    WatchmanFile {
        name: name.to_string(),
        exists,
        new,
    }
}

fn module_text(loaded: &LoadResult, module: &str) -> Option<String> {
    let db = loaded.analysis_host.raw_database();
    let file_id = db
        .module_index(loaded.project_id)
        .file_for_module(&ModuleName::new(module))?;
    Some(db.file_text(file_id).text(db).to_string())
}

#[test]
fn the_daemon_applies_created_changed_and_deleted_files() {
    let project = TestProject::new(&[("src/a.erl", CALLER)]);
    let mut loaded = load_for_daemon(&project);
    assert_eq_expected!(None, module_text(&loaded, "b"));

    project.write("src/b.erl", CALLEE);
    watchman::apply_changes(
        project.root.as_ref(),
        &[watchman_event("src/b.erl", true, true)],
        &mut loaded,
    );
    assert_eq_expected!(Some(CALLEE.to_string()), module_text(&loaded, "b"));

    project.write("src/b.erl", "-module(b).\n");
    watchman::apply_changes(
        project.root.as_ref(),
        &[watchman_event("src/b.erl", true, false)],
        &mut loaded,
    );
    assert_eq_expected!(Some("-module(b).\n".to_string()), module_text(&loaded, "b"));

    project.remove("src/b.erl");
    watchman::apply_changes(
        project.root.as_ref(),
        &[watchman_event("src/b.erl", false, false)],
        &mut loaded,
    );
    assert_eq_expected!(None, module_text(&loaded, "b"));
}
