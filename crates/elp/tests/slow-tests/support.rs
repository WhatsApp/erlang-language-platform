/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::Cell;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::sync::Once;
use std::thread;
use std::time::Duration;

use crossbeam_channel::after;
use crossbeam_channel::select;
use crossbeam_channel::Receiver;
use elp::config::Config;
use elp::server::setup;
use elp_ide::diagnostics::DiagnosticCode;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_log::Logger;
use expect_test::Expect;
use lsp_server::Connection;
use lsp_server::ErrorCode;
use lsp_server::Message;
use lsp_server::Notification;
use lsp_server::Request;
use lsp_types::notification::DidOpenTextDocument;
use lsp_types::notification::Exit;
use lsp_types::request::CodeActionRequest;
use lsp_types::request::Shutdown;
use lsp_types::CodeActionContext;
use lsp_types::CodeActionParams;
use lsp_types::DidOpenTextDocumentParams;
use lsp_types::PartialResultParams;
use lsp_types::Range;
use lsp_types::TextDocumentIdentifier;
use lsp_types::TextDocumentItem;
use lsp_types::Url;
use lsp_types::WorkDoneProgressParams;
use serde::Serialize;
use serde_json::json;
use serde_json::Value;
use tempfile::Builder;
use tempfile::TempDir;

pub(crate) struct Project {
    tmp_dir: TempDir,
}

impl Project {
    pub(crate) fn new() -> Project {
        Project {
            tmp_dir: Builder::new().prefix("elp_").tempdir().unwrap(),
        }
    }

    pub(crate) fn check_diagnostic(
        self,
        workspace_root: &AbsPathBuf,
        module: &str,
        expected_resp: Expect,
    ) {
        // Verify published diagnostic.

        let action =
            |mock: TestServer, id: TextDocumentIdentifier| -> Value { mock.get_diagnostic(&id) };
        self.mock_lsp(workspace_root, module, action, expected_resp);
    }

    pub(crate) fn check_code_action(
        self,
        workspace_root: &AbsPathBuf,
        module: &str,
        range: Range,
        expected_resp: Expect,
    ) {
        // Verify code action ("Quick Fix" in the IDE).

        let action = |mock: TestServer, id: TextDocumentIdentifier| -> Value {
            mock.send_request::<CodeActionRequest>(CodeActionParams {
                text_document: id,
                range,
                context: CodeActionContext::default(),
                partial_result_params: PartialResultParams::default(),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
        };
        self.mock_lsp(workspace_root, module, action, expected_resp);
    }

    fn mock_lsp<F: FnOnce(TestServer, TextDocumentIdentifier) -> Value>(
        self,
        workspace_root: &AbsPathBuf,
        module: &str,
        action: F,
        expected_resp: Expect,
    ) {
        static INIT: Once = Once::new();
        INIT.call_once(|| {
            env_logger::builder()
                .is_test(true)
                .parse_env("ELP_LOG")
                .try_init()
                .unwrap();
            profile::init_from(crate::PROFILE);
        });

        let tmp_dir_path = AbsPathBuf::assert(self.tmp_dir.path().to_path_buf());

        // Simpler version of elp::run_server //////////////////////////////////////
        // * Not handling sub-server
        // * Using memory connection rather than stdio.
        log::info!("test_server will start");
        let (connection, client) = Connection::memory();

        // Mimic ServerSetup, without the handshake
        // If the need for distinct config arises, move config in Project structure
        // (as it is done by rust analyser).
        let mut config = Config::new(
            tmp_dir_path,
            lsp_types::ClientCapabilities {
                text_document: Some(lsp_types::TextDocumentClientCapabilities {
                    definition: Some(lsp_types::GotoCapability {
                        link_support: Some(true),
                        ..Default::default()
                    }),
                    code_action: Some(lsp_types::CodeActionClientCapabilities {
                        code_action_literal_support: Some(
                            lsp_types::CodeActionLiteralSupport::default(),
                        ),
                        ..Default::default()
                    }),
                    hover: Some(lsp_types::HoverClientCapabilities {
                        content_format: Some(vec![lsp_types::MarkupKind::Markdown]),
                        ..Default::default()
                    }),
                    ..Default::default()
                }),
                window: Some(lsp_types::WindowClientCapabilities {
                    // Disable progress messages,
                    // so we don't have to filter them.
                    work_done_progress: Some(false),
                    ..Default::default()
                }),
                experimental: Some(json!({
                    // Necessary, since we'll wait for 'Running' notification.
                    // Also: closer to real-life usage.
                    "serverStatusNotification": true,
                })),
                ..Default::default()
            },
        );
        config.ignore_diagnostic(DiagnosticCode::MissingCompileWarnMissingSpec);

        let handle = thread::spawn(|| {
            let server = setup::setup_server(config, connection, Logger::default())?;
            server.main_loop()
        });

        {
            // We want to drop the mock at the end of this block to initiate shutdown sequence.
            // Otherwise handle.join() would hang.

            let mock = TestServer::new(client, self.tmp_dir);

            let document = workspace_root.join(module);
            let id = mock.doc_id(&document.as_path().display().to_string());

            // Indicate the document was opened.
            // - It mimicks what the IDE would do.
            // - It is required for Eqwalizer diagnostics, which are
            //   only ran on explicitly opened documents.
            mock.notification::<DidOpenTextDocument>(DidOpenTextDocumentParams {
                text_document: TextDocumentItem::new(
                    id.uri.clone(),
                    "erlang".to_string(),
                    0,
                    fs::read_to_string(document).unwrap(),
                ),
            });

            mock.wait_until_workspace_is_loaded();

            let actual = action(mock, id);
            let actual_string = serde_json::to_string_pretty(&actual).unwrap();
            let actual_string = replace_paths(actual_string);
            expected_resp.assert_eq(&actual_string);
        }

        handle.join().unwrap().unwrap();
    }
}

fn replace_paths(actual_string: String) -> String {
    let to_replace = env!("CARGO_WORKSPACE_DIR");
    actual_string.replace(to_replace, "/[..]/")
}

pub(crate) fn code_action_project(
    workspace_root: &AbsPathBuf,
    module: &str,
    range: Range,
    expected_resp: Expect,
) {
    Project::new().check_code_action(workspace_root, module, range, expected_resp);
}

pub(crate) fn diagnostic_project(workspace_root: &AbsPathBuf, module: &str, expected_resp: Expect) {
    Project::new().check_diagnostic(workspace_root, module, expected_resp);
}

// Bridge between (test) Project and real Server.
// It is called "Server" in rust analyser,
// yet it's principally mocking the LSP client.
pub(crate) struct TestServer {
    req_id: Cell<i32>,
    messages: RefCell<Vec<Message>>,
    client: Connection,
    /// XXX: remove the tempdir last
    dir: TempDir,
}

impl TestServer {
    fn new(client: Connection, dir: TempDir) -> TestServer {
        TestServer {
            req_id: Cell::new(1),
            messages: Default::default(),
            client,
            dir,
        }
    }

    pub(crate) fn doc_id(&self, rel_path: &str) -> TextDocumentIdentifier {
        let path = self.dir.path().join(rel_path);
        TextDocumentIdentifier {
            uri: Url::from_file_path(path).unwrap(),
        }
    }

    pub(crate) fn notification<N>(&self, params: N::Params)
    where
        N: lsp_types::notification::Notification,
        N::Params: Serialize,
    {
        let r = Notification::new(N::METHOD.to_string(), params);
        self.send_notification(r)
    }

    /// Send request to elp server, and wait for response.
    pub(crate) fn send_request<R>(&self, params: R::Params) -> Value
    where
        R: lsp_types::request::Request,
        R::Params: Serialize,
    {
        let id = self.req_id.get();
        self.req_id.set(id.wrapping_add(1));

        let r = Request::new(id.into(), R::METHOD.to_string(), params);
        self.send_request_(r)
    }
    fn send_request_(&self, r: Request) -> Value {
        let id = r.id.clone();
        self.client.sender.send(r.clone().into()).unwrap();
        while let Some(msg) = self
            .recv()
            .unwrap_or_else(|Timeout| panic!("timeout waiting for response to request: {:?}", r))
        {
            match msg {
                Message::Request(req) => {
                    if req.method == "client/registerCapability" {
                        let params = req.params.to_string();
                        if ["workspace/didChangeWatchedFiles", "textDocument/didSave"]
                            .iter()
                            .any(|&it| params.contains(it))
                        {
                            continue;
                        }
                    }
                    continue;
                }
                Message::Notification(_) => {}
                Message::Response(res) => {
                    assert_eq!(res.id, id);
                    if let Some(err) = res.error {
                        if err.code == ErrorCode::ContentModified as i32 {
                            // "elp still loading".
                            // wait for notification before sending first request.
                            log::error!("response: {:#?}", err);
                            std::thread::sleep(std::time::Duration::from_millis(100));
                            continue;
                        } else {
                            panic!("error response: {:#?}", err);
                        }
                    }
                    return res.result.unwrap();
                }
            }
        }
        panic!("no response for {:?}", r);
    }

    pub(crate) fn wait_until_workspace_is_loaded(&self) {
        // We wait until 'Running' phase, so that the request we want to test
        // won't be answered by "elp still loading".
        // ELP itself might still do other stuff for eqwalizer, which isn't an issue.
        // On the contrary: the test is closer to real-life, with concurrent tasks.
        while let Some(msg) = self
            .recv()
            .unwrap_or_else(|Timeout| panic!("timeout whilst waiting for workspace to load: "))
        {
            match msg {
                // Filter our other requests such as "client/registerCapability"
                Message::Request(_req) => {}
                Message::Response(res) => {
                    panic!("No request sent, didn't expect response: {:#?}", res);
                }
                Message::Notification(notif) => match notif.method.as_str() {
                    "elp/status" => {
                        let params = notif.params.to_string();
                        if params.contains("Running") {
                            return;
                        }
                    }
                    "textDocument/publishDiagnostics" | "$/progress" => {
                        log::info!("======{:#?}", notif);
                    }
                    "telemetry/event" => {
                        log::info!("------{:#?}", notif);
                    }
                    _ => {
                        panic!("{:#?}", notif);
                    }
                },
            }
        }
        unreachable!()
    }

    pub(crate) fn get_diagnostic(&self, id: &TextDocumentIdentifier) -> Value {
        let module = &id.uri.as_str();
        while let Some(msg) = self
            .recv()
            .unwrap_or_else(|Timeout| panic!("timeout whilst waiting for diagnostic: "))
        {
            match msg {
                // Filter our other requests such as "client/registerCapability"
                Message::Request(_req) => {}
                Message::Response(res) => {
                    panic!("No request sent, didn't expect response: {:#?}", res);
                }
                Message::Notification(notif) => {
                    match notif.method.as_str() {
                        "elp/event" => {
                            // Filter events such as "EqWAlizing completed"
                            continue;
                        }
                        "textDocument/publishDiagnostics" => {
                            if notif.params.to_string().contains(module) {
                                let diagnostics = &notif.params["diagnostics"];
                                match diagnostics {
                                    Value::Array(_) => return notif.params,
                                    _ => {
                                        panic!("Unexpected diagnostic format: {:#?}", diagnostics)
                                    }
                                }
                            }
                            // Filter (empty) diagnostics of other modules.
                            continue;
                        }
                        _ => {
                            // Filter irrelevant notifications.
                            continue;
                        }
                    }
                }
            }
        }
        unreachable!()
    }

    fn recv(&self) -> Result<Option<Message>, Timeout> {
        let msg = recv_timeout(&self.client.receiver)?;
        let msg = msg.map(|msg| {
            self.messages.borrow_mut().push(msg.clone());
            msg
        });
        Ok(msg)
    }
    fn send_notification(&self, not: Notification) {
        self.client.sender.send(Message::Notification(not)).unwrap();
    }
}

impl Drop for TestServer {
    fn drop(&mut self) {
        self.send_request::<Shutdown>(());
        self.notification::<Exit>(());
    }
}

struct Timeout;

fn recv_timeout(receiver: &Receiver<Message>) -> Result<Option<Message>, Timeout> {
    let timeout = Duration::from_secs(30);
    select! {
        recv(receiver) -> msg => Ok(msg.ok()),
        recv(after(timeout)) -> _ => Err(Timeout),
    }
}
