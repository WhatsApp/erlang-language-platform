/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::panic;

use anyhow::Result;
use anyhow::bail;
use crossbeam_channel::Sender;
use elp_ide::is_cancelled;
use lsp_server::ExtractError;
use lsp_server::RequestId;
use lsp_server::Response;
use serde::Serialize;
use serde::de::DeserializeOwned;

use super::Server;
use super::Snapshot;
use crate::LspError;
use crate::server::Task;

#[must_use]
pub(crate) struct RequestDispatcher<'a> {
    pub(crate) req: Option<lsp_server::Request>,
    pub(crate) server: &'a mut Server,
}

impl<'a> RequestDispatcher<'a> {
    pub fn new(server: &'a mut Server, req: lsp_server::Request) -> Self {
        RequestDispatcher {
            req: Some(req),
            server,
        }
    }

    /// Dispatches the request onto the current thread
    pub(crate) fn on_sync<R>(
        mut self,
        f: fn(&mut Server, R::Params) -> Result<R::Result>,
    ) -> Result<Self>
    where
        R: lsp_types::request::Request + 'static,
        R::Params: DeserializeOwned + panic::UnwindSafe + fmt::Debug + 'static,
        R::Result: Serialize + 'static,
    {
        let (id, params) = match self.parse::<R>() {
            Some(it) => it,
            None => return Ok(self),
        };

        let world = panic::AssertUnwindSafe(&mut *self.server);

        let response = panic::catch_unwind(move || {
            // force closure to capture the entire world, not just &mut Server
            // and dropping the AssertUnwindSafe information
            // (since we only use world.0 and closures in Rust 2021 capture only used fields)
            let world = world;
            let _pctx =
                stdx::panic_context::enter(format!("\nrequest: {} {:#?}", R::METHOD, params));
            let result = f(world.0, params);
            result_to_response::<R>(id, result)
        })
        .map_err(|_err| anyhow::Error::msg(format!("sync task {:?} panicked", R::METHOD)))?;
        self.server.send_response(response);
        Ok(self)
    }

    /// Dispatches the request onto thread pool
    pub(crate) fn on<R>(mut self, f: fn(Snapshot, R::Params) -> Result<R::Result>) -> Self
    where
        R: lsp_types::request::Request + 'static,
        R::Params: DeserializeOwned + Send + fmt::Debug + 'static,
        R::Result: Serialize + 'static,
    {
        let (id, params) = match self.parse::<R>() {
            Some(it) => it,
            None => return self,
        };

        self.server.task_pool.handle.spawn_with_sender({
            let world = self.server.snapshot();

            move |sender| {
                let _pctx =
                    stdx::panic_context::enter(format!("\nrequest: {} {:#?}", R::METHOD, params));
                let error_bomb = ErrorBomb::new(sender.clone(), id.clone());
                let result = f(world, params);
                error_bomb.defuse();
                sender
                    .send(Task::Response(result_to_response::<R>(id, result)))
                    .unwrap();
            }
        });

        self
    }

    pub(crate) fn finish(mut self) {
        if let Some(req) = self.req.take() {
            // The request has not been processed by any of the dispatch handlers
            let id = req.id;
            self.server.send_response(Response::new_err(
                id,
                lsp_server::ErrorCode::MethodNotFound as i32,
                "unknown request".to_string(),
            ));
        }
    }

    fn parse<R>(&mut self) -> Option<(lsp_server::RequestId, R::Params)>
    where
        R: lsp_types::request::Request + 'static,
        R::Params: DeserializeOwned + 'static,
    {
        let req = match &self.req {
            Some(req) if req.method == R::METHOD => self.req.take().unwrap(),
            _ => return None,
        };

        let res = crate::from_json(R::METHOD, req.params);
        match res {
            Ok(params) => Some((req.id, params)),
            Err(err) => {
                let response = lsp_server::Response::new_err(
                    req.id,
                    lsp_server::ErrorCode::InvalidParams as i32,
                    err.to_string(),
                );
                self.server.send_response(response);
                None
            }
        }
    }
}

// ---------------------------------------------------------------------

fn result_to_response<R>(
    id: lsp_server::RequestId,
    result: Result<R::Result>,
) -> lsp_server::Response
where
    R: lsp_types::request::Request + 'static,
    R::Params: DeserializeOwned + 'static,
    R::Result: Serialize + 'static,
{
    match result {
        Ok(resp) => lsp_server::Response::new_ok(id, &resp),
        Err(e) => match e.downcast::<LspError>() {
            Ok(lsp_error) => lsp_server::Response::new_err(id, lsp_error.code, lsp_error.message),
            Err(e) => {
                if is_cancelled(&*e) {
                    lsp_server::Response::new_err(
                        id,
                        lsp_server::ErrorCode::ContentModified as i32,
                        "content modified".to_string(),
                    )
                } else {
                    lsp_server::Response::new_err(
                        id,
                        lsp_server::ErrorCode::InternalError as i32,
                        e.to_string(),
                    )
                }
            }
        },
    }
}

struct ErrorBomb {
    sender: Sender<Task>,
    request_id: Option<RequestId>,
}

impl ErrorBomb {
    fn new(sender: Sender<Task>, id: RequestId) -> Self {
        Self {
            sender,
            request_id: Some(id),
        }
    }

    fn defuse(mut self) {
        self.request_id = None
    }
}

impl Drop for ErrorBomb {
    fn drop(&mut self) {
        if let Some(id) = self.request_id.take() {
            self.sender
                .send(Task::Response(lsp_server::Response::new_err(
                    id,
                    lsp_server::ErrorCode::InternalError as i32,
                    "internal error".to_string(),
                )))
                .unwrap();
        }
    }
}

#[must_use]
pub struct NotificationDispatcher<'a, S, R> {
    notif: Option<lsp_server::Notification>,
    server: &'a mut S,
    result: Option<R>,
}

impl<'a, S, R> NotificationDispatcher<'a, S, R> {
    pub fn new(server: &'a mut S, notif: lsp_server::Notification) -> Self {
        NotificationDispatcher {
            notif: Some(notif),
            server,
            result: None,
        }
    }

    pub fn on<N>(mut self, f: fn(&mut S, N::Params) -> Result<R>) -> Result<Self>
    where
        N: lsp_types::notification::Notification + 'static,
        N::Params: DeserializeOwned + Send + 'static,
    {
        let notif = match self.notif.take() {
            Some(notif) => notif,
            None => return Ok(self),
        };

        let params = match notif.extract(N::METHOD) {
            Ok(params) => params,
            Err(ExtractError::JsonError { method, error }) => {
                bail!("Invalid request\nMethod: {method}\n error: {error}");
            }
            Err(ExtractError::MethodMismatch(notif)) => {
                self.notif = Some(notif);
                return Ok(self);
            }
        };

        self.result = Some(f(self.server, params)?);
        Ok(self)
    }

    pub fn finish(self) {
        if let Some(notif) = self.notif {
            log::error!("unhandled notification: {:?}", notif);
        }
    }
}
