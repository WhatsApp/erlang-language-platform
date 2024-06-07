/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crossbeam_channel::Receiver;
use crossbeam_channel::SendError;
use crossbeam_channel::Sender;
use lsp_types::NumberOrString;
use lsp_types::ProgressParams;
use lsp_types::ProgressParamsValue;
use lsp_types::WorkDoneProgress;
use lsp_types::WorkDoneProgressBegin;
use lsp_types::WorkDoneProgressEnd;
use lsp_types::WorkDoneProgressReport;

#[derive(Debug)]
pub enum ProgressTask {
    BeginNotify(ProgressParams),
    Notify(ProgressParams),
}

// Follow structs don't derive Clone on purpose - this would violate the invariants
// and result in duplicate messages

#[derive(Debug)]
pub struct ProgressManager {
    counter: usize,
    sender: Sender<ProgressTask>,
    receiver: Receiver<ProgressTask>,
}

impl Default for ProgressManager {
    fn default() -> Self {
        let (sender, receiver) = crossbeam_channel::unbounded();
        ProgressManager {
            counter: 0,
            sender,
            receiver,
        }
    }
}

impl ProgressManager {
    pub fn receiver(&self) -> &Receiver<ProgressTask> {
        &self.receiver
    }

    pub fn begin_spinner(&mut self, title: String) -> Spinner {
        Spinner::begin(self.sender.clone(), self.next_token(), title)
    }

    pub fn begin_bar(&mut self, title: String, total: Option<usize>) -> ProgressBar {
        ProgressBar::begin(self.sender.clone(), self.next_token(), title, total)
    }

    fn next_token(&mut self) -> NumberOrString {
        let token = NumberOrString::String(format!("ELP/{}", self.counter));
        self.counter += 1;
        token
    }
}

#[derive(Debug)]
#[must_use]
pub struct Spinner {
    token: NumberOrString,
    sender: Sender<ProgressTask>,
}

impl Spinner {
    fn begin(sender: Sender<ProgressTask>, token: NumberOrString, title: String) -> Self {
        let msg = WorkDoneProgressBegin {
            title,
            cancellable: None,
            message: None,
            percentage: None,
        };
        send_begin(&sender, token.clone(), msg);
        Self { token, sender }
    }

    pub fn end(self) {
        // let Drop do the job
    }
}

impl Drop for Spinner {
    fn drop(&mut self) {
        send_progress(
            &self.sender,
            self.token.clone(),
            WorkDoneProgress::End(WorkDoneProgressEnd { message: None }),
        )
    }
}

#[derive(Debug)]
#[must_use]
pub struct ProgressBar {
    token: NumberOrString,
    sender: Sender<ProgressTask>,
}

impl ProgressBar {
    fn begin(
        sender: Sender<ProgressTask>,
        token: NumberOrString,
        title: String,
        total: Option<usize>,
    ) -> Self {
        let msg = WorkDoneProgressBegin {
            title,
            cancellable: None,
            message: total.map(|total| format!("0/{}", total)),
            percentage: Some(0),
        };
        send_begin(&sender, token.clone(), msg);
        Self { token, sender }
    }

    pub fn report(&self, done: usize, total: usize) {
        let message = format!("{}/{}", done, total);
        let percent = done as f64 / total.max(1) as f64;
        let msg = WorkDoneProgress::Report(WorkDoneProgressReport {
            cancellable: None,
            message: Some(message),
            percentage: Some((percent * 100.0) as u32),
        });
        send_progress(&self.sender, self.token.clone(), msg);
    }

    pub fn end(self) {
        // let Drop do the job
    }
}

impl Drop for ProgressBar {
    fn drop(&mut self) {
        send_progress(
            &self.sender,
            self.token.clone(),
            WorkDoneProgress::End(WorkDoneProgressEnd { message: None }),
        )
    }
}

fn send_begin(sender: &Sender<ProgressTask>, token: NumberOrString, msg: WorkDoneProgressBegin) {
    let params = ProgressParams {
        token,
        value: ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(msg)),
    };
    sender.send(ProgressTask::BeginNotify(params)).unwrap();
}

fn send_progress(sender: &Sender<ProgressTask>, token: NumberOrString, msg: WorkDoneProgress) {
    let params = ProgressParams {
        token,
        value: ProgressParamsValue::WorkDone(msg),
    };
    if let Err(SendError(err)) = sender.send(ProgressTask::Notify(params)) {
        log::error!("Failed to send progress message: {:?}", err);
    }
}
