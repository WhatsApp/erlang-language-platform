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

use super::telemetry_manager::WithTelemetry;
use super::telemetry_manager::reporter_telemetry_end;
use super::telemetry_manager::reporter_telemetry_next;
use super::telemetry_manager::reporter_telemetry_start;

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
        Spinner::begin(
            self.sender.clone(),
            self.next_token(),
            title,
            WithTelemetry::No,
        )
    }

    pub fn begin_spinner_with_telemetry(&mut self, title: String) -> Spinner {
        Spinner::begin(
            self.sender.clone(),
            self.next_token(),
            title,
            WithTelemetry::Yes,
        )
    }

    pub fn begin_bar(&mut self, title: String, total: Option<usize>) -> ProgressBar {
        ProgressBar::begin(
            self.sender.clone(),
            self.next_token(),
            title,
            total,
            WithTelemetry::No,
        )
    }

    pub fn begin_bar_with_telemetry(&mut self, title: String, total: Option<usize>) -> ProgressBar {
        ProgressBar::begin(
            self.sender.clone(),
            self.next_token(),
            title,
            total,
            WithTelemetry::Yes,
        )
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
    /// The message sent when sending the final end report.
    /// It is used in e2e tests to check for a specific task being done.
    end_message: String,
}

impl Spinner {
    fn begin(
        sender: Sender<ProgressTask>,
        token: NumberOrString,
        title: String,
        telemetry: WithTelemetry,
    ) -> Self {
        let msg = WorkDoneProgressBegin {
            title: title.clone(),
            cancellable: None,
            message: None,
            percentage: None,
        };

        if telemetry == WithTelemetry::Yes {
            reporter_telemetry_start(token.clone(), title.clone());
        }
        send_begin(&sender, token.clone(), msg);
        Self {
            token,
            sender,
            end_message: title,
        }
    }

    pub fn report(&self, message: String) {
        let msg = WorkDoneProgress::Report(WorkDoneProgressReport {
            cancellable: None,
            message: Some(message.clone()),
            percentage: None,
        });
        reporter_telemetry_next(self.token.clone(), message);
        send_progress(&self.sender, self.token.clone(), msg);
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
            WorkDoneProgress::End(WorkDoneProgressEnd {
                message: Some(self.end_message.clone()),
            }),
        );
        reporter_telemetry_end(self.token.clone());
    }
}

#[derive(Debug)]
#[must_use]
pub struct ProgressBar {
    token: NumberOrString,
    sender: Sender<ProgressTask>,
    /// The message sent when sending the final end report.
    /// It is used in e2e tests to check for a specific task being done.
    end_message: String,
}

impl ProgressBar {
    fn begin(
        sender: Sender<ProgressTask>,
        token: NumberOrString,
        title: String,
        total: Option<usize>,
        report_telemetry: WithTelemetry,
    ) -> Self {
        let msg = WorkDoneProgressBegin {
            title: title.clone(),
            cancellable: None,
            message: total.map(|_total| format!("0%")),
            percentage: Some(0),
        };
        if report_telemetry == WithTelemetry::Yes {
            reporter_telemetry_start(token.clone(), title.clone());
        }
        send_begin(&sender, token.clone(), msg);
        Self {
            token,
            sender,
            end_message: title,
        }
    }

    pub fn report(&self, done: usize, total: usize) {
        let percent_f = done as f64 / total.max(1) as f64;
        let percent = (percent_f * 100.0) as u32;
        let message = format!("{}%", percent);
        let msg = WorkDoneProgress::Report(WorkDoneProgressReport {
            cancellable: None,
            message: Some(message),
            percentage: Some(percent),
        });
        // We do not update any associated telemetry here, it will
        // report on the whole bar at the end in the drop.
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
            WorkDoneProgress::End(WorkDoneProgressEnd {
                message: Some(self.end_message.clone()),
            }),
        );
        reporter_telemetry_end(self.token.clone());
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
