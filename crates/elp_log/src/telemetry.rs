/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! A lightweight telemetry facade, inspired by the logging framework
//! used by Rust Analyzer: https://github.com/rust-lang/log
//!
//! This module provides a single telemetry API.

//! If no telemetry implementation is selected, the facade falls back to
//! a “noop” implementation that ignores all telemetry messages. The overhead
//! in this case is very small - just an integer load, comparison and
//! jump.

use std::time::SystemTime;

use lazy_static::lazy_static;
use serde::Deserialize;
use serde::Serialize;

pub type TelemetryData = serde_json::Value;
pub type Duration = u32;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TelemetryMessage {
    #[serde(rename = "type")]
    pub typ: String,
    pub duration_ms: Option<Duration>,
    pub start_time: Option<SystemTime>,
    pub data: TelemetryData,
}

pub type TelemetrySender = crossbeam_channel::Sender<TelemetryMessage>;
pub type TelemetryReceiver = crossbeam_channel::Receiver<TelemetryMessage>;

lazy_static! {
    static ref CHANNEL: (TelemetrySender, TelemetryReceiver) = crossbeam_channel::unbounded();
}

pub fn sender() -> &'static TelemetrySender {
    &CHANNEL.0
}

pub fn receiver() -> &'static TelemetryReceiver {
    &CHANNEL.1
}

fn build_message(
    typ: String,
    data: TelemetryData,
    duration_ms: Option<Duration>,
    start_time: Option<SystemTime>,
) -> TelemetryMessage {
    TelemetryMessage {
        // Note: the "type" field is required, otherwise the telemetry
        // mapper in the vscode extension will not route the message
        // to chronicle, and hence scuba. The value is mapped to the
        //"extras.eventName" field.
        typ,
        duration_ms,
        start_time,
        data,
    }
}

fn do_send(
    typ: String,
    data: serde_json::Value,
    duration: Option<Duration>,
    start_time: Option<SystemTime>,
) {
    let message = build_message(typ, data, duration, start_time);
    let _ = sender().send(message);
}

pub fn send(typ: String, data: serde_json::Value) {
    do_send(typ, data, None, None);
}

pub fn send_with_duration(
    typ: String,
    data: serde_json::Value,
    duration: Duration,
    start_time: SystemTime,
) {
    do_send(typ, data, Some(duration), Some(start_time));
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    #[test]
    fn it_works2() {
        let typ = String::from("telemetry");
        let data = serde_json::to_value("Hello telemetry!").unwrap();
        super::send(typ, data);

        let msg = super::receiver().try_recv().unwrap();
        expect![[r#"
            TelemetryMessage {
                typ: "telemetry",
                duration_ms: None,
                start_time: None,
                data: String("Hello telemetry!"),
            }
        "#]]
        .assert_debug_eq(&msg);
    }
}
