/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A lightweight telemetry facade, inspired by the logging framework
//! used by Rust Analyzer: https://github.com/rust-lang/log
//!
//! This module provides a single telemetry API.

//! If no telemetry implementation is selected, the facade falls back to
//! a “noop” implementation that ignores all telemetry messages. The overhead
//! in this case is very small - just an integer load, comparison and
//! jump.

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
) -> TelemetryMessage {
    TelemetryMessage {
        // Note: the "type" field is required, otherwise the telemetry
        // mapper in the vscode extension will not route the message
        // to chronicle, and hence scuba. The value is mapped to the
        //"extras.eventName" field.
        typ,
        duration_ms,
        data,
    }
}

fn do_send(typ: String, data: serde_json::Value, duration: Option<Duration>) {
    let message = build_message(typ, data, duration);
    let _ = sender().send(message);
}

pub fn send(typ: String, data: serde_json::Value) {
    do_send(typ, data, None);
}

pub fn send_with_duration(typ: String, data: serde_json::Value, duration: Duration) {
    do_send(typ, data, Some(duration));
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    #[test]
    fn it_works() {
        let typ = String::from("telemetry");
        let data = serde_json::to_value("Hello telemetry!").unwrap();
        super::send(typ, data);

        let msg = super::receiver().try_recv().unwrap();
        expect![[r#"
            TelemetryMessage {
                typ: "telemetry",
                duration_ms: None,
                data: String("Hello telemetry!"),
            }
        "#]]
        .assert_debug_eq(&msg);
    }
}
