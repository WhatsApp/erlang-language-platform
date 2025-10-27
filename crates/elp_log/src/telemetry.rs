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

pub use humantime::format_rfc3339_millis;
use lazy_static::lazy_static;
use serde::Deserialize;
use serde::Serialize;

pub type TelemetryData = serde_json::Value;
pub type DurationMs = u32;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TelemetryMessage {
    #[serde(rename = "type")]
    pub typ: String,
    pub duration_ms: Option<DurationMs>,
    pub start_time: Option<SystemTime>,
    pub start_time_string: Option<String>,
    pub end_time_string: Option<String>,
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
    duration_ms: Option<DurationMs>,
    start_time: Option<SystemTime>,
) -> TelemetryMessage {
    let start_time_string = start_time.map(|time| format_rfc3339_millis(time).to_string());
    let end_time_string = start_time.map(|time| match duration_ms {
        Some(d) => {
            format_rfc3339_millis(time + std::time::Duration::from_millis(d as u64)).to_string()
        }
        None => format_rfc3339_millis(time).to_string(),
    });

    TelemetryMessage {
        // Note: the "type" field is required, otherwise the telemetry
        // mapper in the vscode extension will not route the message
        // to chronicle, and hence scuba. The value is mapped to the
        //"extras.eventName" field.
        typ,
        duration_ms,
        start_time,
        start_time_string,
        end_time_string,
        data,
    }
}

fn do_send(
    typ: String,
    data: serde_json::Value,
    duration: Option<DurationMs>,
    start_time: Option<SystemTime>,
) {
    let message = build_message(typ, data, duration, start_time);
    let _ = sender().send(message);
}

pub fn send(typ: String, data: serde_json::Value) {
    do_send(typ, data, None, Some(SystemTime::now()));
}

pub fn send_with_duration(
    typ: String,
    data: serde_json::Value,
    duration: DurationMs,
    start_time: SystemTime,
) {
    do_send(typ, data, Some(duration), Some(start_time));
}

pub fn report_elapsed_time(what: &str, start_time: SystemTime) {
    let data = serde_json::Value::String(what.to_string());
    let duration = start_time.elapsed().map(|e| e.as_millis()).unwrap_or(0) as u32;
    send_with_duration("telemetry".to_string(), data, duration, start_time);
}

#[cfg(test)]
mod tests {
    use std::time::SystemTime;

    use expect_test::expect;

    #[test]
    fn it_works2() {
        let typ = String::from("telemetry");
        let data = serde_json::to_value("Hello telemetry!").unwrap();
        super::send(typ, data);

        let mut msg = super::receiver().try_recv().unwrap();
        msg.start_time = msg.start_time.map(|_st| SystemTime::UNIX_EPOCH);
        msg.start_time_string = msg
            .start_time_string
            .map(|_| "2025-09-22T11:38:41.274Z".to_string());
        msg.end_time_string = msg
            .end_time_string
            .map(|_| "2025-09-22T11:38:41.321".to_string());
        expect![[r#"
            TelemetryMessage {
                typ: "telemetry",
                duration_ms: None,
                start_time: Some(
                    SystemTime {
                        tv_sec: 0,
                        tv_nsec: 0,
                    },
                ),
                start_time_string: Some(
                    "2025-09-22T11:38:41.274Z",
                ),
                end_time_string: Some(
                    "2025-09-22T11:38:41.321",
                ),
                data: String("Hello telemetry!"),
            }
        "#]]
        .assert_debug_eq(&msg);
    }
}
