/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Manage Telemetry data for the server.

use std::sync::LazyLock;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use elp_ide::elp_ide_db::salsa_telemetry;
use elp_ide::elp_ide_db::salsa_telemetry::SalsaEventCounts;
use elp_log::telemetry;
use elp_log::telemetry::send_with_duration;
use fxhash::FxHashMap;
use parking_lot::Mutex;
use serde::Serialize;

use crate::memory_usage::Bytes;
use crate::memory_usage::MemoryUsage;

const PERIODIC_INTERVAL_MS: Duration = Duration::from_millis(60_000);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum OperationalState {
    NotStarted,
    Started,
    Restarted,
}
pub(crate) struct TelemetryManager {
    last_periodic_processed: Instant,
    server_started_at: SystemTime,
    operational_state: OperationalState,
    last_salsa_counts: SalsaEventCounts,
}
impl TelemetryManager {
    pub(crate) fn new() -> Self {
        Self {
            last_periodic_processed: Instant::now(),
            server_started_at: SystemTime::now(),
            operational_state: OperationalState::NotStarted,
            last_salsa_counts: SalsaEventCounts::default(),
        }
    }

    pub(crate) fn on_periodic(&mut self, num_open_files: usize, num_loaded_files: u32) {
        if self.last_periodic_processed.elapsed() >= PERIODIC_INTERVAL_MS {
            self.last_periodic_processed = Instant::now();
            let mem_usage = MemoryUsage::now();
            #[derive(Serialize)]
            struct MemoryStats {
                allocated: Bytes,
                active: Bytes,
                resident: Bytes,
                num_open_files: usize,
                num_loaded_files: u32,
            }
            let mem_stats = MemoryStats {
                allocated: mem_usage.allocated,
                active: mem_usage.active,
                resident: mem_usage.resident,
                num_open_files,
                num_loaded_files,
            };
            match serde_json::to_value(mem_stats) {
                Ok(mem_usage_value) => {
                    telemetry::send("periodic_memory_stats".to_string(), mem_usage_value);
                }
                Err(err) => {
                    log::warn!("on_periodic: unable to serialize {mem_usage:?}, err: {err}");
                }
            };

            self.send_salsa_event_counts();
        }
    }

    /// Emit the salsa event counts: both the process-lifetime cumulative total
    /// (`*_total`) and the increment since the previous periodic tick (`*_delta`)
    /// for each counter. Reuse of interned slots and cross-thread query blocking
    /// both lengthen the main-loop cancellation wait, so a rising delta here
    /// alongside `main_loop_db_write_stall` events points at the cause; the
    /// totals give the absolute scale without having to sum the deltas.
    fn send_salsa_event_counts(&mut self) {
        let current = salsa_telemetry::counts();
        let delta = current.delta_since(&self.last_salsa_counts);
        self.last_salsa_counts = current;

        let data = serde_json::json!({
            "did_intern_total": current.did_intern,
            "did_intern_delta": delta.did_intern,
            "did_reuse_interned_total": current.did_reuse_interned,
            "did_reuse_interned_delta": delta.did_reuse_interned,
            "did_validate_interned_total": current.did_validate_interned,
            "did_validate_interned_delta": delta.did_validate_interned,
            "will_block_on_total": current.will_block_on,
            "will_block_on_delta": delta.will_block_on,
            "did_set_cancellation_flag_total": current.did_set_cancellation_flag,
            "did_set_cancellation_flag_delta": delta.did_set_cancellation_flag,
        });
        telemetry::send("salsa_event_counts".to_string(), data);
    }

    pub(crate) fn operational(&mut self, buck_quick_start: bool) {
        let title = if buck_quick_start {
            match self.operational_state {
                OperationalState::NotStarted => "ELP operational (quick start)",
                OperationalState::Started => "ELP operational (fully)",
                OperationalState::Restarted => return,
            }
        } else {
            match self.operational_state {
                OperationalState::NotStarted => "ELP operational",
                OperationalState::Started => return,
                OperationalState::Restarted => return,
            }
        };

        let duration = self
            .server_started_at
            .elapsed()
            .map(|e| e.as_millis())
            .unwrap_or(0) as u32;

        #[derive(Serialize)]
        struct Operational {
            title: String,
        }
        let data = Operational {
            title: title.to_string(),
        };
        let data = serde_json::to_value(data).unwrap_or_else(|err| {
            serde_json::Value::String(format!("JSON serialization failed: {err}"))
        });
        telemetry::send_with_duration(
            String::from("telemetry"),
            data,
            duration,
            self.server_started_at,
        );
        self.operational_state = if self.operational_state == OperationalState::NotStarted {
            OperationalState::Started
        } else {
            OperationalState::Restarted
        };
    }
}

// ---------------------------------------------------------------------

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum WithTelemetry {
    Yes,
    No,
}

pub(crate) fn reporter_telemetry_start(
    token: lsp_types::NumberOrString,
    title: String,
) -> ReporterTelemetry {
    let mut telemetry = TELEMETRY.lock();
    telemetry.new_reporter(token, title)
}

pub(crate) fn reporter_telemetry_next(token: lsp_types::NumberOrString, message: String) {
    let mut telemetry = TELEMETRY.lock();
    telemetry.next_segment(token, Some(message));
}

pub(crate) fn reporter_telemetry_end(token: lsp_types::NumberOrString) {
    let mut telemetry = TELEMETRY.lock();
    if let Some(t) = telemetry.end(token) {
        t.send_final_telemetry();
    }
}

static TELEMETRY: LazyLock<Mutex<ReporterTelemetryManager>> =
    LazyLock::new(|| Mutex::new(ReporterTelemetryManager::default()));

#[derive(Default, Debug)]
struct ReporterTelemetryManager {
    /// telemetry state indexed by reporting progress tokens, storing last time it changed state
    active: FxHashMap<lsp_types::NumberOrString, ReporterTelemetry>,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct ReporterTelemetry {
    #[serde(skip_serializing)]
    start_time: SystemTime,
    title: String,
    #[serde(skip_serializing)]
    segment_start_time: SystemTime,
    #[serde(skip_serializing)]
    segment_message: String,
    segments: Vec<(String, u32)>,
}

impl ReporterTelemetryManager {
    fn new_reporter(
        &mut self,
        token: lsp_types::NumberOrString,
        title: String,
    ) -> ReporterTelemetry {
        let time = SystemTime::now();
        let val = ReporterTelemetry {
            start_time: time,
            title: title.clone(),
            segment_start_time: time,
            segment_message: title,
            segments: Vec::new(),
        };
        self.active.insert(token, val.clone());
        val
    }

    fn next_segment(&mut self, token: lsp_types::NumberOrString, message: Option<String>) {
        self.active.entry(token.clone()).and_modify(|then| {
            then.update(message);
        });
    }

    fn end(&mut self, token: lsp_types::NumberOrString) -> Option<ReporterTelemetry> {
        self.next_segment(token.clone(), None);
        self.active.get(&token).cloned()
    }
}

impl ReporterTelemetry {
    fn update(&mut self, message: Option<String>) {
        // First capture the prior segment timing
        self.segments.push((
            self.segment_message.clone(),
            self.segment_duration().as_millis() as u32,
        ));
        // Then do the update
        if let Some(message) = message {
            let time = SystemTime::now();
            self.segment_start_time = time;
            self.segment_message = message;
        }
    }

    pub(crate) fn segment_duration(&self) -> Duration {
        self.segment_start_time.elapsed().unwrap_or_default()
    }

    pub(crate) fn full_duration(&self) -> Duration {
        self.start_time.elapsed().unwrap_or_default()
    }

    pub(crate) fn send_final_telemetry(&self) {
        let data = serde_json::to_value(self).unwrap_or_else(|err| {
            serde_json::Value::String(format!("JSON serialization failed: {err}"))
        });
        send_with_duration(
            "elp_reporter_telemetry".to_string(),
            data,
            self.full_duration().as_millis() as u32,
            self.start_time,
        );
    }
}

#[cfg(test)]
mod test {
    use std::sync::LazyLock;

    use expect_test::expect;
    use lsp_types::NumberOrString;
    use parking_lot::Mutex;

    use crate::server::telemetry_manager::ReporterTelemetryManager;

    #[test]
    fn test_lifecycle() {
        static TEST_TELEMETRY: LazyLock<Mutex<ReporterTelemetryManager>> =
            LazyLock::new(|| Mutex::new(ReporterTelemetryManager::default()));

        let token = NumberOrString::Number(1);
        let message = "Start message".to_string();

        let orig = {
            // Control lock scope by having it in braces
            let mut telemetry = TEST_TELEMETRY.lock();
            telemetry.new_reporter(token.clone(), message)
        };

        {
            // Control lock scope by having it in braces
            let mut telemetry = TEST_TELEMETRY.lock();
            let update_message = "update message".to_string();
            telemetry.next_segment(token.clone(), Some(update_message.clone()));
            let new = telemetry.active.get(&token).unwrap();
            assert_ne!(orig.segment_start_time, new.segment_start_time);
            assert_eq!(orig.start_time, new.start_time);
            assert_eq!(new.segment_message, update_message);
        }

        {
            // Control lock scope by having it in braces
            let mut telemetry = TEST_TELEMETRY.lock();
            telemetry.next_segment(token.clone(), None);
            let new = telemetry.active.get_mut(&token).unwrap();
            assert_ne!(orig.segment_start_time, new.segment_start_time);
            assert_eq!(orig.start_time, new.start_time);
            // We sometimes run on a rollover boundary, set the value to zero to avoid flakiness
            for seg in &mut new.segments {
                seg.1 = 0;
            }
            let expected = format!("{:#?}\n", &new.segments);
            expect![[r#"
                [
                    (
                        "Start message",
                        0,
                    ),
                    (
                        "update message",
                        0,
                    ),
                ]
            "#]]
            .assert_eq(&expected);
        }
    }
}
