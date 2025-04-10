/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Manage Telemetry data for the server.

use std::time::Duration;
use std::time::Instant;

use elp_log::telemetry;
use serde::Serialize;

use crate::memory_usage::Bytes;
use crate::memory_usage::MemoryUsage;

const PERIODIC_INTERVAL_MS: Duration = Duration::from_millis(60_000);

pub(crate) struct TelemetryManager {
    last_periodic_processed: Instant,
}
impl TelemetryManager {
    pub(crate) fn new() -> Self {
        Self {
            last_periodic_processed: Instant::now(),
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
                    log::warn!(
                        "on_periodic: unable to serialize {:?}, err: {}",
                        mem_usage,
                        err
                    );
                }
            };
        }
    }
}
