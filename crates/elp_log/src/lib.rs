/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

pub use env_logger::filter::Builder;
pub use env_logger::filter::Filter;
use fxhash::FxHashMap;
use log::LevelFilter;
use log::Log;
use log::Metadata;
use log::Record;
use parking_lot::RwLock;

mod file;
pub mod telemetry;

pub use file::FileLogger;
use serde::Serialize;

use crate::telemetry::send_with_duration;

/// Trait similar to Log, but supports direct re-configuration through
/// env_logger-like filter
pub trait ReconfigureLog: Send + Sync {
    fn filter(&self) -> &Filter;

    fn reconfigure(&mut self, filter: Builder);

    fn write(&self, record: &Record);

    fn flush(&self);
}

#[derive(Default)]
struct Shared {
    writers: FxHashMap<String, Box<dyn ReconfigureLog>>,
}

impl Shared {
    fn max_level(&self) -> LevelFilter {
        self.writers
            .values()
            .map(|logger| logger.filter().filter())
            .max()
            .unwrap_or(LevelFilter::Off)
    }
}

/// Re-configurable logger writer
#[derive(Default, Clone)]
pub struct Logger {
    shared: Arc<RwLock<Shared>>,
}

impl Logger {
    pub fn register_logger<S: Into<String>>(&self, name: S, logger: Box<dyn ReconfigureLog>) {
        let mut shared = self.shared.write();
        shared.writers.insert(name.into(), logger);

        log::set_max_level(shared.max_level());
    }

    pub fn reconfigure(&self, name: &str, filter: Builder) {
        let mut shared = self.shared.write();

        if let Some(logger) = shared.writers.get_mut(name) {
            logger.reconfigure(filter);
        }

        log::set_max_level(shared.max_level());
    }

    pub fn install(&self) {
        let max_level = self.shared.read().max_level();

        let _ =
            log::set_boxed_logger(Box::new(self.clone())).map(|()| log::set_max_level(max_level));
    }
}

impl Log for Logger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        self.shared
            .read()
            .writers
            .values()
            .any(|logger| logger.filter().enabled(metadata))
    }

    fn log(&self, record: &Record) {
        for logger in self.shared.read().writers.values() {
            if logger.filter().matches(record) {
                logger.write(record);
            }
        }
    }

    fn flush(&self) {
        for logger in self.shared.read().writers.values() {
            logger.flush();
        }
    }
}

/// Usage: `let _timer = timeit!(displayable&serializable)`
/// Logs elapsed time at INFO level when `drop`d.
/// **Always** use a named variable
/// so `drop` happens at the end of the function
#[macro_export]
macro_rules! timeit {
    ($display:expr) => {
        $crate::TimeIt::new(module_path!(), $display, $crate::Telemetry::No)
    };
    ($($arg:tt)+) => {
        $crate::TimeIt::new(module_path!(), format!($($arg)+), $crate::Telemetry::No)
    };
}

/// Usage: `let _timer = timeit_with_telemetry!(displayable&serializable)`
/// Same as timeit!, but also send a LSP telemetry/event
#[macro_export]
macro_rules! timeit_with_telemetry {
    ($display:expr) => {
        $crate::TimeIt::new(module_path!(), $display, $crate::Telemetry::Always)
    };
    ($($arg:tt)+) => {
        $crate::TimeIt::new(module_path!(), format!($($arg)+), $crate::Telemetry::Always)
    };
}

/// Usage: `let _timer = timeit_slow!("metric_name", Duration::from_millis(300))`
/// Same as timeit_with_telemetry!, but do work only if latency is more than configured value
#[macro_export]
macro_rules! timeit_exceeds {
    ($display:expr, $duration:expr) => {
        $crate::TimeIt::new(
            module_path!(),
            $display,
            $crate::Telemetry::Exceeds($duration),
        )
    };
}

#[derive(Debug)]
pub enum Telemetry {
    No,
    Always,
    Exceeds(Duration),
}

// inspired by rust-analyzer `timeit`
// https://github.com/rust-lang/rust-analyzer/blob/65a1538/crates/stdx/src/lib.rs#L18
/// Logs the elapsed time when `drop`d
#[must_use = "logs the elapsed time when `drop`d"]
#[derive(Debug)]
pub struct TimeIt<T = String>
where
    T: Display,
    T: Serialize,
    T: Clone,
    T: Debug,
{
    data: T,
    module_path: &'static str,
    instant: Option<Instant>,
    start_time: SystemTime,
    telemetry: Telemetry,
}

impl<T> TimeIt<T>
where
    T: Display,
    T: Serialize,
    T: Clone,
    T: Debug,
{
    pub fn new(module_path: &'static str, data: T, telemetry: Telemetry) -> Self {
        TimeIt {
            data,
            module_path,
            instant: Some(Instant::now()),
            start_time: SystemTime::now(),
            telemetry,
        }
    }

    fn send_with_duration(&self, duration_ms: u32, start_time: SystemTime) {
        match serde_json::to_value(self.data.clone()) {
            Ok(value) => {
                send_with_duration(String::from("telemetry"), value, duration_ms, start_time)
            }
            Err(err) => log::warn!(
                "Error serializing telemetry data. data: {}, err: {}",
                self.data,
                err
            ),
        };
    }

    fn log(&self, duration_ms: u32) {
        log::info!(
            target: self.module_path,
            "timeit '{}': {}ms",
            self.data.clone(),
            duration_ms
        );
    }
}

impl<T> Drop for TimeIt<T>
where
    T: Display,
    T: Serialize,
    T: Clone,
    T: Debug,
{
    fn drop(&mut self) {
        if let Some(instant) = self.instant.take() {
            let duration_ms = instant.elapsed().as_millis() as u32;
            match self.telemetry {
                Telemetry::No => self.log(duration_ms),
                Telemetry::Always => {
                    self.log(duration_ms);
                    self.send_with_duration(duration_ms, self.start_time);
                }
                Telemetry::Exceeds(threshold) => {
                    if duration_ms > threshold.as_millis() as u32 {
                        self.log(duration_ms);
                        self.send_with_duration(duration_ms, self.start_time)
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::io::Read;

    use tempfile::NamedTempFile;

    use super::*;

    #[test]
    fn it_works() {
        let mut file = NamedTempFile::new().unwrap();
        let log_file = file.reopen().unwrap();

        let file_logger = FileLogger::new(Some(log_file), true, None);

        let logger = Logger::default();
        logger.register_logger("test", Box::new(file_logger));
        logger.install();

        log::error!("This will be logged!");
        log::trace!("This won't be logged!");

        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        // When executing this test via buck2 the crate name is changed as part
        // of the unittest rule generated.  This ensures we are compatible with
        // both buck2 and cargo.
        let name = if env::var_os("BUCK2_DAEMON_UUID").is_some() {
            "elp_log_unittest"
        } else {
            "elp_log"
        };
        assert_eq!(format!("[ERROR {name}::tests] This will be logged!\n"), buf);
    }
}
