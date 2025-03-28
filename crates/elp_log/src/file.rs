/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs::File;
use std::io;
use std::io::BufWriter;
use std::io::Write;
use std::time::SystemTime;

use humantime::format_rfc3339_millis;
use log::Record;
use parking_lot::Mutex;

use crate::Builder;
use crate::Filter;
use crate::ReconfigureLog;

/// Simple logger that logs either to stderr or to a file, using `env_logger`
/// filter syntax. Amusingly, there's no crates.io crate that can do this and
/// only this.
///
/// Taken from https://github.com/rust-lang/rust-analyzer/blob/81a9ad3672d547b2f5d265766bbb6c79909fb2da/crates/rust-analyzer/src/bin/logger.rs
pub struct FileLogger {
    filter: Filter,
    file: Option<Mutex<BufWriter<File>>>,
    no_buffering: bool,
}

impl FileLogger {
    pub fn new(log_file: Option<File>, no_buffering: bool, filter: Option<&str>) -> Self {
        let filter = {
            let mut builder = Builder::new();
            if let Some(filter) = filter {
                builder.parse(filter);
            }
            builder.build()
        };

        let file = log_file.map(|it| Mutex::new(BufWriter::new(it)));

        Self {
            filter,
            file,
            no_buffering,
        }
    }
}

impl ReconfigureLog for FileLogger {
    fn flush(&self) {
        match &self.file {
            Some(w) => {
                let _ = w.lock().flush();
            }
            None => {
                let _ = io::stderr().flush();
            }
        }
    }

    fn filter(&self) -> &Filter {
        &self.filter
    }

    fn reconfigure(&mut self, mut filter: Builder) {
        self.filter = filter.build();
    }

    fn write(&self, record: &Record) {
        let now = SystemTime::now();
        let formatted = format!(
            "[{}] [{} {}] {}",
            format_rfc3339_millis(now),
            record.level(),
            record.target(),
            record.args()
        );
        let should_flush = match &self.file {
            Some(w) => {
                let _ = writeln!(w.lock(), "{}", formatted);
                self.no_buffering
            }
            None => {
                eprintln!("{}", formatted);
                true // flush stderr unconditionally
            }
        };

        if should_flush {
            self.flush();
        }
    }
}
