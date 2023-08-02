/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Stderr;
use std::io::Write;
use std::time::Duration;

use codespan_reporting::term::termcolor::Buffer;
use codespan_reporting::term::termcolor::ColorChoice;
use codespan_reporting::term::termcolor::ColorSpec;
use codespan_reporting::term::termcolor::StandardStream;
use codespan_reporting::term::termcolor::WriteColor;
use indicatif::ProgressBar;
use indicatif::ProgressStyle;

pub trait Cli: Write + WriteColor {
    fn progress(&self, len: u64, prefix: &'static str) -> ProgressBar;

    fn spinner(&self, prefix: &'static str) -> ProgressBar;

    fn err(&mut self) -> &mut dyn Write;
}

pub struct Real(StandardStream, Stderr);

impl Default for Real {
    fn default() -> Self {
        Self(
            StandardStream::stdout(ColorChoice::Always),
            std::io::stderr(),
        )
    }
}

impl Cli for Real {
    fn progress(&self, len: u64, prefix: &'static str) -> ProgressBar {
        if len == 1 {
            self.spinner(prefix)
        } else {
            let pb = ProgressBar::new(len);
            pb.set_style(
                ProgressStyle::with_template("  {prefix:25!} {bar} {pos}/{len} {wide_msg}")
                    .expect("BUG: invalid template"),
            );
            pb.set_prefix(prefix);
            pb
        }
    }

    fn spinner(&self, prefix: &'static str) -> ProgressBar {
        let pb = ProgressBar::new_spinner();
        pb.set_style(
            ProgressStyle::with_template("{spinner} {prefix} {wide_msg}")
                .expect("BUG: invalid template"),
        );
        pb.enable_steady_tick(Duration::from_millis(120));
        pb.set_prefix(prefix);
        pb
    }

    fn err(&mut self) -> &mut dyn Write {
        &mut self.1
    }
}

impl Write for Real {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

impl WriteColor for Real {
    fn supports_color(&self) -> bool {
        self.0.supports_color()
    }

    fn set_color(&mut self, spec: &ColorSpec) -> std::io::Result<()> {
        self.0.set_color(spec)
    }

    fn reset(&mut self) -> std::io::Result<()> {
        self.0.reset()
    }
}

pub struct Fake(Buffer, Vec<u8>);

impl Default for Fake {
    fn default() -> Self {
        Self(Buffer::no_color(), Vec::new())
    }
}

impl Fake {
    pub fn to_strings(self) -> (String, String) {
        let stdout = String::from_utf8(self.0.into_inner()).unwrap();
        let stderr = String::from_utf8(self.1).unwrap();
        (stdout, stderr)
    }
}

impl Cli for Fake {
    fn progress(&self, _len: u64, _prefix: &str) -> ProgressBar {
        ProgressBar::hidden()
    }

    fn spinner(&self, _prefix: &str) -> ProgressBar {
        ProgressBar::hidden()
    }

    fn err(&mut self) -> &mut dyn Write {
        &mut self.1
    }
}

impl Write for Fake {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

impl WriteColor for Fake {
    fn supports_color(&self) -> bool {
        self.0.supports_color()
    }

    fn set_color(&mut self, spec: &ColorSpec) -> std::io::Result<()> {
        self.0.set_color(spec)
    }

    fn reset(&mut self) -> std::io::Result<()> {
        self.0.reset()
    }
}
