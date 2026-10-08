/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::io::Write;
use std::time::Duration;

use codespan_reporting::term::termcolor::Buffer;
use codespan_reporting::term::termcolor::Color;
use codespan_reporting::term::termcolor::ColorChoice;
use codespan_reporting::term::termcolor::ColorSpec;
use codespan_reporting::term::termcolor::StandardStream;
use codespan_reporting::term::termcolor::WriteColor;
use indicatif::ProgressBar;
use indicatif::ProgressStyle;

pub trait Cli: Write + WriteColor {
    fn simple_progress(&self, len: u64, prefix: &'static str) -> ProgressBar;

    fn progress(&self, len: u64, prefix: &'static str) -> ProgressBar;

    fn spinner(&self, prefix: &'static str) -> ProgressBar;

    fn err(&mut self) -> &mut dyn Write;

    /// User-facing status that is neither a result nor an error (progress notes,
    /// deprecation warnings, ...). The default writes it plainly to [`err`];
    /// terminal CLIs override this to render it in yellow.
    ///
    /// [`err`]: Cli::err
    fn info(&mut self, message: &str) -> std::io::Result<()> {
        writeln!(self.err(), "{message}")
    }
}

pub struct StandardCli(StandardStream, StandardStream);

impl StandardCli {
    fn new(color_choice: ColorChoice) -> Self {
        Self(
            StandardStream::stdout(color_choice),
            StandardStream::stderr(color_choice),
        )
    }

    fn progress_with_style(
        &self,
        len: u64,
        prefix: &'static str,
        style: &'static str,
    ) -> ProgressBar {
        if len == 1 {
            self.spinner(prefix)
        } else {
            let pb = ProgressBar::new(len);
            pb.set_style(ProgressStyle::with_template(style).expect("BUG: invalid template"));
            pb.set_prefix(prefix);
            pb
        }
    }
}

impl Cli for StandardCli {
    fn progress(&self, len: u64, prefix: &'static str) -> ProgressBar {
        self.progress_with_style(len, prefix, "  {prefix:25!} {bar} {pos}/{len} {wide_msg}")
    }

    fn simple_progress(&self, len: u64, prefix: &'static str) -> ProgressBar {
        self.progress_with_style(len, prefix, "  {prefix:25!} {bar} {wide_msg}")
    }

    fn spinner(&self, prefix: &'static str) -> ProgressBar {
        let pb = ProgressBar::new_spinner();
        pb.set_style(
            ProgressStyle::with_template("{spinner} {prefix} [{elapsed_precise}] {wide_msg}")
                .expect("BUG: invalid template"),
        );
        pb.enable_steady_tick(Duration::from_millis(120));
        pb.set_prefix(prefix);
        pb
    }

    fn err(&mut self) -> &mut dyn Write {
        &mut self.1
    }

    fn info(&mut self, message: &str) -> std::io::Result<()> {
        self.1
            .set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;
        write!(self.1, "{message}")?;
        self.1.reset()?;
        writeln!(self.1)
    }
}

impl Write for StandardCli {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

impl WriteColor for StandardCli {
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

pub struct Real(StandardCli);
pub struct NoColor(StandardCli);

impl Default for Real {
    fn default() -> Self {
        Real(StandardCli::new(ColorChoice::Always))
    }
}

impl Default for NoColor {
    fn default() -> Self {
        NoColor(StandardCli::new(ColorChoice::Never))
    }
}

impl Cli for Real {
    fn progress(&self, len: u64, prefix: &'static str) -> ProgressBar {
        self.0.progress(len, prefix)
    }

    fn simple_progress(&self, len: u64, prefix: &'static str) -> ProgressBar {
        self.0.simple_progress(len, prefix)
    }

    fn spinner(&self, prefix: &'static str) -> ProgressBar {
        self.0.spinner(prefix)
    }

    fn err(&mut self) -> &mut dyn Write {
        self.0.err()
    }

    fn info(&mut self, message: &str) -> std::io::Result<()> {
        self.0.info(message)
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

impl Cli for NoColor {
    fn progress(&self, len: u64, prefix: &'static str) -> ProgressBar {
        self.0.progress(len, prefix)
    }

    fn simple_progress(&self, len: u64, prefix: &'static str) -> ProgressBar {
        self.0.simple_progress(len, prefix)
    }

    fn spinner(&self, prefix: &'static str) -> ProgressBar {
        self.0.spinner(prefix)
    }

    fn err(&mut self) -> &mut dyn Write {
        self.0.err()
    }

    fn info(&mut self, message: &str) -> std::io::Result<()> {
        self.0.info(message)
    }
}

impl Write for NoColor {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

impl WriteColor for NoColor {
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum FakeStream {
    Stdout,
    Stderr,
}

impl FakeStream {
    fn label(self) -> &'static str {
        match self {
            Self::Stdout => "stdout",
            Self::Stderr => "stderr",
        }
    }
}

struct FakeWrite {
    stream: FakeStream,
    bytes: Vec<u8>,
}

#[derive(Default)]
struct FakeCapture {
    writes: Vec<FakeWrite>,
}

impl Write for FakeCapture {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if !buf.is_empty() {
            self.writes.push(FakeWrite {
                stream: FakeStream::Stderr,
                bytes: buf.to_vec(),
            });
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

pub struct Fake {
    stdout: Buffer,
    capture: FakeCapture,
}

impl Default for Fake {
    fn default() -> Self {
        Self {
            stdout: Buffer::no_color(),
            capture: FakeCapture::default(),
        }
    }
}

impl Fake {
    pub fn to_strings(self) -> (String, String) {
        let (stdout, stderr, _) = self.into_parts();
        (stdout, stderr)
    }

    pub fn to_strings_with_tagged(self) -> (String, String, String) {
        let (stdout, stderr, writes) = self.into_parts();
        let tagged = render_tagged_writes(writes);
        (stdout, stderr, tagged)
    }

    fn into_parts(self) -> (String, String, Vec<FakeWrite>) {
        let Self { stdout, capture } = self;
        let writes = capture.writes;
        let stdout = String::from_utf8(stdout.into_inner()).unwrap();
        let stderr = writes
            .iter()
            .filter(|write| write.stream == FakeStream::Stderr)
            .flat_map(|write| write.bytes.iter().copied())
            .collect();
        let stderr = String::from_utf8(stderr).unwrap();
        (stdout, stderr, writes)
    }
}

fn render_tagged_writes(writes: Vec<FakeWrite>) -> String {
    let mut merged: Vec<FakeWrite> = Vec::new();
    for write in writes {
        if let Some(last) = merged.last_mut()
            && last.stream == write.stream
        {
            last.bytes.extend(write.bytes);
        } else {
            merged.push(write);
        }
    }

    let mut output = String::new();
    for write in merged {
        if !output.is_empty() && !output.ends_with('\n') {
            output.push('\n');
        }
        let text = String::from_utf8(write.bytes).unwrap();
        for line in text.split_inclusive('\n') {
            output.push_str(write.stream.label());
            output.push_str(" |");
            if line == "\n" {
                output.push('\n');
            } else {
                output.push(' ');
                output.push_str(line);
            }
        }
    }
    output
}

impl Cli for Fake {
    fn progress(&self, _len: u64, _prefix: &str) -> ProgressBar {
        ProgressBar::hidden()
    }

    fn simple_progress(&self, _len: u64, _prefix: &'static str) -> ProgressBar {
        ProgressBar::hidden()
    }

    fn spinner(&self, _prefix: &str) -> ProgressBar {
        ProgressBar::hidden()
    }

    fn err(&mut self) -> &mut dyn Write {
        &mut self.capture
    }
}

impl Write for Fake {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let written = self.stdout.write(buf)?;
        if written > 0 {
            self.capture.writes.push(FakeWrite {
                stream: FakeStream::Stdout,
                bytes: buf[..written].to_vec(),
            });
        }
        Ok(written)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.stdout.flush()
    }
}

impl WriteColor for Fake {
    fn supports_color(&self) -> bool {
        self.stdout.supports_color()
    }

    fn set_color(&mut self, spec: &ColorSpec) -> std::io::Result<()> {
        self.stdout.set_color(spec)
    }

    fn reset(&mut self) -> std::io::Result<()> {
        self.stdout.reset()
    }
}

#[cfg(test)]
mod tests {
    use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;

    use super::*;

    /// The default `info()` writes to the stderr channel (here the `Fake`
    /// buffer), never to stdout — so `--format json` stdout stays clean.
    #[test]
    fn default_info_goes_to_stderr_not_stdout() {
        let mut cli = Fake::default();
        cli.info("status line").unwrap();
        let (stdout, stderr) = cli.to_strings();

        let expected_stdout = "";
        assert_eq_expected!(expected_stdout, stdout.as_str());
        let expected_stderr = "status line\n";
        assert_eq_expected!(expected_stderr, stderr.as_str());
    }

    #[test]
    fn tagged_output_preserves_order_across_unterminated_stream_writes() {
        let mut cli = Fake::default();
        write!(cli, "first").unwrap();
        cli.info("status").unwrap();
        write!(cli, "second").unwrap();

        let (stdout, stderr, tagged) = cli.to_strings_with_tagged();
        let expected_stdout = "firstsecond";
        assert_eq_expected!(expected_stdout, stdout.as_str());
        let expected_stderr = "status\n";
        assert_eq_expected!(expected_stderr, stderr.as_str());
        let expected_tagged = "\
stdout | first
stderr | status
stdout | second";
        assert_eq_expected!(expected_tagged, tagged.as_str());
    }

    #[test]
    fn tagged_output_preserves_stream_order_and_line_boundaries() {
        let mut cli = Fake::default();
        write!(cli, "out").unwrap();
        assert_eq_expected!(0, cli.err().write(&[]).unwrap());
        writeln!(cli, "put").unwrap();
        cli.info("status").unwrap();
        writeln!(cli, "result").unwrap();
        writeln!(cli.err()).unwrap();

        let (stdout, stderr, tagged) = cli.to_strings_with_tagged();
        let expected_stdout = "output\nresult\n";
        assert_eq_expected!(expected_stdout, stdout.as_str());
        let expected_stderr = "status\n\n";
        assert_eq_expected!(expected_stderr, stderr.as_str());
        let expected_tagged = "\
stdout | output
stderr | status
stdout | result
stderr |
";
        assert_eq_expected!(expected_tagged, tagged.as_str());
    }
}
