/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::ops::Range;
use std::path::Path;
use std::path::PathBuf;
use std::str;
use std::sync::Arc;
use std::sync::LazyLock;
use std::time::Instant;

use anyhow::Context;
use anyhow::Result;
use codespan_reporting::diagnostic::Diagnostic as ReportingDiagnostic;
use codespan_reporting::diagnostic::Label;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::Color;
use codespan_reporting::term::termcolor::ColorSpec;
use codespan_reporting::term::termcolor::WriteColor;
use elp::arc_types;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp::convert;
use elp_ide::Analysis;
use elp_ide::AnalysisHost;
use elp_ide::TextRange;
use elp_ide::elp_ide_db::EqwalizerDiagnostic;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_ide::elp_ide_db::memory_usage::MemoryUsage;
use elp_ide::elp_ide_db::memory_usage::memory_usage;
use indicatif::ProgressBar;
use itertools::Itertools;
use parking_lot::Mutex;
use vfs::Vfs;

pub trait Reporter {
    fn write_eqwalizer_diagnostics(
        &mut self,
        file_id: FileId,
        diagnostics: &[EqwalizerDiagnostic],
    ) -> Result<()>;
    fn write_parse_diagnostics(&mut self, diagnostics: &[ParseDiagnostic]) -> Result<()>;
    #[allow(unused)]
    fn write_file_advice(&mut self, file_id: FileId, description: String) -> Result<()>;
    fn write_error_count(&mut self) -> Result<()>;
    fn write_stats(&mut self, count: u64, total: u64) -> Result<()>;

    fn progress(&self, len: u64, prefix: &'static str) -> ProgressBar;
}

#[derive(Debug, Clone)]
pub struct ParseDiagnostic {
    pub file_id: FileId,
    pub relative_path: PathBuf,
    pub line_num: u32,
    pub msg: String,
    pub range: Option<TextRange>,
}

pub struct PrettyReporter<'a> {
    analysis: &'a Analysis,
    loaded: &'a LoadResult,
    cli: &'a mut dyn Cli,
    error_count: usize,
    start: Instant,
}

pub struct JsonReporter<'a> {
    analysis: &'a Analysis,
    loaded: &'a LoadResult,
    cli: &'a mut dyn Cli,
    start: Instant,
}

impl<'a> PrettyReporter<'a> {
    pub fn new(analysis: &'a Analysis, loaded: &'a LoadResult, cli: &'a mut dyn Cli) -> Self {
        Self {
            analysis,
            loaded,
            cli,
            error_count: 0,
            start: Instant::now(),
        }
    }

    fn get_reporting_data(
        &self,
        file_id: FileId,
    ) -> Result<(SimpleFiles<String, Arc<str>>, usize)> {
        let file_path = &self.loaded.vfs.file_path(file_id);
        let root_path = &self
            .analysis
            .project_data(file_id)?
            .with_context(|| "could not find project data")?
            .root_dir;
        let relative_path = get_relative_path(root_path, file_path);
        let content = self.analysis.file_text(file_id)?;
        let mut files: SimpleFiles<String, Arc<str>> = SimpleFiles::new();
        let id = files.add(relative_path.display().to_string(), content);
        Ok((files, id))
    }
}

impl Reporter for PrettyReporter<'_> {
    fn write_eqwalizer_diagnostics(
        &mut self,
        file_id: FileId,
        diagnostics: &[EqwalizerDiagnostic],
    ) -> Result<()> {
        let (reporting_files, reporting_id) = self.get_reporting_data(file_id)?;
        for diagnostic in diagnostics {
            let range: Range<usize> =
                diagnostic.range.start().into()..diagnostic.range.end().into();
            let expr = match &diagnostic.expression {
                Some(s) => format!("{s}.\n"),
                None => "".to_string(),
            };

            let code = format!("{} (See {})", diagnostic.code, diagnostic.uri);
            let msg = format!("{}{}", expr, diagnostic.message);
            let msg_label = Label::primary(reporting_id, range.clone()).with_message(&msg);
            let mut labels = vec![msg_label];
            if let Some(s) = &diagnostic.explanation {
                let explanation_label =
                    Label::secondary(reporting_id, range).with_message(format!("\n\n{s}"));
                labels.push(explanation_label);
            };
            let d: ReportingDiagnostic<usize> = ReportingDiagnostic::error()
                .with_message(code)
                .with_labels(labels);

            term::emit(&mut self.cli, &REPORTING_CONFIG, &reporting_files, &d).unwrap();
        }
        self.error_count += diagnostics.len();
        Ok(())
    }

    fn write_parse_diagnostics(&mut self, diagnostics: &[ParseDiagnostic]) -> Result<()> {
        for diagnostic in diagnostics {
            let range = diagnostic.range.unwrap_or_default();
            let range: Range<usize> = range.start().into()..range.end().into();
            let (reporting_files, reporting_id) = self.get_reporting_data(diagnostic.file_id)?;
            let label = Label::primary(reporting_id, range).with_message(&diagnostic.msg);
            let d: ReportingDiagnostic<usize> = ReportingDiagnostic::error()
                .with_message("parse_error")
                .with_labels(vec![label]);
            term::emit(&mut self.cli, &REPORTING_CONFIG, &reporting_files, &d).unwrap();
        }
        Ok(())
    }

    fn write_file_advice(&mut self, file_id: FileId, description: String) -> Result<()> {
        let (reporting_files, reporting_id) = self.get_reporting_data(file_id)?;
        let label = Label::primary(reporting_id, 1..2).with_message(description);
        let d: ReportingDiagnostic<usize> = ReportingDiagnostic::note()
            .with_message("advice")
            .with_labels(vec![label]);
        term::emit(&mut self.cli, &REPORTING_CONFIG, &reporting_files, &d).unwrap();
        Ok(())
    }

    fn write_error_count(&mut self) -> Result<()> {
        write_error_count_summary(self.cli, self.error_count)
    }

    fn write_stats(&mut self, count: u64, total: u64) -> Result<()> {
        let duration = self.start.elapsed().as_secs();
        self.cli
            .info(&format_eqwalize_stats(count, total, duration))?;
        Ok(())
    }

    fn progress(&self, len: u64, prefix: &'static str) -> ProgressBar {
        self.cli.progress(len, prefix)
    }
}

impl<'a> JsonReporter<'a> {
    pub fn new(analysis: &'a Analysis, loaded: &'a LoadResult, cli: &'a mut dyn Cli) -> Self {
        Self {
            analysis,
            loaded,
            cli,
            start: Instant::now(),
        }
    }
}

impl Reporter for JsonReporter<'_> {
    fn write_eqwalizer_diagnostics(
        &mut self,
        file_id: FileId,
        diagnostics: &[EqwalizerDiagnostic],
    ) -> Result<()> {
        let line_index = self.analysis.line_index(file_id)?;
        let file_path = &self.loaded.vfs.file_path(file_id);
        let root_path = &self
            .analysis
            .project_data(file_id)?
            .with_context(|| "could not find project data")?
            .root_dir;
        let relative_path = get_relative_path(root_path, file_path);
        for diagnostic in diagnostics {
            let diagnostic =
                convert::eqwalizer_to_arc_diagnostic(diagnostic, &line_index, relative_path);
            let diagnostic = serde_json::to_string(&diagnostic)?;
            writeln!(self.cli, "{diagnostic}")?;
        }
        Ok(())
    }

    fn write_parse_diagnostics(&mut self, diagnostics: &[ParseDiagnostic]) -> Result<()> {
        for diagnostic in diagnostics {
            let severity = arc_types::Severity::Error;
            let diagnostic = arc_types::Diagnostic::new(
                diagnostic.relative_path.as_path(),
                diagnostic.line_num,
                None,
                severity,
                "ELP".to_string(),
                diagnostic.msg.clone(),
                None,
                None,
            );
            let diagnostic = serde_json::to_string(&diagnostic)?;
            writeln!(self.cli, "{diagnostic}")?;
        }
        Ok(())
    }

    fn write_file_advice(&mut self, file_id: FileId, description: String) -> Result<()> {
        let file_path = &self.loaded.vfs.file_path(file_id);
        let root_path = &self
            .analysis
            .project_data(file_id)?
            .with_context(|| "could not find project data")?
            .root_dir;
        let relative_path = get_relative_path(root_path, file_path);
        let diagnostic = arc_types::Diagnostic::new(
            relative_path,
            1,
            None,
            arc_types::Severity::Advice,
            "ELP".to_string(),
            description,
            None,
            None,
        );
        let diagnostic = serde_json::to_string(&diagnostic)?;
        writeln!(self.cli, "{diagnostic}")?;
        Ok(())
    }

    fn write_error_count(&mut self) -> Result<()> {
        Ok(())
    }

    fn write_stats(&mut self, count: u64, total: u64) -> Result<()> {
        let duration = self.start.elapsed().as_secs();
        self.cli
            .info(&format_eqwalize_stats(count, total, duration))?;
        Ok(())
    }

    fn progress(&self, len: u64, prefix: &'static str) -> ProgressBar {
        self.cli.progress(len, prefix)
    }
}

/// The eqwalize summary line ("eqWAlized N module(s) ..."), shared by both
/// reporters. It is status, not a result, so it is surfaced via `Cli::info`
/// (stderr on a terminal, an `info` wire message under `--connect`) rather than
/// stdout — keeping `--format json` stdout clean.
fn format_eqwalize_stats(count: u64, total: u64, duration: u64) -> String {
    if count == total {
        format!("eqWAlized {count} module(s) in {duration}s")
    } else {
        format!(
            "eqWAlized {} module(s) ({} cached) in {}s",
            count,
            total - count,
            duration
        )
    }
}

pub fn write_error_count_summary(writer: &mut dyn WriteColor, error_count: usize) -> Result<()> {
    if error_count == 0 {
        writer.set_color(&GREEN_COLOR_SPEC)?;
        write!(writer, "NO ERRORS")?;
        writer.reset()?;
        writeln!(writer)?;
    } else {
        writer.set_color(&CYAN_COLOR_SPEC)?;
        let noun = if error_count == 1 { "ERROR" } else { "ERRORS" };
        write!(writer, "{} {}", error_count, noun)?;
        writer.reset()?;
        writeln!(writer)?;
    }
    Ok(())
}

pub fn format_raw_parse_error(errs: &[ParseDiagnostic]) -> String {
    errs.iter()
        .map(|err| {
            format!(
                "{}:{} {}",
                err.relative_path.display(),
                err.line_num,
                err.msg,
            )
        })
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn get_relative_path<'a>(root: &AbsPath, file: &'a VfsPath) -> &'a Path {
    let file = file.as_path().unwrap();
    match file.strip_prefix(root) {
        Some(relative) => relative.as_ref(),
        None => file.as_ref(),
    }
}

static REPORTING_CONFIG: LazyLock<term::Config> = LazyLock::new(|| {
    let mut config = codespan_reporting::term::Config::default();
    config
        .styles
        .primary_label_error
        .set_fg(Some(Color::Ansi256(9)));
    config.styles.line_number.set_fg(Some(Color::Ansi256(33)));
    config.styles.source_border.set_fg(Some(Color::Ansi256(33)));
    config
});
static GREEN_COLOR_SPEC: LazyLock<ColorSpec> = LazyLock::new(|| {
    let mut spec = ColorSpec::default();
    spec.set_fg(Some(Color::Green));
    spec
});
static CYAN_COLOR_SPEC: LazyLock<ColorSpec> = LazyLock::new(|| {
    let mut spec = ColorSpec::default();
    spec.set_fg(Some(Color::Cyan));
    spec
});

// ---------------------------------------------------------------------

pub(crate) fn dump_stats(cli: &mut dyn Cli, list_modules: bool) {
    let stats = STATS.lock();
    if list_modules {
        writeln!(cli, "--------------start of modules----------").ok();
        stats.iter().sorted().for_each(|stat| {
            writeln!(cli, "{stat}").ok();
        });
    }
    writeln!(cli, "{} modules processed", stats.len()).ok();
    let mem_usage = MemoryUsage::now();
    writeln!(cli, "{mem_usage}").ok();
}

static STATS: Mutex<Vec<String>> = Mutex::new(Vec::new());

pub(crate) fn add_stat(stat: String) {
    let mut stats = STATS.lock();
    stats.push(stat);
}

pub(crate) fn print_memory_usage(
    mut host: AnalysisHost,
    vfs: Vfs,
    cli: &mut dyn Cli,
) -> Result<()> {
    let mem = host.per_query_memory_usage();

    let before = memory_usage();
    drop(vfs);
    let vfs = before.allocated - memory_usage().allocated;

    let before = memory_usage();
    drop(host);
    let unaccounted = before.allocated - memory_usage().allocated;
    let remaining = memory_usage().allocated;

    for (name, bytes, entries) in mem {
        writeln!(cli, "{bytes:>8} {entries:>6} {name}")?;
    }
    writeln!(cli, "{vfs:>8}        VFS")?;
    writeln!(cli, "{unaccounted:>8}        Unaccounted")?;
    writeln!(cli, "{remaining:>8}        Remaining")?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;

    use super::*;

    #[test]
    fn format_eqwalize_stats_all_eqwalized() {
        let expected = "eqWAlized 10 module(s) in 5s".to_string();
        assert_eq_expected!(expected, format_eqwalize_stats(10, 10, 5));
    }

    #[test]
    fn format_eqwalize_stats_with_cached() {
        // 3 of 10 were cached (eqwalized 7).
        let expected = "eqWAlized 7 module(s) (3 cached) in 5s".to_string();
        assert_eq_expected!(expected, format_eqwalize_stats(7, 10, 5));
    }
}
