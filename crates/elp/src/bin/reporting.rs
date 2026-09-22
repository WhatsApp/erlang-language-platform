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
use codespan_reporting::term::Styles;
use codespan_reporting::term::StylesWriter;
use codespan_reporting::term::termcolor::Buffer;
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
use elp_ide::diagnostics;
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

use crate::daemon_protocol::DaemonResponse;
use crate::daemon_protocol::RenderedDiagnostic;

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

pub struct WireReporter<'a> {
    analysis: &'a Analysis,
    loaded: &'a LoadResult,
    cli: &'a mut dyn Cli,
    start: Instant,
    format: WireFormat,
}

#[derive(Clone, Copy)]
enum WireFormat {
    Json,
    Daemon,
}

pub(crate) struct IdeDiagnosticContext<'a> {
    pub analysis: &'a Analysis,
    pub vfs: &'a Vfs,
    pub file_id: FileId,
    pub path: &'a Path,
    pub diagnostic: &'a diagnostics::Diagnostic,
}

pub(crate) fn render_ide_diagnostic(
    context: IdeDiagnosticContext<'_>,
    diagnostic: &arc_types::Diagnostic,
) -> Result<RenderedDiagnostic> {
    let source = context.analysis.file_text(context.file_id)?;
    let mut files = SimpleFiles::new();
    let reporting_id = files.add(context.path.display().to_string(), source);
    let range: Range<usize> =
        context.diagnostic.range.start().into()..context.diagnostic.range.end().into();
    let label =
        Label::primary(reporting_id, range).with_message(context.diagnostic.message.clone());
    let header = match diagnostic.doc_path() {
        Some(uri) => format!("{} (See {})", diagnostic.name(), uri),
        None => diagnostic.name().to_string(),
    };
    let rendered_diagnostic = arc_severity_reporting(diagnostic.severity())
        .with_message(header)
        .with_labels(vec![label]);
    let mut rendered = render_reporting_diagnostic(&files, &rendered_diagnostic)?;
    for line in related_information_lines(
        context.analysis,
        context.vfs,
        context.file_id,
        context.diagnostic,
    )? {
        rendered.plain.push_str(&line);
        rendered.plain.push('\n');
        rendered.ansi.push_str(&line);
        rendered.ansi.push('\n');
    }
    Ok(rendered)
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
}

fn get_reporting_data(
    analysis: &Analysis,
    loaded: &LoadResult,
    file_id: FileId,
) -> Result<(SimpleFiles<String, Arc<str>>, usize)> {
    let file_path = &loaded.vfs.file_path(file_id);
    let root_path = &analysis
        .project_data(file_id)?
        .with_context(|| "could not find project data")?
        .root_dir;
    let relative_path = get_relative_path(root_path, file_path);
    let content = analysis.file_text(file_id)?;
    let mut files: SimpleFiles<String, Arc<str>> = SimpleFiles::new();
    let id = files.add(relative_path.display().to_string(), content);
    Ok((files, id))
}

impl Reporter for PrettyReporter<'_> {
    fn write_eqwalizer_diagnostics(
        &mut self,
        file_id: FileId,
        diagnostics: &[EqwalizerDiagnostic],
    ) -> Result<()> {
        let (reporting_files, reporting_id) =
            get_reporting_data(self.analysis, self.loaded, file_id)?;
        for diagnostic in diagnostics {
            let diagnostic = eqwalizer_reporting_diagnostic(reporting_id, diagnostic);
            emit_reporting_diagnostic(&mut self.cli, &reporting_files, &diagnostic)?;
        }
        self.error_count += diagnostics.len();
        Ok(())
    }

    fn write_parse_diagnostics(&mut self, diagnostics: &[ParseDiagnostic]) -> Result<()> {
        for diagnostic in diagnostics {
            let (reporting_files, reporting_id) =
                get_reporting_data(self.analysis, self.loaded, diagnostic.file_id)?;
            let diagnostic = parse_reporting_diagnostic(reporting_id, diagnostic);
            emit_reporting_diagnostic(&mut self.cli, &reporting_files, &diagnostic)?;
        }
        Ok(())
    }

    fn write_file_advice(&mut self, file_id: FileId, description: String) -> Result<()> {
        let (reporting_files, reporting_id) =
            get_reporting_data(self.analysis, self.loaded, file_id)?;
        let diagnostic = advice_reporting_diagnostic(reporting_id, description);
        emit_reporting_diagnostic(&mut self.cli, &reporting_files, &diagnostic)?;
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

impl<'a> WireReporter<'a> {
    pub fn json(analysis: &'a Analysis, loaded: &'a LoadResult, cli: &'a mut dyn Cli) -> Self {
        Self {
            analysis,
            loaded,
            cli,
            start: Instant::now(),
            format: WireFormat::Json,
        }
    }

    pub fn daemon(analysis: &'a Analysis, loaded: &'a LoadResult, cli: &'a mut dyn Cli) -> Self {
        Self {
            analysis,
            loaded,
            cli,
            start: Instant::now(),
            format: WireFormat::Daemon,
        }
    }

    fn write_diagnostic(
        &mut self,
        diagnostic: arc_types::Diagnostic,
        rendered: Option<RenderedDiagnostic>,
    ) -> Result<()> {
        match self.format {
            WireFormat::Json => {
                writeln!(self.cli, "{}", serde_json::to_string(&diagnostic)?)?;
            }
            WireFormat::Daemon => {
                let response = DaemonResponse::diagnostic(diagnostic, rendered);
                writeln!(self.cli, "{}", serde_json::to_string(&response)?)?;
            }
        }
        Ok(())
    }

    fn reporting_data(&self, file_id: FileId) -> Option<(SimpleFiles<String, Arc<str>>, usize)> {
        match self.format {
            WireFormat::Json => None,
            WireFormat::Daemon => get_reporting_data(self.analysis, self.loaded, file_id)
                .inspect_err(|error| {
                    log::warn!("Failed to prepare daemon diagnostic rendering: {error:#}");
                })
                .ok(),
        }
    }

    fn render_diagnostic(
        reporting_data: Option<&(SimpleFiles<String, Arc<str>>, usize)>,
        diagnostic: impl FnOnce(usize) -> ReportingDiagnostic<usize>,
    ) -> Option<RenderedDiagnostic> {
        let (files, reporting_id) = reporting_data?;
        render_reporting_diagnostic(files, &diagnostic(*reporting_id))
            .inspect_err(|error| {
                log::warn!("Failed to render daemon diagnostic: {error:#}");
            })
            .ok()
    }
}

fn eqwalizer_reporting_diagnostic(
    reporting_id: usize,
    diagnostic: &EqwalizerDiagnostic,
) -> ReportingDiagnostic<usize> {
    let range: Range<usize> = diagnostic.range.start().into()..diagnostic.range.end().into();
    let expression = diagnostic
        .expression
        .as_ref()
        .map(|expression| format!("{expression}.\n"))
        .unwrap_or_default();
    let message = format!("{}{}", expression, diagnostic.message);
    let mut labels = vec![Label::primary(reporting_id, range.clone()).with_message(message)];
    if let Some(explanation) = &diagnostic.explanation {
        labels
            .push(Label::secondary(reporting_id, range).with_message(format!("\n\n{explanation}")));
    }

    ReportingDiagnostic::error()
        .with_message(format!("{} (See {})", diagnostic.code, diagnostic.uri))
        .with_labels(labels)
}

fn parse_reporting_diagnostic(
    reporting_id: usize,
    diagnostic: &ParseDiagnostic,
) -> ReportingDiagnostic<usize> {
    let range = diagnostic.range.unwrap_or_default();
    let range: Range<usize> = range.start().into()..range.end().into();
    let label = Label::primary(reporting_id, range).with_message(&diagnostic.msg);
    ReportingDiagnostic::error()
        .with_message("parse_error")
        .with_labels(vec![label])
}

fn advice_reporting_diagnostic(
    reporting_id: usize,
    description: String,
) -> ReportingDiagnostic<usize> {
    let label = Label::primary(reporting_id, 1..2).with_message(description);
    ReportingDiagnostic::note()
        .with_message("advice")
        .with_labels(vec![label])
}

fn arc_severity_reporting(severity: &arc_types::Severity) -> ReportingDiagnostic<usize> {
    match severity {
        arc_types::Severity::Error => ReportingDiagnostic::error(),
        arc_types::Severity::Warning | arc_types::Severity::Autofix => {
            ReportingDiagnostic::warning()
        }
        arc_types::Severity::Advice | arc_types::Severity::Disabled => ReportingDiagnostic::note(),
    }
}

fn emit_reporting_diagnostic<W: WriteColor>(
    writer: &mut W,
    files: &SimpleFiles<String, Arc<str>>,
    diagnostic: &ReportingDiagnostic<usize>,
) -> Result<()> {
    term::emit_to_write_style(
        &mut StylesWriter::new(writer, &REPORTING_STYLE),
        &REPORTING_CONFIG,
        files,
        diagnostic,
    )?;
    Ok(())
}

fn render_reporting_diagnostic(
    files: &SimpleFiles<String, Arc<str>>,
    diagnostic: &ReportingDiagnostic<usize>,
) -> Result<RenderedDiagnostic> {
    let mut plain = Buffer::no_color();
    emit_reporting_diagnostic(&mut plain, files, diagnostic)?;
    let plain =
        String::from_utf8(plain.into_inner()).context("rendered diagnostic was not UTF-8")?;

    let mut ansi = Buffer::ansi();
    emit_reporting_diagnostic(&mut ansi, files, diagnostic)?;
    let ansi = String::from_utf8(ansi.into_inner()).context("rendered diagnostic was not UTF-8")?;

    Ok(RenderedDiagnostic::new(plain, ansi))
}

impl Reporter for WireReporter<'_> {
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
        let reporting_data = self.reporting_data(file_id);
        for diagnostic in diagnostics {
            let wire_diagnostic =
                convert::eqwalizer_to_arc_diagnostic(diagnostic, &line_index, relative_path);
            let rendered = Self::render_diagnostic(reporting_data.as_ref(), |reporting_id| {
                eqwalizer_reporting_diagnostic(reporting_id, diagnostic)
            });
            self.write_diagnostic(wire_diagnostic, rendered)?;
        }
        Ok(())
    }

    fn write_parse_diagnostics(&mut self, diagnostics: &[ParseDiagnostic]) -> Result<()> {
        for diagnostic in diagnostics {
            let wire_diagnostic = arc_types::Diagnostic::new(
                diagnostic.relative_path.as_path(),
                diagnostic.line_num,
                None,
                arc_types::Severity::Error,
                "ELP".to_string(),
                diagnostic.msg.clone(),
                None,
                None,
            );
            let reporting_data = self.reporting_data(diagnostic.file_id);
            let rendered = Self::render_diagnostic(reporting_data.as_ref(), |reporting_id| {
                parse_reporting_diagnostic(reporting_id, diagnostic)
            });
            self.write_diagnostic(wire_diagnostic, rendered)?;
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
        let wire_diagnostic = arc_types::Diagnostic::new(
            relative_path,
            1,
            None,
            arc_types::Severity::Advice,
            "ELP".to_string(),
            description.clone(),
            None,
            None,
        );
        let reporting_data = self.reporting_data(file_id);
        let rendered = Self::render_diagnostic(reporting_data.as_ref(), |reporting_id| {
            advice_reporting_diagnostic(reporting_id, description)
        });
        self.write_diagnostic(wire_diagnostic, rendered)
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

pub(crate) fn related_information_lines(
    analysis: &Analysis,
    vfs: &Vfs,
    file_id: FileId,
    diagnostic: &diagnostics::Diagnostic,
) -> Result<Vec<String>> {
    diagnostic
        .related_info
        .iter()
        .flatten()
        .map(|info| {
            let line_index = analysis.line_index(info.file_id)?;
            let start = line_index.line_col(info.range.start());
            let end = line_index.line_col(info.range.end());
            let location = if info.file_id == file_id {
                String::new()
            } else if let Some(module_name) = analysis.module_name(info.file_id).ok().flatten() {
                format!("[{}] ", module_name.as_str())
            } else if let Some(project_data) = analysis.project_data(info.file_id).ok().flatten() {
                let path = get_relative_path(&project_data.root_dir, vfs.file_path(info.file_id));
                format!("[{}] ", path.display())
            } else {
                String::new()
            };
            Ok(format!(
                "        {location}{}:{}-{}:{}: {}",
                start.line + 1,
                start.col_utf16 + 1,
                end.line + 1,
                end.col_utf16 + 1,
                info.message
            ))
        })
        .collect()
}

static REPORTING_CONFIG: LazyLock<term::Config> =
    LazyLock::new(codespan_reporting::term::Config::default);
static REPORTING_STYLE: LazyLock<Styles> = LazyLock::new(|| {
    let mut styles = Styles::default();
    styles.primary_label_error.set_fg(Some(Color::Ansi256(9)));
    styles.line_number.set_fg(Some(Color::Ansi256(33)));
    styles.source_border.set_fg(Some(Color::Ansi256(33)));
    styles
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
    fn rendered_diagnostic_preserves_plain_and_ansi_output() {
        let mut files = SimpleFiles::new();
        let file_id = files.add(
            "src/foo.erl".to_string(),
            Arc::<str>::from("foo() -> ok.\n"),
        );
        let diagnostic = ReportingDiagnostic::error()
            .with_message("eqWAlizer: incompatible_types")
            .with_labels(vec![
                Label::primary(file_id, 0..5).with_message("expected integer"),
            ]);

        let rendered = render_reporting_diagnostic(&files, &diagnostic)
            .expect("diagnostic rendering should succeed");

        assert!(rendered.plain.contains("foo() -> ok."));
        assert!(rendered.plain.contains("expected integer"));
        assert!(!rendered.plain.contains("\u{1b}["));
        assert!(rendered.ansi.contains("\u{1b}["));
    }

    #[test]
    fn daemon_render_failure_falls_back_to_unrendered() {
        let reporting_data = (SimpleFiles::new(), 0);
        let rendered = WireReporter::render_diagnostic(Some(&reporting_data), |reporting_id| {
            ReportingDiagnostic::error().with_labels(vec![Label::primary(reporting_id, 0..1)])
        });

        assert!(rendered.is_none());
    }

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
