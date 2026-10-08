/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::io;
use std::ops::Range;
use std::path::Path;
use std::path::PathBuf;
use std::str;
use std::sync::Arc;
use std::sync::LazyLock;

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

use crate::args::Format;
use crate::daemon_protocol::DaemonResponse;
use crate::daemon_protocol::RenderedDiagnostic;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Destination {
    Human,
    Json,
    Daemon,
}

#[derive(Debug, Clone)]
pub struct ParseDiagnostic {
    pub relative_path: PathBuf,
    pub line_num: u32,
    pub msg: String,
    pub range: Option<TextRange>,
}

pub(crate) struct Report<'a> {
    cli: &'a mut dyn Cli,
    destination: Destination,
    error_count: usize,
    lint_header_written: bool,
}

#[derive(Clone, Copy)]
pub(crate) struct IdeDiagnosticContext<'a> {
    pub analysis: &'a Analysis,
    pub vfs: &'a Vfs,
    pub file_id: FileId,
    pub path: Option<&'a Path>,
    pub use_cli_severity: bool,
    pub arc_patch: bool,
}

type ReportingData = (SimpleFiles<String, Arc<str>>, usize);

impl<'a> Report<'a> {
    pub(crate) fn for_command(cli: &'a mut dyn Cli, format: Option<Format>) -> Self {
        let destination = match format {
            None => Destination::Human,
            Some(Format::Json | Format::ImplicitJson) => Destination::Json,
            Some(Format::Daemon | Format::DaemonJson) => Destination::Daemon,
        };
        Self::new(cli, destination)
    }

    pub(crate) fn for_daemon(cli: &'a mut dyn Cli) -> Self {
        Self::new(cli, Destination::Daemon)
    }

    fn new(cli: &'a mut dyn Cli, destination: Destination) -> Self {
        Self {
            cli,
            destination,
            error_count: 0,
            lint_header_written: false,
        }
    }

    pub(crate) fn info(&mut self, message: &str) -> io::Result<()> {
        self.cli.info(message)
    }

    pub(crate) fn progress(&self, len: u64, prefix: &'static str) -> ProgressBar {
        self.cli.progress(len, prefix)
    }

    pub(crate) fn write_system_stats(
        &mut self,
        host: AnalysisHost,
        vfs: Vfs,
        process_usage: impl fmt::Display,
    ) -> Result<()> {
        for_each_memory_usage_line(host, vfs, |line| {
            self.info(&line.to_string())?;
            Ok(())
        })?;
        self.info(&process_usage.to_string())?;
        Ok(())
    }

    pub(crate) fn human_line(&mut self, args: fmt::Arguments<'_>) -> Result<()> {
        if self.destination == Destination::Human {
            writeln!(self.cli, "{args}")?;
        }
        Ok(())
    }

    pub(crate) fn human_info(&mut self, message: &str) -> Result<()> {
        if self.destination == Destination::Human {
            self.info(message)?;
        }
        Ok(())
    }

    pub(crate) fn write_lint_count(&mut self, name: &str, count: usize) -> Result<()> {
        match self.destination {
            Destination::Human => {
                self.write_lint_header()?;
                writeln!(self.cli, "  {name}: {count}")?;
            }
            Destination::Json | Destination::Daemon => {
                self.info(&format!("  {name}: {count}"))?;
            }
        }
        Ok(())
    }

    pub(crate) fn write_lint_diagnostics(
        &mut self,
        context: IdeDiagnosticContext<'_>,
        diagnostics: &[diagnostics::Diagnostic],
    ) -> Result<()> {
        if diagnostics.is_empty() {
            return Ok(());
        }

        let derived_path = if context.path.is_none() && self.destination != Destination::Human {
            let file_path = context.vfs.file_path(context.file_id);
            let project_data = context
                .analysis
                .project_data(context.file_id)?
                .context("could not find project data")?;
            Some(get_relative_path(&project_data.root_dir, file_path).to_path_buf())
        } else {
            None
        };
        let context = IdeDiagnosticContext {
            path: context.path.or(derived_path.as_deref()),
            ..context
        };

        self.write_lint_header()?;
        for diagnostic in diagnostics {
            let is_error =
                diagnostic.severity(context.use_cli_severity) == diagnostics::Severity::Error;
            self.emit_local(
                is_error,
                || {
                    let path = context.path.context("lint diagnostic path missing")?;
                    let line_index = context.analysis.line_index(context.file_id)?;
                    let mut converted = convert::ide_to_arc_diagnostic(
                        &line_index,
                        path,
                        diagnostic,
                        context.use_cli_severity,
                    );
                    if context.arc_patch
                        && let Ok(file_text) = context.analysis.file_text(context.file_id)
                        && let Some(fix) = convert::extract_arc_fix(
                            diagnostic,
                            &line_index,
                            context.file_id,
                            &file_text,
                        )
                    {
                        converted =
                            converted.with_fix(fix.line, fix.char, fix.original, fix.replacement);
                    }
                    Ok(converted)
                },
                |cli| write_ide_diagnostic(cli, context, diagnostic),
                |converted| Ok(Some(render_ide_diagnostic(context, diagnostic, converted)?)),
            )?;
        }
        Ok(())
    }

    pub(crate) fn write_fix_diagnostic(
        &mut self,
        context: IdeDiagnosticContext<'_>,
        diagnostic: &diagnostics::Diagnostic,
    ) -> Result<()> {
        if self.destination == Destination::Human {
            write_ide_diagnostic(self.cli, context, diagnostic)?;
        }
        Ok(())
    }

    pub(crate) fn write_eqwalizer_diagnostics(
        &mut self,
        analysis: &Analysis,
        loaded: &LoadResult,
        file_id: FileId,
        diagnostics: &[EqwalizerDiagnostic],
    ) -> Result<()> {
        if diagnostics.is_empty() {
            return Ok(());
        }

        let relative_path = get_relative_file_path(analysis, loaded, file_id)?;
        let structured_data = match self.destination {
            Destination::Human => None,
            Destination::Json | Destination::Daemon => {
                Some((analysis.line_index(file_id)?, relative_path.as_path()))
            }
        };
        let reporting_data = self.reporting_data(analysis, file_id, &relative_path)?;
        for diagnostic in diagnostics {
            self.emit_local(
                true,
                || {
                    let (line_index, relative_path) = structured_data
                        .as_ref()
                        .context("structured eqwalizer reporting data missing")?;
                    Ok(convert::eqwalizer_to_arc_diagnostic(
                        diagnostic,
                        line_index,
                        relative_path,
                    ))
                },
                |cli| {
                    let (files, reporting_id) = reporting_data
                        .as_ref()
                        .context("human reporting data missing")?;
                    let diagnostic = eqwalizer_reporting_diagnostic(*reporting_id, diagnostic);
                    emit_reporting_diagnostic(cli, files, &diagnostic)
                },
                |_| {
                    Ok(render_daemon_diagnostic(
                        reporting_data.as_ref(),
                        |reporting_id| eqwalizer_reporting_diagnostic(reporting_id, diagnostic),
                    ))
                },
            )?;
        }
        Ok(())
    }

    pub(crate) fn write_parse_diagnostics(
        &mut self,
        analysis: &Analysis,
        loaded: &LoadResult,
        file_id: FileId,
        diagnostics: &[ParseDiagnostic],
    ) -> Result<()> {
        if diagnostics.is_empty() {
            return Ok(());
        }
        let relative_path = get_relative_file_path(analysis, loaded, file_id)?;
        let reporting_data = self.reporting_data(analysis, file_id, &relative_path)?;
        for diagnostic in diagnostics {
            self.emit_local(
                false,
                || {
                    Ok(arc_types::Diagnostic::new(
                        diagnostic.relative_path.as_path(),
                        diagnostic.line_num,
                        None,
                        arc_types::Severity::Error,
                        "ELP".to_string(),
                        diagnostic.msg.clone(),
                        None,
                        None,
                    ))
                },
                |cli| {
                    let (files, reporting_id) = reporting_data
                        .as_ref()
                        .context("human reporting data missing")?;
                    let diagnostic = parse_reporting_diagnostic(*reporting_id, diagnostic);
                    emit_reporting_diagnostic(cli, files, &diagnostic)
                },
                |_| {
                    Ok(render_daemon_diagnostic(
                        reporting_data.as_ref(),
                        |reporting_id| parse_reporting_diagnostic(reporting_id, diagnostic),
                    ))
                },
            )?;
        }
        Ok(())
    }

    pub(crate) fn write_error_summary(&mut self) -> Result<()> {
        if self.destination == Destination::Human {
            write_error_count_summary(self.cli, self.error_count)?;
        }
        Ok(())
    }

    fn write_lint_header(&mut self) -> Result<()> {
        if self.destination == Destination::Human && !self.lint_header_written {
            writeln!(self.cli, "Diagnostics reported:")?;
            self.lint_header_written = true;
        }
        Ok(())
    }

    fn reporting_data(
        &self,
        analysis: &Analysis,
        file_id: FileId,
        relative_path: &Path,
    ) -> Result<Option<ReportingData>> {
        match self.destination {
            Destination::Human => Ok(Some(get_reporting_data(analysis, file_id, relative_path)?)),
            Destination::Json => Ok(None),
            Destination::Daemon => Ok(get_reporting_data(analysis, file_id, relative_path)
                .inspect_err(|error| {
                    log::warn!("Failed to prepare daemon diagnostic rendering: {error:#}");
                })
                .ok()),
        }
    }

    fn emit_local(
        &mut self,
        is_error: bool,
        diagnostic: impl FnOnce() -> Result<arc_types::Diagnostic>,
        human: impl FnOnce(&mut dyn Cli) -> Result<()>,
        daemon_render: impl FnOnce(&arc_types::Diagnostic) -> Result<Option<RenderedDiagnostic>>,
    ) -> Result<()> {
        match self.destination {
            Destination::Human => human(self.cli)?,
            Destination::Json => {
                let diagnostic = diagnostic()?;
                writeln!(self.cli, "{}", serde_json::to_string(&diagnostic)?)?;
            }
            Destination::Daemon => {
                let diagnostic = diagnostic()?;
                let rendered = daemon_render(&diagnostic)?;
                let response = DaemonResponse::<()>::diagnostic(diagnostic, rendered);
                writeln!(self.cli, "{}", serde_json::to_string(&response)?)?;
            }
        }
        self.error_count += usize::from(is_error);
        Ok(())
    }
}

fn write_ide_diagnostic(
    cli: &mut dyn Cli,
    context: IdeDiagnosticContext<'_>,
    diagnostic: &diagnostics::Diagnostic,
) -> Result<()> {
    let line_index = context.analysis.line_index(context.file_id)?;
    let diagnostic_text = diagnostic.print(&line_index, context.use_cli_severity);
    match context.path {
        Some(path) => writeln!(cli, "{}:{diagnostic_text}", path.display())?,
        None => writeln!(cli, "      {diagnostic_text}")?,
    }
    for line in
        related_information_lines(context.analysis, context.vfs, context.file_id, diagnostic)?
    {
        writeln!(cli, "{line}")?;
    }
    Ok(())
}

fn render_ide_diagnostic(
    context: IdeDiagnosticContext<'_>,
    diagnostic: &diagnostics::Diagnostic,
    converted: &arc_types::Diagnostic,
) -> Result<RenderedDiagnostic> {
    let source = context.analysis.file_text(context.file_id)?;
    let mut files = SimpleFiles::new();
    let path = context.path.context("lint diagnostic path missing")?;
    let reporting_id = files.add(path.display().to_string(), source);
    let range: Range<usize> = diagnostic.range.start().into()..diagnostic.range.end().into();
    let label = Label::primary(reporting_id, range).with_message(diagnostic.message.clone());
    let header = match converted.doc_path() {
        Some(uri) => format!("{} (See {})", converted.name(), uri),
        None => converted.name().to_string(),
    };
    let reporting_diagnostic = arc_severity_reporting(converted.severity())
        .with_message(header)
        .with_labels(vec![label]);
    let mut rendered = render_reporting_diagnostic(&files, &reporting_diagnostic)?;
    for line in
        related_information_lines(context.analysis, context.vfs, context.file_id, diagnostic)?
    {
        rendered.plain.push_str(&line);
        rendered.plain.push('\n');
        rendered.ansi.push_str(&line);
        rendered.ansi.push('\n');
    }
    Ok(rendered)
}

fn render_daemon_diagnostic(
    reporting_data: Option<&ReportingData>,
    diagnostic: impl FnOnce(usize) -> ReportingDiagnostic<usize>,
) -> Option<RenderedDiagnostic> {
    let (files, reporting_id) = reporting_data?;
    render_reporting_diagnostic(files, &diagnostic(*reporting_id))
        .inspect_err(|error| {
            log::warn!("Failed to render daemon diagnostic: {error:#}");
        })
        .ok()
}

fn get_relative_file_path(
    analysis: &Analysis,
    loaded: &LoadResult,
    file_id: FileId,
) -> Result<PathBuf> {
    let file_path = loaded.vfs.file_path(file_id);
    let project_data = analysis
        .project_data(file_id)?
        .context("could not find project data")?;
    Ok(get_relative_path(&project_data.root_dir, file_path).to_path_buf())
}

fn get_reporting_data(
    analysis: &Analysis,
    file_id: FileId,
    relative_path: &Path,
) -> Result<(SimpleFiles<String, Arc<str>>, usize)> {
    let content = analysis.file_text(file_id)?;
    let mut files: SimpleFiles<String, Arc<str>> = SimpleFiles::new();
    let id = files.add(relative_path.display().to_string(), content);
    Ok((files, id))
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

fn arc_severity_reporting(severity: &arc_types::Severity) -> ReportingDiagnostic<usize> {
    match severity {
        arc_types::Severity::Error => ReportingDiagnostic::error(),
        arc_types::Severity::Warning | arc_types::Severity::Autofix => {
            ReportingDiagnostic::warning()
        }
        arc_types::Severity::Advice | arc_types::Severity::Disabled => ReportingDiagnostic::note(),
    }
}

fn emit_reporting_diagnostic<W: WriteColor + ?Sized>(
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

/// The eqwalize summary line ("eqWAlized N module(s) ..."), shared by all
/// reporting modes. It is status, not a result, so it is surfaced via `Cli::info`
/// (stderr on a terminal, an `info` wire message under `--connect`) rather than
/// stdout — keeping `--format json` stdout clean.
pub(crate) fn format_eqwalize_stats(count: u64, total: u64, duration: u64) -> String {
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
    for_each_stat_line(list_modules, |line| {
        writeln!(cli, "{line}").ok();
    });
}

pub(crate) fn dump_stats_report(report: &mut Report<'_>, list_modules: bool) {
    for_each_stat_line(list_modules, |line| {
        report.info(&line.to_string()).ok();
    });
}

fn for_each_stat_line(list_modules: bool, mut write_line: impl for<'a> FnMut(fmt::Arguments<'a>)) {
    let stats = STATS.lock();
    if list_modules {
        write_line(format_args!("--------------start of modules----------"));
        for stat in stats.iter().sorted() {
            write_line(format_args!("{stat}"));
        }
    }
    write_line(format_args!("{} modules processed", stats.len()));
    let mem_usage = MemoryUsage::now();
    write_line(format_args!("{mem_usage}"));
}

static STATS: Mutex<Vec<String>> = Mutex::new(Vec::new());

pub(crate) fn add_stat(stat: String) {
    let mut stats = STATS.lock();
    stats.push(stat);
}

pub(crate) fn print_memory_usage(host: AnalysisHost, vfs: Vfs, cli: &mut dyn Cli) -> Result<()> {
    for_each_memory_usage_line(host, vfs, |line| {
        writeln!(cli, "{line}")?;
        Ok(())
    })
}

fn for_each_memory_usage_line(
    mut host: AnalysisHost,
    vfs: Vfs,
    mut write_line: impl for<'a> FnMut(fmt::Arguments<'a>) -> Result<()>,
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
        write_line(format_args!("{bytes:>8} {entries:>6} {name}"))?;
    }
    write_line(format_args!("{vfs:>8}        VFS"))?;
    write_line(format_args!("{unaccounted:>8}        Unaccounted"))?;
    write_line(format_args!("{remaining:>8}        Remaining"))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use elp::build::fixture;
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
        let rendered = render_daemon_diagnostic(Some(&reporting_data), |reporting_id| {
            ReportingDiagnostic::error().with_labels(vec![Label::primary(reporting_id, 0..1)])
        });

        assert!(rendered.is_none());
    }

    #[test]
    fn standalone_plain_rendering_matches_daemon_plain_rendering() {
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
        let mut standalone = Buffer::no_color();
        emit_reporting_diagnostic(&mut standalone, &files, &diagnostic)
            .expect("standalone diagnostic rendering should succeed");
        let standalone = String::from_utf8(standalone.into_inner())
            .expect("standalone diagnostic should be UTF-8");

        let daemon = render_reporting_diagnostic(&files, &diagnostic)
            .expect("daemon diagnostic rendering should succeed");

        assert_eq_expected!(standalone, daemon.plain);
    }

    #[test]
    fn report_constructors_select_the_destination() {
        let mut cli = elp::cli::Fake::default();
        {
            let report = Report::for_command(&mut cli, None);
            assert!(matches!(report.destination, Destination::Human));
        }

        {
            let report = Report::for_command(&mut cli, Some(Format::Json));
            assert!(matches!(report.destination, Destination::Json));
        }

        {
            let report = Report::for_command(&mut cli, Some(Format::ImplicitJson));
            assert!(matches!(report.destination, Destination::Json));
        }

        {
            let report = Report::for_command(&mut cli, Some(Format::Daemon));
            assert!(matches!(report.destination, Destination::Daemon));
        }

        {
            let report = Report::for_command(&mut cli, Some(Format::DaemonJson));
            assert!(matches!(report.destination, Destination::Daemon));
        }

        let report = Report::for_daemon(&mut cli);
        assert!(matches!(report.destination, Destination::Daemon));
    }

    #[test]
    fn daemon_lint_diagnostic_derives_missing_result_path() {
        let loaded = fixture::load_result(
            r#"
            //- /app_a/src/foo.erl app:app_a
              -module(foo).
              foo() -> ok.
            "#,
        );
        let analysis = loaded.analysis();
        let file_id = analysis
            .module_file_id(loaded.project_id, "foo")
            .expect("module lookup should succeed")
            .expect("fixture module should exist");
        let diagnostic = diagnostics::Diagnostic {
            message: "problem".to_string(),
            ..Default::default()
        };
        let mut cli = elp::cli::Fake::default();

        {
            let mut report = Report::for_daemon(&mut cli);
            report
                .write_lint_diagnostics(
                    IdeDiagnosticContext {
                        analysis: &analysis,
                        vfs: &loaded.vfs,
                        file_id,
                        path: None,
                        use_cli_severity: false,
                        arc_patch: false,
                    },
                    &[diagnostic],
                )
                .expect("daemon diagnostic should derive its path");
        }

        let (stdout, stderr) = cli.to_strings();
        let response: serde_json::Value =
            serde_json::from_str(stdout.trim()).expect("daemon output should be JSON");
        assert_eq_expected!(Some("diagnostic"), response["type"].as_str());
        assert_eq_expected!(
            Some("app_a/src/foo.erl"),
            response["diagnostic"]["path"].as_str()
        );
        assert_eq_expected!("", stderr.as_str());
    }

    #[test]
    fn system_stats_use_info_channel() {
        let loaded = fixture::load_result(
            r#"
            //- /app_a/src/foo.erl app:app_a
              -module(foo).
            "#,
        );
        let (analysis_host, vfs) = loaded.into_parts();
        let mut cli = elp::cli::Fake::default();

        {
            let mut report = Report::for_command(&mut cli, None);
            report
                .write_system_stats(analysis_host, vfs, "process usage")
                .expect("system stats should be reported");
        }

        let (stdout, stderr) = cli.to_strings();
        assert_eq_expected!("", stdout.as_str());
        assert!(stderr.contains("VFS"), "missing VFS stats: {stderr}");
        assert!(
            stderr.contains("process usage"),
            "missing process stats: {stderr}"
        );
    }

    #[test]
    fn stats_report_does_not_write_to_structured_stdout() {
        let mut cli = elp::cli::Fake::default();
        {
            let mut report = Report::for_command(&mut cli, Some(Format::Json));
            dump_stats_report(&mut report, false);
        }
        let (stdout, stderr) = cli.to_strings();

        assert!(stdout.is_empty(), "stats should not be written to stdout");
        assert!(
            stderr.contains("modules processed"),
            "stats should be written to stderr"
        );
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
