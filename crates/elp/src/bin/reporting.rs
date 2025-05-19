/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Range;
use std::path::Path;
use std::path::PathBuf;
use std::str;
use std::sync::Arc;
use std::time::Instant;

use anyhow::Context;
use anyhow::Result;
use codespan_reporting::diagnostic::Diagnostic as ReportingDiagnostic;
use codespan_reporting::diagnostic::Label;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::Color;
use codespan_reporting::term::termcolor::ColorSpec;
use elp::arc_types;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp::convert;
use elp::memory_usage::MemoryUsage;
use elp_ide::Analysis;
use elp_ide::TextRange;
use elp_ide::elp_ide_db::EqwalizerDiagnostic;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use indicatif::ProgressBar;
use itertools::Itertools;
use lazy_static::lazy_static;
use parking_lot::Mutex;

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
                Some(s) => format!("{}.\n", s),
                None => "".to_string(),
            };

            let code = format!("{} (See {})", diagnostic.code, diagnostic.uri);
            let msg = format!("{}{}", expr, diagnostic.message);
            let msg_label = Label::primary(reporting_id, range.clone()).with_message(&msg);
            let mut labels = vec![msg_label];
            if let Some(s) = &diagnostic.explanation {
                let explanation_label =
                    Label::secondary(reporting_id, range).with_message(format!("\n\n{}", s));
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
        if self.error_count == 0 {
            self.cli.set_color(&GREEN_COLOR_SPEC)?;
            write!(self.cli, "NO ERRORS")?;
            self.cli.reset()?;
            writeln!(self.cli)?;
        } else {
            self.cli.set_color(&CYAN_COLOR_SPEC)?;
            let noun = if self.error_count == 1 {
                "ERROR"
            } else {
                "ERRORS"
            };
            write!(self.cli, "{} {}", self.error_count, noun)?;
            self.cli.reset()?;
            writeln!(self.cli)?;
        }
        Ok(())
    }

    fn write_stats(&mut self, count: u64, total: u64) -> Result<()> {
        let duration = self.start.elapsed().as_secs();
        self.cli.set_color(&YELLOW_COLOR_SPEC)?;
        if count == total {
            write!(self.cli, "eqWAlized {} module(s) in {}s", count, duration)?;
        } else {
            write!(
                self.cli,
                "eqWAlized {} module(s) ({} cached) in {}s",
                count,
                total - count,
                duration
            )?;
        }
        self.cli.reset()?;
        writeln!(self.cli)?;
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
        let eqwalizer_enabled = self.analysis.is_eqwalizer_enabled(file_id).unwrap();
        let file_path = &self.loaded.vfs.file_path(file_id);
        let root_path = &self
            .analysis
            .project_data(file_id)?
            .with_context(|| "could not find project data")?
            .root_dir;
        let relative_path = get_relative_path(root_path, file_path);
        for diagnostic in diagnostics {
            let diagnostic = convert::eqwalizer_to_arc_diagnostic(
                diagnostic,
                &line_index,
                relative_path,
                eqwalizer_enabled,
            );
            let diagnostic = serde_json::to_string(&diagnostic)?;
            writeln!(self.cli, "{}", diagnostic)?;
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
            );
            let diagnostic = serde_json::to_string(&diagnostic)?;
            writeln!(self.cli, "{}", diagnostic)?;
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
        );
        let diagnostic = serde_json::to_string(&diagnostic)?;
        writeln!(self.cli, "{}", diagnostic)?;
        Ok(())
    }

    fn write_error_count(&mut self) -> Result<()> {
        Ok(())
    }

    fn write_stats(&mut self, _count: u64, _total: u64) -> Result<()> {
        Ok(())
    }

    fn progress(&self, len: u64, prefix: &'static str) -> ProgressBar {
        self.cli.progress(len, prefix)
    }
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

lazy_static! {
    static ref REPORTING_CONFIG: term::Config = {
        let mut config = codespan_reporting::term::Config::default();
        config
            .styles
            .primary_label_error
            .set_fg(Some(Color::Ansi256(9)));
        config.styles.line_number.set_fg(Some(Color::Ansi256(33)));
        config.styles.source_border.set_fg(Some(Color::Ansi256(33)));
        config
    };
    static ref GREEN_COLOR_SPEC: ColorSpec = {
        let mut spec = ColorSpec::default();
        spec.set_fg(Some(Color::Green));
        spec
    };
    static ref CYAN_COLOR_SPEC: ColorSpec = {
        let mut spec = ColorSpec::default();
        spec.set_fg(Some(Color::Cyan));
        spec
    };
    static ref YELLOW_COLOR_SPEC: ColorSpec = {
        let mut spec = ColorSpec::default();
        spec.set_fg(Some(Color::Yellow));
        spec
    };
}

// ---------------------------------------------------------------------

pub(crate) fn dump_stats(cli: &mut dyn Cli, list_modules: bool) {
    let stats = STATS.lock();
    if list_modules {
        writeln!(cli, "--------------start of modules----------").ok();
        stats.iter().sorted().for_each(|stat| {
            writeln!(cli, "{}", stat).ok();
        });
    }
    writeln!(cli, "{} modules processed", stats.len()).ok();
    let mem_usage = MemoryUsage::now();
    writeln!(cli, "{}", mem_usage).ok();
}

lazy_static! {
    static ref STATS: Mutex<Vec<String>> = {
        let stats = Vec::new();
        Mutex::new(stats)
    };
}

pub(crate) fn add_stat(stat: String) {
    let mut stats = STATS.lock();
    stats.push(stat);
}
