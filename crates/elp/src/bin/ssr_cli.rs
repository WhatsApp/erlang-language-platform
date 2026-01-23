/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fs;
use std::path::Path;
use std::str;
use std::thread;
use std::time::SystemTime;

use anyhow::Result;
use anyhow::bail;
use crossbeam_channel::unbounded;
use elp::build::load;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp::convert;
use elp::memory_usage::MemoryUsage;
use elp::otp_file_to_ignore;
use elp_eqwalizer::Mode;
use elp_ide::Analysis;
use elp_ide::AnalysisHost;
use elp_ide::diagnostics;
use elp_ide::diagnostics::DiagnosticsConfig;
use elp_ide::diagnostics::LintConfig;
use elp_ide::diagnostics::LintsFromConfig;
use elp_ide::diagnostics::MatchSsr;
use elp_ide::elp_ide_db::LineCol;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::ModuleName;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_log::telemetry;
use elp_project_model::AppName;
use elp_project_model::AppType;
use elp_project_model::DiscoverConfig;
use elp_project_model::buck::BuckQueryConfig;
use hir::Semantic;
use paths::Utf8PathBuf;
use rayon::prelude::ParallelBridge;
use rayon::prelude::ParallelIterator;

use crate::args::Ssr;
use crate::reporting;
use crate::reporting::print_memory_usage;

fn normalize_ssr_pattern(pattern: &str) -> String {
    if pattern.starts_with("ssr:") {
        pattern.to_string()
    } else {
        format!("ssr: {}.", pattern)
    }
}

pub fn run_ssr_command(
    args: &Ssr,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
    use_color: bool,
) -> Result<()> {
    if args.include_tests {
        writeln!(
            cli.err(),
            "Warning: --include-tests is deprecated, has no effect, and will be removed in a future release."
        )?;
    }
    let start_time = SystemTime::now();
    let memory_start = MemoryUsage::now();

    // Validate all SSR patterns early
    let analysis_host = AnalysisHost::default();
    let analysis = analysis_host.analysis();
    for pattern in &args.ssr_specs {
        let normalized_pattern = normalize_ssr_pattern(pattern);
        match analysis.validate_ssr_pattern(&normalized_pattern) {
            Ok(Ok(())) => {}
            Ok(Err(e)) => bail!("invalid SSR pattern '{}': {}", pattern, e),
            Err(_cancelled) => bail!("SSR pattern validation was cancelled"),
        }
    }

    // Parse the strategy from CLI arguments
    let strategy = args.parse_strategy()?;

    // Create the lint config with all SSR patterns
    let mut lint_config = LintConfig::default();
    for pattern in &args.ssr_specs {
        let normalized_pattern = normalize_ssr_pattern(pattern);
        let severity = if args.dump_config {
            // Set the severity so that squiggles are shown in the VS Code UI
            Some(diagnostics::Severity::Information)
        } else {
            None
        };
        let ssr_lint = diagnostics::Lint::LintMatchSsr(MatchSsr {
            ssr_pattern: normalized_pattern,
            message: None,
            strategy: Some(strategy),
            severity,
        });
        lint_config.ad_hoc_lints.lints.push(ssr_lint);
    }

    // Build the diagnostics config
    let diagnostics_config = DiagnosticsConfig::default()
        .configure_diagnostics(&lint_config, &Some("ad-hoc: ssr-match".to_string()), &None)?
        .set_include_generated(args.include_generated)
        .set_experimental(false)
        .set_use_cli_severity(false);

    if args.dump_config {
        let result = toml::to_string::<LintsFromConfig>(&diagnostics_config.lints_from_config)?;
        // This is a subsection of .elp_lint.toml, add subsection prefix
        let result = result.replace("[[lints]]", "[[ad_hoc_lints.lints]]");
        writeln!(cli, "\n# Add this to your .elp_lint.toml")?;
        writeln!(cli, "{}", result)?;
        return Ok(());
    }

    // Load the project
    let mut loaded = load_project(args, cli, query_config)?;
    telemetry::report_elapsed_time("ssr operational", start_time);

    let r = run_ssr(cli, &mut loaded, &diagnostics_config, args, use_color);

    telemetry::report_elapsed_time("ssr done", start_time);

    let memory_end = MemoryUsage::now();
    let memory_used = memory_end - memory_start;

    // Print memory usage at the end if requested and format is normal
    if args.is_format_normal() && args.report_system_stats {
        print_memory_usage(loaded.analysis_host, loaded.vfs, cli)?;
        writeln!(cli, "{}", memory_used)?;
    }
    r
}

pub fn run_ssr(
    cli: &mut dyn Cli,
    loaded: &mut LoadResult,
    diagnostics_config: &DiagnosticsConfig,
    args: &Ssr,
    use_color: bool,
) -> Result<()> {
    let analysis = loaded.analysis();
    let (file_id, name) = match &args.module {
        Some(module) => match analysis.module_file_id(loaded.project_id, module)? {
            Some(file_id) => {
                if args.is_format_normal() {
                    writeln!(cli, "module specified: {module}")?;
                }
                (Some(file_id), analysis.module_name(file_id)?)
            }
            None => panic!("Module not found: {module}"),
        },
        None => match &args.file {
            Some(file_name) => {
                if args.is_format_normal() {
                    writeln!(cli, "file specified: {file_name}")?;
                }
                let path_buf = Utf8PathBuf::from_path_buf(fs::canonicalize(file_name).unwrap())
                    .expect("UTF8 conversion failed");
                let path = AbsPath::assert(&path_buf);
                let path = path.as_os_str().to_str().unwrap();
                (
                    loaded
                        .vfs
                        .file_id(&VfsPath::new_real_path(path.to_string()))
                        .map(|(id, _)| id),
                    path_buf.as_path().file_name().map(ModuleName::new),
                )
            }
            None => (None, None),
        },
    };

    let mut match_count = 0;

    match (file_id, name) {
        (None, _) => {
            // Streaming case: process all modules
            let project_id = loaded.project_id;
            do_parse_all_streaming(
                cli,
                &analysis,
                &project_id,
                diagnostics_config,
                args,
                use_color,
                loaded,
                &mut match_count,
            )?;
        }
        (Some(file_id), Some(name)) => {
            if let Some(app) = &args.app
                && let Ok(Some(file_app)) = analysis.file_app_name(file_id)
                && file_app != AppName(app.to_string())
            {
                panic!("Module {} does not belong to app {}", name.as_str(), app)
            }
            if let Some(diag) = do_parse_one(&analysis, diagnostics_config, file_id, &name, args)? {
                match_count = 1;
                print_single_result(cli, loaded, &diag, args, use_color)?;
            }
        }
        (Some(file_id), _) => {
            panic!("Could not get name from file_id for {file_id:?}")
        }
    };

    if match_count == 0 {
        if args.is_format_normal() {
            writeln!(cli, "No matches found")?;
        }
    } else if args.is_format_normal() {
        writeln!(cli, "\nMatches found in {} modules", match_count)?;
    }

    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn do_parse_all_streaming(
    cli: &mut dyn Cli,
    analysis: &Analysis,
    project_id: &ProjectId,
    config: &DiagnosticsConfig,
    args: &Ssr,
    use_color: bool,
    loaded: &mut LoadResult,
    match_count: &mut usize,
) -> Result<()> {
    let module_index = analysis.module_index(*project_id).unwrap();
    let app_name = args.app.as_ref().map(|name| AppName(name.to_string()));

    // Create a channel for streaming results
    let (tx, rx) = unbounded();

    // Spawn a thread to process modules in parallel and send results
    let analysis_clone = analysis.clone();
    let config_clone = config.clone();
    let args_clone = args.clone();

    // Collect modules into an owned vector
    let modules: Vec<_> = module_index
        .iter_own()
        .map(|(name, source, file_id)| (name.as_str().to_string(), source, file_id))
        .collect();

    thread::spawn(move || {
        modules
            .into_iter()
            .par_bridge()
            .map_with(
                (analysis_clone, tx),
                |(db, tx), (module_name, _file_source, file_id)| {
                    if !otp_file_to_ignore(db, file_id)
                        && db.file_app_type(file_id).ok() != Some(Some(AppType::Dep))
                        && (app_name.is_none()
                            || db.file_app_name(file_id).ok().as_ref() == Some(&app_name))
                        && let Ok(Some(result)) =
                            do_parse_one(db, &config_clone, file_id, &module_name, &args_clone)
                    {
                        // Send result through channel
                        let _ = tx.send(result);
                    }
                },
            )
            .for_each(|_| {}); // Consume the iterator
        // Channel is dropped here, signaling end of results
    });

    // Process and print results as they arrive from the channel
    for result in rx {
        *match_count += 1;
        print_single_result(cli, loaded, &result, args, use_color)?;
    }

    Ok(())
}

fn print_single_result(
    cli: &mut dyn Cli,
    loaded: &mut LoadResult,
    result: &(String, FileId, Vec<diagnostics::Diagnostic>),
    args: &Ssr,
    use_color: bool,
) -> Result<()> {
    let (name, file_id, diags) = result;

    if args.is_format_json() {
        for diag in diags {
            let vfs_path = loaded.vfs.file_path(*file_id);
            let analysis = loaded.analysis();
            let root_path = &analysis
                .project_data(*file_id)
                .unwrap_or_else(|_err| panic!("could not find project data"))
                .unwrap_or_else(|| panic!("could not find project data"))
                .root_dir;
            let relative_path = reporting::get_relative_path(root_path, vfs_path);
            print_diagnostic_json(diag, &analysis, *file_id, relative_path, false, cli)?;
        }
    } else {
        writeln!(cli, "  {}: {}", name, diags.len())?;

        // Determine if we should show source context
        let show_source = args.show_source
            || args.before_context.is_some()
            || args.after_context.is_some()
            || args.context.is_some()
            || args.group_separator.is_some()
            || args.no_group_separator;
        let (before_lines, after_lines) = calculate_context_lines(args);
        let has_context = before_lines > 0 || after_lines > 0;
        let group_separator = should_show_group_separator(args, has_context && show_source);

        for (idx, diag) in diags.iter().enumerate() {
            // Print group separator before each match (except the first) if showing source with context
            if show_source
                && idx > 0
                && let Some(ref sep) = group_separator
            {
                writeln!(cli, "{}", sep)?;
            }
            // Get relative path for diagnostic output
            let vfs_path = loaded.vfs.file_path(*file_id);
            let analysis = loaded.analysis();
            let root_path = &analysis
                .project_data(*file_id)
                .unwrap_or_else(|_err| panic!("could not find project data"))
                .unwrap_or_else(|| panic!("could not find project data"))
                .root_dir;
            let relative_path = reporting::get_relative_path(root_path, vfs_path);

            // Only show path when showing source context
            let path_to_show = if show_source {
                Some(relative_path)
            } else {
                None
            };
            print_diagnostic(diag, &loaded.analysis(), *file_id, path_to_show, false, cli)?;

            // Only show source context if --show-source or --show-source-markers is set
            if show_source {
                if use_color {
                    print_source_with_context(
                        diag,
                        &loaded.analysis(),
                        *file_id,
                        before_lines,
                        after_lines,
                        true,
                        cli,
                    )?;
                } else {
                    print_source_with_context_markers(
                        diag,
                        &loaded.analysis(),
                        *file_id,
                        before_lines,
                        after_lines,
                        cli,
                    )?;
                }
                writeln!(cli)?;
            }
        }
    }
    Ok(())
}

fn load_project(
    args: &Ssr,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
) -> Result<LoadResult> {
    log::info!("Loading project at: {:?}", args.project);
    let config = DiscoverConfig::new(args.rebar, &args.profile);
    load::load_project_at(
        cli,
        &args.project,
        config,
        IncludeOtp::Yes,
        Mode::Server,
        query_config,
    )
}
fn do_parse_one(
    db: &Analysis,
    config: &DiagnosticsConfig,
    file_id: FileId,
    name: &str,
    args: &Ssr,
) -> Result<Option<(String, FileId, Vec<diagnostics::Diagnostic>)>> {
    if !args.include_generated && db.is_generated(file_id)? {
        return Ok(None);
    }

    // Run only the SSR lint configured in lints_from_config
    let diagnostics = db.with_db(|database| {
        let sema = Semantic::new(database);
        let mut diags = Vec::new();
        config
            .lints_from_config
            .get_diagnostics(&mut diags, &sema, file_id);
        diags
    })?;

    if !diagnostics.is_empty() {
        let res = (name.to_string(), file_id, diagnostics);
        Ok(Some(res))
    } else {
        Ok(None)
    }
}

fn print_diagnostic(
    diag: &diagnostics::Diagnostic,
    analysis: &Analysis,
    file_id: FileId,
    path: Option<&Path>,
    use_cli_severity: bool,
    cli: &mut dyn Cli,
) -> Result<(), anyhow::Error> {
    let line_index = analysis.line_index(file_id)?;
    let diag_str = diag.print(&line_index, use_cli_severity);
    if let Some(path) = path {
        writeln!(cli, "{}:{}", path.display(), diag_str)?;
    } else {
        writeln!(cli, "      {}", diag_str)?;
    }
    Ok(())
}

fn print_diagnostic_json(
    diagnostic: &diagnostics::Diagnostic,
    analysis: &Analysis,
    file_id: FileId,
    path: &Path,
    use_cli_severity: bool,
    cli: &mut dyn Cli,
) -> Result<(), anyhow::Error> {
    let line_index = analysis.line_index(file_id)?;
    let converted_diagnostic =
        convert::ide_to_arc_diagnostic(&line_index, path, diagnostic, use_cli_severity);
    writeln!(
        cli,
        "{}",
        serde_json::to_string(&converted_diagnostic).unwrap_or_else(|err| panic!(
            "print_diagnostics_json failed for '{converted_diagnostic:?}': {err}"
        ))
    )?;
    Ok(())
}

/// Print a line with color highlighting
fn print_line_with_color(
    line_num: usize,
    line_content: &str,
    is_match_line: bool,
    start: &LineCol,
    end: &LineCol,
    current_line: u32,
    cli: &mut dyn Cli,
) -> Result<(), anyhow::Error> {
    // Line number in gray
    write!(cli, "\x1b[90m{:4} |\x1b[0m ", line_num)?;

    if !is_match_line {
        // Non-match line: print normally
        writeln!(cli, "{}", line_content)?;
    } else {
        // Match line: highlight the matched portion
        if current_line == start.line && current_line == end.line {
            // Single-line match
            let start_col = start.col_utf16 as usize;
            let end_col = end.col_utf16 as usize;

            let before = &line_content[..start_col.min(line_content.len())];
            let matched =
                &line_content[start_col.min(line_content.len())..end_col.min(line_content.len())];
            let after = &line_content[end_col.min(line_content.len())..];

            write!(cli, "{}", before)?;
            write!(cli, "\x1b[91;1m{}\x1b[0m", matched)?; // Red bold
            writeln!(cli, "{}", after)?;
        } else if current_line == start.line {
            // First line of multi-line match
            let start_col = start.col_utf16 as usize;
            let before = &line_content[..start_col.min(line_content.len())];
            let matched = &line_content[start_col.min(line_content.len())..];

            write!(cli, "{}", before)?;
            writeln!(cli, "\x1b[91;1m{}\x1b[0m", matched)?; // Red bold
        } else if current_line == end.line {
            // Last line of multi-line match
            let end_col = end.col_utf16 as usize;
            let matched = &line_content[..end_col.min(line_content.len())];
            let after = &line_content[end_col.min(line_content.len())..];

            write!(cli, "\x1b[91;1m{}\x1b[0m", matched)?; // Red bold
            writeln!(cli, "{}", after)?;
        } else {
            // Middle line of multi-line match
            writeln!(cli, "\x1b[91;1m{}\x1b[0m", line_content)?; // Red bold
        }
    }

    Ok(())
}

/// Calculate context lines from the new grep-style arguments
fn calculate_context_lines(args: &Ssr) -> (usize, usize) {
    // -C/--context takes precedence and sets both before and after
    if let Some(context) = args.context {
        return (context, context);
    }

    // Otherwise use individual before/after values, defaulting to 0
    let before = args.before_context.unwrap_or(0);
    let after = args.after_context.unwrap_or(0);
    (before, after)
}

/// Determine if a group separator should be shown
fn should_show_group_separator(args: &Ssr, has_context: bool) -> Option<String> {
    // If --no-group-separator is set, don't show separator
    if args.no_group_separator {
        return None;
    }

    // Only show separators if there's context to separate
    if !has_context {
        return None;
    }

    // Use custom separator if provided, otherwise default to "--"
    Some(
        args.group_separator
            .clone()
            .unwrap_or_else(|| "--".to_string()),
    )
}

/// Print source code context with the specified before/after context lines
fn print_source_with_context(
    diag: &diagnostics::Diagnostic,
    analysis: &Analysis,
    file_id: FileId,
    before_lines: usize,
    after_lines: usize,
    use_color: bool,
    cli: &mut dyn Cli,
) -> Result<(), anyhow::Error> {
    let line_index = analysis.line_index(file_id)?;
    let source = &analysis.file_text(file_id)?;

    let range = diag.range;
    let start = line_index.line_col(range.start());
    let end = line_index.line_col(range.end());

    let lines: Vec<&str> = source.lines().collect();
    let total_lines = lines.len();

    // Calculate the range of lines to display
    let first_line = start.line.saturating_sub(before_lines as u32) as usize;
    let last_line = ((end.line + after_lines as u32 + 1) as usize).min(total_lines);

    // Display the source context
    for line_idx in first_line..last_line {
        let line_num = line_idx + 1;
        let line_content = lines.get(line_idx).unwrap_or(&"");

        // Check if this line contains part of the match
        let is_match_line = line_idx >= start.line as usize && line_idx <= end.line as usize;

        if use_color {
            print_line_with_color(
                line_num,
                line_content,
                is_match_line,
                &start,
                &end,
                line_idx as u32,
                cli,
            )?;
        } else {
            // Just print the line without any highlighting
            write!(cli, "{:4} | ", line_num)?;
            writeln!(cli, "{}", line_content)?;
        }
    }

    Ok(())
}

/// Print source code context with text markers
fn print_source_with_context_markers(
    diag: &diagnostics::Diagnostic,
    analysis: &Analysis,
    file_id: FileId,
    before_lines: usize,
    after_lines: usize,
    cli: &mut dyn Cli,
) -> Result<(), anyhow::Error> {
    let line_index = analysis.line_index(file_id)?;
    let source = &analysis.file_text(file_id)?;

    let range = diag.range;
    let start = line_index.line_col(range.start());
    let end = line_index.line_col(range.end());

    let lines: Vec<&str> = source.lines().collect();
    let total_lines = lines.len();

    // Calculate the range of lines to display
    let first_line = start.line.saturating_sub(before_lines as u32) as usize;
    let last_line = ((end.line + after_lines as u32 + 1) as usize).min(total_lines);

    // Display the source context
    for line_idx in first_line..last_line {
        let line_num = line_idx + 1;
        let line_content = lines.get(line_idx).unwrap_or(&"");

        // Check if this line contains part of the match
        let is_match_line = line_idx >= start.line as usize && line_idx <= end.line as usize;

        print_line_with_markers(
            line_num,
            line_content,
            is_match_line,
            &start,
            &end,
            line_idx as u32,
            cli,
        )?;
    }

    Ok(())
}

/// Print a line with text markers (like diagnostic carets)
fn print_line_with_markers(
    line_num: usize,
    line_content: &str,
    is_match_line: bool,
    start: &LineCol,
    end: &LineCol,
    current_line: u32,
    cli: &mut dyn Cli,
) -> Result<(), anyhow::Error> {
    // Line number
    write!(cli, "{:4} | ", line_num)?;
    writeln!(cli, "{}", line_content)?;

    if is_match_line {
        // Print marker line with ^^^ under the match
        write!(cli, "     | ")?; // Indent to match line content

        if current_line == start.line && current_line == end.line {
            // Single-line match
            let start_col = start.col_utf16 as usize;
            let end_col = end.col_utf16 as usize;
            let marker_len = (end_col - start_col).max(1);

            // Spaces before the marker
            for _ in 0..start_col {
                write!(cli, " ")?;
            }
            // Marker carets
            for _ in 0..marker_len {
                write!(cli, "^")?;
            }
            writeln!(cli)?;
        } else if current_line == start.line {
            // First line of multi-line match
            let start_col = start.col_utf16 as usize;
            let marker_len = line_content.len().saturating_sub(start_col).max(1);

            for _ in 0..start_col {
                write!(cli, " ")?;
            }
            for _ in 0..marker_len {
                write!(cli, "^")?;
            }
            writeln!(cli)?;
        } else if current_line == end.line {
            // Last line of multi-line match
            let end_col = end.col_utf16 as usize;

            for _ in 0..end_col {
                write!(cli, "^")?;
            }
            writeln!(cli)?;
        } else {
            // Middle line of multi-line match
            for _ in 0..line_content.len() {
                write!(cli, "^")?;
            }
            writeln!(cli)?;
        }
    }

    Ok(())
}
