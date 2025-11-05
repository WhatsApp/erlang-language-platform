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
use std::time::SystemTime;

use anyhow::Result;
use anyhow::bail;
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
use elp_ide::diagnostics::FallBackToAll;
use elp_ide::diagnostics::LintConfig;
use elp_ide::diagnostics::LintsFromConfig;
use elp_ide::diagnostics::MatchSsr;
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
use indicatif::ParallelProgressIterator;
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
) -> Result<()> {
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
        let severity = if args.info_severity {
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
        .configure_diagnostics(
            &lint_config,
            &Some("ad-hoc: ssr-match".to_string()),
            &None,
            FallBackToAll::Yes,
        )?
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

    let r = run_ssr(cli, &mut loaded, &diagnostics_config, args);

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

    let mut diags = match (file_id, name) {
        (None, _) => do_parse_all(cli, &analysis, &loaded.project_id, diagnostics_config, args)?,
        (Some(file_id), Some(name)) => {
            if let Some(app) = &args.app
                && let Ok(Some(file_app)) = analysis.file_app_name(file_id)
                && file_app != AppName(app.to_string())
            {
                panic!("Module {} does not belong to app {}", name.as_str(), app)
            }
            do_parse_one(&analysis, diagnostics_config, file_id, &name, args)?
                .map_or(vec![], |x| vec![x])
        }
        (Some(file_id), _) => {
            panic!("Could not get name from file_id for {file_id:?}")
        }
    };
    if diags.is_empty() {
        if args.is_format_normal() {
            writeln!(cli, "No matches found")?;
        }
    } else {
        diags.sort_by(|(a, _, _), (b, _, _)| a.cmp(b));
        if args.is_format_json() {
            for (_name, file_id, diags) in &diags {
                for diag in diags {
                    // We use JSON output for CI, and want to see warnings too.
                    // So do not filter on errors only
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
            }
        } else {
            writeln!(cli, "Matches found in {} modules:", diags.len())?;
            for (name, file_id, diags) in &diags {
                writeln!(cli, "  {}: {}", name, diags.len())?;
                for diag in diags {
                    print_diagnostic(diag, &loaded.analysis(), *file_id, false, cli)?;
                }
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

fn do_parse_all(
    cli: &dyn Cli,
    analysis: &Analysis,
    project_id: &ProjectId,
    config: &DiagnosticsConfig,
    args: &Ssr,
) -> Result<Vec<(String, FileId, Vec<diagnostics::Diagnostic>)>> {
    let module_index = analysis.module_index(*project_id).unwrap();
    let module_iter = module_index.iter_own();

    let pb = cli.progress(module_iter.len() as u64, "Scanning modules");
    let app_name = args.app.as_ref().map(|name| AppName(name.to_string()));

    Ok(module_iter
        .par_bridge()
        .progress_with(pb)
        .map_with(
            analysis.clone(),
            |db, (module_name, _file_source, file_id)| {
                if !otp_file_to_ignore(db, file_id)
                    && db.file_app_type(file_id).ok() != Some(Some(AppType::Dep))
                    && (app_name.is_none()
                        || db.file_app_name(file_id).ok().as_ref() == Some(&app_name))
                {
                    do_parse_one(db, config, file_id, module_name.as_str(), args).unwrap()
                } else {
                    None
                }
            },
        )
        .flatten()
        .collect())
}

fn do_parse_one(
    db: &Analysis,
    config: &DiagnosticsConfig,
    file_id: FileId,
    name: &str,
    args: &Ssr,
) -> Result<Option<(String, FileId, Vec<diagnostics::Diagnostic>)>> {
    if !args.include_tests && db.is_test_suite_or_test_helper(file_id)?.unwrap_or(false) {
        return Ok(None);
    }

    // Run only the SSR lint configured in lints_from_config
    let diagnostics = db.with_db(|database| {
        let sema = Semantic::new(database);
        let mut diags = Vec::new();
        config
            .lints_from_config
            .get_diagnostics(&mut diags, &sema, file_id);
        sema.parse(file_id);
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
    use_cli_severity: bool,
    cli: &mut dyn Cli,
) -> Result<(), anyhow::Error> {
    let line_index = analysis.line_index(file_id)?;
    writeln!(cli, "      {}", diag.print(&line_index, use_cli_severity))?;
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
