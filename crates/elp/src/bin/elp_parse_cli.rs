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
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
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
use elp_ide::diagnostics;
use elp_ide::diagnostics::DiagnosticsConfig;
use elp_ide::diagnostics::LabeledDiagnostics;
use elp_ide::diagnostics::RemoveElpReported;
use elp_ide::diagnostics_collection::DiagnosticCollection;
use elp_ide::elp_ide_db::Includes;
use elp_ide::elp_ide_db::LineIndex;
use elp_ide::elp_ide_db::LineIndexDatabase;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FileSource;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::ModuleName;
use elp_ide::elp_ide_db::elp_base_db::Vfs;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_log::telemetry;
use elp_project_model::AppType;
use elp_project_model::DiscoverConfig;
use elp_project_model::buck::BuckQueryConfig;
use indicatif::ParallelProgressIterator;
use indicatif::ProgressIterator;
use lsp_types::DiagnosticSeverity;
use lsp_types::NumberOrString;
use paths::Utf8PathBuf;
use rayon::iter::ParallelBridge;
use rayon::iter::ParallelIterator;
use vfs::AbsPath;

use crate::args::ParseAllElp;
use crate::reporting;
use crate::reporting::print_memory_usage;

fn parse_severity(severity: &str) -> Option<diagnostics::Severity> {
    match severity {
        "error" => Some(diagnostics::Severity::Error),
        "warning" => Some(diagnostics::Severity::Warning),
        "weak_warning" => Some(diagnostics::Severity::WeakWarning),
        "information" => Some(diagnostics::Severity::Information),
        _ => None,
    }
}

fn severity_rank(severity: diagnostics::Severity) -> u8 {
    match severity {
        diagnostics::Severity::Error => 1,
        diagnostics::Severity::Warning => 2,
        diagnostics::Severity::WeakWarning => 3,
        diagnostics::Severity::Information => 4,
    }
}

fn meets_severity_threshold(
    diag_severity: diagnostics::Severity,
    min_severity: Option<diagnostics::Severity>,
) -> bool {
    match min_severity {
        None => true,
        Some(min) => severity_rank(diag_severity) <= severity_rank(min),
    }
}

#[derive(Debug)]
struct ParseResult {
    name: String,
    file_id: FileId,
    diagnostics: DiagnosticCollection,
}

pub fn parse_all(
    args: &ParseAllElp,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
) -> Result<()> {
    log::info!("Loading project at: {:?}", args.project);

    let start_time = SystemTime::now();
    // Track memory usage at the start
    let memory_start = MemoryUsage::now();

    let config = DiscoverConfig::new(args.rebar, &args.profile);
    let loaded = load::load_project_at(
        cli,
        &args.project,
        config,
        IncludeOtp::Yes,
        Mode::Cli,
        query_config,
    )?;

    if let Some(to) = &args.to {
        fs::create_dir_all(to)?
    };

    let analysis = loaded.analysis();

    let (file_id, name) = match &args.module {
        Some(module) => {
            if args.is_format_normal() {
                writeln!(cli, "module specified: {module}")?;
            }
            let file_id = analysis.module_file_id(loaded.project_id, module)?;
            (file_id, analysis.module_name(file_id.unwrap())?)
        }

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
                        .map(|(file_id, _)| file_id),
                    path_buf.as_path().file_name().map(ModuleName::new),
                )
            }
            None => (None, None),
        },
    };

    let cfg = DiagnosticsConfig {
        experimental: args.experimental_diags,
        include_generated: args.include_generated,
        ..Default::default()
    };

    let mut res = match (file_id, name, args.serial) {
        (None, _, true) => do_parse_all_seq(cli, &loaded, &cfg, &args.to)?,
        (None, _, false) => do_parse_all_par(cli, &loaded, &cfg, &args.to)?,
        (Some(file_id), Some(name), _) => {
            do_parse_one(&analysis, &cfg, &args.to, file_id, &name)?.map_or(vec![], |x| vec![x])
        }
        (Some(file_id), _, _) => panic!("Could not get name from file_id for {file_id:?}"),
    };

    if args.dump_include_resolutions {
        dump_includes_resolutions(cli, &loaded, &args.to)?;
    }

    let db = loaded.analysis_host.raw_database();

    telemetry::report_elapsed_time("parse-elp operational", start_time);

    let memory_end = MemoryUsage::now();
    let memory_used = memory_end - memory_start;

    if res.is_empty() {
        if args.is_format_normal() {
            writeln!(cli, "No errors reported")?;
        }
        if args.is_format_normal() && args.report_system_stats {
            print_memory_usage(loaded.analysis_host, loaded.vfs, cli)?;
            writeln!(cli, "{}", memory_used)?;
        }
        Ok(())
    } else {
        if args.is_format_normal() {
            writeln!(cli, "Diagnostics reported in {} modules:", res.len())?;
        }
        res.sort_by(|a, b| a.name.cmp(&b.name));
        let mut err_in_diag = false;
        let min_severity = args
            .severity
            .as_ref()
            .and_then(|s| parse_severity(s.as_str()));
        for diags in res {
            let mut combined: Vec<diagnostics::Diagnostic> =
                diags.diagnostics.diagnostics_for(diags.file_id);
            if args.is_format_normal() {
                writeln!(cli, "  {}: {}", diags.name, combined.len())?;
            }
            if args.print_diags {
                let line_index = db.file_line_index(diags.file_id);
                combined.sort_by(|a, b| a.range.start().cmp(&b.range.start()));
                for diag in combined {
                    if !meets_severity_threshold(diag.severity, min_severity) {
                        continue;
                    }
                    if args.is_format_json() {
                        err_in_diag = true;
                        let vfs_path = loaded.vfs.file_path(diags.file_id);
                        let analysis = loaded.analysis();
                        let root_path = &analysis
                            .project_data(diags.file_id)
                            .unwrap_or_else(|_err| panic!("could not find project data"))
                            .unwrap_or_else(|| panic!("could not find project data"))
                            .root_dir;
                        let relative_path = reporting::get_relative_path(root_path, vfs_path);
                        print_diagnostic_json(
                            &diag,
                            &analysis,
                            diags.file_id,
                            relative_path,
                            args.use_cli_severity,
                            cli,
                        )?;
                    } else {
                        print_diagnostic(&diag, &line_index, &mut err_in_diag, cli)?;
                    }
                }
            }
        }

        telemetry::report_elapsed_time("parse-elp done", start_time);

        if args.is_format_normal() && args.report_system_stats {
            print_memory_usage(loaded.analysis_host, loaded.vfs, cli)?;
            writeln!(cli, "{}", memory_used)?;
        }

        if err_in_diag {
            bail!("Parse failures found")
        } else {
            Ok(())
        }
    }
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

fn print_diagnostic(
    diag: &diagnostics::Diagnostic,
    line_index: &LineIndex,
    err_in_diag: &mut bool,
    cli: &mut dyn Cli,
) -> Result<(), anyhow::Error> {
    let diag = convert::ide_to_lsp_diagnostic(line_index, diag, |_file_id| None);
    let severity = match diag.severity {
        None => DiagnosticSeverity::ERROR,
        Some(sev) => {
            *err_in_diag |= sev == DiagnosticSeverity::ERROR;
            sev
        }
    };
    writeln!(
        cli,
        "      {}:{}-{}:{}::[{:?}] [{}] {}",
        diag.range.start.line,
        diag.range.start.character,
        diag.range.end.line,
        diag.range.end.character,
        severity,
        maybe_code_as_string(diag.code),
        diag.message
    )?;
    Ok(())
}

fn maybe_code_as_string(mc: Option<NumberOrString>) -> String {
    match mc {
        Some(ns) => match ns {
            NumberOrString::Number(n) => format!("{n}"),
            NumberOrString::String(s) => s,
        },
        None => "".to_string(),
    }
}

fn do_parse_all_par(
    cli: &dyn Cli,
    loaded: &LoadResult,
    config: &DiagnosticsConfig,
    to: &Option<PathBuf>,
) -> Result<Vec<ParseResult>> {
    let module_index = loaded.analysis().module_index(loaded.project_id).unwrap();
    let module_iter = module_index.iter_own();

    let pb = cli.progress(module_iter.len() as u64, "Parsing modules");

    Ok(module_iter
        .par_bridge()
        .progress_with(pb)
        .map_with(
            loaded.analysis(),
            |db, (module_name, file_source, file_id)| {
                if !otp_file_to_ignore(db, file_id)
                    && file_source == FileSource::Src
                    && db.file_app_type(file_id).ok() != Some(Some(AppType::Dep))
                {
                    do_parse_one(db, config, to, file_id, module_name.as_str()).unwrap()
                } else {
                    None
                }
            },
        )
        .flatten()
        .collect())
}

fn do_parse_all_seq(
    cli: &dyn Cli,
    loaded: &LoadResult,
    config: &DiagnosticsConfig,
    to: &Option<PathBuf>,
) -> Result<Vec<ParseResult>> {
    let module_index = loaded.analysis().module_index(loaded.project_id).unwrap();
    let module_iter = module_index.iter_own();

    let pb = cli.progress(module_iter.len() as u64, "Parsing modules (sequential)");

    let db = loaded.analysis();
    Ok(module_iter
        .progress_with(pb)
        .flat_map(|(module_name, file_source, file_id)| {
            if !otp_file_to_ignore(&db, file_id)
                && file_source == FileSource::Src
                && db.file_app_type(file_id).ok() != Some(Some(AppType::Dep))
            {
                do_parse_one(&db, config, to, file_id, module_name.as_str()).unwrap()
            } else {
                None
            }
        })
        .collect())
}

fn do_parse_one(
    db: &Analysis,
    config: &DiagnosticsConfig,
    to: &Option<PathBuf>,
    file_id: FileId,
    name: &str,
) -> Result<Option<ParseResult>> {
    let native = db.native_diagnostics(config, &vec![], file_id)?;
    let erlang_service_diagnostics =
        db.erlang_service_diagnostics(file_id, config, RemoveElpReported::Yes)?;
    let line_index = db.line_index(file_id)?;

    // Should we return the included file diagnostics as well? Not doing so now.
    let erlang_service = erlang_service_diagnostics
        .into_iter()
        .find(|(f, _diags)| f == &file_id)
        .map(|(_, diags)| diags)
        .unwrap_or(LabeledDiagnostics::default());

    if let Some(to) = to {
        let to_path = to.join(format!("{name}.diag"));
        let mut output = File::create(to_path)?;

        for diagnostic in native.iter() {
            let diagnostic =
                convert::ide_to_lsp_diagnostic(&line_index, diagnostic, |_file_id| None);
            writeln!(output, "{diagnostic:?}")?;
        }
        for diagnostic in erlang_service.iter() {
            let diagnostic =
                convert::ide_to_lsp_diagnostic(&line_index, diagnostic, |_file_id| None);
            writeln!(output, "{diagnostic:?}")?;
        }
    }
    if !(native.is_empty() && erlang_service.is_empty()) {
        let mut diagnostics = DiagnosticCollection::default();
        diagnostics.set_native(file_id, native);
        diagnostics.set_erlang_service(file_id, erlang_service);
        let res = ParseResult {
            name: name.to_string(),
            file_id,
            diagnostics,
        };
        Ok(Some(res))
    } else {
        Ok(None)
    }
}

// ---------------------------------------------------------------------

fn dump_includes_resolutions(
    cli: &dyn Cli,
    loaded: &LoadResult,
    to: &Option<PathBuf>,
) -> Result<()> {
    let module_index = loaded.analysis().module_index(loaded.project_id).unwrap();
    let module_iter = module_index.iter_own();

    let pb = cli.progress(module_iter.len() as u64, "Analyzing include resolutions");

    let vfs = &loaded.vfs;
    let mut all_includes: Vec<_> = module_iter
        .par_bridge()
        .progress_with(pb)
        .map_with(
            loaded.analysis(),
            |db, (_module_name, file_source, file_id)| {
                if !otp_file_to_ignore(db, file_id)
                    && file_source == FileSource::Src
                    && db.file_app_type(file_id).ok() != Some(Some(AppType::Dep))
                {
                    if is_project_file(vfs, db, file_id) {
                        db.resolved_includes(file_id).ok()
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
        )
        .filter_map(|it| {
            it.unwrap_or_default()
                .map(|include| {
                    if !include.file.contains("SUITE") {
                        Some(include)
                    } else {
                        None
                    }
                })
                .unwrap_or_default()
        })
        .collect();

    all_includes.sort_by(|a, b| a.file.cmp(&b.file));
    if let Some(to) = to {
        let to_path = to.join("includes.json");
        let mut output = File::create(to_path)?;
        dump_include_resolution(&all_includes, &mut output);
    } else {
        let mut output = std::io::stdout();
        dump_include_resolution(&all_includes, &mut output);
    };
    Ok(())
}

fn is_project_file(vfs: &Vfs, db: &Analysis, id: FileId) -> bool {
    let root_abs = &db.project_data(id).unwrap().unwrap().root_dir;
    let path = vfs.file_path(id);
    let path = path.as_path().unwrap();
    path.starts_with(root_abs)
}

fn dump_include_resolution(includes: &[Includes], to: &mut impl std::io::Write) {
    if let Ok(str) = serde_json::to_string(includes) {
        _ = to.write(str.as_bytes());
    }
}
