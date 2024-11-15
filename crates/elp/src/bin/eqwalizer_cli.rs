/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::sync::Arc;

use anyhow::bail;
use anyhow::Context;
use anyhow::Result;
use codespan_reporting::term::termcolor::Color;
use codespan_reporting::term::termcolor::ColorSpec;
use elp::build;
use elp::build::load;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp::convert;
use elp_eqwalizer::EqwalizerConfig;
use elp_eqwalizer::EqwalizerDiagnosticsDatabase;
use elp_eqwalizer::Mode;
use elp_ide::diagnostics::Diagnostic;
use elp_ide::diagnostics::DiagnosticsConfig;
use elp_ide::diagnostics::LabeledDiagnostics;
use elp_ide::diagnostics::RemoveElpReported;
use elp_ide::diagnostics_collection::DiagnosticCollection;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::ModuleName;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_ide::elp_ide_db::EqwalizerDiagnostics;
use elp_ide::elp_ide_db::LineIndex;
use elp_ide::elp_ide_db::LineIndexDatabase;
use elp_ide::erlang_service;
use elp_ide::Analysis;
use elp_project_model::buck::BuckQueryConfig;
use elp_project_model::AppName;
use elp_project_model::DiscoverConfig;
use elp_project_model::ProjectBuildData;
use fxhash::FxHashMap;
use indicatif::ParallelProgressIterator;
use itertools::Itertools;
use lazy_static::lazy_static;
use rayon::prelude::*;

use crate::args::Eqwalize;
use crate::args::EqwalizeAll;
use crate::args::EqwalizeApp;
use crate::args::EqwalizeStats;
use crate::args::EqwalizeTarget;
use crate::reporting;
use crate::reporting::ParseDiagnostic;
use crate::reporting::Reporter;

struct EqwalizerInternalArgs<'a> {
    analysis: &'a Analysis,
    loaded: &'a LoadResult,
    file_ids: Vec<FileId>,
    reporter: &'a mut dyn reporting::Reporter,
}

pub fn eqwalize_module(
    args: &Eqwalize,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
) -> Result<()> {
    let config = DiscoverConfig::new(args.rebar, &args.profile);
    let mut loaded = load::load_project_at(
        cli,
        &args.project,
        config,
        IncludeOtp::Yes,
        Mode::Cli,
        query_config,
    )?;
    build::compile_deps(&loaded, cli)?;
    do_eqwalize_module(args, &mut loaded, cli)
}

pub fn do_eqwalize_module(
    args: &Eqwalize,
    loaded: &mut LoadResult,
    cli: &mut dyn Cli,
) -> Result<()> {
    set_eqwalizer_config(loaded, args.clause_coverage);
    let analysis = &loaded.analysis();
    let mut file_ids = vec![];
    for module in &args.modules {
        let suggest_name = Path::new(module).file_stem().and_then(|name| name.to_str());
        let context_str = match suggest_name {
            Some(name) if name != module => format!(
                "Module {} not found. Did you mean elp eqwalize {}?",
                module, name
            ),
            _ => format!("Module {} not found", module),
        };
        let file_id = analysis
            .module_file_id(loaded.project_id, module)?
            .with_context(|| context_str)?;
        file_ids.push(file_id);
    }

    let mut json_reporter;
    let mut pretty_reporter;

    let reporter: &mut dyn Reporter = match args.format {
        None => {
            pretty_reporter = reporting::PrettyReporter::new(analysis, loaded, cli);
            &mut pretty_reporter
        }
        Some(_) => {
            json_reporter = reporting::JsonReporter::new(analysis, loaded, cli);
            &mut json_reporter
        }
    };

    eqwalize(EqwalizerInternalArgs {
        analysis,
        loaded,
        file_ids,
        reporter,
    })
}

pub const SHELL_HINT: &str = "\
eqWAlizing frequently? Consider using command \x1b[0;33melp shell\x1b[0m to cut down on processing time.";

pub fn eqwalize_all(
    args: &EqwalizeAll,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
) -> Result<()> {
    // Hack to avoid hint appearing in tests
    cli.spinner(SHELL_HINT).finish();
    let config = DiscoverConfig::new(args.rebar, &args.profile);
    let mut loaded = load::load_project_at(
        cli,
        &args.project,
        config,
        IncludeOtp::Yes,
        Mode::Cli,
        query_config,
    )?;
    build::compile_deps(&loaded, cli)?;
    do_eqwalize_all(args, &mut loaded, cli)
}

pub fn do_eqwalize_all(
    args: &EqwalizeAll,
    loaded: &mut LoadResult,
    cli: &mut dyn Cli,
) -> Result<()> {
    set_eqwalizer_config(loaded, args.clause_coverage);
    let analysis = &loaded.analysis();
    let module_index = analysis.module_index(loaded.project_id)?;
    let include_generated = args.include_generated.into();
    let pb = cli.progress(module_index.len_own() as u64, "Gathering modules");
    let file_ids: Vec<FileId> = module_index
        .iter_own()
        .par_bridge()
        .progress_with(pb.clone())
        .map_with(analysis.clone(), |analysis, (_name, _source, file_id)| {
            if analysis
                .should_eqwalize(file_id, include_generated)
                .unwrap()
            {
                Some(file_id)
            } else {
                None
            }
        })
        .flatten()
        .collect();
    pb.finish();

    let mut json_reporter;
    let mut pretty_reporter;

    let reporter: &mut dyn Reporter = match args.format {
        None => {
            pretty_reporter = reporting::PrettyReporter::new(analysis, loaded, cli);
            &mut pretty_reporter
        }
        Some(_) => {
            json_reporter = reporting::JsonReporter::new(analysis, loaded, cli);
            &mut json_reporter
        }
    };

    eqwalize(EqwalizerInternalArgs {
        analysis,
        loaded,
        file_ids,
        reporter,
    })
}

pub fn eqwalize_app(
    args: &EqwalizeApp,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
) -> Result<()> {
    let config = DiscoverConfig::new(args.rebar, &args.profile);
    let mut loaded = load::load_project_at(
        cli,
        &args.project,
        config,
        IncludeOtp::Yes,
        Mode::Cli,
        query_config,
    )?;
    build::compile_deps(&loaded, cli)?;
    do_eqwalize_app(args, &mut loaded, cli)
}

pub fn do_eqwalize_app(
    args: &EqwalizeApp,
    loaded: &mut LoadResult,
    cli: &mut dyn Cli,
) -> Result<()> {
    set_eqwalizer_config(loaded, args.clause_coverage);
    let analysis = &loaded.analysis();
    let module_index = analysis.module_index(loaded.project_id)?;
    let include_generated = args.include_generated.into();
    let file_ids: Vec<FileId> = module_index
        .iter_own()
        .filter_map(|(_name, _source, file_id)| {
            if analysis.file_app_name(file_id).ok()? == Some(AppName(args.app.clone()))
                && analysis
                    .should_eqwalize(file_id, include_generated)
                    .unwrap()
            {
                Some(file_id)
            } else {
                None
            }
        })
        .collect();
    let mut reporter = reporting::PrettyReporter::new(analysis, loaded, cli);
    eqwalize(EqwalizerInternalArgs {
        analysis,
        loaded,
        file_ids,
        reporter: &mut reporter,
    })
}

pub fn eqwalize_target(
    args: &EqwalizeTarget,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
) -> Result<()> {
    let config = DiscoverConfig::buck();
    let mut loaded = load::load_project_at(
        cli,
        &args.project,
        config,
        IncludeOtp::Yes,
        Mode::Cli,
        query_config,
    )?;

    set_eqwalizer_config(&mut loaded, args.clause_coverage);

    let buck = match &loaded.project.project_build_data {
        ProjectBuildData::Buck(buck) => buck,
        _ => bail!("only buck project supported"),
    };
    let (_, target) = args.target.split_once("//").unwrap_or(("", &args.target));
    let buck_target = target.strip_suffix("/...").unwrap_or(target);
    let buck_target = buck_target.strip_suffix(':').unwrap_or(buck_target);

    let analysis = &loaded.analysis();
    let include_generated = args.include_generated.into();
    let mut file_ids: Vec<FileId> = Default::default();
    let mut at_least_one_found = false;
    let exact_match = buck_target.contains(':');
    for (name, target) in &buck.target_info.targets {
        let (_, name) = name.split_once("//").unwrap();
        let matches = if exact_match {
            name == buck_target
        } else {
            name.starts_with(buck_target)
        };
        if matches {
            for src in &target.src_files {
                let vfs_path = VfsPath::from(src.clone());
                if let Some(file_id) = loaded.vfs.file_id(&vfs_path) {
                    at_least_one_found = true;
                    if analysis
                        .should_eqwalize(file_id, include_generated)
                        .unwrap()
                    {
                        file_ids.push(file_id);
                    }
                }
            }
        }
    }
    match (file_ids.is_empty(), at_least_one_found) {
        (true, true) => bail!("Eqwalizer is disabled for all source files in given target"),
        (true, false) => bail!(
            r###"Can't find any source files for given target.
You can verify your target with: buck2 targets {0}
Examples of valid targets:
elp eqwalize-target //erl/util/... #all targets listed in buck2 targets //erl/util/...
elp eqwalize-target waserver//erl/chatd #all targets listed in buck2 targets waserver//erl/chatd/...
elp eqwalize-target //erl/chatd:chatd #concrete target: buck2 targets waserver//erl/chatd:chatd
elp eqwalize-target erl/chatd #same as //erl/chatd/... but enables shell completion
            "###,
            args.target
        ),
        _ => (),
    };

    let mut reporter = reporting::PrettyReporter::new(analysis, &loaded, cli);
    eqwalize(EqwalizerInternalArgs {
        analysis,
        loaded: &loaded,
        file_ids,
        reporter: &mut reporter,
    })
}

pub fn eqwalize_stats(
    args: &EqwalizeStats,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
) -> Result<()> {
    let config = DiscoverConfig::new(args.rebar, &args.profile);
    let loaded = load::load_project_at(
        cli,
        &args.project,
        config,
        IncludeOtp::Yes,
        Mode::Cli,
        query_config,
    )?;
    build::compile_deps(&loaded, cli)?;
    let analysis = &loaded.analysis();
    let module_index = analysis.module_index(loaded.project_id)?;
    let include_generated = args.include_generated.into();
    let project_id = loaded.project_id;
    let pb = cli.progress(module_index.len_own() as u64, "Computing stats");
    let stats: FxHashMap<FileId, (ModuleName, Vec<Diagnostic>)> = module_index
        .iter_own()
        .par_bridge()
        .progress_with(pb.clone())
        .map_with(analysis.clone(), |analysis, (name, _source, file_id)| {
            if analysis
                .should_eqwalize(file_id, include_generated)
                .expect("cancelled")
            {
                analysis
                    .eqwalizer_stats(project_id, file_id)
                    .expect("cancelled")
                    .map(|stats| (file_id, (name.clone(), stats)))
            } else {
                None
            }
        })
        .flatten()
        .collect();
    pb.finish();
    for (file_id, (_name, stats)) in stats
        .into_iter()
        .sorted_by(|(_, (name1, _)), (_, (name2, _))| Ord::cmp(name1, name2))
    {
        let vfs_path = loaded.vfs.file_path(file_id);
        let analysis = loaded.analysis();
        let root_path = &analysis
            .project_data(file_id)
            .unwrap_or_else(|_err| panic!("could not find project data"))
            .unwrap_or_else(|| panic!("could not find project data"))
            .root_dir;
        let relative_path = reporting::get_relative_path(root_path, &vfs_path);
        let line_index = analysis.line_index(file_id)?;
        for stat in stats {
            print_diagnostic_json(&stat, &line_index, relative_path, cli)?;
        }
    }
    Ok(())
}

fn print_diagnostic_json(
    diagnostic: &Diagnostic,
    line_index: &LineIndex,
    path: &Path,
    cli: &mut dyn Cli,
) -> Result<()> {
    let converted_diagnostic = convert::ide_to_arc_diagnostic(line_index, path, diagnostic);
    writeln!(
        cli,
        "{}",
        serde_json::to_string(&converted_diagnostic).unwrap_or_else(|err| panic!(
            "print_diagnostics_json failed for '{:?}': {}",
            converted_diagnostic, err
        ))
    )?;
    Ok(())
}

fn eqwalize(
    EqwalizerInternalArgs {
        analysis,
        loaded,
        file_ids,
        reporter,
    }: EqwalizerInternalArgs,
) -> Result<()> {
    if file_ids.is_empty() {
        bail!("No files to eqWAlize detected")
    }

    pre_parse_for_speed(reporter, analysis.clone(), &file_ids);

    let files_count = file_ids.len();
    let pb = reporter.progress(files_count as u64, "EqWAlizing");
    let output = loaded.with_eqwalizer_progress_bar(pb.clone(), move |analysis| {
        let project_id = loaded.project_id;
        let max_tasks = loaded.project.eqwalizer_config.max_tasks;
        let chunk_size = (files_count + max_tasks - 1) / max_tasks;
        file_ids
            .chunks(chunk_size)
            .par_bridge()
            .map_with(analysis, move |analysis, file_ids| {
                analysis
                    .eqwalizer_diagnostics(project_id, file_ids.to_vec())
                    .expect("cancelled")
            })
            .fold(EqwalizerDiagnostics::default, |acc, output| {
                acc.combine((*output).clone())
            })
            .reduce(EqwalizerDiagnostics::default, |acc, other| {
                acc.combine(other)
            })
    });
    let eqwalized = pb.position();
    pb.finish();
    match output {
        EqwalizerDiagnostics::Diagnostics {
            errors: diagnostics_by_module,
            ..
        } => {
            for (module, diagnostics) in diagnostics_by_module
                .into_iter()
                .sorted_by(|(name1, _), (name2, _)| Ord::cmp(name1, name2))
            {
                let file_id = analysis
                    .module_index(loaded.project_id)?
                    .file_for_module(module.as_str())
                    .with_context(|| format!("module {} not found", module))?;
                reporter.write_eqwalizer_diagnostics(file_id, &diagnostics)?;
            }
            if analysis.eqwalizer().mode == Mode::Shell {
                reporter.write_stats(eqwalized, files_count as u64)?;
            }
            reporter.write_error_count()?;
            Ok(())
        }
        EqwalizerDiagnostics::NoAst { module } => {
            if let Some(file_id) = analysis.module_file_id(loaded.project_id, &module)? {
                let config = DiagnosticsConfig::default();
                let erlang_service_diagnostics =
                    analysis.erlang_service_diagnostics(file_id, &config, RemoveElpReported::No)?;
                let erlang_service = erlang_service_diagnostics
                    .into_iter()
                    .find(|(f, _diags)| f == &file_id)
                    .map(|(_, diags)| diags)
                    .unwrap_or(LabeledDiagnostics::default());
                let mut diagnostics = DiagnosticCollection::default();
                diagnostics.set_erlang_service(file_id, erlang_service);
                // `diagnostics_for` will also combine related diagnostics
                let diags = diagnostics.diagnostics_for(file_id);
                let mut parse_diagnostics: Vec<ParseDiagnostic> = Vec::default();
                let line_index = analysis.with_db(|db| db.file_line_index(file_id))?;
                for diag in diags {
                    let vfs_path = loaded.vfs.file_path(file_id);
                    let analysis = loaded.analysis();
                    let root_path = &analysis
                        .project_data(file_id)
                        .unwrap_or_else(|_err| panic!("could not find project data"))
                        .unwrap_or_else(|| panic!("could not find project data"))
                        .root_dir;
                    let relative_path = reporting::get_relative_path(root_path, &vfs_path);

                    let line_num = convert::position(&line_index, diag.range.start()).line + 1;
                    parse_diagnostics.push(ParseDiagnostic {
                        file_id,
                        relative_path: relative_path.to_path_buf(),
                        line_num,
                        msg: diag.message,
                        range: Some(diag.range),
                    });
                }
                // The cached parse errors must be non-empty otherwise we wouldn't have `NoAst`
                assert!(
                    !parse_diagnostics.is_empty(),
                    "Expecting erlang service diagnostics, but none found, for '{}'",
                    module
                );
                let parse_diagnostics: Vec<_> = parse_diagnostics
                    .into_iter()
                    .sorted_by(|d1, d2| {
                        Ord::cmp(&d1.range.map(|r| r.start()), &d2.range.map(|r| r.start()))
                    })
                    .collect();
                reporter.write_parse_diagnostics(&parse_diagnostics)?;
                Ok(())
            } else {
                bail!(
                    "Could not type-check because module {} was not found",
                    module
                )
            }
        }
        EqwalizerDiagnostics::Error(error) => {
            bail!("Could not eqwalize: {}", error)
        }
    }
}

fn pre_parse_for_speed(reporter: &dyn Reporter, analysis: Analysis, file_ids: &[FileId]) {
    let pb = reporter.progress(file_ids.len() as u64, "Parsing modules");
    file_ids
        .par_iter()
        .progress_with(pb.clone())
        .for_each_with(analysis, |analysis, &file_id| {
            let _ = analysis.module_ast(file_id, erlang_service::Format::OffsetEtf, vec![], vec![]);
        });
    pb.finish();
}

fn set_eqwalizer_config(loaded: &mut LoadResult, clause_coverage: bool) -> () {
    let config = EqwalizerConfig {
        clause_coverage: clause_coverage.then_some(true),
        ..EqwalizerConfig::default()
    };
    let db = loaded.analysis_host.raw_database_mut();
    if config != *db.eqwalizer_config() {
        db.set_eqwalizer_config(Arc::new(config));
    }
}

lazy_static! {
    static ref YELLOW_COLOR_SPEC: ColorSpec = {
        let mut spec = ColorSpec::default();
        spec.set_fg(Some(Color::Yellow));
        spec
    };
}
