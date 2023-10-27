/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::io;
use std::path::Path;

use anyhow::anyhow;
use anyhow::bail;
use anyhow::Context;
use anyhow::Result;
use elp::build;
use elp::build::load;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp_eqwalizer::Mode;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_ide::elp_ide_db::EqwalizerDiagnostics;
use elp_ide::elp_ide_db::EqwalizerStats;
use elp_ide::erlang_service;
use elp_ide::Analysis;
use elp_project_model::AppName;
use elp_project_model::AppType;
use elp_project_model::DiscoverConfig;
use elp_project_model::ProjectBuildData;
use fxhash::FxHashMap;
use indicatif::ParallelProgressIterator;
use itertools::Itertools;
use rayon::prelude::*;

use crate::args::Eqwalize;
use crate::args::EqwalizeAll;
use crate::args::EqwalizeApp;
use crate::args::EqwalizePassthrough;
use crate::args::EqwalizeStats;
use crate::args::EqwalizeTarget;
use crate::erlang_service_cli;
use crate::reporting;
use crate::reporting::Reporter;

/// Max parallel eqWAlizer tasks.
///
/// Since eqWAlizer is frequently limited by memory, this can't be fully parallel
const MAX_EQWALIZER_TASKS: usize = 4;

/// Thread stack size for eqWAlizer tasks, in bytes.
///
/// Due to inefficient encoding of lists, the default stack size of 2MiB may not be
/// enough for some generated modules.
const THREAD_STACK_SIZE: usize = 10_000_000;

struct EqwalizerInternalArgs<'a> {
    analysis: &'a Analysis,
    loaded: &'a LoadResult,
    file_ids: Vec<FileId>,
    reporter: &'a mut dyn reporting::Reporter,
}

pub fn eqwalize_module(args: &Eqwalize, cli: &mut dyn Cli) -> Result<()> {
    let config = DiscoverConfig::new(args.rebar, &args.profile);
    let loaded = load::load_project_at(cli, &args.project, config, IncludeOtp::Yes, Mode::Cli)?;
    build::compile_deps(&loaded, cli)?;
    do_eqwalize_module(args, &loaded, cli)
}

pub fn do_eqwalize_module(args: &Eqwalize, loaded: &LoadResult, cli: &mut dyn Cli) -> Result<()> {
    let analysis = &loaded.analysis();
    let suggest_name = Path::new(&args.module)
        .file_stem()
        .and_then(|name| name.to_str());
    let context_str = match suggest_name {
        Some(name) if name != args.module => format!(
            "Module {} not found. Did you mean elp eqwalize {}?",
            &args.module, name
        ),
        _ => format!("Module {} not found", &args.module),
    };
    let file_id = analysis
        .module_file_id(loaded.project_id, &args.module)?
        .with_context(|| context_str)?;
    let reporter = &mut reporting::PrettyReporter::new(analysis, loaded, cli);
    eqwalize(EqwalizerInternalArgs {
        analysis,
        loaded,
        file_ids: vec![file_id],
        reporter,
    })
}

pub fn eqwalize_all(args: &EqwalizeAll, cli: &mut dyn Cli) -> Result<()> {
    let config = DiscoverConfig::new(args.rebar, &args.profile);
    let loaded = load::load_project_at(cli, &args.project, config, IncludeOtp::Yes, Mode::Cli)?;
    build::compile_deps(&loaded, cli)?;
    do_eqwalize_all(args, &loaded, cli)
}

pub fn do_eqwalize_all(args: &EqwalizeAll, loaded: &LoadResult, cli: &mut dyn Cli) -> Result<()> {
    let analysis = &loaded.analysis();
    let module_index = analysis.module_index(loaded.project_id)?;
    let include_generated = args.include_generated;
    let pb = cli.progress(module_index.len_own() as u64, "Gathering modules");
    let file_ids: Vec<FileId> = module_index
        .iter_own()
        .par_bridge()
        .progress_with(pb.clone())
        .map_with(analysis.clone(), |analysis, (_name, _source, file_id)| {
            if should_eqwalize(analysis, file_id, include_generated) {
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

pub fn eqwalize_app(args: &EqwalizeApp, cli: &mut dyn Cli) -> Result<()> {
    let config = DiscoverConfig::new(args.rebar, &args.profile);
    let loaded = load::load_project_at(cli, &args.project, config, IncludeOtp::Yes, Mode::Cli)?;
    build::compile_deps(&loaded, cli)?;
    do_eqwalize_app(args, &loaded, cli)
}

pub fn do_eqwalize_app(args: &EqwalizeApp, loaded: &LoadResult, cli: &mut dyn Cli) -> Result<()> {
    let analysis = &loaded.analysis();
    let module_index = analysis.module_index(loaded.project_id)?;
    let include_generated = args.include_generated;
    let file_ids: Vec<FileId> = module_index
        .iter_own()
        .filter_map(|(_name, _source, file_id)| {
            if analysis.file_app_name(file_id).ok()? == Some(AppName(args.app.clone()))
                && should_eqwalize(analysis, file_id, include_generated)
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

pub fn eqwalize_target(args: &EqwalizeTarget, cli: &mut dyn Cli) -> Result<()> {
    let config = DiscoverConfig::buck();
    let loaded = load::load_project_at(cli, &args.project, config, IncludeOtp::Yes, Mode::Cli)?;

    let buck = match &loaded.project.project_build_data {
        ProjectBuildData::Buck(buck) => buck,
        _ => bail!("only buck project supported"),
    };
    let (_, target) = args.target.split_once("//").unwrap_or(("", &args.target));
    let buck_target = target.strip_suffix("/...").unwrap_or(target);
    let buck_target = buck_target.strip_suffix(':').unwrap_or(buck_target);

    let analysis = &loaded.analysis();
    let include_generated = args.include_generated;
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
                    if should_eqwalize(analysis, file_id, include_generated) {
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

pub fn eqwalize_passthrough(args: &EqwalizePassthrough, cli: &mut dyn Cli) -> Result<()> {
    let config = DiscoverConfig::new(!args.buck, &args.profile);
    let loaded = load::load_project_at(
        cli,
        &args.project,
        config,
        IncludeOtp::No,
        Mode::Passthrough,
    )?;
    build::compile_deps(&loaded, cli)?;

    let ast_dir = loaded.project.root().join("_build").join("elp").join("ast");

    ensure_empty_directory_exists(&ast_dir)?;
    let parse_diagnostics = erlang_service_cli::do_parse_all(
        cli,
        &loaded,
        ast_dir.as_ref(),
        erlang_service::Format::OffsetEtf,
        &None,
        args.buck,
    )?;
    if !parse_diagnostics.is_empty() {
        writeln!(
            cli,
            "{}",
            reporting::format_json_parse_error(&parse_diagnostics)
        )
        .unwrap();
        bail!("Aborting because there was an error parsing");
    }

    let status = loaded.analysis().eqwalizer().passthrough(
        args.args.as_ref(),
        loaded.project.build_info_file().unwrap().as_ref(),
        ast_dir.as_ref(),
    )?;

    let code = status
        .code()
        .ok_or_else(|| anyhow!("No return code for {:?}", args.args))?;
    std::process::exit(code);
}

pub fn eqwalize_stats(args: &EqwalizeStats, cli: &mut dyn Cli) -> Result<()> {
    let config = DiscoverConfig::new(args.rebar, &args.profile);
    let loaded = load::load_project_at(cli, &args.project, config, IncludeOtp::Yes, Mode::Cli)?;
    build::compile_deps(&loaded, cli)?;
    let analysis = &loaded.analysis();
    let module_index = analysis.module_index(loaded.project_id)?;
    let include_generated = args.include_generated;
    let project_id = loaded.project_id;
    let pb = cli.progress(module_index.len_own() as u64, "Computing stats");
    let stats: FxHashMap<&str, EqwalizerStats> = module_index
        .iter_own()
        .par_bridge()
        .progress_with(pb.clone())
        .map_with(analysis.clone(), |analysis, (name, _source, file_id)| {
            if should_eqwalize(analysis, file_id, include_generated) {
                analysis
                    .eqwalizer_stats(project_id, file_id)
                    .expect("cancelled")
                    .map(|stats| (name.as_str(), (*stats).clone()))
            } else {
                None
            }
        })
        .flatten()
        .collect();
    pb.finish();
    let _ = cli.write(serde_json::to_string(&stats)?.as_bytes())?;
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
        let chunk_size = (files_count + MAX_EQWALIZER_TASKS - 1) / MAX_EQWALIZER_TASKS;
        let pool = rayon::ThreadPoolBuilder::new()
            .stack_size(THREAD_STACK_SIZE)
            .build()
            .unwrap();
        let project_id = loaded.project_id;
        pool.install(|| {
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
                let parse_diagnostics = erlang_service_cli::do_parse_one(
                    analysis,
                    None,
                    file_id,
                    erlang_service::Format::OffsetEtf,
                )?;
                // The cached parse errors must be non-empty otherwise we wouldn't have `NoAst`
                assert!(!parse_diagnostics.is_empty());
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
            let _ = analysis.module_ast(file_id, erlang_service::Format::OffsetEtf);
        });
    pb.finish();
}

fn should_eqwalize(analysis: &Analysis, file_id: FileId, include_generated: bool) -> bool {
    let is_in_app = analysis.file_app_type(file_id).ok() == Some(Some(AppType::App));
    is_in_app
        && analysis
            .is_eqwalizer_enabled(file_id, include_generated)
            .unwrap()
}

fn ensure_empty_directory_exists(path: impl AsRef<Path>) -> io::Result<()> {
    let path = path.as_ref();
    fs::create_dir_all(path)?;
    fs::remove_dir_all(path)?;
    fs::create_dir(path)
}
