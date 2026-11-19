/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! `elp eqwalizer-onboard`: find the modules that eqWAlizer does not check yet
//! but that already type check, and optionally opt them in.

use std::collections::VecDeque;
use std::fs;
use std::path::PathBuf;

use anyhow::Context;
use anyhow::Result;
use anyhow::bail;
use clap::ValueHint;
use elp::build;
use elp::build::load;
use elp::build::load::LoadConfig;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp::otp_file_to_ignore;
use elp::sort_by_file_size_descending;
use elp_eqwalizer::Mode;
use elp_ide::Analysis;
use elp_ide::elp_ide_db::EqwalizerDatabase;
use elp_ide::elp_ide_db::EqwalizerDiagnostics;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::RootQueryDb;
use elp_project_model::AppName;
use elp_project_model::AppType;
use elp_project_model::DiscoverConfig;
use elp_project_model::buck::BuckQueryConfig;
use elp_syntax::AstNode;
use elp_syntax::ast;
use itertools::Itertools;

use crate::eqwalizer_cli::set_eqwalizer_config;
use crate::eqwalizer_cli::target_src_file_ids;
use crate::eqwalizer_cli::typecheck_chunks;
use crate::reporting;

const TYPING_ATTRIBUTE: &str = "-typing([eqwalizer]).";

#[derive(Clone, Debug, clap::Args)]
pub struct Onboard {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::AnyPath)]
    pub project: PathBuf,
    /// Rebar3 profile to pickup
    #[arg(long = "as", value_name = "PROFILE", default_value = "test")]
    pub profile: String,
    /// Run with rebar
    #[arg(long)]
    pub rebar: bool,
    /// Only consider the modules of this application
    #[arg(long, value_name = "APP", conflicts_with = "target")]
    pub app: Option<String>,
    /// Only consider the modules of this Buck target
    #[arg(long, value_name = "TARGET", conflicts_with = "rebar")]
    pub target: Option<String>,
    /// Add `-typing([eqwalizer]).` to the modules that type check
    #[arg(long)]
    pub opt_in: bool,
}

/// What type checking a module that is not opted in found.
enum Outcome {
    Errors(usize),
    ParseErrors,
    /// eqWAlizer failed on the module, for the given reason.
    CheckFailed(String),
}

struct Checked {
    file_id: FileId,
    path: PathBuf,
    outcome: Outcome,
}

pub fn onboard(args: &Onboard, cli: &mut dyn Cli, query_config: &BuckQueryConfig) -> Result<()> {
    let config = if args.target.is_some() {
        DiscoverConfig::buck()
    } else {
        DiscoverConfig::new(args.rebar, &args.profile)
    };
    let mut loaded = load::load_project_at(
        cli,
        &args.project,
        config,
        LoadConfig::new(Mode::Cli, *query_config),
    )?;
    build::compile_deps(&mut loaded, cli)?;
    set_eqwalizer_config(&mut loaded);
    let analysis = loaded.analysis();

    let candidates = candidates(&analysis, &loaded, args)?;
    if candidates.is_empty() {
        let message = if loaded.project.eqwalizer_config.enable_all {
            "eqWAlizer already checks every module. To opt modules in one at a time, set `enable_all = false` in the `[eqwalizer]` section of `.elp.toml`."
        } else {
            "Every module is already opted in to eqWAlizer or opted out of it."
        };
        cli.info(message)?;
        return Ok(());
    }

    let checked = check(&analysis, &loaded, candidates, cli)?;
    let (clean, with_errors): (Vec<_>, Vec<_>) = checked
        .into_iter()
        .partition(|checked| matches!(checked.outcome, Outcome::Errors(0)));

    writeln!(cli, "Modules that type check ({}):", clean.len())?;
    for checked in clean.iter().sorted_by(|a, b| a.path.cmp(&b.path)) {
        writeln!(cli, "  {}", checked.path.display())?;
    }
    writeln!(cli, "Modules with errors ({}):", with_errors.len())?;
    for checked in with_errors
        .iter()
        .sorted_by_key(|checked| match checked.outcome {
            Outcome::Errors(count) => (0, count, checked.path.clone()),
            Outcome::ParseErrors => (1, 0, checked.path.clone()),
            Outcome::CheckFailed(_) => (2, 0, checked.path.clone()),
        })
    {
        let description = match &checked.outcome {
            Outcome::Errors(1) => "1 error".to_string(),
            Outcome::Errors(count) => format!("{count} errors"),
            Outcome::ParseErrors => "parse errors".to_string(),
            Outcome::CheckFailed(reason) => format!("could not be checked ({reason})"),
        };
        writeln!(cli, "  {}: {description}", checked.path.display())?;
    }

    if clean.is_empty() {
        return Ok(());
    }
    if args.opt_in {
        for checked in &clean {
            add_typing_attribute(&analysis, &loaded, checked.file_id).with_context(|| {
                format!("adding {TYPING_ATTRIBUTE} to {}", checked.path.display())
            })?;
        }
        cli.info(&format!(
            "Added `{TYPING_ATTRIBUTE}` to {} module(s).",
            clean.len()
        ))?;
    } else {
        cli.info(&format!(
            "Run the same command with `--opt-in` to add `{TYPING_ATTRIBUTE}` to the {} module(s) that type check.",
            clean.len()
        ))?;
    }
    Ok(())
}

/// The project modules, in the application or target `args` select, that
/// eqWAlizer does not check, leaving out the ones explicitly opted out and
/// generated ones.
fn candidates(analysis: &Analysis, loaded: &LoadResult, args: &Onboard) -> Result<Vec<FileId>> {
    let ignore_patterns = &loaded
        .project
        .eqwalizer_config
        .ignore_modules_compiled_patterns;
    let module_index = analysis.module_index(loaded.project_input)?;
    let selected = match (&args.app, &args.target) {
        (_, Some(target)) => target_src_file_ids(target, loaded)?,
        (Some(app), None) => {
            let app = AppName(app.clone());
            let mut file_ids = Vec::new();
            for (_name, file_id) in module_index.iter_own() {
                if analysis.file_app_name(file_id)? == Some(app.clone()) {
                    file_ids.push(file_id);
                }
            }
            if file_ids.is_empty() {
                bail!("No modules found in application {app}");
            }
            file_ids
        }
        (None, None) => module_index
            .iter_own()
            .map(|(_name, file_id)| file_id)
            .collect(),
    };
    let mut candidates = Vec::new();
    for file_id in selected {
        let Some(name) = module_index.module_for_file(file_id) else {
            continue;
        };
        let is_candidate = analysis.file_app_type(file_id)? == Some(AppType::Project)
            && !otp_file_to_ignore(analysis, file_id)
            && !analysis.is_eqwalizer_enabled(file_id)?
            && !analysis.with_db(|db| db.has_eqwalizer_ignore_marker(file_id))?
            && !ignore_patterns
                .iter()
                .any(|pattern| pattern.matches(name.as_str()))
            && !analysis.is_generated(file_id)?;
        if is_candidate {
            candidates.push(file_id);
        }
    }
    Ok(candidates)
}

/// Type checks `candidates` with up to `max_tasks` eqWAlizer processes in
/// parallel. A module eqWAlizer fails on (it cannot get the module's forms,
/// or it times out) stops the whole process, so that module is recorded and
/// the rest of its chunk checked again.
fn check(
    analysis: &Analysis,
    loaded: &LoadResult,
    mut candidates: Vec<FileId>,
    cli: &dyn Cli,
) -> Result<Vec<Checked>> {
    let module_index = analysis.module_index(loaded.project_input)?;
    let max_tasks = loaded.project.eqwalizer_config.max_tasks.max(1);
    sort_by_file_size_descending(analysis, &mut candidates, |id| *id);
    let mut queue: VecDeque<Vec<FileId>> = elp_ide::distribute_round_robin(candidates, max_tasks)
        .into_iter()
        .filter(|chunk| !chunk.is_empty())
        .collect();
    let mut outcomes = Vec::new();
    while !queue.is_empty() {
        let round: Vec<_> = (0..max_tasks).map_while(|_| queue.pop_front()).collect();
        let pb = cli.progress(
            round.iter().map(Vec::len).sum::<usize>() as u64,
            "EqWAlizing",
        );
        for (chunk, output) in typecheck_chunks(loaded, round, pb.clone()) {
            let failed = match &*output {
                EqwalizerDiagnostics::Diagnostics { errors, .. } => {
                    for file_id in chunk {
                        let count = module_index
                            .module_for_file(file_id)
                            .and_then(|name| errors.get(name.as_str()))
                            .map_or(0, Vec::len);
                        outcomes.push((file_id, Outcome::Errors(count)));
                    }
                    continue;
                }
                EqwalizerDiagnostics::NoAst { module } => (
                    module.as_str(),
                    Outcome::ParseErrors,
                    format!("module {module} has parse errors"),
                ),
                EqwalizerDiagnostics::Error(error) => {
                    let (module, reason) = error
                        .strip_prefix("eqWAlizing module ")
                        .and_then(|rest| rest.split_once(":\n"))
                        .unwrap_or(("", error.as_str()));
                    let reason = reason.replace('\n', " ");
                    (module, Outcome::CheckFailed(reason.clone()), reason)
                }
            };
            let (module, outcome, reason) = failed;
            match module_index
                .file_for_module(module)
                .filter(|file_id| chunk.contains(file_id))
            {
                Some(failed) => {
                    outcomes.push((failed, outcome));
                    let rest: Vec<_> = chunk.into_iter().filter(|id| *id != failed).collect();
                    if !rest.is_empty() {
                        queue.push_back(rest);
                    }
                }
                // The failure cannot be pinned on a module of the chunk: check
                // each half on its own until it can.
                None => match chunk.as_slice() {
                    [file_id] => outcomes.push((*file_id, Outcome::CheckFailed(reason))),
                    _ => {
                        let mut first = chunk;
                        let second = first.split_off(first.len() / 2);
                        queue.push_back(first);
                        queue.push_back(second);
                    }
                },
            }
        }
        pb.finish();
    }
    outcomes
        .into_iter()
        .map(|(file_id, outcome)| {
            Ok(Checked {
                file_id,
                path: relative_path(analysis, loaded, file_id)?,
                outcome,
            })
        })
        .collect()
}

fn relative_path(analysis: &Analysis, loaded: &LoadResult, file_id: FileId) -> Result<PathBuf> {
    let root = analysis
        .project_root(file_id)?
        .context("could not find the project root")?;
    Ok(reporting::get_relative_path(&root, &loaded.file_path(file_id)).to_path_buf())
}

/// Inserts the `-typing([eqwalizer]).` attribute on the line after the
/// `-module` attribute.
fn add_typing_attribute(analysis: &Analysis, loaded: &LoadResult, file_id: FileId) -> Result<()> {
    let text = analysis.file_text(file_id)?;
    let module_attribute_end = analysis
        .with_db(|db| {
            db.parse(file_id)
                .tree()
                .forms()
                .find_map(|form| match form {
                    ast::Form::ModuleAttribute(attr) => Some(attr.syntax().text_range().end()),
                    _ => None,
                })
        })?
        .context("no -module attribute")?;
    let end = usize::from(module_attribute_end);
    let new_text = match text[end..].find('\n') {
        Some(newline) => {
            let line_end = end + newline + 1;
            let eol = if text[..line_end].ends_with("\r\n") {
                "\r\n"
            } else {
                "\n"
            };
            format!(
                "{}{TYPING_ATTRIBUTE}{eol}{}",
                &text[..line_end],
                &text[line_end..]
            )
        }
        None => format!("{text}\n{TYPING_ATTRIBUTE}\n"),
    };
    let path = loaded.file_path(file_id);
    let path = path.as_path().context("not a file on disk")?;
    fs::write(path, new_text)?;
    Ok(())
}
