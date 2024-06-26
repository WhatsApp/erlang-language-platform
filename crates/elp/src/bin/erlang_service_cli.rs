/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::path::Path;
use std::str;

use anyhow::Context;
use anyhow::Error;
use anyhow::Result;
use elp::build;
use elp::build::load;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp::convert;
use elp::otp_file_to_ignore;
use elp_eqwalizer::Mode;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::erlang_service;
use elp_ide::erlang_service::DiagnosticLocation;
use elp_ide::Analysis;
use elp_log::timeit;
use elp_project_model::buck::BuckQueryConfig;
use elp_project_model::AppType;
use elp_project_model::DiscoverConfig;
use indicatif::ParallelProgressIterator;
use rayon::prelude::*;

use crate::args::ParseAll;
use crate::reporting;
use crate::reporting::ParseDiagnostic;

pub fn parse_all(args: &ParseAll, cli: &mut dyn Cli, query_config: &BuckQueryConfig) -> Result<()> {
    let config = DiscoverConfig::new(!args.buck, &args.profile);
    let loaded = load::load_project_at(
        cli,
        &args.project,
        config,
        IncludeOtp::No,
        Mode::Cli,
        query_config,
    )?;
    build::compile_deps(&loaded, cli)?;
    fs::create_dir_all(&args.to)?;
    let format = erlang_service::Format::OffsetEtf;

    let parse_diagnostics = do_parse_all(cli, &loaded, &args.to, format, &args.module, args.buck)?;
    if !parse_diagnostics.is_empty() {
        writeln!(
            cli,
            "{}",
            reporting::format_raw_parse_error(&parse_diagnostics)
        )
        .unwrap();
        return Err(Error::msg("Parsing failed with diagnostics."));
    }
    Ok(())
}

pub fn do_parse_all(
    cli: &dyn Cli,
    loaded: &LoadResult,
    to: &Path,
    format: erlang_service::Format,
    module: &Option<String>,
    buck: bool,
) -> Result<Vec<ParseDiagnostic>> {
    let file_cnt = loaded.vfs.len();
    let _timer = timeit!("parse {} files", file_cnt);

    let pb = cli.progress(file_cnt as u64, "Parsing modules");
    let mut result = loaded
        .analysis()
        .module_index(loaded.project_id)?
        .iter_own()
        .par_bridge()
        .progress_with(pb)
        .map_with(
            loaded.analysis(),
            move |db, (name, _, file_id)| -> Result<Vec<ParseDiagnostic>> {
                let empty = Ok(vec![]);
                match module {
                    Some(module) if module != name.as_str() => {
                        return empty;
                    }
                    _ => {}
                }
                if !buck && db.file_app_type(file_id).ok() == Some(Some(AppType::Dep)) {
                    return empty;
                }

                do_parse_one(db, Some((name, to)), file_id, format)
                    .with_context(|| format!("Failed to parse module {}", name.as_str()))
            },
        )
        .try_reduce(Vec::new, |mut acc, diagnostics| {
            acc.extend(diagnostics);
            Ok(acc)
        })?;
    result.sort_by(|f, l| f.relative_path.cmp(&l.relative_path));
    Ok(result)
}

pub fn do_parse_one(
    db: &Analysis,
    to: Option<(&str, &Path)>,
    file_id: FileId,
    format: erlang_service::Format,
) -> Result<Vec<ParseDiagnostic>> {
    if format == erlang_service::Format::Text {
        panic!("text format is for test purposes only!")
    }

    if otp_file_to_ignore(db, file_id) {
        return Ok(vec![]);
    }

    let result = db.module_ast(file_id, format, vec![], vec![])?;
    if result.is_ok() {
        if let Some((name, to)) = to {
            let to_path = to.join(format!("{}.etf", name));
            fs::write(to_path, &*result.ast)?;
        }
        Ok(vec![])
    } else {
        let line_index = db.line_index(file_id)?;
        let root_dir = &db.project_data(file_id)?.unwrap().root_dir;
        let diagnostic = result
            .errors
            .iter()
            .chain(result.warnings.iter())
            .map(|err| {
                let relative_path: &Path = err.path.strip_prefix(root_dir).unwrap_or(&err.path);
                let (range, line_num) = match err.location {
                    None => (None, convert::position(&line_index, 0.into()).line + 1),
                    Some(DiagnosticLocation::Normal(range)) => (
                        Some(range),
                        convert::position(&line_index, range.start()).line + 1,
                    ),
                    Some(DiagnosticLocation::Included {
                        directive_location,
                        error_location: _,
                    }) => (
                        Some(directive_location),
                        convert::position(&line_index, directive_location.start()).line + 1,
                    ),
                };
                ParseDiagnostic {
                    file_id,
                    relative_path: relative_path.to_owned(),
                    line_num,
                    msg: err.msg.to_owned(),
                    range,
                }
            })
            .collect();
        Ok(diagnostic)
    }
}

// ---------------------------------------------------------------------
