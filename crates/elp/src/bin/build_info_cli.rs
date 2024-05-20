/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::fs::File;
use std::io::Write;

use anyhow::bail;
use anyhow::Result;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_project_model::buck::query_buck_targets_raw;
use elp_project_model::buck::BuckQueryConfig;
use elp_project_model::json::JsonConfig;
use elp_project_model::ElpConfig;
use elp_project_model::EqwalizerConfig;
use elp_project_model::IncludeParentDirs;
use elp_project_model::Project;
use elp_project_model::ProjectBuildData;
use elp_project_model::ProjectManifest;

use crate::args::BuildInfo;
use crate::args::ProjectInfo;

pub(crate) fn save_build_info(args: BuildInfo, query_config: &BuckQueryConfig) -> Result<()> {
    let root = fs::canonicalize(&args.project)?;
    let root = AbsPathBuf::assert(root);
    let (_elp_config, manifest) = ProjectManifest::discover(&root)?;
    let project = Project::load(&manifest, EqwalizerConfig::default(), query_config)?;
    if args.json {
        let mut writer = File::create(&args.to)?;
        let json_str = serde_json::to_string_pretty::<JsonConfig>(&project.as_json(root))?;
        writer.write_all(json_str.as_bytes())?;
        Ok(())
    } else {
        if let Some(build_info_file) = project.build_info_file() {
            std::fs::copy(build_info_file, args.to)?;
            Ok(())
        } else {
            bail!("Loaded project does not have build info generated")
        }
    }
}

pub(crate) fn save_project_info(args: ProjectInfo, query_config: &BuckQueryConfig) -> Result<()> {
    let mut writer: Box<dyn Write> = match args.to {
        Some(to) => Box::new(
            std::fs::OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(to)?,
        ),
        None => Box::new(std::io::stdout()),
    };
    let root = fs::canonicalize(&args.project)?;
    let root = AbsPathBuf::assert(root);
    let (manifest, project) = match load_project(&root, query_config) {
        Ok(res) => res,
        Err(err) => {
            writer.write_all(
                format!("could not load manifest:\n-----\n{err},\n-----\nfalling back\n")
                    .as_bytes(),
            )?;
            load_fallback(&root, query_config)?
        }
    };

    if args.buck_query {
        if let ProjectBuildData::Buck(buck) = &project.project_build_data {
            let buck_targets_query = query_buck_targets_raw(&buck.buck_conf);
            writer.write_all(b"================buck targets query raw================\n")?;
            writer.write_all(format!("{:#?}\n", &buck_targets_query).as_bytes())?;
        };
    }
    writer.write_all(b"================manifest================\n")?;
    writer.write_all(format!("{:#?}\n", &manifest).as_bytes())?;
    writer.write_all(b"================project_build_data================\n")?;
    writer.write_all(format!("{:#?}\n", &project.project_build_data).as_bytes())?;
    writer.write_all(b"================project_app_data================\n")?;
    writer.write_all(format!("{:#?}\n", &project.project_apps).as_bytes())?;
    Ok(())
}

fn load_project(
    root: &AbsPath,
    query_config: &BuckQueryConfig,
) -> Result<(ProjectManifest, Project)> {
    let (elp_config, manifest) = ProjectManifest::discover(root)?;
    let project = Project::load(&manifest, elp_config.eqwalizer, query_config)?;
    Ok((manifest, project))
}

fn load_fallback(
    root: &AbsPath,
    query_config: &BuckQueryConfig,
) -> Result<(ProjectManifest, Project)> {
    let manifest = ProjectManifest::discover_no_manifest(root, IncludeParentDirs::Yes);
    let elp_config = ElpConfig::default();
    let project = Project::load(&manifest, elp_config.eqwalizer, query_config)?;
    Ok((manifest, project))
}
