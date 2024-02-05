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
use elp_project_model::buck::EqwalizerConfig;
use elp_project_model::json::JsonConfig;
use elp_project_model::json::JsonProjectAppData;
use elp_project_model::AppName;
use elp_project_model::ElpConfig;
use elp_project_model::IncludeParentDirs;
use elp_project_model::Project;
use elp_project_model::ProjectManifest;

use crate::args::BuildInfo;
use crate::args::ProjectInfo;

pub(crate) fn save_build_info(args: BuildInfo) -> Result<()> {
    let root = fs::canonicalize(&args.project)?;
    let root = AbsPathBuf::assert(root);
    let (_elp_config, manifest) = ProjectManifest::discover(&root)?;

    let project = Project::load(&manifest, EqwalizerConfig::default())?;
    let project_app_data = project.non_otp_apps().cloned().collect::<Vec<_>>();
    let root_without_file = if root.as_path().as_ref().is_file() {
        root.parent().unwrap_or(&root).to_path_buf()
    } else {
        root
    };

    if args.json {
        let mut writer = File::create(&args.to)?;
        let json_app_data: Vec<_> = project_app_data
            .iter()
            .filter_map(|project_app_data| {
                if project_app_data.name == AppName("eqwalizer_support".to_string()) {
                    // This is derived from OTP when the project is loaded again
                    None
                } else {
                    Some(JsonProjectAppData::from_project_app_data(
                        &root_without_file,
                        project_app_data,
                    ))
                }
            })
            .collect();
        let json = JsonConfig {
            apps: json_app_data,
            deps: vec![],
            config_path: None,
        };

        let json_str = serde_json::to_string_pretty::<JsonConfig>(&json)?;
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

pub(crate) fn save_project_info(args: ProjectInfo) -> Result<()> {
    let root = fs::canonicalize(&args.project)?;
    let root = AbsPathBuf::assert(root);
    let (manifest, project) = load_project(&root).or_else(|_| load_fallback(&root))?;

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
    writer.write_all(b"================manifest================\n")?;
    writer.write_all(format!("{:#?}\n", &manifest).as_bytes())?;
    writer.write_all(b"================project_build_data================\n")?;
    writer.write_all(format!("{:#?}\n", &project.project_build_data).as_bytes())?;
    writer.write_all(b"================project_app_data================\n")?;
    writer.write_all(format!("{:#?}\n", &project.project_apps).as_bytes())?;
    Ok(())
}

fn load_project(root: &AbsPath) -> Result<(ProjectManifest, Project)> {
    let (elp_config, manifest) = ProjectManifest::discover(root)?;
    let project = Project::load(&manifest, elp_config.eqwalizer)?;
    Ok((manifest, project))
}

fn load_fallback(root: &AbsPath) -> Result<(ProjectManifest, Project)> {
    let manifest = ProjectManifest::discover_no_manifest(root, IncludeParentDirs::Yes);
    let elp_config = ElpConfig::default();
    let project = Project::load(&manifest, elp_config.eqwalizer)?;
    Ok((manifest, project))
}
