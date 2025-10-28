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

use anyhow::Result;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_project_model::AppType;
use elp_project_model::ElpConfig;
use elp_project_model::IncludeParentDirs;
use elp_project_model::Project;
use elp_project_model::ProjectAppData;
use elp_project_model::ProjectBuildData;
use elp_project_model::ProjectManifest;
use elp_project_model::buck::BuckQueryConfig;
use elp_project_model::buck::BuckTarget;
use elp_project_model::buck::query_buck_targets;
use elp_project_model::json::JsonConfig;
use fxhash::FxHashMap;

use crate::args::BuildInfo;
use crate::args::ProjectInfo;

pub(crate) fn save_build_info(args: BuildInfo, query_config: &BuckQueryConfig) -> Result<()> {
    let root = fs::canonicalize(&args.project)?;
    let root = AbsPathBuf::assert_utf8(root);
    let (elp_config, manifest) = ProjectManifest::discover(&root)?;
    let project = Project::load(&manifest, &elp_config, query_config, &|_| {})?;
    let mut writer = File::create(&args.to)?;
    let json_str = serde_json::to_string_pretty::<JsonConfig>(&project.as_json(root))?;
    writer.write_all(json_str.as_bytes())?;
    Ok(())
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
    let root = AbsPathBuf::assert_utf8(root);
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

    if args.buck_query
        && let ProjectBuildData::Buck(buck) = &project.project_build_data
    {
        let buck_targets_query = query_buck_targets(&buck.buck_conf, query_config);
        if let Ok(targets) = &buck_targets_query {
            writer.write_all(format!("{:#?}\n", sort_buck_targets(targets)).as_bytes())?;
        } else {
            writer.write_all(format!("{:#?}\n", &buck_targets_query).as_bytes())?;
        }
    } else if args.target_types {
        writer.write_all(b"================target types================\n")?;
        for line in buck_targets_and_types(&project.project_apps) {
            writer.write_all(format!("{}\n", line).as_bytes())?;
        }
    } else {
        writer.write_all(b"================manifest================\n")?;
        writer.write_all(format!("{:#?}\n", &manifest).as_bytes())?;
        writer.write_all(b"================project_build_data================\n")?;
        writer.write_all(format!("{:#?}\n", &project.project_build_data).as_bytes())?;
        writer.write_all(b"================project_app_data================\n")?;
        writer.write_all(format!("{:#?}\n", &project.project_apps).as_bytes())?;
    }
    Ok(())
}

fn sort_buck_targets(hash_map: &FxHashMap<String, BuckTarget>) -> Vec<(String, &BuckTarget)> {
    let mut vec = hash_map
        .iter()
        .map(|(n, t)| (format!("target_name:{}", n), t))
        .collect::<Vec<_>>();
    vec.sort_by(|a, b| a.0.cmp(&b.0));
    vec
}

fn buck_targets_and_types(apps: &[ProjectAppData]) -> Vec<String> {
    let tn = |tn| -> String {
        if let Some(tn) = tn {
            tn
        } else {
            "".to_string()
        }
    };
    let mut vec = apps
        .iter()
        .filter(|app| app.app_type != AppType::Otp)
        .filter(|app| app.is_buck_generated != Some(true))
        .map(|app| {
            format!(
                "{:?} {:<30} {}",
                app.app_type,
                app.name,
                tn(app.buck_target_name.clone())
            )
        })
        .collect::<Vec<_>>();
    vec.sort();
    vec
}

fn load_project(
    root: &AbsPath,
    query_config: &BuckQueryConfig,
) -> Result<(ProjectManifest, Project)> {
    let (elp_config, manifest) = ProjectManifest::discover(root)?;
    let project = Project::load(&manifest, &elp_config, query_config, &|_| {})?;
    Ok((manifest, project))
}

fn load_fallback(
    root: &AbsPath,
    query_config: &BuckQueryConfig,
) -> Result<(ProjectManifest, Project)> {
    let manifest = ProjectManifest::discover_no_manifest(root, IncludeParentDirs::Yes);
    let elp_config = ElpConfig::default();
    let project = Project::load(&manifest, &elp_config, query_config, &|_| {})?;
    Ok((manifest, project))
}
