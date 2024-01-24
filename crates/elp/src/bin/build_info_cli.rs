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
use elp_project_model::buck;
use elp_project_model::otp::Otp;
use elp_project_model::ElpConfig;
use elp_project_model::IncludeParentDirs;
use elp_project_model::Project;
use elp_project_model::ProjectManifest;

use crate::args::BuildInfo;
use crate::args::ProjectInfo;

pub(crate) fn save_build_info(args: BuildInfo) -> Result<()> {
    let root = fs::canonicalize(&args.project)?;
    let root = AbsPathBuf::assert(root);
    let manifest = ProjectManifest::discover(&root);

    let buck = match manifest {
        Ok((_elp_config, ProjectManifest::TomlBuck(buck))) => buck,
        _ => bail!("Can't find buck root for {:?}", root),
    };

    let target_info = buck::load_buck_targets(&buck)?;
    let otp_root = Otp::find_otp()?;
    let project_app_data = buck::targets_to_project_data(&target_info.targets, &otp_root);
    let build_info_term = buck::build_info(&buck, &project_app_data, &otp_root);
    let writer = File::create(&args.to)?;
    build_info_term.encode(writer)?;
    Ok(())
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
    writer.write_all(b"================project_data================\n")?;
    writer.write_all(format!("{:#?}\n", &project).as_bytes())?;
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
