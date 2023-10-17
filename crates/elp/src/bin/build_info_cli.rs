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

use anyhow::bail;
use anyhow::Result;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_project_model::buck;
use elp_project_model::otp::Otp;
use elp_project_model::DiscoverConfig;
use elp_project_model::ProjectManifest;

use crate::args::BuildInfo;

pub(crate) fn save_build_info(args: BuildInfo) -> Result<()> {
    let root = fs::canonicalize(&args.project)?;
    let root = AbsPathBuf::assert(root);
    let manifest = ProjectManifest::discover_single(&root, &DiscoverConfig::buck());

    let config = match manifest {
        Ok(ProjectManifest::Toml(buck)) => buck,
        _ => bail!("Can't find buck root for {:?}", root),
    };

    let target_info = buck::load_buck_targets(&config.buck)?;
    let project_app_data = buck::targets_to_project_data(&target_info.targets);
    let otp_root = Otp::find_otp()?;
    let build_info_term = buck::build_info(&config.buck, &project_app_data, &otp_root);
    let writer = File::create(&args.to)?;
    build_info_term.encode(writer)?;
    Ok(())
}
