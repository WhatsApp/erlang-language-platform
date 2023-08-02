/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod load;
pub mod types;

use anyhow::Result;
use elp_project_model::ProjectBuildData::Rebar;

use crate::build::types::LoadResult;
use crate::cli::Cli;

pub fn compile_deps(loaded: &LoadResult, cli: &dyn Cli) -> Result<()> {
    match loaded.project.project_build_data {
        Rebar(_) => {
            let pb = cli.spinner("Compiling dependencies");
            loaded.project.compile_deps()?;
            loaded.update_erlang_service_paths();
            pb.finish();
        }
        _ => (),
    }
    Ok(())
}
