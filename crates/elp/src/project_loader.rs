/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Instant;

use anyhow::bail;
use anyhow::Result;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_log::telemetry;
use elp_project_model::otp::Otp;
use elp_project_model::DiscoverConfig;
use elp_project_model::ProjectManifest;
use fxhash::FxHashSet;

pub struct ProjectLoader {
    project_roots: FxHashSet<AbsPathBuf>,
    start: Instant,
    initialized: bool,
}

impl ProjectLoader {
    pub fn new() -> Self {
        let mut project_roots = FxHashSet::default();
        let otp_root = Otp::find_otp().unwrap();
        let otp_root = AbsPathBuf::assert(otp_root);
        project_roots.insert(otp_root);
        let start = Instant::now();
        let initialized = false;
        ProjectLoader {
            project_roots,
            start,
            initialized,
        }
    }

    pub fn load_manifest_if_new(&mut self, path: &AbsPath) -> Result<Option<ProjectManifest>> {
        let mut path_it = path;
        while let Some(path) = path_it.parent() {
            if self.project_roots.contains(path) {
                return Ok(None);
            }
            path_it = path;
        }
        let conf = DiscoverConfig::buck();
        let manifest = match ProjectManifest::discover_single(&path, &conf) {
            Ok(manifest) => Ok(manifest),
            Err(buck_err) => ProjectManifest::discover_single(&path, &conf.to_rebar())
                .map_err(|rebar_err| (buck_err, rebar_err)),
        };

        match manifest {
            Ok(manifest) => {
                if let Some(root) = manifest.root().parent() {
                    log::info!("Opening new project with root {:?}", &root);
                    self.project_roots.insert(root.to_path_buf());
                }
                Ok(Some(manifest))
            }
            Err((buck_err, rebar_err)) => {
                log::warn!(
                    "Couldn't open neither buck nor rebar project for path {:?}. buck err: {:?}, rebar err: {:?}",
                    path,
                    buck_err,
                    rebar_err
                );
                bail!(
                    "Couldn't open neither buck nor rebar project for path {:?}",
                    path
                )
            }
        }
    }

    pub fn load_completed(&mut self) {
        if !self.initialized {
            self.initialized = true;
            let diff = self.start.elapsed().as_millis() as u32;
            let data = serde_json::Value::String("project_loading_completed".to_string());
            telemetry::send_with_duration(module_path!().to_string(), data, diff);
        }
    }
}
