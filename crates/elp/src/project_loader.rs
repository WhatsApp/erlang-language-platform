/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Instant;

use anyhow::Result;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_log::telemetry;
use elp_project_model::otp::Otp;
use elp_project_model::ProjectManifest;
use fxhash::FxHashMap;

pub struct ProjectLoader {
    pub(crate) project_roots: FxHashMap<AbsPathBuf, Option<ProjectManifest>>,
    start: Instant,
    initialized: bool,
}

impl ProjectLoader {
    pub fn new() -> Self {
        let mut project_roots = FxHashMap::default();
        let otp_root = Otp::find_otp().unwrap();
        let otp_root = AbsPathBuf::assert(otp_root);
        project_roots.insert(otp_root, None);
        let start = Instant::now();
        let initialized = false;
        ProjectLoader {
            project_roots,
            start,
            initialized,
        }
    }

    pub fn clear(&mut self, paths: &Vec<AbsPathBuf>) -> bool {
        let mut result = false;
        for path in paths {
            let mut path_it: &AbsPath = path.as_ref();
            while let Some(path) = path_it.parent() {
                if self.project_roots.remove(path).is_some() {
                    result = true;
                    break;
                }
                path_it = path;
            }
        }
        result
    }

    pub fn load_manifest(&mut self, path: &AbsPath) -> Result<Option<ProjectManifest>> {
        let manifest = ProjectManifest::discover(path);

        match manifest {
            Ok(Some(manifest)) => {
                if let Some(root) = manifest.root().parent() {
                    log::info!("Opening new project with root {:?}", &root);
                    self.project_roots
                        .insert(root.to_path_buf(), Some(manifest.clone()));
                }
                Ok(Some(manifest))
            }
            Ok(None) => {
                log::info!("Could not find a project manifest for path {:?}", path);
                Ok(None)
            }
            Err(err) => {
                log::error!(
                    "Project discovery failed for path {:?}, error {}",
                    path,
                    err
                );
                //cache parent path not to discover project for every file without project
                if let Some(parent) = path.parent() {
                    self.project_roots.insert(parent.to_path_buf(), None);
                }
                Ok(None)
            }
        }
    }

    pub fn load_manifest_if_new(&mut self, path: &AbsPath) -> Result<Option<ProjectManifest>> {
        let mut path_it = path;
        while let Some(path) = path_it.parent() {
            if self.project_roots.contains_key(path) {
                return Ok(None);
            }
            path_it = path;
        }
        self.load_manifest(path)
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
