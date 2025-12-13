/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Duration;
use std::time::SystemTime;

use anyhow::Result;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_log::telemetry;
use elp_project_model::ElpConfig;
use elp_project_model::IncludeParentDirs;
use elp_project_model::ProjectManifest;
use elp_project_model::buck::BuckQueryConfig;
use elp_project_model::otp::Otp;
use fxhash::FxHashMap;
use fxhash::FxHashSet;

pub struct ProjectLoader {
    pub(crate) project_roots: FxHashMap<AbsPathBuf, Option<ProjectManifest>>,
    start: SystemTime,
    initialized: bool,
}

impl ProjectLoader {
    pub fn new() -> Self {
        let mut project_roots = FxHashMap::default();
        let otp_root = Otp::find_otp();
        if let Ok(otp_root) = otp_root {
            let otp_root = AbsPathBuf::assert(otp_root);
            project_roots.insert(otp_root, None);
        }
        let start = SystemTime::now();
        let initialized = false;
        ProjectLoader {
            project_roots,
            start,
            initialized,
        }
    }

    pub fn clear(&mut self, paths: &FxHashSet<AbsPathBuf>) -> bool {
        let mut result = false;
        for path in paths {
            let mut path_it: &AbsPath = path.as_ref();
            loop {
                if self.project_roots.remove(path_it).is_some() {
                    result = true;
                    break;
                }
                match path_it.parent() {
                    Some(parent) => path_it = parent,
                    None => break,
                }
            }
        }
        result
    }

    pub fn load_manifest(
        &mut self,
        path: &AbsPath,
    ) -> (ElpConfig, Result<ProjectManifest>, ProjectManifest) {
        let (config, manifest) = match ProjectManifest::discover(path) {
            Ok((config, manifest)) => (config, Ok(manifest)),
            Err(x) => (ElpConfig::default(), Err(x)),
        };
        let fallback = ProjectManifest::discover_no_manifest(path, IncludeParentDirs::Yes);

        match manifest {
            Ok(manifest) => {
                if let Some(root) = manifest.root().parent() {
                    log::info!("Opening new project with root {:?}", &root);
                    self.project_roots
                        .insert(root.to_path_buf(), Some(manifest.clone()));
                }
                (config, Ok(manifest), fallback)
            }
            Err(err) => {
                //cache parent path not to discover project for every file without project
                if let Some(parent) = path.parent() {
                    self.project_roots
                        .insert(parent.to_path_buf(), Some(fallback.clone()));
                }
                (config, Err(err), fallback)
            }
        }
    }

    pub fn load_manifest_if_new(
        &mut self,
        path: &AbsPath,
    ) -> Option<(ElpConfig, Result<ProjectManifest>, ProjectManifest)> {
        let mut path_it = path;
        loop {
            if self.project_roots.contains_key(path_it) {
                match self.project_roots.get(path_it) {
                    Some(None) => {
                        return Some(self.load_manifest(path_it));
                    }
                    _ => {
                        return None;
                    }
                }
            }
            match path_it.parent() {
                Some(parent) => path_it = parent,
                None => break,
            }
        }

        Some(self.load_manifest(path))
    }

    pub fn load_completed(&mut self) {
        if !self.initialized {
            self.initialized = true;
            let diff = self.start.elapsed().map(|e| e.as_millis()).unwrap_or(0) as u32;
            let data = serde_json::Value::String("project_loading_completed".to_string());
            telemetry::send_with_duration(module_path!().to_string(), data, diff, self.start);
        }
    }
}

/// If using buck quick start, it happens in two stages, first to
/// get the basic project config, then to invoke the generation of
/// any artifacts that will become part of the project.
#[derive(Debug, PartialEq, Eq)]
pub enum BuckGenerated {
    /// Initial value
    NoLoadDone,
    /// After first load (buck targets)
    NoGenerated,
    /// After second load (elp.bxl)
    Generated,
}

pub struct ReloadManager {
    /// Files that have changed since the last reload.
    changed_files: FxHashSet<AbsPathBuf>,
    /// This field is updated when a `changed_files` file is added.
    /// It allows us to wait until the last file has been added
    /// when a branch switch is done to avoid doing a reload for each.
    /// We wait until RELOAD_QUIESCENT_WAIT_TIME has elapsed before doing
    /// the reload.
    last_change: SystemTime,
    /// ReloadManager clients should ensure this is set when a reload
    /// task is active, reset when done.
    reload_in_progress: bool,
    buck_generated: BuckGenerated,
    buck_quick_start: bool,
}

/// How long to wait after the last changed file was added before
/// processing them.
const RELOAD_QUIESCENT_WAIT_TIME: Duration = Duration::from_millis(500);

impl ReloadManager {
    pub fn new(buck_quick_start: bool) -> ReloadManager {
        ReloadManager {
            changed_files: FxHashSet::default(),
            last_change: SystemTime::now(),
            reload_in_progress: false,
            buck_generated: BuckGenerated::NoLoadDone,
            buck_quick_start,
        }
    }

    /// Used to check if any files are queued, and if so cancel an
    /// existing reload
    pub fn ok_to_switch_workspace(&self) -> bool {
        if self.buck_quick_start {
            // `BuckGenerated::NoLoadDone` or `BuckGenerated::NoGenerated`.

            if self.buck_generated == BuckGenerated::NoLoadDone {
                // We are doing a 2-stage load, and have just completed the `buck targets` step.
                // So time to activate the Project, this is the whole point of the two stage process
                true
            } else {
                self.changed_files.is_empty()
            }
        } else {
            // Do not switch if there are files which will trigger a reload.
            // This lets us start that process sooner without wasted effort
            // switching when it is going to change anyway.
            self.changed_files.is_empty()
        }
    }

    pub fn set_reload_active(&mut self) -> BuckQueryConfig {
        self.reload_in_progress = true;
        self.get_query_config()
    }

    pub fn get_query_config(&self) -> BuckQueryConfig {
        if self.buck_quick_start {
            match self.buck_generated {
                BuckGenerated::NoLoadDone => BuckQueryConfig::BuckTargetsOnly,
                BuckGenerated::NoGenerated => BuckQueryConfig::BuildGeneratedCode,
                BuckGenerated::Generated => BuckQueryConfig::BuildGeneratedCode,
            }
        } else {
            BuckQueryConfig::BuildGeneratedCode
        }
    }

    /// This is called when the `Task::FetchProject` is done in `server.rs`,
    /// but only after `switch_workspace_ok` has returned true.
    pub fn set_reload_done(&mut self, a_file_per_project: FxHashSet<AbsPathBuf>) {
        if self.buck_quick_start {
            match &self.buck_generated {
                BuckGenerated::NoLoadDone => {
                    if self.changed_files.is_empty() && !a_file_per_project.is_empty() {
                        // We have done the initial "buck targets" query on at least one Project,
                        // move on to doing `elp.bxl`
                        self.buck_generated = BuckGenerated::NoGenerated;
                        self.changed_files = a_file_per_project;
                    } else {
                        // We already have changed files from another source, so
                        // need to repeat this step. Do not change state.
                    }
                }
                BuckGenerated::NoGenerated => {
                    self.buck_generated = BuckGenerated::Generated;
                }
                BuckGenerated::Generated => {}
            };
        }
        self.reload_in_progress = false;
    }

    /// If there are changed files and `RELOAD_QUIESCENT_WAIT_TIME`
    /// has passed since the last was inserted, return and remove
    /// them, provided we do not have a currently active reload
    /// process.
    pub fn query_changed_files(&mut self) -> Option<FxHashSet<AbsPathBuf>> {
        if !self.reload_in_progress
            && !self.changed_files.is_empty()
            && self
                .last_change
                .elapsed()
                .unwrap_or_else(|_| Duration::from_millis(0))
                > RELOAD_QUIESCENT_WAIT_TIME
        {
            self.last_change = SystemTime::now();
            Some(std::mem::take(&mut self.changed_files))
        } else {
            None
        }
    }

    pub fn add(&mut self, path: AbsPathBuf) {
        if self.changed_files.insert(path) {
            // Only update the time if the path is newly added
            self.last_change = SystemTime::now();
        }
    }

    pub fn set_buck_quickstart(&mut self, buck_quick_start: bool) {
        self.buck_quick_start = buck_quick_start;
    }
}
