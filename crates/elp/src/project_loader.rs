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
        if let Ok(otp_root) = Otp::find_otp() {
            project_roots.insert(AbsPathBuf::assert(otp_root.to_path_buf()), None);
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
                    log::info!("Opening new project with root {:?}", root);
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
                return None;
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

/// The buck project load happens in two stages, first to get the
/// basic project config, then to invoke the generation of any
/// artifacts that will become part of the project.
#[derive(Debug, PartialEq, Eq)]
pub enum BuckGenerated {
    /// Initial value
    NoLoadDone,
    /// After a load that reported no buck project. There is no `elp.bxl`
    /// stage to run for what has been loaded so far, but a buck project
    /// discovered later in the session still needs one, so this is not an
    /// end state.
    NoBuckProject,
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
}

/// How long to wait after the last changed file was added before
/// processing them.
const RELOAD_QUIESCENT_WAIT_TIME: Duration = Duration::from_millis(500);

impl ReloadManager {
    pub fn new() -> ReloadManager {
        ReloadManager {
            changed_files: FxHashSet::default(),
            last_change: SystemTime::now(),
            reload_in_progress: false,
            buck_generated: BuckGenerated::NoLoadDone,
        }
    }

    /// Used to check if any files are queued, and if so cancel an
    /// existing reload
    pub fn ok_to_switch_workspace(&self) -> bool {
        if self.buck_generated == BuckGenerated::NoLoadDone {
            // We have just completed the `buck targets` step. So time to
            // activate the Project, this is the whole point of the two stage
            // process
            true
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
        match self.buck_generated {
            BuckGenerated::NoLoadDone => BuckQueryConfig::BuckTargetsOnly,
            BuckGenerated::NoBuckProject => BuckQueryConfig::BuckTargetsOnly,
            BuckGenerated::NoGenerated => BuckQueryConfig::BuildGeneratedCode,
            BuckGenerated::Generated => BuckQueryConfig::BuildGeneratedCode,
        }
    }

    /// This is called when the `Task::FetchProject` is done in `server.rs`,
    /// but only after `switch_workspace_ok` has returned true.
    pub fn set_reload_done(&mut self, a_file_per_project: FxHashSet<AbsPathBuf>) {
        match &self.buck_generated {
            // We already have changed files from another source, so
            // need to repeat this step. Do not change state.
            BuckGenerated::NoLoadDone if !self.changed_files.is_empty() => {}
            // This load saw no buck project, so it has no `elp.bxl` stage to
            // run. Move out of `NoLoadDone` anyway: that state makes
            // `ok_to_switch_workspace` skip the changed-files check, and a
            // workspace that never leaves it would lose the check for the
            // whole session.
            BuckGenerated::NoLoadDone | BuckGenerated::NoBuckProject
                if a_file_per_project.is_empty() =>
            {
                self.buck_generated = BuckGenerated::NoBuckProject;
            }
            BuckGenerated::NoLoadDone | BuckGenerated::NoBuckProject => {
                // We have done the initial "buck targets" query on at least one Project,
                // move on to doing `elp.bxl`
                self.buck_generated = BuckGenerated::NoGenerated;
                self.changed_files.extend(a_file_per_project);
            }
            BuckGenerated::NoGenerated => {
                self.buck_generated = BuckGenerated::Generated;
            }
            BuckGenerated::Generated => {}
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
}

#[cfg(test)]
mod tests {
    use std::time::SystemTime;

    use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
    use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;
    use fxhash::FxHashMap;
    use fxhash::FxHashSet;
    use paths::Utf8PathBuf;

    use super::BuckGenerated;
    use super::BuckQueryConfig;
    use super::ProjectLoader;
    use super::ReloadManager;

    fn abs(path: &str) -> AbsPathBuf {
        AbsPathBuf::assert(Utf8PathBuf::from(path))
    }

    // Build a loader with an empty root map directly, so the tests don't depend on
    // `Otp::find_otp()` (which `ProjectLoader::new` calls and which is environment
    // sensitive). The child module can reach the private fields.
    fn empty_loader() -> ProjectLoader {
        ProjectLoader {
            project_roots: FxHashMap::default(),
            start: SystemTime::now(),
            initialized: false,
        }
    }

    fn path_set(items: &[&str]) -> FxHashSet<AbsPathBuf> {
        items.iter().map(|p| abs(p)).collect()
    }

    #[test]
    fn clear_returns_false_for_unknown_paths() {
        let mut loader = empty_loader();
        loader.project_roots.insert(abs("/tmp/elp/proj_a"), None);

        let cleared = loader.clear(&path_set(&["/tmp/elp/proj_b"]));

        assert!(!cleared, "clear should return false when no root matches");
        assert!(
            loader.project_roots.contains_key(&abs("/tmp/elp/proj_a")),
            "the unrelated root must remain"
        );
    }

    #[test]
    fn clear_removes_exact_root_and_reports_true() {
        let mut loader = empty_loader();
        loader.project_roots.insert(abs("/tmp/elp/proj_a"), None);

        let cleared = loader.clear(&path_set(&["/tmp/elp/proj_a"]));

        assert!(
            cleared,
            "clear should return true when the exact root is present"
        );
        assert!(
            !loader.project_roots.contains_key(&abs("/tmp/elp/proj_a")),
            "the matched root must be removed"
        );
    }

    #[test]
    fn clear_walks_up_to_ancestor_root() {
        let mut loader = empty_loader();
        loader.project_roots.insert(abs("/tmp/elp/proj_a"), None);

        // A file deep inside the project resolves to its ancestor root.
        let cleared = loader.clear(&path_set(&["/tmp/elp/proj_a/src/mod/foo.erl"]));

        assert!(
            cleared,
            "clear should walk parent dirs to find the ancestor root"
        );
        assert!(
            !loader.project_roots.contains_key(&abs("/tmp/elp/proj_a")),
            "the ancestor root must be removed via the parent walk"
        );
    }

    #[test]
    fn load_manifest_if_new_skips_known_root_and_children() {
        let mut loader = empty_loader();
        loader.project_roots.insert(abs("/tmp/elp/proj_a"), None);

        // The exact known root is skipped (no reload, no filesystem discovery)...
        assert!(
            loader
                .load_manifest_if_new(abs("/tmp/elp/proj_a").as_ref())
                .is_none(),
            "a known root must not be reloaded"
        );
        // ...and so is any file beneath it (an ancestor is already known). This is
        // the intra-batch dedup the reload refactor relies on when it collects
        // manifests under the lock before releasing it for the buck2 load.
        assert!(
            loader
                .load_manifest_if_new(abs("/tmp/elp/proj_a/src/foo.erl").as_ref())
                .is_none(),
            "a child of a known root must not trigger a new load"
        );
        // The skipped lookups must not have inserted anything new.
        assert_eq_expected!(1, loader.project_roots.len());
    }

    #[test]
    fn buck_workspace_runs_both_load_stages() {
        let mut manager = ReloadManager::new();

        // Stage 1 is the bare `buck targets` query...
        assert_eq_expected!(BuckQueryConfig::BuckTargetsOnly, manager.get_query_config());
        assert!(
            manager.ok_to_switch_workspace(),
            "the project must be activated as soon as stage 1 lands"
        );

        // ...and completing it queues a file per project to drive stage 2.
        manager.set_reload_done(path_set(&["/tmp/elp/proj_a/src"]));
        assert_eq_expected!(BuckGenerated::NoGenerated, manager.buck_generated);
        assert_eq_expected!(
            BuckQueryConfig::BuildGeneratedCode,
            manager.get_query_config()
        );

        manager.set_reload_done(FxHashSet::default());
        assert_eq_expected!(BuckGenerated::Generated, manager.buck_generated);
    }

    #[test]
    fn non_buck_workspace_keeps_the_changed_files_guard() {
        let mut manager = ReloadManager::new();

        // No buck project, so the load reports no file to generate for. There is
        // no stage 2 to wait for, so the state machine must leave `NoLoadDone`.
        manager.set_reload_done(FxHashSet::default());
        assert_eq_expected!(BuckGenerated::NoBuckProject, manager.buck_generated);

        // Which is what keeps the guard alive: a queued config change must hold
        // off the workspace switch until the reload that it triggers is done.
        manager.add(abs("/tmp/elp/proj_a/rebar.config"));
        assert!(
            !manager.ok_to_switch_workspace(),
            "a queued changed file must block the switch"
        );
    }

    #[test]
    fn a_buck_project_found_after_a_non_buck_load_still_runs_stage_2() {
        let mut manager = ReloadManager::new();

        // The first project opened in the session is not a buck one.
        manager.set_reload_done(FxHashSet::default());
        assert_eq_expected!(BuckGenerated::NoBuckProject, manager.buck_generated);

        // Opening a file in a buck project later in the session loads it via its
        // own stage 1, so `NoBuckProject` must not be an end state: the queued
        // file has to survive to drive `elp.bxl` for it.
        manager.set_reload_done(path_set(&["/tmp/elp/proj_b/src"]));
        assert_eq_expected!(BuckGenerated::NoGenerated, manager.buck_generated);
        assert_eq_expected!(
            BuckQueryConfig::BuildGeneratedCode,
            manager.get_query_config()
        );
        let expected_queued = path_set(&["/tmp/elp/proj_b/src"]);
        assert_eq_expected!(expected_queued, manager.changed_files);

        manager.set_reload_done(FxHashSet::default());
        assert_eq_expected!(BuckGenerated::Generated, manager.buck_generated);
    }

    #[test]
    fn repeated_non_buck_loads_stay_in_no_buck_project() {
        let mut manager = ReloadManager::new();

        manager.set_reload_done(FxHashSet::default());
        manager.set_reload_done(FxHashSet::default());

        // Still the stage 1 query, since no buck project has been seen yet.
        assert_eq_expected!(BuckGenerated::NoBuckProject, manager.buck_generated);
        assert_eq_expected!(BuckQueryConfig::BuckTargetsOnly, manager.get_query_config());
    }

    #[test]
    fn changed_files_during_the_first_load_repeat_it() {
        let mut manager = ReloadManager::new();

        // A change that arrives while stage 1 is in flight invalidates it, so the
        // state must not advance and the next query is stage 1 again.
        manager.add(abs("/tmp/elp/proj_a/BUCK"));
        manager.set_reload_done(path_set(&["/tmp/elp/proj_a/src"]));

        assert_eq_expected!(BuckGenerated::NoLoadDone, manager.buck_generated);
        assert_eq_expected!(BuckQueryConfig::BuckTargetsOnly, manager.get_query_config());
    }
}
