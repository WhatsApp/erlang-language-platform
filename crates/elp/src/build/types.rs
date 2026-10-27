/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::mem::ManuallyDrop;

use elp_ide::Analysis;
use elp_ide::AnalysisHost;
use elp_ide::elp_ide_db::EqwalizerProgressReporter;
use elp_ide::elp_ide_db::SidecarCleanupGuard;
use elp_ide::elp_ide_db::elp_base_db::ChangeKind;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FileMap;
use elp_ide::elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_project_model::Project;
use fxhash::FxHashSet;
use indicatif::ProgressBar;
use itertools::Itertools;

use crate::reload::PendingChanges;
use crate::reload::apply_source_roots;
use crate::reload::file_changes;
use crate::reload::write_file_changes;

/// Expensive-to-drop fields stay in `ManuallyDrop` so one-shot commands skip
/// the Salsa and file map destructor cascades. The cleanup guard still releases
/// external Erlang-service and Eqwalizer processes when this value leaves scope.
#[derive(Debug)]
pub struct LoadResult {
    pub analysis_host: ManuallyDrop<AnalysisHost>,
    _sidecar_cleanup: SidecarCleanupGuard,
    pub file_map: ManuallyDrop<FileMap>,
    pub project_id: ProjectId,
    pub project: Project,
    pub file_set_config: FileSetConfig,
}

impl LoadResult {
    pub fn new(
        analysis_host: AnalysisHost,
        file_map: FileMap,
        project_id: ProjectId,
        project: Project,
        file_set_config: FileSetConfig,
    ) -> Self {
        let sidecar_cleanup = analysis_host.raw_database().sidecar_cleanup_guard();
        LoadResult {
            analysis_host: ManuallyDrop::new(analysis_host),
            _sidecar_cleanup: sidecar_cleanup,
            file_map: ManuallyDrop::new(file_map),
            project_id,
            project,
            file_set_config,
        }
    }

    pub fn into_parts(self) -> (AnalysisHost, FileMap) {
        let LoadResult {
            analysis_host,
            file_map,
            ..
        } = self;
        (
            ManuallyDrop::into_inner(analysis_host),
            ManuallyDrop::into_inner(file_map),
        )
    }

    pub fn with_eqwalizer_progress_bar<R>(
        &self,
        pb: ProgressBar,
        f: impl FnOnce(Analysis) -> R,
    ) -> R {
        struct Reporter {
            bar: ProgressBar,
            current: FxHashSet<String>,
        }

        impl EqwalizerProgressReporter for Reporter {
            fn start_module(&mut self, module: String) {
                self.current.insert(module);
                let current = self.current.iter().join(", ");
                self.bar.set_message(current);
            }

            fn done_module(&mut self, module: &str) {
                self.current.remove(module);
                let current = self.current.iter().join(", ");
                self.bar.set_message(current);
                self.bar.inc(1);
            }
        }

        impl Drop for Reporter {
            fn drop(&mut self) {
                self.bar.set_message("")
            }
        }

        self.analysis_host
            .raw_database()
            .set_eqwalizer_progress_reporter(Some(Box::new(Reporter {
                bar: pb,
                current: Default::default(),
            })));

        let r = f(self.analysis());

        self.analysis_host
            .raw_database()
            .set_eqwalizer_progress_reporter(None);

        r
    }

    pub fn apply_pending_changes(&mut self, pending: PendingChanges) {
        let changes = file_changes(self.analysis_host.raw_database(), &self.file_map, pending);
        let created_or_deleted = changes
            .iter()
            .any(|change| change.kind != ChangeKind::Modify);
        let db = self.analysis_host.raw_database_mut();
        write_file_changes(db, changes);
        if created_or_deleted {
            apply_source_roots(db, &self.file_map, &self.file_set_config);
        }
    }

    /// The id of the file at `path`, if it exists.
    pub fn file_id(&self, path: &VfsPath) -> Option<FileId> {
        let db = self.analysis_host.raw_database();
        self.file_map
            .file_id(path)
            .filter(|file_id| db.file(*file_id).exists(db))
    }

    pub fn file_path(&self, file_id: FileId) -> VfsPath {
        let db = self.analysis_host.raw_database();
        db.file(file_id).path(db).clone()
    }

    pub fn analysis(&self) -> Analysis {
        self.analysis_host.analysis()
    }

    pub fn drop_erlang_services(&mut self) {
        self.analysis_host.raw_database_mut().drop_erlang_services();
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;

    use elp_ide::elp_ide_db::EqwalizerProgressReporter;

    use crate::build::fixture;

    struct DropReporter {
        dropped: Arc<AtomicBool>,
    }

    impl EqwalizerProgressReporter for DropReporter {
        fn start_module(&mut self, _module: String) {}

        fn done_module(&mut self, _module: &str) {}
    }

    impl Drop for DropReporter {
        fn drop(&mut self) {
            self.dropped.store(true, Ordering::SeqCst);
        }
    }

    #[test]
    fn dropping_load_result_skips_analysis_host_teardown() {
        let dropped = Arc::new(AtomicBool::new(false));
        let loaded = fixture::load_result(
            r#"
            //- /app/src/main.erl app:app
            -module(main).
            "#,
        );
        loaded
            .analysis_host
            .raw_database()
            .set_eqwalizer_progress_reporter(Some(Box::new(DropReporter {
                dropped: dropped.clone(),
            })));

        drop(loaded);

        assert!(
            !dropped.load(Ordering::SeqCst),
            "dropping LoadResult should skip the database destructor cascade"
        );
    }
}
