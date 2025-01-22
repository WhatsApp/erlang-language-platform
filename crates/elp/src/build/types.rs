/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::Vfs;
use elp_ide::elp_ide_db::EqwalizerProgressReporter;
use elp_ide::Analysis;
use elp_ide::AnalysisHost;
use elp_project_model::Project;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use indicatif::ProgressBar;
use itertools::Itertools;

use crate::line_endings::LineEndings;

#[derive(Debug)]
pub struct LoadResult {
    pub analysis_host: AnalysisHost,
    pub vfs: Vfs,
    pub line_ending_map: FxHashMap<FileId, LineEndings>,
    pub project_id: ProjectId,
    pub project: Project,
    pub file_set_config: FileSetConfig,
}

impl LoadResult {
    pub fn new(
        analysis_host: AnalysisHost,
        vfs: Vfs,
        line_ending_map: FxHashMap<FileId, LineEndings>,
        project_id: ProjectId,
        project: Project,
        file_set_config: FileSetConfig,
    ) -> Self {
        LoadResult {
            analysis_host,
            vfs,
            line_ending_map,
            project_id,
            project,
            file_set_config,
        }
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

    pub fn analysis(&self) -> Analysis {
        self.analysis_host.analysis()
    }

    pub fn update_erlang_service_paths(&self) {
        self.analysis_host
            .raw_database()
            .update_erlang_service_paths();
    }
}
