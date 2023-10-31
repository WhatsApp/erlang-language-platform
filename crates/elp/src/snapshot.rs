/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::sync::Arc;

use anyhow::Context;
use anyhow::Result;
use elp_ai::AiCompletion;
use elp_ai::CompletionReceiver;
use elp_ide::diagnostics::LabeledDiagnostics;
use elp_ide::diagnostics::LintsFromConfig;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FilePosition;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::Vfs;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_ide::elp_ide_db::EqwalizerDiagnostics;
use elp_ide::Analysis;
use elp_log::timeit_with_telemetry;
use elp_project_model::Project;
use fxhash::FxHashMap;
use itertools::Itertools;
use lsp_types::Diagnostic;
use lsp_types::SemanticTokens;
use lsp_types::Url;
use parking_lot::Mutex;
use parking_lot::RwLock;
use serde::Deserialize;
use serde::Serialize;

use crate::config::Config;
use crate::convert;
use crate::line_endings::LineEndings;
use crate::server::file_id_to_path;
use crate::server::file_id_to_url;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TelemetryData {
    NativeDiagnostics { file_url: Url },
    EqwalizerDiagnostics { file_url: Url },
    ParseServerDiagnostics { file_url: Url },
    EdocDiagnostics { file_url: Url },
    MetaDiagnostics { file_url: Url },
    CommonTestDiagnostics { file_url: Url },
    Initialize,
}

impl fmt::Display for TelemetryData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TelemetryData::NativeDiagnostics { file_url } => {
                write!(f, "Native Diagnostics file_url: {}", file_url)
            }
            TelemetryData::EqwalizerDiagnostics { file_url } => {
                write!(f, "Eqwalizer Diagnostics file_url: {}", file_url)
            }
            TelemetryData::ParseServerDiagnostics { file_url } => {
                write!(f, "Parse Server Diagnostics file_url: {}", file_url)
            }
            TelemetryData::EdocDiagnostics { file_url } => {
                write!(f, "EDoc Diagnostics file_url: {}", file_url)
            }
            TelemetryData::MetaDiagnostics { file_url } => {
                write!(f, "Meta Diagnostics file_url: {}", file_url)
            }
            TelemetryData::CommonTestDiagnostics { file_url } => {
                write!(f, "CT Diagnostics file_url: {}", file_url)
            }
            TelemetryData::Initialize => {
                write!(f, "Initialize")
            }
        }
    }
}

pub type SharedMap<Key, Value> = Arc<RwLock<FxHashMap<Key, Value>>>;

/// An immutable snapshot of the world's state at a point in time.
pub struct Snapshot {
    pub(crate) config: Arc<Config>,
    pub(crate) ad_hoc_lints: Arc<LintsFromConfig>,
    // Note: Analysis is a salsa::Snapshot.  According to the docs,
    // any attempt to `set` an input will block.
    pub(crate) analysis: Analysis,
    pub(crate) semantic_tokens_cache: Arc<Mutex<FxHashMap<Url, SemanticTokens>>>,
    vfs: Arc<RwLock<Vfs>>,
    open_document_versions: SharedMap<VfsPath, i32>,
    line_ending_map: SharedMap<FileId, LineEndings>,
    pub(crate) projects: Arc<Vec<Project>>,
    ai_completion: Arc<Mutex<AiCompletion>>,
}

impl Snapshot {
    pub fn new(
        config: Arc<Config>,
        ad_hoc_lints: Arc<LintsFromConfig>,
        analysis: Analysis,
        vfs: Arc<RwLock<Vfs>>,
        open_document_versions: Arc<RwLock<FxHashMap<VfsPath, i32>>>,
        line_ending_map: Arc<RwLock<FxHashMap<FileId, LineEndings>>>,
        projects: Arc<Vec<Project>>,
        ai_completion: Arc<Mutex<AiCompletion>>,
    ) -> Self {
        Snapshot {
            config,
            ad_hoc_lints,
            analysis,
            semantic_tokens_cache: Arc::new(Default::default()),
            vfs,
            open_document_versions,
            line_ending_map,
            projects,
            ai_completion,
        }
    }

    pub(crate) fn url_to_file_id(&self, url: &Url) -> Result<FileId> {
        let path = convert::vfs_path(url)?;
        let vfs = self.vfs.read();
        let res = vfs
            .file_id(&path)
            .context(format!("file not found: {}", path))?;
        Ok(res)
    }

    pub(crate) fn file_id_to_path(&self, id: FileId) -> Option<AbsPathBuf> {
        file_id_to_path(&self.vfs.read(), id).ok()
    }

    pub(crate) fn file_id_to_url(&self, id: FileId) -> Url {
        file_id_to_url(&self.vfs.read(), id)
    }

    pub(crate) fn url_file_version(&self, url: &Url) -> Option<i32> {
        let path = convert::vfs_path(url).ok()?;
        Some(*self.open_document_versions.read().get(&path)?)
    }

    pub(crate) fn line_endings(&self, id: FileId) -> LineEndings {
        self.line_ending_map.read()[&id]
    }

    pub(crate) fn ai_completion(&self, position: FilePosition) -> Result<CompletionReceiver> {
        let mut ai_completion = self.ai_completion.lock();
        let code = self.analysis.file_text(position.file_id)?;
        let offset = u32::from(position.offset) as usize;
        let prefix = &code[..offset];
        Ok(ai_completion.complete(prefix.to_string()))
    }

    pub fn native_diagnostics(&self, file_id: FileId) -> Option<LabeledDiagnostics<Diagnostic>> {
        let file_url = self.file_id_to_url(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::NativeDiagnostics { file_url });

        let line_index = self.analysis.line_index(file_id).ok()?;
        let url = file_id_to_url(&self.vfs.read(), file_id);

        Some(
            self.analysis
                .diagnostics(
                    &self.config.diagnostics(self.ad_hoc_lints.clone()),
                    file_id,
                    false,
                )
                .ok()?
                .convert(&|d| convert::ide_to_lsp_diagnostic(&line_index, &url, d)),
        )
    }

    pub fn eqwalizer_diagnostics(&self, file_id: FileId) -> Option<Vec<Diagnostic>> {
        log::warn!("snapshot.eqwalizer_diagnostics:{:?}", &file_id);
        let file_url = self.file_id_to_url(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::EqwalizerDiagnostics { file_url });

        // Check, if the file is actually a module
        let _ = self.analysis.module_name(file_id).ok()??;

        let project_id = self.analysis.project_id(file_id).ok()??;
        log::warn!("snapshot.eqwalizer_diagnostics:p{:?}", &file_id);

        let eqwalizer_enabled = self.analysis.is_eqwalizer_enabled(file_id, false).ok()?;
        if !eqwalizer_enabled {
            return Some(vec![]);
        }

        let line_index = self.analysis.line_index(file_id).ok()?;
        log::warn!("snapshot.eqwalizer_diagnostics:l:{:?}", &file_id);

        let diags = self
            .analysis
            .eqwalizer_diagnostics(project_id, vec![file_id])
            .ok()?;
        log::warn!("snapshot.eqwalizer_diagnostics:d:{:?}", &file_id);
        match &*diags {
            EqwalizerDiagnostics::Diagnostics { errors, .. } => Some(
                errors
                    .iter()
                    .flat_map(|(_, diags)| {
                        diags.iter().map(|d| {
                            convert::eqwalizer_to_lsp_diagnostic(d, &line_index, eqwalizer_enabled)
                        })
                    })
                    .collect(),
            ),
            EqwalizerDiagnostics::NoAst { .. } => Some(vec![]),
            EqwalizerDiagnostics::Error(err) => {
                log::error!("EqWAlizer failed for {:?}: {}", file_id, err);
                Some(vec![])
            }
        }
    }

    pub fn edoc_diagnostics(&self, file_id: FileId) -> Option<Vec<(FileId, Vec<Diagnostic>)>> {
        let file_url = self.file_id_to_url(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::EdocDiagnostics {
            file_url: file_url.clone()
        });
        let line_index = self.analysis.line_index(file_id).ok()?;

        let diags = &*self.analysis.edoc_diagnostics(file_id).ok()?;

        Some(
            diags
                .iter()
                .map(|(file_id, ds)| {
                    (
                        *file_id,
                        ds.iter()
                            .map(|d| convert::ide_to_lsp_diagnostic(&line_index, &file_url, d))
                            .collect(),
                    )
                })
                .collect(),
        )
    }

    pub fn ct_diagnostics(&self, file_id: FileId) -> Option<Vec<Diagnostic>> {
        let file_url = self.file_id_to_url(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::CommonTestDiagnostics {
            file_url: file_url.clone()
        });
        let line_index = self.analysis.line_index(file_id).ok()?;

        let diags = &*self.analysis.ct_diagnostics(file_id).ok()?;

        Some(
            diags
                .iter()
                .map(|d| convert::ide_to_lsp_diagnostic(&line_index, &file_url, d))
                .collect(),
        )
    }

    pub fn erlang_service_diagnostics(
        &self,
        file_id: FileId,
    ) -> Option<Vec<(FileId, LabeledDiagnostics<lsp_types::Diagnostic>)>> {
        log::warn!("erlang_service diagnostics:{:?}", &file_id);
        let file_url = self.file_id_to_url(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::ParseServerDiagnostics {
            file_url: file_url.clone()
        });
        let line_index = self.analysis.line_index(file_id).ok()?;

        let diags = &*self.analysis.erlang_service_diagnostics(file_id).ok()?;

        Some(
            diags
                .iter()
                .map(|(file_id, ds)| {
                    (
                        *file_id,
                        ds.convert(&|d| convert::ide_to_lsp_diagnostic(&line_index, &file_url, d)),
                    )
                })
                .collect(),
        )
    }

    pub fn get_project(&self, project_id: ProjectId) -> Option<Project> {
        self.projects
            .iter()
            .enumerate()
            .find_or_first(|(id, _project)| ProjectId(*id as u32) == project_id)
            .map(|(_id, project)| project.clone())
    }

    pub fn set_up_projects(&self) {
        for project in self.projects.as_ref() {
            if let Err(err) = set_up_project(project) {
                log::error!(
                    "Failed to set up project {} for parsing: {}",
                    project.name(),
                    err
                );
            };
        }
    }
}

fn set_up_project(project: &Project) -> Result<()> {
    project.compile_deps()
}
