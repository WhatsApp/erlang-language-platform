/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::sync::Arc;

use anyhow::Context;
use anyhow::Result;
use elp_eqwalizer::ast::Pos;
use elp_eqwalizer::types::Type;
use elp_ide::Analysis;
use elp_ide::diagnostics;
use elp_ide::diagnostics::DiagnosticsConfig;
use elp_ide::diagnostics::LabeledDiagnostics;
use elp_ide::diagnostics::RemoveElpReported;
use elp_ide::diagnostics_collection::DiagnosticCollection;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FileKind;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::Vfs;
use elp_log::timeit_with_telemetry;
use elp_project_model::Project;
use fxhash::FxHashMap;
use itertools::Itertools;
use lsp_types::SemanticTokens;
use lsp_types::Url;
use parking_lot::Mutex;
use parking_lot::RwLock;
use serde::Deserialize;
use serde::Serialize;

use crate::config::Config;
use crate::convert;
use crate::line_endings::LineEndings;
use crate::mem_docs::MemDocs;
use crate::server::EqwalizerTypes;
use crate::server::file_id_to_path;
use crate::server::file_id_to_url;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TelemetryData {
    NativeDiagnostics { file_url: Url },
    EqwalizerDiagnostics { file_url: Url },
    EqwalizerProjectDiagnostics { project_name: String },
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
                write!(f, "Native Diagnostics file_url: {file_url}")
            }
            TelemetryData::EqwalizerDiagnostics { file_url } => {
                write!(f, "Eqwalizer Diagnostics file_url: {file_url}")
            }
            TelemetryData::EqwalizerProjectDiagnostics { project_name } => {
                write!(
                    f,
                    "Eqwalizer Project Diagnostics project_name: {project_name}"
                )
            }
            TelemetryData::ParseServerDiagnostics { file_url } => {
                write!(f, "Parse Server Diagnostics file_url: {file_url}")
            }
            TelemetryData::EdocDiagnostics { file_url } => {
                write!(f, "EDoc Diagnostics file_url: {file_url}")
            }
            TelemetryData::MetaDiagnostics { file_url } => {
                write!(f, "Meta Diagnostics file_url: {file_url}")
            }
            TelemetryData::CommonTestDiagnostics { file_url } => {
                write!(f, "CT Diagnostics file_url: {file_url}")
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
    pub(crate) diagnostics_config: Arc<DiagnosticsConfig>,
    // Note: Analysis is a salsa::Snapshot.  According to the docs,
    // any attempt to `set` an input will block.
    pub(crate) analysis: Analysis,
    pub(crate) diagnostics: Arc<DiagnosticCollection>,
    pub(crate) eqwalizer_types: Arc<EqwalizerTypes>,
    pub(crate) semantic_tokens_cache: Arc<Mutex<FxHashMap<Url, SemanticTokens>>>,
    vfs: Arc<RwLock<Vfs>>,
    pub(crate) mem_docs: Arc<RwLock<MemDocs>>,
    line_ending_map: SharedMap<FileId, LineEndings>,
    pub(crate) projects: Arc<Vec<Project>>,
}

impl Snapshot {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        config: Arc<Config>,
        diagnostics_config: Arc<DiagnosticsConfig>,
        analysis: Analysis,
        diagnostics: Arc<DiagnosticCollection>,
        eqwalizer_types: Arc<EqwalizerTypes>,
        vfs: Arc<RwLock<Vfs>>,
        mem_docs: Arc<RwLock<MemDocs>>,
        line_ending_map: Arc<RwLock<FxHashMap<FileId, LineEndings>>>,
        projects: Arc<Vec<Project>>,
    ) -> Self {
        Snapshot {
            config,
            diagnostics_config,
            analysis,
            diagnostics,
            eqwalizer_types,
            semantic_tokens_cache: Arc::new(Default::default()),
            vfs,
            mem_docs,
            line_ending_map,
            projects,
        }
    }

    pub(crate) fn url_to_file_id(&self, url: &Url) -> Result<FileId> {
        let path = convert::vfs_path(url)?;
        let vfs = self.vfs.read();
        let (res, _) = vfs
            .file_id(&path)
            .context(format!("file not found: {path}"))?;
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
        self.mem_docs.read().get(&path).map(|v| v.version)
    }

    pub(crate) fn line_endings(&self, id: FileId) -> LineEndings {
        self.line_ending_map.read()[&id]
    }

    pub fn update_cache_for_file(
        &self,
        file_id: FileId,
        optimize_for_eqwalizer: bool,
    ) -> Result<()> {
        let _ = self.analysis.def_map(file_id)?;
        if optimize_for_eqwalizer {
            let should_eqwalize = self.analysis.should_eqwalize(file_id, false)?;
            if should_eqwalize {
                let _ = self.analysis.module_ast(file_id)?;
            }
        }
        Ok(())
    }

    pub fn native_diagnostics(
        &self,
        file_id: FileId,
        include_otp: bool,
    ) -> Option<LabeledDiagnostics> {
        if !include_otp && self.is_otp(file_id) {
            return None;
        }

        let file_url = self.file_id_to_url(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::NativeDiagnostics { file_url });

        self.analysis
            .native_diagnostics(&self.diagnostics_config.clone(), &vec![], file_id)
            .ok()
    }

    pub fn eqwalizer_diagnostics(
        &self,
        file_id: FileId,
        include_otp: bool,
    ) -> Option<Vec<diagnostics::Diagnostic>> {
        if !include_otp && self.is_otp(file_id) {
            return None;
        }

        let file_url = self.file_id_to_url(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::EqwalizerDiagnostics { file_url });
        self.analysis.eqwalizer_diagnostics_for_file(file_id).ok()?
    }

    pub fn eqwalizer_project_diagnostics(
        &self,
        project_id: ProjectId,
        max_tasks: usize,
    ) -> Option<Vec<(FileId, Vec<diagnostics::Diagnostic>)>> {
        let module_index = self.analysis.module_index(project_id).ok()?;

        let file_ids: Vec<FileId> = module_index
            .iter_own()
            .filter_map(|(_, _, file_id)| {
                if let Ok(true) = self.analysis.should_eqwalize(file_id, false) {
                    Some(file_id)
                } else {
                    None
                }
            })
            .collect();

        log::info!(
            "Calculating eqwalizer diagnostics for {} files",
            file_ids.len()
        );

        let project_name = match self.get_project(project_id) {
            Some(project) => project.name(),
            None => "undefined".to_string(),
        };

        let _timer =
            timeit_with_telemetry!(TelemetryData::EqwalizerProjectDiagnostics { project_name });

        self.analysis
            .eqwalizer_diagnostics_by_project(project_id, file_ids, max_tasks)
            .ok()?
    }

    pub fn eqwalizer_types(
        &self,
        file_id: FileId,
        include_otp: bool,
    ) -> Option<Arc<Vec<(Pos, Type)>>> {
        if !include_otp && self.is_otp(file_id) {
            return None;
        }

        let file_url = self.file_id_to_url(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::EqwalizerDiagnostics { file_url });
        self.analysis.types_for_file(file_id).ok()?
    }

    pub fn edoc_diagnostics(
        &self,
        file_id: FileId,
        include_otp: bool,
        config: &DiagnosticsConfig,
    ) -> Option<Vec<(FileId, Vec<diagnostics::Diagnostic>)>> {
        let file_kind = self.analysis.file_kind(file_id).ok()?;
        if file_kind != FileKind::SrcModule && file_kind != FileKind::TestModule {
            return None;
        }

        if !include_otp && self.is_otp(file_id) {
            return None;
        }

        let file_url = self.file_id_to_url(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::EdocDiagnostics {
            file_url: file_url.clone()
        });

        let diags = &*self.analysis.edoc_diagnostics(file_id, config).ok()?;

        Some(
            diags
                .iter()
                .map(|(file_id, ds)| (*file_id, ds.clone()))
                .collect(),
        )
    }

    pub fn ct_diagnostics(
        &self,
        file_id: FileId,
        config: &DiagnosticsConfig,
    ) -> Option<Vec<diagnostics::Diagnostic>> {
        let file_kind = self.analysis.file_kind(file_id).ok()?;
        if file_kind != FileKind::TestModule {
            return None;
        }

        if !config.include_otp && self.is_otp(file_id) {
            return None;
        }

        let file_url = self.file_id_to_url(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::CommonTestDiagnostics {
            file_url: file_url.clone()
        });

        self.analysis.ct_diagnostics(file_id, config).ok()
    }

    pub fn erlang_service_diagnostics(
        &self,
        file_id: FileId,
        config: &DiagnosticsConfig,
    ) -> Option<Vec<(FileId, LabeledDiagnostics)>> {
        if !config.include_otp && self.is_otp(file_id) {
            return None;
        }

        let file_url = self.file_id_to_url(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::ParseServerDiagnostics {
            file_url: file_url.clone()
        });

        let diags = &*self
            .analysis
            .erlang_service_diagnostics(file_id, config, RemoveElpReported::Yes)
            .ok()?;

        Some(
            diags
                .iter()
                .map(|(file_id, ds)| (*file_id, ds.clone()))
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

    pub fn workspace_root(&self, file_id: FileId) -> AbsPathBuf {
        let project_data = self.analysis.project_data(file_id);
        match project_data {
            Ok(Some(project_data)) => project_data.root_dir.clone(),
            _ => self.config.root_path.clone(),
        }
    }

    fn is_otp(&self, file_id: FileId) -> bool {
        match self.analysis.is_otp(file_id) {
            Ok(is_otp) => Some(true) == is_otp,
            Err(_) => false,
        }
    }
}

fn set_up_project(project: &Project) -> Result<()> {
    project.compile_deps()
}
