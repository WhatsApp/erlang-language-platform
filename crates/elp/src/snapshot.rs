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
use elp_ide::Cancellable;
use elp_ide::diagnostics;
use elp_ide::diagnostics::DiagnosticsConfig;
use elp_ide::diagnostics::DiagnosticsTrigger;
use elp_ide::diagnostics::LabeledDiagnostics;
use elp_ide::diagnostics::RemoveElpReported;
use elp_ide::diagnostics_collection::DiagnosticCollection;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FileMap;
use elp_ide::elp_ide_db::elp_base_db::LineEndings;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_log::timeit_with_telemetry;
use elp_project_model::Project;
use elp_project_model::ProjectBuildData;
use fxhash::FxHashMap;
use itertools::Itertools;
use lsp_types::SemanticTokens;
use lsp_types::Uri;
use parking_lot::Mutex;
use parking_lot::RwLock;
use serde::Deserialize;
use serde::Serialize;
use vfs::AnchoredPathBuf;

use crate::config::Config;
use crate::convert;
use crate::convert::uri_from_abs_path;
use crate::mem_docs::MemDocs;
use crate::server::EqwalizerTypes;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TelemetryData {
    NativeDiagnostics {
        file_uri: Uri,
    },
    EqwalizerDiagnostics {
        file_uri: Uri,
    },
    ParseServerDiagnostics {
        file_uri: Uri,
    },
    MetaDiagnostics {
        file_uri: Uri,
    },
    Initialize,
    References {
        file_uri: Uri,
        position: lsp_types::Position,
    },
}

impl fmt::Display for TelemetryData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TelemetryData::NativeDiagnostics { file_uri } => {
                write!(f, "Native Diagnostics file_uri: {}", file_uri)
            }
            TelemetryData::EqwalizerDiagnostics { file_uri } => {
                write!(f, "Eqwalizer Diagnostics file_uri: {}", file_uri)
            }
            TelemetryData::ParseServerDiagnostics { file_uri } => {
                write!(f, "Parse Server Diagnostics file_uri: {}", file_uri)
            }
            TelemetryData::MetaDiagnostics { file_uri } => {
                write!(f, "Meta Diagnostics file_uri: {}", file_uri)
            }
            TelemetryData::Initialize => {
                write!(f, "Initialize")
            }
            TelemetryData::References {
                file_uri,
                position: _,
            } => {
                write!(f, "Find References file_uri: {}", file_uri)
            }
        }
    }
}

/// An immutable snapshot of the world's state at a point in time.
#[derive(Clone)]
pub struct Snapshot {
    pub(crate) config: Arc<Config>,
    pub(crate) diagnostics_config: Arc<DiagnosticsConfig>,
    // Note: Analysis is a salsa::Snapshot.  According to the docs,
    // any attempt to `set` an input will block.
    pub(crate) analysis: Analysis,
    pub(crate) diagnostics: Arc<DiagnosticCollection>,
    pub(crate) eqwalizer_types: Arc<EqwalizerTypes>,
    pub(crate) semantic_tokens_cache: Arc<Mutex<FxHashMap<Uri, SemanticTokens>>>,
    file_map: Arc<FileMap>,
    pub(crate) mem_docs: Arc<RwLock<MemDocs>>,
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
        file_map: Arc<FileMap>,
        mem_docs: Arc<RwLock<MemDocs>>,
        projects: Arc<Vec<Project>>,
    ) -> Self {
        Snapshot {
            config,
            diagnostics_config,
            analysis,
            diagnostics,
            eqwalizer_types,
            semantic_tokens_cache: Arc::new(Default::default()),
            file_map,
            mem_docs,
            projects,
        }
    }

    pub(crate) fn uri_to_file_id(&self, uri: &Uri) -> Result<FileId> {
        let path = convert::vfs_path(uri)?;
        self.file_map
            .file_id(&path)
            .filter(|file_id| self.analysis.file_exists(*file_id))
            .context(format!("file not found: {path}"))
    }

    pub(crate) fn file_id_to_path(&self, id: FileId) -> Option<AbsPathBuf> {
        convert::abs_path(&self.file_id_to_uri(id)).ok()
    }

    pub(crate) fn file_id_to_uri(&self, id: FileId) -> Uri {
        let path = self.analysis.file_path(id);
        convert::uri_from_abs_path(path.as_path().unwrap())
    }

    pub(crate) fn uri_file_version(&self, uri: &Uri) -> Option<i32> {
        let path = convert::vfs_path(uri).ok()?;
        self.mem_docs.read().get(&path).map(|v| v.version)
    }

    pub(crate) fn line_endings(&self, id: FileId) -> LineEndings {
        self.analysis.line_endings(id)
    }

    pub(crate) fn anchored_path(&self, path: &AnchoredPathBuf) -> Option<Uri> {
        let mut base = self.analysis.file_path(path.anchor);
        base.pop();
        let path = base.join(&path.path)?;
        let path = path.as_path()?;
        Some(uri_from_abs_path(path))
    }

    pub fn update_cache_for_file(&self, file_id: FileId) -> Result<()> {
        let _ = self.analysis.def_map(file_id)?;
        Ok(())
    }

    pub fn native_diagnostics(
        &self,
        file_id: FileId,
        include_otp: bool,
        trigger: &DiagnosticsTrigger,
    ) -> Cancellable<Option<LabeledDiagnostics>> {
        if !include_otp && self.is_otp(file_id) {
            return Ok(None);
        }

        let file_uri = self.file_id_to_uri(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::NativeDiagnostics { file_uri });

        self.analysis
            .native_diagnostics(&self.diagnostics_config.clone(), trigger, &vec![], file_id)
            .map(Some)
    }

    pub fn eqwalizer_diagnostics(
        &self,
        file_id: FileId,
        include_otp: bool,
    ) -> Cancellable<Option<Vec<diagnostics::Diagnostic>>> {
        if !include_otp && self.is_otp(file_id) {
            return Ok(None);
        }

        let file_uri = self.file_id_to_uri(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::EqwalizerDiagnostics { file_uri });
        self.analysis.eqwalizer_diagnostics_for_file(file_id)
    }

    pub fn eqwalizer_types(
        &self,
        file_id: FileId,
        include_otp: bool,
    ) -> Cancellable<Option<Arc<Vec<(Pos, Type)>>>> {
        if !include_otp && self.is_otp(file_id) {
            return Ok(None);
        }

        let file_uri = self.file_id_to_uri(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::EqwalizerDiagnostics { file_uri });
        self.analysis.types_for_file(file_id)
    }

    pub fn erlang_service_diagnostics(
        &self,
        file_id: FileId,
        config: &DiagnosticsConfig,
    ) -> Cancellable<Option<Vec<(FileId, LabeledDiagnostics)>>> {
        if !config.include_otp && self.is_otp(file_id) {
            return Ok(None);
        }

        let file_uri = self.file_id_to_uri(file_id);
        let _timer = timeit_with_telemetry!(TelemetryData::ParseServerDiagnostics {
            file_uri: file_uri.clone()
        });

        let diags =
            &*self
                .analysis
                .erlang_service_diagnostics(file_id, config, RemoveElpReported::Yes)?;

        Ok(Some(
            diags
                .iter()
                .map(|(file_id, ds)| (*file_id, ds.clone()))
                .collect(),
        ))
    }

    pub fn get_project(&self, project_id: ProjectId) -> Option<Project> {
        self.projects
            .iter()
            .enumerate()
            .find_or_first(|(id, _project)| ProjectId(*id as u32) == project_id)
            .map(|(_id, project)| project.clone())
    }

    /// Compiles the projects' dependencies. Returns whether any project has
    /// dependencies to compile: only rebar projects do.
    pub fn set_up_projects(&self) -> bool {
        let mut compiles_deps = false;
        for project in self.projects.as_ref() {
            compiles_deps |= matches!(project.project_build_data, ProjectBuildData::Rebar(_));
            if let Err(err) = set_up_project(project) {
                log::error!(
                    "Failed to set up project {} for parsing: {}",
                    project.name(),
                    err
                );
            };
        }
        compiles_deps
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
