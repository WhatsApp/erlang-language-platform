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
use std::panic::AssertUnwindSafe;
use std::panic::RefUnwindSafe;
use std::path::PathBuf;
use std::sync::Arc;

use elp_base_db::AbsPathBuf;
use elp_base_db::AppData;
use elp_base_db::AppDataId;
use elp_base_db::AppDataInput;
use elp_base_db::FileId;
use elp_base_db::FilePosition;
use elp_base_db::FileRange;
use elp_base_db::FileSourceRootInput;
use elp_base_db::FileText;
use elp_base_db::Files;
use elp_base_db::ProjectData;
use elp_base_db::ProjectDataInput;
use elp_base_db::ProjectId;
use elp_base_db::RootQueryDb;
use elp_base_db::SourceAppDataInput;
use elp_base_db::SourceDatabase;
use elp_base_db::SourceRoot;
use elp_base_db::SourceRootId;
use elp_base_db::SourceRootInput;
use elp_base_db::Upcast;
use elp_base_db::limit_logged_string;
use elp_base_db::salsa;
use elp_eqwalizer::EqwalizerConfig;
use elp_eqwalizer::Mode;
use elp_eqwalizer::db::EqwalizerDiagnosticsDatabase;
use elp_eqwalizer::ipc::IpcHandle;
use elp_syntax::AstNode;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxToken;
use elp_types_db::TypedSemantic;
use elp_types_db::eqwalizer::types::Type;
use erlang_service::Connection;
use fxhash::FxHashMap;
use helpers::pick_best_token;
use hir::InFile;
use hir::Semantic;
use hir::db::DefDatabase;
use hir::db::InternDatabase;
use parking_lot::Mutex;
use parking_lot::RwLock;
use parking_lot::RwLockUpgradableReadGuard;
use paths::Utf8PathBuf;
use salsa::Database;
use serde::Deserialize;
use serde::Serialize;

mod apply_change;
pub mod common_test;
mod defs;
pub mod diagnostic_code;
pub mod docs;
pub mod eqwalizer;
mod erl_ast;
mod line_index;
// @fb-only
pub mod metadata;
mod search;

// ---------------------------------------------------------------------
pub mod assists;
pub mod helpers;
pub mod rename;
pub mod source_change;

pub use defs::ReferenceClass;
pub use defs::ReferenceType;
pub use defs::SymbolClass;
pub use defs::SymbolDefinition;
pub use diagnostic_code::DiagnosticCode;
pub use elp_base_db;
pub use elp_base_db::impl_intern_key;
pub use elp_eqwalizer::Eqwalizer;
pub use elp_eqwalizer::EqwalizerDiagnostic;
pub use elp_eqwalizer::EqwalizerDiagnostics;
pub use elp_erlang_service as erlang_service;
pub use eqwalizer::EqwalizerDatabase;
pub use erl_ast::ErlAstDatabase;
pub use line_index::LineCol;
pub use line_index::LineIndex;
pub use search::FindUsages;
pub use search::ReferenceCategory;
pub use search::SearchScope;
pub use search::UsageSearchResult;

pub type FxIndexMap<K, V> =
    indexmap::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

// ---------------------------------------------------------------------

type EqwalizerProgressReporterBox =
    Arc<AssertUnwindSafe<Mutex<Option<Box<dyn EqwalizerProgressReporter>>>>>;

pub trait EqwalizerProgressReporter: Send + Sync + RefUnwindSafe {
    fn start_module(&mut self, module: String);
    fn done_module(&mut self, module: &str);
}

#[salsa::db]
#[allow(clippy::type_complexity)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
    files: Arc<Files>,
    erlang_services: Arc<AssertUnwindSafe<RwLock<FxHashMap<ProjectId, Connection>>>>,
    eqwalizer: Eqwalizer,
    eqwalizer_progress_reporter: EqwalizerProgressReporterBox,
    ipc_handles: Arc<AssertUnwindSafe<RwLock<FxHashMap<String, Arc<Mutex<IpcHandle>>>>>>,
}
impl Default for RootDatabase {
    fn default() -> Self {
        let mut db = RootDatabase {
            storage: salsa::Storage::default(),
            files: Arc::default(),
            erlang_services: Arc::default(),
            eqwalizer: Eqwalizer::default(),
            eqwalizer_progress_reporter: EqwalizerProgressReporterBox::default(),
            ipc_handles: Arc::default(),
        };
        db.set_eqwalizer_config(Arc::new(EqwalizerConfig::default()));
        db
    }
}

impl RefUnwindSafe for RootDatabase {}

impl Clone for RootDatabase {
    fn clone(&self) -> Self {
        Self {
            storage: self.storage.clone(),
            files: self.files.clone(),
            erlang_services: self.erlang_services.clone(),
            eqwalizer: self.eqwalizer.clone(),
            eqwalizer_progress_reporter: self.eqwalizer_progress_reporter.clone(),
            ipc_handles: self.ipc_handles.clone(),
        }
    }
}

impl Upcast<dyn RootQueryDb> for RootDatabase {
    fn upcast(&self) -> &(dyn RootQueryDb + 'static) {
        self
    }
}

impl Upcast<dyn InternDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn InternDatabase + 'static) {
        self
    }
}

impl Upcast<dyn DefDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn DefDatabase + 'static) {
        self
    }
}

impl fmt::Debug for RootDatabase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RootDatabase").finish_non_exhaustive()
    }
}

#[salsa::db]
impl salsa::Database for RootDatabase {
    fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
}

#[salsa::db]
impl SourceDatabase for RootDatabase {
    fn file_text(&self, file_id: FileId) -> FileText {
        self.files.file_text(file_id)
    }

    fn set_file_text(&mut self, file_id: FileId, text: Arc<str>) {
        let files = self.files.clone();
        files.set_file_text(self, file_id, text);
    }

    fn source_root(&self, source_root_id: SourceRootId) -> SourceRootInput {
        self.files.source_root(source_root_id)
    }

    fn set_source_root(&mut self, source_root_id: SourceRootId, source_root: Arc<SourceRoot>) {
        let files = self.files.clone();
        files.set_source_root(self, source_root_id, source_root);
    }

    fn file_source_root(&self, id: FileId) -> FileSourceRootInput {
        self.files.file_source_root(id)
    }

    fn set_file_source_root(&mut self, id: FileId, source_root_id: SourceRootId) {
        let files = self.files.clone();
        files.set_file_source_root(self, id, source_root_id);
    }

    fn app_data_by_id(&self, id: AppDataId) -> AppDataInput {
        self.files.app_data(id)
    }

    fn set_app_data_by_id(&mut self, id: AppDataId, app_data: Option<Arc<AppData>>) {
        let files = self.files.clone();
        files.set_app_data(self, id, app_data)
    }

    fn app_data_id(&self, source_root_id: SourceRootId) -> SourceAppDataInput {
        self.files.source_app_data(source_root_id)
    }

    fn set_app_data_id(&mut self, id: SourceRootId, app_data_id: AppDataId) {
        let files = self.files.clone();
        files.set_source_app_data(self, id, app_data_id)
    }

    fn project_data(&self, project_id: ProjectId) -> ProjectDataInput {
        self.files.project_data(project_id)
    }

    fn set_project_data(&mut self, id: ProjectId, project_data: Arc<ProjectData>) {
        let files = self.files.clone();
        files.set_project_data(self, id, project_data)
    }
}

impl RootDatabase {
    pub fn snapshot(&self) -> Self {
        Self {
            storage: self.storage.clone(),
            files: self.files.clone(),
            erlang_services: self.erlang_services.clone(),
            eqwalizer: self.eqwalizer.clone(),
            eqwalizer_progress_reporter: self.eqwalizer_progress_reporter.clone(),
            ipc_handles: self.ipc_handles.clone(),
        }
    }

    pub fn request_cancellation(&mut self) {
        let _p = tracing::info_span!("RootDatabase::request_cancellation").entered();
        self.synthetic_write(salsa::Durability::LOW);
    }

    pub fn clear_erlang_services(&mut self) {
        self.erlang_services.write().clear();
    }

    pub fn erlang_service_for(&self, project_id: ProjectId) -> Connection {
        let read = self.erlang_services.upgradable_read();
        if let Some(conn) = read.get(&project_id).cloned() {
            return conn;
        }
        let mut write = RwLockUpgradableReadGuard::upgrade(read);
        write
            .entry(project_id)
            .or_insert_with(|| {
                let conn = Connection::start().expect("failed to establish connection");
                let project_data = self.project_data(project_id).project_data(self);
                let path: Vec<PathBuf> = project_data
                    .deps_ebins
                    .iter()
                    .map(|path| path.clone().into())
                    .collect();
                if !path.is_empty() {
                    // For a test fixture this should never happen
                    conn.add_code_path(path);
                }
                conn
            })
            .clone()
    }

    pub fn update_erlang_service_paths(&self) {
        for (&project_id, connection) in self.erlang_services.read().iter() {
            let project_data = self.project_data(project_id).project_data(self);
            let paths = project_data
                .deps_ebins
                .iter()
                .map(|path| path.clone().into())
                .collect();
            connection.add_code_path(paths);
        }
    }

    pub fn set_eqwalizer_progress_reporter(
        &self,
        report: Option<Box<dyn EqwalizerProgressReporter>>,
    ) {
        *self.eqwalizer_progress_reporter.lock() = report
    }

    pub fn eqwalizer(&self) -> &Eqwalizer {
        &self.eqwalizer
    }

    pub fn set_eqwalizer_mode(&mut self, mode: Mode) {
        self.eqwalizer.mode = mode
    }

    pub fn resolved_includes(&self, file_id: FileId) -> Option<Includes> {
        let source_file = self.parse(file_id).tree();
        // Context for T171541590
        let _ = stdx::panic_context::enter(format!("\nresolved_includes: {:?}", file_id));
        let project_id = self.file_app_data(file_id)?.project_id;
        let root_abs = &self.project_data(project_id).project_data(self).root_dir;
        let form_list = self.file_form_list(file_id);
        let line_index = self.file_line_index(file_id);
        let includes: Vec<_> = form_list
            .includes()
            .filter_map(|(idx, include)| {
                let resolved = self.resolve_include(InFile::new(file_id, idx))?;
                let range = include.form_id().get(&source_file).syntax().text_range();
                let line_col = line_index.line_col(range.start());
                if let Some(p) = Includes::app_file_path(self, resolved, root_abs) {
                    let target_path = p.to_string();
                    Some(Include {
                        line: line_col.line as u64 + 1,
                        target_path,
                    })
                } else {
                    None
                }
            })
            .collect();

        if includes.is_empty() {
            None
        } else {
            Includes::app_file_path(self, file_id, root_abs).map(|file| Includes {
                file: file.to_string(),
                includes,
            })
        }
    }
}

#[ra_ap_query_group_macro::query_group(LineIndexDatabaseStorage)]
pub trait LineIndexDatabase: RootQueryDb {
    fn file_line_index(&self, file_id: FileId) -> Arc<LineIndex>;
}

fn file_line_index(db: &dyn LineIndexDatabase, file_id: FileId) -> Arc<LineIndex> {
    let text = db.file_text(file_id).text(db);
    Arc::new(LineIndex::new(&text))
}

// ---------------------------------------------------------------------

#[derive(Serialize, Deserialize, Debug)]
pub struct Includes {
    pub file: String,
    includes: Vec<Include>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Include {
    line: u64,
    target_path: String,
}

impl Includes {
    /// Return a file path suitable for comparison in the
    /// eqwalizer-produced includes.json file.
    /// In particular, prefix otp paths with `/otp`, and make sure the
    /// paths are relative to the workspace root.
    fn app_file_path(
        db: &RootDatabase,
        file_id: FileId,
        root_abs: &AbsPathBuf,
    ) -> Option<Utf8PathBuf> {
        // Context for T171541590
        let _ = stdx::panic_context::enter(format!("\napp_file_path: {:?}", file_id));
        let root_id = db.file_source_root(file_id).source_root_id(db);
        let root = db.source_root(root_id).source_root(db);
        let path = root.path_for_file(&file_id)?;
        let app_data = db.file_app_data(file_id)?;

        let path = path.as_path()?;
        if path.starts_with(root_abs) {
            path.strip_prefix(root_abs)
                .map(|rel| -> Utf8PathBuf { rel.as_utf8_path().to_path_buf() })
        } else {
            let otp_root = Utf8PathBuf::from("/otp");
            let parent_dir = app_data.dir.parent()?;
            let otp_root = otp_root.join(path.strip_prefix(parent_dir)?);
            Some(otp_root)
        }
    }
}

// ---------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SymbolKind {
    File,
    Module,
    Function,
    Record,
    RecordField,
    Type,
    Define,
    Variable,
    Callback,
}

// ---------------------------------------------------------------------

pub fn find_best_token(sema: &Semantic<'_>, position: FilePosition) -> Option<InFile<SyntaxToken>> {
    let _p = tracing::info_span!("find_best_token").entered();
    let syntax = sema
        .parse(position.file_id)
        .map(|file| file.syntax().clone());

    let token = syntax.with_value(pick_best_token(
        syntax.value.token_at_offset(position.offset),
        |kind| match kind {
            SyntaxKind::ATOM
            | SyntaxKind::VAR
            | SyntaxKind::STRING
            | SyntaxKind::INTEGER
            | SyntaxKind::FLOAT => 2,
            _ => 1,
        },
    )?);
    Some(token)
}

// ---------------------------------------------------------------------

impl TypedSemantic for RootDatabase {
    fn eqwalizer_diagnostics(&self, file_id: FileId) -> Option<Vec<EqwalizerDiagnostic>> {
        // Check, if the file is actually a module
        let app_data = self.file_app_data(file_id)?;
        let module = self
            .module_index(app_data.project_id)
            .module_for_file(file_id)
            .cloned()?;

        let project_id = app_data.project_id;

        let eqwalizer_enabled = self.is_eqwalizer_enabled(file_id, false);
        if !eqwalizer_enabled {
            return Some(vec![]);
        }

        let diags = eqwalizer::eqwalizer_diagnostics_by_project(self, project_id, vec![file_id]);
        match &*diags {
            EqwalizerDiagnostics::Diagnostics { errors, .. } => Some(
                errors
                    .iter()
                    .flat_map(|(_, diags)| diags.iter().cloned())
                    .collect(),
            ),
            EqwalizerDiagnostics::NoAst { .. } => Some(vec![]),
            EqwalizerDiagnostics::Error(err) => {
                log::error!(
                    "EqWAlizer failed for {}: {}",
                    module.as_str(),
                    limit_logged_string(err)
                );
                Some(vec![])
            }
        }
    }

    fn eqwalizer_type_at_position(&self, range: FileRange) -> Option<Arc<(Type, FileRange)>> {
        self.type_at_position(range)
    }
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use elp_base_db::RootQueryDb;
    use elp_base_db::fixture::WithFixture;
    use elp_text_edit::TextRange;

    use crate::RootDatabase;

    #[test]
    fn clamp_range() {
        let fixture = r#"
             -mod~ule(main).
            "#;
        let (db, fixture) = RootDatabase::with_fixture(fixture);
        let position = fixture.position();

        debug_assert_eq!(
            db.clamp_range(position.file_id, TextRange::new(2.into(), 2000.into())),
            TextRange::new(2.into(), 15.into())
        )
    }

    #[test]
    fn clamp_offset() {
        let fixture = r#"
             -mod~ule(main).
            "#;
        let (db, fixture) = RootDatabase::with_fixture(fixture);
        let position = fixture.position();
        debug_assert_eq!(db.clamp_offset(position.file_id, 2000.into()), 15.into())
    }
}
