/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::panic::AssertUnwindSafe;
use std::panic::RefUnwindSafe;
use std::path::PathBuf;
use std::sync::Arc;

use elp_base_db::salsa;
use elp_base_db::AbsPathBuf;
use elp_base_db::FileId;
use elp_base_db::FileLoader;
use elp_base_db::FileLoaderDelegate;
use elp_base_db::FilePosition;
use elp_base_db::FileRange;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_base_db::Upcast;
use elp_eqwalizer::ipc::IpcHandle;
use elp_eqwalizer::EqwalizerConfig;
use elp_eqwalizer::EqwalizerDiagnosticsDatabase;
use elp_eqwalizer::Mode;
use elp_syntax::AstNode;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxToken;
use elp_types_db::eqwalizer::types::Type;
use elp_types_db::IncludeGenerated;
use elp_types_db::TypedSemantic;
use erlang_service::Connection;
use fxhash::FxHashMap;
use helpers::pick_best_token;
use hir::db::DefDatabase;
use hir::db::InternDatabase;
use hir::InFile;
use hir::Semantic;
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

#[salsa::database(
    LineIndexDatabaseStorage,
    docs::DocDatabaseStorage,
    elp_base_db::SourceDatabaseExtStorage,
    elp_base_db::SourceDatabaseStorage,
    eqwalizer::EqwalizerDatabaseStorage,
    common_test::CommonTestDatabaseStorage,
    elp_eqwalizer::ast::db::EqwalizerASTDatabaseStorage,
    elp_eqwalizer::analyses::EqwalizerAnalysesDatabaseStorage,
    elp_eqwalizer::EqwalizerDiagnosticsDatabaseStorage,
    erl_ast::ErlAstDatabaseStorage,
    hir::db::InternDatabaseStorage,
    hir::db::DefDatabaseStorage
)]

pub struct RootDatabase {
    storage: salsa::Storage<Self>,
    erlang_services: Arc<AssertUnwindSafe<RwLock<FxHashMap<ProjectId, Connection>>>>,
    eqwalizer: Eqwalizer,
    eqwalizer_progress_reporter: EqwalizerProgressReporterBox,
    ipc_handles: Arc<AssertUnwindSafe<RwLock<FxHashMap<String, Arc<Mutex<IpcHandle>>>>>>,
}
impl Default for RootDatabase {
    fn default() -> Self {
        let mut db = RootDatabase {
            storage: salsa::Storage::default(),
            erlang_services: Arc::default(),
            eqwalizer: Eqwalizer::default(),
            eqwalizer_progress_reporter: EqwalizerProgressReporterBox::default(),
            ipc_handles: Arc::default(),
        };
        db.set_eqwalizer_config(Arc::new(EqwalizerConfig::default()));
        db
    }
}

impl Upcast<dyn SourceDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn SourceDatabase + 'static) {
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

impl FileLoader for RootDatabase {
    fn file_text(&self, file_id: FileId) -> Arc<str> {
        FileLoaderDelegate(self).file_text(file_id)
    }
}

impl fmt::Debug for RootDatabase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RootDatabase").finish_non_exhaustive()
    }
}

impl salsa::Database for RootDatabase {}

impl salsa::ParallelDatabase for RootDatabase {
    fn snapshot(&self) -> salsa::Snapshot<RootDatabase> {
        salsa::Snapshot::new(RootDatabase {
            storage: self.storage.snapshot(),
            erlang_services: self.erlang_services.clone(),
            eqwalizer: self.eqwalizer.clone(),
            eqwalizer_progress_reporter: self.eqwalizer_progress_reporter.clone(),
            ipc_handles: self.ipc_handles.clone(),
        })
    }
}

impl RootDatabase {
    pub fn request_cancellation(&mut self) {
        let _p = tracing::info_span!("RootDatabase::request_cancellation").entered();
        self.salsa_runtime_mut()
            .synthetic_write(salsa::Durability::LOW);
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
                let project_data = self.project_data(project_id);
                let path: Vec<PathBuf> = project_data
                    .deps_ebins
                    .iter()
                    .map(|path| path.clone().into())
                    .collect();
                if path.len() > 0 {
                    // For a test fixture this should never happen
                    conn.add_code_path(path);
                }
                conn
            })
            .clone()
    }

    pub fn update_erlang_service_paths(&self) {
        for (&project_id, connection) in self.erlang_services.read().iter() {
            let project_data = self.project_data(project_id);
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
        let root_abs = &self.project_data(project_id).root_dir;
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

#[salsa::query_group(LineIndexDatabaseStorage)]
pub trait LineIndexDatabase: SourceDatabase {
    fn file_line_index(&self, file_id: FileId) -> Arc<LineIndex>;
}

fn file_line_index(db: &dyn LineIndexDatabase, file_id: FileId) -> Arc<LineIndex> {
    let text = db.file_text(file_id);
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
        let root_id = db.file_source_root(file_id);
        let root = db.source_root(root_id);
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
            SyntaxKind::ATOM | SyntaxKind::VAR | SyntaxKind::STRING => 2,
            _ => 1,
        },
    )?);
    Some(token)
}

// ---------------------------------------------------------------------

impl TypedSemantic for RootDatabase {
    fn eqwalizer_diagnostics(
        &self,
        file_id: FileId,
        include_generated: IncludeGenerated,
    ) -> Option<Vec<EqwalizerDiagnostic>> {
        // Check, if the file is actually a module
        let app_data = self.file_app_data(file_id)?;
        let module = self
            .module_index(app_data.project_id)
            .module_for_file(file_id)
            .cloned()?;

        let project_id = app_data.project_id;

        let eqwalizer_enabled = self.is_eqwalizer_enabled(file_id, include_generated);
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
                log::error!("EqWAlizer failed for {}: {}", module.as_str(), err);
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
    use elp_base_db::fixture::WithFixture;
    use elp_base_db::SourceDatabase;
    use text_edit::TextRange;

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
