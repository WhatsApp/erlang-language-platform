/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Database used for testing `hir`.

use std::fmt;
use std::panic;
use std::sync::Arc;

use elp_base_db::App;
use elp_base_db::File;
use elp_base_db::FileId;
use elp_base_db::FileRange;
use elp_base_db::FileSourceRootInput;
use elp_base_db::Files;
use elp_base_db::RootQueryDb;
use elp_base_db::SourceDatabase;
use elp_base_db::SourceRoot;
use elp_base_db::SourceRootAppInput;
use elp_base_db::SourceRootId;
use elp_base_db::SourceRootInput;
use elp_base_db::Upcast;
use elp_base_db::salsa;
use elp_types_db::TypedSemantic;
use elp_types_db::eqwalizer;

use crate::db::InternDatabase;

#[salsa::db]
#[derive(Default, Clone)]
pub(crate) struct TestDB {
    storage: salsa::Storage<TestDB>,
    files: Arc<Files>,
}

impl Upcast<dyn RootQueryDb> for TestDB {
    fn upcast(&self) -> &(dyn RootQueryDb + 'static) {
        self
    }
}

impl Upcast<dyn InternDatabase> for TestDB {
    fn upcast(&self) -> &(dyn InternDatabase + 'static) {
        self
    }
}

#[salsa::db]
impl salsa::Database for TestDB {}

impl fmt::Debug for TestDB {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TestDB").finish()
    }
}

#[salsa::db]
impl SourceDatabase for TestDB {
    fn file(&self, file_id: FileId) -> File {
        self.files.file(file_id)
    }

    fn insert_file(&self, file_id: FileId, file: File) {
        self.files.insert_file(file_id, file);
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

    fn source_root_app(&self, id: SourceRootId) -> SourceRootAppInput {
        self.files.source_root_app(id)
    }

    fn set_source_root_app(&mut self, id: SourceRootId, app: Option<App>) {
        let files = self.files.clone();
        files.set_source_root_app(self, id, app);
    }
}

impl panic::RefUnwindSafe for TestDB {}

impl TypedSemantic for TestDB {
    fn eqwalizer_diagnostics(
        &self,
        _file_id: FileId,
    ) -> Option<Vec<eqwalizer::EqwalizerDiagnostic>> {
        panic!("Eqwalizer data is not available in HIR tests")
    }

    fn eqwalizer_type_at_position(
        &self,
        _range: elp_base_db::FileRange,
    ) -> Option<Arc<(eqwalizer::types::Type, FileRange)>> {
        panic!("Eqwalizer data is not available in HIR tests")
    }

    fn is_eqwalizer_enabled(&self, _file_id: elp_base_db::FileId) -> bool {
        false
    }
}
