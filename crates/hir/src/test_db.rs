/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Database used for testing `hir`.

use std::fmt;
use std::panic;
use std::sync::Arc;

use elp_base_db::salsa;
use elp_base_db::FileId;
use elp_base_db::FileLoader;
use elp_base_db::FileLoaderDelegate;
use elp_base_db::FileRange;
use elp_base_db::SourceDatabase;
use elp_base_db::Upcast;
use elp_types_db::eqwalizer;
use elp_types_db::EqwalizerIncludes;
use elp_types_db::TypedSemantic;

use crate::db::InternDatabase;

#[salsa::database(
    elp_base_db::SourceDatabaseExtStorage,
    elp_base_db::SourceDatabaseStorage,
    crate::db::DefDatabaseStorage,
    crate::db::InternDatabaseStorage
)]
#[derive(Default)]
pub(crate) struct TestDB {
    storage: salsa::Storage<TestDB>,
}

impl Upcast<dyn SourceDatabase> for TestDB {
    fn upcast(&self) -> &(dyn SourceDatabase + 'static) {
        self
    }
}

impl Upcast<dyn InternDatabase> for TestDB {
    fn upcast(&self) -> &(dyn InternDatabase + 'static) {
        self
    }
}

impl salsa::Database for TestDB {}

impl fmt::Debug for TestDB {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TestDB").finish()
    }
}

impl panic::RefUnwindSafe for TestDB {}

impl FileLoader for TestDB {
    fn file_text(&self, file_id: FileId) -> Arc<str> {
        FileLoaderDelegate(self).file_text(file_id)
    }
}

impl TypedSemantic for TestDB {
    fn eqwalizer_diagnostics(
        &self,
        _file_id: FileId,
        _eqwalizer_includes: EqwalizerIncludes,
    ) -> Option<Vec<eqwalizer::EqwalizerDiagnostic>> {
        panic!("Eqwalizer data is not available in HIR tests")
    }

    fn eqwalizer_type_at_position(
        &self,
        _range: elp_base_db::FileRange,
    ) -> Option<Arc<(eqwalizer::types::Type, FileRange)>> {
        panic!("Eqwalizer data is not available in HIR tests")
    }
}
