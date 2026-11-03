/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Database used for testing `base_db`.

use std::sync::Arc;

use vfs::FileId;

use crate::App;
use crate::File;
use crate::FileSourceRootInput;
use crate::Files;
use crate::SourceDatabase;
use crate::SourceRoot;
use crate::SourceRootAppInput;
use crate::SourceRootId;
use crate::SourceRootInput;

#[salsa::db]
#[derive(Default, Clone)]
pub(crate) struct TestDB {
    storage: salsa::Storage<TestDB>,
    files: Arc<Files>,
}

#[salsa::db]
impl salsa::Database for TestDB {}

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
