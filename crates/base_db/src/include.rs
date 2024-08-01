/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use vfs::FileId;

use crate::SourceDatabase;
use crate::SourceRoot;
use crate::SourceRootId;

pub struct IncludeCtx<'a> {
    db: &'a dyn SourceDatabase,
    source_root_id: SourceRootId,
    source_root: Arc<SourceRoot>,
    pub file_id: FileId,
}

impl<'a> IncludeCtx<'a> {
    pub fn new(db: &'a dyn SourceDatabase, file_id: FileId) -> Self {
        // Context for T171541590
        let _ = stdx::panic_context::enter(format!("\nIncludeCtx::new: {:?}", file_id));
        let source_root_id = db.file_source_root(file_id);
        let source_root = db.source_root(source_root_id);
        Self {
            db,
            file_id,
            source_root_id,
            source_root,
        }
    }

    pub fn resolve_include(&self, path: &str) -> Option<FileId> {
        self.resolve_relative(path)
            .or_else(|| self.resolve_local(path))
    }

    pub fn resolve_include_lib(&self, path: &str) -> Option<FileId> {
        self.resolve_include(path)
            .or_else(|| self.resolve_remote(path))
    }

    fn resolve_relative(&self, path: &str) -> Option<FileId> {
        self.source_root.relative_path(self.file_id, path)
    }

    fn resolve_local(&self, path: &str) -> Option<FileId> {
        let app_data = self.db.app_data(self.source_root_id)?;
        app_data.include_path.iter().find_map(|include| {
            let name = include.join(path);
            self.source_root.file_for_path(&name.into())
        })
    }

    fn resolve_remote(&self, path: &str) -> Option<FileId> {
        let app_data = self.db.app_data(self.source_root_id)?;
        let project_data = self.db.project_data(app_data.project_id);

        let (app_name, path) = path.split_once('/')?;
        let source_root_id = project_data.app_roots.get(app_name)?;
        let source_root = self.db.source_root(source_root_id);
        let target_app_data = self.db.app_data(source_root_id)?;
        let path = target_app_data.dir.join(path);
        source_root.file_for_path(&path.into())
    }
}
