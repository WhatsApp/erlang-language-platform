/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use elp_syntax::SmolStr;
use vfs::FileId;
use vfs::VfsPath;

use crate::SourceDatabase;
use crate::SourceRoot;

pub struct IncludeCtx<'a> {
    db: &'a dyn SourceDatabase,
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
            source_root,
        }
    }

    pub fn resolve_include(&self, path: &str) -> Option<FileId> {
        self.resolve_relative(path)
            .or_else(|| self.db.resolve_local(self.file_id, path.into()))
    }

    pub fn resolve_include_lib(&self, path: &str) -> Option<FileId> {
        self.resolve_include(path)
            .or_else(|| self.db.resolve_remote(self.file_id, path.into()))
    }

    pub fn resolve_include_doc(&self, path: &str) -> Option<FileId> {
        self.resolve_relative(path)
    }

    fn resolve_relative(&self, path: &str) -> Option<FileId> {
        self.source_root.relative_path(self.file_id, path)
    }

    /// Called via salsa for inserting in the graph
    pub(crate) fn resolve_local_query(
        db: &dyn SourceDatabase,
        file_id: FileId,
        path: SmolStr,
    ) -> Option<FileId> {
        let path: &str = &path;
        let app_data = db.file_app_data(file_id)?;
        app_data.include_path.iter().find_map(|include| {
            let name = include.join(path);
            db.include_file_id(app_data.project_id, VfsPath::from((&name).clone()))
        })
    }

    /// Called via salsa for inserting in the graph
    pub(crate) fn resolve_remote_query(
        db: &dyn SourceDatabase,
        file_id: FileId,
        path: SmolStr,
    ) -> Option<FileId> {
        let app_data = db.file_app_data(file_id)?;
        let project_data = db.project_data(app_data.project_id);
        let (app_name, path) = path.split_once('/')?;
        let source_root_id = project_data.app_roots.get(app_name)?;
        let target_app_data = db.app_data(source_root_id)?;
        let path = target_app_data.dir.join(path);
        db.include_file_id(app_data.project_id, VfsPath::from((&path).clone()))
    }
}
