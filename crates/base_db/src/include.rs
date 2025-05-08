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

use crate::AppData;
use crate::ProjectId;
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
            db.include_file_id(app_data.project_id, VfsPath::from(name.clone()))
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
        let (app_name, include_path) = path.split_once('/')?;
        let source_root_id = project_data.app_roots.get(app_name)?;
        let target_app_data = db.app_data(source_root_id)?;
        let path = target_app_data.dir.join(include_path);
        db.include_file_id(app_data.project_id, VfsPath::from(path.clone()))
            .or_else(|| {
                find_generated_include_lib(db, app_data.project_id, include_path, &target_app_data)
            })
    }
}

fn find_generated_include_lib(
    db: &dyn SourceDatabase,
    project_id: ProjectId,
    include_path: &str,
    target_app_data: &AppData,
) -> Option<FileId> {
    // buck2 builds create an include file mapping when
    // invoking the OTP compiler, by manipulating symlinks
    // in the output directory.
    //
    // Since we are only generating some files, not
    // building them, and we prefer to work with the files
    // in their canonical locations, we deal with this
    // here.
    //

    // The target_app_data has an `include_path` field. An "include/"
    // prefix on the path should be replaced with an entry from the
    // set of include paths/
    let path = include_path.strip_prefix("include/")?;
    target_app_data.include_path.iter().find_map(|dir| {
        let path = dir.join(path);
        db.include_file_id(project_id, VfsPath::from(path.clone()))
    })
}

pub fn generated_file_include_lib(
    db: &dyn SourceDatabase,
    file_id: FileId,
    included_file_id: FileId,
    include_path: VfsPath,
) -> Option<String> {
    // In `find_generated_include_lib`, the processing does
    // - split the path into app and rest
    // - get the app_data based on the app
    // - if the rest starts with "include/"
    //   - strip it
    //   - find a dir in the app include_path list such that the
    //     concatenation gives a valid file
    //
    // We need to do the reverse here.

    let inc_app_data = db.file_app_data(included_file_id)?;
    let candidate_path = inc_app_data
        .include_path
        .iter()
        .find_map(|dir| include_path.as_path()?.strip_prefix(dir))?;
    let candidate = format!("{}/include/{}", inc_app_data.name, candidate_path.as_str());
    let resolved_file_id = IncludeCtx::new(db, file_id).resolve_include_lib(&candidate)?;
    if resolved_file_id == included_file_id {
        // We have an equivalent include
        Some(candidate)
    } else {
        None
    }
}
