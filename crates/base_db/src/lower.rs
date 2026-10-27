/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lowering: setting the project model in the database from the loaded
//! projects. The language server, the CLI loaders and test fixtures all lower
//! through [`lower`].

use std::sync::Arc;

use fxhash::FxHashMap;
use vfs::AbsPathBuf;

use crate::AppDataId;
use crate::FileMap;
use crate::ProjectApps;
use crate::RootQueryDb;
use crate::SourceDatabase;
use crate::SourceRoot;
use crate::SourceRootId;

/// Sets the project model from `project_apps`. Every file an app lists gets a
/// `FileId` from `file_map`, whether or not it has loaded. Returns the app of
/// each generated source file.
pub fn lower(
    db: &mut dyn RootQueryDb,
    file_map: &FileMap,
    project_apps: &ProjectApps,
) -> FxHashMap<AbsPathBuf, AppDataId> {
    project_apps.app_structure().apply(db, file_map)
}

/// Sets `roots` as the source roots, numbered in order, and the source root of
/// each of their files.
pub fn set_source_roots(db: &mut dyn SourceDatabase, roots: Vec<SourceRoot>) {
    for (idx, root) in roots.into_iter().enumerate() {
        let root_id = SourceRootId(idx as u32);
        for file_id in root.iter() {
            db.set_file_source_root(file_id, root_id);
        }
        db.set_source_root(root_id, Arc::new(root));
    }
}
