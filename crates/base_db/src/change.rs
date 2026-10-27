/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Defines a unit of change that can applied to the database to get the next
//! state. Changes are transactional.

use std::fmt;
use std::sync::Arc;

use vfs::FileId;
use vfs::VfsPath;

use crate::FileMap;
use crate::LineEndings;
use crate::RootQueryDb;
use crate::SourceDatabase;
use crate::SourceRoot;
use crate::SourceRootId;
use crate::input::AppStructure;

/// Encapsulate a bunch of raw `.set` calls on the database.
#[derive(Default)]
pub struct Change {
    pub roots: Option<Vec<SourceRoot>>,
    /// The file map that gave the files their `FileId`s.
    pub file_map: FileMap,
    /// Files new to the database, with their text.
    pub files_added: Vec<(FileId, Arc<str>)>,
    pub app_structure: Option<AppStructure>,
}

impl fmt::Debug for Change {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut d = fmt.debug_struct("Change");
        if let Some(roots) = &self.roots {
            d.field("roots", roots);
        }
        if !self.files_added.is_empty() {
            d.field("files_added", &self.files_added.len());
        }
        if self.app_structure.is_some() {
            d.field("app_structure", &self.app_structure);
        }
        d.finish()
    }
}

impl Change {
    pub fn new() -> Change {
        Change::default()
    }

    pub fn set_roots(&mut self, roots: Vec<SourceRoot>) {
        self.roots = Some(roots);
    }

    /// Gives the file at `path` a `FileId`, whose text `apply` sets.
    ///
    /// # Panics
    ///
    /// Panics if the change already has a file at `path`.
    pub fn add_file(&mut self, db: &dyn SourceDatabase, path: VfsPath, text: Arc<str>) -> FileId {
        assert!(
            self.file_map.file_id(&path).is_none(),
            "the fixture lists {path} more than once"
        );
        let file_id = self.file_map.intern(db, path);
        self.files_added.push((file_id, text));
        file_id
    }

    pub fn set_app_structure(&mut self, a: AppStructure) {
        self.app_structure = Some(a);
    }

    /// Returns the file map, which knows the change's files.
    pub fn apply(self, db: &mut dyn RootQueryDb) -> FileMap {
        let _p = tracing::info_span!("Change::apply").entered();
        let Change {
            roots,
            file_map,
            files_added,
            app_structure,
        } = self;
        if let Some(roots) = roots {
            for (idx, root) in roots.into_iter().enumerate() {
                let root_id = SourceRootId(idx as u32);
                for file_id in root.iter() {
                    db.set_file_source_root(file_id, root_id);
                }
                db.set_source_root(root_id, Arc::new(root));
            }
        }

        if let Some(app_structure) = app_structure {
            app_structure.apply(db, &|path| {
                file_map
                    .file_id(&VfsPath::from(path.clone()))
                    .unwrap_or_else(|| panic!("{path} is not a fixture file"))
            });
        }

        for (file_id, text) in files_added {
            let line_endings = LineEndings::detect(&text);
            db.file(file_id)
                .set_contents(db, Some((text, line_endings)));
        }
        file_map
    }
}
