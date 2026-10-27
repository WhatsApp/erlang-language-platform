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
use std::hash::BuildHasherDefault;
use std::sync::PoisonError;
use std::sync::RwLock;
use std::sync::RwLockReadGuard;
use std::sync::RwLockWriteGuard;

use fxhash::FxHasher;
use indexmap::IndexMap;
use vfs::FileId;
use vfs::VfsPath;

/// The `FileId` of every path ELP has seen.
///
/// Ids are dense, in first-seen order, and never reused or forgotten: a deleted
/// file keeps its id, and recreating it brings the same id back. Lookups by
/// path find only files that exist, that is, whose contents are in the database.
#[derive(Default)]
pub struct FileMap {
    /// Whether each path's file exists; a path's index is its `FileId`.
    files: RwLock<Files>,
}

type Files = IndexMap<VfsPath, bool, BuildHasherDefault<FxHasher>>;

impl FileMap {
    /// The id of `path`, allocated if the path is new. A new path's file does
    /// not exist until [`set_exists`](Self::set_exists).
    pub fn intern(&self, path: VfsPath) -> FileId {
        if let Some(index) = self.files().get_index_of(&path) {
            return id_at(index);
        }
        let mut files = self.files_mut();
        let entry = files.entry(path);
        let index = entry.index();
        entry.or_insert(false);
        id_at(index)
    }

    /// The id of `path`, if its file exists.
    pub fn file_id(&self, path: &VfsPath) -> Option<FileId> {
        let files = self.files();
        let (index, _, exists) = files.get_full(path)?;
        exists.then(|| id_at(index))
    }

    /// # Panics
    ///
    /// Panics if `file_id` was not interned in this map.
    pub fn file_path(&self, file_id: FileId) -> VfsPath {
        let files = self.files();
        let (path, _) = files
            .get_index(file_id.index() as usize)
            .expect("the file id should have been interned");
        path.clone()
    }

    pub fn exists(&self, file_id: FileId) -> bool {
        self.files()[file_id.index() as usize]
    }

    pub fn set_exists(&self, file_id: FileId, exists: bool) {
        self.files_mut()[file_id.index() as usize] = exists;
    }

    /// The files that exist, in id order.
    pub fn existing_files(&self) -> Vec<(FileId, VfsPath)> {
        self.files()
            .iter()
            .enumerate()
            .filter(|(_, (_, exists))| **exists)
            .map(|(index, (path, _))| (id_at(index), path.clone()))
            .collect()
    }

    fn files(&self) -> RwLockReadGuard<'_, Files> {
        self.files.read().unwrap_or_else(PoisonError::into_inner)
    }

    fn files_mut(&self) -> RwLockWriteGuard<'_, Files> {
        self.files.write().unwrap_or_else(PoisonError::into_inner)
    }
}

fn id_at(index: usize) -> FileId {
    FileId::from_raw(index as u32)
}

impl fmt::Debug for FileMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FileMap")
            .field("n_files", &self.files().len())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use vfs::FileId;
    use vfs::VfsPath;

    use super::FileMap;
    use crate::assert_eq_expected;

    fn path(path: &str) -> VfsPath {
        VfsPath::new_real_path(path.to_string())
    }

    #[test]
    fn interning_allocates_one_dense_id_per_path() {
        let file_map = FileMap::default();
        let ids = ["/a.erl", "/b.erl", "/a.erl"].map(|p| file_map.intern(path(p)));

        assert_eq_expected!([0, 1, 0].map(FileId::from_raw), ids);
        assert_eq_expected!(path("/b.erl"), file_map.file_path(ids[1]));
    }

    #[test]
    fn a_file_keeps_its_id_when_deleted_and_recreated() {
        let file_map = FileMap::default();
        let id = file_map.intern(path("/a.erl"));
        assert_eq_expected!(None, file_map.file_id(&path("/a.erl")));

        file_map.set_exists(id, true);
        assert_eq_expected!(Some(id), file_map.file_id(&path("/a.erl")));

        file_map.set_exists(id, false);
        assert_eq_expected!(None, file_map.file_id(&path("/a.erl")));
        assert_eq_expected!(id, file_map.intern(path("/a.erl")));

        file_map.set_exists(id, true);
        assert_eq_expected!(Some(id), file_map.file_id(&path("/a.erl")));
    }

    #[test]
    fn existing_files_are_listed_in_id_order() {
        let file_map = FileMap::default();
        for (p, exists) in [("/c.erl", true), ("/a.erl", false), ("/b.erl", true)] {
            let id = file_map.intern(path(p));
            file_map.set_exists(id, exists);
        }

        let expected = vec![
            (FileId::from_raw(0), path("/c.erl")),
            (FileId::from_raw(2), path("/b.erl")),
        ];
        assert_eq_expected!(expected, file_map.existing_files());
    }
}
