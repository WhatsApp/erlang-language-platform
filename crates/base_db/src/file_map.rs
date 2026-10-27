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
use indexmap::IndexSet;
use vfs::FileId;
use vfs::VfsPath;

use crate::File;
use crate::SourceDatabase;

/// The `FileId` of every path ELP has seen.
///
/// Ids are dense, in first-seen order, and never reused or forgotten. Each has
/// a `File` in the database, created when its path is interned, which keeps
/// whether the file exists: a deleted file keeps its id and its `File`.
#[derive(Default)]
pub struct FileMap {
    /// A path's index is its `FileId`.
    paths: RwLock<Paths>,
}

type Paths = IndexSet<VfsPath, BuildHasherDefault<FxHasher>>;

impl FileMap {
    /// The id of `path`. A new path gets an id and a `File` that doesn't exist
    /// yet.
    pub fn intern(&self, db: &dyn SourceDatabase, path: VfsPath) -> FileId {
        if let Some(index) = self.paths().get_index_of(&path) {
            return id_at(index);
        }
        let mut paths = self.paths_mut();
        let (index, new) = paths.insert_full(path.clone());
        let file_id = id_at(index);
        if new {
            // Under the lock, so the id is not handed out before its `File`
            // exists. Creating an input doesn't wait for snapshots.
            File::create(db, file_id, path, None);
        }
        file_id
    }

    /// The id of `path`, if interned, whether or not the file exists.
    pub fn file_id(&self, path: &VfsPath) -> Option<FileId> {
        self.paths().get_index_of(path).map(id_at)
    }

    /// The files that exist, in id order.
    pub fn existing_files(&self, db: &dyn SourceDatabase) -> Vec<(FileId, VfsPath)> {
        self.paths()
            .iter()
            .enumerate()
            .map(|(index, path)| (id_at(index), path))
            .filter(|(file_id, _)| db.file(*file_id).exists(db))
            .map(|(file_id, path)| (file_id, path.clone()))
            .collect()
    }

    pub fn existing_count(&self, db: &dyn SourceDatabase) -> usize {
        let len = self.paths().len();
        (0..len)
            .filter(|index| db.file(id_at(*index)).exists(db))
            .count()
    }

    fn paths(&self) -> RwLockReadGuard<'_, Paths> {
        self.paths.read().unwrap_or_else(PoisonError::into_inner)
    }

    fn paths_mut(&self) -> RwLockWriteGuard<'_, Paths> {
        self.paths.write().unwrap_or_else(PoisonError::into_inner)
    }
}

fn id_at(index: usize) -> FileId {
    FileId::from_raw(index as u32)
}

impl fmt::Debug for FileMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FileMap")
            .field("n_files", &self.paths().len())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use vfs::FileId;
    use vfs::VfsPath;

    use super::FileMap;
    use crate::LineEndings;
    use crate::SourceDatabase;
    use crate::assert_eq_expected;
    use crate::test_db::TestDB;

    fn path(path: &str) -> VfsPath {
        VfsPath::new_real_path(path.to_string())
    }

    fn set_exists(db: &mut TestDB, file_id: FileId, exists: bool) {
        let contents = exists.then(|| (Arc::from(""), LineEndings::Unix));
        db.file(file_id).set_contents(db, contents);
    }

    #[test]
    fn interning_allocates_one_dense_id_per_path() {
        let db = TestDB::default();
        let file_map = FileMap::default();
        let ids = ["/a.erl", "/b.erl", "/a.erl"].map(|p| file_map.intern(&db, path(p)));

        assert_eq_expected!([0, 1, 0].map(FileId::from_raw), ids);
        assert_eq_expected!(&path("/b.erl"), db.file(ids[1]).path(&db));
    }

    #[test]
    fn a_file_keeps_its_id_when_deleted_and_recreated() {
        let mut db = TestDB::default();
        let file_map = FileMap::default();
        let id = file_map.intern(&db, path("/a.erl"));
        let file = db.file(id);
        assert_eq_expected!(false, file.exists(&db));

        set_exists(&mut db, id, true);
        assert_eq_expected!(true, file.exists(&db));

        set_exists(&mut db, id, false);
        assert_eq_expected!(false, file.exists(&db));
        assert_eq_expected!(id, file_map.intern(&db, path("/a.erl")));
        assert_eq_expected!(Some(id), file_map.file_id(&path("/a.erl")));

        set_exists(&mut db, id, true);
        assert_eq_expected!(true, file.exists(&db));
        assert_eq!(file, db.file(id), "the file should keep its File");
    }

    #[test]
    fn existing_files_are_listed_in_id_order() {
        let mut db = TestDB::default();
        let file_map = FileMap::default();
        for (p, exists) in [("/c.erl", true), ("/a.erl", false), ("/b.erl", true)] {
            let id = file_map.intern(&db, path(p));
            set_exists(&mut db, id, exists);
        }

        let expected = vec![
            (FileId::from_raw(0), path("/c.erl")),
            (FileId::from_raw(2), path("/b.erl")),
        ];
        assert_eq_expected!(expected, file_map.existing_files(&db));
    }

    #[test]
    fn existing_count_skips_files_that_do_not_exist() {
        let mut db = TestDB::default();
        let file_map = FileMap::default();
        for (p, exists) in [("/a.erl", true), ("/b.erl", false)] {
            let id = file_map.intern(&db, path(p));
            set_exists(&mut db, id, exists);
        }

        assert_eq_expected!(1, file_map.existing_count(&db));
    }
}
