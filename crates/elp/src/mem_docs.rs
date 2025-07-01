/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! In-memory document information.

// Based initially on
// https://github.com/rust-lang/rust-analyzer/blob/fd74511f34ae6c165466543cc6e55ea60e7365af/crates/rust-analyzer/src/mem_docs.rs

use fxhash::FxHashMap;
use vfs::VfsPath;

/// Holds the set of in-memory documents.
///
/// For these document, their true contents is maintained by the client. It
/// might be different from what's on disk.
#[derive(Default, Clone)]
pub struct MemDocs {
    mem_docs: FxHashMap<VfsPath, DocumentData>,
}

impl MemDocs {
    pub(crate) fn contains(&self, path: &VfsPath) -> bool {
        self.mem_docs.contains_key(path)
    }

    pub(crate) fn insert(&mut self, path: VfsPath, data: DocumentData) -> Result<(), ()> {
        match self.mem_docs.insert(path, data) {
            Some(_) => Err(()),
            None => Ok(()),
        }
    }

    pub(crate) fn remove(&mut self, path: &VfsPath) -> Result<(), ()> {
        match self.mem_docs.remove(path) {
            Some(_) => Ok(()),
            None => Err(()),
        }
    }

    pub(crate) fn get(&self, path: &VfsPath) -> Option<&DocumentData> {
        self.mem_docs.get(path)
    }

    pub(crate) fn get_mut(&mut self, path: &VfsPath) -> Option<&mut DocumentData> {
        // NB: don't set `self.added_or_removed` here, as that purposefully only
        // tracks changes to the key set.
        self.mem_docs.get_mut(path)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &VfsPath> {
        self.mem_docs.keys()
    }

    pub(crate) fn len(&self) -> usize {
        self.mem_docs.len()
    }
}

/// Information about a document that the Language Client
/// knows about.
/// Its lifetime is driven by the textDocument/didOpen and textDocument/didClose
/// client notifications.
#[derive(Debug, Clone)]
pub(crate) struct DocumentData {
    pub(crate) version: i32,
    pub(crate) data: Vec<u8>,
}

impl DocumentData {
    pub(crate) fn new(version: i32, data: Vec<u8>) -> Self {
        DocumentData { version, data }
    }
}
