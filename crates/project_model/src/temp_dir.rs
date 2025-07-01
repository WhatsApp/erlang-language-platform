/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! A temporary directory that is cleaned up on Drop, with an option
//! to keep it for after-the-fact analysis.

// This is initially based on
// https://github.com/rust-lang/rust-analyzer/blob/2024-01-08/crates/rust-analyzer/tests/slow-tests/testdir.rs

use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

#[derive(Debug, Clone)]
pub struct TempDir {
    pub path: PathBuf,
    pub keep: bool,
}

impl TempDir {
    pub fn new() -> TempDir {
        let temp_dir = std::env::temp_dir();
        // On MacOS builders on GitHub actions, the temp dir is a symlink, and
        // that causes problems down the line. Specifically:
        // * Cargo may emit different PackageId depending on the working directory
        // * rust-analyzer may fail to map LSP URIs to correct paths.
        //
        // Work-around this by canonicalizing. Note that we don't want to do this
        // on *every* OS, as on windows `canonicalize` itself creates problems.
        #[cfg(target_os = "macos")]
        let temp_dir = temp_dir.canonicalize().unwrap();

        let base = temp_dir.join("testdir");
        let pid = std::process::id();

        static CNT: AtomicUsize = AtomicUsize::new(0);
        for _ in 0..100 {
            let cnt = CNT.fetch_add(1, Ordering::Relaxed);
            let path = base.join(format!("{pid}_{cnt}"));
            if path.is_dir() {
                continue;
            }
            fs::create_dir_all(&path).unwrap();
            return TempDir { path, keep: false };
        }
        panic!("Failed to create a temporary directory")
    }

    pub fn keep(mut self) -> TempDir {
        self.keep = true;
        self
    }
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl Default for TempDir {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        if self.keep {
            return;
        }
        remove_dir_all(&self.path).unwrap_or_else(|err| {
            panic!(
                "failed to remove temporary directory {}: {err}",
                self.path.display()
            )
        })
    }
}

#[cfg(not(windows))]
fn remove_dir_all(path: &Path) -> io::Result<()> {
    fs::remove_dir_all(path)
}

#[cfg(windows)]
fn remove_dir_all(path: &Path) -> io::Result<()> {
    for _ in 0..99 {
        if fs::remove_dir_all(path).is_ok() {
            return Ok(());
        }
        std::thread::sleep(std::time::Duration::from_millis(10))
    }
    fs::remove_dir_all(path)
}
