/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Like [`std::time::Instant`], but for memory.
//!
//! Measures the total size of all currently allocated objects.
//!
//! Based initially on
//! https://github.com/rust-lang/rust-analyzer/blob/81ff38f53b9a14ac608feb30b21ed42a41d016c6/crates/profile/src/memory_usage.rs
use std::fmt;

use serde::Serialize;

#[derive(Copy, Clone, Serialize)]
pub struct MemoryUsage {
    pub allocated: Bytes,
    pub active: Bytes,
    pub resident: Bytes,
}

impl fmt::Display for MemoryUsage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Memory usage:").ok();
        writeln!(f, "  allocated: {}", self.allocated).ok();
        writeln!(f, "  active: {}", self.active).ok();
        writeln!(f, "  resident: {}", self.resident)
    }
}

impl std::ops::Sub for MemoryUsage {
    type Output = MemoryUsage;
    fn sub(self, rhs: MemoryUsage) -> MemoryUsage {
        MemoryUsage {
            allocated: self.allocated - rhs.allocated,
            active: self.active - rhs.active,
            resident: self.resident - rhs.resident,
        }
    }
}

impl MemoryUsage {
    pub fn now() -> MemoryUsage {
        #[cfg(not(target_env = "msvc"))]
        {
            jemalloc_ctl::epoch::advance().unwrap();
            MemoryUsage {
                allocated: Bytes(jemalloc_ctl::stats::allocated::read().unwrap() as isize),
                active: Bytes(jemalloc_ctl::stats::active::read().unwrap() as isize),
                resident: Bytes(jemalloc_ctl::stats::resident::read().unwrap() as isize),
            }
        }
        #[cfg(target_env = "msvc")]
        {
            MemoryUsage {
                allocated: Bytes(0),
                active: Bytes(0),
                resident: Bytes(0),
            }
        }
    }
}

#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Serialize)]
pub struct Bytes(isize);

impl Bytes {
    pub fn new(bytes: isize) -> Bytes {
        Bytes(bytes)
    }
}

impl Bytes {
    pub fn megabytes(self) -> isize {
        self.0 / 1024 / 1024
    }
}

impl fmt::Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bytes = self.0;
        let mut value = bytes;
        let mut suffix = "b";
        if value.abs() > 4096 {
            value /= 1024;
            suffix = "kb";
            if value.abs() > 4096 {
                value /= 1024;
                suffix = "mb";
            }
        }
        f.pad(&format!("{value}{suffix}"))
    }
}

impl std::ops::AddAssign<usize> for Bytes {
    fn add_assign(&mut self, x: usize) {
        self.0 += x as isize;
    }
}

impl std::ops::Sub for Bytes {
    type Output = Bytes;
    fn sub(self, rhs: Bytes) -> Bytes {
        Bytes(self.0 - rhs.0)
    }
}
