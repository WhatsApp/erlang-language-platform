/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Like [`std::time::Instant`], but for memory.
//!
//! Measures the total size of all currently allocated objects.
//!
//! Based initially on
//! https://github.com/rust-lang/rust-analyzer/blob/81ff38f53b9a14ac608feb30b21ed42a41d016c6/crates/profile/src/memory_usage.rs
use std::fmt;

use serde::Serialize;

#[derive(Copy, Clone, Serialize, Debug)]
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
    /// All-zero reading, used when the allocator cannot be queried.
    const UNKNOWN: MemoryUsage = MemoryUsage {
        allocated: Bytes(0),
        active: Bytes(0),
        resident: Bytes(0),
    };

    pub fn now() -> MemoryUsage {
        imp::now().unwrap_or(Self::UNKNOWN)
    }
}

/// Buck builds link jemalloc through the allocator that the build system
/// selects, so the statistics are read back over `malloc_stats_print`. That
/// symbol is weak: when the binary ends up on another allocator — under a
/// sanitizer, say — the call reports nothing rather than failing, so there is
/// nothing to gate at build time.
#[cfg(buck_build)]
mod imp {
    use serde_json::Value;

    use super::Bytes;
    use super::MemoryUsage;

    fn stat(stats: &Value, name: &str) -> Option<Bytes> {
        let bytes = stats["jemalloc"]["stats"][name].as_u64()?;
        Some(Bytes(bytes as isize))
    }

    pub(super) fn now() -> Option<MemoryUsage> {
        if !memory::is_using_jemalloc() {
            return None;
        }

        // `J` selects JSON; the remaining flags drop the per-arena, bin and
        // extent sections we do not read. Refreshing the statistics epoch is
        // part of the call.
        let stats = allocator_stats::malloc_stats("Jmdablxg").ok()?;
        let stats: Value = serde_json::from_str(&stats).ok()?;

        Some(MemoryUsage {
            allocated: stat(&stats, "allocated")?,
            active: stat(&stats, "active")?,
            resident: stat(&stats, "resident")?,
        })
    }
}

/// Cargo builds install jemalloc as the Rust global allocator, so the
/// statistics come from the matching `jemalloc-ctl` bindings instead.
#[cfg(not(buck_build))]
mod imp {
    use super::Bytes;
    use super::MemoryUsage;

    #[cfg(not(any(target_env = "msvc", target_os = "openbsd")))]
    pub(super) fn now() -> Option<MemoryUsage> {
        jemalloc_ctl::epoch::advance().ok()?;
        Some(MemoryUsage {
            allocated: Bytes(jemalloc_ctl::stats::allocated::read().ok()? as isize),
            active: Bytes(jemalloc_ctl::stats::active::read().ok()? as isize),
            resident: Bytes(jemalloc_ctl::stats::resident::read().ok()? as isize),
        })
    }

    #[cfg(any(target_env = "msvc", target_os = "openbsd"))]
    pub(super) fn now() -> Option<MemoryUsage> {
        None
    }
}

/// Free-function form for ergonomic use as a drop-in for the previous
/// `profile::memory_usage()` API.
pub fn memory_usage() -> MemoryUsage {
    MemoryUsage::now()
}

#[derive(
    Default, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Serialize, Debug
)]
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
