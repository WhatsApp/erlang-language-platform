/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! A process-wide registry of in-flight units of work that hold a salsa
//! snapshot (`Analysis`).
//!
//! A mutable database write on the main loop blocks until every outstanding
//! snapshot is dropped (salsa's `cancel_others`); when that wait is long
//! (`report_db_write_stall`) we want to know *what* is still holding a snapshot,
//! generically — not just eqWAlizer.
//!
//! Registration is at the **work-unit** boundary (a spawned task or a request
//! handler), NOT per `Analysis`/snapshot clone. The work-unit boundary is
//! low-frequency — bounded by task-pool concurrency and the number of
//! outstanding requests — so a single mutex here is not a contention concern.
//! Registering per snapshot clone would instead sit on the hot path (one clone
//! per request and one per rayon worker per diagnostics batch) and the on-stall
//! dump could serialize the very snapshot-drops that unblock the main loop.
//! Short-lived, main-thread-synchronous snapshots are deliberately not tracked:
//! they are dropped within the turn and can never be the blocker.

use std::sync::LazyLock;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::time::Duration;
use std::time::Instant;

use fxhash::FxHashMap;
use parking_lot::Mutex;

static NEXT_ID: AtomicU64 = AtomicU64::new(0);

static REGISTRY: LazyLock<Mutex<FxHashMap<u64, WorkEntry>>> =
    LazyLock::new(|| Mutex::new(FxHashMap::default()));

struct WorkEntry {
    label: String,
    started_at: Instant,
}

/// RAII handle for a registered unit of work. Deregisters on drop, so normal
/// completion, early return, cancellation unwind, and panic all clean up.
#[must_use = "the work unit stays registered until this guard is dropped"]
pub(crate) struct WorkGuard {
    id: u64,
}

impl Drop for WorkGuard {
    fn drop(&mut self) {
        REGISTRY.lock().remove(&self.id);
    }
}

/// Register a unit of work that is about to hold a snapshot. Call at the start
/// of the spawned task / request closure and keep the returned guard alive for
/// its duration.
pub(crate) fn begin(label: impl Into<String>) -> WorkGuard {
    let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
    REGISTRY.lock().insert(
        id,
        WorkEntry {
            label: label.into(),
            started_at: Instant::now(),
        },
    );
    WorkGuard { id }
}

/// Currently in-flight work units as `(label, age)`, oldest first. Cheap: the
/// map holds at most a handful of entries (task-pool concurrency + outstanding
/// requests).
pub(crate) fn in_flight() -> Vec<(String, Duration)> {
    let now = Instant::now();
    let mut entries: Vec<(String, Duration)> = REGISTRY
        .lock()
        .values()
        .map(|e| (e.label.clone(), now.saturating_duration_since(e.started_at)))
        .collect();
    entries.sort_by_key(|(_, age)| std::cmp::Reverse(*age));
    entries
}
