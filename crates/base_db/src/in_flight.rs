/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! A single process-wide registry of in-flight work that holds (directly or
//! transitively) a salsa `Analysis` snapshot.
//!
//! A mutable database write on the LSP main loop blocks until every outstanding
//! snapshot is dropped (salsa's `cancel_others`); when that wait is long, the
//! watchdog and the salsa-write-stall reporter want to name *what* still holds a
//! snapshot — cause-agnostically, and all the way down.
//!
//! This lives in `elp_base_db` because it is the lowest crate common to every
//! layer that can pin the write, so all of them register into ONE map and the
//! stall reporters read the merged, oldest-first view in a single call — no
//! per-crate registries to merge. Callers namespace their labels by prefix:
//!
//!   * `elp` server — the work unit: `request:<method>`, `code_action:…`,
//!     `code_lens:…`, `compile_deps`, `update_cache`, …
//!   * `elp_ide` `native_diagnostics` — the recompute phase: `native:linters`,
//!     `native:redundant_suppression`, …
//!   * `elp_erlang_service` — the subprocess IPC leg: `erlang_service:CTI`,
//!     `erlang_service:COM:in_callback`, …
//!
//! So a wedge dumps the full chain, e.g. `["request:textDocument/codeAction",
//! "code_action:assists_with_fixes", "native:linters"]`, and the deepest entry
//! names the pin — subprocess leg or pure-Rust phase — without a stuck-stack.
//!
//! Registration is at coarse boundaries only — a spawned task / request handler,
//! a diagnostics phase, an IPC round-trip — never per snapshot clone, per linter,
//! or per node. That keeps the map to a handful of entries, so a single
//! `std::sync::Mutex` (no `parking_lot` dependency in this crate) is not a
//! contention concern, and the on-stall dump never serializes the very
//! snapshot-drops that unblock the main loop. Short-lived, main-thread-synchronous
//! snapshots are deliberately not registered: they are dropped within the turn and
//! can never be the thing the main-loop write is blocked behind.

use std::sync::LazyLock;
use std::sync::Mutex;
use std::sync::PoisonError;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::time::Duration;
use std::time::Instant;

use fxhash::FxHashMap;

static NEXT_ID: AtomicU64 = AtomicU64::new(0);

static REGISTRY: LazyLock<Mutex<FxHashMap<u64, Entry>>> =
    LazyLock::new(|| Mutex::new(FxHashMap::default()));

struct Entry {
    label: String,
    started_at: Instant,
    /// OS thread id of the thread that registered this work — captured here
    /// because that thread is the one holding the snapshot, so the stall
    /// reporter can introspect *its* kernel state (`/proc`), not just the main
    /// loop's, to tell an on-CPU pure-Rust pin from a blocked one. `None` off
    /// Linux.
    tid: Option<u32>,
}

/// A snapshot of one in-flight entry: its label, how long it has been held, and
/// the OS thread id holding it (for `/proc` introspection; `None` off Linux).
pub struct InFlight {
    pub label: String,
    pub age: Duration,
    pub tid: Option<u32>,
}

/// RAII handle for a registered unit of work. Deregisters on drop, so normal
/// completion, early return, cancellation unwind, and panic all clean up.
#[must_use = "the work stays registered until this guard is dropped"]
pub struct InFlightGuard {
    id: u64,
}

impl Drop for InFlightGuard {
    fn drop(&mut self) {
        REGISTRY
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .remove(&self.id);
    }
}

/// Register a unit of work that (directly or transitively) holds a snapshot.
/// Call at the start of the work and keep the returned guard alive for its
/// duration. Namespace `label` by prefix (see the module docs).
pub fn begin(label: impl Into<String>) -> InFlightGuard {
    let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
    REGISTRY
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .insert(
            id,
            Entry {
                label: label.into(),
                started_at: Instant::now(),
                tid: current_tid(),
            },
        );
    InFlightGuard { id }
}

/// Everything currently in flight, oldest first. Cheap: the map holds at most a
/// handful of entries. This is what the stall reporters dump.
pub fn in_flight() -> Vec<InFlight> {
    let now = Instant::now();
    let mut entries: Vec<InFlight> = REGISTRY
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .values()
        .map(|e| InFlight {
            label: e.label.clone(),
            age: now.saturating_duration_since(e.started_at),
            tid: e.tid,
        })
        .collect();
    entries.sort_by_key(|e| std::cmp::Reverse(e.age));
    entries
}

/// The calling thread's OS thread id, via `/proc/thread-self` (`<pid>/task/<tid>`
/// — the final component is the tid). Linux-only; `None` elsewhere.
#[cfg(target_os = "linux")]
fn current_tid() -> Option<u32> {
    std::fs::read_link("/proc/thread-self")
        .ok()
        .and_then(|p| p.file_name()?.to_str()?.parse::<u32>().ok())
}

#[cfg(not(target_os = "linux"))]
fn current_tid() -> Option<u32> {
    None
}
