/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Process-wide counters for a handful of salsa runtime events, used to
//! diagnose main-loop stalls.
//!
//! A mutable database access (e.g. applying a `DidChangeTextDocument` edit)
//! blocks the main loop until every outstanding read snapshot is dropped. Two
//! salsa behaviours make that wait longer and more frequent: interned-value GC,
//! which reuses interned slots and so invalidates the memos keyed on them
//! (forcing recomputation, including eqWAlizer reruns), and cross-thread query
//! blocking between the parallel diagnostics workers. These counters let us
//! correlate field stalls with both.
//!
//! The salsa event callback fires on arbitrary threads (rayon workers and the
//! main loop), so the counters are plain relaxed atomics and cumulative for the
//! lifetime of the process; the LSP server samples them periodically and reports
//! deltas.

use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use elp_base_db::salsa::EventKind;

static DID_INTERN: AtomicU64 = AtomicU64::new(0);
static DID_REUSE_INTERNED: AtomicU64 = AtomicU64::new(0);
static DID_VALIDATE_INTERNED: AtomicU64 = AtomicU64::new(0);
static WILL_BLOCK_ON: AtomicU64 = AtomicU64::new(0);
static DID_SET_CANCELLATION_FLAG: AtomicU64 = AtomicU64::new(0);

/// Snapshot of the cumulative salsa event counters.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SalsaEventCounts {
    /// A value was newly interned.
    pub did_intern: u64,
    /// An interned slot was reused (GC). Reuse bumps the id generation and
    /// invalidates every memo keyed on it, forcing recomputation.
    pub did_reuse_interned: u64,
    /// A previously interned value was revalidated in a new revision.
    pub did_validate_interned: u64,
    /// A query thread blocked waiting for another thread to finish the same
    /// query (parallel diagnostics contention).
    pub will_block_on: u64,
    /// A handle set the cancellation flag, i.e. a mutable DB write began. One
    /// per main-loop edit that reaches salsa.
    pub did_set_cancellation_flag: u64,
}

impl SalsaEventCounts {
    /// Per-field difference `self - earlier`, saturating at zero.
    pub fn delta_since(&self, earlier: &SalsaEventCounts) -> SalsaEventCounts {
        SalsaEventCounts {
            did_intern: self.did_intern.saturating_sub(earlier.did_intern),
            did_reuse_interned: self
                .did_reuse_interned
                .saturating_sub(earlier.did_reuse_interned),
            did_validate_interned: self
                .did_validate_interned
                .saturating_sub(earlier.did_validate_interned),
            will_block_on: self.will_block_on.saturating_sub(earlier.will_block_on),
            did_set_cancellation_flag: self
                .did_set_cancellation_flag
                .saturating_sub(earlier.did_set_cancellation_flag),
        }
    }
}

/// Record a salsa runtime event. Wire this in as the `salsa::Storage` event
/// callback. The events we care about are rare; the cost on the hot path is the
/// `Event` salsa constructs before calling us plus this branch.
pub fn record_event(kind: &EventKind) {
    match kind {
        EventKind::DidInternValue { .. } => {
            DID_INTERN.fetch_add(1, Ordering::Relaxed);
        }
        EventKind::DidReuseInternedValue { .. } => {
            DID_REUSE_INTERNED.fetch_add(1, Ordering::Relaxed);
        }
        EventKind::DidValidateInternedValue { .. } => {
            DID_VALIDATE_INTERNED.fetch_add(1, Ordering::Relaxed);
        }
        EventKind::WillBlockOn { .. } => {
            WILL_BLOCK_ON.fetch_add(1, Ordering::Relaxed);
        }
        EventKind::DidSetCancellationFlag => {
            DID_SET_CANCELLATION_FLAG.fetch_add(1, Ordering::Relaxed);
        }
        _ => {}
    }
}

/// Read the cumulative counters.
pub fn counts() -> SalsaEventCounts {
    SalsaEventCounts {
        did_intern: DID_INTERN.load(Ordering::Relaxed),
        did_reuse_interned: DID_REUSE_INTERNED.load(Ordering::Relaxed),
        did_validate_interned: DID_VALIDATE_INTERNED.load(Ordering::Relaxed),
        will_block_on: WILL_BLOCK_ON.load(Ordering::Relaxed),
        did_set_cancellation_flag: DID_SET_CANCELLATION_FLAG.load(Ordering::Relaxed),
    }
}
