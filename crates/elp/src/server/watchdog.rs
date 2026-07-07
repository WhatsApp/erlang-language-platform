/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! An out-of-band watchdog for the LSP main loop.
//!
//! The existing stall instrumentation is *post-hoc*: `timeit_exceeds!` fires
//! from a `Drop`, and `report_db_write_stall` compares `elapsed()` only after
//! the salsa write returns. Both can therefore report a slow turn but never a
//! turn that does not return — the exact failure we are chasing (a salsa-write
//! that blocks forever, a `project_loader` mutex held across a wedged buck2, a
//! cancellation hang). The periodic telemetry sampler runs on the main loop
//! too, so it dies with it.
//!
//! This watchdog lives on its own thread. The main loop publishes a heartbeat via
//! RAII guards: [`arm`] at the top of each turn returns a guard that disarms when
//! the turn ends (including on an early `?` return), and [`phase`] refines the
//! current section, returning a guard that restores the previous phase on drop —
//! so a phase names its blocker for exactly its scope and does not "stick" to
//! later work in the turn. When any one phase stays active past [`STALL_THRESHOLD`]
//! the watchdog reports it once, naming the phase and attaching everything we have
//! to identify the blocker: in-flight work units, salsa event counts, and
//! best-effort main-thread introspection (`/proc`, Linux).
//!
//! Robustness note: [`telemetry::send`] only *enqueues* onto a channel that the
//! (possibly stalled) main loop drains, so for a permanently wedged loop the
//! telemetry event is not delivered until/unless the loop resumes. Logging is
//! different: a record is delivered to the client by the lsp-server writer
//! thread (`LspLogger` -> `connection.sender`), not the main loop, so it gets
//! out even during a stall — but only if it clears the client sink's level
//! filter, which defaults to `Error`. The stall report therefore logs at `Error`
//! so it stays visible in the editor while the loop is wedged.

use std::sync::Arc;
use std::sync::LazyLock;
#[cfg(target_os = "linux")]
use std::sync::OnceLock;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;
use std::time::Instant;

use elp_base_db::in_flight;
use elp_log::telemetry;
use fxhash::FxHashMap;
use parking_lot::Mutex;

/// How often the watchdog wakes to inspect the heartbeat.
const POLL_INTERVAL: Duration = Duration::from_millis(500);

/// A single main-loop phase active longer than this is reported as a stall. Set
/// well above any healthy interactive turn — a turn still running after this long
/// is almost certainly wedged rather than merely slow — so the report is a strong
/// signal rather than noise from the occasional slow-but-recovering turn.
const STALL_THRESHOLD: Duration = Duration::from_secs(30);

/// Bumped on every [`arm`]/[`phase`] transition. The watchdog keys its
/// "report once" de-duplication on this, so a multi-second stall in one phase is
/// reported once, not once per poll.
static EPOCH: AtomicU64 = AtomicU64::new(0);

/// The main loop's current unit of work, or `None` while it is parked in
/// `next_event` waiting for the next message (idle waiting is not a stall).
static CURRENT: LazyLock<Mutex<Option<Beat>>> = LazyLock::new(|| Mutex::new(None));

#[derive(Clone)]
struct Beat {
    /// The event being processed this turn (e.g. `Notification { method: ... }`).
    turn_label: String,
    /// The finer phase within the turn (e.g. `vfs_salsa_write`). Equal to
    /// `turn_label` until a [`phase`] refines it.
    phase_label: String,
    turn_started: Instant,
    phase_started: Instant,
    epoch: u64,
}

/// Mark the start of a new main-loop turn. Call at the top of the loop and hold
/// the returned guard for the turn; it disarms on drop, so the heartbeat clears
/// even if the turn exits early via `?`.
pub(crate) fn arm(turn_label: impl Into<String>) -> TurnGuard {
    // Runs on the main-loop thread, so this is where its tid is observable for
    // the watchdog's later `/proc` introspection.
    capture_main_tid();
    let now = Instant::now();
    let epoch = EPOCH.fetch_add(1, Ordering::Relaxed);
    let label = turn_label.into();
    *CURRENT.lock() = Some(Beat {
        phase_label: label.clone(),
        turn_label: label,
        turn_started: now,
        phase_started: now,
        epoch,
    });
    TurnGuard
}

/// Disarms the heartbeat (main loop idle, parked waiting for the next event) when
/// dropped at the end of a turn — on the normal path and on an early `?` return.
#[must_use = "the turn stays armed until this guard is dropped"]
pub(crate) struct TurnGuard;

impl Drop for TurnGuard {
    fn drop(&mut self) {
        *CURRENT.lock() = None;
    }
}

/// Refine the current turn's phase, resetting the stall clock so the reported
/// duration is time-in-phase: the phase we are stuck in is what names the blocker.
/// Hold the returned guard for the scope of the phase; on drop it restores the
/// enclosing phase, so slow work later in the turn is not misattributed. No-op
/// when the loop is not armed (some of these paths also run during startup,
/// before the loop begins).
pub(crate) fn phase(phase_label: impl Into<String>) -> PhaseGuard {
    let now = Instant::now();
    let epoch = EPOCH.fetch_add(1, Ordering::Relaxed);
    let prev = CURRENT.lock().as_mut().map(|beat| {
        let prev = (beat.phase_label.clone(), beat.phase_started, beat.epoch);
        beat.phase_label = phase_label.into();
        beat.phase_started = now;
        beat.epoch = epoch;
        prev
    });
    PhaseGuard { prev }
}

/// Restores the enclosing phase on drop (see [`phase`]). Carries the previous
/// `(label, phase_started, epoch)` so the restored phase keeps its original stall
/// clock and de-duplication identity.
#[must_use = "the phase reverts as soon as this guard is dropped"]
pub(crate) struct PhaseGuard {
    prev: Option<(String, Instant, u64)>,
}

impl Drop for PhaseGuard {
    fn drop(&mut self) {
        let Some((label, started, epoch)) = self.prev.take() else {
            return;
        };
        if let Some(beat) = CURRENT.lock().as_mut() {
            beat.phase_label = label;
            beat.phase_started = started;
            beat.epoch = epoch;
        }
    }
}

/// Owns the watchdog thread; stops and joins it on drop. Hold it for the
/// lifetime of the main loop.
#[must_use = "the watchdog thread stops when this handle is dropped"]
pub(crate) struct Watchdog {
    stop: Arc<AtomicBool>,
    handle: Option<JoinHandle<()>>,
}

impl Drop for Watchdog {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::Relaxed);
        if let Some(handle) = self.handle.take() {
            let _ = handle.join();
        }
    }
}

/// Spawn the watchdog thread.
pub(crate) fn spawn() -> Watchdog {
    let stop = Arc::new(AtomicBool::new(false));
    let handle = {
        let stop = Arc::clone(&stop);
        thread::Builder::new()
            .name("elp-watchdog".to_string())
            .spawn(move || run(&stop))
            .expect("failed to spawn watchdog thread")
    };
    Watchdog {
        stop,
        handle: Some(handle),
    }
}

fn run(stop: &AtomicBool) {
    // The phase epoch we last reported on, so a stall is reported once per phase
    // rather than every poll.
    let mut reported_epoch: Option<u64> = None;
    while !stop.load(Ordering::Relaxed) {
        thread::sleep(POLL_INTERVAL);
        let Some(beat) = CURRENT.lock().clone() else {
            continue;
        };
        let stuck_for = beat.phase_started.elapsed();
        if stuck_for >= STALL_THRESHOLD && reported_epoch != Some(beat.epoch) {
            reported_epoch = Some(beat.epoch);
            report_stall(&beat, stuck_for);
        }
    }
}

fn report_stall(beat: &Beat, stuck_for: Duration) {
    // Whatever still holds a snapshot has held it for (some of) the stall, so it
    // is a candidate blocker. Oldest first.
    let in_flight_entries = in_flight::in_flight();
    let in_flight_work: Vec<String> = in_flight_entries
        .iter()
        .take(20)
        .map(|e| format!("{} ({}ms)", e.label, e.age.as_millis()))
        .collect();
    let stuck_ms = stuck_for.as_millis() as u64;
    let turn_ms = beat.turn_started.elapsed().as_millis() as u64;

    // What the main-loop thread is doing right now (`/proc`, Linux): the state
    // char and `wchan` distinguish the failure modes we care about (a lock/salsa
    // barrier vs a wedged subprocess).
    let main_thread = main_thread_state();
    let main_thread_suffix = main_thread
        .as_deref()
        .map(|s| format!("; main thread: {s}"))
        .unwrap_or_default();

    // The same introspection for the worker thread(s) actually holding the
    // snapshot the main-thread write is blocked behind — sampled *now*, during
    // the stall, while the holder is still parked. This is what distinguishes an
    // on-CPU pure-Rust pin (`state=R`) from one blocked on a lock/salsa barrier
    // (`futex`) or a subprocess pipe (`pipe_read`) — the last classification the
    // named-phase chain could not give on its own.
    let holder_threads = holder_thread_states(&in_flight_entries);
    let holder_suffix = if holder_threads.is_empty() {
        String::new()
    } else {
        format!("; holder threads: {holder_threads:?}")
    };

    // Delivery to the client (LspLogger -> connection.sender -> lsp-server writer
    // thread) is independent of the main loop, so this reaches the editor even
    // while the loop is wedged — but only if it clears the client sink's level
    // filter, which defaults to Error.
    log::error!(
        "main loop watchdog: stalled {stuck_ms}ms in phase '{}' (turn '{}', {turn_ms}ms total); in-flight work: {in_flight_work:?}{main_thread_suffix}{holder_suffix}",
        beat.phase_label,
        beat.turn_label,
    );

    let salsa = elp_ide::elp_ide_db::salsa_telemetry::counts();
    let mut data = serde_json::json!({
        "title": "ELP main loop watchdog stall",
        "phase": beat.phase_label,
        "turn": beat.turn_label,
        "stuck_ms": stuck_ms,
        "turn_ms": turn_ms,
        "in_flight_count": in_flight_entries.len(),
        "in_flight_work": in_flight_work,
        "in_flight_threads": holder_threads,
        "salsa_did_reuse_interned": salsa.did_reuse_interned,
        "salsa_will_block_on": salsa.will_block_on,
        "salsa_did_set_cancellation_flag": salsa.did_set_cancellation_flag,
    });
    if let Some(main_thread) = main_thread {
        data["main_thread"] = serde_json::Value::String(main_thread);
    }
    telemetry::send("main_loop_watchdog_stall".to_string(), data);
}

/// Best-effort one-line description of what a thread is doing right now, from
/// `/proc` — the thread state char (`R`/`S`/`D`/…) and `wchan` (the kernel
/// routine it is blocked in, e.g. `futex_wait` for a lock/salsa barrier vs
/// `pipe_read` for a wedged subprocess). No new dependency; Linux-only.
#[cfg(target_os = "linux")]
fn thread_state(tid: u32) -> Option<String> {
    let base = std::path::Path::new("/proc/self/task").join(tid.to_string());
    let comm = std::fs::read_to_string(base.join("comm"))
        .ok()
        .map(|s| s.trim().to_string());
    let wchan = std::fs::read_to_string(base.join("wchan"))
        .ok()
        .map(|s| s.trim().to_string());
    // `stat` field 3 is the state char. The `comm` in parens can itself contain
    // spaces and parens, so parse the fields after the last ')'.
    let state = std::fs::read_to_string(base.join("stat"))
        .ok()
        .and_then(|s| Some(s.rsplit_once(')')?.1.split_whitespace().next()?.to_string()));
    Some(format!(
        "state={} wchan={} comm={}",
        state.as_deref().unwrap_or("?"),
        wchan.as_deref().filter(|s| !s.is_empty()).unwrap_or("?"),
        comm.as_deref().unwrap_or("?"),
    ))
}

#[cfg(not(target_os = "linux"))]
fn thread_state(_tid: u32) -> Option<String> {
    None
}

/// `/proc` state of the main-loop thread (see [`thread_state`]).
fn main_thread_state() -> Option<String> {
    #[cfg(target_os = "linux")]
    {
        thread_state((*MAIN_TID.get()?)?)
    }
    #[cfg(not(target_os = "linux"))]
    {
        None
    }
}

/// `/proc` state of the worker thread(s) holding an outstanding snapshot — the
/// threads the main-loop write is actually blocked behind. `entries` is
/// oldest-first, and the guards on one holder thread are a nested stack, so the
/// *last* entry seen for a tid is its deepest (most specific) phase; we pair that
/// label with the thread's live kernel state. Deduped by tid — the nested
/// `request:… -> … -> native:linters` guards share one thread and one `/proc`
/// read.
fn holder_thread_states(entries: &[in_flight::InFlight]) -> Vec<String> {
    // tid -> deepest phase label seen for it; `order` records first-appearance
    // (oldest-first) so the output keeps that order without a linear scan.
    let mut deepest: FxHashMap<u32, String> = FxHashMap::default();
    let mut order: Vec<u32> = Vec::new();
    for entry in entries {
        let Some(tid) = entry.tid else { continue };
        if deepest.insert(tid, entry.label.clone()).is_none() {
            order.push(tid);
        }
    }
    order
        .into_iter()
        .take(20)
        .map(|tid| {
            let label = &deepest[&tid];
            let state = thread_state(tid).unwrap_or_else(|| "?".to_string());
            format!("{label} [tid {tid}]: {state}")
        })
        .collect()
}

/// The main-loop thread's tid, captured once (on that thread) for `/proc`.
#[cfg(target_os = "linux")]
static MAIN_TID: OnceLock<Option<u32>> = OnceLock::new();

#[cfg(target_os = "linux")]
fn capture_main_tid() {
    MAIN_TID.get_or_init(|| {
        // `/proc/thread-self` -> `<pid>/task/<tid>`; the final component is the
        // calling thread's tid.
        std::fs::read_link("/proc/thread-self")
            .ok()
            .and_then(|p| p.file_name()?.to_str()?.parse::<u32>().ok())
    });
}

#[cfg(not(target_os = "linux"))]
fn capture_main_tid() {}
