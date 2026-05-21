/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Native daemon mode for ELP.
//!
//! A persistent background process that keeps a project loaded in memory
//! for fast turnaround. Communicates via Unix domain sockets.

use std::collections::hash_map::DefaultHasher;
use std::env;
use std::fs;
use std::hash::Hash;
use std::hash::Hasher;
use std::io;
use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Write;
use std::os::unix::net::UnixListener;
use std::os::unix::net::UnixStream;
use std::path::Path;
use std::path::PathBuf;
use std::process;
use std::process::Stdio;
use std::thread;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

// ---------------------------------------------------------------------------
// Unix daemonization
// ---------------------------------------------------------------------------

/// Daemonize the current process using the daemonize crate.
///
/// This function performs the classic Unix double-fork to detach the process
/// from the parent process tree, allowing it to persist even if the parent exits.
#[cfg(unix)]
fn daemonize() -> Result<()> {
    use daemonize::Daemonize;

    let daemonize = Daemonize::new()
        .umask(0o027)
        // Keep the current working directory since the daemon needs access to project files
        // Traditional daemons chdir to "/", but we need to access the project
        .working_directory(std::env::current_dir()?)
        // Preserve the stdout/stderr FDs the parent wired up (start_daemon
        // attaches stderr to daemon.log). Without this, daemonize replaces
        // both with /dev/null, silently dropping every post-startup eprintln.
        .stdout(daemonize::Stdio::keep())
        .stderr(daemonize::Stdio::keep());

    daemonize.start().context("Failed to daemonize process")?;
    Ok(())
}

#[cfg(not(unix))]
fn daemonize() -> Result<()> {
    // No-op on non-Unix platforms
    Ok(())
}

use anyhow::Context;
use anyhow::Result;
use anyhow::bail;
use codespan_reporting::term::termcolor::ColorSpec;
use codespan_reporting::term::termcolor::WriteColor;
use elp::build::load;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp::read_lint_config_file;
use elp::watchman::UpdateResult;
use elp::watchman::Watchman;
use elp_eqwalizer::Mode;
use elp_ide::diagnostics::LintConfig;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_log::telemetry;
use elp_project_model::DiscoverConfig;
use elp_project_model::ElpConfig;
use elp_project_model::ProjectManifest;
use elp_project_model::buck::BuckQueryConfig;
use indicatif::ProgressBar;
use serde::Serialize;

use crate::args::DaemonCommand;
use crate::args::DaemonRun;
use crate::args::Eqwalize;
use crate::args::EqwalizeAll;
use crate::args::EqwalizeApp;
use crate::args::EqwalizeTarget;
use crate::args::Lint;
use crate::args::Shell;
use crate::eqwalizer_cli;
use crate::lint_cli;
use crate::reporting;
use crate::shell::ShellCommand;

// ---------------------------------------------------------------------------
// Socket path utilities
// ---------------------------------------------------------------------------

const DAEMON_DIR_PREFIX: &str = "elp-daemon-";

/// Default for `[daemon].idle_timeout_secs` when the project's `.elp.toml`
/// doesn't set one. 30 minutes — long enough to bridge typical "go for coffee"
/// breaks, short enough that idle daemons don't accumulate on dev hosts.
const DEFAULT_IDLE_TIMEOUT_SECS: u64 = 1800;

/// Resolve the daemon's idle timeout from the project config.
/// Returns `None` when the user explicitly set `0` ("never time out").
fn effective_idle_timeout(config: Option<u64>) -> Option<Duration> {
    let seconds = config.unwrap_or(DEFAULT_IDLE_TIMEOUT_SECS);
    if seconds == 0 {
        None
    } else {
        Some(Duration::from_secs(seconds))
    }
}

/// Extract the project root directory from a manifest.
fn project_root(manifest: &ProjectManifest) -> PathBuf {
    let p = manifest.root();
    let root: &Path = p.parent().unwrap_or(p).as_ref();
    root.to_path_buf()
}

fn daemon_id(canonical_root: &Path, profile: &str) -> String {
    let mut hasher = DefaultHasher::new();
    canonical_root.hash(&mut hasher);
    profile.hash(&mut hasher);
    format!("{:016x}", hasher.finish())
}

fn daemon_dir(canonical_root: &Path, profile: &str) -> PathBuf {
    let id = daemon_id(canonical_root, profile);
    env::temp_dir().join(format!("{DAEMON_DIR_PREFIX}{id}"))
}

// ---------------------------------------------------------------------------
// Wire protocol
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct DoneMessage {
    r#type: &'static str,
    status: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    stderr: Option<String>,
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    restart: bool,
}

impl DoneMessage {
    fn ok() -> Self {
        DoneMessage {
            r#type: "done",
            status: "ok",
            message: None,
            stderr: None,
            restart: false,
        }
    }

    fn error(msg: String) -> Self {
        DoneMessage {
            r#type: "done",
            status: "error",
            message: Some(msg),
            stderr: None,
            restart: false,
        }
    }

    fn with_restart(mut self) -> Self {
        self.restart = true;
        self
    }

    fn with_stderr(mut self, stderr: Vec<u8>) -> Self {
        if !stderr.is_empty() {
            self.stderr = Some(String::from_utf8_lossy(&stderr).into_owned());
        }
        self
    }
}

// ---------------------------------------------------------------------------
// SocketCli — Cli implementation that writes to a UnixStream
// ---------------------------------------------------------------------------

struct SocketCli {
    writer: BufWriter<UnixStream>,
    stderr: Vec<u8>,
}

impl SocketCli {
    fn new(stream: UnixStream) -> Self {
        Self {
            writer: BufWriter::new(stream),
            stderr: Vec::new(),
        }
    }
}

impl Write for SocketCli {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.writer.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}

impl WriteColor for SocketCli {
    fn supports_color(&self) -> bool {
        false
    }

    fn set_color(&mut self, _spec: &ColorSpec) -> io::Result<()> {
        Ok(())
    }

    fn reset(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl Cli for SocketCli {
    fn progress(&self, _len: u64, _prefix: &str) -> ProgressBar {
        ProgressBar::hidden()
    }

    fn simple_progress(&self, _len: u64, _prefix: &'static str) -> ProgressBar {
        ProgressBar::hidden()
    }

    fn spinner(&self, _prefix: &'static str) -> ProgressBar {
        ProgressBar::hidden()
    }

    fn err(&mut self) -> &mut dyn Write {
        &mut self.stderr
    }
}

// ---------------------------------------------------------------------------
// Daemon server
// ---------------------------------------------------------------------------

/// RAII guard that removes the daemon directory (and all files within) on drop.
/// Ensures cleanup on both normal exit and panic unwinding.
struct DaemonGuard {
    dir: PathBuf,
}

impl Drop for DaemonGuard {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

/// Immutable per-daemon state borrowed by every request handler.
struct DaemonContext<'a> {
    /// Discovered manifest root — used for shell command parsing.
    project: &'a Path,
    /// User-supplied `--project` arg — used for `.elp_lint.toml` walk-up.
    /// Can differ from `project` (Buck pushes manifest root much higher).
    user_project: &'a Path,
    manifest: &'a ProjectManifest,
    elp_config: &'a ElpConfig,
    query_config: &'a BuckQueryConfig,
    ifdef: bool,
}

/// Mutable per-daemon state that survives across requests.
struct DaemonState {
    loaded: LoadResult,
    watchman: Watchman,
    lint_config: LintConfig,
}

pub fn daemon_command(
    cmd: &DaemonCommand,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
    ifdef: bool,
) -> Result<()> {
    match cmd {
        DaemonCommand::Run(args) => run_daemon_server(args, cli, query_config, ifdef),
        DaemonCommand::Stop => stop_all_daemons(cli),
        DaemonCommand::Status(args) => show_status(&args.project, &args.profile, args.rebar, cli),
    }
}

fn run_daemon_server(
    args: &DaemonRun,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
    ifdef: bool,
) -> Result<()> {
    let start_time = SystemTime::now();

    // Discover manifest once — shared for daemon dir + project loading
    // Do this BEFORE daemonization so errors are reported to the parent
    let config = DiscoverConfig::new(args.rebar, &args.profile);
    let (elp_config, manifest) = load::discover_manifest(&args.project, &config)?;
    let root = project_root(&manifest);
    let dir = daemon_dir(&root, &args.profile);

    // Daemonize if requested (must happen before creating socket/pid files)
    if args.daemonize {
        fs::create_dir_all(&dir)?;
        daemonize()?;
    }

    // Create daemon directory and derive paths from it
    fs::create_dir_all(&dir)?;
    let _guard = DaemonGuard { dir: dir.clone() };
    let sock = dir.join("daemon.sock");
    let pid_file = dir.join("daemon.pid");
    let version_file = dir.join("daemon.version");

    // Check for existing daemon
    if sock.exists() {
        if UnixStream::connect(&sock).is_ok() {
            bail!("Daemon already running for this project");
        }
        // Stale socket, clean up
        let _ = fs::remove_file(&sock);
    }

    // Write PID and version files AFTER daemonization so PID is correct
    fs::write(&pid_file, process::id().to_string())?;
    fs::write(&version_file, elp::version())?;

    // Load project from already-discovered manifest (no re-discovery)
    let mut watchman = Watchman::new(&root)?;
    let mut loaded = load::load_project_from_manifest(
        cli,
        &manifest,
        &elp_config,
        IncludeOtp::Yes,
        Mode::Shell,
        query_config,
        ifdef,
    )?;
    watchman.set_project_dirs(&loaded);

    // Read .elp_lint.toml (walks up from project); missing → default; parse
    // error at spawn → fail fast. Reloaded on watchman events.
    let lint_config = read_lint_config_file(&args.project, &None)?;
    elp::apply_lint_config(&mut loaded.analysis_host, &lint_config);

    let mut state = DaemonState {
        loaded,
        watchman,
        lint_config,
    };
    let ctx = DaemonContext {
        project: &root,
        user_project: &args.project,
        manifest: &manifest,
        elp_config: &elp_config,
        query_config,
        ifdef,
    };

    telemetry::report_elapsed_time("daemon operational", start_time);

    eprintln!("[elp-daemon] Ready, listening on {}", sock.display());

    // Listen on Unix socket — use a dedicated thread for blocking accept()
    // and a channel to pass connections to the main loop. This avoids the
    // busy-wait of set_nonblocking + sleep and eliminates connection latency.
    let listener = UnixListener::bind(&sock)?;
    let (conn_tx, conn_rx) = crossbeam_channel::unbounded();
    thread::spawn(move || {
        loop {
            match listener.accept() {
                Ok((stream, _)) => {
                    if conn_tx.send(stream).is_err() {
                        break;
                    }
                }
                Err(e) => {
                    eprintln!("[elp-daemon] Accept error: {e}");
                    break;
                }
            }
        }
    });

    let idle_timeout = effective_idle_timeout(elp_config.daemon.idle_timeout_secs);
    let mut last_activity = Instant::now();

    loop {
        if let Some(timeout) = idle_timeout
            && last_activity.elapsed() > timeout
        {
            eprintln!("[elp-daemon] Idle timeout reached, shutting down");
            break;
        }
        match conn_rx.recv_timeout(Duration::from_secs(2)) {
            Ok(stream) => {
                last_activity = Instant::now();
                let should_stop = handle_connection(stream, &mut state, &ctx, cli);
                if let Ok(true) = should_stop {
                    eprintln!("[elp-daemon] Stop requested, shutting down");
                    break;
                }
                if let Err(e) = should_stop {
                    eprintln!("[elp-daemon] Error handling connection: {e}");
                }
            }
            Err(crossbeam_channel::RecvTimeoutError::Timeout) => continue,
            Err(crossbeam_channel::RecvTimeoutError::Disconnected) => {
                eprintln!("[elp-daemon] Accept thread terminated");
                break;
            }
        }
    }

    // _guard's Drop cleans up the daemon directory
    telemetry::report_elapsed_time("daemon done", start_time);
    eprintln!("[elp-daemon] Shutdown complete");
    Ok(())
}

/// Handle a single client connection. Returns Ok(true) if the daemon should stop.
fn handle_connection(
    stream: UnixStream,
    state: &mut DaemonState,
    ctx: &DaemonContext<'_>,
    cli: &mut dyn Cli,
) -> Result<bool> {
    let mut reader = BufReader::new(&stream);
    let mut line = String::new();
    reader.read_line(&mut line)?;
    let line = line.trim_end().to_string();

    if line.is_empty() {
        return Ok(false);
    }

    // Strip the JSON payload from log output for lint commands — it can be
    // multi-KB and dominates the log file.
    if let Some(prefix) = line.split_once(' ').map(|(p, _)| p)
        && prefix == "lint"
    {
        eprintln!("[elp-daemon] Command: lint <json>");
    } else {
        eprintln!("[elp-daemon] Command: {line}");
    }

    // Handle stop command
    if line == "__stop__" {
        let done = serde_json::to_string(&DoneMessage::ok())?;
        let mut writer = BufWriter::new(&stream);
        writeln!(writer, "{done}")?;
        writer.flush()?;
        return Ok(true);
    }

    // Check for file changes and apply them
    match state.watchman.poll_and_apply_changes(&mut state.loaded)? {
        UpdateResult::NeedsRestart { reason } => {
            eprintln!("[elp-daemon] {reason}");
            let done = serde_json::to_string(&DoneMessage::ok().with_restart())?;
            let mut writer = BufWriter::new(&stream);
            writeln!(writer, "{done}")?;
            writer.flush()?;
            return Ok(true);
        }
        UpdateResult::NeedsFullReload { reason } => {
            eprintln!("[elp-daemon] {reason}");
            state.loaded = load::load_project_from_manifest(
                cli,
                ctx.manifest,
                ctx.elp_config,
                IncludeOtp::Yes,
                Mode::Shell,
                ctx.query_config,
                ctx.ifdef,
            )?;
            state.watchman.set_project_dirs(&state.loaded);
            // Fresh analysis_host loses the lint config; re-apply.
            elp::apply_lint_config(&mut state.loaded.analysis_host, &state.lint_config);
        }
        UpdateResult::NeedsLintConfigReload { reason } => {
            eprintln!("[elp-daemon] {reason}");
            // Parse failure → restart rather than carry stale config forward.
            match read_lint_config_file(ctx.user_project, &None) {
                Ok(new_config) => {
                    state.lint_config = new_config;
                    elp::apply_lint_config(&mut state.loaded.analysis_host, &state.lint_config);
                }
                Err(e) => {
                    eprintln!("[elp-daemon] Failed to reload .elp_lint.toml, restarting: {e}");
                    let done = serde_json::to_string(&DoneMessage::ok().with_restart())?;
                    let mut writer = BufWriter::new(&stream);
                    writeln!(writer, "{done}")?;
                    writer.flush()?;
                    return Ok(true);
                }
            }
        }
        UpdateResult::Updated => {}
    }

    // Lint requests use a JSON-encoded `Lint` struct on the wire — too many
    // flags to express through the shell parser. Handle them before falling
    // through to ShellCommand::parse.
    if let Some(json) = line.strip_prefix("lint ") {
        let mut socket_cli = SocketCli::new(stream.try_clone()?);
        let done = match serde_json::from_str::<Lint>(json) {
            Ok(mut lint_args) => {
                lint_args.format = Some("json".to_string());
                match lint_cli::do_lint(
                    &lint_args,
                    &state.lint_config,
                    &mut state.loaded,
                    &mut socket_cli,
                ) {
                    Ok(()) => DoneMessage::ok(),
                    Err(e) => DoneMessage::error(e.to_string()),
                }
            }
            Err(e) => DoneMessage::error(format!("invalid lint payload: {e}")),
        };
        let done = done.with_stderr(std::mem::take(&mut socket_cli.stderr));
        let done = serde_json::to_string(&done)?;
        writeln!(socket_cli, "{done}")?;
        socket_cli.flush()?;
        return Ok(false);
    }

    // Create a shell struct for command parsing
    let shell = Shell {
        project: ctx.project.to_path_buf(),
        command: vec![],
    };

    // Parse and execute command, writing output to the socket
    let mut socket_cli = SocketCli::new(stream.try_clone()?);

    let (done, should_quit) = match ShellCommand::parse(&shell, line) {
        Ok(None) => (DoneMessage::ok(), false),
        Ok(Some(ShellCommand::Help)) => (DoneMessage::ok(), false),
        Ok(Some(ShellCommand::Quit)) => (DoneMessage::ok(), true),
        Ok(Some(ShellCommand::ShellEqwalize(mut eqwalize))) => {
            eqwalize.format = Some("json".to_string());
            let done = match eqwalizer_cli::do_eqwalize_module(
                &eqwalize,
                &mut state.loaded,
                &mut socket_cli,
            ) {
                Ok(()) => DoneMessage::ok(),
                Err(e) => DoneMessage::error(e.to_string()),
            };
            (done, false)
        }
        Ok(Some(ShellCommand::ShellEqwalizeApp(mut eqwalize_app))) => {
            eqwalize_app.format = Some("json".to_string());
            let done = match eqwalizer_cli::do_eqwalize_app(
                &eqwalize_app,
                &mut state.loaded,
                &mut socket_cli,
            ) {
                Ok(()) => DoneMessage::ok(),
                Err(e) => DoneMessage::error(e.to_string()),
            };
            (done, false)
        }
        Ok(Some(ShellCommand::ShellEqwalizeAll(mut eqwalize_all))) => {
            eqwalize_all.format = Some("json".to_string());
            let done = match eqwalizer_cli::do_eqwalize_all(
                &eqwalize_all,
                &mut state.loaded,
                &mut socket_cli,
            ) {
                Ok(()) => DoneMessage::ok(),
                Err(e) => DoneMessage::error(e.to_string()),
            };
            (done, false)
        }
        Ok(Some(ShellCommand::ShellEqwalizeTarget(mut eqwalize_target))) => {
            eqwalize_target.format = Some("json".to_string());
            let done = match eqwalizer_cli::do_eqwalize_target(
                &eqwalize_target,
                &mut state.loaded,
                &mut socket_cli,
            ) {
                Ok(()) => DoneMessage::ok(),
                Err(e) => DoneMessage::error(e.to_string()),
            };
            (done, false)
        }
        Err(err) => (DoneMessage::error(err.to_string()), false),
    };

    // Attach any collected stderr to the done message and send it
    let done = done.with_stderr(std::mem::take(&mut socket_cli.stderr));
    let done = serde_json::to_string(&done)?;
    writeln!(socket_cli, "{done}")?;
    socket_cli.flush()?;
    Ok(should_quit)
}

// ---------------------------------------------------------------------------
// Daemon client (connect mode)
// ---------------------------------------------------------------------------

fn connect_and_run(
    command_line: &str,
    project: &Path,
    profile: &str,
    rebar: bool,
    format_json: bool,
    cli: &mut dyn Cli,
) -> Result<()> {
    let conf = DiscoverConfig::new(rebar, profile);
    let (_elp_config, manifest) = load::discover_manifest(project, &conf)?;
    let root = project_root(&manifest);
    let dir = daemon_dir(&root, profile);
    let sock = dir.join("daemon.sock");

    // Try to connect, auto-start if needed
    let mut stream = match UnixStream::connect(&sock) {
        Ok(s) => s,
        Err(_) => start_daemon(&dir, &sock, project, profile, rebar)?,
    };

    // Check for version mismatch; if the daemon is from a different build, restart it
    let version_file = dir.join("daemon.version");
    if let Ok(daemon_version) = fs::read_to_string(&version_file)
        && daemon_version.trim() != elp::version()
    {
        eprintln!(
            "Daemon version mismatch (daemon: {}, client: {}), restarting...",
            daemon_version.trim(),
            elp::version()
        );
        let mut writer = BufWriter::new(&stream);
        let _ = writeln!(writer, "__stop__");
        let _ = writer.flush();
        drop(writer);
        drop(stream);

        let start = Instant::now();
        while sock.exists() && start.elapsed() < Duration::from_secs(10) {
            thread::sleep(Duration::from_millis(100));
        }
        cleanup_stale_in_dir(&dir);

        stream = start_daemon(&dir, &sock, project, profile, rebar)?;
    }

    // Send command
    let mut writer = BufWriter::new(&stream);
    writeln!(writer, "{command_line}")?;
    writer.flush()?;
    // Signal end of request by shutting down write side
    stream.shutdown(std::net::Shutdown::Write)?;

    // Read response lines
    let reader = BufReader::new(&stream);
    let mut exit_code = 0;
    let mut diagnostic_count: usize = 0;
    for line in reader.lines() {
        let line = line?;
        // Try to detect done message first (small JSON with "type" field)
        let v: serde_json::Value = serde_json::from_str(&line)?;
        if v.get("type").and_then(|t| t.as_str()) == Some("done") {
            // Check for restart request (e.g., config change)
            if v.get("restart").and_then(|r| r.as_bool()) == Some(true) {
                eprintln!("ELP config changed, restarting daemon...");
                // Wait for daemon to shut down
                let start = Instant::now();
                while sock.exists() && start.elapsed() < Duration::from_secs(10) {
                    thread::sleep(Duration::from_millis(100));
                }
                cleanup_stale_in_dir(&dir);
                // Start new daemon and retry the command
                return connect_and_run(command_line, project, profile, rebar, format_json, cli);
            }
            // Forward any stderr from the daemon
            if let Some(stderr) = v.get("stderr").and_then(|s| s.as_str()) {
                write!(cli.err(), "{stderr}")?;
            }
            if v.get("status").and_then(|s| s.as_str()) == Some("error") {
                exit_code = 1;
                if let Some(msg) = v.get("message").and_then(|m| m.as_str()) {
                    writeln!(cli.err(), "Error: {msg}")?;
                }
            }
            break;
        }
        // Diagnostic line
        diagnostic_count += 1;
        if format_json {
            writeln!(cli, "{line}")?;
        } else {
            let diag: elp::arc_types::Diagnostic = serde_json::from_str(&line)?;
            write!(cli, "{diag}")?;
        }
    }

    if !format_json {
        reporting::write_error_count_summary(cli, diagnostic_count)?;
    }

    if exit_code != 0 {
        // Shared between eqwalize and lint; the daemon's own error message
        // was already printed to stderr above, so the bail is just an
        // exit-code carrier.
        bail!("Errors found");
    }
    Ok(())
}

/// Spawn a new daemon process and wait for it to become ready.
fn start_daemon(
    dir: &Path,
    sock: &Path,
    project: &Path,
    profile: &str,
    rebar: bool,
) -> Result<UnixStream> {
    cleanup_stale_in_dir(dir);

    eprintln!("Starting elp daemon...");
    let exe = env::current_exe()?;
    let log = dir.join("daemon.log");
    fs::create_dir_all(dir)?;
    let log_file = fs::File::create(&log)?;

    let mut cmd = process::Command::new(exe);
    cmd.arg("daemon")
        .arg("--project")
        .arg(project)
        .arg("--as")
        .arg(profile)
        .arg("--daemonize");
    if rebar {
        cmd.arg("--rebar");
    }
    cmd.stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(log_file);

    let _child = cmd.spawn()?;

    // Wait for socket to appear
    let start = Instant::now();
    loop {
        thread::sleep(Duration::from_secs(1));
        if let Ok(s) = UnixStream::connect(sock) {
            eprintln!("Daemon ready.");
            return Ok(s);
        }
        if start.elapsed() > Duration::from_secs(120) {
            bail!(
                "Daemon failed to start within 120s. Check {}",
                log.display()
            );
        }
    }
}

// ---------------------------------------------------------------------------
// Public connect entry points
// ---------------------------------------------------------------------------

pub fn connect_eqwalize(args: &Eqwalize, cli: &mut dyn Cli) -> Result<()> {
    let cmd = format!("eqwalize {}", args.modules.join(" "));
    let format_json = args.format.is_some();
    connect_and_run(
        &cmd,
        &args.project,
        &args.profile,
        args.rebar,
        format_json,
        cli,
    )
}

pub fn connect_eqwalize_all(args: &EqwalizeAll, cli: &mut dyn Cli) -> Result<()> {
    let cmd = "eqwalize-all".to_string();
    let format_json = args.format.is_some();
    connect_and_run(
        &cmd,
        &args.project,
        &args.profile,
        args.rebar,
        format_json,
        cli,
    )
}

pub fn connect_eqwalize_app(args: &EqwalizeApp, cli: &mut dyn Cli) -> Result<()> {
    let cmd = format!("eqwalize-app {}", args.app);
    let format_json = args.format.is_some();
    connect_and_run(
        &cmd,
        &args.project,
        &args.profile,
        args.rebar,
        format_json,
        cli,
    )
}

pub fn connect_eqwalize_target(args: &EqwalizeTarget, cli: &mut dyn Cli) -> Result<()> {
    let cmd = format!("eqwalize-target {}", args.target);
    let format_json = args.format.is_some();
    // eqwalize-target is buck-only, so profile is always "test" and rebar is always false
    connect_and_run(&cmd, &args.project, "test", false, format_json, cli)
}

/// Reject `Lint` flag combinations that don't make sense in daemon mode.
///
/// Some flags (config sources) must be fixed at daemon spawn time; others
/// (filesystem outputs, fix application, process-level stats) would resolve
/// against the daemon process instead of the client.
fn validate_lint_for_daemon(args: &Lint) -> Result<()> {
    if args.read_config {
        bail!(
            "--read-config is not supported with --connect; the daemon reads .elp_lint.toml at startup"
        );
    }
    if args.config_file.is_some() {
        bail!(
            "--config-file is not supported with --connect; the daemon's lint config is fixed at startup"
        );
    }
    if args.to.is_some() {
        bail!(
            "--to is not supported with --connect (paths resolve relative to the daemon process, not the client)"
        );
    }
    if args.report_system_stats {
        bail!(
            "--report-system-stats is not supported with --connect (would measure daemon process state, not lint cost)"
        );
    }
    if args.apply_fix {
        bail!("--apply-fix is not yet supported with --connect");
    }
    // `--no-diags` flips the JSON writer to emit a plain-text per-module
    // summary, which the daemon client's per-line JSON parser would reject.
    if !args.print_diags {
        bail!(
            "--no-diags is not supported with --connect (the daemon's JSON wire format has no plain-text summary)"
        );
    }
    Ok(())
}

pub fn connect_lint(args: &Lint, cli: &mut dyn Cli) -> Result<()> {
    validate_lint_for_daemon(args)?;
    let cmd = format!("lint {}", serde_json::to_string(args)?);
    let format_json = args.format.is_some();
    connect_and_run(
        &cmd,
        &args.project,
        &args.profile,
        args.rebar,
        format_json,
        cli,
    )
}

// ---------------------------------------------------------------------------
// Stop / Status
// ---------------------------------------------------------------------------

fn stop_all_daemons(cli: &mut dyn Cli) -> Result<()> {
    let tmp = env::temp_dir();
    let mut stopped = 0u32;
    let mut found = 0u32;

    if let Ok(entries) = fs::read_dir(&tmp) {
        for entry in entries.flatten() {
            let name = entry.file_name();
            let name = name.to_string_lossy();
            if !name.starts_with(DAEMON_DIR_PREFIX) {
                continue;
            }
            if !entry.path().is_dir() {
                continue;
            }
            found += 1;
            if stop_daemon_in_dir(&entry.path()) {
                writeln!(cli, "Stopped daemon in {}", entry.path().display())?;
                stopped += 1;
            }
        }
    }

    if found == 0 {
        writeln!(cli, "No daemons found.")?;
    } else if stopped == 0 {
        writeln!(cli, "No running daemons found.")?;
    } else {
        writeln!(cli, "Stopped {stopped} daemon(s).")?;
    }
    Ok(())
}

fn stop_daemon_in_dir(dir: &Path) -> bool {
    let sock_file = dir.join("daemon.sock");
    let pid_file = dir.join("daemon.pid");

    // Try graceful stop via socket
    if let Ok(stream) = UnixStream::connect(&sock_file) {
        let mut writer = BufWriter::new(&stream);
        if writeln!(writer, "__stop__").is_ok() && writer.flush().is_ok() {
            // Read response
            let mut buf = [0u8; 256];
            let _ = io::Read::read(&mut &stream, &mut buf);
            return true;
        }
    }

    // Fallback: kill via PID file
    if let Ok(pid_str) = fs::read_to_string(&pid_file) {
        let pid = pid_str.trim();
        if process::Command::new("kill")
            .arg(pid)
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false)
        {
            return true;
        }
    }

    false
}

fn show_status(project: &Path, profile: &str, rebar: bool, cli: &mut dyn Cli) -> Result<()> {
    let conf = DiscoverConfig::new(rebar, profile);
    let (_elp_config, manifest) = load::discover_manifest(project, &conf)?;
    let root = project_root(&manifest);
    let dir = daemon_dir(&root, profile);
    let sock = dir.join("daemon.sock");
    let pid_file = dir.join("daemon.pid");
    let log = dir.join("daemon.log");

    if UnixStream::connect(&sock).is_ok() {
        let pid = fs::read_to_string(&pid_file).unwrap_or_else(|_| "unknown".to_string());
        writeln!(cli, "Daemon running (PID: {})", pid.trim())?;
        writeln!(cli, "Socket: {}", sock.display())?;
        writeln!(cli, "Log: {}", log.display())?;
    } else {
        writeln!(cli, "No daemon running for this project.")?;
        if log.exists() {
            writeln!(cli, "Last log: {}", log.display())?;
        }
    }
    Ok(())
}

fn cleanup_stale_in_dir(dir: &Path) {
    let sock = dir.join("daemon.sock");
    let pid_file = dir.join("daemon.pid");

    if sock.exists() {
        let _ = fs::remove_file(&sock);
    }
    if pid_file.exists() {
        // Check if process is actually dead
        if let Ok(pid_str) = fs::read_to_string(&pid_file) {
            let pid = pid_str.trim();
            // kill -0 checks if process exists without sending a signal
            let alive = process::Command::new("kill")
                .args(["-0", pid])
                .output()
                .map(|o| o.status.success())
                .unwrap_or(false);
            if !alive {
                let _ = fs::remove_file(&pid_file);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // -- DoneMessage serialization --

    #[test]
    fn done_message_ok() {
        let json = serde_json::to_string(&DoneMessage::ok()).unwrap();
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(v["type"], "done");
        assert_eq!(v["status"], "ok");
        assert!(v.get("message").is_none());
        assert!(v.get("stderr").is_none());
    }

    #[test]
    fn done_message_error() {
        let json = serde_json::to_string(&DoneMessage::error("bad thing".into())).unwrap();
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(v["type"], "done");
        assert_eq!(v["status"], "error");
        assert_eq!(v["message"], "bad thing");
        assert!(v.get("stderr").is_none());
    }

    #[test]
    fn done_message_with_stderr_non_empty() {
        let msg = DoneMessage::ok().with_stderr(b"some warning\n".to_vec());
        let json = serde_json::to_string(&msg).unwrap();
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(v["stderr"], "some warning\n");
    }

    #[test]
    fn done_message_with_stderr_empty() {
        let msg = DoneMessage::ok().with_stderr(vec![]);
        let json = serde_json::to_string(&msg).unwrap();
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert!(v.get("stderr").is_none());
    }

    // -- effective_idle_timeout --

    #[test]
    fn effective_idle_timeout_uses_default_when_unset() {
        use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;

        let expected = Some(Duration::from_secs(DEFAULT_IDLE_TIMEOUT_SECS));
        assert_eq_expected!(expected, effective_idle_timeout(None));
    }

    #[test]
    fn effective_idle_timeout_honours_config() {
        use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;

        let expected = Some(Duration::from_secs(60));
        assert_eq_expected!(expected, effective_idle_timeout(Some(60)));
    }

    #[test]
    fn effective_idle_timeout_zero_means_never() {
        use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;

        let expected: Option<Duration> = None;
        assert_eq_expected!(expected, effective_idle_timeout(Some(0)));
    }

    // -- Lint payload + validation --

    /// Locks down the wire JSON shape AND verifies both encode and decode
    /// directions. If a new field is added to `Lint`, this snapshot will
    /// fail and force an intentional update (run with `UPDATE_EXPECT=1`),
    /// which surfaces the wire-format change at review time.
    #[test]
    fn lint_payload_round_trips_json() {
        use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;
        use expect_test::expect;

        let original = Lint {
            project: PathBuf::from("/some/project"),
            module: Some("my_mod".to_string()),
            app: Some("my_app".to_string()),
            file: vec!["a.erl".to_string(), "b.erl".to_string()],
            path: Some(PathBuf::from("/some/path")),
            rebar: true,
            profile: "prod".to_string(),
            connect: true,
            include_generated: true,
            print_diags: false,
            format: Some("json".to_string()),
            include_erlc_diagnostics: true,
            include_eqwalizer_diagnostics: true,
            include_suppressed: true,
            use_cli_severity: true,
            severity: Some("warning".to_string()),
            diagnostic_ignore: Some("W0001".to_string()),
            diagnostic_filter: Some("W0010".to_string()),
            experimental_diags: true,
            arc_patch: true,
            ignore_apps: vec!["dep_a".to_string(), "dep_b".to_string()],
            ..Lint::default()
        };

        // struct -> JSON (pretty-printed for readable snapshot)
        let expected_json = serde_json::to_string_pretty(&original).unwrap();
        expect![[r#"
            {
              "project": "/some/project",
              "module": "my_mod",
              "app": "my_app",
              "file": [
                "a.erl",
                "b.erl"
              ],
              "path": "/some/path",
              "rebar": true,
              "profile": "prod",
              "connect": true,
              "include_generated": true,
              "include_tests": false,
              "print_diags": false,
              "format": "json",
              "include_erlc_diagnostics": true,
              "include_ct_diagnostics": false,
              "include_edoc_diagnostics": false,
              "include_eqwalizer_diagnostics": true,
              "include_suppressed": true,
              "use_cli_severity": true,
              "severity": "warning",
              "diagnostic_ignore": "W0001",
              "diagnostic_filter": "W0010",
              "experimental_diags": true,
              "read_config": false,
              "config_file": null,
              "apply_fix": false,
              "ignore_fix_only": false,
              "fixme_fix_only": false,
              "to": null,
              "recursive": false,
              "with_check": false,
              "check_eqwalize_all": false,
              "one_shot": false,
              "arc_patch": true,
              "report_system_stats": false,
              "no_stream": false,
              "ignore_apps": [
                "dep_a",
                "dep_b"
              ]
            }"#]]
        .assert_eq(&expected_json);

        // JSON -> struct -> JSON (other direction; same JSON must come back)
        let parsed: Lint = serde_json::from_str(&expected_json).unwrap();
        let reserialized = serde_json::to_string_pretty(&parsed).unwrap();
        assert_eq_expected!(expected_json, reserialized);
    }

    #[test]
    fn lint_payload_tolerates_unknown_fields() {
        use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;

        // Forward compatibility: an older daemon receiving a newer client's
        // payload should ignore fields it doesn't recognise rather than error.
        let json = r#"{"project":"/p","profile":"test","__future_field__":42}"#;
        let parsed: Lint = serde_json::from_str(json).unwrap();
        let expected_project = PathBuf::from("/p");
        assert_eq_expected!(expected_project, parsed.project);
        let expected_profile = "test";
        assert_eq_expected!(expected_profile, parsed.profile);
    }

    #[test]
    fn validate_lint_for_daemon_accepts_safe_args() {
        let args = Lint {
            project: PathBuf::from("."),
            module: Some("foo".to_string()),
            connect: true,
            // print_diags=true matches the bpaf default for `--no-diags`
            // (absent ⇒ true). Lint::default() leaves it at bool::default()=false.
            print_diags: true,
            ..Lint::default()
        };
        assert!(validate_lint_for_daemon(&args).is_ok());
    }

    fn assert_rejects_with_flag(flag: &str, args: Lint) {
        let err = match validate_lint_for_daemon(&args) {
            Ok(()) => panic!("expected {flag} to be rejected with --connect"),
            Err(e) => e,
        };
        let msg = err.to_string();
        assert!(
            msg.contains(flag),
            "error message for {flag} should mention the flag name; got: {msg}"
        );
    }

    #[test]
    fn validate_lint_for_daemon_rejects_read_config() {
        assert_rejects_with_flag(
            "--read-config",
            Lint {
                connect: true,
                read_config: true,
                ..Lint::default()
            },
        );
    }

    #[test]
    fn validate_lint_for_daemon_rejects_config_file() {
        assert_rejects_with_flag(
            "--config-file",
            Lint {
                connect: true,
                config_file: Some("x.toml".to_string()),
                ..Lint::default()
            },
        );
    }

    #[test]
    fn validate_lint_for_daemon_rejects_to() {
        assert_rejects_with_flag(
            "--to",
            Lint {
                connect: true,
                to: Some(PathBuf::from("/tmp/out")),
                ..Lint::default()
            },
        );
    }

    #[test]
    fn validate_lint_for_daemon_rejects_report_system_stats() {
        assert_rejects_with_flag(
            "--report-system-stats",
            Lint {
                connect: true,
                report_system_stats: true,
                ..Lint::default()
            },
        );
    }

    #[test]
    fn validate_lint_for_daemon_rejects_apply_fix() {
        assert_rejects_with_flag(
            "--apply-fix",
            Lint {
                connect: true,
                apply_fix: true,
                ..Lint::default()
            },
        );
    }

    #[test]
    fn validate_lint_for_daemon_rejects_no_diags() {
        // `print_diags = false` corresponds to passing `--no-diags`.
        assert_rejects_with_flag(
            "--no-diags",
            Lint {
                connect: true,
                print_diags: false,
                ..Lint::default()
            },
        );
    }

    // -- Diagnostic Display --

    #[test]
    fn diagnostic_display_full() {
        let diag = elp::arc_types::Diagnostic::new(
            Path::new("src/foo.erl"),
            42,
            Some(10),
            elp::arc_types::Severity::Warning,
            "incompatible_types".to_string(),
            "Expected integer(), got atom()".to_string(),
            None,
            None,
        );
        let output = format!("{diag}");
        assert_eq!(
            output,
            "warning: src/foo.erl:42:10 [incompatible_types]\n  Expected integer(), got atom()\n"
        );
    }

    #[test]
    fn diagnostic_display_no_char_column() {
        let diag = elp::arc_types::Diagnostic::new(
            Path::new("src/bar.erl"),
            7,
            None,
            elp::arc_types::Severity::Error,
            "syntax_error".to_string(),
            "Unexpected token".to_string(),
            None,
            None,
        );
        let output = format!("{diag}");
        assert_eq!(
            output,
            "error: src/bar.erl:7 [syntax_error]\n  Unexpected token\n"
        );
    }

    #[test]
    fn diagnostic_roundtrip_display() {
        let diag = elp::arc_types::Diagnostic::new(
            Path::new("src/foo.erl"),
            42,
            Some(10),
            elp::arc_types::Severity::Warning,
            "incompatible_types".to_string(),
            "Expected integer(), got atom()".to_string(),
            None,
            None,
        );
        let json = serde_json::to_string(&diag).unwrap();
        let parsed: elp::arc_types::Diagnostic = serde_json::from_str(&json).unwrap();
        assert_eq!(format!("{diag}"), format!("{parsed}"));
    }

    #[test]
    fn diagnostic_display_with_end_position() {
        use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;

        let diag = elp::arc_types::Diagnostic::new(
            Path::new("src/foo.erl"),
            42,
            Some(10),
            elp::arc_types::Severity::Warning,
            "incompatible_types".to_string(),
            "Expected integer(), got atom()".to_string(),
            None,
            None,
        )
        .with_end_position(43, 6);
        assert_eq_expected!(
            "warning: src/foo.erl:42:10-43:6 [incompatible_types]\n  Expected integer(), got atom()\n",
            format!("{diag}")
        );
    }

    #[test]
    fn diagnostic_roundtrip_display_with_end_position() {
        let diag = elp::arc_types::Diagnostic::new(
            Path::new("src/foo.erl"),
            42,
            Some(10),
            elp::arc_types::Severity::Warning,
            "incompatible_types".to_string(),
            "Expected integer(), got atom()".to_string(),
            None,
            None,
        )
        .with_end_position(43, 6);
        let json = serde_json::to_string(&diag).unwrap();
        let parsed: elp::arc_types::Diagnostic = serde_json::from_str(&json).unwrap();
        assert_eq!(format!("{diag}"), format!("{parsed}"));
    }

    #[test]
    fn diagnostic_display_eqwalizer_format() {
        let desc = "```lang=error,counterexample\n`'error'`.\nExpression has type: 'error'\nContext expected type: 'ok'\n```\n\n> [docs on `incompatible_types`](https://fb.me/eqwalizer_errors#incompatible_types)";
        let diag = elp::arc_types::Diagnostic::new(
            Path::new("src/foo.erl"),
            42,
            Some(10),
            elp::arc_types::Severity::Error,
            "eqWAlizer: incompatible_types".to_string(),
            desc.to_string(),
            None,
            None,
        );
        let output = format!("{diag}");
        assert_eq!(
            output,
            "error: src/foo.erl:42:10 [eqWAlizer: incompatible_types]\n  `'error'`.\n  Expression has type: 'error'\n  Context expected type: 'ok'\n"
        );
    }

    // -- Daemon lifecycle integration tests --

    fn watchman_available() -> bool {
        process::Command::new("watchman")
            .arg("version")
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false)
    }

    #[test]
    fn daemon_stop_command() {
        if !watchman_available() {
            eprintln!("Skipping: watchman not available");
            return;
        }

        let project = PathBuf::from(crate::test_utils::project_path("standard"));
        let profile = format!("test-daemon-stop-{}", process::id());

        let query_config = BuckQueryConfig::BuildGeneratedCode;

        let profile_bg = profile.clone();
        let project_bg = project.clone();
        let handle = thread::spawn(move || {
            let mut cli = elp::cli::Fake::default();
            run_daemon_server(
                &DaemonRun {
                    project: project_bg,
                    profile: profile_bg,
                    rebar: false,
                    daemonize: false,
                },
                &mut cli,
                &query_config,
                false,
            )
        });

        // Wait for daemon to become ready using the status command
        let start = Instant::now();
        loop {
            // Fail fast if daemon thread exited (e.g., watchman failure in CI)
            if handle.is_finished() {
                let result = handle.join().expect("Daemon thread panicked");
                match result {
                    Err(e) => {
                        eprintln!("Skipping: daemon failed to start: {e}");
                        return;
                    }
                    Ok(()) => panic!("Daemon exited before becoming ready"),
                }
            }

            let mut cli = elp::cli::Fake::default();
            if show_status(&project, &profile, false, &mut cli).is_ok() {
                let (stdout, _) = cli.to_strings();
                if stdout.contains("Daemon running") {
                    break;
                }
            }
            assert!(
                start.elapsed() < Duration::from_secs(120),
                "Daemon did not become ready"
            );
            thread::sleep(Duration::from_millis(200));
        }

        // Stop the daemon using the stop command
        let mut cli = elp::cli::Fake::default();
        stop_all_daemons(&mut cli).unwrap();
        let (stdout, _) = cli.to_strings();
        assert!(
            stdout.contains("Stopped"),
            "Expected stop confirmation: {stdout}"
        );

        // Daemon thread should exit cleanly
        let result = handle.join().expect("Daemon thread panicked");
        assert!(result.is_ok(), "Daemon returned error: {result:?}");
    }
}
