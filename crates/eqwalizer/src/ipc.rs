/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fs;
use std::io::BufRead;
use std::io::BufReader;
use std::io::ErrorKind;
use std::io::Write;
use std::process::Child;
use std::process::ChildStdin;
use std::process::ChildStdout;
use std::process::Command;
use std::process::Stdio;
use std::thread;
use std::time::Duration;

use anyhow::Context;
use anyhow::Result;
use anyhow::bail;
use elp_base_db::limit_logged_string;
use elp_types_db::StringId;
use elp_types_db::eqwalizer::EqwalizerDiagnostic;
use elp_types_db::eqwalizer::Id;
use elp_types_db::eqwalizer::ext_types::ExtType;
use elp_types_db::eqwalizer::form;
use elp_types_db::eqwalizer::types::Type;
use fxhash::FxHashMap;
use serde::Deserialize;
use serde::Serialize;
use stdx::JodChild;
#[cfg(unix)]
use timeout_readwrite::TimeoutReader;
#[cfg(unix)]
use timeout_readwrite::TimeoutWriter;

#[cfg(windows)]
mod win_timeout {
    use std::io::Read;
    use std::io::Result;
    use std::io::Write;
    use std::time::Duration;

    pub struct TimeoutReader<R>(R, Duration);
    pub struct TimeoutWriter<W>(W, Duration);

    impl<R> TimeoutReader<R> {
        pub fn new(inner: R, timeout: Duration) -> Self {
            TimeoutReader(inner, timeout)
        }
    }
    impl<W> TimeoutWriter<W> {
        pub fn new(inner: W, timeout: Duration) -> Self {
            TimeoutWriter(inner, timeout)
        }
    }

    impl<R: Read> Read for TimeoutReader<R> {
        fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
            self.0.read(buf)
        }
    }

    impl<W: Write> Write for TimeoutWriter<W> {
        fn write(&mut self, buf: &[u8]) -> Result<usize> {
            self.0.write(buf)
        }
        fn flush(&mut self) -> Result<()> {
            self.0.flush()
        }
    }
}

#[cfg(windows)]
use win_timeout::TimeoutReader;
#[cfg(windows)]
use win_timeout::TimeoutWriter;

use crate::ast::Pos;
use crate::db::EqwalizerDiagnosticsDatabase;

#[derive(Deserialize, Debug)]
pub enum EqWAlizerASTFormat {
    ConvertedForms,
    TransitiveStub,
}

#[derive(Deserialize, Debug)]
pub enum MsgFromEqWAlizer {
    EnteringModule {
        module: String,
    },
    GetAstBytes {
        module: String,
        format: EqWAlizerASTFormat,
    },
    ValidateType {
        ty: ExtType,
    },
    EqwalizingStart {
        module: String,
    },
    EqwalizingDone {
        module: String,
    },
    Dependencies {
        modules: Vec<String>,
    },
    Done {
        #[serde(default)]
        diagnostics: FxHashMap<String, Vec<EqwalizerDiagnostic>>,
        #[serde(default)]
        type_info: FxHashMap<String, Vec<(Pos, Type)>>,
    },
    GetTypeDecl {
        module: String,
        id: Id,
    },
    GetRecDecl {
        module: String,
        id: StringId,
    },
    /// Request for a native record declaration by module and name.
    GetNativeRecDecl {
        module: String,
        id: StringId,
    },
    GetFunSpec {
        module: String,
        id: Id,
    },
    GetOverloadedFunSpec {
        module: String,
        id: Id,
    },
    GetCallbacks {
        module: String,
    },
    FunCheckDone {
        results: Vec<FunCheckResult>,
    },
}

#[derive(Serialize, Debug)]
pub enum MsgToEqWAlizer {
    ELPEnteringModule,
    ELPExitingModule,
    GetAstBytesReply { len: u32 },
    ValidatedType { len: u32 },
    InvalidType { len: u32 },
    CannotCompleteRequest,
    GetTypeDeclReply { len: u32 },
    GetRecDeclReply { len: u32 },
    GetNativeRecDeclReply { len: u32 },
    GetFunSpecReply { len: u32 },
    GetOverloadedFunSpecReply { len: u32 },
    GetCallbacksReply { len: u32 },
    FunsToCheckReply { len: u32 },
}

#[derive(Serialize, Debug, Clone)]
pub struct FunToCheck {
    pub module: String,
    pub id: Id,
    pub fun_decl: form::FunDecl,
}

#[derive(Deserialize, Debug, Clone)]
pub struct FunCheckResult {
    pub module: String,
    pub id: Id,
    pub errors: Vec<EqwalizerDiagnostic>,
}

pub struct IpcHandle {
    writer: TimeoutWriter<ChildStdin>,
    reader: BufReader<TimeoutReader<ChildStdout>>,
    _child_for_drop: JodChild,
}

/// How often a blocking receive or send wakes to check for salsa cancellation
/// (mirrors `erlang_service::request_reply_handle`).
const RECV_POLL_TIMEOUT: Duration = Duration::from_millis(100);
/// Receive/send ceiling for contexts that never cancel (e.g. CLI), preserving
/// the historical ~4 min timeout. 4 min / 100 ms = 2400 ticks.
const MAX_RECV_POLLS: u32 = 2400;

/// Windows caveat: `TimeoutReader`/`TimeoutWriter` are no-op pass-throughs
/// there (see `win_timeout`), so reads and writes never yield a poll tick —
/// Windows gets no cancellation relief, unchanged from before this commit.

impl IpcHandle {
    fn spawn_cmd(cmd: &mut Command) -> Result<Child> {
        // Spawn can fail due to a race condition with the creation/closing of the
        // eqWAlizer executable, so we retry until the file is properly closed.
        // See https://github.com/rust-lang/rust/issues/114554
        loop {
            match cmd.spawn() {
                Ok(c) => return Ok(c),
                Err(err) => {
                    if err.kind() == ErrorKind::ExecutableFileBusy {
                        thread::sleep(Duration::from_millis(10));
                    } else {
                        // Provide extra debugging detail, to track down T198872667
                        let command_str = cmd.get_program();
                        let attr = fs::metadata(command_str);
                        let error_str = format!(
                            "err: {}, command_str: {:?}, cmd: {:?}, meta_data: {:?}",
                            err, command_str, cmd, &attr
                        );
                        // Show up in error log
                        log::error!("{}", limit_logged_string(&error_str));
                        // And show up as an eqwalizer error
                        bail!(error_str)
                    }
                }
            }
        }
    }

    pub fn from_command(cmd: &mut Command) -> Result<Self> {
        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            // for debugging purposes
            .stderr(Stdio::inherit());

        let mut child = Self::spawn_cmd(cmd)?;
        let stdin = child
            .stdin
            .take()
            .context("failed to get stdin for eqwalizer process")?;
        let stdout = child
            .stdout
            .take()
            .context("failed to get stdout for eqwalizer process")?;

        let _child_for_drop = JodChild(child);
        let writer = TimeoutWriter::new(stdin, RECV_POLL_TIMEOUT);
        let reader = BufReader::new(TimeoutReader::new(stdout, RECV_POLL_TIMEOUT));

        Ok(Self {
            writer,
            reader,
            _child_for_drop,
        })
    }

    /// Receive a message, waking every `RECV_POLL_TIMEOUT` to check `db` for
    /// salsa cancellation, which panics with `Cancelled` if the query was
    /// cancelled (dropping this handle, killing the subprocess and releasing
    /// the snapshot).
    pub fn receive(&mut self, db: &dyn EqwalizerDiagnosticsDatabase) -> Result<MsgFromEqWAlizer> {
        let buf = self.receive_line(db).context("receiving message")?;
        let deserialized = serde_json::from_str(&buf)
            .with_context(|| format!("parsing for eqwalizer: {buf:?}"))?;
        Ok(deserialized)
    }

    pub fn receive_newline(&mut self, db: &dyn EqwalizerDiagnosticsDatabase) -> Result<()> {
        let _ = self.receive_line(db).context("receiving newline")?;
        Ok(())
    }

    pub fn send(
        &mut self,
        msg: &MsgToEqWAlizer,
        db: &dyn EqwalizerDiagnosticsDatabase,
    ) -> Result<()> {
        let mut msg = serde_json::to_string(msg).expect("failed to serialize msg to eqwalizer");
        msg.push('\n');
        self.write_all_cancellable(msg.as_bytes(), db)
            .with_context(|| format!("writing message: {msg:?}"))
    }

    pub fn send_bytes(&mut self, msg: &[u8], db: &dyn EqwalizerDiagnosticsDatabase) -> Result<()> {
        self.write_all_cancellable(msg, db)
            .with_context(|| format!("writing bytes of size {}", msg.len()))
    }

    /// Write all of `bytes`, waking every `RECV_POLL_TIMEOUT` to check `db` for
    /// salsa cancellation, which panics with `Cancelled` if the query was
    /// cancelled. `TimeoutWriter` yields a poll tick
    /// (`ErrorKind::TimedOut`/`WouldBlock`) when no buffer space is available,
    /// and `write` (unlike `write_all`) reports partial progress so we can
    /// resume from the right offset after a wake.
    fn write_all_cancellable(
        &mut self,
        mut bytes: &[u8],
        db: &dyn EqwalizerDiagnosticsDatabase,
    ) -> Result<()> {
        let mut polls_remaining = MAX_RECV_POLLS;
        while !bytes.is_empty() {
            db.unwind_if_revision_cancelled();
            match self.writer.write(bytes) {
                Ok(0) => bail!("eqwalizer stdin closed before all bytes were written"),
                Ok(n) => {
                    bytes = &bytes[n..];
                    polls_remaining = MAX_RECV_POLLS;
                }
                Err(err) if err.kind() == ErrorKind::Interrupted => {}
                // No buffer space yet; wake to re-check cancellation, then retry.
                Err(err)
                    if err.kind() == ErrorKind::TimedOut || err.kind() == ErrorKind::WouldBlock =>
                {
                    polls_remaining -= 1;
                    if polls_remaining == 0 {
                        bail!(
                            "timed out writing to eqwalizer (after {}s)",
                            (MAX_RECV_POLLS as u128 * RECV_POLL_TIMEOUT.as_millis()) / 1000
                        );
                    }
                }
                Err(err) => return Err(err).context("failed write to eqwalizer stdin"),
            }
        }
        Ok(())
    }

    fn receive_line(&mut self, db: &dyn EqwalizerDiagnosticsDatabase) -> Result<String> {
        let mut buf = String::new();
        let mut polls_remaining = MAX_RECV_POLLS;
        loop {
            db.unwind_if_revision_cancelled();
            match self.reader.read_line(&mut buf) {
                Ok(0) => bail!("eqwalizer stdout closed before a newline (EOF)"),
                Ok(_) => return Ok(buf),
                // Timed out with no data; wake to re-check cancellation, then retry.
                Err(err)
                    if err.kind() == ErrorKind::TimedOut || err.kind() == ErrorKind::WouldBlock => {
                }
                Err(err) => return Err(err).context("failed read_line from eqwalizer stdout"),
            }
            polls_remaining -= 1;
            if polls_remaining == 0 {
                bail!(
                    "timed out waiting for a message from eqwalizer (after {}s)",
                    (MAX_RECV_POLLS as u128 * RECV_POLL_TIMEOUT.as_millis()) / 1000
                );
            }
        }
    }
}
