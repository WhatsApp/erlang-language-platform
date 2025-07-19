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
use std::io::BufWriter;
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
    use std::io::{Read, Write, Result};
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
use win_timeout::{TimeoutReader, TimeoutWriter};

use crate::ast::Pos;

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
    GetFunSpecReply { len: u32 },
    GetOverloadedFunSpecReply { len: u32 },
    GetCallbacksReply { len: u32 },
}

pub struct IpcHandle {
    writer: BufWriter<TimeoutWriter<ChildStdin>>,
    reader: BufReader<TimeoutReader<ChildStdout>>,
    _child_for_drop: JodChild,
}

const WRITE_TIMEOUT: Duration = Duration::from_secs(240);
const READ_TIMEOUT: Duration = Duration::from_secs(240);

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
        let writer = BufWriter::new(TimeoutWriter::new(stdin, WRITE_TIMEOUT));
        let reader = BufReader::new(TimeoutReader::new(stdout, READ_TIMEOUT));

        Ok(Self {
            writer,
            reader,
            _child_for_drop,
        })
    }

    pub fn receive(&mut self) -> Result<MsgFromEqWAlizer> {
        let buf = self.receive_line().context("receiving message")?;
        let deserialized = serde_json::from_str(&buf)
            .with_context(|| format!("parsing for eqwalizer: {buf:?}"))?;
        Ok(deserialized)
    }

    pub fn receive_newline(&mut self) -> Result<()> {
        let _ = self.receive_line().context("receiving newline")?;
        Ok(())
    }

    pub fn send(&mut self, msg: &MsgToEqWAlizer) -> Result<()> {
        let msg = serde_json::to_string(msg).expect("failed to serialize msg to eqwalizer");
        writeln!(self.writer, "{msg}").with_context(|| format!("writing message: {msg:?}"))?;
        self.writer
            .flush()
            .with_context(|| format!("flushing message: {msg:?}"))?;
        Ok(())
    }

    pub fn send_bytes(&mut self, msg: &[u8]) -> Result<()> {
        // Don't exceed pipe buffer size on Mac or Linux
        // https://unix.stackexchange.com/a/11954/147568
        let chunk_size = 65_536;
        for (idx, chunk) in msg.chunks(chunk_size).enumerate() {
            self.writer
                .write_all(chunk)
                .with_context(|| format!("writing bytes chunk {} of size {}", idx, chunk.len()))?;
        }
        self.writer
            .flush()
            .with_context(|| format!("flushing bytes of size {}", msg.len()))?;
        Ok(())
    }

    fn receive_line(&mut self) -> Result<String> {
        let mut buf = String::new();
        self.reader
            .read_line(&mut buf)
            .context("failed read_line from eqwalizer stdout")?;
        Ok(buf)
    }
}
