/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::process::Child;
use std::process::ChildStdin;
use std::process::ChildStdout;
use std::process::Command;
use std::process::Stdio;
use std::sync::Arc;

use anyhow::Result;
use crossbeam_channel::bounded;
use crossbeam_channel::unbounded;
use crossbeam_channel::Receiver;
use crossbeam_channel::Sender;
use jod_thread::JoinHandle;
use stdx::JodChild;
use tempfile::TempPath;

// @fb-only: mod meta_only;

#[derive(Debug)]
struct SharedState {
    _thread_for_drop: JoinHandle,
    _child_for_drop: JodChild,
    pub(crate) _file_for_drop: Option<TempPath>,
}

#[derive(Clone, Debug)]
pub struct Connection {
    sender: Sender<Request>,
    pub(crate) _for_drop: Arc<SharedState>,
}

#[derive(Debug, Clone)]
enum Request {
    Complete(String, Sender<Option<String>>),
}

pub struct AiCompletion {
    connection: Option<Connection>,
}

pub type CompletionReceiver = Receiver<Option<String>>;

impl AiCompletion {
    pub fn disabled() -> AiCompletion {
        AiCompletion { connection: None }
    }

    pub fn complete(&mut self, code: String) -> CompletionReceiver {
        if let Some(connection) = &self.connection {
            let (sender, receiver) = bounded(0);
            let msg = Request::Complete(code, sender);
            match connection.sender.try_send(msg) {
                Ok(()) => {
                    return receiver;
                }
                Err(_err) => {
                    // failed to send - connection was dropped, disable ai completions
                    log::warn!("disabling ai completions");
                    self.connection = None
                }
            }
        }

        always(None)
    }
}

pub fn always<T>(value: T) -> Receiver<T> {
    let (sender, receiver) = unbounded();
    sender.send(value).unwrap();
    receiver
}

impl Connection {
    pub fn spawn(path: &Path) -> Result<Connection> {
        let mut cmd = Command::new(path);
        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());

        let mut proc = cmd.spawn()?;

        let (sender, thread) = stdio_transport(&mut proc);

        Ok(Connection {
            sender,
            _for_drop: Arc::new(SharedState {
                _file_for_drop: None,
                _child_for_drop: JodChild(proc),
                _thread_for_drop: thread,
            }),
        })
    }
}

fn stdio_transport(proc: &mut Child) -> (Sender<Request>, JoinHandle) {
    let instream = BufWriter::new(proc.stdin.take().unwrap());
    let mut outstream = BufReader::new(proc.stdout.take().unwrap());

    let (sender, receiver) = bounded::<Request>(0);
    let handle = jod_thread::spawn(
        move || match thread_run(receiver, instream, &mut outstream) {
            Result::Ok(()) => {}
            Err(err) => {
                let mut buf = vec![0; 512];
                let _ = outstream.read(&mut buf);
                let remaining = String::from_utf8_lossy(&buf);
                log::error!(
                    "thread failed with {}\nremaining data:\n\n{}",
                    err,
                    remaining
                );
            }
        },
    );

    (sender, handle)
}

fn thread_run(
    receiver: Receiver<Request>,
    mut instream: BufWriter<ChildStdin>,
    outstream: &mut BufReader<ChildStdout>,
) -> Result<()> {
    let mut line_buf = String::new();

    while let Ok(request) = receiver.recv() {
        match request {
            Request::Complete(string, sender) => {
                writeln!(instream, "COMPLETE {}", string.len())?;
                instream.write_all(string.as_bytes())?;
                instream.flush()?;

                line_buf.clear();
                outstream.read_line(&mut line_buf)?;
                let parts = line_buf.split_ascii_whitespace().collect::<Vec<_>>();
                if parts.is_empty() {
                    break;
                }

                match parts[0] {
                    "OK" => {
                        let value = parts.get(1).map(ToString::to_string).unwrap_or_default();
                        log::debug!("received result {}", value);
                        let _ = sender.send(Some(value));
                    }
                    "NO_RESULT" => {
                        log::debug!("received no result");
                        let _ = sender.send(None);
                    }
                    "ERROR" => {
                        log::error!("AI server error: {}", line_buf);
                        let _ = sender.send(None);
                        break;
                    }
                    _ => {
                        log::error!("Unrecognised message: {}", line_buf);
                        let _ = sender.send(None);
                        break;
                    }
                }
            }
        }
    }

    Ok(())
}
