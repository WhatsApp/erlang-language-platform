/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::env;
use std::ffi::OsString;
use std::fmt;
use std::fs;
use std::io::Write;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;
use std::os::unix::prelude::PermissionsExt;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
use std::time::Instant;

use anyhow::Context;
use anyhow::Result;
use ast::Error;
use ast::Pos;
use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_types_db::eqwalizer::types::Type;
pub use elp_types_db::eqwalizer::EqwalizerDiagnostic;
use fxhash::FxHashMap;
use lazy_static::lazy_static;
use parking_lot::Mutex;
use tempfile::Builder;
use tempfile::TempPath;

pub mod ipc;
use ipc::IpcHandle;
use ipc::MsgFromEqWAlizer;
use ipc::MsgToEqWAlizer;

use crate::ipc::EqWAlizerASTFormat;

pub mod analyses;
pub mod ast;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Mode {
    Cli,
    Server,
    Shell,
}
impl Mode {
    fn to_env_var(&self) -> &str {
        match self {
            Mode::Cli => "elp_cli",
            Mode::Server => "elp_ide",
            Mode::Shell => "shell",
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct EqwalizerConfig {
    pub gradual_typing: Option<bool>,
    pub check_redundant_guards: Option<bool>,
    pub fault_tolerance: Option<bool>,
    pub occurrence_typing: Option<bool>,
    pub clause_coverage: Option<bool>,
}
impl EqwalizerConfig {
    fn set_cmd_env(&self, cmd: &mut CommandProxy<'_>) {
        self.gradual_typing
            .map(|cfg| cmd.env("EQWALIZER_GRADUAL_TYPING", cfg.to_string()));
        self.check_redundant_guards
            .map(|cfg| cmd.env("EQWALIZER_CHECK_REDUNDANT_GUARDS", cfg.to_string()));
        self.fault_tolerance
            .map(|cfg| cmd.env("EQWALIZER_TOLERATE_ERRORS", cfg.to_string()));
        self.occurrence_typing
            .map(|cfg| cmd.env("EQWALIZER_EQWATER", cfg.to_string()));
        self.clause_coverage
            .map(|cfg| cmd.env("EQWALIZER_CLAUSE_COVERAGE", cfg.to_string()));
    }

    pub fn default_test() -> EqwalizerConfig {
        EqwalizerConfig {
            gradual_typing: Some(false),
            check_redundant_guards: Some(false),
            fault_tolerance: Some(false),
            occurrence_typing: Some(true),
            clause_coverage: Some(false),
        }
    }
}

// Bundle file with command to make sure it's not removed too early
#[derive(Clone)]
pub struct Eqwalizer {
    cmd: OsString,
    args: Vec<OsString>,
    pub mode: Mode,
    // Used only for the Drop implementation
    _file: Option<Arc<TempPath>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EqwalizerDiagnostics {
    Diagnostics {
        errors: FxHashMap<String, Vec<EqwalizerDiagnostic>>,
        type_info: FxHashMap<String, Vec<(Pos, Type)>>,
    },
    NoAst {
        module: String,
    },
    Error(String),
}

impl Default for EqwalizerDiagnostics {
    fn default() -> Self {
        EqwalizerDiagnostics::Diagnostics {
            errors: Default::default(),
            type_info: Default::default(),
        }
    }
}

impl EqwalizerDiagnostics {
    pub fn combine(mut self, other: Self) -> Self {
        match &mut self {
            EqwalizerDiagnostics::NoAst {
                module: self_module,
            } => match &other {
                EqwalizerDiagnostics::NoAst {
                    module: other_module,
                } => {
                    if other_module > self_module {
                        self
                    } else {
                        other
                    }
                }
                _ => self,
            },
            EqwalizerDiagnostics::Error(_) => self,
            EqwalizerDiagnostics::Diagnostics { errors, type_info } => match other {
                EqwalizerDiagnostics::Diagnostics {
                    errors: other_errors,
                    type_info: other_type_info,
                } => {
                    errors.extend(other_errors.into_iter());
                    type_info.extend(other_type_info.into_iter());
                    self
                }
                EqwalizerDiagnostics::Error(_) => other.clone(),
                EqwalizerDiagnostics::NoAst { .. } => other.clone(),
            },
        }
    }
}

pub trait DbApi {
    fn eqwalizing_start(&self, module: String);
    fn eqwalizing_done(&self, module: String);
    fn set_module_ipc_handle(&self, module: ModuleName, handle: Option<Arc<Mutex<IpcHandle>>>);
    fn module_ipc_handle(&self, module: ModuleName) -> Option<Arc<Mutex<IpcHandle>>>;
}

#[salsa::query_group(EqwalizerDiagnosticsDatabaseStorage)]
pub trait EqwalizerDiagnosticsDatabase: ast::db::EqwalizerASTDatabase + DbApi {
    #[salsa::input]
    fn eqwalizer_config(&self) -> Arc<EqwalizerConfig>;

    fn module_diagnostics(
        &self,
        project_id: ProjectId,
        module: String,
    ) -> (Arc<EqwalizerDiagnostics>, Instant);
}

impl Default for Eqwalizer {
    fn default() -> Self {
        EQWALIZER.to_owned()
    }
}

lazy_static! {
    // We make a static version of the eqwalizer executable environment to
    // - Prevent race conditions in tests from the temporary file creation
    //   process (T182801661)
    // - Speed up tests, since we create a RootDatabase once per test
    //   needing the erlang service
    static ref EQWALIZER: Eqwalizer = Eqwalizer::ensure_exe();
}

impl Eqwalizer {
    // Identify the required Eqwalizer executable, and ensure it is
    // available on the file system
    fn ensure_exe() -> Self {
        let env = env::var("ELP_EQWALIZER_PATH");
        let (path, ext, temp_file) = if let Ok(path) = env {
            let path = PathBuf::from(path);
            let ext = path
                .extension()
                .unwrap_or_default()
                .to_str()
                .unwrap()
                .to_string();
            (path, ext, None)
        } else {
            let extension = env!("ELP_EQWALIZER_EXT").to_string();
            let eqwalizer_src = include_bytes!(concat!(env!("OUT_DIR"), "/eqwalizer"));
            let mut temp_file = Builder::new()
                .prefix("eqwalizer")
                .tempfile()
                .expect("can't create eqwalizer temp executable");
            temp_file
                .write_all(eqwalizer_src)
                .expect("can't create eqwalizer temp executable");

            let temp_file = temp_file.into_temp_path();

            let mut perm = fs::metadata(&temp_file)
                .expect("can't create eqwalizer temp executable")
                .permissions();
            perm.set_mode(0o755);
            fs::set_permissions(&temp_file, perm).expect("can't create eqwalizer temp executable");

            (temp_file.to_path_buf(), extension, Some(temp_file))
        };

        let (cmd, args) = match ext.as_str() {
            "jar" => (
                "java".into(),
                vec!["-Xss20M".into(), "-jar".into(), path.into()],
            ),
            "" => (path.into(), vec![]),
            _ => panic!("Unknown eqwalizer executable {:?}", path),
        };

        Self {
            cmd,
            args,
            mode: Mode::Server,
            _file: temp_file.map(Arc::new),
        }
    }

    // Return a smart pointer to bundle lifetime with the temp file's lifetime
    pub fn cmd(&self) -> CommandProxy<'_> {
        let mut cmd = Command::new(&self.cmd);
        cmd.args(&self.args);
        CommandProxy::new(cmd)
    }

    pub fn typecheck(
        &self,
        build_info_path: &Path,
        db: &dyn EqwalizerDiagnosticsDatabase,
        project_id: ProjectId,
        modules: Vec<&str>,
    ) -> EqwalizerDiagnostics {
        let mut cmd = self.cmd();
        db.eqwalizer_config().set_cmd_env(&mut cmd);
        cmd.arg("ipc");
        cmd.args(modules);
        cmd.env("EQWALIZER_MODE", self.mode.to_env_var());
        add_env(&mut cmd, build_info_path, None);

        match do_typecheck(cmd, db, project_id) {
            Ok(diags) => diags,
            Err(err) => EqwalizerDiagnostics::Error(format!("{}", err)),
        }
    }
}

fn do_typecheck(
    mut cmd: CommandProxy,
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
) -> Result<EqwalizerDiagnostics, anyhow::Error> {
    // Never cache the results of this function
    db.salsa_runtime().report_untracked_read();
    let handle = Arc::new(Mutex::new(
        IpcHandle::from_command(&mut cmd)
            .with_context(|| format!("starting eqWAlizer process: {:?}", cmd))?,
    ));
    let mut diagnostics = EqwalizerDiagnostics::default();
    loop {
        db.unwind_if_cancelled();
        let msg = handle.lock().receive()?;
        match msg {
            MsgFromEqWAlizer::EnteringModule { module } => {
                let module_name = ModuleName::new(&module);
                db.set_module_ipc_handle(module_name.clone(), Some(handle.clone()));
                let diags = db.module_diagnostics(project_id, module).0;
                db.set_module_ipc_handle(module_name, None);
                diagnostics = diagnostics.combine((*diags).clone());
                match diagnostics {
                    EqwalizerDiagnostics::Error(_) | EqwalizerDiagnostics::NoAst { .. } => {
                        return Ok(diagnostics);
                    }
                    EqwalizerDiagnostics::Diagnostics { .. } => (),
                }
                handle.lock().send(&MsgToEqWAlizer::ELPExitingModule)?;
            }
            MsgFromEqWAlizer::Done { .. } => {
                return Ok(diagnostics);
            }
            msg => {
                log::warn!(
                    "received unexpected message from eqwalizer, ignoring: {:?}",
                    msg
                )
            }
        }
    }
}

fn module_diagnostics(
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: String,
) -> (Arc<EqwalizerDiagnostics>, Instant) {
    // A timestamp is added to the return value to force Salsa to store new
    // diagnostics, and not attempt to back-date them if they are equal to
    // the memoized ones.
    let timestamp = Instant::now();
    // Dummy read eqWAlizer config for Salsa
    // Ideally, the config should be passed per module to eqWAlizer instead
    // of being set in the command's environment
    let _ = db.eqwalizer_config();
    match get_module_diagnostics(db, project_id, module.clone()) {
        Ok(diag) => (Arc::new(diag), timestamp),
        Err(err) => (
            Arc::new(EqwalizerDiagnostics::Error(format!(
                "eqWAlizing module {}:\n{}",
                module, err
            ))),
            timestamp,
        ),
    }
}

fn get_module_diagnostics(
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: String,
) -> Result<EqwalizerDiagnostics, anyhow::Error> {
    let handle_mutex = db
        .module_ipc_handle(ModuleName::new(&module))
        .ok_or(anyhow::Error::msg(format!(
            "no eqWAlizer handle for module {}",
            module
        )))?;
    let mut handle = handle_mutex.lock();
    handle.send(&MsgToEqWAlizer::ELPEnteringModule)?;
    loop {
        db.unwind_if_cancelled();
        match handle.receive()? {
            MsgFromEqWAlizer::GetAstBytes { module, format } => {
                log::debug!(
                    "received from eqwalizer: GetAstBytes for module {} (format = {:?})",
                    module,
                    format
                );
                let module_name = ModuleName::new(&module);
                let ast = {
                    match format {
                        EqWAlizerASTFormat::RawForms => {
                            db.get_erl_ast_bytes(project_id, module_name)
                        }
                        EqWAlizerASTFormat::ConvertedForms => {
                            db.converted_ast_bytes(project_id, module_name)
                        }
                        EqWAlizerASTFormat::RawStub => {
                            db.get_erl_stub_bytes(project_id, module_name)
                        }
                        EqWAlizerASTFormat::ConvertedStub => {
                            db.converted_stub_bytes(project_id, module_name)
                        }
                        EqWAlizerASTFormat::ExpandedStub => {
                            db.expanded_stub_bytes(project_id, module_name)
                        }
                        EqWAlizerASTFormat::ContractiveStub => {
                            db.contractive_stub_bytes(project_id, module_name)
                        }
                        EqWAlizerASTFormat::CovariantStub => {
                            db.covariant_stub_bytes(project_id, module_name)
                        }
                        EqWAlizerASTFormat::TransitiveStub => {
                            db.transitive_stub_bytes(project_id, module_name)
                        }
                    }
                };
                match ast {
                    Ok(ast_bytes) => {
                        log::debug!(
                            "sending to eqwalizer: GetAstBytesReply for module {}",
                            module
                        );
                        let ast_bytes_len = ast_bytes.len().try_into()?;
                        let reply = &MsgToEqWAlizer::GetAstBytesReply { ast_bytes_len };
                        handle.send(reply)?;
                        handle.receive_newline()?;
                        handle.send_bytes(&ast_bytes).with_context(|| {
                            format!(
                                "sending to eqwalizer: bytes for module {} (format = {:?})",
                                module, format
                            )
                        })?;
                    }
                    Err(Error::ModuleNotFound(_)) => {
                        log::debug!(
                            "module not found, sending to eqwalizer: empty GetAstBytesReply for module {}",
                            module
                        );
                        let ast_bytes_len = 0;
                        let reply = &MsgToEqWAlizer::GetAstBytesReply { ast_bytes_len };
                        handle.send(reply)?;
                        handle.receive_newline()?;
                    }
                    Err(Error::ParseError) => {
                        log::debug!(
                            "parse error, sending to eqwalizer: CannotCompleteRequest for module {}",
                            module
                        );
                        let reply = &MsgToEqWAlizer::CannotCompleteRequest;
                        handle.send(reply)?;
                        return Ok(EqwalizerDiagnostics::NoAst { module });
                    }
                    Err(err) => {
                        log::debug!(
                            "error {} sending to eqwalizer: CannotCompleteRequest for module {}",
                            err,
                            module
                        );
                        let reply = &MsgToEqWAlizer::CannotCompleteRequest;
                        handle.send(reply)?;
                        return Ok(EqwalizerDiagnostics::Error(err.to_string()));
                    }
                }
            }
            MsgFromEqWAlizer::EqwalizingStart { module } => db.eqwalizing_start(module),
            MsgFromEqWAlizer::EqwalizingDone { module } => db.eqwalizing_done(module),
            MsgFromEqWAlizer::Done {
                diagnostics,
                type_info,
            } => {
                log::debug!(
                    "received from eqwalizer: Done with diagnostics length {}",
                    diagnostics.len()
                );
                return Ok(EqwalizerDiagnostics::Diagnostics {
                    errors: diagnostics,
                    type_info,
                });
            }
            MsgFromEqWAlizer::Dependencies { modules } => {
                modules.iter().for_each(|module| {
                    let module = ModuleName::new(module);
                    _ = db.transitive_stub_bytes(project_id, module);
                });
            }
            msg => {
                log::warn!(
                    "received unexpected message from eqwalizer, ignoring: {:?}",
                    msg
                )
            }
        }
    }
}

fn add_env(cmd: &mut Command, build_info_path: &Path, elp_ast_dir: Option<&Path>) {
    cmd.env("EQWALIZER_BUILD_INFO", build_info_path);
    if let Some(elp_ast_dir) = elp_ast_dir {
        cmd.env("EQWALIZER_ELP_AST_DIR", elp_ast_dir);
    }
}

/// This ensures the enclosed Command struct won't outlive the related temp file
pub struct CommandProxy<'file>(Command, PhantomData<&'file TempPath>);

impl<'file> CommandProxy<'file> {
    pub fn new(cmd: Command) -> Self {
        Self(cmd, PhantomData)
    }
}

impl<'file> Deref for CommandProxy<'file> {
    type Target = Command;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'file> DerefMut for CommandProxy<'file> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl fmt::Debug for CommandProxy<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}
