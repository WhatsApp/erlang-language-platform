/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::env;
use std::ffi::OsString;
use std::fs;
use std::io::Write;
use std::ops::ControlFlow;
#[cfg(unix)]
use std::os::unix::prelude::PermissionsExt;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
use std::sync::LazyLock;

use anyhow::Context;
use anyhow::Result;
use ast::Error;
use ast::Pos;
use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_base_db::limit_logged_string;
pub use elp_types_db::eqwalizer::EqwalizerDiagnostic;
use elp_types_db::eqwalizer::types::Type;
use fxhash::FxHashMap;
use parking_lot::Mutex;
use tempfile::Builder;
use tempfile::TempPath;

pub mod db;

pub mod ipc;
use ipc::IpcHandle;
use ipc::MsgFromEqWAlizer;
use ipc::MsgToEqWAlizer;

use crate::ast::expand::CastExpander;
use crate::ast::trans_valid::TransitiveChecker;
use crate::db::EqwalizerDiagnosticsDatabase;
use crate::ipc::EqWAlizerASTFormat;

pub mod analyses;
pub mod ast;
pub use elp_types_db::eqwalizer::types;
pub use ipc::FunCheckResult;
pub use ipc::FunToCheck;

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

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct EqwalizerConfig {
    pub overloaded_spec_dynamic_result: Option<bool>,
    pub report_dynamic_lambdas: Option<bool>,
}
impl EqwalizerConfig {
    fn set_cmd_env(&self, cmd: &mut Command) {
        self.overloaded_spec_dynamic_result
            .map(|cfg| cmd.env("EQWALIZER_OVERLOADED_SPEC_DYNAMIC_RESULT", cfg.to_string()));
        self.report_dynamic_lambdas
            .map(|cfg| cmd.env("EQWALIZER_REPORT_DYNAMIC_LAMBDAS", cfg.to_string()));
    }

    pub fn default_test() -> EqwalizerConfig {
        EqwalizerConfig {
            overloaded_spec_dynamic_result: Some(false),
            report_dynamic_lambdas: Some(false),
        }
    }
}

// Bundle file with command to make sure it's not removed too early
#[derive(Clone)]
pub struct Eqwalizer {
    pub mode: Mode,
}

#[derive(Clone)]
pub struct EqwalizerExe {
    cmd: PathBuf,
    args: Vec<OsString>,
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
                    errors.extend(other_errors);
                    type_info.extend(other_type_info);
                    self
                }
                EqwalizerDiagnostics::Error(_) => other.clone(),
                EqwalizerDiagnostics::NoAst { .. } => other.clone(),
            },
        }
    }
}

impl Default for Eqwalizer {
    fn default() -> Self {
        Self { mode: Mode::Server }
    }
}

// We make a static version of the eqwalizer executable environment to
// - Prevent race conditions in tests from the temporary file creation
//   process (T182801661)
// - Speed up tests, since we create a RootDatabase once per test
//   needing the erlang service
// We wrap it in an Arc to make sure it never goes out of scope,
// triggering the Drop handler, until the programme exits.
// It has a Mutex so it can be updated if the operating systen deletes the file
// for a long-running ELP server.
static EQWALIZER_EXE: LazyLock<Mutex<EqwalizerExe>> =
    LazyLock::new(|| Mutex::new(EqwalizerExe::ensure_exe()));

impl EqwalizerExe {
    // The eqwalizer native image is embedded into the binary at compile
    // time via `env!("ELP_EQWALIZER_PATH")` — Cargo sets this from
    // build.rs and Buck sets it via `env = {...}` in the BUCK file. The
    // `ELP_EQWALIZER_PATH` *runtime* env var override below stays as the
    // developer escape hatch (e.g. to point at a locally-built binary
    // without recompiling ELP).
    fn get_eqwalizer_path() -> (PathBuf, String, Option<TempPath>) {
        if let Ok(path) = env::var("ELP_EQWALIZER_PATH") {
            let path = PathBuf::from(path);
            let ext = path
                .extension()
                .unwrap_or_default()
                .to_str()
                .expect("valid UTF-8 path")
                .to_string();
            (path, ext, None)
        } else {
            let extension = env!("ELP_EQWALIZER_EXT").to_string();
            let eqwalizer_src = include_bytes!(env!("ELP_EQWALIZER_PATH"));
            let mut temp_file = Builder::new()
                .prefix("eqwalizer")
                .tempfile()
                .expect("eqwalizer temp executable should be writable");
            temp_file
                .write_all(eqwalizer_src)
                .expect("eqwalizer temp executable should be writable");

            let temp_file = temp_file.into_temp_path();

            let mut perm = fs::metadata(&temp_file)
                .expect("eqwalizer temp executable should be writable")
                .permissions();

            #[cfg(windows)]
            {
                perm.set_readonly(false);
            }
            #[cfg(unix)]
            {
                perm.set_mode(0o755);
            }

            fs::set_permissions(&temp_file, perm)
                .expect("eqwalizer temp executable should be writable");

            (temp_file.to_path_buf(), extension, Some(temp_file))
        }
    }

    // Identify the required Eqwalizer executable, and ensure it is
    // available on the file system
    fn ensure_exe() -> Self {
        let (path, ext, temp_file) = Self::get_eqwalizer_path();

        let (cmd, args) = match ext.as_str() {
            "jar" => (
                env!("ELP_EQWALIZER_JAVA").into(),
                vec!["-Xss40M".into(), "-jar".into(), path.into()],
            ),
            "" => (path, vec![]),
            _ => panic!("Unknown eqwalizer executable {path:?}"),
        };

        Self {
            cmd,
            args,
            _file: temp_file.map(Arc::new),
        }
    }

    pub fn cmd(&self) -> Command {
        let mut cmd = Command::new(&self.cmd);
        cmd.args(&self.args);
        cmd
    }
}

impl Eqwalizer {
    fn cmd(&self) -> Command {
        let mut exe = EQWALIZER_EXE.lock();
        // This check is for the temporary directory being removed by an external process.
        // As such, it is only applicable if ther *is* a temporary directory.
        if exe._file.is_some() && !exe.cmd.is_file() {
            log::error!("Eqwalizer exe has disappeared, recreating");
            // We have a problem with the eqwalizer exe file, recreate it
            *exe = EqwalizerExe::ensure_exe();
        }
        exe.cmd()
    }

    pub fn typecheck(
        &self,
        db: &dyn EqwalizerDiagnosticsDatabase,
        project_id: ProjectId,
        modules: Vec<&str>,
    ) -> EqwalizerDiagnostics {
        let mut cmd = self.cmd();
        db.eqwalizer_config().set_cmd_env(&mut cmd);
        cmd.arg("ipc");
        cmd.args(modules);
        cmd.env("EQWALIZER_MODE", self.mode.to_env_var());

        match do_typecheck(cmd, db, project_id) {
            Ok(diags) => diags,
            Err(err) => EqwalizerDiagnostics::Error(format!("{err:?}")),
        }
    }

    pub fn typecheck_functions(
        &self,
        db: &dyn EqwalizerDiagnosticsDatabase,
        project_id: ProjectId,
        funs: Vec<ipc::FunToCheck>,
    ) -> Result<Vec<ipc::FunCheckResult>> {
        let mut cmd = self.cmd();
        db.eqwalizer_config().set_cmd_env(&mut cmd);
        cmd.arg("ipc-check-funs");
        cmd.env("EQWALIZER_MODE", self.mode.to_env_var());

        do_typecheck_functions(cmd, db, project_id, funs)
    }
}

fn do_typecheck(
    mut cmd: Command,
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
) -> Result<EqwalizerDiagnostics, anyhow::Error> {
    // Never cache the results of this function
    db.salsa_runtime().report_untracked_read();
    let handle = Arc::new(Mutex::new(
        IpcHandle::from_command(&mut cmd)
            .with_context(|| format!("starting eqWAlizer process: {cmd:?}"))?,
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
                    "received unexpected message from eqwalizer, ignoring: {}",
                    limit_logged_string(&format!("{msg:?}"))
                )
            }
        }
    }
}

fn do_typecheck_functions(
    mut cmd: Command,
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    funs: Vec<ipc::FunToCheck>,
) -> Result<Vec<ipc::FunCheckResult>> {
    let mut handle = IpcHandle::from_command(&mut cmd)
        .with_context(|| format!("starting eqWAlizer process for ipc-check-funs: {cmd:?}"))?;

    // Send the functions to check
    let funs_bytes =
        serde_json::to_vec(&funs).context("serializing FunToCheck list for eqwalizer")?;
    let len: u32 = funs_bytes.len().try_into()?;
    handle.send(&MsgToEqWAlizer::FunsToCheckReply { len })?;
    handle.receive_newline()?;
    handle
        .send_bytes(&funs_bytes)
        .context("sending FunToCheck bytes to eqwalizer")?;

    // Handle dependency requests until we get FunCheckDone
    match serve_eqwalizer(&mut handle, db, project_id, |msg, _handle| match msg {
        MsgFromEqWAlizer::FunCheckDone { results } => Ok(ControlFlow::Break(results)),
        msg => {
            log::warn!(
                "ipc-check-funs: received unexpected message from eqwalizer, ignoring: {}",
                limit_logged_string(&format!("{msg:?}"))
            );
            Ok(ControlFlow::Continue(()))
        }
    })? {
        Completion::Done(results) => Ok(results),
        Completion::IpcError(diags) => {
            anyhow::bail!("eqwalizer IPC error: {diags:?}")
        }
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
            "no eqWAlizer handle for module {module}"
        )))?;
    let mut handle = handle_mutex.lock();
    handle.send(&MsgToEqWAlizer::ELPEnteringModule)?;
    match serve_eqwalizer(&mut handle, db, project_id, |msg, handle| match msg {
        MsgFromEqWAlizer::ValidateType { ty } => {
            log::debug!("received from eqwalizer: ValidateType");
            let pos = ty.pos().to_owned();
            let mut expander = CastExpander::new(db, project_id, module.clone().into());
            let expanded_ty = expander.expand(ty).map_err(Error::TypeConversionError)?;
            let mut trans_checker = TransitiveChecker::new(db, project_id, module.clone().into());
            let validated_ty = match expanded_ty {
                Ok(exp_ty) => trans_checker.check_type(pos, exp_ty),
                Err(invalid) => Err(invalid),
            };
            match validated_ty {
                Ok(valid_ty) => {
                    let type_bytes = serde_json::to_vec(&valid_ty)
                        .expect("validated type should be JSON serializable");
                    let len = type_bytes.len().try_into()?;
                    let reply = &MsgToEqWAlizer::ValidatedType { len };
                    handle.send(reply)?;
                    handle.receive_newline()?;
                    handle
                        .send_bytes(&type_bytes)
                        .with_context(|| "sending to eqwalizer: ValidatedType".to_string())?
                }
                Err(invalid) => {
                    let invalid_bytes = serde_json::to_vec(&invalid)
                        .expect("validated type should be JSON serializable");
                    let len = invalid_bytes.len().try_into()?;
                    let reply = &MsgToEqWAlizer::InvalidType { len };
                    handle.send(reply)?;
                    handle.receive_newline()?;
                    handle
                        .send_bytes(&invalid_bytes)
                        .with_context(|| "sending to eqwalizer: InvalidType".to_string())?
                }
            }
            Ok(ControlFlow::Continue(()))
        }
        MsgFromEqWAlizer::EqwalizingStart { module } => {
            db.eqwalizing_start(module);
            Ok(ControlFlow::Continue(()))
        }
        MsgFromEqWAlizer::EqwalizingDone { module } => {
            db.eqwalizing_done(module);
            Ok(ControlFlow::Continue(()))
        }
        MsgFromEqWAlizer::Done {
            diagnostics,
            type_info,
        } => {
            log::debug!(
                "received from eqwalizer: Done with diagnostics length {}",
                diagnostics.len()
            );
            Ok(ControlFlow::Break(EqwalizerDiagnostics::Diagnostics {
                errors: diagnostics,
                type_info,
            }))
        }
        msg => {
            log::warn!(
                "received unexpected message from eqwalizer, ignoring: {}",
                limit_logged_string(&format!("{msg:?}"))
            );
            Ok(ControlFlow::Continue(()))
        }
    })? {
        Completion::Done(diags) | Completion::IpcError(diags) => Ok(diags),
    }
}

enum Completion<R> {
    Done(R),
    IpcError(EqwalizerDiagnostics),
}

/// Shared IPC loop that handles dependency requests from eqwalizer.
/// Calls `on_message` for any message that is not a dependency request.
/// Returns when `on_message` returns `ControlFlow::Break(result)`.
fn serve_eqwalizer<R>(
    handle: &mut IpcHandle,
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    mut on_message: impl FnMut(MsgFromEqWAlizer, &mut IpcHandle) -> Result<ControlFlow<R>>,
) -> Result<Completion<R>> {
    loop {
        db.unwind_if_cancelled();
        let msg = handle.receive()?;
        match msg {
            MsgFromEqWAlizer::GetAstBytes { module, format } => {
                log::debug!(
                    "received from eqwalizer: GetAstBytes for module {module} (format = {format:?})"
                );
                let module_name = ModuleName::new(&module);
                let ast = match format {
                    EqWAlizerASTFormat::ConvertedForms => {
                        db.eqwalizer_ast_bytes(project_id, module_name)
                    }
                    EqWAlizerASTFormat::TransitiveStub => {
                        db.transitive_stub_bytes(project_id, module_name)
                    }
                };
                match ast {
                    Ok(ast_bytes) => {
                        log::debug!("sending to eqwalizer: GetAstBytesReply for module {module}");
                        let len = ast_bytes.len().try_into()?;
                        handle.send(&MsgToEqWAlizer::GetAstBytesReply { len })?;
                        handle.receive_newline()?;
                        handle.send_bytes(&ast_bytes).with_context(|| {
                            format!(
                                "sending to eqwalizer: bytes for module {module} (format = {format:?})"
                            )
                        })?;
                    }
                    Err(Error::ModuleNotFound(_)) => {
                        log::debug!(
                            "module not found, sending to eqwalizer: empty GetAstBytesReply for module {module}"
                        );
                        handle.send(&MsgToEqWAlizer::GetAstBytesReply { len: 0 })?;
                        handle.receive_newline()?;
                    }
                    Err(Error::ParseError) => {
                        log::debug!(
                            "parse error, sending to eqwalizer: CannotCompleteRequest for module {module}"
                        );
                        handle.send(&MsgToEqWAlizer::CannotCompleteRequest)?;
                        return Ok(Completion::IpcError(EqwalizerDiagnostics::NoAst { module }));
                    }
                    Err(err) => {
                        log::debug!(
                            "error {err} sending to eqwalizer: CannotCompleteRequest for module {module}"
                        );
                        handle.send(&MsgToEqWAlizer::CannotCompleteRequest)?;
                        return Ok(Completion::IpcError(EqwalizerDiagnostics::Error(
                            err.to_string(),
                        )));
                    }
                }
            }
            MsgFromEqWAlizer::GetTypeDecl { module, id } => {
                let result = db.type_decl_bytes(project_id, ModuleName::new(&module), id);
                if let Some(diag) = send_bytes(result, handle, module, |len| {
                    MsgToEqWAlizer::GetTypeDeclReply { len }
                })? {
                    return Ok(Completion::IpcError(diag));
                }
            }
            MsgFromEqWAlizer::GetRecDecl { module, id } => {
                let result = db.rec_decl_bytes(project_id, ModuleName::new(&module), id);
                if let Some(diag) = send_bytes(result, handle, module, |len| {
                    MsgToEqWAlizer::GetRecDeclReply { len }
                })? {
                    return Ok(Completion::IpcError(diag));
                }
            }
            MsgFromEqWAlizer::GetFunSpec { module, id } => {
                let result = db.fun_spec_bytes(project_id, ModuleName::new(&module), id);
                if let Some(diag) = send_bytes(result, handle, module, |len| {
                    MsgToEqWAlizer::GetFunSpecReply { len }
                })? {
                    return Ok(Completion::IpcError(diag));
                }
            }
            MsgFromEqWAlizer::GetOverloadedFunSpec { module, id } => {
                let result = db.overloaded_fun_spec_bytes(project_id, ModuleName::new(&module), id);
                if let Some(diag) = send_bytes(result, handle, module, |len| {
                    MsgToEqWAlizer::GetOverloadedFunSpecReply { len }
                })? {
                    return Ok(Completion::IpcError(diag));
                }
            }
            MsgFromEqWAlizer::GetCallbacks { module } => {
                let result = db.callbacks_bytes(project_id, ModuleName::new(&module));
                if let Some(diag) = send_bytes(result, handle, module, |len| {
                    MsgToEqWAlizer::GetCallbacksReply { len }
                })? {
                    return Ok(Completion::IpcError(diag));
                }
            }
            MsgFromEqWAlizer::Dependencies { modules } => {
                modules.iter().for_each(|module| {
                    let module = ModuleName::new(module);
                    _ = db.transitive_stub_bytes(project_id, module);
                });
            }
            msg => {
                if let ControlFlow::Break(result) = on_message(msg, handle)? {
                    return Ok(Completion::Done(result));
                }
            }
        }
    }
}

fn send_bytes(
    result: Result<Option<Arc<Vec<u8>>>, Error>,
    handle: &mut IpcHandle,
    module: String,
    reply: fn(u32) -> MsgToEqWAlizer,
) -> Result<Option<EqwalizerDiagnostics>> {
    match result {
        Ok(Some(ast_bytes)) => {
            let len = ast_bytes.len().try_into()?;
            let msg = reply(len);
            handle.send(&msg)?;
            handle.receive_newline()?;
            handle.send_bytes(&ast_bytes)?;
            Ok(None)
        }
        Ok(None) | Err(Error::ModuleNotFound(_)) => {
            let len = 0;
            let msg = reply(len);
            handle.send(&msg)?;
            handle.receive_newline()?;
            Ok(None)
        }
        Err(Error::ParseError) => {
            let msg = &MsgToEqWAlizer::CannotCompleteRequest;
            handle.send(msg)?;
            Ok(Some(EqwalizerDiagnostics::NoAst { module }))
        }
        Err(err) => {
            let msg = &MsgToEqWAlizer::CannotCompleteRequest;
            handle.send(msg)?;
            Ok(Some(EqwalizerDiagnostics::Error(err.to_string())))
        }
    }
}
