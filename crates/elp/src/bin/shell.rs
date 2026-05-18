/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::ops::ControlFlow;
use std::time::SystemTime;

use anyhow::Result;
use elp::build::load;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp::watchman::UpdateResult;
use elp::watchman::Watchman;
use elp_eqwalizer::Mode;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_log::telemetry;
use elp_project_model::DiscoverConfig;
use elp_project_model::buck::BuckQueryConfig;
use rustyline::error::ReadlineError;

use crate::args::Eqwalize;
use crate::args::EqwalizeAll;
use crate::args::EqwalizeApp;
use crate::args::EqwalizeTarget;
use crate::args::Shell;
use crate::eqwalizer_cli;

#[derive(Debug, Clone)]
pub(crate) enum ShellError {
    UnexpectedCommand(String),
    UnexpectedOption(String, String),
    UnexpectedArg(String, String),
    MissingArg(String),
}
impl fmt::Display for ShellError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ShellError::UnexpectedCommand(cmd) => write!(f, "Unexpected command {cmd}"),
            ShellError::UnexpectedOption(cmd, arg) => {
                write!(f, "Unexpected option {arg} for command {cmd}")
            }
            ShellError::UnexpectedArg(cmd, arg) => {
                write!(f, "Unexpected arg {arg} for command {cmd}")
            }
            ShellError::MissingArg(cmd) => write!(f, "Missing arg for command {cmd}"),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum ShellCommand {
    ShellEqwalize(Eqwalize),
    ShellEqwalizeAll(EqwalizeAll),
    ShellEqwalizeApp(EqwalizeApp),
    ShellEqwalizeTarget(EqwalizeTarget),
    Help,
    Quit,
}
impl ShellCommand {
    pub(crate) fn parse(shell: &Shell, line: String) -> Result<Option<ShellCommand>, ShellError> {
        let project = shell.project.clone();
        let rebar = false;
        let profile = "test".to_string();
        let tokens: Vec<&str> = line.split_ascii_whitespace().collect();
        if let [cmd, args @ ..] = &tokens[..] {
            let (options, args): (Vec<&str>, Vec<_>) =
                args.iter().partition(|&&arg| arg.starts_with('-'));
            match *cmd {
                "help" => return Ok(Some(ShellCommand::Help)),
                "eqwalize" => {
                    if let [option, ..] = options[..] {
                        return Err(ShellError::UnexpectedOption(
                            "eqwalize".into(),
                            option.into(),
                        ));
                    }
                    if !args.is_empty() {
                        return Ok(Some(ShellCommand::ShellEqwalize(Eqwalize {
                            project,
                            profile,
                            format: None,
                            rebar,
                            connect: false,
                            modules: args.iter().map(|s| s.to_string()).collect(),
                            bail_on_error: false,
                        })));
                    }
                    return Err(ShellError::MissingArg("eqwalize".into()));
                }
                "eqwalize-app" => {
                    let include_generated = options.contains(&"--include-generated");
                    if let Some(other) = options
                        .into_iter()
                        .find(|&opt| opt != "--include-generated")
                    {
                        return Err(ShellError::UnexpectedOption(
                            "eqwalize-app".into(),
                            other.into(),
                        ));
                    }
                    if let [_, arg, ..] = args[..] {
                        return Err(ShellError::UnexpectedArg("eqwalize-app".into(), arg.into()));
                    }
                    if let [app] = args[..] {
                        return Ok(Some(ShellCommand::ShellEqwalizeApp(EqwalizeApp {
                            project,
                            profile,
                            format: None,
                            rebar,
                            connect: false,
                            app: app.into(),
                            include_generated,
                            bail_on_error: false,
                        })));
                    }
                    return Err(ShellError::MissingArg("eqwalize-app".into()));
                }
                "eqwalize-all" => {
                    let include_generated = options.contains(&"--include-generated");
                    if let Some(other) = options
                        .into_iter()
                        .find(|&opt| opt != "--include-generated")
                    {
                        return Err(ShellError::UnexpectedOption(
                            "eqwalize-all".into(),
                            other.into(),
                        ));
                    }
                    if let [arg, ..] = args[..] {
                        return Err(ShellError::UnexpectedArg("eqwalize-all".into(), arg.into()));
                    }
                    return Ok(Some(ShellCommand::ShellEqwalizeAll(EqwalizeAll {
                        project,
                        profile,
                        rebar,
                        connect: false,
                        format: None,
                        include_generated,
                        bail_on_error: false,
                        stats: false,
                        list_modules: false,
                    })));
                }
                "eqwalize-target" => {
                    if let [option, ..] = options[..] {
                        return Err(ShellError::UnexpectedOption(
                            "eqwalize-target".into(),
                            option.into(),
                        ));
                    }
                    if let [_, arg, ..] = args[..] {
                        return Err(ShellError::UnexpectedArg(
                            "eqwalize-target".into(),
                            arg.into(),
                        ));
                    }
                    if let [target] = args[..] {
                        return Ok(Some(ShellCommand::ShellEqwalizeTarget(EqwalizeTarget {
                            project,
                            format: None,
                            include_generated: false,
                            connect: false,
                            bail_on_error: false,
                            target: target.into(),
                        })));
                    }
                    return Err(ShellError::MissingArg("eqwalize-target".into()));
                }
                "exit" | "quit" => return Ok(Some(ShellCommand::Quit)),
                s => return Err(ShellError::UnexpectedCommand(s.into())),
            }
        }
        Ok(None)
    }
}

pub const HELP: &str = "\
COMMANDS:
    help                       Print this help
    exit                       Exit the interactive session
    quit                       Exit the interactive session
    eqwalize <modules>         Eqwalize specified modules
        --clause-coverage      Use experimental clause coverage checker
    eqwalize-all               Eqwalize all modules in the current project
        --clause-coverage      Use experimental clause coverage checker
    eqwalize-app <app>         Eqwalize all modules in specified application
        --clause-coverage      Use experimental clause coverage checker
    eqwalize-target <target>   Eqwalize all modules in specified buck target
";

pub const WELCOME: &str = "\
\n
ELP shell automatically keeps state in between eqWAlizer commands to cut down on processing time.
Type \x1b[0;33mhelp\x1b[0m to see available commands (eqwalize, eqwalize-all, ...).\n
";

pub fn run_shell(
    shell: &Shell,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
    ifdef: bool,
) -> Result<()> {
    let start_time = SystemTime::now();

    let mut watchman = Watchman::new(&shell.project)?;
    let config = DiscoverConfig::new(false, "test");
    let mut loaded = load::load_project_at(
        cli,
        &shell.project,
        config.clone(),
        IncludeOtp::Yes,
        Mode::Shell,
        query_config,
        ifdef,
    )?;
    watchman.set_project_dirs(&loaded);
    telemetry::report_elapsed_time("shell operational", start_time);
    let mut rl = rustyline::DefaultEditor::new()?;
    write!(cli, "{WELCOME}")?;
    if !shell.command.is_empty() {
        let command = shell.command.join(" ");
        writeln!(cli, "Executing initial command: {command}")?;
        if execute_line(shell, command, &mut loaded, cli)?.is_break() {
            return Ok(());
        }
    }
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                match watchman.poll_and_apply_changes(&mut loaded)? {
                    UpdateResult::Updated => {}
                    // The interactive shell only runs eqwalize, which doesn't
                    // consult lint config — safe to ignore.
                    UpdateResult::NeedsLintConfigReload { .. } => {}
                    UpdateResult::NeedsFullReload { reason }
                    | UpdateResult::NeedsRestart { reason } => {
                        let _ = writeln!(cli, "{reason}");
                        loaded = load::load_project_at(
                            cli,
                            &shell.project,
                            config.clone(),
                            IncludeOtp::Yes,
                            Mode::Shell,
                            query_config,
                            ifdef,
                        )?;
                        watchman.set_project_dirs(&loaded);
                    }
                }
                if execute_line(shell, line, &mut loaded, cli)?.is_break() {
                    break;
                }
            }
            Err(ReadlineError::Interrupted) => {
                writeln!(cli, "Interrupted")?;
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                writeln!(cli, "Error: {err:?}")?;
                break;
            }
        }
    }
    telemetry::report_elapsed_time("shell done", start_time);
    Ok(())
}

fn execute_line(
    shell: &Shell,
    line: String,
    loaded: &mut LoadResult,
    cli: &mut dyn Cli,
) -> Result<ControlFlow<()>> {
    match ShellCommand::parse(shell, line) {
        Ok(None) => Ok(ControlFlow::Continue(())),
        Ok(Some(ShellCommand::Help)) => {
            write!(cli, "{HELP}")?;
            Ok(ControlFlow::Continue(()))
        }
        Ok(Some(ShellCommand::Quit)) => Ok(ControlFlow::Break(())),
        Ok(Some(ShellCommand::ShellEqwalize(eqwalize))) => {
            eqwalizer_cli::do_eqwalize_module(&eqwalize, loaded, cli)
                .or_else(|e| writeln!(cli, "Error: {e}"))?;
            Ok(ControlFlow::Continue(()))
        }
        Ok(Some(ShellCommand::ShellEqwalizeApp(eqwalize_app))) => {
            eqwalizer_cli::do_eqwalize_app(&eqwalize_app, loaded, cli)
                .or_else(|e| writeln!(cli, "Error: {e}"))?;
            Ok(ControlFlow::Continue(()))
        }
        Ok(Some(ShellCommand::ShellEqwalizeAll(eqwalize_all))) => {
            eqwalizer_cli::do_eqwalize_all(&eqwalize_all, loaded, cli)
                .or_else(|e| writeln!(cli, "Error: {e}"))?;
            Ok(ControlFlow::Continue(()))
        }
        Ok(Some(ShellCommand::ShellEqwalizeTarget(eqwalize_target))) => {
            eqwalizer_cli::do_eqwalize_target(&eqwalize_target, loaded, cli)
                .or_else(|e| writeln!(cli, "Error: {e}"))?;
            Ok(ControlFlow::Continue(()))
        }
        Err(err) => {
            write!(cli, "{err}\n{HELP}")?;
            Ok(ControlFlow::Continue(()))
        }
    }
}
