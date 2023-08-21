/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;

use anyhow::Result;
use elp::build::load;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp::document::Document;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::FileKind;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide::elp_ide_db::elp_base_db::SourceRoot;
use elp_ide::elp_ide_db::elp_base_db::SourceRootId;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_project_model::DiscoverConfig;
use rustyline::error::ReadlineError;
use serde::Deserialize;

use crate::args::Eqwalize;
use crate::args::EqwalizeAll;
use crate::args::EqwalizeApp;
use crate::args::Shell;
use crate::eqwalizer_cli;

#[derive(Debug, Clone, Deserialize)]
struct Watchman {
    watch: PathBuf,
}

#[derive(Debug, Clone, Deserialize)]
struct WatchmanClock {
    clock: String,
}

#[derive(Debug, Clone, Deserialize)]
struct WatchmanChanges {
    files: Vec<WatchmanFile>,
}

#[derive(Debug, Clone, Deserialize)]
struct WatchmanFile {
    name: String,
    exists: bool,
}

impl Watchman {
    fn cmd() -> Command {
        Command::new("watchman")
    }

    fn new(project: &Path) -> Result<Self> {
        let mut cmd = Self::cmd();
        cmd.arg("watch-project");
        cmd.arg(project.as_os_str());
        Ok(serde_json::from_slice(&cmd.output()?.stdout)?)
    }

    fn get_clock(&self) -> Result<WatchmanClock> {
        let mut cmd = Self::cmd();
        cmd.arg("clock");
        cmd.arg(self.watch.as_os_str());
        Ok(serde_json::from_slice(&cmd.output()?.stdout)?)
    }

    fn get_changes(&self, from: &WatchmanClock, patterns: Vec<&str>) -> Result<WatchmanChanges> {
        let mut cmd = Command::new("watchman");
        cmd.arg("since");
        cmd.arg(self.watch.as_os_str());
        cmd.arg(&from.clock);
        cmd.args(patterns);
        Ok(serde_json::from_slice(&cmd.output()?.stdout)?)
    }
}

#[derive(Debug, Clone)]
enum ShellError {
    UnexpectedCommand(String),
    UnexpectedOption(String, String),
    UnexpectedArg(String, String),
    MissingArg(String),
}
impl fmt::Display for ShellError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ShellError::UnexpectedCommand(cmd) => write!(f, "Unexpected command {}", cmd),
            ShellError::UnexpectedOption(cmd, arg) => {
                write!(f, "Unexpected option {} for command {}", arg, cmd)
            }
            ShellError::UnexpectedArg(cmd, arg) => {
                write!(f, "Unexpected arg {} for command {}", arg, cmd)
            }
            ShellError::MissingArg(cmd) => write!(f, "Missing arg for command {}", cmd),
        }
    }
}

#[derive(Debug, Clone)]
enum ShellCommand {
    ShellEqwalize(Eqwalize),
    ShellEqwalizeAll(EqwalizeAll),
    ShellEqwalizeApp(EqwalizeApp),
    Help,
    Quit,
}
impl ShellCommand {
    fn parse(shell: &Shell, line: String) -> Result<Option<ShellCommand>, ShellError> {
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
                    if let [_, arg, ..] = args[..] {
                        return Err(ShellError::UnexpectedArg("eqwalize".into(), arg.into()));
                    }
                    if let [module] = args[..] {
                        return Ok(Some(ShellCommand::ShellEqwalize(Eqwalize {
                            project,
                            profile,
                            rebar,
                            module: module.into(),
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
                            rebar,
                            app: app.into(),
                            include_generated,
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
                        format: None,
                        include_generated,
                    })));
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
    eqwalize <module>          Eqwalize specified module
    eqwalize-all               Eqwalize all modules in the current project
        --include-generated    Include generated modules
    eqwalize-app <app>         Eqwalize all modules in specified application
        --include-generated    Include generated modules
";

// Adapted from elp::server
fn process_changes_to_vfs_store(loaded: &mut LoadResult) -> bool {
    let changed_files = loaded.vfs.take_changes();

    if changed_files.is_empty() {
        return false;
    }

    let raw_database = loaded.analysis_host.raw_database_mut();

    for file in &changed_files {
        if raw_database.file_kind(file.file_id) == FileKind::Header {
            raw_database.set_include_files_revision(raw_database.include_files_revision() + 1);
        }
        if file.exists() {
            let bytes = loaded.vfs.file_contents(file.file_id).to_vec();
            let document = Document::from_bytes(bytes);
            raw_database.set_file_text(file.file_id, Arc::from(document.content));
        } else {
            raw_database.set_file_text(file.file_id, Arc::from(""));
        };
    }

    if changed_files
        .iter()
        .any(|file| file.is_created_or_deleted())
    {
        let sets = loaded.file_set_config.partition(&loaded.vfs);
        for (idx, set) in sets.into_iter().enumerate() {
            let root_id = SourceRootId(idx as u32);
            for file_id in set.iter() {
                raw_database.set_file_source_root(file_id, root_id);
            }
            let root = SourceRoot::new(set);
            raw_database.set_source_root(root_id, Arc::new(root));
        }
    }

    true
}

fn update_changes(
    loaded: &mut LoadResult,
    watchman: &Watchman,
    last_read: &WatchmanClock,
) -> Result<WatchmanClock> {
    let vfs = &mut loaded.vfs;
    let time = watchman.get_clock()?;
    let file_changes = watchman.get_changes(last_read, vec!["**/*.hrl", "**/*.erl"])?;
    file_changes.files.into_iter().for_each(|file| {
        let path = watchman.watch.join(file.name);
        let vfs_path = VfsPath::from(AbsPathBuf::assert(path.clone()));
        if !file.exists {
            vfs.set_file_contents(vfs_path, None);
        } else {
            let contents =
                fs::read(&path).unwrap_or_else(|_| panic!("Cannot read created file {:?}", path));
            vfs.set_file_contents(vfs_path, Some(contents));
        }
    });
    process_changes_to_vfs_store(loaded);
    Ok(time)
}

pub fn run_shell(shell: &Shell, cli: &mut dyn Cli) -> Result<()> {
    let watchman = Watchman::new(&shell.project)
        .map_err(|_err| anyhow::Error::msg(
            "Could not find project. Are you in an Erlang project directory, or is one specified using --project?"
        ))?;
    let config = DiscoverConfig::new(false, "test");
    let mut loaded = load::load_project_at(cli, &shell.project, config, IncludeOtp::Yes)?;
    loaded.analysis_host.raw_database_mut().in_shell();
    let mut rl = rustyline::DefaultEditor::new()?;
    let mut last_read = watchman.get_clock()?;
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                last_read = update_changes(&mut loaded, &watchman, &last_read)?;
                match ShellCommand::parse(shell, line) {
                    Ok(None) => (),
                    Ok(Some(ShellCommand::Help)) => write!(cli, "{}", HELP)?,
                    Ok(Some(ShellCommand::Quit)) => break,
                    Ok(Some(ShellCommand::ShellEqwalize(eqwalize))) => {
                        eqwalizer_cli::do_eqwalize_module(&eqwalize, &loaded, cli)
                            .or_else(|e| writeln!(cli, "Error: {}", e))?;
                    }
                    Ok(Some(ShellCommand::ShellEqwalizeApp(eqwalize_app))) => {
                        eqwalizer_cli::do_eqwalize_app(&eqwalize_app, &loaded, cli)
                            .or_else(|e| writeln!(cli, "Error: {}", e))?;
                    }
                    Ok(Some(ShellCommand::ShellEqwalizeAll(eqwalize_all))) => {
                        eqwalizer_cli::do_eqwalize_all(&eqwalize_all, &loaded, cli)
                            .or_else(|e| writeln!(cli, "Error: {}", e))?;
                    }
                    Err(err) => write!(cli, "{}\n{}", err, HELP)?,
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
                writeln!(cli, "Error: {:?}", err)?;
                break;
            }
        }
    }
    Ok(())
}
