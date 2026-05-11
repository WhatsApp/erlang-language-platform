/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! This provides extra build system commands, most notably:
//! `cargo xtask codegen` for code generation.

#[cfg(not(buck_build))]
use std::env;
#[cfg(not(buck_build))]
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use anyhow::Result;
use bpaf::Bpaf;
use bpaf::Parser;
use bpaf::construct;
use xshell::Shell;
use xshell::cmd;

use crate::Command::Help;

mod codegen;

fn main() -> Result<()> {
    let args = args().run();
    match args.command {
        Command::CodeGen(_) => {
            let mode = codegen::Mode::Overwrite;
            codegen::CodegenCmd { mode }.run()
        }
        Help() => {
            eprintln!(
                "\
cargo xtask
Run custom build command.

USAGE:
    cargo xtask <SUBCOMMAND>

SUBCOMMANDS:
    codegen"
            );
            Ok(())
        }
    }
}

#[derive(Debug, Clone, Bpaf)]
#[bpaf(options)]
struct Args {
    #[bpaf(external(command))]
    command: Command,
}

#[derive(Clone, Debug)]
enum Command {
    CodeGen(CodeGen),
    Help(),
}

fn command() -> impl Parser<Command> {
    let code_gen = code_gen()
        .map(Command::CodeGen)
        .to_options()
        .command("codegen")
        .help("Generate ast from tree-sitter grammar");

    construct!([code_gen]).fallback(Help())
}

#[derive(Clone, Debug, Bpaf)]
struct CodeGen {}

#[cfg(buck_build)]
pub fn project_root() -> PathBuf {
    let rustfmt_toml = buck_resources::get("whatsapp/elp/xtask/rustfmt_toml")
        .expect("failed to get rustfmt_toml resource");
    rustfmt_toml
        .canonicalize()
        .expect("failed to canonicalize rustfmt_toml path")
        .parent()
        .expect("rustfmt_toml has no parent directory")
        .to_path_buf()
}

#[cfg(not(buck_build))]
pub fn project_root() -> PathBuf {
    Path::new(
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .ancestors()
    .nth(1)
    .unwrap()
    .to_path_buf()
}

const PREAMBLE: &str = "\x40generated file, do not edit by hand, see `xtask/src/codegen.rs`";

#[cfg(buck_build)]
pub fn reformat(text: &str) -> Result<String> {
    let sh = Shell::new()?;
    let rustfmt = buck_resources::get("whatsapp/elp/xtask/rustfmt")?;
    let rustfmt_toml = buck_resources::get("whatsapp/elp/xtask/rustfmt_toml")?;
    let stdout = cmd!(sh, "{rustfmt} --config-path {rustfmt_toml}")
        .stdin(text)
        .read()?;
    Ok(format!("//! {PREAMBLE}\n\n{stdout}\n"))
}

#[cfg(not(buck_build))]
pub fn reformat(text: &str) -> Result<String> {
    let sh = Shell::new()?;
    ensure_rustfmt(&sh)?;
    let rustfmt_toml = project_root().join("rustfmt.toml");
    let stdout = cmd!(sh, "rustfmt --config-path {rustfmt_toml} ")
        .stdin(text)
        .read()?;
    Ok(format!("//! {PREAMBLE}\n\n{stdout}\n"))
}

#[cfg(not(buck_build))]
fn ensure_rustfmt(sh: &Shell) -> Result<()> {
    cmd!(sh, "rustfmt --version")
        .read()
        .context("Failed to run rustfmt. Please ensure rustfmt is installed and on PATH.")?;
    Ok(())
}
