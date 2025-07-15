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

use std::env;
use std::fmt;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Result;
use anyhow::bail;
use bpaf::Bpaf;
use bpaf::Parser;
use bpaf::construct;
use krates::Builder;
use krates::Cmd;
use krates::DepKind;
use krates::Krates;
use krates::Node;
use krates::Scope;
use krates::cm;
use krates::petgraph;
use krates::petgraph::dot::Config;
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
        Command::Graph(_) => {
            match make_crate_graph() {
                Ok(contents) => {
                    let sh = Shell::new()?;
                    let dot_file = project_root().join("docs/crate_graph.dot");
                    let png_file = project_root().join("docs/crate_graph.png");
                    sh.write_file(dot_file.clone(), contents)?;
                    // Convert to png by
                    // dot -Tpng crate_graph.dot -ocrate_graph.png

                    cmd!(sh, "dot -Tpng {dot_file} -o{png_file}").output()?;
                    Ok(())
                }
                Err(err) => Err(anyhow::anyhow!("{}", err)),
            }
        }
        Help() => {
            eprintln!(
                "\
cargo xtask
Run custom build command.

USAGE:
    cargo xtask <SUBCOMMAND>

SUBCOMMANDS:
    codegen
    graph"
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
    Graph(Graph),
    Help(),
}

fn command() -> impl Parser<Command> {
    let code_gen = code_gen()
        .map(Command::CodeGen)
        .to_options()
        .command("codegen")
        .help("Generate ast from tree-sitter grammar");

    let graph = graph()
        .map(Command::Graph)
        .to_options()
        .command("graph")
        .help("Generate crate graph");

    construct!([code_gen, graph]).fallback(Help())
}

#[derive(Clone, Debug, Bpaf)]
struct CodeGen {}

#[derive(Clone, Debug, Bpaf)]
struct Graph {}

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

pub fn reformat(text: &str) -> Result<String> {
    let sh = Shell::new()?;
    ensure_rustfmt(&sh)?;
    let rustfmt_toml = project_root().join("rustfmt.toml");
    let stdout = cmd!(sh, "rustfmt --config-path {rustfmt_toml} ")
        .stdin(text)
        .read()?;
    Ok(format!("//! {PREAMBLE}\n\n{stdout}\n"))
}

fn ensure_rustfmt(sh: &Shell) -> Result<()> {
    let out = cmd!(sh, "rustfmt --version").read()?;
    if !out.contains("stable") {
        bail!(
            "Failed to run rustfmt from toolchain 'stable'. \
             Please run `rustup component add rustfmt --toolchain stable` to install it.",
        )
    }
    Ok(())
}

fn make_crate_graph() -> Result<String, krates::Error> {
    let cargo_toml = project_root().join("Cargo.toml");
    let mut cmd = Cmd::new();
    cmd.manifest_path(cargo_toml);
    // Enable all features, works for either an entire workspace or a single crate
    cmd.all_features();

    let mut builder = Builder::new();
    builder.workspace(true);
    builder.ignore_kind(DepKind::Normal, Scope::NonWorkspace);
    builder.ignore_kind(DepKind::Build, Scope::All);
    builder.ignore_kind(DepKind::Dev, Scope::All);
    builder.exclude(
        [
            "always-assert:0.1.2",
            "anyhow:1.0.40",
            "anymap:0.12.1",
            "beam_file:0.2.4",
            "byteorder:1.4.3",
            "cov-mark:2.0.0-pre.1",
            "crossbeam-channel:0.5.1",
            "dashmap:4.0.2",
            "dissimilar:1.0.2",
            "drop_bomb:0.1.5",
            "either:1.6.1",
            "env_logger:0.8.3",
            "fxhash:0.2.1",
            "indexmap:1.6.2",
            "indicatif:0.16.2",
            "itertools:0.10.1",
            "krates:0.9.0",
            "la-arena:0.2.1",
            "lazy_static:1.4.0",
            "libflate:1.1.0",
            "limit:0.0.0",
            "log:0.4.14",
            "lsp-server:0.5.0",
            "lsp-types:0.89.0",
            "nonempty:0.6.0",
            "num:0.4.0",
            "num-bigint:0.4.0",
            "num-derive:0.3.3",
            "num-traits:0.2.14",
            "once_cell:1.8.0",
            "parking_lot:0.11.1",
            "paths:0.0.0",
            "pico-args:0.4.1",
            "proc-macro2:1.0.27",
            "profile:0.0.0",
            "quote:1.0.9",
            "rayon:1.5.1",
            "regex:1.5.4",
            "rowan:0.14.1",
            "salsa:0.17.0-pre.1",
            "serde:1.0.126",
            "serde_json:1.0.64",
            "serde_path_to_error:0.1.4",
            "smallvec:1.6.1",
            "smol_str:0.1.21",
            "stdx:0.0.0",
            "tempfile:3.2.0",
            "text_edit:0.0.0",
            "thiserror:1.0.24",
            "threadpool:1.8.1",
            "tikv-jemallocator:0.4.1",
            "tree-sitter:0.19.5",
            "tree-sitter-erlang:0.1.0",
            "vfs:0.0.0",
            "vfs-notify:0.0.0",
            "walkdir:2.3.2",
            "xshell:0.1.12",
            "xtask:0.1.0",
        ]
        .iter()
        .map(|spec| spec.parse().unwrap()),
    );

    let krates: Krates = builder.build(cmd, |pkg: cm::Package| {
        println!("Crate {} was filtered out", pkg.id);
    })?;

    // clean up the labels for the nodes
    let graph = krates.graph().map(
        |_, p| {
            let name = match p {
                Node::Krate { krate, .. } => krate.name.to_string(),
                Node::Feature { name, .. } => name.to_string(),
            };
            Some(GraphNode { name })
        },
        |_, e| e,
    );

    // Print a dot graph of the entire crate graph
    let contents = format!(
        "{:?}",
        petgraph::dot::Dot::with_config(&graph, &[Config::EdgeNoLabel])
    );

    Ok(contents)
}

struct GraphNode {
    name: String,
}

impl fmt::Display for GraphNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Debug for GraphNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
