/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cmp::Ordering;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io::IsTerminal;
use std::path::PathBuf;

use clap::CommandFactory;
use clap::ValueHint;
use clap_complete::aot::Shell as CompletionShell;
use clap_complete::engine::CompletionCandidate;
use elp_ide::elp_ide_db::DiagnosticCode;
use elp_project_model::buck::BuckQueryConfig;
use itertools::Itertools;
use serde::Deserialize;
use serde::Serialize;
use strum::AsRefStr;

use crate::build_info_cli::BuildInfo;
use crate::build_info_cli::ProjectInfo;
use crate::config_stanza::ConfigStanza;
use crate::daemon::Daemon;
use crate::dialyzer_cli::DialyzeAll;
use crate::elp_parse_cli::ParseAllElp;
use crate::eqwalizer_cli::Eqwalize;
use crate::eqwalizer_cli::EqwalizeAll;
use crate::eqwalizer_cli::EqwalizeApp;
use crate::eqwalizer_cli::EqwalizeStats;
use crate::eqwalizer_cli::EqwalizeTarget;
use crate::erlang_service_cli::ParseAll;
use crate::explain_cli::Explain;
use crate::glean::Glean;
use crate::lint_cli::Lint;
use crate::lint_compare::LintCompare;
use crate::lint_list_cli::LintList;
use crate::shell::Shell;
use crate::ssr_cli::Ssr;

#[derive(Clone, Debug, clap::Args)]
pub struct GenerateCompletions {
    /// Shell to generate completions for
    #[arg(value_enum, value_name = "SHELL")]
    pub shell: CompletionShell,
}

#[derive(Clone, Debug, clap::Args)]
pub struct RunServer {}

#[derive(Clone, Debug, clap::Args)]
pub struct Version {}

/// One-line description shown at the top of `elp ssr --help` / `elp search --help`.
const SSR_DESCR: &str = "Search Erlang code by syntax-tree shape (not text) using SSR (Structural Search and Replace) patterns.";

/// One-line description for the `ssr` alias. Distinct from `SSR_DESCR` so that
/// `elp --help` still shows `ssr` is an alias for the canonical `search`.
const SSR_ALIAS_DESCR: &str =
    "Alias for 'search': search Erlang code structurally using SSR patterns.";

/// Footer with a placeholder cheat-sheet, examples, and a link to the full guide.
/// Shared by both `ssr` and `search` so their `--help` footers stay identical.
const SSR_FOOTER: &str = "\
Patterns are Erlang code with placeholders:
    _@Name   match any single expression or pattern; reuse a name to require the two matches be equal
    _@_      anonymous wildcard (match anything, bind nothing)
    _@@Name  glob: match zero or more sibling elements (at most one glob per sequence)

Constrain placeholders with a `where` clause, e.g. `_@X where is_atom(_@X)`.

Examples:
    elp search 'lists:reverse(lists:reverse(_@List))'
    elp search 'case _@Cond of true -> _@Body; false -> _@Body end'
    elp search --format json 'foo(_@@Args)'

A bare PATTERN is shorthand for `ssr: PATTERN.`. Prefix with `LABEL:ssr: ` to tag matches.

Full syntax guide: https://whatsapp.github.io/erlang-language-platform/docs/structural-search";

#[derive(Clone, Debug, AsRefStr, clap::Subcommand)]
#[strum(serialize_all = "kebab-case")]
pub enum Command {
    /// Tree-sitter parse all files in a project for specified rebar.config file
    #[command(name = "parse-elp")]
    ParseAllElp(ParseAllElp),
    /// Dump ast for all files in a project for specified rebar.config file
    #[command(name = "parse-all")]
    ParseAll(ParseAll),
    /// Eqwalize specified modules
    #[command(name = "eqwalize")]
    Eqwalize(Eqwalize),
    /// Eqwalize all opted-in modules in a project
    #[command(name = "eqwalize-all")]
    EqwalizeAll(EqwalizeAll),
    /// Eqwalize all opted-in modules in specified buck target
    #[command(name = "eqwalize-target")]
    EqwalizeTarget(EqwalizeTarget),
    /// Eqwalize all opted-in modules in specified application
    #[command(name = "eqwalize-app")]
    EqwalizeApp(EqwalizeApp),
    /// Return statistics about code quality for eqWAlizer
    #[command(name = "eqwalize-stats", hide = true)]
    EqwalizeStats(EqwalizeStats),
    /// Run Dialyzer on the whole project by shelling out to a `dialyzer-run` tool on the path to do the legwork.
    #[command(name = "dialyze-all", hide = true)]
    DialyzeAll(DialyzeAll),
    /// Generate build info JSON file
    #[command(name = "build-info")]
    BuildInfo(BuildInfo),
    /// Generate shell completions
    #[command(name = "generate-completions")]
    GenerateCompletions(GenerateCompletions),
    /// Run lsp server
    #[command(name = "server")]
    RunServer(RunServer),
    /// Parse files in project and emit diagnostics, optionally apply fixes.
    #[command(name = "lint")]
    Lint(Lint),
    /// Diff two `elp lint --format=json` JSONL artifacts.
    #[command(name = "lint-compare")]
    LintCompare(LintCompare),
    /// Search Erlang code structurally using SSR (Structural Search and Replace) patterns.
    #[command(name = "search", about = SSR_DESCR, after_help = SSR_FOOTER)]
    Search(Ssr),
    /// Alias for 'search': search Erlang code structurally using SSR patterns.
    #[command(name = "ssr", about = SSR_ALIAS_DESCR, after_help = SSR_FOOTER)]
    Ssr(Ssr),
    /// Print version
    #[command(name = "version")]
    Version(Version),
    /// Starts an interactive ELP shell
    #[command(name = "shell")]
    Shell(Shell),
    /// Manage a persistent ELP daemon for fast turnaround
    #[command(name = "daemon")]
    Daemon(Daemon),
    /// Explain a diagnostic code
    #[command(name = "explain")]
    Explain(Explain),
    /// List all available lint diagnostic codes
    #[command(name = "lint-list")]
    LintList(LintList),
    /// Generate project info file
    #[command(name = "project-info")]
    ProjectInfo(ProjectInfo),
    /// Glean indexer
    #[command(name = "glean")]
    Glean(Glean),
    /// Dump a JSON config stanza suitable for use in VS Code project.json
    #[command(name = "config")]
    ConfigStanza(ConfigStanza),
}

#[derive(Debug, Clone, clap::Parser)]
#[command(name = "elp")]
pub struct Args {
    // These top-level options are `global = true` so they can be supplied
    // either before or after a subcommand. bpaf parsed them position
    // independently; clap requires `global` to preserve that behaviour.
    // Callers such as the Glean indexer pass `--erl`/`--escript` after the
    // `glean` subcommand.
    #[arg(long, value_name = "LOG_FILE", value_hint = ValueHint::FilePath, global = true, help_heading = "Global options")]
    pub log_file: Option<PathBuf>,
    #[arg(long, value_name = "ERL", value_hint = ValueHint::ExecutablePath, global = true, help_heading = "Global options")]
    pub erl: Option<PathBuf>,
    #[arg(long, value_name = "ESCRIPT", value_hint = ValueHint::ExecutablePath, global = true, help_heading = "Global options")]
    pub escript: Option<PathBuf>,
    #[arg(long, global = true, help_heading = "Global options")]
    pub no_log_buffering: bool,

    /// When using buck, do not invoke a build step for generated files.
    #[arg(long, global = true, help_heading = "Global options")]
    pub no_buck_generated: bool,

    /// Use buck2 targets for first stage project loading
    #[arg(long, global = true, help_heading = "Global options")]
    pub buck_quick_start: bool,

    /// Deprecated: ifdef/ifndef condition evaluation is now enabled by default.
    /// Pass `--ifdef false` to disable it.
    #[arg(long, global = true, num_args = 0..=1, default_missing_value = "true", help_heading = "Global options")]
    pub ifdef: Option<bool>,

    /// Use color in output
    #[arg(
        long = "color",
        alias = "colour",
        value_name = "WHEN",
        default_value = "always",
        global = true,
        help_heading = "Global options"
    )]
    pub color: Option<Color>,

    // Backward-compatible hidden flags for the old bpaf-based completion
    // generation. The OD provisioning scripts use these; remove after
    // T276231496 migrates them to `elp generate-completions <shell>`.
    #[arg(long = "bpaf-complete-style-bash", hide = true)]
    pub bpaf_compat_bash: bool,
    #[arg(long = "bpaf-complete-style-zsh", hide = true)]
    pub bpaf_compat_zsh: bool,
    #[arg(long = "bpaf-complete-style-fish", hide = true)]
    pub bpaf_compat_fish: bool,

    #[command(subcommand)]
    pub command: Option<Command>,
}

impl Args {
    pub fn query_config(&self) -> BuckQueryConfig {
        if self.buck_quick_start {
            BuckQueryConfig::BuckTargetsOnly
        } else if self.no_buck_generated {
            BuckQueryConfig::NoBuildGeneratedCode
        } else {
            BuckQueryConfig::BuildGeneratedCode
        }
    }

    /// Determine if color should be used based on the --color argument
    pub fn should_use_color(&self) -> bool {
        match self.color {
            Some(Color::Always) => true,
            Some(Color::Never) => false,
            Some(Color::Auto) | None => {
                env::var("NO_COLOR").is_err() && std::io::stdout().is_terminal()
            }
        }
    }

    pub fn render_help() -> String {
        Args::command().render_help().to_string()
    }

    pub fn bpaf_compat_shell(&self) -> Option<CompletionShell> {
        if self.bpaf_compat_bash {
            Some(CompletionShell::Bash)
        } else if self.bpaf_compat_zsh {
            Some(CompletionShell::Zsh)
        } else if self.bpaf_compat_fish {
            Some(CompletionShell::Fish)
        } else {
            None
        }
    }
}

impl Command {
    pub fn normalize(&mut self) {
        if let Command::Lint(args) = self {
            args.normalize()
        }
    }
}

// --- Enumerated argument values ---
//
// These derive `clap::ValueEnum`, so clap parses and validates them directly
// (no hand-written value parsers). The CLI value strings are the kebab-cased
// variant names; `#[value(name = ...)]` pins any that must differ.

/// Output format for diagnostics. Absent means the default human-readable output.
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    clap::ValueEnum,
    Serialize,
    Deserialize
)]
#[serde(rename_all = "snake_case")]
pub enum Format {
    Json,
}

/// Minimum diagnostic severity to report.
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    clap::ValueEnum,
    Serialize,
    Deserialize
)]
#[serde(rename_all = "snake_case")]
pub enum Severity {
    Error,
    Warning,
    #[value(name = "weak_warning")]
    WeakWarning,
    Information,
}

/// Macro expansion strategy for structural search.
#[derive(Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
pub enum MacroStrategyArg {
    Expand,
    NoExpand,
    VisibleExpand,
}

/// When to use color in output.
#[derive(Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
pub enum Color {
    Always,
    Never,
    Auto,
}

// --- Dynamic shell completers ---
//
// These are wired into clap via `#[arg(add = ...)]` and invoked by the
// dynamic-completion engine (see the `CompleteEnv` hook in `main`). They
// preserve the dynamic completion that the previous bpaf-based CLI provided.

#[derive(Deserialize)]
struct ModuleConfig {
    modules: Vec<String>,
}

const MODULES_FILE: &str = ".modules.toml";

/// Complete a `--module` argument by fuzzy-matching the current input against
/// the module names listed in the nearest `.modules.toml`.
pub(crate) fn module_completer(current: &OsStr) -> Vec<CompletionCandidate> {
    let input = current.to_string_lossy();
    let mut modules = vec![];
    let curr = env::current_dir().unwrap();
    let mut potential_path = Some(curr.as_path());
    while let Some(path) = potential_path {
        let file_path = path.join(MODULES_FILE);

        if !file_path.is_file() {
            potential_path = path.parent();
            continue;
        } else {
            if let Ok(content) = fs::read_to_string(file_path)
                && let Ok(config) = toml::from_str::<ModuleConfig>(&content)
            {
                for module_name in config.modules.into_iter() {
                    modules.push(module_name)
                }
            }
            break;
        }
    }
    get_suggesions(&input, modules)
}

/// Complete a diagnostic `--code` argument with every known diagnostic code and
/// label. clap prefix-filters the returned candidates against the current input.
pub(crate) fn diagnostic_code_candidates() -> Vec<CompletionCandidate> {
    DiagnosticCode::all_diagnostic_codes()
        .flat_map(|code| [code.as_code().to_string(), code.as_label().to_string()])
        .map(CompletionCandidate::new)
        .collect()
}

fn get_suggesions(input: &str, modules: Vec<String>) -> Vec<CompletionCandidate> {
    const MAX_RESULTS: usize = 10;

    modules
        .into_iter()
        .map(|key| (strsim::normalized_damerau_levenshtein(input, &key), key))
        .sorted_by(|a, b| PartialOrd::partial_cmp(&b.0, &a.0).unwrap_or(Ordering::Less))
        .take(MAX_RESULTS)
        .map(|(_lev, v)| CompletionCandidate::new(v))
        .collect()
}

// --- Shell completion generation ---

pub fn generate_completions(shell: CompletionShell, buf: &mut dyn std::io::Write) {
    clap_complete::generate(shell, &mut Args::command(), "elp", buf);
}

// --- Impl blocks ---

#[cfg(test)]
mod tests {
    use clap::CommandFactory;
    use clap::Parser;

    use super::Args;
    use super::Command;

    #[test]
    fn check_options() {
        Args::command().debug_assert();
    }

    /// The Glean indexer invokes elp with the global `--erl`/`--escript`
    /// options *after* the `glean` subcommand. These options are declared on
    /// the top-level `Args`, so they must be `global = true` to be accepted in
    /// that position (bpaf parsed them position independently).
    #[test]
    fn global_options_accepted_after_subcommand() {
        let args = Args::try_parse_from([
            "elp",
            "glean",
            "--schema2",
            "--multi",
            "--project",
            "/some/project",
            "--erl",
            "/path/to/erl",
            "--escript",
            "/path/to/escript",
            "--to",
            "/path/to/out",
        ])
        .expect("global options must be accepted after a subcommand");

        assert_eq!(
            args.erl.as_deref(),
            Some(std::path::Path::new("/path/to/erl"))
        );
        assert_eq!(
            args.escript.as_deref(),
            Some(std::path::Path::new("/path/to/escript"))
        );
        assert!(matches!(args.command, Some(Command::Glean(_))));
    }
}
