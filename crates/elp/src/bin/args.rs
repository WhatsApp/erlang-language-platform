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

use anyhow::Result;
use clap::ArgAction;
use clap::CommandFactory;
use clap::ValueHint;
use clap_complete::aot::Shell as CompletionShell;
use clap_complete::engine::ArgValueCandidates;
use clap_complete::engine::ArgValueCompleter;
use clap_complete::engine::CompletionCandidate;
use elp_ide::elp_ide_db::DiagnosticCode;
use elp_project_model::buck::BuckQueryConfig;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::Strategy;
use itertools::Itertools;
use serde::Deserialize;
use serde::Serialize;
use strum::AsRefStr;

use crate::build_info_cli::BuildInfo;
use crate::build_info_cli::ProjectInfo;
use crate::lint_cli::Lint;

#[derive(Clone, Debug, clap::Args)]
pub struct ParseAllElp {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::DirPath)]
    pub project: PathBuf,
    /// Parse a single module from the project, not the entire project
    #[arg(long, value_name = "MODULE", add = ArgValueCompleter::new(module_completer))]
    pub module: Option<String>,
    /// Parse a single file from the project, not the entire project. \nThis can be an include file or escript, etc.
    #[arg(long)]
    pub file: Option<String>,
    /// Path to a directory where to dump result files
    #[arg(long, value_name = "TO", value_hint = ValueHint::DirPath)]
    pub to: Option<PathBuf>,
    /// Do not print the full diagnostics for a file, just the count
    #[arg(long = "no-diags", action = ArgAction::SetFalse, default_value_t = true)]
    pub print_diags: bool,
    /// Report experimental diagnostics too, if diagnostics are enabled
    #[arg(long = "experimental")]
    pub experimental_diags: bool,
    /// Rebar3 profile to pickup
    #[arg(long = "as", value_name = "PROFILE", default_value = "test")]
    pub profile: String,
    /// Report the resolution of include directives for comparison with OTP ones
    #[arg(long = "dump-includes")]
    pub dump_include_resolutions: bool,
    /// Run with rebar
    #[arg(long)]
    pub rebar: bool,
    /// Also process generated modules
    #[arg(long)]
    pub include_generated: bool,
    /// Parse the files serially, not in parallel
    #[arg(long)]
    pub serial: bool,
    /// If specified, use the provided CLI severity mapping instead of the default one
    #[arg(long)]
    pub use_cli_severity: bool,
    /// Customize the output format (defaults to human-readable)
    #[arg(long, value_name = "FORMAT")]
    pub format: Option<Format>,
    /// Report system memory usage and other statistics
    #[arg(long = "report-system-stats")]
    pub report_system_stats: bool,
    /// Minimum severity level to report
    #[arg(long, value_name = "SEVERITY")]
    pub severity: Option<Severity>,
}

#[derive(Clone, Debug, clap::Args)]
pub struct ParseAll {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::AnyPath)]
    pub project: PathBuf,
    /// Path to a directory where to dump .etf files
    #[arg(long)]
    pub to: PathBuf,
    /// Rebar3 profile to pickup
    #[arg(long = "as", value_name = "PROFILE", default_value = "test")]
    pub profile: String,
    /// Parse a single module from the project, not the entire project
    #[arg(long, value_name = "MODULE", add = ArgValueCompleter::new(module_completer))]
    pub module: Option<String>,
    /// Run with buck
    #[arg(long)]
    pub buck: bool,
    /// Print statistics when done
    #[arg(long)]
    pub stats: bool,
    /// When printing statistics, include the list of modules parsed
    #[arg(long)]
    pub list_modules: bool,
}

#[derive(Clone, Debug, clap::Args)]
pub struct Eqwalize {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::AnyPath)]
    pub project: PathBuf,
    /// Rebar3 profile to pickup
    #[arg(long = "as", value_name = "PROFILE", default_value = "test")]
    pub profile: String,
    /// Customize the output format (defaults to human-readable)
    #[arg(long, value_name = "FORMAT")]
    pub format: Option<Format>,
    /// Run with rebar
    #[arg(long)]
    pub rebar: bool,
    /// Use a persistent daemon for fast turnaround (auto-starts if needed)
    #[arg(long)]
    pub connect: bool,
    /// Exit with a non-zero status code if any errors are found
    #[arg(long)]
    pub bail_on_error: bool,
    /// Eqwalize specified modules (not files)
    #[arg(value_name = "MODULES", required = true)]
    pub modules: Vec<String>,
}

#[derive(Clone, Debug, clap::Args)]
pub struct EqwalizeAll {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::AnyPath)]
    pub project: PathBuf,
    /// Rebar3 profile to pickup
    #[arg(long = "as", value_name = "PROFILE", default_value = "test")]
    pub profile: String,
    /// Customize the output format (defaults to human-readable)
    #[arg(long, value_name = "FORMAT")]
    pub format: Option<Format>,
    /// Run with rebar
    #[arg(long)]
    pub rebar: bool,
    /// Use a persistent daemon for fast turnaround (auto-starts if needed)
    #[arg(long)]
    pub connect: bool,
    /// Also eqwalize opted-in generated modules from project (deprecated)
    #[arg(long, hide = true)]
    pub include_generated: bool,
    /// Exit with a non-zero status code if any errors are found
    #[arg(long)]
    pub bail_on_error: bool,
    /// Print statistics when done
    #[arg(long)]
    pub stats: bool,
    /// When printing statistics, include the list of modules parsed
    #[arg(long)]
    pub list_modules: bool,
}

#[derive(Clone, Debug, clap::Args)]
pub struct EqwalizeTarget {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::AnyPath)]
    pub project: PathBuf,
    /// Customize the output format (defaults to human-readable)
    #[arg(long, value_name = "FORMAT")]
    pub format: Option<Format>,
    /// Also eqwalize opted-in generated modules from application (deprecated)
    #[arg(long, hide = true)]
    pub include_generated: bool,
    /// Use a persistent daemon for fast turnaround (auto-starts if needed)
    #[arg(long)]
    pub connect: bool,
    /// Exit with a non-zero status code if any errors are found
    #[arg(long)]
    pub bail_on_error: bool,
    /// target, like //erl/chatd/...
    #[arg(value_name = "TARGET")]
    pub target: String,
}

#[derive(Clone, Debug, clap::Args)]
pub struct EqwalizeApp {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::AnyPath)]
    pub project: PathBuf,
    /// Rebar3 profile to pickup
    #[arg(long = "as", value_name = "PROFILE", default_value = "test")]
    pub profile: String,
    /// Customize the output format (defaults to human-readable)
    #[arg(long, value_name = "FORMAT")]
    pub format: Option<Format>,
    /// Also eqwalize opted-in generated modules from project (deprecated)
    #[arg(long, hide = true)]
    pub include_generated: bool,
    /// Run with rebar
    #[arg(long)]
    pub rebar: bool,
    /// Use a persistent daemon for fast turnaround (auto-starts if needed)
    #[arg(long)]
    pub connect: bool,
    /// Exit with a non-zero status code if any errors are found
    #[arg(long)]
    pub bail_on_error: bool,
    /// app name
    #[arg(value_name = "APP")]
    pub app: String,
}

#[derive(Clone, Debug, clap::Args)]
pub struct EqwalizeStats {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::AnyPath)]
    pub project: PathBuf,
    /// Rebar3 profile to pickup
    #[arg(long = "as", value_name = "PROFILE", default_value = "test")]
    pub profile: String,
    /// Run with rebar
    #[arg(long)]
    pub rebar: bool,
    /// Also eqwalize opted-in generated modules from project (deprecated)
    #[arg(long, hide = true)]
    pub include_generated: bool,
    /// If specified, use the provided CLI severity mapping instead of the default one
    #[arg(long)]
    pub use_cli_severity: bool,
}

#[derive(Clone, Debug, clap::Args)]
pub struct DialyzeAll {}

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

/// `elp lint-compare` -- pure data primitive that takes two
/// `elp lint --format=json` JSONL artifacts and reports the
/// per-(`name`, `severity`) diagnostic count delta between them.
///
/// No project is loaded, no lint is run -- this command operates
/// purely on the two input files. The expected workflow is:
///   1. `elp lint --format=json > before.jsonl` (at base revision)
///   2. `elp lint --format=json > after.jsonl`  (at diff revision)
///   3. `elp lint-compare --base before.jsonl --diff after.jsonl --report-path report.md`
///
/// Step 3 is the same primitive whether the caller is CI comparing
/// two diff revisions, a developer checking what they changed
/// locally, or a tool comparing two ELP releases. The compare step
/// has zero project dependencies (no BUCK, no OTP, no source tree).
#[derive(Debug, Clone, Default, clap::Args)]
pub struct LintCompare {
    /// JSONL artifact for the **base** side of the comparison
    /// (typically an `elp lint --format=json` run at a prior project
    /// state). Diagnostics present here but missing on the diff side
    /// are reported as removed.
    #[arg(long, value_name = "BASE_JSONL", value_hint = ValueHint::FilePath)]
    pub base: PathBuf,

    /// JSONL artifact for the **diff** side of the comparison
    /// (typically an `elp lint --format=json` run at the current
    /// project state). Diagnostics present here but missing on the
    /// base side are reported as new.
    #[arg(long, value_name = "DIFF_JSONL", value_hint = ValueHint::FilePath)]
    pub diff: PathBuf,

    /// Optional path to write the Markdown comparison report
    /// (per-(diagnostic, severity) summary table) to.
    /// When omitted, the report is written to stdout.
    #[arg(long, value_name = "REPORT_PATH", value_hint = ValueHint::FilePath)]
    pub report_path: Option<PathBuf>,
}

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

#[derive(Clone, Debug, clap::Args)]
pub struct Ssr {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::AnyPath)]
    pub project: PathBuf,
    /// Parse a single module from the project, not the entire project.
    #[arg(long, value_name = "MODULE", add = ArgValueCompleter::new(module_completer))]
    pub module: Option<String>,
    /// Parse a single application from the project, not the entire project.
    #[arg(long = "app", alias = "application", value_name = "APP")]
    pub app: Option<String>,
    /// Parse a single file from the project, not the entire project. This can be an include file or escript, etc.
    #[arg(long, value_name = "FILE")]
    pub file: Option<String>,

    /// Run with rebar
    #[arg(long)]
    pub rebar: bool,
    /// Rebar3 profile to pickup
    #[arg(long = "as", value_name = "PROFILE", default_value = "test")]
    pub profile: String,

    /// Also generate diagnostics for generated files
    #[arg(long)]
    pub include_generated: bool,
    /// Deprecated: has no effect, will be removed in future
    #[arg(long)]
    pub include_tests: bool,

    /// Customize the output format (defaults to human-readable)
    #[arg(long, value_name = "FORMAT")]
    pub format: Option<Format>,

    /// Macro expansion strategy (default: expand)
    #[arg(long = "macros", value_name = "STRATEGY")]
    pub macro_strategy: Option<MacroStrategyArg>,

    /// Explicitly match parentheses. If omitted, they are ignored.
    #[arg(long = "parens")]
    pub paren_strategy: bool,

    /// Dump a configuration snippet that can be put in .elp_lint.toml to match the given SSR patterns
    #[arg(long)]
    pub dump_config: bool,

    /// Show source code context for matches
    #[arg(long = "show-source")]
    pub show_source: bool,

    /// Print NUM lines of leading context, enables --show-source
    #[arg(short = 'B', long = "before-context", value_name = "NUM")]
    pub before_context: Option<usize>,

    /// Print NUM lines of trailing context, enables --show-source
    #[arg(short = 'A', long = "after-context", value_name = "NUM")]
    pub after_context: Option<usize>,

    /// Print NUM lines of output context, enables --show-source
    #[arg(short = 'C', long = "context", value_name = "NUM")]
    pub context: Option<usize>,

    /// Print SEP on line between matches with context, enables --show-source
    #[arg(long = "group-separator", value_name = "SEP")]
    pub group_separator: Option<String>,

    /// Do not print separator for matches with context, enables --show-source
    #[arg(long = "no-group-separator")]
    pub no_group_separator: bool,

    /// Report system memory usage and other statistics
    #[arg(long = "report-system-stats")]
    pub report_system_stats: bool,

    /// SSR specs to use. Each accepts `PATTERN`, `ssr: PATTERN.`, or
    /// `LABEL:ssr: PATTERN.` forms; the LABEL surfaces as `patternLabel` in
    /// JSON output.
    #[arg(value_name = "SSR_SPECS", required = true)]
    pub ssr_specs: Vec<String>,
}

#[derive(Clone, Debug, clap::Args)]
pub struct Explain {
    /// Error code to explain
    #[arg(long, value_name = "CODE", add = ArgValueCandidates::new(diagnostic_code_candidates))]
    pub code: String,
}

#[derive(Clone, Debug, clap::Args)]
pub struct LintList {}

#[derive(Clone, Debug, clap::Args)]
pub struct Shell {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::AnyPath)]
    pub project: PathBuf,
    /// Initial command to run on shell startup
    #[arg(value_name = "INITIAL_COMMAND")]
    pub command: Vec<String>,
}

#[derive(Clone, Debug)]
pub enum DaemonCommand {
    Run(DaemonRun),
    Stop,
    Status(DaemonStatus),
}

#[derive(Clone, Debug, clap::Args)]
pub struct DaemonRun {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::AnyPath)]
    pub project: PathBuf,
    /// Rebar3 profile to pickup
    #[arg(long = "as", value_name = "PROFILE", default_value = "test")]
    pub profile: String,
    /// Run with rebar
    #[arg(long)]
    pub rebar: bool,
    /// Detach from parent process and run as daemon (internal use only)
    #[arg(long, hide = true)]
    pub daemonize: bool,
}

#[derive(Clone, Debug, clap::Args)]
pub struct DaemonStatus {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::AnyPath)]
    pub project: PathBuf,
    /// Rebar3 profile to pickup
    #[arg(long = "as", value_name = "PROFILE", default_value = "test")]
    pub profile: String,
    /// Run with rebar
    #[arg(long)]
    pub rebar: bool,
}

#[derive(Clone, Debug, clap::Subcommand)]
pub enum DaemonSubcommand {
    /// Stop all running daemons
    Stop,
    /// Show daemon status for this project
    Status(DaemonStatus),
}

#[derive(Clone, Debug, clap::Args)]
#[command(about = "Manage a persistent ELP daemon for fast turnaround")]
pub struct Daemon {
    #[command(subcommand)]
    pub subcommand: Option<DaemonSubcommand>,

    #[command(flatten)]
    pub run_args: DaemonRun,
}

impl Daemon {
    pub fn into_command(self) -> DaemonCommand {
        match self.subcommand {
            Some(DaemonSubcommand::Stop) => DaemonCommand::Stop,
            Some(DaemonSubcommand::Status(s)) => DaemonCommand::Status(s),
            None => DaemonCommand::Run(self.run_args),
        }
    }
}

#[derive(Clone, Debug, clap::Args)]
pub struct Glean {
    /// Path to directory with project, or to a JSON file
    #[arg(long, value_name = "PROJECT", default_value = ".", value_hint = ValueHint::AnyPath)]
    pub project: PathBuf,
    #[arg(long, value_name = "MODULE", add = ArgValueCompleter::new(module_completer))]
    pub module: Option<String>,
    /// Path to a directory where to dump result
    #[arg(long, value_name = "TO", value_hint = ValueHint::DirPath)]
    pub to: Option<PathBuf>,
    /// Deprecated no-op.
    #[arg(long)]
    pub schema2: bool,
    /// Pretty print
    #[arg(long)]
    pub pretty: bool,
    /// Output each fact separately
    #[arg(long)]
    pub multi: bool,
    /// Print indexer metrics as JSON
    #[arg(long)]
    pub print_metrics: bool,
    /// Prefix for every emitted `src.File` path
    #[arg(long, value_name = "PATH")]
    pub source_root: Option<String>,
}

#[derive(Clone, Debug, clap::Args)]
pub struct ConfigStanza {}

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

fn parse_macro_strategy(macro_strategy: Option<MacroStrategyArg>) -> MacroStrategy {
    match macro_strategy {
        Some(MacroStrategyArg::NoExpand) => MacroStrategy::DoNotExpand,
        Some(MacroStrategyArg::Expand) | None => MacroStrategy::Expand,
        Some(MacroStrategyArg::VisibleExpand) => MacroStrategy::ExpandButIncludeMacroCall,
    }
}

impl Ssr {
    pub fn is_format_normal(&self) -> bool {
        self.format.is_none()
    }

    pub fn is_format_json(&self) -> bool {
        self.format == Some(Format::Json)
    }

    pub fn parse_strategy(&self) -> Result<Strategy> {
        let macros = parse_macro_strategy(self.macro_strategy);
        let parens = if self.paren_strategy {
            ParenStrategy::VisibleParens
        } else {
            ParenStrategy::InvisibleParens
        };
        Ok(Strategy { macros, parens })
    }
}

impl ParseAllElp {
    pub fn is_format_normal(&self) -> bool {
        self.format.is_none()
    }

    pub fn is_format_json(&self) -> bool {
        self.format == Some(Format::Json)
    }
}

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
