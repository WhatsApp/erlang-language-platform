/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Ordering;
use std::env;
use std::fs;
use std::path::PathBuf;

use bpaf::construct;
use bpaf::long;
use bpaf::Bpaf;
use bpaf::Parser;
use elp_project_model::buck::BuckQueryConfig;
use elp_project_model::buck::BuildGeneratedCode;
use itertools::Itertools;
use serde::Deserialize;

use crate::args::Command::Help;

#[derive(Clone, Debug, Bpaf)]
pub struct ParseAllElp {
    /// Path to directory with project, or to a JSON file (defaults to `.`)
    #[bpaf(argument("PROJECT"), fallback(PathBuf::from(".")))]
    pub project: PathBuf,
    /// Parse a single module from the project, not the entire project
    #[bpaf(argument("MODULE"), complete(module_completer), optional)]
    pub module: Option<String>,
    /// Parse a single file from the project, not the entire project. \nThis can be an include file or escript, etc.
    pub file: Option<String>,
    /// Path to a directory where to dump result files
    #[bpaf(argument("TO"))]
    pub to: Option<PathBuf>,
    #[bpaf(external(parse_print_diags))]
    pub print_diags: bool,
    #[bpaf(external(parse_experimental_diags))]
    pub experimental_diags: bool,
    /// Rebar3 profile to pickup (default is test)
    #[bpaf(long("as"), argument("PROFILE"), fallback("test".to_string()))]
    pub profile: String,
    /// Report the resolution of include directives for comparison with OTP ones
    #[bpaf(long("dump-includes"))]
    pub dump_include_resolutions: bool,
    /// Run with rebar
    pub rebar: bool,
    /// Also process generated modules
    pub include_generated: bool,
    /// Parse the files serially, not in parallel
    pub serial: bool,
    /// Show diagnostics in JSON format
    #[bpaf(
        argument("FORMAT"),
        complete(format_completer),
        fallback(None),
        guard(format_guard, "Please use json")
    )]
    pub format: Option<String>,
}

#[derive(Clone, Debug, Bpaf)]
pub struct ParseAll {
    /// Path to directory with project, or to a JSON file (defaults to `.`)
    #[bpaf(argument("PROJECT"), fallback(PathBuf::from(".")))]
    pub project: PathBuf,
    /// Path to a directory where to dump .etf files
    pub to: PathBuf,
    /// Rebar3 profile to pickup (default is test)
    #[bpaf(long("as"), argument("PROFILE"), fallback("test".to_string()))]
    pub profile: String,
    /// Parse a single module from the project, not the entire project
    #[bpaf(argument("MODULE"), complete(module_completer), optional)]
    pub module: Option<String>,
    /// Run with buck
    pub buck: bool,
    /// Print statistics when done
    pub stats: bool,
    /// When printing statistics, include the list of modules parsed
    pub list_modules: bool,
}

#[derive(Clone, Debug, Bpaf)]
pub struct Eqwalize {
    /// Path to directory with project, or to a JSON file (defaults to `.`)
    #[bpaf(argument("PROJECT"), fallback(PathBuf::from(".")))]
    pub project: PathBuf,
    /// Rebar3 profile to pickup (default is test)
    #[bpaf(long("as"), argument("PROFILE"), fallback("test".to_string()))]
    pub profile: String,
    /// Show diagnostics in JSON format
    #[bpaf(
        argument("FORMAT"),
        complete(format_completer),
        fallback(None),
        guard(format_guard, "Please use json")
    )]
    pub format: Option<String>,
    /// Run with rebar
    pub rebar: bool,
    /// Exit with a non-zero status code if any errors are found
    pub bail_on_error: bool,
    /// Eqwalize specified modules
    #[bpaf(
        positional("MODULES"),
        guard(at_least_1, "there should be at least one module")
    )]
    pub modules: Vec<String>,
}

#[derive(Clone, Debug, Bpaf)]
pub struct EqwalizeAll {
    /// Path to directory with project, or to a JSON file (defaults to `.`)
    #[bpaf(argument("PROJECT"), fallback(PathBuf::from(".")))]
    pub project: PathBuf,
    /// Rebar3 profile to pickup (default is test)
    #[bpaf(long("as"), argument("PROFILE"), fallback("test".to_string()))]
    pub profile: String,
    /// Show diagnostics in JSON format
    #[bpaf(
        argument("FORMAT"),
        complete(format_completer),
        fallback(None),
        guard(format_guard, "Please use json")
    )]
    pub format: Option<String>,
    /// Run with rebar
    pub rebar: bool,
    /// Also eqwalize opted-in generated modules from project
    pub include_generated: bool,
    /// Exit with a non-zero status code if any errors are found
    pub bail_on_error: bool,
    /// Print statistics when done
    pub stats: bool,
    /// When printing statistics, include the list of modules parsed
    pub list_modules: bool,
}

#[derive(Clone, Debug, Bpaf)]
pub struct EqwalizeTarget {
    /// Path to directory with project, or to a JSON file (defaults to `.`)
    #[bpaf(argument("PROJECT"), fallback(PathBuf::from(".")))]
    pub project: PathBuf,
    /// Also eqwalize opted-in generated modules from application
    pub include_generated: bool,
    /// Exit with a non-zero status code if any errors are found
    pub bail_on_error: bool,
    /// target, like //erl/chatd/...
    #[bpaf(positional("TARGET"))]
    pub target: String,
}

#[derive(Clone, Debug, Bpaf)]
pub struct EqwalizeApp {
    /// Path to directory with project, or to a JSON file (defaults to `.`)
    #[bpaf(argument("PROJECT"), fallback(PathBuf::from(".")))]
    pub project: PathBuf,
    /// Rebar3 profile to pickup (default is test)
    #[bpaf(long("as"), argument("PROFILE"), fallback("test".to_string()))]
    pub profile: String,
    /// Also eqwalize opted-in generated modules from project
    pub include_generated: bool,
    /// Run with rebar
    pub rebar: bool,
    /// Exit with a non-zero status code if any errors are found
    pub bail_on_error: bool,
    /// app name
    #[bpaf(positional::< String > ("APP"))]
    pub app: String,
}

#[derive(Clone, Debug, Bpaf)]
pub struct EqwalizeStats {
    /// Path to directory with project, or to a JSON file (defaults to `.`)
    #[bpaf(argument("PROJECT"), fallback(PathBuf::from(".")))]
    pub project: PathBuf,
    /// Rebar3 profile to pickup (default is test)
    #[bpaf(long("as"), argument("PROFILE"), fallback("test".to_string()))]
    pub profile: String,
    /// Run with rebar
    pub rebar: bool,
    /// Also eqwalize opted-in generated modules from project
    pub include_generated: bool,
}

#[derive(Clone, Debug, Bpaf)]
pub struct DialyzeAll {}

#[derive(Clone, Debug, Bpaf)]
pub struct BuildInfo {
    /// Path to directory with project, or to a JSON file (defaults to `.`)
    #[bpaf(argument("PROJECT"), fallback(PathBuf::from(".")))]
    pub project: PathBuf,
    /// Path to a (JSON) file to write the build information
    #[bpaf(argument("TO"))]
    pub to: PathBuf,
}

#[derive(Clone, Debug, Bpaf)]
pub struct GenerateCompletions {
    #[bpaf(positional::< String > ("shell"), complete(shell_completer), guard(shell_guard, "Please use bash|zsh|fish"))]
    /// bash, zsh or fish
    pub shell: String,
}

#[derive(Clone, Debug, Bpaf)]
pub struct RunServer {}

#[derive(Clone, Debug, Bpaf)]
pub struct Version {}

#[derive(Debug, Clone, Bpaf)]
pub struct Lint {
    /// Path to directory with project, or to a JSON file (defaults to `.`)
    #[bpaf(argument("PROJECT"), fallback(PathBuf::from(".")))]
    pub project: PathBuf,
    /// Parse a single module from the project, not the entire project.
    #[bpaf(argument("MODULE"))]
    pub module: Option<String>,
    /// Parse a single file from the project, not the entire project. This can be an include file or escript, etc.
    #[bpaf(argument("FILE"))]
    pub file: Option<String>,
    /// Path to a directory where to dump result files
    #[bpaf(argument("TO"))]
    pub to: Option<PathBuf>,
    /// Do not print the full diagnostics for a file, just the count
    #[bpaf(external(parse_print_diags))]
    pub print_diags: bool,
    #[bpaf(external(parse_experimental_diags))]
    pub experimental_diags: bool,
    /// Rebar3 profile to pickup (default is test)
    #[bpaf(long("as"), argument("PROFILE"), fallback("test".to_string()))]
    pub profile: String,
    /// Show diagnostics in JSON format
    #[bpaf(
        argument("FORMAT"),
        complete(format_completer),
        fallback(None),
        guard(format_guard, "Please use json")
    )]
    pub format: Option<String>,
    /// Run with rebar
    pub rebar: bool,
    pub include_generated: bool,
    /// Include diagnostics produced by erlc
    pub include_erlc_diagnostics: bool,
    /// Include Common Test diagnostics
    pub include_ct_diagnostics: bool,
    /// Include EDoc diagnostics
    pub include_edoc_diagnostics: bool,
    /// Include Eqwalizer diagnostics
    pub include_eqwalizer_diagnostics: bool,
    /// Include Suppressed diagnostics (e.g. elp:fixme)
    pub include_suppressed: bool,
    /// Also generate diagnostics for test files
    pub include_tests: bool,
    /// If the diagnostic has an associated fix, apply it. The modified file will be in the --to directory, or original file if --in-place is set.
    pub apply_fix: bool,
    /// If applying fixes, apply any new ones that arise from the
    /// prior fixes recursively. Limited in scope to the clause of the
    /// prior change.
    pub recursive: bool,
    /// When applying a fix, modify the original file.
    pub in_place: bool,
    /// After applying a fix step, check that the diagnostics are clear, else roll back
    pub with_check: bool,
    /// After applying a fix step, check that all eqwalizer project diagnostics are clear, else roll back
    pub check_eqwalize_all: bool,
    /// Apply to all matching diagnostic occurrences at once, rather
    /// than one at a time.
    pub one_shot: bool,
    /// Optional prefix to prepend to each fact. Only used when --format=json is set
    pub prefix: Option<String>,
    /// Ignore the specified diagnostic, by code or label
    #[bpaf(argument("CODE"))]
    pub diagnostic_ignore: Option<String>,
    /// Filter out all reported diagnostics except this one, by code or label
    #[bpaf(argument("CODE"))]
    pub diagnostic_filter: Option<String>,
    /// Only apply elp:ignore fixes
    pub ignore_fix_only: bool,
    /// Get some configuration from a .elp_lint.toml file instead in the project root
    pub read_config: bool,
    /// Override normal configuration file. When set, acts as if READ_CONFIG is true.
    #[bpaf(argument("CONFIG_FILE"))]
    pub config_file: Option<String>,
    /// Rest of args are space separated list of apps to ignore
    #[bpaf(positional("IGNORED_APPS"))]
    pub ignore_apps: Vec<String>,
}

#[derive(Clone, Debug, Bpaf)]
pub struct Explain {
    /// Error code to explain
    #[bpaf(argument("CODE"))]
    pub code: String,
}

#[derive(Clone, Debug, Bpaf)]
pub struct Shell {
    /// Path to directory with project, or to a JSON file (defaults to `.`)
    #[bpaf(argument("PROJECT"), fallback(PathBuf::from(".")))]
    pub project: PathBuf,
}

#[derive(Clone, Debug, Bpaf)]
pub struct ProjectInfo {
    /// Path to directory with project, or to a JSON file (defaults to `.`)
    #[bpaf(argument("PROJECT"), fallback(PathBuf::from(".")))]
    pub project: PathBuf,
    /// Path to a directory where to dump wa.build_info
    #[bpaf(argument("TO"))]
    pub to: Option<PathBuf>,
    /// Include the buck uquery results in the output
    pub buck_query: bool,
}

#[derive(Clone, Debug, Bpaf)]
pub struct Glean {
    /// Path to directory with project, or to a JSON file (defaults to `.`)
    #[bpaf(argument("PROJECT"), fallback(PathBuf::from(".")))]
    pub project: PathBuf,
    #[bpaf(argument("MODULE"))]
    pub module: Option<String>,
    /// Path to a directory where to dump result
    #[bpaf(argument("TO"))]
    pub to: Option<PathBuf>,
    /// Produce glean db with macros, types, xrefs. Incompatible with previous
    pub v2: bool,
    /// Pretty print
    pub pretty: bool,
    /// Output each fact separately
    pub multi: bool,
    /// Optional prefix to prepend to each fact
    pub prefix: Option<String>,
}

#[derive(Clone, Debug, Bpaf)]
pub struct ConfigStanza {}

#[derive(Clone, Debug)]
pub enum Command {
    ParseAllElp(ParseAllElp),
    ParseAll(ParseAll),
    Eqwalize(Eqwalize),
    EqwalizeAll(EqwalizeAll),
    EqwalizeTarget(EqwalizeTarget),
    EqwalizeApp(EqwalizeApp),
    EqwalizeStats(EqwalizeStats),
    DialyzeAll(DialyzeAll),
    BuildInfo(BuildInfo),
    GenerateCompletions(GenerateCompletions),
    RunServer(RunServer),
    Lint(Lint),
    Version(Version),
    Shell(Shell),
    Explain(Explain),
    ProjectInfo(ProjectInfo),
    Glean(Glean),
    ConfigStanza(ConfigStanza),
    Help(),
}

#[derive(Debug, Clone, Bpaf)]
#[bpaf(options)]
pub struct Args {
    #[bpaf(argument("LOG_FILE"))]
    pub log_file: Option<PathBuf>,
    #[bpaf(argument("ERL"))]
    pub erl: Option<PathBuf>,
    #[bpaf(argument("ESCRIPT"))]
    pub escript: Option<PathBuf>,
    pub no_log_buffering: bool,

    /// Use BXL when querying for a buck project model
    pub buck_bxl: bool,

    /// When using BXL to query for a buck project model, invoke a
    /// build step for generated files. If present, forces `--buck-bxl`.
    pub buck_generated: bool,

    #[bpaf(external(command))]
    pub command: Command,
}

impl Args {
    pub fn query_config(&self) -> BuckQueryConfig {
        if self.buck_generated {
            // buck forces BXL mode
            BuckQueryConfig::Bxl(BuildGeneratedCode::Yes)
        } else if self.buck_bxl {
            BuckQueryConfig::Bxl(BuildGeneratedCode::No)
        } else {
            BuckQueryConfig::Original
        }
    }
}

pub fn command() -> impl Parser<Command> {
    let parse_elp = parse_all_elp()
        .map(Command::ParseAllElp)
        .to_options()
        .command("parse-elp")
        .help("Tree-sitter parse all files in a project for specified rebar.config file");

    let parse_all = parse_all()
        .map(Command::ParseAll)
        .to_options()
        .command("parse-all")
        .help("Dump ast for all files in a project for specified rebar.config file");

    let eqwalize = eqwalize()
        .map(Command::Eqwalize)
        .to_options()
        .command("eqwalize")
        .help("Eqwalize specified module");

    let eqwalize_all = eqwalize_all()
        .map(Command::EqwalizeAll)
        .to_options()
        .command("eqwalize-all")
        .help("Eqwalize all opted-in modules in a project");

    let eqwalize_target = eqwalize_target()
        .map(Command::EqwalizeTarget)
        .to_options()
        .command("eqwalize-target")
        .help("Eqwalize all opted-in modules in specified buck target");

    let eqwalize_app = eqwalize_app()
        .map(Command::EqwalizeApp)
        .to_options()
        .command("eqwalize-app")
        .help("Eqwalize all opted-in modules in specified application");

    let eqwalize_stats = eqwalize_stats()
        .map(Command::EqwalizeStats)
        .to_options()
        .command("eqwalize-stats")
        .help("Return statistics about code quality for eqWAlizer");

    let dialyze_all = dialyze_all()
        .map(Command::DialyzeAll)
        .to_options()
        .command("dialyze-all")
        .help("Run Dialyzer on the whole project by shelling out to a `dialyzer-run` tool on the path to do the legwork.")
        .hide_usage();

    let build_info = build_info()
        .map(Command::BuildInfo)
        .to_options()
        .command("build-info")
        .help("Generate build info JSON file");

    let generate_completions = generate_completions()
        .map(Command::GenerateCompletions)
        .to_options()
        .command("generate-completions")
        .help("Generate shell completions");

    let lint = lint()
        .map(Command::Lint)
        .to_options()
        .command("lint")
        .help("Parse files in project and emit diagnostics, optionally apply fixes.");

    let run_server = run_server()
        .map(Command::RunServer)
        .to_options()
        .command("server")
        .help("Run lsp server");

    let version = version()
        .map(Command::Version)
        .to_options()
        .command("version")
        .help("Print version");

    let shell = shell()
        .map(Command::Shell)
        .to_options()
        .command("shell")
        .help("Starts an interactive ELP shell");

    let explain = explain()
        .map(Command::Explain)
        .to_options()
        .command("explain")
        .help("Explain a diagnostic code");

    let project_info = project_info()
        .map(Command::ProjectInfo)
        .to_options()
        .command("project-info")
        .help("Generate project info file");

    let glean = glean()
        .map(Command::Glean)
        .to_options()
        .command("glean")
        .help("Glean indexer");

    let config_stanza = config_stanza()
        .map(Command::ConfigStanza)
        .to_options()
        .command("config")
        .help("Dump a JSON config stanza suitable for use in VS Code project.json");

    construct!([
        eqwalize,
        eqwalize_all,
        eqwalize_app,
        eqwalize_target,
        dialyze_all,
        lint,
        run_server,
        generate_completions,
        parse_all,
        parse_elp,
        build_info,
        version,
        shell,
        eqwalize_stats,
        explain,
        project_info,
        glean,
        config_stanza,
    ])
    .fallback(Help())
}

fn parse_print_diags() -> impl Parser<bool> {
    long("no-diags")
        .help("Do not print the full diagnostics for a file, just the count")
        .flag(/* present */ false, true)
}

fn parse_experimental_diags() -> impl Parser<bool> {
    long("experimental")
        .help("Report experimental diagnostics too, if diagnostics are enabled")
        .flag(/* present */ true, false)
}

#[derive(Deserialize)]
struct ModuleConfig {
    modules: Vec<String>,
}

const MODULES_FILE: &str = ".modules.toml";

#[allow(clippy::ptr_arg)]
fn module_completer(input: &String) -> Vec<(String, Option<String>)> {
    let mut modules = vec![];
    let curr = env::current_dir().unwrap();
    let mut potential_path = Some(curr.as_path());
    while let Some(path) = potential_path {
        let file_path = path.join(MODULES_FILE);

        if !file_path.is_file() {
            potential_path = path.parent();
            continue;
        } else {
            if let Ok(content) = fs::read_to_string(file_path) {
                if let Ok(config) = toml::from_str::<ModuleConfig>(&content) {
                    for module_name in config.modules.into_iter() {
                        modules.push(module_name)
                    }
                }
            }
            break;
        }
    }
    get_suggesions(input, modules)
}

fn format_completer(_: &Option<String>) -> Vec<(String, Option<String>)> {
    vec![("json".to_string(), None)]
}

fn format_guard(format: &Option<String>) -> bool {
    match format {
        None => true,
        Some(f) if f == "json" => true,
        _ => false,
    }
}

fn at_least_1(data: &Vec<String>) -> bool {
    data.len() >= 1
}

#[allow(clippy::ptr_arg)]
fn shell_completer(shell: &String) -> Vec<(String, Option<String>)> {
    let completions = match shell.to_lowercase().chars().next() {
        Some('b') => vec!["bash"],
        Some('f') => vec!["fish"],
        Some('z') => vec!["zsh"],
        _ => vec!["bash", "fish", "zsh"],
    };
    completions
        .into_iter()
        .map(|sh| (sh.into(), None))
        .collect()
}

#[allow(clippy::match_like_matches_macro)]
fn shell_guard(shell: &String) -> bool {
    match shell.as_ref() {
        "bash" => true,
        "zsh" => true,
        "fish" => true,
        _ => false,
    }
}

fn get_suggesions(input: &str, modules: Vec<String>) -> Vec<(String, Option<String>)> {
    const MAX_RESULTS: usize = 10;

    modules
        .into_iter()
        .map(|key| (strsim::normalized_damerau_levenshtein(input, &key), key))
        .sorted_by(|a, b| PartialOrd::partial_cmp(&b.0, &a.0).unwrap_or(Ordering::Less))
        .take(MAX_RESULTS)
        .map(|(_lev, v)| (v, None))
        .collect()
}

#[cfg(target_os = "macos")]
pub fn gen_completions(shell: &str) -> &str {
    match shell {
        "bash" => "elp --bpaf-complete-style-bash",
        "zsh" => {
            "elp --bpaf-complete-style-zsh | sudo dd of=/usr/local/share/zsh/site-functions/_elp && echo 'autoload -U compinit; compinit ' >> ~/.zshrc && zsh"
        }
        "fish" => "elp --bpaf-complete-style-fish > ~/.config/fish/completions/elp.fish",
        _ => unreachable!(),
    }
}

#[cfg(target_os = "linux")]
pub fn gen_completions(shell: &str) -> &str {
    match shell {
        "bash" => {
            "elp --bpaf-complete-style-bash | sudo dd of=/usr/share/bash-completion/completions/elp && bash"
        }
        "zsh" => {
            "elp --bpaf-complete-style-zsh | sudo dd of=/usr/share/zsh/site-functions/_elp && echo 'autoload -U compinit; compinit ' >> ~/.zshrc && zsh"
        }
        "fish" => "elp --bpaf-complete-style-fish > ~/.config/fish/completions/elp.fish",
        _ => unreachable!(),
    }
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
pub fn gen_completions(shell: &str) -> String {
    format!("elp --bpaf-complete-style-{}", shell)
}

impl Lint {
    pub fn is_format_normal(&self) -> bool {
        self.format.is_none()
    }

    pub fn is_format_json(&self) -> bool {
        self.format == Some("json".to_string())
    }
}

impl ParseAllElp {
    pub fn is_format_normal(&self) -> bool {
        self.format.is_none()
    }

    pub fn is_format_json(&self) -> bool {
        self.format == Some("json".to_string())
    }
}

#[cfg(test)]
mod tests {

    use bpaf::Parser;

    use super::command;

    #[test]
    fn check_options() {
        command().to_options().check_invariants(false)
    }
}
