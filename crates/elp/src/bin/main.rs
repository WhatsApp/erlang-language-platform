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
use std::fs;
use std::io::IsTerminal;
use std::io::Write;
use std::path::PathBuf;
use std::process;
use std::sync::Once;

use anyhow::Result;
use bpaf::batteries;
use elp::ServerSetup;
use elp::cli;
use elp::cli::Cli;
use elp_ide::erlang_service::ESCRIPT;
use elp_log::FileLogger;
use elp_log::Logger;
use elp_log::timeit;
use elp_project_model::eqwalizer_support;
use elp_project_model::otp::ERL;
use include_dir::Dir;
use include_dir::include_dir;
use lsp_server::Connection;

mod args;
mod build_info_cli;
mod config_stanza;
mod dialyzer_cli;
mod elp_parse_cli;
mod eqwalizer_cli;
mod erlang_service_cli;
mod explain_cli;
mod glean;
mod lint_cli;
// @fb-only
mod reporting;
mod shell;
mod ssr_cli;

// Use jemalloc as the global allocator
#[cfg(not(any(target_env = "msvc", target_os = "openbsd")))]
use jemallocator::Jemalloc;

use crate::args::Args;

#[cfg(not(any(target_env = "msvc", target_os = "openbsd")))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;
static EQWALIZER_SUPPORT_DIR: Dir = include_dir!("$EQWALIZER_SUPPORT_DIR");
static INIT: Once = Once::new();

/// Thread stack size for rayon, in bytes.
///
/// Due to inefficient encoding of lists, the default stack size of 2MiB may not be
/// enough to parse some generated modules, and eqWAlizer may stack overflow.
const THREAD_STACK_SIZE: usize = 10_000_000;

fn main() {
    let _timer = timeit!("main");
    let args = args::args().run();
    let use_color = match args.color.as_deref() {
        Some("always") => true,
        Some("never") => false,
        Some("auto") | None => {
            // Check NO_COLOR environment variable - if set (regardless of value), disable color
            // Also check if stdout is connected to a TTY
            env::var("NO_COLOR").is_err() && std::io::stdout().is_terminal()
        }
        _ => false, // Should be caught by the guard, but handle anyway
    };
    let mut cli: Box<dyn cli::Cli> = if use_color {
        Box::new(cli::Real::default())
    } else {
        Box::new(cli::NoColor::default())
    };
    let res = try_main(&mut *cli, args);
    let code = handle_res(res, cli.err());
    process::exit(code);
}

fn handle_res(result: Result<()>, stderr: &mut dyn Write) -> i32 {
    if let Err(err) = result {
        writeln!(stderr, "{err:#}").unwrap();
        101
    } else {
        0
    }
}

fn setup_static(args: &Args) {
    if let Err(err) = eqwalizer_support::setup_eqwalizer_support(&EQWALIZER_SUPPORT_DIR) {
        log::warn!("Failed to setup eqwalizer_support: {err}");
    }
    if let Some(erl) = &args.erl {
        let path = fs::canonicalize(erl).expect("erl path should be valid");
        let mut erl = ERL.write().unwrap();
        *erl = path.to_string_lossy().to_string();
    }

    if let Some(escript) = &args.escript {
        let path = fs::canonicalize(escript).expect("escript path should be valid");
        let mut escript = ESCRIPT.write().unwrap();
        *escript = path.to_string_lossy().to_string();
    }
}

fn setup_cli_telemetry(args: &Args) {
    match &args.command {
        args::Command::RunServer(_) => {
            // Do nothing, we have server telemetry
        }
        _ => {
            // Initialize CLI telemetry, if used
            // @fb-only
        }
    }
}

fn try_main(cli: &mut dyn Cli, args: Args) -> Result<()> {
    let logger = setup_logging(&args.log_file, args.no_log_buffering)?;
    setup_cli_telemetry(&args);

    INIT.call_once(|| {
        setup_static(&args);
        setup_thread_pool();
    });
    let query_config = args.query_config();
    match args.command {
        args::Command::RunServer(_) => run_server(logger)?,
        args::Command::ParseAll(args) => erlang_service_cli::parse_all(&args, cli, &query_config)?,
        args::Command::ParseAllElp(args) => elp_parse_cli::parse_all(&args, cli, &query_config)?,
        args::Command::Eqwalize(args) => eqwalizer_cli::eqwalize_module(&args, cli, &query_config)?,
        args::Command::EqwalizeAll(args) => eqwalizer_cli::eqwalize_all(&args, cli, &query_config)?,
        args::Command::DialyzeAll(args) => dialyzer_cli::dialyze_all(&args, cli)?,
        args::Command::EqwalizeApp(args) => eqwalizer_cli::eqwalize_app(&args, cli, &query_config)?,
        args::Command::EqwalizeStats(args) => {
            eqwalizer_cli::eqwalize_stats(&args, cli, &query_config)?
        }
        args::Command::EqwalizeTarget(args) => {
            eqwalizer_cli::eqwalize_target(&args, cli, &query_config)?
        }
        args::Command::BuildInfo(args) => build_info_cli::save_build_info(args, &query_config)?,
        args::Command::ProjectInfo(args) => build_info_cli::save_project_info(args, &query_config)?,
        args::Command::Lint(args) => lint_cli::run_lint_command(&args, cli, &query_config)?,
        args::Command::Ssr(ssr_args) => {
            ssr_cli::run_ssr_command(&ssr_args, cli, &query_config, &args.color)?
        }
        args::Command::GenerateCompletions(args) => {
            let instructions = args::gen_completions(&args.shell);
            writeln!(cli, "#Please run this:\n{instructions}")?
        }
        args::Command::Version(_) => writeln!(cli, "elp {}", elp::version())?,
        args::Command::Shell(args) => shell::run_shell(&args, cli, &query_config)?,
        args::Command::Help() => {
            let help = batteries::get_usage(args::args());
            writeln!(cli, "{help}")?
        }
        args::Command::Explain(args) => explain_cli::explain(&args, cli)?,
        args::Command::Glean(args) => glean::index(&args, cli, &query_config)?,
        args::Command::ConfigStanza(args) => config_stanza::config_stanza(&args, cli)?,
    }

    log::logger().flush();

    Ok(())
}

fn setup_logging(log_file: &Option<PathBuf>, no_buffering: bool) -> Result<Logger> {
    // TODO: Audit that the environment access only happens in single-threaded code.
    unsafe { env::set_var("RUST_BACKTRACE", "short") };

    let log_file = match log_file {
        Some(path) => {
            if let Some(parent) = path.parent() {
                let _ = fs::create_dir_all(parent);
            }
            Some(fs::File::create(path)?)
        }
        None => None,
    };
    let filter = env::var("ELP_LOG").ok();
    let file_logger = FileLogger::new(log_file, no_buffering, filter.as_deref());

    let logger = Logger::default();
    logger.register_logger("default", Box::new(file_logger));
    logger.install();

    Ok(logger)
}

fn setup_thread_pool() {
    if let Err(err) = rayon::ThreadPoolBuilder::new()
        .stack_size(THREAD_STACK_SIZE)
        .build_global()
    {
        log::warn!("Failed to setup thread pool: {err}");
    }
}

fn run_server(logger: Logger) -> Result<()> {
    log::info!("server will start, pid: {}", process::id());
    let (connection, io_threads) = Connection::stdio();

    ServerSetup::new(connection, logger)
        .to_server()?
        .main_loop()?;

    io_threads.join()?;
    log::info!("server did shut down");

    Ok(())
}

// To run the tests
// cargo test --package elp --bin elp

#[cfg(test)]
mod tests {
    use std::env::current_dir;
    use std::ffi::OsString;
    use std::path::Path;
    use std::str;
    use std::sync::Arc;

    use anyhow::Context;
    use bpaf::Args;
    use elp::build;
    use elp::build::load;
    use elp::cli::Fake;
    use elp_eqwalizer::EqwalizerConfig;
    use elp_eqwalizer::EqwalizerDiagnostics;
    use elp_eqwalizer::Mode;
    use elp_eqwalizer::db::EqwalizerDiagnosticsDatabase;
    use elp_ide::elp_ide_db::diagnostic_code::BASE_URL;
    use elp_ide::elp_ide_db::elp_base_db::FileId;
    use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
    use elp_project_model::AppName;
    use elp_project_model::DiscoverConfig;
    use elp_project_model::buck::BuckConfig;
    use elp_project_model::buck::BuckQueryConfig;
    use elp_project_model::buck::get_prelude_cell;
    use elp_project_model::otp;
    use elp_project_model::otp::OTP_VERSION;
    use elp_project_model::otp::otp_supported_by_eqwalizer;
    use expect_test::Expect;
    use expect_test::ExpectFile;
    use expect_test::expect;
    use expect_test::expect_file;
    use lazy_static::lazy_static;
    use paths::Utf8PathBuf;
    use rayon::prelude::*;
    use regex::Regex;
    use tempfile::Builder;
    use tempfile::TempDir;
    use test_case::test_case;
    use vfs::AbsPathBuf;

    use super::reporting::Reporter;
    use super::*;

    const BUCK_QUERY_CONFIG: BuckQueryConfig = BuckQueryConfig::BuildGeneratedCode;

    macro_rules! args_vec {
        ($($e:expr$(,)?)+) => {
            vec![$(OsString::from($e),)+]
        }
    }

    fn elp(args: Vec<OsString>) -> (String, String, i32) {
        let mut cli = Fake::default();
        let args = Args::from(args.as_slice());
        let args = args::args().run_inner(args).unwrap();
        let res = try_main(&mut cli, args);
        let code = handle_res(res, cli.err());
        let (stdout, stderr) = cli.to_strings();
        (stdout, stderr, code)
    }

    fn make_tmp_dir() -> TempDir {
        TempDir::new().expect("Could not create temporary directory")
    }

    #[test]
    fn etf_files_from_dummy_project_are_generated() {
        // Create tmp dir for output, typically /tmp/elp_xxxxxx on unix.
        let tmp = Builder::new().prefix("elp_").tempdir().unwrap();
        let outdir = PathBuf::from(tmp.path());
        let (_stdout, stderr, code) = elp(args_vec![
            "parse-all",
            "--project",
            "../../test_projects/standard",
            "--to",
            tmp.path(),
        ]);
        // Now check .etf files were generated.
        let exists = |p| outdir.join(p).exists();
        assert!(exists("app_a.etf"));
        assert!(exists("app_a_SUITE.etf"));
        assert!(exists("app_a_mod2.etf"));
        assert!(exists("app_b.etf"));
        // The source file has a syntax error, so we don't create an etf
        assert!(!exists("parse_error_a_example_bad.etf"));
        assert_eq!(code, 0);
        assert!(stderr.is_empty());
    }

    fn parse_all_complete(project: &str) -> Result<i32> {
        // Just check the command returns.
        let project_path = format!("../../test_projects/{project}");
        let tmp = Builder::new().prefix("elp_parse_all_").tempdir().unwrap();
        let (_stdout, _stderr, code) = elp(args_vec![
            "parse-all",
            "--project",
            project_path,
            "--to",
            tmp.path(),
        ]);
        Ok(code)
    }

    fn eqwalize_snapshot(project: &str, module: &str, fast: bool, buck: bool) {
        eqwalize_snapshot_json(project, module, fast, buck, false);
    }

    fn eqwalize_snapshot_json(project: &str, module: &str, fast: bool, buck: bool, json: bool) {
        if !buck || cfg!(feature = "buck") {
            let mut args = args_vec!["eqwalize", module];
            if !buck {
                args.push("--rebar".into());
            }
            if json {
                args.push("--format".into());
                args.push("json".into());
            }
            let (args, path) = add_project(args, project, None, None);
            let fast_str = if fast { "_fast" } else { "" };
            let extension = if json { "jsonl" } else { "pretty" };
            let exp_path = expect_file!(format!(
                "../resources/test/{}/eqwalize_{}{}.{}",
                project, module, fast_str, extension
            ));

            let (stdout, stderr, code) = elp(args);
            match code {
                0 => {
                    assert_normalised_file(exp_path, &stdout, path, false);
                    assert!(stderr.is_empty());
                }
                _ => {
                    assert_normalised_file(exp_path, &stderr, path, false);
                    assert!(stdout.is_empty());
                }
            }
        }
    }

    // We can't run eqwalize_snapshot on all individual snapshots as running a new eqWAlizer
    // instance for each one is too costly.
    // This function is a simplified/inlined version of eqwalizer_cli::eqwalize_app,
    // with panics in case of failures, and checks eqWAlization results per module.
    pub fn eqwalize_all_snapshots(project: &str, app: &str, buck: bool, config: EqwalizerConfig) {
        if otp_supported_by_eqwalizer() && (!buck || cfg!(feature = "buck")) {
            let cli = Fake::default();
            let project_config = DiscoverConfig::new(!buck, "test");
            let str_path = project_path(project);
            let project_path: &Path = Path::new(&str_path);
            let mut loaded = load::load_project_at(
                &cli,
                project_path,
                project_config,
                IncludeOtp::Yes,
                Mode::Cli,
                &BUCK_QUERY_CONFIG,
            )
            .with_context(|| format!("Failed to load project at {str_path}"))
            .unwrap();
            loaded
                .analysis_host
                .raw_database_mut()
                .set_eqwalizer_config(Arc::new(config));
            build::compile_deps(&loaded, &cli)
                .with_context(|| format!("Failed to compile deps for project {project}"))
                .unwrap();

            let analysis = loaded.analysis();
            let module_index = analysis
                .module_index(loaded.project_id)
                .with_context(|| format!("No module index for project {project}"))
                .unwrap();
            let file_ids: Vec<FileId> = module_index
                .iter_own()
                .filter_map(|(_name, _source, file_id)| {
                    if analysis.file_app_name(file_id).ok()? == Some(AppName(app.into())) {
                        Some(file_id)
                    } else {
                        None
                    }
                })
                .collect();

            let files_count = file_ids.len();
            let project_id = loaded.project_id;
            // Use 4 instances for tests
            let chunk_size = files_count.div_ceil(4).max(1);
            let output = file_ids
                .clone()
                .chunks(chunk_size)
                .par_bridge()
                .map_with(analysis.clone(), move |analysis, file_ids| {
                    analysis
                        .eqwalizer_diagnostics(project_id, file_ids.to_vec())
                        .expect("cancelled")
                })
                .fold(EqwalizerDiagnostics::default, |acc, output| {
                    acc.combine((*output).clone())
                })
                .reduce(EqwalizerDiagnostics::default, |acc, other| {
                    acc.combine(other)
                });

            match output {
                EqwalizerDiagnostics::Diagnostics {
                    errors: diagnostics_by_module,
                    ..
                } => {
                    for file_id in file_ids {
                        let mut cli = Fake::default();
                        let pretty_reporter =
                            &mut reporting::PrettyReporter::new(&analysis, &loaded, &mut cli);
                        let module = module_index.module_for_file(file_id).unwrap();
                        if let Some(diagnostics) = diagnostics_by_module.get(module.as_str()) {
                            pretty_reporter
                                .write_eqwalizer_diagnostics(file_id, diagnostics)
                                .with_context(|| {
                                    format!("Failed to write diagnostics for {}", module.as_str())
                                })
                                .unwrap();
                        }
                        pretty_reporter
                            .write_error_count()
                            .with_context(|| {
                                format!("Failed to write diagnostics for {}", module.as_str())
                            })
                            .unwrap();

                        let otp_version = OTP_VERSION.as_ref().expect("MISSING OTP VERSION");
                        let otp_version_regex =
                            regex::bytes::Regex::new(&format!("{}OTPVersionDependent", "@"))
                                .unwrap();
                        let contents = analysis.file_text(file_id).unwrap();
                        let otp_version_dependent = otp_version_regex
                            .is_match(&contents.as_bytes()[0..(2001.min(contents.len()))]);
                        let exp_path = {
                            if otp_version_dependent {
                                expect_file!(format!(
                                    "../resources/test/{}/{}/{}-OTP-{}.pretty",
                                    project,
                                    app,
                                    module.as_str(),
                                    otp_version,
                                ))
                            } else {
                                expect_file!(format!(
                                    "../resources/test/{}/{}/{}.pretty",
                                    project,
                                    app,
                                    module.as_str(),
                                ))
                            }
                        };
                        let (stdout, _) = cli.to_strings();
                        assert_normalised_file(exp_path, &stdout, project_path.into(), false);
                    }
                }
                EqwalizerDiagnostics::NoAst { module } => {
                    panic!("Could not run tests because module {module} was not found")
                }
                EqwalizerDiagnostics::Error(error) => {
                    panic!("Could not run tests: {error}")
                }
            }
        }
    }

    #[test]
    fn elp_parse_all_report_compile_error() {
        // We just check the process doesn't hang. See T114609762.
        let code = parse_all_complete("parse_error").unwrap();
        assert_eq!(code, 101);
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_diagnostics_match_snapshot_app_a(buck: bool) {
        eqwalize_snapshot("standard", "app_a", false, buck);
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_diagnostics_match_snapshot_app_a_json(buck: bool) {
        if otp_supported_by_eqwalizer() {
            eqwalize_snapshot_json("standard", "app_a", false, buck, true);
        }
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_diagnostics_match_snapshot_app_a_mod2(buck: bool) {
        eqwalize_snapshot("standard", "app_a_mod2", true, buck);
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_diagnostics_match_snapshot_app_a_lists(buck: bool) {
        eqwalize_snapshot("standard", "app_a_lists", true, buck);
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_fails_on_bad_module_name(buck: bool) {
        eqwalize_snapshot("standard", "meinong", false, buck);
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_fails_on_bad_parse(buck: bool) {
        eqwalize_snapshot("parse_error", "parse_error_a_bad", false, buck);
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_fails_on_bad_parse_of_related_module(buck: bool) {
        eqwalize_snapshot("parse_error", "parse_error_a_reference_bad", false, buck);
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_succeeds_even_when_unrelated_module_has_parse_error(buck: bool) {
        eqwalize_snapshot("parse_error", "parse_error_a", false, buck);
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_all_diagnostics_match_snapshot_jsonl(buck: bool) {
        if otp_supported_by_eqwalizer() {
            simple_snapshot(
                args_vec!["eqwalize-all", "--format", "json"],
                "standard",
                expect_file!("../resources/test/standard/eqwalize_all_diagnostics.jsonl"),
                buck,
                None,
            );
        }
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_all_diagnostics_match_snapshot_jsonl_gen(buck: bool) {
        if otp_supported_by_eqwalizer() {
            simple_snapshot(
                args_vec!["eqwalize-all", "--format", "json"],
                "standard",
                expect_file!("../resources/test/standard/eqwalize_all_diagnostics_gen.jsonl"),
                buck,
                None,
            );
        }
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_all_diagnostics_match_snapshot_pretty(buck: bool) {
        if otp_supported_by_eqwalizer() {
            simple_snapshot(
                args_vec!["eqwalize-all"],
                "standard",
                expect_file!("../resources/test/standard/eqwalize_all_diagnostics.pretty"),
                buck,
                None,
            );
        }
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_app_diagnostics_match_snapshot_pretty(buck: bool) {
        if otp_supported_by_eqwalizer() {
            let expected = if buck {
                expect_file!("../resources/test/standard/eqwalize_app_diagnostics.pretty")
            } else {
                expect_file!("../resources/test/standard/eqwalize_app_diagnostics_rebar.pretty")
            };
            simple_snapshot(
                args_vec!["eqwalize-app", "app_a",],
                "standard",
                expected,
                buck,
                None,
            );
        }
    }

    #[test]
    fn eqwalize_target_diagnostics_match_snapshot_pretty() {
        if cfg!(feature = "buck") {
            simple_snapshot(
                args_vec![
                    "eqwalize-target",
                    "//whatsapp/elp/test_projects/standard:app_a",
                ],
                "standard",
                expect_file!("../resources/test/standard/eqwalize_target_diagnostics.pretty"),
                true,
                None,
            );
        }
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_app_diagnostics_match_snapshot_pretty_gen(buck: bool) {
        let expected = if buck {
            expect_file!("../resources/test/standard/eqwalize_app_diagnostics_gen.pretty")
        } else {
            expect_file!("../resources/test/standard/eqwalize_app_diagnostics_gen_rebar.pretty")
        };

        if otp_supported_by_eqwalizer() {
            simple_snapshot(
                args_vec!["eqwalize-app", "app_a",],
                "standard",
                expected,
                buck,
                None,
            );
        }
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_reports_cascading_syntax_errors(buck: bool) {
        simple_snapshot(
            args_vec!["eqwalize", "parse_error_a_cascade",],
            "diagnostics",
            expect_file!("../resources/test/standard/eqwalize_all_parse_error_cascade.pretty"),
            buck,
            None,
        );
    }

    #[ignore]
    #[test]
    fn dialyzer_cli() {
        simple_dialyzer_snapshot(
            args_vec!["dialyze-all"],
            "diagnostics",
            expect_file!("../resources/test/standard/dialyze_all.stdout"),
            0,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_diagnostics1(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["parse-elp", "--module", "diagnostics",],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_all_diagnostics1.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_elp_file_attribute(buck: bool) {
        simple_snapshot(
            args_vec!["parse-elp", "--module", "file_attribute",],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_elp_file_attribute.stdout"),
            buck,
            None,
        );
    }

    #[test]
    // Can only be buck test, --rebar ignores other config
    fn eqwalize_all_reads_json_config() {
        simple_snapshot_with_json_config(
            args_vec!["parse-elp", "--module", "diagnostics",],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_all_diagnostics1.stdout"),
            true,
            None,
            Some("test_build_info.json"),
            101,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_diagnostics_json(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["parse-elp", "--module", "diagnostics", "--format", "json"],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_all_diagnostics_json.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_diagnostics_exclude_generated(buck: bool) {
        simple_snapshot(
            args_vec!["parse-elp", "--module", "erlang_diagnostics_errors_gen"],
            "diagnostics",
            expect_file!(
                "../resources/test/standard/parse_all_diagnostics_exclude_generated.jsonl"
            ),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_diagnostics_include_generated(buck: bool) {
        simple_snapshot(
            args_vec![
                "parse-elp",
                "--module",
                "erlang_diagnostics_errors_gen",
                "--include-generated"
            ],
            "diagnostics",
            expect_file!(
                "../resources/test/standard/parse_all_diagnostics_include_generated.jsonl"
            ),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_diagnostics_related(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["parse-elp", "--module", "cascading",],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_all_diagnostics_cascading.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_diagnostics_syntax_errors(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["parse-elp", "--module", "syntax",],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_all_diagnostics_syntax.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_diagnostics_hrl(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["parse-elp",],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_all_diagnostics_hrl.stdout"),
            buck,
            Some("app_a/include/broken_diagnostics.hrl"),
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_diagnostics_broken_parse_trans(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["parse-elp",],
            "diagnostics",
            expect_file!(
                "../resources/test/diagnostics/parse_all_diagnostics_broken_parse_trans.stdout"
            ),
            buck,
            Some("app_a/src/broken_parse_trans.erl"),
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_diagnostics_escript1(buck: bool) {
        simple_snapshot(
            args_vec!["parse-elp",],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_all_diagnostics_escript.stdout"),
            buck,
            Some("app_a/src/diagnostics.escript"),
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_diagnostics_escript2(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["parse-elp",],
            "diagnostics",
            expect_file!(
                "../resources/test/diagnostics/parse_all_diagnostics_errors_escript.stdout"
            ),
            buck,
            Some("app_a/src/diagnostics_errors.escript"),
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_diagnostics_escript3(buck: bool) {
        simple_snapshot(
            args_vec!["parse-elp",],
            "diagnostics",
            expect_file!(
                "../resources/test/diagnostics/parse_all_diagnostics_warnings_escript.stdout"
            ),
            buck,
            Some("app_a/src/diagnostics_warnings.escript"),
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_otp_fix(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["parse-elp", "--module", "otp_7655",],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_all_otp_7655.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_all_crlf_file(buck: bool) {
        simple_snapshot(
            args_vec!["parse-elp", "--include-generated", "--module", "crlf",],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_all_crlf.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_elp_report_system_stats(buck: bool) {
        simple_snapshot_output_contains(
            args_vec!["parse-elp", "--report-system-stats", "--module", "app_a"],
            "standard",
            &[
                "Memory usage:",
                "allocated:",
                "active:",
                "resident:",
                "FileTextQuery",
            ],
            buck,
            None,
            0,
        );
    }

    #[test]
    fn build_info_json_not_buck() {
        let tmp_dir = make_tmp_dir();
        let tmp_file = tmp_dir.path().join("test_build_info.json");
        let project = "diagnostics";
        let path_str = format!("{}/test_build_info.json", project_path(project));
        let args = args_vec![
            "build-info",
            "--to",
            tmp_file.clone(),
            "--project",
            path_str
        ];
        let (stdout, stderr, code) = elp(args);
        assert_eq!(
            code, 0,
            "failed with unexpected exit code: got {} not {}\nstdout:\n{}\nstderr:\n{}",
            code, 0, stdout, stderr
        );
        assert!(
            stderr.is_empty(),
            "expected stderr to be empty, got:\n{stderr}"
        );
        assert!(tmp_file.clone().exists());
        let content = fs::read_to_string(tmp_file).unwrap();
        expect![[r#"
            {
              "apps": [
                {
                  "name": "app_a",
                  "dir": "app_a",
                  "src_dirs": [
                    "src"
                  ],
                  "extra_src_dirs": [
                    "test"
                  ],
                  "include_dirs": [
                    "include"
                  ],
                  "macros": {
                    "MEANING_OF_LIFE": "fortytwo",
                    "TEST": "true",
                    "COMMON_TEST": "true"
                  }
                }
              ],
              "deps": []
            }"#]]
        .assert_eq(content.as_str());
    }

    #[test]
    fn build_info_json_buck() {
        if cfg!(feature = "buck") {
            let tmp_dir = make_tmp_dir();
            let tmp_file = tmp_dir.path().join("test_build_info.json");
            let project = "diagnostics";
            let path_str = project_path(project);
            let args = args_vec![
                "build-info",
                "--to",
                tmp_file.clone(),
                "--project",
                path_str.clone()
            ];
            let (stdout, stderr, code) = elp(args);
            assert_eq!(
                code, 0,
                "failed with unexpected exit code: got {} not {}\nstdout:\n{}\nstderr:\n{}",
                code, 0, stdout, stderr
            );
            assert!(
                stderr.is_empty(),
                "expected stderr to be empty, got:\n{stderr}"
            );
            assert!(tmp_file.clone().exists());
            let content = fs::read_to_string(tmp_file).unwrap();
            let mut buck_config = BuckConfig::default();
            buck_config.buck_root = Some(AbsPathBuf::assert_utf8(current_dir().unwrap()));
            let prelude_cell = get_prelude_cell(&buck_config).expect("could not get prelude");
            let prelude_cell = prelude_cell.strip_prefix("/").unwrap();
            let content = content.replace(prelude_cell, "/[prelude]/");

            let mut buck_config = BuckConfig::default();

            let abs = fs::canonicalize(path_str).unwrap();
            buck_config.buck_root =
                Some(AbsPathBuf::assert(Utf8PathBuf::from_path_buf(abs).unwrap()));
            let content = normalise_prelude_path(content, buck_config);

            expect![[r#"
                {
                  "apps": [
                    {
                      "name": "test_exec",
                      "dir": "/[prelude]//erlang/common_test/test_exec/src",
                      "src_dirs": [
                        ""
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [],
                      "macros": {}
                    },
                    {
                      "name": "diagnostics_app_a",
                      "dir": "app_a",
                      "src_dirs": [
                        "src"
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [
                        "include"
                      ],
                      "macros": {
                        "COMMON_TEST": "true",
                        "TEST": "true"
                      }
                    },
                    {
                      "name": "app_a_SUITE",
                      "dir": "app_a/test",
                      "src_dirs": [],
                      "extra_src_dirs": [
                        ""
                      ],
                      "include_dirs": [],
                      "macros": {
                        "COMMON_TEST": "true",
                        "TEST": "true"
                      }
                    },
                    {
                      "name": "common",
                      "dir": "/[prelude]//erlang/common_test/common",
                      "src_dirs": [
                        "src"
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [
                        "include"
                      ],
                      "macros": {}
                    },
                    {
                      "name": "cth_hooks",
                      "dir": "/[prelude]//erlang/common_test/cth_hooks/src",
                      "src_dirs": [
                        ""
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [
                        ""
                      ],
                      "macros": {}
                    },
                    {
                      "name": "buck2_shell_utils",
                      "dir": "/[prelude]//erlang/shell/src",
                      "src_dirs": [
                        ""
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [],
                      "macros": {}
                    },
                    {
                      "name": "test_binary",
                      "dir": "/[prelude]//erlang/common_test/test_binary/src",
                      "src_dirs": [
                        ""
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [],
                      "macros": {}
                    },
                    {
                      "name": "test_cli_lib",
                      "dir": "/[prelude]//erlang/common_test/test_cli_lib/src",
                      "src_dirs": [
                        ""
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [],
                      "macros": {}
                    }
                  ],
                  "deps": []
                }"#]]
            .assert_eq(content.as_str());
        }
    }

    fn normalise_prelude_path(content: String, buck_config: BuckConfig) -> String {
        let prelude_cell = get_prelude_cell(&buck_config).expect("could not get prelude");
        let prelude_cell = prelude_cell.strip_prefix("/").unwrap();
        content.replace(prelude_cell, "/[prelude]/")
    }

    #[test]
    #[ignore]
    fn build_info_json_buck_bxl_generated() {
        if cfg!(feature = "buck") {
            let tmp_dir = make_tmp_dir();
            let tmp_file = tmp_dir.path().join("test_build_info.json");
            let project = "diagnostics";
            let path_str = project_path(project);
            let args = args_vec![
                "build-info",
                "--to",
                tmp_file.clone(),
                "--project",
                path_str
            ];
            let (stdout, stderr, code) = elp(args);
            assert_eq!(
                code, 0,
                "failed with unexpected exit code: got {} not {}\nstdout:\n{}\nstderr:\n{}",
                code, 0, stdout, stderr
            );
            assert!(
                stderr.is_empty(),
                "expected stderr to be empty, got:\n{stderr}"
            );
            assert!(tmp_file.clone().exists());
            let content = fs::read_to_string(tmp_file).unwrap();
            let mut buck_config = BuckConfig::default();
            buck_config.buck_root = Some(AbsPathBuf::assert_utf8(current_dir().unwrap()));
            let prelude_cell = get_prelude_cell(&buck_config).expect("could not get prelude");
            let prelude_cell = prelude_cell.strip_prefix("/").unwrap();
            let content = content.replace(prelude_cell, "/[prelude]/");
            expect![[r#"
                {
                  "apps": [
                    {
                      "name": "test_exec",
                      "dir": "/[prelude]//erlang/common_test/test_exec/src",
                      "src_dirs": [
                        ""
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [],
                      "macros": {
                        "COMMON_TEST": "true",
                        "TEST": "true"
                      }
                    },
                    {
                      "name": "diagnostics_app_a",
                      "dir": "app_a",
                      "src_dirs": [
                        "src"
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [
                        "include"
                      ],
                      "macros": {
                        "COMMON_TEST": "true",
                        "TEST": "true"
                      }
                    },
                    {
                      "name": "app_a_SUITE",
                      "dir": "app_a/test",
                      "src_dirs": [],
                      "extra_src_dirs": [
                        ""
                      ],
                      "include_dirs": [],
                      "macros": {
                        "COMMON_TEST": "true",
                        "TEST": "true"
                      }
                    },
                    {
                      "name": "common",
                      "dir": "/[prelude]//erlang/common_test/common",
                      "src_dirs": [
                        "src"
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [
                        "include"
                      ],
                      "macros": {
                        "COMMON_TEST": "true",
                        "TEST": "true"
                      }
                    },
                    {
                      "name": "cth_hooks",
                      "dir": "/[prelude]//erlang/common_test/cth_hooks/src",
                      "src_dirs": [
                        ""
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [
                        ""
                      ],
                      "macros": {
                        "COMMON_TEST": "true",
                        "TEST": "true"
                      }
                    },
                    {
                      "name": "buck2_shell_utils",
                      "dir": "/[prelude]//erlang/shell/src",
                      "src_dirs": [
                        ""
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [],
                      "macros": {
                        "COMMON_TEST": "true",
                        "TEST": "true"
                      }
                    },
                    {
                      "name": "test_binary",
                      "dir": "/[prelude]//erlang/common_test/test_binary/src",
                      "src_dirs": [
                        ""
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [],
                      "macros": {
                        "COMMON_TEST": "true",
                        "TEST": "true"
                      }
                    },
                    {
                      "name": "test_cli_lib",
                      "dir": "/[prelude]//erlang/common_test/test_cli_lib/src",
                      "src_dirs": [
                        ""
                      ],
                      "extra_src_dirs": [],
                      "include_dirs": [],
                      "macros": {
                        "COMMON_TEST": "true",
                        "TEST": "true"
                      }
                    }
                  ],
                  "deps": []
                }"#]]
            .assert_eq(content.as_str());
        }
    }

    #[test]
    fn parse_elp_custom_build_tool() {
        simple_snapshot_expect_error(
            args_vec!["parse-elp", "--module", "app_b"],
            "custom_build_tool",
            expect_file!("../resources/test/custom_build_tool/parse_elp_custom_build_tool.jsonl"),
            true,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_2(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["lint", "--module", "app_a", "--diagnostic-filter", "P1700",],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint2.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_app(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["lint", "--app", "app_a", "--diagnostic-filter", "P1700",],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_app.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_application(buck: bool) {
        simple_snapshot_expect_error(
            args_vec![
                "lint",
                "--application",
                "app_a",
                "--diagnostic-filter",
                "P1700",
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_app.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_report_suppressed(buck: bool) {
        simple_snapshot(
            args_vec![
                "lint",
                "--module",
                "suppressed",
                "--experimental",
                "--include-suppressed",
                "--diagnostic-filter",
                "W0007",
            ],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/lint_report_suppressed.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_recursive(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix(
            args_vec![
                "lint",
                "--module",
                "lint_recursive",
                "--diagnostic-filter",
                "W0007",
                "--apply-fix",
                "--recursive",
                "--experimental",
                "--to",
                tmp_path,
            ],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_elp_lint_recursive.stdout"),
            0,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/lint_recursive"),
            &[("app_a/src/lint_recursive.erl", "lint_recursive.erl")],
            false,
        )
        .expect("bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_ignore_apps_a(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix(
            args_vec![
                "lint",
                "--diagnostic-filter",
                "W0010",
                "--experimental",
                // ignored apps
                "app_a",
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_ignore_apps.stdout"),
            0,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/lint_recursive"),
            &[],
            false,
        )
        .expect("bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_ignore_apps_b(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix(
            args_vec![
                "lint",
                "--diagnostic-filter",
                "W0010",
                "--experimental",
                // ignored apps
                "app_b",
                "app_c",
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_ignore_apps_b.stdout"),
            0,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/lint_recursive"),
            &[],
            false,
        )
        .expect("bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_config_file_used(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix_stderr(
            args_vec![
                "lint",
                "--diagnostic-filter",
                "W0010",
                "--experimental",
                "--read-config"
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_config_output.stdout"),
            101,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/lint_recursive"),
            &[],
            false,
            Some(expect![[r#"
                Errors found
            "#]]),
        )
        .expect("bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_custom_config_file_invalid(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix_stderr(
            args_vec![
                "lint",
                "--experimental",
                "--config-file",
                "../../test_projects/linter/does_not_exist.toml"
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_custom_config_invalid_output.stdout"),
            101,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/lint_recursive"),
            &[],
            false,
            Some(expect![[r#"
                unable to read "../../test_projects/linter/does_not_exist.toml": No such file or directory (os error 2)
            "#]]),
        )
        .expect("bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_custom_config_file_used(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix(
            args_vec![
                "lint",
                "--experimental",
                "--config-file",
                "../../test_projects/linter/elp_lint_test1.toml"
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_custom_config_output.stdout"),
            0,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/lint_recursive"),
            &[],
            false,
        )
        .expect("bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_custom_ad_hoc_lints(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix(
            args_vec![
                "lint",
                "--experimental",
                "--config-file",
                "../../test_projects/linter/elp_lint_adhoc.toml",
                "--module",
                "app_b",
                "--apply-fix",
                "--one-shot",
                "--to",
                tmp_path,
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_adhoc_output.stdout"),
            0,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/from_config"),
            &[("app_b/src/app_b.erl", "app_b.erl")],
            false,
        )
        .expect("bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_diagnostic_ignore(buck: bool) {
        simple_snapshot(
            args_vec![
                "lint",
                "--experimental",
                "--diagnostic-ignore",
                "W0011",
                "--config-file",
                "../../test_projects/linter/elp_lint_test_ignore.toml"
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_ignore.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_reports_erlang_service_diagnostics(buck: bool) {
        simple_snapshot(
            args_vec![
                "lint",
                "--diagnostic-filter",
                "L1318",
                "--module",
                "expression_updates_literal"
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_erlang_service.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_config_file_parse_error(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix_stderr(
            args_vec!["lint", "--experimental", "--read-config"],
            "linter_bad_config",
            expect_file!("../resources/test/linter/parse_elp_lint_bad_config_output.stdout"),
            101,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/lint_recursive"),
            &[],
            false,
            Some(expect![[r#"
                failed to read "../../test_projects/linter_bad_config/.elp_lint.toml":expected a right bracket, found an identifier at line 6 column 4
            "#]]),
        )
        .expect("bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_no_diagnostics_filter_all_enabled(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["lint",],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_no_lint_specified_output.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_no_diagnostics_filter_all_enabled_json(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["lint", "--format", "json"],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_no_lint_specified_json_output.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_apply_fix_no_diagnostics_enabled(buck: bool) {
        simple_snapshot_expect_stderror(
            args_vec!["lint", "--apply-fix",],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_apply_fix_no_lint_output.stdout"),
            buck,
            None,
            false,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_explicit_enable_diagnostic(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix_stderr(
            args_vec![
                "lint",
                "--config-file",
                "../../test_projects/linter/elp_lint_test2.toml"
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_explicit_enable_output.stdout"),
            101,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/lint_recursive"),
            &[],
            false,
            Some(expect![[r#"
                Errors found
            "#]]),
        )
        .expect("bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_json_output(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix_stderr(
            args_vec![
                "lint",
                "--diagnostic-filter",
                "W0010",
                "--experimental",
                "--format",
                "json",
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_json_output.stdout"),
            101,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/lint_recursive"),
            &[],
            false,
            Some(expect![[r#"
                Errors found
            "#]]),
        )
        .expect("bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_applies_fix_using_to_dir(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix_stderr(
            args_vec![
                "lint",
                "--module",
                "lints",
                "--diagnostic-filter",
                "P1700",
                "--to",
                tmp_path,
                "--apply-fix",
            ],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_elp_lint_fix.stdout"),
            101,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/head_mismatch"),
            &[("app_a/src/lints.erl", "lints.erl")],
            false,
            Some(expect![[r#"
                Errors found
            "#]]),
        )
        .expect("Bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_applies_fix_using_to_dir_json_output(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix_stderr(
            args_vec![
                "lint",
                "--module",
                "lints",
                "--diagnostic-filter",
                "P1700",
                "--format",
                "json",
                "--to",
                tmp_path,
                "--apply-fix"
            ],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_elp_lint_fix_json.stdout"),
            101,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/head_mismatch"),
            &[("app_a/src/lints.erl", "lints.erl")],
            false,
            Some(expect![[r#"
                Errors found
            "#]]),
        )
        .expect("Bad test");
    }

    #[test]
    fn lint_applies_fix_in_place() {
        // These tests make changes in the source tree.

        // We manually force them to run sequentially, and no other
        // test should access the same test project, to prevent race
        // conditions.

        do_lint_applies_fix_in_place(false);
        if cfg!(feature = "buck") {
            do_lint_applies_fix_in_place(true);
        }
    }

    fn do_lint_applies_fix_in_place(buck: bool) {
        let project = "in_place_tests";
        check_lint_fix_stderr(
            args_vec![
                "lint",
                "--module",
                "lints",
                "--diagnostic-filter",
                "P1700",
                "--apply-fix",
                "--in-place"
            ],
            project,
            expect_file!("../resources/test/diagnostics/parse_elp_lint_fix.stdout"),
            101,
            buck,
            None,
            Path::new(&project_path(project)),
            Path::new("../resources/test/lint/head_mismatch"),
            &[("app_a/src/lints.erl", "app_a/src/lints.erl")],
            true,
            Some(expect![[r#"
                Errors found
            "#]]),
        )
        .expect("Bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_applies_ignore_fix_if_requested(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix(
            args_vec![
                "lint",
                "--module",
                "app_b",
                "--diagnostic-filter",
                "W0011",
                "--to",
                tmp_path,
                "--apply-fix",
                "--ignore-fix-only",
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_fix_ignore.stdout"),
            0,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/ignore_app_env"),
            &[("app_b/src/app_b.erl", "app_b.erl")],
            false,
        )
        .expect("Bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_applies_code_action_fixme_if_requested(buck: bool) {
        let tmp_dir = make_tmp_dir();
        let tmp_path = tmp_dir.path();
        check_lint_fix_stderr(
            args_vec![
                "lint",
                "--module",
                "spelling",
                "--diagnostic-filter",
                "W0013",
                "--to",
                tmp_path,
                "--apply-fix",
                "--ignore-fix-only",
            ],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_lint_fixme_spelling.stdout"),
            101,
            buck,
            None,
            tmp_path,
            Path::new("../resources/test/lint/ignore_app_env"),
            &[("app_a/src/spelling.erl", "spelling.erl")],
            false,
            Some(expect![[r#"
                Errors found
            "#]]),
        )
        .expect("Bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_errors_on_deprecated_l1500(buck: bool) {
        simple_snapshot_expect_stderror(
            args_vec!["lint", "--module", "app_a", "--diagnostic-filter", "L1500",],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_elp_l1500_deprecated.stdout"),
            buck,
            None,
            false,
        )
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_edoc(buck: bool) {
        simple_snapshot(
            args_vec![
                "lint",
                "--include-edoc-diagnostics",
                "--diagnostic-filter"
                "O0039"
            ],
            "linter",
            expect_file!("../resources/test/linter/elp_lint_edoc.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_ct_default_no_tests(buck: bool) {
        simple_snapshot(
            args_vec![
                "lint",
                "--include-ct-diagnostics",
                "--diagnostic-filter"
                "W0008"
            ],
            "linter",
            expect_file!("../resources/test/linter/elp_lint_ct_no_tests_flag.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_ct_include_tests(buck: bool) {
        simple_snapshot_expect_error(
            args_vec![
                "lint",
                "--include-ct-diagnostics",
                "--include-tests",
                "--diagnostic-filter"
                "W0008"
            ],
            "linter",
            expect_file!("../resources/test/linter/elp_lint_ct.stdout"),
            buck,
            None,
        );
    }

    #[test]
    fn lint_resolves_generated_includes() {
        if cfg!(feature = "buck") {
            simple_snapshot_expect_error(
                args_vec!["lint"],
                "buck_tests_2",
                expect_file!("../resources/test/buck_tests_2/resolves_generated_includes.stdout"),
                true,
                None,
            );
        }
    }

    #[test]
    fn lint_reports_bxl_project_error() {
        if cfg!(feature = "buck") {
            simple_snapshot_expect_stderror(
                args_vec!["lint",],
                "buck_bad_config",
                expect_file!("../resources/test/buck_bad_config/bxl_error_message.stdout"),
                true,
                None,
                true,
            );
        }
    }

    #[test]
    fn lint_warnings_as_errors() {
        simple_snapshot_expect_error(
            args_vec![
                "lint",
                "--config-file",
                "../../test_projects/linter/elp_lint_warnings_as_errors.toml"
            ],
            "linter",
            expect_file!("../resources/test/linter/warnings_as_errors.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_custom_function_matches() {
        simple_snapshot(
            args_vec![
                "lint",
                "--config-file",
                "../../test_projects/linter/elp_lint_custom_function_matches.toml",
                "--module",
                "custom_function_matches"
            ],
            "linter",
            expect_file!("../resources/test/linter/custom_function_matches.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_unavailable_type() {
        simple_snapshot(
            args_vec![
                "lint",
                "--config-file",
                "../../test_projects/xref/elp_lint_unavailable_type.toml",
                "--module",
                "unavailable_type"
            ],
            "xref",
            expect_file!("../resources/test/xref/unavailable_type.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_from_config() {
        simple_snapshot(
            args_vec![
                "lint",
                "--config-file",
                "../../test_projects/linter/elp_lint_ssr_adhoc.toml",
            ],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_from_bad_config() {
        simple_snapshot_expect_stderror(
            args_vec![
                "lint",
                "--config-file",
                "../../test_projects/linter/elp_lint_ssr_adhoc_parse_fail.toml",
            ],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_parse_fail.stdout"),
            true,
            None,
            false,
        )
    }

    #[test]
    fn lint_ssr_as_cli_arg() {
        simple_snapshot(
            args_vec!["ssr", "ssr: {_@A, _@B}.",],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_cli.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_as_cli_arg_without_prefix() {
        simple_snapshot(
            args_vec!["ssr", "{_@A, _@B}",],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_cli.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_with_context_and_separator() {
        simple_snapshot(
            args_vec![
                "--colour",
                "never",
                "ssr",
                "--context",
                "2",
                "--group-separator",
                "====",
                "{_@A, _@B}",
            ],
            "linter",
            expect_file!("../resources/test/linter/ssr_context_separator.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_with_context_and_separator_color() {
        simple_snapshot(
            args_vec![
                "--colour",
                "always",
                "ssr",
                "--context",
                "2",
                "--group-separator",
                "====",
                "{_@A, _@B}",
            ],
            "linter",
            expect_file!("../resources/test/linter/ssr_context_separator_color.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_as_cli_arg_multiple_patterns() {
        simple_snapshot(
            args_vec!["ssr", "3" "{4}",],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_cli_multiple.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_as_cli_arg_malformed() {
        simple_snapshot_expect_stderror(
            args_vec!["ssr", "ssr: {_@A, = _@B}.",],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_cli_parse_error.stdout"),
            true,
            None,
            false,
        )
    }

    #[test]
    fn lint_ssr_as_cli_parens_visible() {
        simple_snapshot(
            args_vec!["ssr", "--parens", "(_@A)",],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_cli_parens_visible.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_as_cli_parens_invisible() {
        // Invisible parens are the default
        simple_snapshot(
            args_vec!["ssr", "(((3)))",],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_cli_parens_invisible.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_as_cli_macros_expand() {
        simple_snapshot(
            args_vec!["ssr", "--macros", "expand", "?BAR(_@AA)", "{4}"],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_cli_macros_expand.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_as_cli_macros_expand_is_default() {
        simple_snapshot(
            args_vec!["ssr", "?BAR(_@AA)", "{4}"],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_cli_macros_expand.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_as_cli_macros_visible_expand() {
        simple_snapshot(
            args_vec!["ssr", "--macros", "visible-expand", "?BAR(_@AA)", "{4}"],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_cli_macros_visible_expand.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_as_cli_macros_no_expand() {
        simple_snapshot(
            args_vec!["ssr", "--macros", "no-expand", "?BAR(_@AA)", "{4}"],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_cli_macros_no_expand.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_as_cli_dump_config() {
        simple_snapshot(
            args_vec!["ssr", "--dump-config", "?BAR(_@AA)", "{4}"],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_cli_dump_config.stdout"),
            true,
            None,
        )
    }

    #[test]
    fn lint_ssr_as_cli_dump_config_without_info() {
        simple_snapshot(
            args_vec!["ssr", "--dump-config", "?BAR(_@AA)", "{4}"],
            "linter",
            expect_file!("../resources/test/linter/ssr_ad_hoc_cli_dump_config.stdout"),
            true,
            None,
        )
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalizer_tests_check(buck: bool) {
        eqwalize_all_snapshots(
            "eqwalizer_tests",
            "check",
            buck,
            EqwalizerConfig::default_test(),
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalizer_tests_debug(buck: bool) {
        eqwalize_all_snapshots(
            "eqwalizer_tests",
            "debug",
            buck,
            EqwalizerConfig::default_test(),
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalizer_tests_elm_core(buck: bool) {
        eqwalize_all_snapshots(
            "eqwalizer_tests",
            "elm_core",
            buck,
            EqwalizerConfig::default_test(),
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalizer_tests_eqwater(buck: bool) {
        eqwalize_all_snapshots(
            "eqwalizer_tests",
            "eqwater",
            buck,
            EqwalizerConfig::default_test(),
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalizer_tests_options(buck: bool) {
        eqwalize_all_snapshots(
            "eqwalizer_tests",
            "options",
            buck,
            EqwalizerConfig {
                report_dynamic_lambdas: Some(true),
                ..EqwalizerConfig::default_test()
            },
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalizer_tests_fault_tolerance(buck: bool) {
        eqwalize_all_snapshots(
            "eqwalizer_tests",
            "fault_tolerance",
            buck,
            EqwalizerConfig::default_test(),
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_all_bail_on_error_failure(buck: bool) {
        if otp_supported_by_eqwalizer() {
            simple_snapshot_expect_error(
                args_vec!["eqwalize-all", "--bail-on-error"],
                "standard",
                expect_file!(
                    "../resources/test/standard/eqwalize_all_bail_on_error_failure.pretty"
                ),
                buck,
                None,
            );
        }
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_all_bail_on_error_success(buck: bool) {
        if otp_supported_by_eqwalizer() {
            simple_snapshot(
                args_vec!["eqwalize", "--bail-on-error", "app_a_no_errors"],
                "standard",
                expect_file!(
                    "../resources/test/standard/eqwalize_all_bail_on_error_success.pretty"
                ),
                buck,
                None,
            );
        }
    }

    #[test]
    fn eqwalize_specific_module_overrides_ignore_modules() {
        if otp_supported_by_eqwalizer() {
            simple_snapshot_expect_error(
                args_vec!["eqwalize", "--bail-on-error", "app_b"],
                "eqwalizer_ignore_modules",
                expect_file!(
                    "../resources/test/eqwalizer_ignore_modules/eqwalize_bail_on_error_failure.pretty"
                ),
                true,
                None,
            );
        }
    }

    #[test]
    fn eqwalize_all_ignore_modules_success() {
        if otp_supported_by_eqwalizer() {
            simple_snapshot(
                args_vec!["eqwalize-all", "--bail-on-error"],
                "eqwalizer_ignore_modules",
                expect_file!(
                    "../resources/test/eqwalizer_ignore_modules/eqwalize_all_bail_on_error_success.pretty"
                ),
                true,
                None,
            );
        }
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_with_color_vs_no_color(buck: bool) {
        if otp_supported_by_eqwalizer() {
            // Test with color (default)
            let (mut args_color, _path) =
                add_project(args_vec!["eqwalize", "app_a"], "standard", None, None);
            if !buck {
                args_color.push("--rebar".into());
            }

            // Test without color
            let (mut args_no_color, _) = add_project(
                args_vec!["--color", "never", "eqwalize", "app_a"],
                "standard",
                None,
                None,
            );
            if !buck {
                args_no_color.push("--rebar".into());
            }

            let (stdout_color, stderr_color, code_color) = elp(args_color);
            let (stdout_no_color, stderr_no_color, code_no_color) = elp(args_no_color);

            // Both should have same exit code
            assert_eq!(code_color, code_no_color);

            // Both should have same stderr behavior
            if code_color == 0 {
                assert!(stderr_color.is_empty());
                assert!(stderr_no_color.is_empty());
            }

            // The content should be similar but no-color version should not contain ANSI escape codes
            // ANSI color codes typically start with \x1b[ or \u{1b}[
            let _has_ansi_color = stdout_color.contains('\x1b');
            let has_ansi_no_color = stdout_no_color.contains('\x1b');

            // With --color never, there should be no ANSI escape sequences
            assert!(
                !has_ansi_no_color,
                "Output with --color never should not contain ANSI escape codes"
            );

            // The outputs should be functionally equivalent when ANSI codes are stripped
            let stripped_color = strip_ansi_codes(&stdout_color);
            assert_eq!(
                stripped_color, stdout_no_color,
                "Content should be identical after stripping ANSI codes"
            );
        }
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_with_no_color_env_var(buck: bool) {
        if otp_supported_by_eqwalizer() {
            // Test with NO_COLOR environment variable set
            unsafe {
                env::set_var("NO_COLOR", "1");
            }

            let (mut args_no_color_env, _) =
                add_project(args_vec!["eqwalize", "app_a"], "standard", None, None);
            if !buck {
                args_no_color_env.push("--rebar".into());
            }

            let (stdout_no_color_env, stderr_no_color_env, code_no_color_env) =
                elp(args_no_color_env);

            // Clean up environment variable
            unsafe {
                env::remove_var("NO_COLOR");
            }

            // Test with normal color (for comparison)
            let (mut args_color, _) =
                add_project(args_vec!["eqwalize", "app_a"], "standard", None, None);
            if !buck {
                args_color.push("--rebar".into());
            }

            let (stdout_color, stderr_color, code_color) = elp(args_color);

            // Both should have same exit code
            assert_eq!(code_color, code_no_color_env);

            // Both should have same stderr behavior
            if code_color == 0 {
                assert!(stderr_color.is_empty());
                assert!(stderr_no_color_env.is_empty());
            }

            // The NO_COLOR env var version should not contain ANSI escape codes
            let has_ansi_no_color_env = stdout_no_color_env.contains('\x1b');
            assert!(
                !has_ansi_no_color_env,
                "Output with NO_COLOR env var should not contain ANSI escape codes"
            );

            // The outputs should be functionally equivalent when ANSI codes are stripped
            let stripped_color = strip_ansi_codes(&stdout_color);
            assert_eq!(
                stripped_color, stdout_no_color_env,
                "Content should be identical after stripping ANSI codes"
            );
        }
    }

    // -----------------------------------------------------------------

    #[test]
    fn help() {
        let args = args::args().run_inner(Args::from(&["--help"])).unwrap_err();
        let expected = expect_file!["../resources/test/help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn eqwalize_all_help() {
        let args = args::args()
            .run_inner(Args::from(&["eqwalize-all", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/eqwalize_all_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn eqwalize_help() {
        let args = args::args()
            .run_inner(Args::from(&["eqwalize", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/eqwalize_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn eqwalize_target_help() {
        let args = args::args()
            .run_inner(Args::from(&["eqwalize-target", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/eqwalize_target_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn eqwalize_app_help() {
        let args = args::args()
            .run_inner(Args::from(&["eqwalize-app", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/eqwalize_app.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn dialyze_all_help() {
        let args = args::args()
            .run_inner(Args::from(&["dialyze-all", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/dialyze_all_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn parse_all_help() {
        let args = args::args()
            .run_inner(Args::from(&["parse-all", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/parse_all_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn parse_elp_help() {
        let args = args::args()
            .run_inner(Args::from(&["parse-elp", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/parse_elp_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn lint_help() {
        let args = args::args()
            .run_inner(Args::from(&["lint", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/lint_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn ssr_help() {
        let args = args::args()
            .run_inner(Args::from(&["ssr", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/ssr_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn search_help() {
        let args = args::args()
            .run_inner(Args::from(&["search", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/ssr_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn build_info_help() {
        let args = args::args()
            .run_inner(Args::from(&["build-info", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/build_info_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn project_info_help() {
        let args = args::args()
            .run_inner(Args::from(&["project-info", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/project_info_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn explain_help() {
        let args = args::args()
            .run_inner(Args::from(&["explain", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/explain_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn glean_help() {
        let args = args::args()
            .run_inner(Args::from(&["glean", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/glean_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    #[test]
    fn config_help() {
        let args = args::args()
            .run_inner(Args::from(&["config", "--help"]))
            .unwrap_err();
        let expected = expect_file!["../resources/test/config_help.stdout"];
        let stdout = args.unwrap_stdout();
        expected.assert_eq(&stdout);
    }

    // -----------------------------------------------------------------

    #[test]
    fn explain_code() {
        let args = args_vec!["explain", "--code", "W0005"];
        let (stdout, stderr, code) = elp(args);
        let expected = expect_file!["../resources/test/explain_code.stdout"];
        expected.assert_eq(stdout.strip_prefix(BASE_URL).unwrap());
        assert!(stderr.is_empty());
        assert_eq!(code, 0);
    }

    #[test]
    fn explain_unknown_code() {
        let args = args_vec!["explain", "--code", "does_not_exist"];
        let (stdout, stderr, code) = elp(args);
        let expected = expect_file!["../resources/test/explain_unkwnown_code.stdout"];
        expected.assert_eq(&stdout);
        assert!(stderr.is_empty());
        assert_eq!(code, 0);
    }

    #[test]
    fn dump_config() {
        let args = args_vec!["config"];
        let (stdout, stderr, code) = elp(args);
        let expected = expect_file!["../resources/test/config_stanza.stdout"];
        expected.assert_eq(&stdout);
        assert!(stderr.is_empty());
        assert_eq!(code, 0);
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_otp27_docstrings(buck: bool) {
        simple_snapshot(
            args_vec!["parse-elp", "--module", "otp27_docstrings"],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_otp27_docstrings.jsonl"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn parse_otp27_sigils(buck: bool) {
        if otp::supports_eep66_sigils() {
            simple_snapshot_expect_error(
                args_vec!["parse-elp", "--module", "otp27_sigils"],
                "diagnostics",
                expect_file!("../resources/test/diagnostics/parse_otp27_sigils.jsonl"),
                buck,
                None,
            );
        }
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn include_lib_non_dependency_fails(buck: bool) {
        if buck {
            simple_snapshot_expect_error(
                args_vec!["parse-elp", "--module", "main_app"],
                "include_lib_dependency_test",
                expect_file!(
                    "../resources/test/include_lib_dependency_test/include_lib_non_dependency_fails.stdout"
                ),
                buck,
                None,
            );
        } else {
            simple_snapshot_expect_error(
                args_vec!["parse-elp", "--module", "main_app"],
                "include_lib_dependency_test",
                expect_file!(
                    "../resources/test/include_lib_dependency_test/include_lib_non_dependency_rebar.stdout"
                ),
                buck,
                None,
            );
        };
    }

    #[track_caller]
    fn simple_snapshot(
        args: Vec<OsString>,
        project: &str,
        expected: ExpectFile,
        buck: bool,
        file: Option<&str>,
    ) {
        simple_snapshot_with_json_config(args, project, expected, buck, file, None, 0)
    }

    #[track_caller]
    fn simple_snapshot_with_json_config(
        args: Vec<OsString>,
        project: &str,
        expected: ExpectFile,
        buck: bool,
        file: Option<&str>,
        json: Option<&str>,
        expected_code: i32,
    ) {
        if !buck || cfg!(feature = "buck") {
            let (mut args, path) = add_project(args, project, file, json);
            if !buck {
                args.push("--rebar".into());
            }
            let (stdout, stderr, code) = elp(args);
            assert_eq!(
                code, expected_code,
                "failed with unexpected exit code: got {code} not {expected_code}\nstdout:\n{stdout}\nstderr:\n{stderr}"
            );
            assert_normalised_file(expected, &stdout, path, false);
            if expected_code == 0 {
                assert!(
                    stderr.is_empty(),
                    "expected stderr to be empty, got:\n{stderr}"
                )
            }
        }
    }

    #[track_caller]
    fn simple_dialyzer_snapshot(
        args: Vec<OsString>,
        _project: &str,
        expected: ExpectFile,
        expected_code: i32,
    ) {
        let (stdout, stderr, code) = elp(args);
        assert_eq!(
            code, expected_code,
            "failed with unexpected exit code: got {code} not {expected_code}\nstdout:\n{stdout}\nstderr:\n{stderr}"
        );
        let path = PathBuf::from("");
        assert_normalised_file(expected, &stdout, path, false);
        if expected_code == 0 {
            assert!(
                stderr.is_empty(),
                "expected stderr to be empty, got:\n{stderr}"
            )
        }
    }

    fn simple_snapshot_expect_error(
        args: Vec<OsString>,
        project: &str,
        expected: ExpectFile,
        buck: bool,
        file: Option<&str>,
    ) {
        if !buck || cfg!(feature = "buck") {
            let (mut args, path) = add_project(args, project, file, None);
            if !buck {
                args.push("--rebar".into());
            }
            let (stdout, stderr, code) = elp(args);
            assert_eq!(
                code, 101,
                "Expected exit code 101, got: {code}\nstdout:\n{stdout}\nstderr:\n{stderr}"
            );
            assert_normalised_file(expected, &stdout, path, false);
        }
    }

    fn simple_snapshot_expect_stderror(
        args: Vec<OsString>,
        project: &str,
        expected: ExpectFile,
        buck: bool,
        file: Option<&str>,
        normalise_urls: bool,
    ) {
        if !buck || cfg!(feature = "buck") {
            let (mut args, path) = add_project(args, project, file, None);
            if !buck {
                args.push("--rebar".into());
            }
            let (stdout, stderr, code) = elp(args);
            assert_eq!(
                code, 101,
                "Expected exit code 101, got: {code}\nstdout:\n{stdout}\nstderr:\n{stderr}"
            );
            assert_normalised_file(expected, &stderr, path, normalise_urls);
        }
    }

    #[track_caller]
    fn simple_snapshot_output_contains(
        args: Vec<OsString>,
        project: &str,
        expected_patterns: &[&str],
        buck: bool,
        file: Option<&str>,
        expected_code: i32,
    ) {
        if !buck || cfg!(feature = "buck") {
            let (mut args, _path) = add_project(args, project, file, None);
            if !buck {
                args.push("--rebar".into());
            }
            let (stdout, stderr, code) = elp(args);
            assert_eq!(
                code, expected_code,
                "Expected exit code {expected_code}, got: {code}\nstdout:\n{stdout}\nstderr:\n{stderr}"
            );

            if expected_code == 0 {
                assert!(stderr.is_empty(), "Expected empty stderr, got:\n{stderr}");
            }

            for pattern in expected_patterns {
                assert!(
                    stdout.contains(pattern),
                    "Expected stdout to contain '{}', but got:\n{}",
                    pattern,
                    stdout
                );
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn check_lint_fix(
        args: Vec<OsString>,
        project: &str,
        expected: ExpectFile,
        expected_code: i32,
        buck: bool,
        file: Option<&str>,
        actual_dir: &Path,
        expected_dir: &Path,
        files: &[(&str, &str)],
        backup_files: bool,
    ) -> Result<()> {
        check_lint_fix_stderr(
            args,
            project,
            expected,
            expected_code,
            buck,
            file,
            actual_dir,
            expected_dir,
            files,
            backup_files,
            None,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn check_lint_fix_stderr(
        args: Vec<OsString>,
        project: &str,
        expected: ExpectFile,
        expected_code: i32,
        buck: bool,
        file: Option<&str>,
        actual_dir: &Path,
        expected_dir: &Path,
        files: &[(&str, &str)],
        backup_files: bool,
        expected_stderr: Option<Expect>,
    ) -> Result<()> {
        if !buck || cfg!(feature = "buck") {
            let (mut args, path) = add_project(args, project, file, None);
            if !buck {
                args.push("--rebar".into());
            }
            let orig_files = files.iter().map(|x| x.0).collect::<Vec<_>>();
            // Take a backup. The Drop instance will restore at the end
            let _backup = if backup_files {
                BackupFiles::save_files(project, &orig_files)
            } else {
                BackupFiles::save_files(project, &[])
            };
            let (stdout, stderr, code) = elp(args);
            assert_eq!(
                code, expected_code,
                "Expected exit code {expected_code}, got: {code}\nstdout:\n{stdout}\nstderr:\n{stderr}"
            );
            if let Some(expected_stderr) = expected_stderr {
                expected_stderr.assert_eq(&stderr);
            } else {
                expect![[""]].assert_eq(&stderr);
            }
            assert_normalised_file(expected, &stdout, path, false);
            for (expected_file, file) in files {
                let expected = expect_file!(expected_dir.join(expected_file));
                let actual = actual_dir.join(file);
                assert!(actual.exists());
                let content = fs::read_to_string(actual).unwrap();
                expected.assert_eq(content.as_str());
            }
        }
        Ok(())
    }

    fn assert_normalised_file(
        expected: ExpectFile,
        actual: &str,
        project_path: PathBuf,
        normalise_urls: bool,
    ) {
        let project_path: &str = &project_path.to_string_lossy();
        let normalised = actual
            .replace(project_path, "{project_path}")
            .replace(BASE_URL, "");
        let normalised = if normalise_urls {
            replace_url(&normalised)
        } else {
            normalised
        };

        expected.assert_eq(&normalised);
    }

    fn replace_url(s: &str) -> String {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"https[^ ]+").unwrap();
        }
        if let Some(res) = RE.find(s) {
            s.replace(res.as_str(), "https://[URL]")
        } else {
            s.to_string()
        }
    }

    fn add_project(
        mut args: Vec<OsString>,
        project: &str,
        file: Option<&str>,
        json: Option<&str>,
    ) -> (Vec<OsString>, PathBuf) {
        let path_str = project_path(project);
        let project_path: PathBuf = path_str.clone().into();
        args.push("--project".into());
        if let Some(json_file) = json {
            let full_file = format!("{path_str}/{json_file}");
            args.push(full_file.into());
        } else {
            args.push(path_str.into());
        }
        if let Some(file) = file {
            args.push("--file".into());
            let file_path = project_path.join(file).into();
            args.push(file_path);
        }
        (args, project_path)
    }

    fn project_path(project: &str) -> String {
        format!("../../test_projects/{project}")
    }

    fn strip_ansi_codes(s: &str) -> String {
        lazy_static! {
            static ref ANSI_RE: Regex = Regex::new(r"\x1b\[[0-9;]*m").unwrap();
        }
        ANSI_RE.replace_all(s, "").to_string()
    }

    struct BackupFiles {
        // Restore the first Path to the second
        restore: Vec<(PathBuf, PathBuf)>,
        #[allow(dead_code)] // Reference to stop Drop handler
        temp_dir: TempDir,
    }
    impl BackupFiles {
        fn save_files(project: &str, files: &[&str]) -> Result<BackupFiles> {
            let path_str = project_path(project);
            let project_path: PathBuf = path_str.into();
            let mut restore = Vec::default();
            let temp_dir = tempfile::tempdir().unwrap();
            files.iter().for_each(|file| {
                let file_path = project_path.join(file);
                assert!(file_path.exists());
                let bak_file_path = temp_dir.path().join(file);
                let parent = bak_file_path.parent().unwrap();
                fs::create_dir_all(parent).unwrap();
                assert!(!bak_file_path.exists());
                fs::copy(file_path.clone(), bak_file_path.clone()).expect("Failed to copy file");
                restore.push((bak_file_path, file_path));
            });
            Ok(BackupFiles { restore, temp_dir })
        }
    }

    impl Drop for BackupFiles {
        fn drop(&mut self) {
            self.restore.iter().for_each(|(from, to)| {
                assert!(from.clone().exists());
                fs::copy(from, to).expect("Failed to restore file");
                fs::remove_file(from).expect("Failed to delete file");
            });
        }
    }
}
