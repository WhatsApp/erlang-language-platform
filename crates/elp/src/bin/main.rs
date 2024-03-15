/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process;
use std::sync::Once;

use anyhow::Result;
use bpaf::batteries;
use elp::cli;
use elp::cli::Cli;
use elp::ServerSetup;
use elp_log::timeit;
use elp_log::FileLogger;
use elp_log::Logger;
use elp_project_model::eqwalizer_support;
use include_dir::include_dir;
use include_dir::Dir;
use lsp_server::Connection;

mod args;
mod build_info_cli;
mod elp_parse_cli;
mod eqwalizer_cli;
mod erlang_service_cli;
mod explain_cli;
mod glean;
mod lint_cli;
mod reporting;
mod shell;

// Use jemalloc as the global allocator
#[cfg(not(target_env = "msvc"))]
use tikv_jemallocator::Jemalloc;

use crate::args::Args;

#[cfg(not(target_env = "msvc"))]
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
    let mut cli = cli::Real::default();
    let args = args::args().run();
    let res = try_main(&mut cli, args);
    let code = handle_res(res, cli.err());
    process::exit(code);
}

fn handle_res(result: Result<()>, stderr: &mut dyn Write) -> i32 {
    if let Err(err) = result {
        writeln!(stderr, "{:#}", err).unwrap();
        101
    } else {
        0
    }
}

fn try_main(cli: &mut dyn Cli, args: Args) -> Result<()> {
    let logger = setup_logging(args.log_file, args.no_log_buffering)?;
    if let Err(err) = eqwalizer_support::setup_eqwalizer_support(&EQWALIZER_SUPPORT_DIR) {
        log::warn!("Failed to setup eqwalizer_support: {}", err);
    }
    INIT.call_once(setup_thread_pool);
    match args.command {
        args::Command::RunServer(_) => run_server(logger)?,
        args::Command::ParseAll(args) => erlang_service_cli::parse_all(&args, cli)?,
        args::Command::ParseAllElp(args) => elp_parse_cli::parse_all(&args, cli)?,
        args::Command::Eqwalize(args) => eqwalizer_cli::eqwalize_module(&args, cli)?,
        args::Command::EqwalizeAll(args) => eqwalizer_cli::eqwalize_all(&args, cli)?,
        args::Command::EqwalizeApp(args) => eqwalizer_cli::eqwalize_app(&args, cli)?,
        args::Command::EqwalizeStats(args) => eqwalizer_cli::eqwalize_stats(&args, cli)?,
        args::Command::EqwalizeTarget(args) => eqwalizer_cli::eqwalize_target(&args, cli)?,
        args::Command::BuildInfo(args) => build_info_cli::save_build_info(args)?,
        args::Command::ProjectInfo(args) => build_info_cli::save_project_info(args)?,
        args::Command::Lint(args) => lint_cli::lint_all(&args, cli)?,
        args::Command::GenerateCompletions(args) => {
            let instructions = args::gen_completions(&args.shell);
            writeln!(cli, "#Please run this:\n{}", instructions)?
        }
        args::Command::Version(_) => writeln!(cli, "elp {}", elp::version())?,
        args::Command::Shell(args) => shell::run_shell(&args, cli)?,
        args::Command::Help() => {
            let help = batteries::get_usage(args::args());
            writeln!(cli, "{}", help)?
        }
        args::Command::Explain(args) => explain_cli::explain(&args, cli)?,
        args::Command::Glean(args) => glean::GleanIndexer::new(&args, cli)?.index()?,
    }

    log::logger().flush();

    Ok(())
}

fn setup_logging(log_file: Option<PathBuf>, no_buffering: bool) -> Result<Logger> {
    env::set_var("RUST_BACKTRACE", "short");

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

fn setup_thread_pool() -> () {
    if let Err(err) = rayon::ThreadPoolBuilder::new()
        .stack_size(THREAD_STACK_SIZE)
        .build_global()
    {
        log::warn!("Failed to setup thread pool: {}", err);
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
    use std::ffi::OsString;
    use std::path::Path;
    use std::str;

    use anyhow::Context;
    use bpaf::Args;
    use elp::build;
    use elp::build::load;
    use elp::cli::Fake;
    use elp_eqwalizer::EqwalizerConfig;
    use elp_eqwalizer::EqwalizerDiagnostics;
    use elp_eqwalizer::Mode;
    use elp_ide::elp_ide_db::diagnostic_code::BASE_URL;
    use elp_ide::elp_ide_db::elp_base_db::FileId;
    use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
    use elp_project_model::AppName;
    use elp_project_model::DiscoverConfig;
    use expect_test::expect;
    use expect_test::expect_file;
    use expect_test::Expect;
    use expect_test::ExpectFile;
    use rayon::prelude::*;
    use tempfile::Builder;
    use tempfile::TempDir;
    use test_case::test_case;

    use super::reporting::Reporter;
    use super::*;

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
        let project_path = format!("../../test_projects/{}", project);
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
        if !buck || cfg!(feature = "buck") {
            let mut args = args_vec!["eqwalize", module];
            if !buck {
                args.push("--rebar".into());
            }
            let (args, path) = add_project(args, project, None, None);
            let fast_str = if fast { "_fast" } else { "" };
            let exp_path = expect_file!(format!(
                "../resources/test/{}/eqwalize_{}{}.pretty",
                project, module, fast_str
            ));

            let (stdout, stderr, code) = elp(args);
            match code {
                0 => {
                    assert_normalised_file(exp_path, &stdout, path);
                    assert!(stderr.is_empty());
                }
                _ => {
                    assert_normalised_file(exp_path, &stderr, path);
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
        if !buck || cfg!(feature = "buck") {
            let mut cli = Fake::default();
            let project_config = DiscoverConfig::new(!buck, "test");
            let str_path = project_path(project);
            let project_path: &Path = Path::new(&str_path);
            let mut loaded = load::load_project_at(
                &mut cli,
                project_path,
                project_config,
                IncludeOtp::Yes,
                Mode::Cli,
            )
            .with_context(|| format!("Failed to load project at {}", str_path))
            .unwrap();
            loaded
                .analysis_host
                .raw_database_mut()
                .set_eqwalizer_config(config);
            build::compile_deps(&loaded, &mut cli)
                .with_context(|| format!("Failed to compile deps for project {}", project))
                .unwrap();

            let analysis = loaded.analysis();
            let module_index = analysis
                .module_index(loaded.project_id)
                .with_context(|| format!("No module index for project {}", project))
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
            let chunk_size = (files_count + 3) / 4;
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
                                .write_eqwalizer_diagnostics(file_id, &diagnostics)
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

                        let exp_path = expect_file!(format!(
                            "../resources/test/{}/{}/{}.pretty",
                            project,
                            app,
                            module.as_str()
                        ));
                        let (stdout, _) = cli.to_strings();
                        assert_normalised_file(exp_path, &stdout, project_path.into());
                    }
                }
                EqwalizerDiagnostics::NoAst { module } => {
                    panic!(
                        "Could not run tests because module {} was not found",
                        module
                    )
                }
                EqwalizerDiagnostics::Error(error) => {
                    panic!("Could not run tests: {}", error)
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
        simple_snapshot(
            args_vec!["eqwalize-all", "--format", "json"],
            "standard",
            expect_file!("../resources/test/standard/eqwalize_all_diagnostics.jsonl"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_all_diagnostics_match_snapshot_jsonl_gen(buck: bool) {
        simple_snapshot(
            args_vec!["eqwalize-all", "--format", "json", "--include-generated"],
            "standard",
            expect_file!("../resources/test/standard/eqwalize_all_diagnostics_gen.jsonl"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_all_diagnostics_match_snapshot_pretty(buck: bool) {
        simple_snapshot(
            args_vec!["eqwalize-all"],
            "standard",
            expect_file!("../resources/test/standard/eqwalize_all_diagnostics.pretty"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_app_diagnostics_match_snapshot_pretty(buck: bool) {
        simple_snapshot(
            args_vec!["eqwalize-app", "app_a",],
            "standard",
            expect_file!("../resources/test/standard/eqwalize_app_diagnostics.pretty"),
            buck,
            None,
        );
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
        simple_snapshot(
            args_vec!["eqwalize-app", "app_a", "--include-generated",],
            "standard",
            expect_file!("../resources/test/standard/eqwalize_app_diagnostics_gen.pretty"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn eqwalize_all_fails_on_bad_parse(buck: bool) {
        simple_snapshot(
            args_vec!["eqwalize-all", "--format", "json",],
            "parse_error",
            expect_file!("../resources/test/standard/eqwalize_all_parse_error.jsonl"),
            buck,
            None,
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
    fn parse_all_diagnostics_force_missing_spec_all(buck: bool) {
        simple_snapshot(
            args_vec![
                "parse-elp",
                "--module",
                "erlang_diagnostics_force_warn_missing_spec_all",
                "--force-warn-missing-spec-all"
            ],
            "diagnostics",
            expect_file!(
                "../resources/test/diagnostics/parse_all_diagnostics_force_warn_missing_spec_all.jsonl"
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

    #[test]
    fn build_info_json_buck() {
        if cfg!(feature = "buck") {
            let tmp_dir = TempDir::new().expect("Could not create temporary directory");
            let tmp_path = tmp_dir.path();
            let tmp_file = tmp_path.join("test_build_info.json");
            fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
            let project = "diagnostics";
            let path_str = project_path(project);
            let args = args_vec![
                "build-info",
                "--to",
                tmp_file.clone(),
                "--json",
                "--project",
                path_str
            ];
            let (stdout, stderr, code) = elp(args);
            assert_eq!(
                code, 0,
                "failed with unexpected exit code: got {} not {}\nstdout:\n{}\nstderr:\n{}",
                code, 0, stdout, stderr
            );
            assert_eq!(
                stderr.is_empty(),
                true,
                "expected stderr to be empty, got:\n{}",
                stderr
            );
            assert!(PathBuf::from(tmp_file.clone()).exists());
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
    fn build_info_json_not_buck() {
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        let tmp_file = tmp_path.join("test_build_info.json");
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
        let project = "diagnostics";
        let path_str = format!("{}/test_build_info.json", project_path(project));
        let args = args_vec![
            "build-info",
            "--to",
            tmp_file.clone(),
            "--json",
            "--project",
            path_str
        ];
        let (stdout, stderr, code) = elp(args);
        assert_eq!(
            code, 0,
            "failed with unexpected exit code: got {} not {}\nstdout:\n{}\nstderr:\n{}",
            code, 0, stdout, stderr
        );
        assert_eq!(
            stderr.is_empty(),
            true,
            "expected stderr to be empty, got:\n{}",
            stderr
        );
        assert!(PathBuf::from(tmp_file.clone()).exists());
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

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_1(buck: bool) {
        simple_snapshot_expect_error(
            args_vec!["lint", "--module", "lints", "--diagnostic-filter", "P1700",],
            "diagnostics",
            expect_file!("../resources/test/diagnostics/parse_elp_lint1.stdout"),
            buck,
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
    fn lint_recursive(buck: bool) {
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
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
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
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
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
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
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
        check_lint_fix(
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
        )
        .expect("bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_custom_config_file_invalid(buck: bool) {
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
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
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
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
    fn lint_config_file_parse_error(buck: bool) {
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
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
    fn lint_no_diagnostics_enabled(buck: bool) {
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
        simple_snapshot_expect_stderror(
            args_vec!["lint", "--experimental",],
            "linter",
            expect_file!("../resources/test/linter/parse_elp_no_lint_output.stdout"),
            buck,
            None,
        );
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_json_output(buck: bool) {
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
        check_lint_fix(
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
        )
        .expect("bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_applies_fix_using_to_dir(buck: bool) {
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
        check_lint_fix(
            args_vec![
                "lint",
                "--module",
                "lints",
                "--diagnostic-filter",
                "P1700",
                "--to",
                tmp_path,
                "--apply-fix"
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
        )
        .expect("Bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_applies_fix_using_to_dir_json_output(buck: bool) {
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
        check_lint_fix(
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
        check_lint_fix(
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
        )
        .expect("Bad test");
    }

    #[test_case(false ; "rebar")]
    #[test_case(true  ; "buck")]
    fn lint_applies_ignore_fix_if_requested(buck: bool) {
        let tmp_dir = TempDir::new().expect("Could not create temporary directory");
        let tmp_path = tmp_dir.path();
        fs::create_dir_all(tmp_path).expect("Could not create temporary directory path");
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
        simple_snapshot(
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
    fn eqwalizer_tests_check_gradual(buck: bool) {
        eqwalize_all_snapshots(
            "eqwalizer_tests",
            "check_gradual",
            buck,
            EqwalizerConfig {
                gradual_typing: Some(true),
                ..EqwalizerConfig::default_test()
            },
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
                check_redundant_guards: Some(true),
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
            EqwalizerConfig {
                gradual_typing: Some(true),
                occurrence_typing: Some(true),
                fault_tolerance: Some(true),
                ..EqwalizerConfig::default_test()
            },
        );
    }

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
                "failed with unexpected exit code: got {} not {}\nstdout:\n{}\nstderr:\n{}",
                code, expected_code, stdout, stderr
            );
            assert_normalised_file(expected, &stdout, path);
            if expected_code == 0 {
                assert_eq!(
                    stderr.is_empty(),
                    true,
                    "expected stderr to be empty, got:\n{}",
                    stderr
                )
            }
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
                "Expected exit code 101, got: {}\nstdout:\n{}\nstderr:\n{}",
                code, stdout, stderr
            );
            assert_normalised_file(expected, &stdout, path);
        }
    }

    fn simple_snapshot_expect_stderror(
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
                "Expected exit code 101, got: {}\nstdout:\n{}\nstderr:\n{}",
                code, stdout, stderr
            );
            assert_normalised_file(expected, &stderr, path);
        }
    }

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
                "Expected exit code {expected_code}, got: {}\nstdout:\n{}\nstderr:\n{}",
                code, stdout, stderr
            );
            if let Some(expected_stderr) = expected_stderr {
                expected_stderr.assert_eq(&stderr);
            }
            assert_normalised_file(expected, &stdout, path);
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

    fn assert_normalised_file(expected: ExpectFile, actual: &str, project_path: PathBuf) {
        let project_path: &str = &project_path.to_string_lossy();
        let normalised = actual
            .replace(project_path, "{project_path}")
            .replace(BASE_URL, "");

        expected.assert_eq(&normalised);
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
            let full_file = format!("{}/{}", path_str, json_file);
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
        format!("../../test_projects/{}", project)
    }

    struct BackupFiles {
        // Restore the first Path to the second
        restore: Vec<(PathBuf, PathBuf)>,
    }
    impl BackupFiles {
        fn save_files(project: &str, files: &[&str]) -> Result<BackupFiles> {
            let path_str = project_path(project);
            let project_path: PathBuf = path_str.into();
            let mut restore = Vec::default();
            files.iter().for_each(|file| {
                let file_path = project_path.join(file);
                let bak_file_path = file_path.with_extension("bak");
                assert!(file_path.exists());
                assert!(!bak_file_path.exists());
                fs::copy(file_path.clone(), bak_file_path.clone()).expect("Failed to copy file");
                restore.push((bak_file_path, file_path));
            });
            Ok(BackupFiles { restore })
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
