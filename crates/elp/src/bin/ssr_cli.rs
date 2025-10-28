/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use anyhow::Result;
use anyhow::bail;
use elp::cli::Cli;
use elp_ide::AnalysisHost;
use elp_ide::diagnostics;
use elp_ide::diagnostics::DiagnosticsConfig;
use elp_ide::diagnostics::FallBackToAll;
use elp_ide::diagnostics::LintConfig;
use elp_ide::diagnostics::MatchSsr;
use elp_project_model::buck::BuckQueryConfig;

use crate::args::Ssr;
use crate::lint_cli;

fn normalize_ssr_pattern(pattern: &str) -> String {
    if pattern.starts_with("ssr:") {
        pattern.to_string()
    } else {
        format!("ssr: {}.", pattern)
    }
}

pub fn run_ssr_command(
    args: &Ssr,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
) -> Result<()> {
    // Normalize the SSR pattern
    let normalized_pattern = normalize_ssr_pattern(&args.ssr_spec);

    // Validate the SSR pattern early
    let analysis_host = AnalysisHost::default();
    let analysis = analysis_host.analysis();
    match analysis.validate_ssr_pattern(&normalized_pattern) {
        Ok(Ok(())) => {}
        Ok(Err(e)) => bail!("invalid SSR pattern '{}': {}", args.ssr_spec, e),
        Err(_cancelled) => bail!("SSR pattern validation was cancelled"),
    }

    // Create the lint config with the SSR pattern
    let mut lint_config = LintConfig::default();
    let ssr_lint = diagnostics::Lint::LintMatchSsr(MatchSsr {
        ssr_pattern: normalized_pattern,
        message: None,
    });
    lint_config.ad_hoc_lints.lints.push(ssr_lint);

    // Build the diagnostics config
    let diagnostics_config = DiagnosticsConfig::default()
        .configure_diagnostics(
            &lint_config,
            &Some("ad-hoc: ssr-match".to_string()),
            &None,
            FallBackToAll::Yes,
        )?
        .set_include_generated(args.include_generated)
        .set_experimental(false)
        .set_use_cli_severity(false);

    if diagnostics_config.enabled.all_enabled() && args.is_format_normal() {
        writeln!(cli, "Reporting all diagnostics codes")?;
    }

    // Convert Ssr args to Lint args to reuse lint_cli functionality
    let lint_args = crate::args::Lint {
        project: args.project.clone(),
        module: args.module.clone(),
        app: args.app.clone(),
        file: args.file.clone(),
        rebar: args.rebar,
        profile: args.profile.clone(),
        include_generated: args.include_generated,
        include_tests: args.include_tests,
        print_diags: true,
        format: args.format.clone(),
        prefix: None,
        include_erlc_diagnostics: false,
        include_ct_diagnostics: false,
        include_edoc_diagnostics: false,
        include_eqwalizer_diagnostics: false,
        include_suppressed: false,
        use_cli_severity: false,
        diagnostic_ignore: None,
        diagnostic_filter: Some("ad-hoc: ssr-match".to_string()),
        experimental_diags: false,
        read_config: false,
        config_file: None,
        apply_fix: false,
        ignore_fix_only: false,
        in_place: false,
        to: None,
        recursive: false,
        with_check: false,
        check_eqwalize_all: false,
        one_shot: false,
        report_system_stats: args.report_system_stats,
        ignore_apps: vec![],
    };

    // Load the project
    let mut loaded = lint_cli::load_project(&lint_args, cli, query_config)?;

    // Run the codemod with the SSR pattern
    lint_cli::do_codemod(cli, &mut loaded, &diagnostics_config, &lint_args)
}

impl Ssr {
    pub fn is_format_normal(&self) -> bool {
        self.format.is_none()
    }
}
