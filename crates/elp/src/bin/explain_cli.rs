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
use clap_complete::engine::ArgValueCandidates;
use elp::cli::Cli;
use elp_ide::diagnostics::DiagnosticCode;

use crate::args::diagnostic_code_candidates;

#[derive(Clone, Debug, clap::Args)]
pub struct Explain {
    /// Error code to explain
    #[arg(long, value_name = "CODE", add = ArgValueCandidates::new(diagnostic_code_candidates))]
    pub code: String,
}

pub fn explain(args: &Explain, cli: &mut dyn Cli) -> Result<()> {
    if let Some(code) = DiagnosticCode::maybe_from_string(&args.code)
        && let Some(uri) = DiagnosticCode::as_uri(&code)
    {
        let label = code.as_label();
        return Ok(writeln!(cli, "{uri} ({label})")?);
    }
    Ok(writeln!(cli, "Unkwnown code: {}", args.code)?)
}
