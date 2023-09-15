/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Result;
use elp::cli::Cli;
use elp_ide::diagnostics::DiagnosticCode;

use crate::args::Explain;

pub fn explain(args: &Explain, cli: &mut dyn Cli) -> Result<()> {
    if let Some(code) = DiagnosticCode::maybe_from_string(&args.code) {
        if let Some(uri) = DiagnosticCode::as_uri(&code) {
            let label = code.as_label();
            return Ok(writeln!(cli, "{uri} ({label})")?);
        }
    }
    Ok(writeln!(cli, "Unkwnown code: {}", args.code)?)
}
