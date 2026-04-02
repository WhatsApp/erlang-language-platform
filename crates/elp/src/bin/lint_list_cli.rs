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
use elp::cli::Cli;
use elp_ide::elp_ide_db::DiagnosticCode;

use crate::args::LintList;

pub fn lint_list(_args: &LintList, cli: &mut dyn Cli) -> Result<()> {
    for code in DiagnosticCode::all_diagnostic_codes() {
        writeln!(cli, "{}", code.as_code())?;
    }
    Ok(())
}
