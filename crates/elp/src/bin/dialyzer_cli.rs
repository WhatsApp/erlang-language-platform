/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::process::Command;
use std::process::Stdio;

use anyhow::Result;
use elp::cli::Cli;

use crate::args::DialyzeAll;

pub fn dialyze_all(args: &DialyzeAll, cli: &mut dyn Cli) -> Result<()> {
    do_dialyze_all(args, cli)
}

pub fn do_dialyze_all(_args: &DialyzeAll, _cli: &mut dyn Cli) -> Result<()> {
    let mut cmd = Command::new("/usr/bin/env");
    cmd.arg("bash")
        .arg("-c")
        .arg("dialyzer-run 2>&1")
        // We let the command act on stdio directly, since we don't need to access it
        .stdin(Stdio::inherit())
        .stderr(Stdio::inherit())
        .stdout(Stdio::inherit());

    let mut child = cmd.spawn()?;

    Ok(child.wait().map(|_exit_code| ())?)
}
