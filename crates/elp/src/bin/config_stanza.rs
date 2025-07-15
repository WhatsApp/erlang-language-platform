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
use elp::config::Config;

use crate::args::ConfigStanza;

pub fn config_stanza(_args: &ConfigStanza, cli: &mut dyn Cli) -> Result<()> {
    let schema = format!("{:#}", Config::json_schema());
    Ok(writeln!(cli, "{schema}")?)
}
