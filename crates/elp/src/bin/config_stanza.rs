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
use elp::config::Config;

use crate::args::ConfigStanza;

pub fn config_stanza(_args: &ConfigStanza, cli: &mut dyn Cli) -> Result<()> {
    let schema = format!("{:#}", Config::json_schema());
    Ok(writeln!(cli, "{}", schema)?)
}
