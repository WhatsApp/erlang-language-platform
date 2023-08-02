/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::fixture::WithFixture;
use elp_ide_db::RootDatabase;

use crate::Completion;

pub(crate) fn render_completions(completions: Vec<Completion>) -> String {
    completions
        .iter()
        .map(|completion| format!("{}", completion))
        .collect::<Vec<_>>()
        .join("\n")
}

pub(crate) fn get_completions(code: &str, trigger_character: Option<char>) -> Vec<Completion> {
    let (db, position) = RootDatabase::with_position(code);
    crate::completions(&db, position, trigger_character)
}
