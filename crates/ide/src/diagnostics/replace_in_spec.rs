/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint/fix: replace_in_spec
//!
//! Return a diagnostic for the spec of a given function, which has a
//! specified type replacement in it

use elp_ide_db::elp_base_db::FileId;
use hir::Semantic;
use serde::Deserialize;
use serde::Serialize;

use super::replace_call::DiagnosticBuilder;
use super::Diagnostic;
use crate::MFA;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum TypeReplacement {
    TypeAliasWithString { from: MFA, to: String },
}

#[allow(unused_variables)]
pub fn replace_in_spec(
    functions: &[MFA],
    replace: &TypeReplacement,
    diagnostic_builder: DiagnosticBuilder,
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
}
