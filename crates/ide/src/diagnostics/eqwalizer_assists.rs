/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::EqwalizerDiagnostic;
use elp_ide_db::elp_base_db::FileId;
use hir::Semantic;

use super::Diagnostic;

mod expected_type;
mod unexported_type;

pub fn add_eqwalizer_assists(
    sema: &Semantic,
    file_id: FileId,
    d: &EqwalizerDiagnostic,
    diagnostic: &mut Diagnostic,
) {
    expected_type::expected_type(sema, file_id, d, diagnostic);
    unexported_type::unexported_type(sema, file_id, d, diagnostic);
}
