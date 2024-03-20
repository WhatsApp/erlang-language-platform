/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::EqwalizerDiagnostic;
use hir::Semantic;

use super::Diagnostic;

mod expected_type;

pub fn add_eqwalizer_assists(
    sema: &Semantic,
    file_id: FileId,
    d: &EqwalizerDiagnostic,
    diagnostic: &mut Diagnostic,
) {
    expected_type::expected_type(sema, file_id, d, diagnostic);
}
