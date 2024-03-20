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

use crate::diagnostics::Diagnostic;

pub fn expected_type(
    _sema: &Semantic,
    _file_id: FileId,
    _d: &EqwalizerDiagnostic,
    _diagnostic: &mut Diagnostic,
) {
}
