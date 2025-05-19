/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Types for use in HIR but which can also be produced by other code
//! intelligence sources, such as Eqwalizer, OTP compiler,
//! common_test.
//!
//! This crate carries the types only, so they can be used in
//! hir::Semantic, without concern for the messy details of actually
//! retrieving them.
//!
//! This also allows us to set up test fixtures to populate them
//! without running heavyweight processes.

pub mod eqwalizer;
use std::sync::Arc;

use elp_base_db::FileId;
use elp_base_db::FileRange;
pub use ustr::Ustr as StringId;

pub trait TypedSemantic {
    fn eqwalizer_diagnostics(&self, file_id: FileId)
    -> Option<Vec<eqwalizer::EqwalizerDiagnostic>>;

    fn eqwalizer_type_at_position(
        &self,
        range: FileRange,
    ) -> Option<Arc<(eqwalizer::types::Type, FileRange)>>;
}
