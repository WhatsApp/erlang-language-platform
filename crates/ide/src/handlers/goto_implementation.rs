/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Backs the `textDocument/implementation` request.
//!
//! Go-to-definition answers "where is this symbol declared"; go-to-implementation
//! answers "what code runs". The two diverge wherever a call is dispatched rather
//! than statically bound, and only the latter may follow a dispatch.

use elp_ide_db::RootDatabase;
use elp_ide_db::elp_base_db::FilePosition;

use crate::RangeInfo;
// @fb-only: use crate::meta_only;
use crate::navigation_target::NavigationTarget;

#[rustfmt::skip]
// @fb-only: pub(crate) fn goto_implementation(db: &RootDatabase, position: FilePosition) -> Option<RangeInfo<Vec<NavigationTarget>>> {
pub(crate) fn goto_implementation(_db: &RootDatabase, _position: FilePosition) -> Option<RangeInfo<Vec<NavigationTarget>>> { // @oss-only
    // @fb-only: meta_only::goto_implementation::goto_implementation(db, position)
    None // @oss-only
}
