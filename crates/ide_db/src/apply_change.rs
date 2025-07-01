/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Applies changes to the IDE state transactionally.

use elp_base_db::Change;
use elp_base_db::FileId;
use paths::AbsPathBuf;

use crate::RootDatabase;

impl RootDatabase {
    pub fn apply_change(
        &mut self,
        change: Change,
        resolve_file_id: &impl Fn(&AbsPathBuf) -> Option<FileId>,
    ) {
        let _p = tracing::info_span!("RootDatabase::apply_change").entered();
        self.request_cancellation();
        log::info!("apply_change {:?}", change);
        change.apply(self, resolve_file_id);
    }
}
