/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Applies changes to the IDE state transactionally.

use elp_base_db::Change;

use crate::RootDatabase;

impl RootDatabase {
    pub fn apply_change(&mut self, change: Change) {
        let _p = tracing::info_span!("RootDatabase::apply_change").entered();
        self.request_cancellation();
        log::info!("apply_change {:?}", change);
        change.apply(self);
    }
}
