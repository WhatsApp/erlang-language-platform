/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Memory usage of the database's queries.

use crate::RootDatabase;
use crate::memory_usage::Bytes;

impl RootDatabase {
    pub fn per_query_memory_usage(&mut self) -> Vec<(String, Bytes, usize)> {
        // In salsa 0.19, the old Query/QueryTable/purge API is no longer available.
        // Per-query memory tracking is not yet supported.
        vec![]
    }
}
