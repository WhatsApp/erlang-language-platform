/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeSet;
use std::sync::LazyLock;

use crate::ast;

pub const FAKE_MODULE: &str = "$compiler_macro";

const FUNS: LazyLock<BTreeSet<ast::Id>> = LazyLock::new(|| {
    BTreeSet::from_iter([ast::Id {
        name: "record_info".into(),
        arity: 2,
    }])
});

pub fn is_compiler_macro(id: &ast::Id) -> bool {
    FUNS.contains(id)
}
