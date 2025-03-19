/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::LazyLock;

use elp_types_db::StringId;
use fxhash::FxHashSet;

use crate::ast;

pub const FAKE_MODULE: LazyLock<StringId> = LazyLock::new(|| StringId::from("$compiler_macro"));

pub const FUNS: LazyLock<FxHashSet<ast::Id>> = LazyLock::new(|| {
    FxHashSet::from_iter([ast::Id {
        name: StringId::from("record_info"),
        arity: 2,
    }])
});

pub fn is_compiler_macro(id: &ast::Id) -> bool {
    FUNS.contains(id)
}
