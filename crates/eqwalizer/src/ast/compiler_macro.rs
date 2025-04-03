/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeSet;

use lazy_static::lazy_static;

use crate::ast;

pub static FAKE_MODULE: &str = "$compiler_macro";

lazy_static! {
    static ref FUNS: BTreeSet<ast::Id> = {
        vec![ast::Id {
            name: "record_info".into(),
            arity: 2,
        }]
        .into_iter()
        .collect()
    };
}

pub fn is_compiler_macro(id: &ast::Id) -> bool {
    FUNS.contains(id)
}
