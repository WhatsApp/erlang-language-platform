/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Various traits that are implemented by ast nodes.
//!
//! The implementations are usually trivial, and live in generated.rs

use super::HasArity;
use crate::ast;
use crate::ast::support;
use crate::ast::AstNode;

pub trait HasLabel: AstNode {
    fn label(&self) -> Option<String> {
        let name: ast::Name = support::child(self.syntax(), 0)?;
        name.text()
    }
}

impl HasLabel for ast::FunDecl {
    fn label(&self) -> Option<String> {
        let name = self.name()?.text()?;
        let arity = self.arity_value()?;
        Some(format!("{}/{}", name, arity))
    }
}
impl HasLabel for ast::RecordDecl {}
impl HasLabel for ast::TypeAlias {
    fn label(&self) -> Option<String> {
        self.name()?.name()?.text()
    }
}
impl HasLabel for ast::PpDefine {
    fn label(&self) -> Option<String> {
        let name = self.name()?.text()?;
        if let Some(arity) = self.arity_value() {
            Some(format!("{}/{}", name, arity))
        } else {
            Some(name)
        }
    }
}
