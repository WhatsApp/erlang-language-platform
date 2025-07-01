/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Various traits that are implemented by ast nodes.
//!
//! The implementations are usually trivial, and live in generated.rs

use super::HasArity;
use crate::ast;
use crate::ast::AstNode;
use crate::ast::support;
use crate::label::Label;

pub trait HasLabel: AstNode {
    fn label(&self) -> Option<Label> {
        let name: ast::Name = support::child(self.syntax(), 0)?;
        name.text().map(Label::new_raw)
    }
}

impl HasLabel for ast::FunDecl {
    fn label(&self) -> Option<Label> {
        let name = self.name()?.text()?;
        let arity = self.arity_value()?;
        Some(Label::new_raw(format!("{}/{}", name, arity)))
    }
}
impl HasLabel for ast::RecordDecl {}
impl HasLabel for ast::TypeAlias {
    fn label(&self) -> Option<Label> {
        self.name()?.name()?.text().map(Label::new_raw)
    }
}
impl HasLabel for ast::PpDefine {
    fn label(&self) -> Option<Label> {
        let name = self.name()?.text()?;
        if let Some(arity) = self.arity_value() {
            Some(Label::new_raw(format!("{}/{}", name, arity)))
        } else {
            Some(Label::new_raw(name))
        }
    }
}
