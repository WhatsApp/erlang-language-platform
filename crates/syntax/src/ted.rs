/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Helpers for constructing syntax elements.
use std::sync::LazyLock;

use crate::Parse;
use crate::SourceFile;
use crate::SyntaxElement;
use crate::SyntaxKind;
use crate::SyntaxNode;
use crate::SyntaxToken;

/// Utility trait to allow calling `ted` functions with references or owned
/// nodes. Do not use outside of this module.
pub trait Element {
    fn syntax_element(self) -> SyntaxElement;
}

impl<E: Element + Clone> Element for &'_ E {
    fn syntax_element(self) -> SyntaxElement {
        self.clone().syntax_element()
    }
}
impl Element for SyntaxElement {
    fn syntax_element(self) -> SyntaxElement {
        self
    }
}
impl Element for SyntaxNode {
    fn syntax_element(self) -> SyntaxElement {
        self.into()
    }
}
impl Element for SyntaxToken {
    fn syntax_element(self) -> SyntaxElement {
        self.into()
    }
}

// ---------------------------------------------------------------------
// From rust-analyzer make.rs

static SOURCE_FILE: LazyLock<Parse<SourceFile>> =
    LazyLock::new(|| SourceFile::parse_text("foo() -> ok."));

pub fn single_space() -> SyntaxToken {
    SOURCE_FILE
        // .tree()
        .syntax_node()
        // .syntax()
        .descendants_with_tokens()
        .filter_map(|it| it.into_token())
        .find(|it| it.kind() == SyntaxKind::WHITESPACE && it.text() == " ")
        .unwrap()
}

pub fn make_whitespace(text: &str) -> SyntaxToken {
    assert!(text.trim().is_empty());
    let sf = SourceFile::parse_text(text).ok().unwrap();
    sf.syntax
        .first_child_or_token()
        .unwrap()
        .into_token()
        .unwrap()
}
