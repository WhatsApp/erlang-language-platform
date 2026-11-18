/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! This module contains functions for editing syntax trees. As the trees are
//! immutable, all function here return a fresh copy of the tree, instead of
//! doing an in-place modification.
use std::fmt;
use std::iter;
use std::ops;

use rowan::TextRange;
use rowan::TextSize;

use crate::NodeOrToken;
use crate::SyntaxElement;
use crate::SyntaxKind::*;
use crate::SyntaxNode;
use crate::SyntaxToken;
use crate::ast::AstNode;
use crate::ted;
use crate::ted::make_whitespace;

#[derive(Debug, Clone, Copy)]
pub struct IndentLevel(pub u8);

impl From<u8> for IndentLevel {
    fn from(level: u8) -> IndentLevel {
        IndentLevel(level)
    }
}

impl fmt::Display for IndentLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let spaces = "                                        ";
        let buf;
        let len = self.0 as usize;
        let indent = if len <= spaces.len() {
            &spaces[..len]
        } else {
            buf = " ".repeat(len);
            &buf
        };
        fmt::Display::fmt(indent, f)
    }
}

impl ops::Add<u8> for IndentLevel {
    type Output = IndentLevel;
    fn add(self, rhs: u8) -> IndentLevel {
        IndentLevel(self.0 + rhs)
    }
}

impl ops::Add<IndentLevel> for IndentLevel {
    type Output = IndentLevel;
    fn add(self, rhs: IndentLevel) -> IndentLevel {
        IndentLevel(self.0 + rhs.0)
    }
}

impl ops::Sub<u8> for IndentLevel {
    type Output = IndentLevel;
    fn sub(self, rhs: u8) -> IndentLevel {
        IndentLevel(self.0 - rhs)
    }
}

impl ops::Sub<IndentLevel> for IndentLevel {
    type Output = IndentLevel;
    fn sub(self, rhs: IndentLevel) -> IndentLevel {
        IndentLevel(self.0 - rhs.0)
    }
}

impl IndentLevel {
    pub fn single() -> IndentLevel {
        IndentLevel(0)
    }
    pub fn is_zero(&self) -> bool {
        self.0 == 0
    }
    pub fn from_element(element: &SyntaxElement) -> IndentLevel {
        match element {
            rowan::NodeOrToken::Node(it) => IndentLevel::from_node(it),
            rowan::NodeOrToken::Token(it) => IndentLevel::from_token(it),
        }
    }

    pub fn from_node(node: &SyntaxNode) -> IndentLevel {
        match node.first_token() {
            Some(it) => Self::from_token(&it),
            None => IndentLevel(0),
        }
    }

    pub fn from_token(token: &SyntaxToken) -> IndentLevel {
        for ws in prev_tokens(token.clone())
            .skip(1)
            .filter(|t| t.kind() == WHITESPACE)
        {
            let text = ws.text();
            if let Some(pos) = text.rfind('\n') {
                let level = text[pos + 1..].chars().count();
                return IndentLevel(level as u8);
            }
        }
        IndentLevel(0)
    }

    pub(super) fn increase_indent(self, node: &SyntaxNode) {
        let tokens = node.preorder_with_tokens().filter_map(|event| match event {
            rowan::WalkEvent::Leave(NodeOrToken::Token(it)) => Some(it),
            _ => None,
        });
        for token in tokens {
            if token.kind() == WHITESPACE && token.text().contains('\n') {
                let new_ws = make_whitespace(&format!("{}{}", token.text(), self));
                ted::replace(token, &new_ws);
            }
        }
    }

    pub(super) fn decrease_indent(self, node: &SyntaxNode) {
        let tokens = node.preorder_with_tokens().filter_map(|event| match event {
            rowan::WalkEvent::Leave(NodeOrToken::Token(it)) => Some(it),
            _ => None,
        });
        for token in tokens {
            if token.kind() == WHITESPACE && token.text().contains('\n') {
                let new_ws = make_whitespace(&token.text().replace(&format!("\n{self}"), "\n"));
                ted::replace(token, &new_ws);
            }
        }
    }
}

/// Given a token, return the `TextSize` of the preceding start of
/// line.
pub fn start_of_line(token: &SyntaxToken) -> TextSize {
    for ws in prev_tokens(token.clone())
        .skip(1)
        .filter(|t| t.kind() == WHITESPACE)
    {
        let text = ws.text();
        if let Some(pos) = text.rfind('\n') {
            return ws.text_range().start() + TextSize::from(pos as u32);
        }
    }
    token.text_range().start()
}

/// Given a token, return the range of its line, from the start of the line
/// to its last character that is not whitespace.
pub fn line_range(token: &SyntaxToken) -> TextRange {
    let start = prev_tokens(token.clone())
        .skip(1)
        .filter(|t| t.kind() == WHITESPACE)
        .find_map(|ws| {
            let pos = ws.text().rfind('\n')?;
            Some(ws.text_range().start() + TextSize::from(pos as u32 + 1))
        })
        .unwrap_or_default();
    let end = next_tokens(token.clone())
        .take_while(|t| !(t.kind() == WHITESPACE && t.text().contains('\n')))
        .filter(|t| t.kind() != WHITESPACE)
        .last()
        .map_or(token.text_range().end(), |t| {
            t.text_range().start() + TextSize::of(t.text().trim_end())
        });
    TextRange::new(start, end)
}

pub fn next_tokens(token: SyntaxToken) -> impl Iterator<Item = SyntaxToken> {
    iter::successors(Some(token), |token| token.next_token())
}

pub fn prev_tokens(token: SyntaxToken) -> impl Iterator<Item = SyntaxToken> {
    iter::successors(Some(token), |token| token.prev_token())
}

/// Soft-deprecated in favor of mutable tree editing API `edit_in_place::Ident`.
pub trait AstNodeEdit: AstNode + Clone + Sized {
    fn indent_level(&self) -> IndentLevel {
        IndentLevel::from_node(self.syntax())
    }
    #[must_use]
    fn indent(&self, level: IndentLevel) -> Self {
        fn indent_inner(node: &SyntaxNode, level: IndentLevel) -> SyntaxNode {
            let res = node.clone_subtree().clone_for_update();
            level.increase_indent(&res);
            res.clone_subtree()
        }

        Self::cast(indent_inner(self.syntax(), level)).expect("indent should preserve node type")
    }
    #[must_use]
    fn dedent(&self, level: IndentLevel) -> Self {
        fn dedent_inner(node: &SyntaxNode, level: IndentLevel) -> SyntaxNode {
            let res = node.clone_subtree().clone_for_update();
            level.decrease_indent(&res);
            res.clone_subtree()
        }

        Self::cast(dedent_inner(self.syntax(), level)).expect("dedent should preserve node type")
    }
    #[must_use]
    fn reset_indent(&self) -> Self {
        let level = IndentLevel::from_node(self.syntax());
        self.dedent(level)
    }
}

impl<N: AstNode + Clone> AstNodeEdit for N {}

#[cfg(test)]
mod tests {

    use super::AstNodeEdit;
    use super::IndentLevel;
    use super::line_range;
    use crate::SourceFile;
    use crate::TextSize;
    use crate::ast::AstNode;

    #[test]
    fn test_increase_indent() {
        let source = SourceFile::parse_text(
            r#"
    foo() ->
        K = 1,
        M = 1,
        M + 1.
    "#,
        );
        let tree = source.tree();
        // Note: we can't use expect! here, it does its own indent management
        assert_eq!(
            r#"
    foo() ->
        K = 1,
        M = 1,
        M + 1.
    "#,
            tree.syntax().to_string().as_str()
        );

        let indented = tree.indent(IndentLevel(8));
        assert_eq!(
            r#"
            foo() ->
                K = 1,
                M = 1,
                M + 1.
            "#,
            indented.syntax().to_string().as_str()
        );
    }

    #[track_caller]
    fn check_line_range(text: &str, token_text: &str, expected: &str) {
        let source = SourceFile::parse_text(text);
        let offset = text
            .find(token_text)
            .expect("token text should be in the source");
        let token = source
            .tree()
            .syntax()
            .token_at_offset(TextSize::from(offset as u32))
            .right_biased()
            .expect("there should be a token at the offset");
        assert_eq!(expected, &text[line_range(&token)]);
    }

    #[test]
    fn test_line_range_first_line() {
        check_line_range("-define(A, 1).\n", "define", "-define(A, 1).");
    }

    #[test]
    fn test_line_range_below_comments() {
        check_line_range(
            "%% Copyright\n\n  -define(A, 1). % Trailing  \r\n-define(B, 2).\n",
            "A,",
            "  -define(A, 1). % Trailing",
        );
    }

    #[test]
    fn test_line_range_last_line_without_newline() {
        check_line_range("-define(A, 1).\n-define(B, 2).  ", "B,", "-define(B, 2).");
    }
}
