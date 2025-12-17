/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! A module with ide helpers for high-level ide features.

use elp_syntax::AstNode;
use elp_syntax::SourceFile;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::SyntaxToken;
use elp_syntax::TextSize;
use elp_syntax::TokenAtOffset;
use elp_syntax::ast;
use hir::FormList;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SnippetCap {
    _private: (),
}

impl SnippetCap {
    pub const fn new(allow_snippets: bool) -> Option<SnippetCap> {
        if allow_snippets {
            Some(SnippetCap { _private: () })
        } else {
            None
        }
    }
}

/// Picks the token with the highest rank returned by the passed in function.
pub fn pick_best_token(
    tokens: TokenAtOffset<SyntaxToken>,
    f: impl Fn(SyntaxKind) -> usize,
) -> Option<SyntaxToken> {
    tokens.max_by_key(move |t| f(t.kind()))
}

/// Given a syntax node, check it it is immediately enclosed in a call,
/// which can represent a function call or a type.
/// For a remote call, the node can be the module or the function name.
/// In the former case, there is an extra level of nesting, so we need
/// to check up to 3 steps up
pub fn get_call(syntax: &SyntaxNode) -> Option<ast::Call> {
    ast::Call::cast(syntax.parent()?)
        .or_else(|| ast::Call::cast(syntax.parent()?.parent()?))
        .or_else(|| ast::Call::cast(syntax.parent()?.parent()?.parent()?))
}

pub fn get_external_fun(syntax: &SyntaxNode) -> Option<ast::ExternalFun> {
    if let Some(external_fun) = ast::ExternalFun::cast(syntax.parent()?) {
        Some(external_fun)
    } else {
        ast::ExternalFun::cast(syntax.parent()?.parent()?)
    }
}

/// Find the first position at the top of the file to add a new
/// form. It will be just after the module attribute, if there is one.
pub fn top_insert_position(form_list: &FormList, source: &SourceFile) -> TextSize {
    if let Some(module_attr) = form_list.module_attribute() {
        let module_attr_range = module_attr.form_id.get(source).syntax().text_range();
        module_attr_range.end() + TextSize::from(1)
    } else {
        TextSize::from(0)
    }
}
