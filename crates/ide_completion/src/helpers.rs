/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_syntax::ast;
use elp_syntax::ast::ExprMax;
use elp_syntax::match_ast;
use elp_syntax::AstNode;
use elp_syntax::SmolStr;
use elp_syntax::SourceFile;
use elp_syntax::SyntaxKind;
use elp_syntax::TextSize;
use hir::InFile;
use hir::NameArity;

use crate::Completion;
use crate::Contents;
use crate::Kind;

pub(crate) fn atom_value(parsed: &InFile<SourceFile>, offset: TextSize) -> Option<String> {
    let node = parsed.value.syntax();
    // Temporary for T153426323
    let _pctx = stdx::panic_context::enter(format!("\natom_value"));
    let token = node.token_at_offset(offset);
    let token = parsed.with_value(elp_ide_db::helpers::pick_best_token(
        token,
        |kind| match kind {
            SyntaxKind::ATOM => 2,
            _ => 1,
        },
    )?);

    let parent = token.value.parent()?;
    match_ast! {
        match parent {
            ast::Atom(a) => {
                a.text()
            },
            _ => None
        }
    }
}

pub(crate) fn format_call(name: &str, arity: u32) -> Contents {
    let args = (1..(arity + 1))
        .map(|n| format!("${{{}:Arg{}}}", n, n))
        .collect::<Vec<_>>()
        .join(", ");
    Contents::Snippet(format!("{}({})", name, args))
}

pub(crate) fn name_slash_arity_completion(
    na: &NameArity,
    prefix: &str,
    kind: Kind,
) -> Option<Completion> {
    if na.name().starts_with(prefix) {
        Some(Completion {
            label: na.to_string(),
            kind,
            contents: Contents::SameAsLabel,
            position: None,
            sort_text: None,
            deprecated: false,
        })
    } else {
        None
    }
}

pub(crate) fn split_remote(remote: &ast::Remote) -> Option<(ast::Atom, SmolStr)> {
    let module_atom = match remote.module()?.module()? {
        ExprMax::Atom(atom) => atom,
        _ => return None,
    };
    let name: SmolStr = remote.fun().and_then(|f| f.name()).unwrap_or_default();
    Some((module_atom, name))
}
