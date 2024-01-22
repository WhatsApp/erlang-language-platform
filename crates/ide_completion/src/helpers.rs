/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_base_db::FileId;
use elp_base_db::FilePosition;
use elp_base_db::SourceDatabase;
use elp_syntax::ast;
use elp_syntax::ast::ExprMax;
use elp_syntax::match_ast;
use elp_syntax::AstNode;
use elp_syntax::SmolStr;
use elp_syntax::SourceFile;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxToken;
use elp_syntax::TextSize;
use hir::FunctionDef;
use hir::InFile;
use hir::NameArity;
use hir::Semantic;
use hir::SpecDef;

use crate::Completion;
use crate::Contents;
use crate::Kind;

pub(crate) fn atom_value(parsed: &InFile<SourceFile>, offset: TextSize) -> Option<String> {
    let node = parsed.value.syntax();
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

pub(crate) fn name_arity_to_call_completion(
    sema: &Semantic,
    file_id: FileId,
    na: &NameArity,
    prefix: &str,
    next_token: &Option<SyntaxToken>,
) -> Option<Completion> {
    let db = sema.db.upcast();
    let def_map = sema.def_map(file_id);

    let def = def_map.get_function(na);
    let position = def.and_then(|def| {
        let fun_decl_ast = def.source(sema.db.upcast());
        Some(FilePosition {
            file_id: def.file.file_id,
            offset: fun_decl_ast.get(0)?.syntax().text_range().start(),
        })
    });
    let deprecated = def_map.is_deprecated(na);
    let spec_def = def_map.get_spec(na);
    let include_args = should_include_args(next_token);

    if na.name().starts_with(prefix) {
        let contents = def.map_or(Some(format_call(na.name(), na.arity())), |def| {
            function_contents(db, def, spec_def, na.name(), include_args)
        })?;
        Some(Completion {
            label: na.to_string(),
            kind: Kind::Function,
            contents,
            position,
            sort_text: None,
            deprecated,
        })
    } else {
        None
    }
}

pub(crate) fn should_include_args(next_token: &Option<SyntaxToken>) -> bool {
    match next_token {
        Some(token) => token.kind() != elp_syntax::SyntaxKind::ANON_LPAREN,
        _ => true,
    }
}

fn function_arg_names(
    db: &dyn SourceDatabase,
    def: &FunctionDef,
    spec_def: Option<&SpecDef>,
) -> Option<String> {
    let param_names = def.arg_names(spec_def, db);
    let res = param_names?
        .iter()
        .enumerate()
        .map(|(i, param_name)| {
            let n = i + 1;
            format!("${{{n}:{param_name}}}")
        })
        .collect::<Vec<_>>()
        .join(", ");
    Some(res)
}

pub(crate) fn function_contents(
    db: &dyn SourceDatabase,
    def: &FunctionDef,
    spec_def: Option<&SpecDef>,
    function_name: &str,
    include_args: bool,
) -> Option<Contents> {
    if include_args {
        let function_arg_names = function_arg_names(db, def, spec_def)?;
        Some(Contents::Snippet(format!(
            "{function_name}({function_arg_names})"
        )))
    } else {
        Some(Contents::Snippet(format!("{function_name}")))
    }
}
