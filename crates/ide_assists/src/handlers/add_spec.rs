/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Write;

use elp_ide_db::SymbolClass;
use elp_ide_db::SymbolDefinition;
use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_syntax::AstNode;
use elp_syntax::ast;

use crate::AssistContext;
use crate::Assists;

// Assist: add_spec
//
// Adds a spec stub above a function, if it doesn't already have one.
//
// ```
// foo(Arg1, some_atom) -> ok.
// ```
// ->
// ```
// -spec foo(Arg1 :: arg1(), arg2()) -> return_type().
// foo(Arg1, some_atom) -> ok.
// ```
pub(crate) fn add_spec(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    let function_def = match ctx.classify_offset()? {
        SymbolClass::Definition(SymbolDefinition::Function(fun_def)) => Some(fun_def),
        _ => None,
    }?;

    if function_def.spec.is_some() {
        return None;
    }

    let source = function_def.source(ctx.db().upcast());
    let name = source.first()?.name()?;
    let name_text = name.text()?;

    let insert = source.first()?.syntax().text_range().start();
    let target = name.syntax().text_range();

    acc.add(
        AssistId("add_spec", AssistKind::Generate),
        "Add spec stub",
        None,
        target,
        None,
        |builder| {
            let type_names = source
                .iter()
                .find_map(|c| match c.clause() {
                    Some(ast::FunctionOrMacroClause::FunctionClause(ref clause)) => {
                        if c.syntax().text_range().contains(ctx.offset()) {
                            Some(clause.clone())
                        } else {
                            None
                        }
                    }
                    _ => None,
                })
                .unwrap()
                .args()
                .into_iter()
                .flat_map(|args| args.args())
                .enumerate()
                .map(|(arg_idx, expr)| type_name(arg_idx + 1, expr));

            match ctx.config.snippet_cap {
                Some(cap) => {
                    let mut snippet_idx = 0;
                    let types_snippets = type_names.fold(String::new(), |mut output, arg_name| {
                        snippet_idx += 1;
                        let _ = write!(output, "${{{}:{}}}, ", snippet_idx, arg_name);
                        output
                    });
                    snippet_idx += 1;
                    let snippet = format!(
                        "-spec {}({}) -> ${{{}:return_type()}}.\n",
                        name_text,
                        types_snippets.trim_end_matches(", "),
                        snippet_idx
                    );
                    builder.edit_file(ctx.frange.file_id);
                    builder.insert_snippet(cap, insert, snippet);
                }
                None => {
                    let types_text = type_names.fold(String::new(), |mut output, arg_name| {
                        let _ = write!(output, "{}, ", arg_name);
                        output
                    });
                    let text = format!(
                        "-spec {}({}) -> return_type().\n",
                        name_text,
                        types_text.trim_end_matches(", ")
                    );
                    builder.edit_file(ctx.frange.file_id);
                    builder.insert(insert, text)
                }
            }
        },
    )
}

pub fn type_name(arg_idx: usize, expr: ast::Expr) -> String {
    if let ast::Expr::ExprMax(ast::ExprMax::Var(var)) = expr {
        format!("{} :: type{}()", var.text(), arg_idx)
    } else {
        format!("type{}()", arg_idx)
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::*;

    /// We use the "expect parse error" checks below for the cases that generate
    ///   snippets (https://code.visualstudio.com/docs/editor/userdefinedsnippets),
    ///   since the snippets themselves are not valid Erlang code, but Erlang code
    ///   templates consumed by the LSP client to enable quick edits of parameters.

    #[test]
    fn test_base_case() {
        check_assist_expect_parse_error(
            add_spec,
            "Add spec stub",
            r#"
~foo(Foo, some_atom) -> ok.
"#,
            expect![[r#"
                -spec foo(${1:Foo :: type1()}, ${2:type2()}) -> ${3:return_type()}.
                foo(Foo, some_atom) -> ok.
            "#]],
        )
    }

    #[test]
    fn test_previous_has_spec() {
        check_assist_expect_parse_error(
            add_spec,
            "Add spec stub",
            r#"
-spec bar() -> ok.
bar() -> ok.
f~oo() -> ok.
"#,
            expect![[r#"
                -spec bar() -> ok.
                bar() -> ok.
                -spec foo() -> ${1:return_type()}.
                foo() -> ok.
            "#]],
        )
    }

    #[test]
    fn test_already_has_spec_above() {
        check_assist_not_applicable(
            add_spec,
            r#"
-spec foo(x(), y()) -> ok.
~foo(Foo, some_atom) -> ok.
    "#,
        );
    }

    #[test]
    fn test_already_has_spec_below() {
        check_assist_not_applicable(
            add_spec,
            r#"
~foo(Foo, some_atom) -> ok.
-spec foo(x(), y()) -> ok.
    "#,
        );
    }
}
