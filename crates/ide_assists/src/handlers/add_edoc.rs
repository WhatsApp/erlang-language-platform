/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Write;

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::ast;
use hir::InFileAstPtr;

use crate::AssistContext;
use crate::Assists;
use crate::helpers::prev_form_nodes;

const DEFAULT_TEXT: &str =
    "[How to write documentation](https://www.erlang.org/doc/system/documentation.html)";
const ARG_TEXT: &str = "Argument description";

// Assist: add_edoc
//
// Adds an edoc comment above a function, if it doesn't already have one.
//
// ```
// foo(Arg1) -> ok.
// ```
// ->
// ```
// -doc """
// [How to write documentation](https://www.erlang.org/doc/system/documentation.html)
// """.
// -doc #{params => #{"Arg1" => "Argument description"}}.
// foo(Arg1) -> ok.
// ```
pub(crate) fn add_edoc(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    let name = match ctx.find_node_at_offset::<ast::Name>()? {
        ast::Name::Atom(name) => name,
        ast::Name::MacroCallExpr(_) | ast::Name::Var(_) => return None,
    };
    let clause = ast::FunctionClause::cast(name.syntax().parent()?)?;
    let function = ast::FunDecl::cast(clause.syntax().parent()?)?;

    let form = ast::Form::FunDecl(function.clone());
    let ast_pointer = AstPtr::new(&form);
    let in_file_ast_pointer = InFileAstPtr::new(ctx.file_id(), ast_pointer);

    let existing_edocs = ctx.db().file_edoc_comments(ctx.file_id());
    let already_has_edoc = existing_edocs
        .is_some_and(|existing_edocs| existing_edocs.contains_key(&in_file_ast_pointer));

    if already_has_edoc {
        return None;
    }

    let insert = prev_form_nodes(function.syntax())
        .filter_map(ast::Spec::cast)
        .map(|spec| spec.syntax().text_range().start())
        .next()
        .unwrap_or_else(|| function.syntax().text_range().start());
    let target = name.syntax().text_range();

    acc.add(
        AssistId("add_edoc", AssistKind::Generate),
        "Add edoc comment",
        None,
        target,
        None,
        |builder| {
            let arg_names = clause
                .args()
                .into_iter()
                .flat_map(|args| args.args())
                .enumerate()
                .map(|(arg_idx, expr)| arg_name(arg_idx + 1, expr));

            match ctx.config.snippet_cap {
                Some(cap) => {
                    let mut snippet_idx = 1;
                    let header_snippet = format!(
                        "-doc \"\"\"\n${{{}:{}}}\n\"\"\".",
                        snippet_idx, DEFAULT_TEXT
                    );
                    let args_snippets = arg_names.fold(String::new(), |mut output, arg_name| {
                        snippet_idx += 1;
                        let _ = write!(
                            output,
                            "\"{}\" => \"${{{}:{}}}\", ",
                            arg_name, snippet_idx, ARG_TEXT
                        );
                        output
                    });
                    let args_snippets = if !args_snippets.is_empty() {
                        format!(
                            "-doc #{{params => #{{{}}}}}.",
                            args_snippets.trim_end_matches(", ")
                        )
                    } else {
                        "".to_string()
                    };
                    let snippet = format!("{}\n{}\n", header_snippet, args_snippets);
                    builder.edit_file(ctx.frange.file_id);
                    builder.insert_snippet(cap, insert, snippet);
                }
                None => {
                    let args_text = arg_names.fold(String::new(), |mut output, arg_name| {
                        let _ = write!(output, "\"{}\" => \"{}\", ", arg_name, ARG_TEXT);
                        output
                    });
                    let args_text = if !args_text.is_empty() {
                        format!(
                            "-doc #{{params => #{{{}}}}}.",
                            args_text.trim_end_matches(", ")
                        )
                    } else {
                        "".to_string()
                    };
                    let text = format!("-doc \"\"\"\n{}\n\"\"\".\n{}\n", DEFAULT_TEXT, args_text);
                    builder.edit_file(ctx.frange.file_id);
                    builder.insert(insert, text)
                }
            }
        },
    )
}

pub fn arg_name(arg_idx: usize, expr: ast::Expr) -> String {
    if let ast::Expr::ExprMax(ast::ExprMax::Var(var)) = expr {
        var.text().to_string()
    } else {
        format!("Arg{}", arg_idx)
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::*;

    #[test]
    fn test_base_case() {
        check_assist(
            add_edoc,
            "Add edoc comment",
            r#"
~foo(Foo, some_atom) -> ok.
"#,
            expect![[r#"
                -doc """
                ${1:[How to write documentation](https://www.erlang.org/doc/system/documentation.html)}
                """.
                -doc #{params => #{"Foo" => "${2:Argument description}", "Arg2" => "${3:Argument description}"}}.
                foo(Foo, some_atom) -> ok.
            "#]],
        )
    }

    #[test]
    fn test_with_spec() {
        check_assist(
            add_edoc,
            "Add edoc comment",
            r#"
-spec foo(x(), y()) -> ok.
~foo(Foo, some_atom) -> ok.
"#,
            expect![[r#"
                -doc """
                ${1:[How to write documentation](https://www.erlang.org/doc/system/documentation.html)}
                """.
                -doc #{params => #{"Foo" => "${2:Argument description}", "Arg2" => "${3:Argument description}"}}.
                -spec foo(x(), y()) -> ok.
                foo(Foo, some_atom) -> ok.
            "#]],
        )
    }

    #[test]
    fn test_previous_has_comment() {
        check_assist(
            add_edoc,
            "Add edoc comment",
            r#"
%% @doc bar
bar() -> ok.
~foo() -> ok.
"#,
            expect![[r#"
                %% @doc bar
                bar() -> ok.
                -doc """
                ${1:[How to write documentation](https://www.erlang.org/doc/system/documentation.html)}
                """.

                foo() -> ok.
            "#]],
        )
    }

    #[test]
    fn test_non_edoc_comment() {
        check_assist(
            add_edoc,
            "Add edoc comment",
            r#"
%% Some comment
~foo() -> ok.
"#,
            expect![[r#"
                %% Some comment
                -doc """
                ${1:[How to write documentation](https://www.erlang.org/doc/system/documentation.html)}
                """.

                foo() -> ok.
            "#]],
        )
    }

    #[test]
    fn test_already_has_edoc() {
        check_assist_not_applicable(
            add_edoc,
            r#"
%% @doc foo
~foo(Foo, some_atom) -> ok.
"#,
        );
    }

    #[test]
    fn test_module_has_edoc() {
        check_assist(
            add_edoc,
            "Add edoc comment",
            r#"
            %% @doc
            %% My test module
            %% @end
            -module(main).
            -export([foo/0]).

            ~foo() -> ok.
            "#,
            expect![[r#"
                %% @doc
                %% My test module
                %% @end
                -module(main).
                -export([foo/0]).

                -doc """
                ${1:[How to write documentation](https://www.erlang.org/doc/system/documentation.html)}
                """.

                foo() -> ok.
            "#]],
        )
    }
}
