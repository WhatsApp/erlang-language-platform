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

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_syntax::AstNode;
use elp_syntax::ast;
use elp_text_edit::TextSize;
use hir::FunctionDef;

use crate::AssistContext;
use crate::Assists;

const DEFAULT_TEXT: &str =
    "[How to write documentation](https://www.erlang.org/doc/system/documentation.html)";
const ARG_TEXT: &str = "Argument description";

// Assist: add_doc
//
// Adds an -doc attribute above a function, if it doesn't already have one.
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
pub(crate) fn add_doc(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    let name = match ctx.find_node_at_offset::<ast::Name>()? {
        ast::Name::Atom(name) => name,
        ast::Name::MacroCallExpr(_) | ast::Name::Var(_) => return None,
    };
    let clause = ast::FunctionClause::cast(name.syntax().parent()?)?;
    let function = ast::FunDecl::cast(clause.syntax().parent()?)?;

    let file_id = ctx.file_id();
    let def = ctx
        .sema
        .find_enclosing_function_def(file_id, function.syntax())?;

    if def.doc_id.is_some() || def.edoc_comments(ctx.db()).is_some() {
        return None;
    }

    let target = name.syntax().text_range();
    let insert_offset = find_insert_offset(ctx, &def)?;

    acc.add(
        AssistId("add_doc", AssistKind::Generate),
        "Add -doc attribute",
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

            let mut idx = 1;
            let header = match ctx.config.snippet_cap {
                Some(_cap) => {
                    format!("-doc \"\"\"\n${{{idx}:{DEFAULT_TEXT}}}\n\"\"\".")
                }
                None => {
                    format!("-doc \"\"\"\n{DEFAULT_TEXT}\n\"\"\".")
                }
            };
            let params = arg_names.fold(String::new(), |mut output, arg_name| {
                idx += 1;
                match ctx.config.snippet_cap {
                    Some(_cap) => {
                        let _ = write!(output, "\"{arg_name}\" => \"${{{idx}:{ARG_TEXT}}}\", ");
                    }
                    None => {
                        let _ = write!(output, "\"{arg_name}\" => \"{ARG_TEXT}\", ");
                    }
                }
                output
            });
            let params = if !params.is_empty() {
                format!(
                    "-doc #{{params => #{{{}}}}}.\n",
                    params.trim_end_matches(", ")
                )
            } else {
                "".to_string()
            };
            let text = format!("{header}\n{params}");
            builder.edit_file(ctx.frange.file_id);
            match ctx.config.snippet_cap {
                Some(cap) => {
                    builder.insert_snippet(cap, insert_offset, text);
                }
                None => builder.insert(insert_offset, text),
            }
        },
    )
}

fn find_insert_offset(ctx: &AssistContext, def: &FunctionDef) -> Option<TextSize> {
    let db = ctx.db();
    let range = def
        .doc_metadata_range(db)
        .or(def.spec_range(db))
        .or(def.range(db))?;
    Some(range.start())
}

pub fn arg_name(arg_idx: usize, expr: ast::Expr) -> String {
    if let ast::Expr::ExprMax(ast::ExprMax::Var(var)) = expr {
        var.text().to_string()
    } else {
        format!("Arg{arg_idx}")
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
            add_doc,
            "Add -doc attribute",
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
            add_doc,
            "Add -doc attribute",
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
    fn test_previous_has_old_style_edoc_comment() {
        check_assist(
            add_doc,
            "Add -doc attribute",
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
    fn test_with_plain_comment() {
        check_assist(
            add_doc,
            "Add -doc attribute",
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
    fn test_already_has_doc_metadata() {
        check_assist(
            add_doc,
            "Add -doc attribute",
            r#"
-doc #{params => #{}}.
~foo() -> ok.
"#,
            expect![[r#"
                -doc """
                ${1:[How to write documentation](https://www.erlang.org/doc/system/documentation.html)}
                """.
                -doc #{params => #{}}.
                foo() -> ok.
            "#]],
        )
    }

    #[test]
    fn test_already_has_old_style_edoc() {
        check_assist_not_applicable(
            add_doc,
            r#"
%% @doc foo
~foo(Foo, some_atom) -> ok.
"#,
        );
    }

    #[test]
    fn test_module_has_old_style_edoc() {
        check_assist(
            add_doc,
            "Add -doc attribute",
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

    #[test]
    fn test_already_doc_attribute() {
        check_assist_not_applicable(
            add_doc,
            r#"
-doc """
This is foo
""".
~foo(Foo, some_atom) -> ok.
"#,
        );
    }

    #[test]
    fn test_module_has_doc_attribute() {
        check_assist(
            add_doc,
            "Add -doc attribute",
            r#"
            -moduledoc "This is the main module".
            -module(main).
            -export([foo/0]).

            ~foo() -> ok.
            "#,
            expect![[r#"
                -moduledoc "This is the main module".
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
