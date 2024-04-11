/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_syntax::ast;
use elp_syntax::AstNode;
use hir::InFile;

use crate::AssistContext;
use crate::Assists;

// Assist: add_impl
//
// Adds an implementation stub below a spec, if it doesn't already have one.
//
// ```
// -spec foo(Arg1 :: arg1(), arg2()) -> return_type().
// ```
// ->
// ```
// -spec foo(Arg1 :: arg1(), arg2()) -> return_type().
// foo(Arg1, Arg2) ->
//   error("not implemented").
// ```
pub(crate) fn add_impl(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    let spec = ctx.find_node_at_offset::<ast::Spec>()?;
    let spec_form = ctx.sema.find_form(InFile::new(ctx.file_id(), &spec))?;
    let def_map = ctx.db().def_map(ctx.file_id());
    let spec_def = def_map.get_unowned_spec(&spec_form.name)?;

    let target = spec.fun()?.syntax().text_range();
    let arg_names = spec_def.arg_names(ctx.db().upcast())?;

    acc.add(
        AssistId("add_impl", AssistKind::Generate),
        "Add implementation",
        None,
        target,
        None,
        |builder| {
            let insert = spec.syntax().text_range().end();
            let name = spec_def.spec.name.name();
            match ctx.config.snippet_cap {
                Some(cap) => {
                    let mut snippet_idx = 0;
                    let args_snippets = arg_names
                        .iter()
                        .map(|arg_name| {
                            snippet_idx += 1;
                            format!("${{{}:{}}}, ", snippet_idx, arg_name.name())
                        })
                        .collect::<String>();
                    snippet_idx += 1;
                    let snippet = format!(
                        "\n{}({}) ->\n  ${{{}:error(\"not implemented\").}}\n",
                        name,
                        args_snippets.trim_end_matches(", "),
                        snippet_idx
                    );
                    builder.edit_file(ctx.frange.file_id);
                    builder.insert_snippet(cap, insert, snippet);
                }
                None => {
                    let args_text = arg_names
                        .iter()
                        .map(|arg_name| format!("{}, ", arg_name.name()))
                        .collect::<String>();
                    let text = format!(
                        "\n{}({}) ->\n  error(\"not implemented\").\n",
                        name,
                        args_text.trim_end_matches(", ")
                    );
                    builder.edit_file(ctx.frange.file_id);
                    builder.insert(insert, text)
                }
            }
        },
    )
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
            add_impl,
            "Add implementation",
            r#"
-spec ~foo(Foo :: term(), some_atom) -> ok.
"#,
            expect![[r#"
                -spec foo(Foo :: term(), some_atom) -> ok.
                foo(${1:Foo}, ${2:Arg2}) ->
                  ${3:error("not implemented").}

            "#]],
        )
    }

    #[test]
    fn test_previous_has_impl() {
        check_assist_expect_parse_error(
            add_impl,
            "Add implementation",
            r#"
-spec bar() -> ok.
bar() -> ok.
-spec ~foo() -> return_type().
"#,
            expect![[r#"
                -spec bar() -> ok.
                bar() -> ok.
                -spec foo() -> return_type().
                foo() ->
                  ${1:error("not implemented").}

            "#]],
        )
    }

    #[test]
    fn test_already_has_impl_above() {
        check_assist_not_applicable(
            add_impl,
            r#"
foo(Foo, some_atom) -> ok.
-spec ~foo(x(), y()) -> ok.
    "#,
        );
    }

    #[test]
    fn test_already_has_impl_below() {
        check_assist_not_applicable(
            add_impl,
            r#"
-spec ~foo(x(), y()) -> ok.
foo(Foo, some_atom) -> ok.
    "#,
        );
    }
}
