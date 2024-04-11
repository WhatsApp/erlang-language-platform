/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::assists::AssistContextDiagnosticCode;
use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::TextSize;
use hir::Expr;
use hir::InFile;

use crate::AssistContext;
use crate::Assists;

// Assist: create_function
//
// Create a local function if it does not exist
//
// ```
// foo() -> bar().
// ```
// ->
// ```
// foo() -> bar().
//
// bar() ->
//     erlang:error(not_implemented).
//
// ```
pub(crate) fn create_function(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    for d in ctx.diagnostics {
        if let AssistContextDiagnosticCode::UndefinedFunction = d.code {
            if ctx.classify_offset().is_none() {
                let call: ast::Call = ctx.find_node_at_offset()?;
                let function_id = ctx
                    .sema
                    .find_enclosing_function(ctx.file_id(), call.syntax())?;

                let call_expr = ctx
                    .sema
                    .to_expr(InFile::new(ctx.file_id(), &ast::Expr::Call(call.clone())))?;
                if let Expr::Call { target, args } = &call_expr[call_expr.value] {
                    let (module_name, function_name) = match &target {
                        hir::CallTarget::Local { name } => {
                            let fun_atom = &call_expr[*name].as_atom()?;
                            let fun_name = ctx.sema.db.lookup_atom(*fun_atom).to_string();
                            (None, fun_name)
                        }
                        hir::CallTarget::Remote { module, name } => {
                            let module = &call_expr[*module].as_atom()?;
                            let fun_atom = &call_expr[*name].as_atom()?;
                            let fun_name = ctx.sema.db.lookup_atom(*fun_atom).to_string();
                            (Some(*module), fun_name)
                        }
                    };
                    let function_arity = args.len();

                    let form_list = ctx.db().file_form_list(ctx.file_id());
                    if let Some(module_name) = module_name {
                        let module_name = ctx.db().lookup_atom(module_name);
                        let module_attr = form_list.module_attribute()?;
                        if module_name != module_attr.name {
                            // Only inline qualified function calls if
                            // they refer to the module we are in.
                            // TODO: implied main module?
                            return None;
                        }
                    }
                    let function_args = ctx.create_function_args(args, &call_expr.body());

                    let fun_def = ctx
                        .sema
                        .function_def(&InFile::new(ctx.file_id(), function_id))?;
                    let function_range = fun_def.range(ctx.sema.db.upcast())?;

                    let insert = function_range.end() + TextSize::from(1);

                    let id = AssistId("create_function", AssistKind::QuickFix);
                    let message = format!("Create function `{function_name}/{function_arity}`");
                    acc.add_from_diagnostic(id, message, None, (*d).clone(), function_range, None, |builder| {
                        let text = format!("\n{function_name}({function_args}) ->\n    erlang:error(not_implemented).\n\n");
                        builder.edit_file(ctx.frange.file_id);
                        builder.insert(insert, text)
                    });
                }
            }
        }
    }
    Some(())
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::*;

    #[test]
    fn one_param_function() {
        check_assist(
            create_function,
            "Create function `foo/1`",
            r#"
 -module(life).

 heavy_calculations(X) -> fo~o(X).
 %%                       ^^^^^^ 💡 L1227: function foo/1 undefined
"#,
            expect![[r#"
                -module(life).

                heavy_calculations(X) -> foo(X).

                foo(X) ->
                    erlang:error(not_implemented).

            "#]],
        )
    }

    #[test]
    fn qualified_function_1() {
        check_assist(
            create_function,
            "Create function `foo/2`",
            r#"
 //- /src/life.erl
 -module(life).

 heavy_calculations(X) -> life:f~oo(X, X+1).
 %%                       ^^^^^^^^^^^ 💡 L1227: function life:foo/2 undefined
"#,
            expect![[r#"
                -module(life).

                heavy_calculations(X) -> life:foo(X, X+1).

                foo(X, XN) ->
                    erlang:error(not_implemented).

            "#]],
        )
    }

    #[test]
    fn qualified_function_2() {
        check_assist_not_applicable(
            create_function,
            r#"
 //- /src/life.erl
 -module(life).

 heavy_calculations(X) -> other:f~oo(X, X+1).
 %%                       ^^^^^^^^^^^^ 💡 L1227: function other:foo/2 undefined
"#,
        )
    }

    #[test]
    fn macro_qualified_function() {
        check_assist(
            create_function,
            "Create function `foo/0`",
            r#"
 -module(life).

 heavy_calculations(X) -> ?MODULE:fo~o().
 %%                       ^^^^^^^^^^^^^ 💡 L1227: function life:foo/0 undefined
"#,
            expect![[r#"
                -module(life).

                heavy_calculations(X) -> ?MODULE:foo().

                foo() ->
                    erlang:error(not_implemented).

            "#]],
        )
    }
}
