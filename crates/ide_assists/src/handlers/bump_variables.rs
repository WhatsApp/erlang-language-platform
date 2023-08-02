/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use ast::AstNode;
use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_ide_db::rename::RenameResult;
use elp_ide_db::rename::SafetyChecks;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::ReferenceClass;
use elp_ide_db::SymbolClass;
use elp_ide_db::SymbolDefinition;
use elp_syntax::ast;
use fxhash::FxHashSet;
use hir::Expr;
use hir::InFile;
use hir::Pat;
use lazy_static::lazy_static;
use regex::Regex;

use crate::AssistContext;
use crate::Assists;

// Assist: bump_variables
//
// In a sequence of variable assignments, rename them to create a gap.
//
// ```
// foo() ->
//   X0 = 1,
//   X1 = X0 +1,
//   X2 = X1 + 4,
//   X2.
// ```
// ->
// ```
// foo() ->
//   X0 = 1,
//   X2 = X0 +1,
//   X3 = X2 + 4,
//   X3.
// ```
pub(crate) fn bump_variables(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    let var: ast::Var = ctx.find_node_at_offset()?;
    if let Some(number) = NumberedVar::from_var(&var.text().as_str()) {
        let variable_name = var.text();
        let variable_range = var.syntax().text_range();
        // We are on a numbered variable.  Check if there are any
        // others with the same stem but a higher number, and
        // referring to this one.

        let file_id = ctx.frange.file_id;
        let function_id = ctx
            .sema
            .find_enclosing_function(ctx.file_id(), var.syntax())?;
        let infile_function = InFile::new(ctx.file_id(), function_id);
        let (_body, body_map) = ctx.db().function_body_with_source(infile_function);
        let clause_id = ctx.sema.find_enclosing_function_clause(var.syntax())?;
        let vars = ctx.sema.fold_clause(
            infile_function,
            clause_id,
            FxHashSet::default(),
            &mut |mut acc, ctx| match ctx.expr {
                Expr::Var(v) => {
                    if let Some(expr) = body_map.expr(ctx.expr_id) {
                        if expr.file_id() == file_id {
                            acc.insert((v, expr));
                        }
                    }
                    acc
                }
                _ => acc,
            },
            &mut |mut acc, ctx| match ctx.pat {
                Pat::Var(v) => {
                    if let Some(pat) = body_map.pat(ctx.pat_id) {
                        if pat.file_id() == file_id {
                            acc.insert((v, pat));
                        }
                    }
                    acc
                }
                _ => acc,
            },
        );
        let mut var_defs = Vec::default();
        vars.into_iter()
            .filter_map(|(v, vs)| {
                let var_name = ctx.db().lookup_var(v);
                let nv = NumberedVar::from_var(var_name.as_str())?;
                Some((vs, nv))
            })
            .filter(|(_vs, nv)| nv.base == number.base && nv.number >= number.number)
            .for_each(|(vs, nv)| {
                || -> Option<()> {
                    let token = ctx.ast_ptr_get(vs)?.syntax().first_token()?;
                    match SymbolClass::classify(&ctx.sema, InFile::new(ctx.file_id(), token))? {
                        SymbolClass::Definition(SymbolDefinition::Var(d)) => {
                            var_defs.push((SymbolDefinition::Var(d), nv));
                        }
                        SymbolClass::Definition(_) => {}
                        SymbolClass::Reference { refs, typ: _ } => {
                            match refs {
                                ReferenceClass::Definition(SymbolDefinition::Var(d)) => {
                                    var_defs.push((SymbolDefinition::Var(d), nv));
                                }
                                _ => {}
                            };
                        }
                    };
                    Some(())
                }();
            });

        // Note: taking naive approach here, assuming the numerical
        // sequence is sane. So not analyzing the actual chain of
        // assignments/usages
        let rename_ops: RenameResult<Vec<SourceChange>> = var_defs
            .iter()
            .map(|(def, nv)| def.rename(&ctx.sema, &|_| nv.bumped(), SafetyChecks::No))
            .collect();

        let rename_op = match rename_ops {
            Ok(ops) => ops.into_iter().reduce(|acc, elem| acc.merge(elem)),
            Err(_) => None,
        };

        if let Some(edits) = rename_op {
            let id = AssistId("bump_variables", AssistKind::QuickFix);
            let message = format!("Bump variable `{variable_name}`");
            acc.add(id, message, variable_range, None, |builder| {
                builder.apply_source_change(edits);
            });
        }
    }
    Some(())
}

#[derive(Debug)]
struct NumberedVar {
    base: String,
    number: usize,
}

impl NumberedVar {
    fn from_var(var_str: &str) -> Option<NumberedVar> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"[0-9]+$").unwrap();
        }
        let res = RE.find(var_str)?;
        let number = &var_str[res.start()..res.end()];
        match number.parse::<usize>() {
            Ok(number) => Some(NumberedVar {
                base: var_str[0..res.start()].to_string(),
                number,
            }),
            Err(_) => None,
        }
    }

    fn bumped(&self) -> String {
        format!("{}{}", self.base, self.number + 1).to_string()
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::*;

    #[test]
    fn bump_vars_1() {
        check_assist(
            bump_variables,
            "Bump variable `X1`",
            r#"
            foo() ->
              X0 = 1,
              X~1 = X0 +1,
              X2 = X1 + 4,
              X2.
            "#,
            expect![[r#"
            foo() ->
              X0 = 1,
              X2 = X0 +1,
              X3 = X2 + 4,
              X3.
            "#]],
        )
    }

    #[test]
    fn bump_vars_2() {
        check_assist(
            bump_variables,
            "Bump variable `X0`",
            r#"
            foo(Y) ->
              X~0 = 1,
              case Y of
                {ok, Val} -> X0;
                {new, X1} -> X1
              end.
            "#,
            expect![[r#"
                foo(Y) ->
                  X1 = 1,
                  case Y of
                    {ok, Val} -> X1;
                    {new, X2} -> X2
                  end.
            "#]],
        )
    }

    #[test]
    fn bump_vars_3() {
        check_assist(
            bump_variables,
            "Bump variable `X0`",
            r#"
            foo(0) ->
              X~0 = 1,
              X1 = X0 + 2,
              X1;
            foo(Y) ->
              X0 = 2,
              X1 = X0 + Y,
              X1.
            "#,
            expect![[r#"
                foo(0) ->
                  X1 = 1,
                  X2 = X1 + 2,
                  X2;
                foo(Y) ->
                  X0 = 2,
                  X1 = X0 + Y,
                  X1.
            "#]],
        )
    }

    #[test]
    fn bump_vars_with_macro() {
        check_assist(
            bump_variables,
            "Bump variable `X1`",
            r#"
            //- /src/blah.erl
            -module(blah).
            -include("inc.hrl").
            foo() ->
              ?X~1 = 1,
              X2 = X1 + 4,
              ?assertEqual(X2,5),
              X2.
            //- /src/inc.hrl
            -define(assertEqual(Expect, Expr),
                    begin
                    ((fun () ->
                        X__X = (Expect)
                      end)())
                    end).
            "#,
            expect![[r#"
                -module(blah).
                -include("inc.hrl").
                foo() ->
                  ?X1 = 1,
                  X3 = X1 + 4,
                  ?assertEqual(X3,5),
                  X3.
            "#]],
        )
    }
}
