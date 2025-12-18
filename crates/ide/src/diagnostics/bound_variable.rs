/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: bound_variable
//
// Return a warning if the LHS of a match already contains a bound variable.
//

use elp_ide_db::elp_base_db::FileId;
use hir::AnyExpr;
use hir::Expr;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;

pub(crate) struct BoundVariableLinter;

impl Linter for BoundVariableLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::BoundVarInLhs
    }

    fn description(&self) -> &'static str {
        "Match on a bound variable"
    }
}

impl GenericLinter for BoundVariableLinter {
    type Context = ();

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let bound_vars_by_function = sema.bound_vars_by_function(file_id);
        let mut res = Vec::new();
        sema.def_map(file_id)
            .get_function_clauses()
            .for_each(|(_, def)| {
                if def.file.file_id == file_id
                    && let Some(bound_vars) = bound_vars_by_function.get(&def.function_clause_id)
                {
                    let in_clause = def.in_clause(sema, def);
                    in_clause.fold_clause(
                        Strategy {
                            macros: MacroStrategy::ExpandButIncludeMacroCall,
                            parens: ParenStrategy::InvisibleParens,
                        },
                        (),
                        &mut |acc, ctx| {
                            if let AnyExpr::Expr(Expr::Match { lhs, rhs: _ }) = ctx.item
                                && bound_vars.contains(&lhs)
                                && let Some(range) = in_clause.range_for_pat(lhs)
                                && range.file_id == def.file.file_id
                                && ctx.in_macro.is_none()
                            {
                                res.push(GenericLinterMatchContext {
                                    range: range.range,
                                    context: (),
                                });
                            };
                            acc
                        },
                    );
                }
            });

        Some(res)
    }
}

pub static LINTER: BoundVariableLinter = BoundVariableLinter;

#[cfg(test)]
mod test {
    use elp_ide_db::DiagnosticCode;
    use expect_test::Expect;

    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix_with_config;

    #[track_caller]
    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction);
        check_diagnostics_with_config(config, fixture)
    }

    #[track_caller]
    pub(crate) fn check_fix(fixture_before: &str, fixture_after: Expect) {
        let config = DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction);
        check_fix_with_config(config, fixture_before, fixture_after)
    }
    #[test]
    fn bound_variable() {
        check_diagnostics(
            r#"
             //- /src/bound.erl
             -module(bound).

             foo() ->
                 AA = bar(),
                 AA = bar().
              %% ^^ 💡 warning: W0060: Match on a bound variable

            "#,
        )
    }

    #[test]
    fn bound_variable_not_reported_in_case() {
        check_diagnostics(
            r#"
             //- /src/bound.erl
             -module(bound).

             foo(Val) ->
                 case Val of
                    undefined -> ok;
                    Val when is_list(Val) -> ok
                 end.

            "#,
        )
    }

    #[test]
    fn bound_variable_not_reported_in_macro() {
        check_diagnostics(
            r#"
             //- /src/bound.erl
             -module(bound).
             -include("inc.hrl").

             foo(Val) ->
                 ?A_MACRO(Val).
             //- /src/inc.hrl
             -define(A_MACRO(X), X=X).
            "#,
        )
    }

    #[test]
    fn bound_variable_ignore_fix() {
        check_fix(
            r#"
             //- /src/bound.erl
             -module(bound).

             foo() ->
                 AA = bar(),
                 A~A = bar().
            "#,
            expect_test::expect![[r#"
                -module(bound).

                foo() ->
                    AA = bar(),
                    % elp:ignore W0060 (bound_var_in_lhs)
                    AA = bar().
            "#]],
        )
    }
}
