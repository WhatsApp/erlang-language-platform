/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: mutable-variable
//
// Diagnostic for detecting OTP mutable variable bug
// https://github.com/erlang/otp/issues/6873
//
// We are looking for a chain of match expressions where the
// constituent elements are already bound.
//
// ```erlang
// test() ->
//     Zero = 0,
//     One = 1,
//
//     Result = One = Zero,
//     ^^^^^^^^^^^^^^^^^^^
// ```
//

use elp_ide_db::elp_base_db::FileId;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::AnyExpr;
use hir::Expr;
use hir::FunctionClauseId;
use hir::PatId;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use crate::Diagnostic;
use crate::diagnostics::DiagnosticCode;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: true,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        mutable_variable_bug(diags, sema, file_id);
    },
};

fn mutable_variable_bug(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) -> Option<()> {
    let mut bound_vars_by_function: FxHashMap<FunctionClauseId, FxHashSet<&PatId>> =
        FxHashMap::default();
    let bound_vars = sema.bound_vars_in_pattern_diagnostic(file_id);
    bound_vars.iter().for_each(|(function_id, pat_id, _var)| {
        bound_vars_by_function
            .entry(function_id.value)
            .and_modify(|vars| {
                vars.insert(pat_id);
            })
            .or_insert_with(|| {
                let mut vars = FxHashSet::default();
                vars.insert(pat_id);
                vars
            });
    });
    sema.def_map(file_id)
        .get_function_clauses()
        .for_each(|(_, def)| {
            if def.file.file_id == file_id
                && let Some(bound_vars) = bound_vars_by_function.get(&def.function_clause_id)
            {
                let in_clause = def.in_clause(sema, def);
                in_clause.fold_clause(
                    Strategy {
                        macros: MacroStrategy::Expand,
                        parens: ParenStrategy::InvisibleParens,
                    },
                    (),
                    &mut |acc, ctx| {
                        if let AnyExpr::Expr(Expr::Match { lhs: _, rhs }) = ctx.item
                            && let Expr::Match { lhs, rhs: _ } = &in_clause[rhs]
                            && bound_vars.contains(lhs)
                            && let Some(range) = in_clause.range_for_any(ctx.item_id)
                            && range.file_id == def.file.file_id
                        {
                            diags.push(Diagnostic::new(
                                DiagnosticCode::MutableVarBug,
                                "Possible mutable variable bug",
                                range.range,
                            ));
                        };
                        acc
                    },
                );
            }
        });

    Some(())
}

#[cfg(test)]
mod tests {

    use crate::tests::check_diagnostics;

    #[test]
    fn mutable_variable_1() {
        check_diagnostics(
            r#"
//- /src/test.erl
-module(test).

-export([test/0]).

test() ->
    Zero = 0,
    One = 1,

    Result = One = Zero,
%%  ^^^^^^^^^^^^^^^^^^^ error: W0005: Possible mutable variable bug

    Result.
"#,
        );
    }

    #[test]
    fn mutable_variable_multiple_clauses() {
        check_diagnostics(
            r#"
//- /src/test.erl
-module(test).

-export([push_eligible/2]).

push_eligible(ProductPlatform, _Pu) ->
        case ProductPlatform of
            ProductPlatform ->
                false;
            ProductPlatform ->
                false
        end,
    false;
push_eligible(_ProductPlatform, Pu) ->
    AppVersion = ABUserInfo = Pu,
%%  ^^^^^^^^^^ 💡 warning: W0007: match is redundant
%%               ^^^^^^^^^^ 💡 warning: W0007: match is redundant
    false.

"#,
        );
    }
}
