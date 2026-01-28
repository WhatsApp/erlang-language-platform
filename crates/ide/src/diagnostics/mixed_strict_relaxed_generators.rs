/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: mixed_strict_relaxed_generators
//
// Return a warning if a zip generator contains a mix of strict and relaxed generators.
// This can lead to subtle, hard-to-predict behavior.
// See https://github.com/erlang/otp/issues/9435

use elp_ide_db::elp_base_db::FileId;
use hir::AnyExpr;
use hir::ComprehensionExpr;
use hir::Expr;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;

pub(crate) struct MixedStrictRelaxedGeneratorsLinter;

impl Linter for MixedStrictRelaxedGeneratorsLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::MixedStrictRelaxedGenerators
    }

    fn description(&self) -> &'static str {
        "Mixing strict and relaxed generators in a zip generator can cause unexpected behavior."
    }
}

/// Returns the strictness of a generator within a comprehension.
/// Only generators have strictness; filter expressions return None.
fn get_generator_strictness(expr: &ComprehensionExpr) -> Option<bool> {
    match expr {
        ComprehensionExpr::ListGenerator { strict, .. }
        | ComprehensionExpr::BinGenerator { strict, .. }
        | ComprehensionExpr::MapGenerator { strict, .. } => Some(*strict),
        ComprehensionExpr::Expr(_) => None,
        ComprehensionExpr::Zip(_) => None,
    }
}

/// Checks if a zip contains mixed strict and relaxed generators.
/// Returns true if there's at least one strict and at least one relaxed generator.
fn zip_has_mixed_strictness(exprs: &[ComprehensionExpr]) -> bool {
    let mut has_strict = false;
    let mut has_relaxed = false;

    for expr in exprs {
        if let Some(strict) = get_generator_strictness(expr) {
            if strict {
                has_strict = true;
            } else {
                has_relaxed = true;
            }
            if has_strict && has_relaxed {
                return true;
            }
        }
    }
    false
}

impl GenericLinter for MixedStrictRelaxedGeneratorsLinter {
    type Context = ();

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let mut res = Vec::new();

        sema.def_map(file_id)
            .get_function_clauses()
            .for_each(|(_, def)| {
                if def.file.file_id == file_id {
                    let in_clause = def.in_clause(sema, def);
                    in_clause.fold_clause(
                        Strategy {
                            macros: MacroStrategy::Expand,
                            parens: ParenStrategy::InvisibleParens,
                        },
                        (),
                        &mut |acc, ctx| {
                            if let AnyExpr::Expr(Expr::Comprehension { exprs, .. }) = ctx.item {
                                // Check each expression in the comprehension for Zip variants
                                for comp_expr in exprs {
                                    if let ComprehensionExpr::Zip(zip_exprs) = comp_expr
                                        && zip_has_mixed_strictness(zip_exprs.as_slice())
                                        && let hir::AnyExprId::Expr(expr_id) = ctx.item_id
                                        && let Some(range) = in_clause.range_for_expr(expr_id)
                                        && range.file_id == def.file.file_id
                                        && ctx.in_macro.is_none()
                                    {
                                        res.push(GenericLinterMatchContext {
                                            range: range.range,
                                            context: (),
                                        });
                                    }
                                }
                            };
                            acc
                        },
                    );
                }
            });

        if res.is_empty() { None } else { Some(res) }
    }
}

pub static LINTER: MixedStrictRelaxedGeneratorsLinter = MixedStrictRelaxedGeneratorsLinter;

#[cfg(test)]
mod tests {
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction);
        check_diagnostics_with_config(config, fixture);
    }

    // -------------------------------------------------------------------------
    // List Generator Tests
    // -------------------------------------------------------------------------

    #[test]
    fn list_all_strict_no_warning() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test() -> [{X, Y} || X <:- [1,2] && Y <:- [3,4]].
            "#,
        )
    }

    #[test]
    fn list_all_relaxed_no_warning() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test() -> [{X, Y} || X <- [1,2] && Y <- [3,4]].
            "#,
        )
    }

    #[test]
    fn list_mixed_relaxed_strict() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test() -> [Y || Y <- [a] && Y <:- [b]].
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0063: Mixing strict and relaxed generators in a zip generator can cause unexpected behavior.
            "#,
        )
    }

    #[test]
    fn list_mixed_strict_relaxed() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test() -> [Y || Y <:- [a] && Y <- [b]].
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0063: Mixing strict and relaxed generators in a zip generator can cause unexpected behavior.
            "#,
        )
    }

    #[test]
    fn list_three_generators_mixed() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test() -> [X || X <- [1] && Y <:- [2] && Z <- [3]].
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0063: Mixing strict and relaxed generators in a zip generator can cause unexpected behavior.
            "#,
        )
    }

    // -------------------------------------------------------------------------
    // Binary Generator Tests
    // -------------------------------------------------------------------------

    #[test]
    fn binary_all_strict_no_warning() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test() -> << <<X, Y>> || <<X>> <:= <<1,2>>, <<Y>> <:= <<3,4>> >>.
            "#,
        )
    }

    #[test]
    fn binary_all_relaxed_no_warning() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test() -> << <<X, Y>> || <<X>> <= <<1,2>>, <<Y>> <= <<3,4>> >>.
            "#,
        )
    }

    #[test]
    fn binary_mixed_relaxed_strict() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test() -> << <<X, Y>> || <<X>> <= <<1,2>> && <<Y>> <:= <<3,4>> >>.
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0063: Mixing strict and relaxed generators in a zip generator can cause unexpected behavior.
            "#,
        )
    }

    // -------------------------------------------------------------------------
    // Map Generator Tests
    // -------------------------------------------------------------------------

    #[test]
    fn map_all_strict_no_warning() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test(M1, M2) -> [{K1, K2} || K1 := _V1 <:- M1 && K2 := _V2 <:- M2].
            "#,
        )
    }

    #[test]
    fn map_all_relaxed_no_warning() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test(M1, M2) -> [{K1, K2} || K1 := _V1 <- M1 && K2 := _V2 <- M2].
            "#,
        )
    }

    #[test]
    fn map_mixed_relaxed_strict() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test(M1, M2) -> [{K1, K2} || K1 := _V1 <- M1 && K2 := _V2 <:- M2].
            %%              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0063: Mixing strict and relaxed generators in a zip generator can cause unexpected behavior.
            "#,
        )
    }

    #[test]
    fn map_comprehension_mixed() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test(M1, M2) -> #{K1 => K2 || K1 := _V1 <- M1 && K2 := _V2 <:- M2}.
            %%              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0063: Mixing strict and relaxed generators in a zip generator can cause unexpected behavior.
            "#,
        )
    }

    // -------------------------------------------------------------------------
    // Cross-Type Generator Tests (mixing list, binary, and map)
    // -------------------------------------------------------------------------

    #[test]
    fn list_and_map_all_strict_no_warning() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test(Map) -> [{X, K} || X <:- [1,2] && K := _V <:- Map].
            "#,
        )
    }

    #[test]
    fn list_strict_map_relaxed_mixed() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test(Map) -> [{X, K} || X <:- [1,2] && K := _V <- Map].
            %%           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0063: Mixing strict and relaxed generators in a zip generator can cause unexpected behavior.
            "#,
        )
    }

    #[test]
    fn list_relaxed_map_strict_mixed() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test(Map) -> [{X, K} || X <- [1,2] && K := _V <:- Map].
            %%           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0063: Mixing strict and relaxed generators in a zip generator can cause unexpected behavior.
            "#,
        )
    }

    #[test]
    fn list_and_binary_mixed() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test() -> [{X, Y} || X <- [1,2] && <<Y>> <:= <<3,4>>].
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0063: Mixing strict and relaxed generators in a zip generator can cause unexpected behavior.
            "#,
        )
    }

    #[test]
    fn all_three_types_consistent_strict_no_warning() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test(M) -> [{X, Y, K} || X <:- [1] && <<Y>> <:= <<2>> && K := _V <:- M].
            "#,
        )
    }

    #[test]
    fn all_three_types_mixed() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test(M) -> [{X, Y, K} || X <- [1] && <<Y>> <:= <<2>> && K := _V <- M].
            %%         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0063: Mixing strict and relaxed generators in a zip generator can cause unexpected behavior.
            "#,
        )
    }

    // -------------------------------------------------------------------------
    // Sequential Generators (should NOT warn)
    // -------------------------------------------------------------------------

    #[test]
    fn sequential_list_mixed_ok() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test() -> [{X, Y} || X <:- [1], Y <- [2]].
            "#,
        )
    }

    #[test]
    fn sequential_map_mixed_ok() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test(M1, M2) -> [{K1, K2} || K1 := _V1 <:- M1, K2 := _V2 <- M2].
            "#,
        )
    }

    #[test]
    fn sequential_cross_type_ok() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test(Map) -> [{X, K} || X <:- [1], K := _V <- Map].
            "#,
        )
    }

    // -------------------------------------------------------------------------
    // Edge Cases
    // -------------------------------------------------------------------------

    #[test]
    fn nested_comprehension_inner_mixed() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test() -> [[Y || Y <- [a] && Y <:- [b]] || _ <- [1]].
            %%         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0063: Mixing strict and relaxed generators in a zip generator can cause unexpected behavior.
            "#,
        )
    }

    #[test]
    fn nested_comprehension_outer_ok() {
        check_diagnostics(
            r#"
            //- /src/test.erl
            -module(test).
            test() -> [[Y || Y <- [a] && Y <- [b]] || X <:- [1]].
            "#,
        )
    }
}
