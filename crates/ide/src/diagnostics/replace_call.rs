/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint/fix: replace_call
//!
//! Return a diagnostic if a given (noop) function is used,
//! and a fix to replace it by something else.

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_syntax::ast;
use elp_syntax::TextRange;
use hir::AnyExprId;
use hir::CallTarget;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFunctionClauseBody;
use hir::Literal;
use hir::Semantic;
use hir::Strategy;
use itertools::Itertools;
use serde::Deserialize;
use serde::Serialize;
use text_edit::TextEdit;

use super::Diagnostic;
use super::Severity;
use crate::codemod_helpers::find_call_in_function;
use crate::codemod_helpers::statement_range;
use crate::codemod_helpers::CheckCall;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::FunctionMatch;
use crate::codemod_helpers::FunctionMatcher;
use crate::codemod_helpers::MakeDiagCtx;
use crate::codemod_helpers::MFA;
use crate::diagnostics::DiagnosticCode;
use crate::fix;

pub type DiagnosticBuilder<'a> = &'a dyn Fn(&MFA, &str, TextRange) -> Option<Diagnostic>;

pub fn replace_call_site(
    mfa: &FunctionMatch,
    replacement: Replacement,
    diagnostic_builder: DiagnosticBuilder,
    acc: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
    replace_call_site_if_args_match(
        mfa,
        &|ctx| Some(("".to_string(), replacement.extra_info(&ctx))),
        &replacement,
        diagnostic_builder,
        acc,
        sema,
        file_id,
    )
}

pub fn replace_call_site_if_args_match(
    fm: &FunctionMatch,
    args_match: CheckCall<(), (String, String)>,
    replacement: &Replacement,
    diagnostic_builder: DiagnosticBuilder,
    acc: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
    sema.def_map(file_id).get_functions().for_each(|(_, def)| {
        if def.file.file_id == file_id {
            find_call_in_function(
                acc,
                sema,
                def,
                &[(fm, ())],
                &args_match,
                &move |MakeDiagCtx {
                           sema,
                           def_fb,
                           target,
                           args,
                           extra,
                           range,
                       }: MakeDiagCtx<'_, (String, String)>| {
                    let mfa = MFA::from_call_target(
                        target,
                        args.len() as u32,
                        sema,
                        &def_fb.body(),
                        file_id,
                    )?;
                    let mfa_str = mfa.label();

                    let diag = diagnostic_builder(&mfa, &extra.0, range)?;

                    if let Some(edit) =
                        replace_call(replacement, sema, def_fb, file_id, args, target, &range)
                    {
                        Some(diag.with_fixes(Some(vec![fix(
                            "replace_call_site",
                            &format!("Replace call to '{:?}' {}", &mfa_str, extra.1),
                            SourceChange::from_text_edit(file_id, edit),
                            range,
                        )])))
                    } else {
                        Some(diag)
                    }
                },
            );
        }
    });
}

#[allow(dead_code)] // @oss-only
pub fn adhoc_diagnostic(mfa: &MFA, extra_info: &str, range: TextRange) -> Option<Diagnostic> {
    let mfa_str = mfa.label();
    let sep = if extra_info.is_empty() { "" } else { " " };
    let diag = Diagnostic::new(
        DiagnosticCode::AdHoc(mfa_str.clone()),
        format!("'{}' called{}{}", &mfa_str, &sep, &extra_info),
        range,
    )
    .with_severity(Severity::WeakWarning)
    .experimental();
    Some(diag)
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Replacement {
    UseOk,
    UseCallArg {
        n: u32,
    },
    Invocation {
        replacement: String,
    },
    /// List of re-arrangements to be made to the argument list.
    /// The first applies to the the first argument, the second to the second, etc.
    /// Each gives the number of the original argument that should be placed there.
    /// Replacement counting starts from 0 for the first argument.
    /// The arity of the changed function will be determined by the
    /// number of entries in the permutation.
    ArgsPermutation {
        perm: Vec<u32>,
    },
}

impl Replacement {
    /// Provide extra info describing the change for use in a diagnostic
    fn extra_info<T>(&self, _ctx: &CheckCallCtx<T>) -> String {
        match self {
            Replacement::UseOk => "with ok".to_string(),
            Replacement::UseCallArg { n } => format!("with argument {n}"),
            Replacement::Invocation { replacement } => format!("with '{replacement}'"),
            Replacement::ArgsPermutation { perm } => format!("with arguments permuted {:?}", perm),
        }
    }
}

fn replace_call(
    replacement: &Replacement,
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    file_id: FileId,
    args: &[ExprId],
    target: &CallTarget<ExprId>,
    call_loc: &TextRange,
) -> Option<TextEdit> {
    let mut edit_builder = TextEdit::builder();
    match replacement {
        Replacement::UseOk => {
            edit_builder.replace(*call_loc, "ok".to_owned());
            Some(edit_builder.finish())
        }
        Replacement::UseCallArg { n } => {
            let &nth = args.get(*n as usize)?;

            let body_map = in_clause.get_body_map();
            let source_file = sema.parse(file_id);

            let nth_str = body_map.expr(nth)?.to_node(&source_file)?.to_string();
            edit_builder.replace(*call_loc, nth_str);
            Some(edit_builder.finish())
        }
        Replacement::Invocation { replacement } => {
            let range: TextRange = match target {
                CallTarget::Local { name } => in_clause
                    .range_for_expr(name.to_owned())
                    .expect("name in local call not found in function body."),
                CallTarget::Remote { module, name } => {
                    let range_module = in_clause
                        .range_for_expr(module.to_owned())
                        .expect("module in remote call not found in function body.");
                    let range_name = in_clause
                        .range_for_expr(name.to_owned())
                        .expect("name in remote call not found in function body.");
                    TextRange::new(range_module.start(), range_name.end())
                }
            };
            edit_builder.replace(range, replacement.to_owned());
            Some(edit_builder.finish())
        }
        Replacement::ArgsPermutation { perm } => {
            let body_map = in_clause.get_body_map();
            let source_file = sema.parse(file_id);
            let opt_args_str: Option<Vec<String>> = args
                .iter()
                .map(|expr| {
                    body_map
                        .expr(*expr)
                        .and_then(|expr| expr.to_node(&source_file))
                        .map(|source| source.to_string())
                })
                .collect();
            if let Some(args_str) = opt_args_str {
                if !args_str.is_empty() {
                    let mut permuted_args: Vec<&String> = Vec::default();
                    perm.iter().for_each(|new| {
                    let new = *new as usize;
                    if new < args_str.len() {
                        permuted_args.push(&args_str[new]);
                    } else {
                        log::warn!(
                            "replace_call: permutation not applicable: {:?}, args.len()={}, so valid is [0..{}]",
                            perm,
                            args_str.len(),
                            args_str.len() - 1
                        );
                    }
                });
                    let replacement = permuted_args.iter().join(", ");
                    if let Some(range_args) = args
                        .iter()
                        .map(|&id| {
                            in_clause
                                .range_for_expr(id)
                                .expect("arg in permutation not found in function body.")
                        })
                        .reduce(|a, b| a.cover(b))
                    {
                        edit_builder.replace(range_args, replacement);
                    }
                }
            }
            Some(edit_builder.finish())
        }
    }
}

pub fn remove_fun_ref_from_list(
    fm: &FunctionMatch,
    diagnostic_builder: DiagnosticBuilder,
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
    let mfas = vec![(fm, ())];
    let matcher = FunctionMatcher::new(&mfas);
    sema.def_map(file_id).get_functions().for_each(|(_, def)| {
        if def.file.file_id == file_id {
            let def_fb = def.in_function_body(sema, def);
            let source_file = sema.parse(file_id);
            def_fb.clone().fold_function(
                Strategy::InvisibleMacros,
                (),
                &mut |_acc, clause_id, ctx| {
                    let body_map = def_fb.get_body_map(clause_id);
                    let in_clause = def_fb.in_clause(clause_id);
                    match ctx.item_id {
                        AnyExprId::Expr(expr_id) => {
                            let matches = match_fun_ref_in_list_in_call_arg(
                                &matcher, sema, in_clause, &expr_id,
                            );
                            matches
                                .iter()
                                .for_each(|(matched_funref_id, target, arity)| {
                                    || -> Option<()> {
                                        let in_file_ast_ptr = body_map.expr(*matched_funref_id)?;
                                        let list_elem_ast =
                                            in_file_ast_ptr.to_node(&source_file)?;
                                        let statement_removal = remove_statement(&list_elem_ast)?;
                                        let mfa = MFA::from_call_target(
                                            target,
                                            *arity,
                                            sema,
                                            &def_fb.body(clause_id),
                                            file_id,
                                        )?;
                                        let range = def_fb
                                            .clone()
                                            .range_for_expr(clause_id, *matched_funref_id)?;
                                        let diag = diagnostic_builder(&mfa, "", range)?;
                                        diags.push(diag.with_fixes(Some(vec![fix(
                                            "remove_fun_ref_from_list",
                                            "Remove noop fun ref from list",
                                            SourceChange::from_text_edit(
                                                file_id,
                                                statement_removal,
                                            ),
                                            range,
                                        )])));
                                        Some(())
                                    }();
                                });
                        }
                        _ => {}
                    }
                },
            );
        }
    })
}

fn match_fun_ref_in_list_in_call_arg<T>(
    matcher: &FunctionMatcher<T>,
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    expr_id: &ExprId,
) -> Vec<(ExprId, CallTarget<ExprId>, u32)> {
    let mut result = vec![];
    let expr = &in_clause[*expr_id];
    if let Expr::Call { args, .. } = expr {
        args.iter().for_each(|arg_id| {
            let arg = &in_clause[*arg_id];
            if let Expr::List { exprs, .. } = arg {
                exprs.iter().for_each(|list_elem_id| {
                    let list_elem = &in_clause[*list_elem_id];
                    if let Expr::CaptureFun {
                        target,
                        arity: arity_expr_id,
                    } = list_elem
                    {
                        if let Expr::Literal(Literal::Integer(arity)) = &in_clause[*arity_expr_id] {
                            if matcher
                                .get_match(target, *arity as u32, None, sema, &in_clause.body())
                                .is_some()
                            {
                                result.push((*list_elem_id, target.clone(), *arity as u32));
                            }
                        }
                    }
                })
            }
        })
    }
    result
}

fn remove_statement(expr: &ast::Expr) -> Option<TextEdit> {
    let range = statement_range(expr);

    let mut edit_builder = TextEdit::builder();
    edit_builder.delete(range);
    Some(edit_builder.finish())
}

#[cfg(test)]
mod tests {

    use std::sync::Arc;

    use hir::Expr;
    use hir::Literal;

    use super::*;
    use crate::codemod_helpers::CheckCallCtx;
    use crate::diagnostics::AdhocSemanticDiagnostics;
    use crate::diagnostics::Lint;
    use crate::diagnostics::LintsFromConfig;
    use crate::diagnostics::ReplaceCall;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix_with_config;
    use crate::DiagnosticsConfig;

    #[track_caller]
    pub(crate) fn check_fix_with_ad_hoc_semantics<'a>(
        ad_hoc_semantic_diagnostics: Vec<&'a dyn AdhocSemanticDiagnostics>,
        fixture_before: &str,
        fixture_after: &str,
    ) {
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
            .disable(DiagnosticCode::UndefinedFunction)
            .set_ad_hoc_semantic_diagnostics(ad_hoc_semantic_diagnostics);
        check_fix_with_config(config, fixture_before, fixture_after)
    }

    #[track_caller]
    pub(crate) fn check_fix_with_lints_from_config<'a>(
        lints_from_config: LintsFromConfig,
        fixture_before: &str,
        fixture_after: &str,
    ) {
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
            .disable(DiagnosticCode::UndefinedFunction)
            .from_config(&Arc::new(lints_from_config));
        check_fix_with_config(config, fixture_before, fixture_after)
    }

    #[track_caller]
    pub(crate) fn check_diagnostics_with_ad_hoc_semantics<'a>(
        ad_hoc_semantic_diagnostics: Vec<&'a dyn AdhocSemanticDiagnostics>,
        fixture: &str,
    ) {
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
            .disable(DiagnosticCode::UndefinedFunction)
            .set_ad_hoc_semantic_diagnostics(ad_hoc_semantic_diagnostics);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn check_fix_remove_call_use_ok() {
        check_fix_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                replace_call_site(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 1,
                    }),
                    Replacement::UseOk,
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/blah_SUITE.erl
            -module(blah_SUITE).

            end_per_suite(Config) ->
                ~foo:fire_bombs(Config).
            //- /src/foo.erl
            -module(foo).
            fire_bombs(Config) ->
                boom.
            "#,
            r#"
            -module(blah_SUITE).

            end_per_suite(Config) ->
                ok.
            "#,
        )
    }

    #[test]
    fn check_fix_remove_call_use_call_args() {
        check_fix_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                replace_call_site(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 2,
                    }),
                    Replacement::UseCallArg { n: 1 },
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/blah_SUITE.erl
            -module(blah_SUITE).

            end_per_suite(Config) ->
                Message = ~foo:fire_bombs(Config, qwerty),
                transmit(Message).
            //- /src/foo.erl
            -module(foo).
            fire_bombs(FooConfig, MissilesCode) ->
                boom.
            "#,
            r#"
            -module(blah_SUITE).

            end_per_suite(Config) ->
                Message = qwerty,
                transmit(Message).
            "#,
        )
    }

    #[test]
    fn check_remove_call_site_if_args_match_uses_match() {
        check_diagnostics_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                replace_call_site_if_args_match(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 2,
                    }),
                    &|CheckCallCtx {
                          args, in_clause, ..
                      }: CheckCallCtx<()>| match in_clause[args[1]] {
                        Expr::Literal(Literal::Integer(42)) => {
                            Some(("with 42".to_string(), "for fix".to_string()))
                        }
                        _ => None,
                    },
                    &Replacement::UseOk,
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/blah_SUITE.erl
            -module(blah_SUITE).

            end_per_suite(Config) ->
                foo:fire_bombs(Config, 44),
                foo:fire_bombs(Config, 43),
                foo:fire_bombs(Config, 42),
            %%% ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: 'foo:fire_bombs/2' called with 42
                foo:fire_bombs(Config, 41),
                foo:fire_bombs(Config, 40).
            //- /src/foo.erl
            -module(foo).
            fire_bombs(A,B) -> {A,B}.
            "#,
        )
    }

    #[test]
    fn check_fix_remove_fun_ref_from_list_first() {
        check_fix_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                remove_fun_ref_from_list(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 1,
                    }),
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/blah_SUITE.erl
            -module(blah_SUITE).

            end_per_suite(Config) ->
                some:clearner_helper([
                    f~un foo:fire_bombs/1,
                    fun foo:regret_it/1,
                    fun foo:too_late_now_think_twice_next_time/2
                ], Config).
            //- /src/foo.erl
            -module(foo).
            -export([fire_bombs/1, regret_it/1, too_late_now_think_twice_next_time/2]).
            fire_bombs(Config) -> boom.
            regret_it(Config) -> oops.
            too_late_now_think_twice_next_time(Config) -> 'oh well'.
            "#,
            r#"
            -module(blah_SUITE).

            end_per_suite(Config) ->
                some:clearner_helper([
                    fun foo:regret_it/1,
                    fun foo:too_late_now_think_twice_next_time/2
                ], Config).
            "#,
        );
    }

    #[test]
    fn check_fix_remove_fun_ref_from_list_middle() {
        check_fix_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                remove_fun_ref_from_list(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 1,
                    }),
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/blah_SUITE.erl
            -module(blah_SUITE).

            end_per_suite(Config) ->
                some:clearner_helper([
                    fun foo:regret_it/1,
                    f~un foo:fire_bombs/1,
                    fun foo:too_late_now_think_twice_next_time/2
                ], Config).
            //- /src/foo.erl
            -module(foo).
            -export([fire_bombs/1, regret_it/1, too_late_now_think_twice_next_time/2]).
            fire_bombs(Config) -> boom.
            regret_it(Config) -> oops.
            too_late_now_think_twice_next_time(Config) -> 'oh well'.
            "#,
            r#"
            -module(blah_SUITE).

            end_per_suite(Config) ->
                some:clearner_helper([
                    fun foo:regret_it/1,
                    fun foo:too_late_now_think_twice_next_time/2
                ], Config).
            "#,
        );
    }

    #[test]
    fn check_fix_remove_fun_ref_from_list_last() {
        check_fix_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                remove_fun_ref_from_list(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 1,
                    }),
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/blah_SUITE.erl
            -module(blah_SUITE).

            end_per_suite(Config) ->
                some:clearner_helper([
                    fun foo:regret_it/1,
                    fun foo:too_late_now_think_twice_next_time/2,
                    f~un foo:fire_bombs/1
                ], Config).
            //- /src/foo.erl
            -module(foo).
            -export([fire_bombs/1, regret_it/1, too_late_now_think_twice_next_time/2]).
            fire_bombs(Config) -> boom.
            regret_it(Config) -> oops.
            too_late_now_think_twice_next_time(Config) -> 'oh well'.
            "#,
            r#"
            -module(blah_SUITE).

            end_per_suite(Config) ->
                some:clearner_helper([
                    fun foo:regret_it/1,
                    fun foo:too_late_now_think_twice_next_time/2
                ], Config).
            "#,
        );
    }

    #[test]
    fn check_fix_remove_fun_ref_from_list_singleton() {
        check_fix_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                remove_fun_ref_from_list(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 1,
                    }),
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/blah_SUITE.erl
            -module(blah_SUITE).

            end_per_suite(Config) ->
                some:clearner_helper([
                    f~un foo:fire_bombs/1
                ], Config).
            //- /src/foo.erl
            -module(foo).
            -export([fire_bombs/1, regret_it/1, too_late_now_think_twice_next_time/2]).
            fire_bombs(Config) -> boom.
            regret_it(Config) -> oops.
            too_late_now_think_twice_next_time(Config) -> 'oh well'.
            "#,
            r#"
            -module(blah_SUITE).

            end_per_suite(Config) ->
                some:clearner_helper([], Config).
            "#,
        );
    }

    #[test]
    fn check_fix_replace_call_replace_call() {
        check_fix_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                replace_call_site(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 2,
                    }),
                    Replacement::Invocation {
                        replacement: "blah:drop_water".to_owned(),
                    },
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/blah_SUITE.erl
            -module(blah_SUITE).

            end_per_suite(Config) ->
                Message = ~foo:fire_bombs(Config, qwerty),
                transmit(Message).
            //- /src/foo.erl
            -module(foo).
            fire_bombs(Config, MissilesCode) ->
                boom.
            "#,
            r#"
            -module(blah_SUITE).

            end_per_suite(Config) ->
                Message = blah:drop_water(Config, qwerty),
                transmit(Message).
            "#,
        )
    }

    #[test]
    fn check_fix_replace_call_permutation() {
        check_fix_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                replace_call_site(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 5,
                    }),
                    Replacement::ArgsPermutation {
                        perm: vec![1, 3, 2],
                    },
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/blah_SUITE.erl
            -module(blah_SUITE).

            end_per_suite(_Config) ->
                Message = ~foo:fire_bombs(Zeroth, First, Second, Third, Fourth),
                transmit(Message).
            //- /src/foo.erl
            -module(foo).
            fire_bombs(_Config, _MissilesCode, 1, 3, 4) ->
                boom.
            "#,
            r#"
            -module(blah_SUITE).

            end_per_suite(_Config) ->
                Message = foo:fire_bombs(First, Third, Second),
                transmit(Message).
            "#,
        )
    }

    #[test]
    fn check_fix_replace_call_permutation_expand_arity() {
        check_fix_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                replace_call_site(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 5,
                    }),
                    Replacement::ArgsPermutation {
                        perm: vec![3, 2, 1, 2, 2, 3, 3],
                    },
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/blah_SUITE.erl
            -module(blah_SUITE).

            end_per_suite(_Config) ->
                Message = ~foo:fire_bombs(Zeroth, First, Second, Third, Fourth),
                transmit(Message).
            //- /src/foo.erl
            -module(foo).
            fire_bombs(_Config, _MissilesCode, 1, 3, 4) ->
                boom.
            "#,
            r#"
            -module(blah_SUITE).

            end_per_suite(_Config) ->
                Message = foo:fire_bombs(Third, Second, First, Second, Second, Third, Third),
                transmit(Message).
            "#,
        )
    }

    #[test]
    fn check_fix_remove_fun_ref_from_list_singleton_via_config() {
        let lints = vec![Lint::ReplaceCall(ReplaceCall {
            matcher: FunctionMatch::MFA(MFA {
                module: "foo".into(),
                name: "fire_bombs".into(),
                arity: 1,
            }),
            action: crate::diagnostics::ReplaceCallAction::RemoveFromList,
        })];
        let lints_from_config = LintsFromConfig { lints };
        check_fix_with_lints_from_config(
            lints_from_config,
            r#"
            //- /src/blah_SUITE.erl
            -module(blah_SUITE).

            end_per_suite(Config) ->
                some:clearner_helper([
                    f~un foo:fire_bombs/1
                ], Config).
            //- /src/foo.erl
            -module(foo).
            -export([fire_bombs/1, regret_it/1, too_late_now_think_twice_next_time/2]).
            fire_bombs(Config) -> boom.
            regret_it(Config) -> oops.
            too_late_now_think_twice_next_time(Config) -> 'oh well'.
            "#,
            r#"
            -module(blah_SUITE).

            end_per_suite(Config) ->
                some:clearner_helper([], Config).
            "#,
        );
    }
}
