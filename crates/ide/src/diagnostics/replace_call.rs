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
//!

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_syntax::ast;
use elp_syntax::TextRange;
use hir::Body;
use hir::CallTarget;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFunctionBody;
use hir::Literal;
use hir::Semantic;
use text_edit::TextEdit;

use super::Diagnostic;
use super::Severity;
use crate::codemod_helpers::find_call_in_function;
use crate::codemod_helpers::statement_range;
use crate::codemod_helpers::CheckCall;
use crate::codemod_helpers::FunctionMatch;
use crate::codemod_helpers::MFA;
use crate::diagnostics::DiagnosticCode;
use crate::fix;

pub type DiagnosticBuilder<'a> = &'a dyn Fn(&MFA, &str, TextRange) -> Option<Diagnostic>;

#[allow(dead_code)]
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
        &|_mfa, _, _target, _args, _def_fb| Some("".to_string()),
        replacement,
        diagnostic_builder,
        acc,
        sema,
        file_id,
    )
}

pub fn replace_call_site_if_args_match(
    fm: &FunctionMatch,
    args_match: CheckCall<()>,
    replacement: Replacement,
    diagnostic_builder: DiagnosticBuilder,
    acc: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
    sema.def_map(file_id)
        .get_functions()
        .iter()
        .for_each(|(_arity, def)| {
            if def.file.file_id == file_id {
                find_call_in_function(
                    acc,
                    sema,
                    def,
                    &vec![(fm, ())],
                    &args_match,
                    move |sema, mut def_fb, target, args, extra_info, range| {
                        let mfa = to_mfa(fm, target, args.len() as u32, sema, &def_fb.body())?;
                        let mfa_str = mfa.label();

                        let diag = diagnostic_builder(&mfa, extra_info, range)?;

                        if let Some(edit) =
                            replace_call(replacement, sema, &mut def_fb, file_id, args, &range)
                        {
                            Some(diag.with_fixes(Some(vec![fix(
                                "replace_call_site",
                                &format!("Replace call to '{:?}'", &mfa_str),
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

pub fn adhoc_diagnostic(mfa: &MFA, extra_info: &str, range: TextRange) -> Option<Diagnostic> {
    let mfa_str = mfa.label();
    let sep = if extra_info.len() > 0 { " " } else { "" };
    let diag = Diagnostic::new(
        DiagnosticCode::AdHoc(mfa_str.clone()),
        format!("'{}' called{}{}", &mfa_str, &sep, &extra_info),
        range.clone(),
    )
    .severity(Severity::WeakWarning)
    .experimental();
    return Some(diag);
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Replacement {
    UseOk,
    UseCallArg(u32),
}

fn replace_call(
    replacement: Replacement,
    sema: &Semantic,
    def_fb: &mut InFunctionBody<&FunctionDef>,
    file_id: FileId,
    args: &[ExprId],
    call_loc: &TextRange,
) -> Option<TextEdit> {
    let opt_replacement_str = match replacement {
        Replacement::UseOk => Some("ok".to_string()),
        Replacement::UseCallArg(n) => {
            let &nth = args.get(n as usize)?;

            let body_map = def_fb.get_body_map(sema.db);
            let source_file = sema.parse(file_id);

            let nth_str = body_map.expr(nth)?.to_node(&source_file)?.to_string();
            Some(nth_str)
        }
    };
    opt_replacement_str.map(|replacement_str| {
        let mut edit_builder = TextEdit::builder();
        edit_builder.replace(*call_loc, replacement_str);
        edit_builder.finish()
    })
}

#[allow(dead_code)]
pub fn remove_fun_ref_from_list(
    mfa: &MFA,
    diagnostic_builder: DiagnosticBuilder,
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
    sema.def_map(file_id)
        .get_functions()
        .iter()
        .for_each(|(_arity, def)| {
            if def.file.file_id == file_id {
                let def_fb = def.in_function_body(sema.db, def);
                def_fb.clone().fold_function(
                    (),
                    &mut |_acc, _, ctx| {
                        let matches =
                            match_fun_ref_in_list_in_call_arg(mfa, sema, &def_fb, &ctx.expr_id);
                        matches.iter().for_each(|matched_funref_id| {
                            if let Some(range) =
                                def_fb.clone().range_for_expr(sema.db, *matched_funref_id)
                            {
                                let body_map = def_fb.clone().get_body_map(sema.db);

                                if let Some(in_file_ast_ptr) = body_map.expr(*matched_funref_id) {
                                    let source_file = sema.parse(file_id);
                                    if let Some(list_elem_ast) =
                                        in_file_ast_ptr.to_node(&source_file)
                                    {
                                        if let Some(statement_removal) =
                                            remove_statement(&list_elem_ast)
                                        {
                                            if let Some(diag) =
                                                diagnostic_builder(mfa, "", range.clone())
                                            {
                                                diags.push(diag.with_fixes(Some(vec![fix(
                                                    "remove_fun_ref_from_list",
                                                    "Remove noop fun ref from list",
                                                    SourceChange::from_text_edit(
                                                        file_id,
                                                        statement_removal,
                                                    ),
                                                    range,
                                                )])));
                                            }
                                        }
                                    }
                                }
                            }
                        });
                    },
                    &mut |acc, _, _| acc,
                );
            }
        })
}

fn match_fun_ref_in_list_in_call_arg(
    funref_mfa: &MFA,
    sema: &Semantic,
    def_fb: &InFunctionBody<&FunctionDef>,
    expr_id: &ExprId,
) -> Vec<ExprId> {
    let mut result = vec![];
    let expr = &def_fb[*expr_id];
    match expr {
        Expr::Call { args, .. } => args.iter().for_each(|arg_id| {
            let arg = &def_fb[*arg_id];
            match arg {
                Expr::List { exprs, .. } => exprs.iter().for_each(|list_elem_id| {
                    let list_elem = &def_fb[*list_elem_id];
                    match list_elem {
                        Expr::CaptureFun {
                            target,
                            arity: arity_expr_id,
                        } => {
                            if let Expr::Literal(Literal::Integer(arity)) = &def_fb[*arity_expr_id]
                            {
                                let target_label =
                                    target.label(*arity as u32, sema, &def_fb.body());
                                let funref_label = &funref_mfa.label();
                                if target_label == Some(funref_label.into()) {
                                    result.push(*list_elem_id);
                                }
                            }
                        }
                        _ => {}
                    }
                }),
                _ => {}
            }
        }),
        _ => {}
    }
    return result;
}

fn remove_statement(expr: &ast::Expr) -> Option<TextEdit> {
    let range = statement_range(expr);

    let mut edit_builder = TextEdit::builder();
    edit_builder.delete(range);
    Some(edit_builder.finish())
}

fn to_mfa(
    fm: &FunctionMatch,
    target: &CallTarget<ExprId>,
    arity: u32,
    sema: &Semantic,
    body: &Body,
) -> Option<MFA> {
    match target {
        CallTarget::Local { name } => {
            let name = sema.db.lookup_atom(body[*name].as_atom()?);
            return Some(MFA::new(fm.module().as_str(), name.as_str(), arity));
        }
        CallTarget::Remote { module, name } => {
            let module = sema.db.lookup_atom(body[*module].as_atom()?);
            let name = sema.db.lookup_atom(body[*name].as_atom()?);
            return Some(MFA::new(module.as_str(), name.as_str(), arity));
        }
    }
}

#[cfg(test)]
mod tests {

    use hir::Expr;
    use hir::Literal;

    use super::*;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix_with_config;
    use crate::DiagnosticsConfig;

    #[test]
    fn check_fix_remove_call_use_ok() {
        let mut config = DiagnosticsConfig {
            adhoc_semantic_diagnostics: vec![&|acc, sema, file_id, _ext| {
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
            ..DiagnosticsConfig::default()
        };
        config
            .disabled
            .insert(DiagnosticCode::MissingCompileWarnMissingSpec);

        check_fix_with_config(
            config,
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
        let mut config = DiagnosticsConfig {
            adhoc_semantic_diagnostics: vec![&|acc, sema, file_id, _ext| {
                replace_call_site(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 2,
                    }),
                    Replacement::UseCallArg(1),
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            ..DiagnosticsConfig::default()
        };
        config
            .disabled
            .insert(DiagnosticCode::MissingCompileWarnMissingSpec);

        check_fix_with_config(
            config,
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
                Message = qwerty,
                transmit(Message).
            "#,
        )
    }

    #[test]
    fn check_remove_call_site_if_args_match_uses_match() {
        let mut config = DiagnosticsConfig {
            adhoc_semantic_diagnostics: vec![&|acc, sema, file_id, _ext| {
                replace_call_site_if_args_match(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 2,
                    }),
                    &|_, _, _target, args, def_fb| match &def_fb[args[1]] {
                        Expr::Literal(Literal::Integer(42)) => Some("with 42".to_string()),
                        _ => None,
                    },
                    Replacement::UseOk,
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            ..DiagnosticsConfig::default()
        };
        config
            .disabled
            .insert(DiagnosticCode::MissingCompileWarnMissingSpec);

        check_diagnostics_with_config(
            config,
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
            "#,
        )
    }

    #[test]
    fn check_fix_remove_fun_ref_from_list_first() {
        let mut config = DiagnosticsConfig {
            adhoc_semantic_diagnostics: vec![&|acc, sema, file_id, _ext| {
                remove_fun_ref_from_list(
                    &MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 1,
                    },
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            ..DiagnosticsConfig::default()
        };
        config
            .disabled
            .insert(DiagnosticCode::MissingCompileWarnMissingSpec);

        check_fix_with_config(
            config,
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
        let mut config = DiagnosticsConfig {
            adhoc_semantic_diagnostics: vec![&|acc, sema, file_id, _ext| {
                remove_fun_ref_from_list(
                    &MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 1,
                    },
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            ..DiagnosticsConfig::default()
        };
        config
            .disabled
            .insert(DiagnosticCode::MissingCompileWarnMissingSpec);

        check_fix_with_config(
            config,
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
        let mut config = DiagnosticsConfig {
            adhoc_semantic_diagnostics: vec![&|acc, sema, file_id, _ext| {
                remove_fun_ref_from_list(
                    &MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 1,
                    },
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            ..DiagnosticsConfig::default()
        };
        config
            .disabled
            .insert(DiagnosticCode::MissingCompileWarnMissingSpec);

        check_fix_with_config(
            config,
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
        let mut config = DiagnosticsConfig {
            adhoc_semantic_diagnostics: vec![&|acc, sema, file_id, _ext| {
                remove_fun_ref_from_list(
                    &MFA {
                        module: "foo".into(),
                        name: "fire_bombs".into(),
                        arity: 1,
                    },
                    &adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            ..DiagnosticsConfig::default()
        };

        config
            .disabled
            .insert(DiagnosticCode::MissingCompileWarnMissingSpec);

        check_fix_with_config(
            config,
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
