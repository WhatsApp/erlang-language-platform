/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint: wid_tuple
//!
//! warn on code of the form `length(lists:flatten(L))` and suggest `lists:flatlength(L)`

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_db::DiagnosticCode;
use hir::Expr;
use hir::FunctionDef;
use hir::InFileAstPtr;
use hir::Semantic;
use text_edit::TextRange;

use crate::codemod_helpers::find_call_in_function;
use crate::codemod_helpers::Args;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::FunctionMatch;
use crate::codemod_helpers::FunctionMatcher;
use crate::codemod_helpers::MakeDiagCtx;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticConditions;
use crate::diagnostics::DiagnosticDescriptor;
use crate::diagnostics::Severity;
use crate::fix;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|acc, sema, file_id, _ext| {
        inefficient_flatlength(acc, sema, file_id);
    },
};

fn inefficient_flatlength(acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    sema.for_each_function(file_id, |def| check_function(acc, sema, def));
}

fn check_function(acc: &mut Vec<Diagnostic>, sema: &Semantic, def: &FunctionDef) {
    let erlang_length = FunctionMatch::mfa("erlang", "length", 1);
    let lists_flatten = FunctionMatch::mfa("lists", "flatten", 1);
    let lists_flatten_match = &vec![(&lists_flatten, ())];
    find_call_in_function(
        acc,
        sema,
        def,
        &vec![(&erlang_length, ())],
        &move |CheckCallCtx {
                   args,
                   in_clause: def_fb,
                   ..
               }: CheckCallCtx<'_, ()>| {
            let body_map = def_fb.get_body_map();
            let body = def_fb.body();
            match args {
                Args::Args(arg_elems) => match arg_elems[..] {
                    [erlang_length_arg] => match &body[erlang_length_arg] {
                        Expr::Call {
                            target: flatten_target,
                            args: flatten_args,
                        } => {
                            let lists_flatten_matcher = FunctionMatcher::new(lists_flatten_match);
                            if let Some(_) = lists_flatten_matcher.get_match(
                                flatten_target,
                                1,
                                Some(&arg_elems),
                                sema,
                                &body,
                            ) {
                                // We already verified that the function call was of arity 1
                                Some(body_map.expr(*flatten_args.first()?)?)
                            } else {
                                None
                            }
                        }
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            }
        },
        &move |MakeDiagCtx {
                   sema,
                   def_fb,
                   range: inefficent_call_range,
                   extra: nested_list_arg,
                   ..
               }| {
            Some(make_diagnostic(
                sema,
                def_fb.file_id(),
                inefficent_call_range,
                nested_list_arg,
            ))
        },
    );
}

fn make_diagnostic(
    sema: &Semantic,
    file_id: FileId,
    inefficient_call_range: TextRange,
    nested_list_arg: &InFileAstPtr<elp_syntax::ast::Expr>,
) -> Diagnostic {
    let message = "Unnecessary intermediate flat-list allocated.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let efficient_flatlength = format!(
        "lists:flatlength({})",
        nested_list_arg
            .to_node(&sema.parse(file_id))
            .unwrap()
            .to_string()
    );
    builder.replace(inefficient_call_range, efficient_flatlength);
    let fixes = vec![fix(
        "length_lists_flatten_to_lists_flatlength",
        "Rewrite to use lists:flatlength/1",
        builder.finish(),
        inefficient_call_range,
    )];
    Diagnostic::new(
        DiagnosticCode::ExpressionCanBeSimplified,
        message,
        inefficient_call_range,
    )
    .with_severity(Severity::Warning)
    .with_ignore_fix(sema, file_id)
    .with_fixes(Some(fixes))
}

#[cfg(test)]
mod tests {

    use expect_test::expect;
    use expect_test::Expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::ExpressionCanBeSimplified
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_filtered_diagnostics(fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        tests::check_fix(fixture_before, fixture_after)
    }

    #[test]
    fn detects_inefficient_flatlength() {
        check_diagnostics(
            r#"
         //- /src/inefficient_flatlength.erl
         -module(inefficient_flatlength).

         fn(NestedList) -> length(lists:flatten(NestedList)).
         %%                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Unnecessary intermediate flat-list allocated.
            "#,
        )
    }

    #[test]
    fn fixes_inefficient_flatlength_over_length_call() {
        check_fix(
            r#"
         //- /src/inefficient_flatlength.erl
         -module(inefficient_flatlength).

         % elp:ignore W0017 (undefined_function)
         fn(NestedList) -> len~gth(lists:flatten(NestedList)).
            "#,
            expect![[r#"
         -module(inefficient_flatlength).

         % elp:ignore W0017 (undefined_function)
         fn(NestedList) -> lists:flatlength(NestedList).
            "#]],
        )
    }

    #[test]
    fn fixes_inefficient_flatlength_over_flatten_call() {
        check_fix(
            r#"
         //- /src/inefficient_flatlength.erl
         -module(inefficient_flatlength).

         % elp:ignore W0017 (undefined_function)
         fn(NestedList) -> length(lists:fl~atten(NestedList)).
            "#,
            expect![[r#"
         -module(inefficient_flatlength).

         % elp:ignore W0017 (undefined_function)
         fn(NestedList) -> lists:flatlength(NestedList).
            "#]],
        )
    }
}
