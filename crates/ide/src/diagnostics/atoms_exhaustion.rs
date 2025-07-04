/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::elp_base_db::FileId;
use elp_text_edit::TextRange;
use hir::FunctionDef;
use hir::Semantic;
use lazy_static::lazy_static;

use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use crate::FunctionMatch;
use crate::codemod_helpers::MatchCtx;
use crate::codemod_helpers::find_call_in_function;
// @fb-only
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: true,
        include_tests: false,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        atoms_exhaustion(diags, sema, file_id);
    },
};

fn atoms_exhaustion(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    lazy_static! {
        static ref BAD_CALLS: Vec<FunctionMatch> = vec![
            FunctionMatch::mfa("erlang", "binary_to_atom", 1),
            FunctionMatch::mfa("erlang", "binary_to_atom", 2),
            FunctionMatch::mfa("erlang", "list_to_atom", 1),
            // T187850479: Make it configurable
            // FunctionMatch::mfa("erlang", "binary_to_term", 1),
            // FunctionMatch::mfa("erlang", "binary_to_term", 2),
        ]
        .into_iter()
        // @fb-only
        .collect();

        static ref BAD_CALLS_MFAS: Vec<(&'static FunctionMatch, ())> = BAD_CALLS
            .iter()
            .map(|matcher| (matcher, ()))
            .collect::<Vec<_>>();
    }

    sema.def_map_local(file_id)
        .get_functions()
        .for_each(|(_arity, def)| {
            // @fb-only
            let is_relevant = true; // @oss-only
            if is_relevant {
                check_function(diagnostics, sema, def, &BAD_CALLS_MFAS);
            }
        });
}

fn check_function(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    mfas: &[(&FunctionMatch, ())],
) {
    #[rustfmt::skip]
    find_call_in_function(
        diags,
        sema,
        def,
        mfas,
        &move |ctx| {
            // @fb-only
                // @fb-only
            let is_safe = false; // @oss-only
            if !is_safe {
                match ctx.args.as_vec()[..] {
                    [_, options] => {
                        let body = ctx.in_clause.body();
                        match &body[options].literal_list_contains_atom(ctx.in_clause, "safe") {
                            Some(true) => None,
                            _ => Some(("".to_string(), "".to_string())),
                        }
                    }
                    _ => Some(("".to_string(), "".to_string())),
                }
            } else {
                None
            }
        },
        &move |MatchCtx { sema, range, .. }| {
            if range.file_id == def.file.file_id {
                let diag = make_diagnostic(sema, def.file.file_id, range.range);
                Some(diag)
            } else {
                None
            }
        },
    );
}

fn make_diagnostic(sema: &Semantic, file_id: FileId, range: TextRange) -> Diagnostic {
    let message = "Risk of atoms exhaustion.".to_string();
    Diagnostic::new(DiagnosticCode::AtomsExhaustion, message, range)
        .with_severity(Severity::Warning)
        .with_ignore_fix(sema, file_id)
}

#[cfg(test)]
mod tests {

    use crate::tests;

    #[test]
    fn test_list_to_atom() {
        tests::check_diagnostics(
            r#"
//- /src/main.erl
   -module(main).
   -export([main/0]).
   main() ->
     erlang:list_to_atom(foo),
%%   ^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Risk of atoms exhaustion.
     list_to_atom(foo).
%%   ^^^^^^^^^^^^^^^^^ 💡 warning: Risk of atoms exhaustion.

//- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
   -module(erlang).
   -export([list_to_atom/1]).
   list_to_atom(_) -> ok.
            "#,
        )
    }

    #[test]
    fn test_binary_to_atom() {
        tests::check_diagnostics(
            r#"
//- /src/main.erl
   -module(main).
   -export([main/0]).
   main() ->
     erlang:binary_to_atom(foo),
%%   ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Risk of atoms exhaustion.
     binary_to_atom(foo).
%%   ^^^^^^^^^^^^^^^^^^^ 💡 warning: Risk of atoms exhaustion.

//- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
   -module(erlang).
   -export([binary_to_atom/1]).
   binary_to_atom(_) -> ok.
            "#,
        )
    }

    #[test]
    fn test_binary_to_term() {
        tests::check_diagnostics(
            r#"
//- /src/main.erl
   -module(main).
   -export([main/0]).
   main() ->
     Foo = term_to_binary(foo),
     binary_to_term(Foo),
     Bar = term_to_binary(bar),
     erlang:binary_to_term(Bar).

//- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
   -module(erlang).
   -export([binary_to_term/1, term_to_binary/1]).
   binary_to_term(_) -> ok.
   term_to_binary(_) -> ok.
            "#,
        )
    }

    #[test]
    fn test_binary_to_term_safe() {
        tests::check_diagnostics(
            r#"
//- /src/main.erl
   -module(main).
   -export([main/0]).
   main() ->
     Foo = term_to_binary(foo),
     binary_to_term(Foo, [safe]),

     Bar = term_to_binary(bar),
     erlang:binary_to_term(Bar, [safe]).

//- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
   -module(erlang).
   -export([binary_to_term/2, term_to_binary/1]).
   binary_to_term(_, _) -> ok.
   term_to_binary(_) -> ok.
            "#,
        )
    }

    #[test]
    fn test_binary_to_term_unsafe() {
        tests::check_diagnostics(
            r#"
//- /src/main.erl
   -module(main).
   -export([main/0]).
   main() ->
     Foo = term_to_binary(foo),
     binary_to_term(Foo, []),
     Bar = term_to_binary(bar),
     erlang:binary_to_term(Bar, []).

//- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
   -module(erlang).
   -export([binary_to_term/2, term_to_binary/1]).
   binary_to_term(_, _) -> ok.
   term_to_binary(_) -> ok.
            "#,
        )
    }
}
