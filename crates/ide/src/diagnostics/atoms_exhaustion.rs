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
use hir::Semantic;

use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
// @fb-only
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::lazy_function_matches;

pub(crate) struct AtomsExhaustionLinter;

impl Linter for AtomsExhaustionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::AtomsExhaustion
    }
    fn description(&self) -> String {
        "Risk of atoms exhaustion.".to_string()
    }
    fn should_process_generated_files(&self) -> bool {
        true
    }
    fn should_process_test_files(&self) -> bool {
        false
    }
    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        // @fb-only
        true // @oss-only
    }
}

impl FunctionCallLinter for AtomsExhaustionLinter {
    type Context = ();

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![
            vec![
                FunctionMatch::mfa("erlang", "binary_to_atom", 1),
                FunctionMatch::mfa("erlang", "binary_to_atom", 2),
                FunctionMatch::mfa("erlang", "list_to_atom", 1),
                // T187850479: Make it configurable
                // FunctionMatch::mfa("erlang", "binary_to_term", 1),
                // FunctionMatch::mfa("erlang", "binary_to_term", 2),
            ]
            .into_iter()
            // @fb-only
            .collect::<Vec<_>>()
        ]
    }

    fn check_match(
        &self,
        context: &CheckCallCtx<'_, ()>,
        sema: &Semantic,
        _file_id: FileId,
    ) -> Option<Self::Context> {
        #[rustfmt::skip]
        // @fb-only
            // @fb-only
        let is_safe = false; // @oss-only
        if !is_safe {
            match context.args.as_vec()[..] {
                [_, options] => {
                    let body = context.in_clause.body();
                    match &body[options].literal_list_contains_atom(context.in_clause, "safe") {
                        Some(true) => None,
                        _ => Some(()),
                    }
                }
                _ => Some(()),
            }
        } else {
            None
        }
    }
}

pub static LINTER: AtomsExhaustionLinter = AtomsExhaustionLinter;

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
%%   ^^^^^^^^^^^^^^^^^^^ 💡 warning: Risk of atoms exhaustion.
     list_to_atom(foo).
%%   ^^^^^^^^^^^^ 💡 warning: Risk of atoms exhaustion.

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
%%   ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Risk of atoms exhaustion.
     binary_to_atom(foo).
%%   ^^^^^^^^^^^^^^ 💡 warning: Risk of atoms exhaustion.

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
