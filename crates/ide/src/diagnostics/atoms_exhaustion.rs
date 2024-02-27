/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FileId;
use hir::FunctionDef;
use hir::Semantic;
use text_edit::TextRange;

use crate::codemod_helpers::find_call_in_function;
use crate::codemod_helpers::CheckCallCtx;
// @fb-only: use crate::diagnostics;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;
use crate::FunctionMatch;

pub(crate) fn atoms_exhaustion(
    diagnostics: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
    let mut mfas = vec![
        FunctionMatch::mfa("erlang", "binary_to_atom", 1),
        FunctionMatch::mfa("erlang", "binary_to_atom", 2),
        FunctionMatch::mfa("erlang", "list_to_atom", 1),
        FunctionMatch::mfa("erlang", "binary_to_term", 1),
        FunctionMatch::mfa("erlang", "binary_to_term", 2),
    ];

    // @fb-only: let extra_mfas = diagnostics::meta_only::atoms_exhaustion_matches();
    let extra_mfas = vec![]; // @oss-only
    mfas.extend(extra_mfas);

    let mfas = mfas.iter().map(|matcher| (matcher, ())).collect::<Vec<_>>();

    sema.def_map(file_id)
        .get_functions()
        .for_each(|(_arity, def)| {
            if def.file.file_id == file_id {
                check_function(diagnostics, sema, def, &mfas);
            }
        });
}

pub(crate) fn check_function(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    mfas: &[(&FunctionMatch, ())],
) {
    find_call_in_function(
        diags,
        sema,
        def,
        mfas,
        &move |CheckCallCtx {
                   args, in_clause, ..
               }: CheckCallCtx<'_, ()>| match args[..] {
            [_, options] => {
                let body = in_clause.body();
                match &body[options].literal_list_contains_atom(&in_clause, "safe") {
                    Some(true) => None,
                    _ => Some(("".to_string(), "".to_string())),
                }
            }
            _ => Some(("".to_string(), "".to_string())),
        },
        move |sema, _def_fb, _target, _args, _diag_extra, _fix_extra, range| {
            let diag = make_diagnostic(sema, def.file.file_id, range);
            Some(diag)
        },
    );
}

fn make_diagnostic(sema: &Semantic, file_id: FileId, range: TextRange) -> Diagnostic {
    let message = "Risk of atoms exhaustion.".to_string();
    Diagnostic::new(DiagnosticCode::AtomsExhaustion, message, range)
        .experimental()
        .with_severity(Severity::Error)
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
%%   ^^^^^^^^^^^^^^^^^^^^^^^^ 💡 error: Risk of atoms exhaustion.
     list_to_atom(foo).
%%   ^^^^^^^^^^^^^^^^^ 💡 error: Risk of atoms exhaustion.

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
%%   ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 error: Risk of atoms exhaustion.
     binary_to_atom(foo).
%%   ^^^^^^^^^^^^^^^^^^^ 💡 error: Risk of atoms exhaustion.

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
%%   ^^^^^^^^^^^^^^^^^^^ 💡 error: Risk of atoms exhaustion.
     Bar = term_to_binary(bar),
     erlang:binary_to_term(Bar).
%%   ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 error: Risk of atoms exhaustion.

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
%%   ^^^^^^^^^^^^^^^^^^^^^^^ 💡 error: Risk of atoms exhaustion.
     Bar = term_to_binary(bar),
     erlang:binary_to_term(Bar, []).
%%   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 error: Risk of atoms exhaustion.

//- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
   -module(erlang).
   -export([binary_to_term/2, term_to_binary/1]).
   binary_to_term(_, _) -> ok.
   term_to_binary(_) -> ok.
            "#,
        )
    }
}
