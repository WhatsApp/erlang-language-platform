/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_assists::Assist;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use hir::FunctionDef;
use hir::Semantic;
use lazy_static::lazy_static;
use text_edit::TextRange;

use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::MatchCtx;
use crate::codemod_helpers::find_call_in_function;
use crate::codemod_helpers::statement_range;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::DiagnosticConditions;
use crate::diagnostics::DiagnosticDescriptor;
use crate::diagnostics::Severity;

const DIAGNOSTIC_CODE: DiagnosticCode = DiagnosticCode::DebuggingFunction;
const DIAGNOSTIC_MESSAGE: &str = "Debugging functions should only be used during local debugging and usages should not be checked in.";
const DIAGNOSTIC_SEVERITY: Severity = Severity::Error;
const REMOVE_FIX_ID: &str = "remove_invocation";
const REMOVE_FIX_LABEL: &str = "Remove invocation";

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: true,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        debugging_functions(diags, sema, file_id);
    },
};

fn debugging_functions(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    lazy_static! {
        static ref BAD_CALLS: Vec<FunctionMatch> =
            vec![FunctionMatch::m("redbug"),].into_iter().collect();
        static ref BAD_CALLS_MFAS: Vec<(&'static FunctionMatch, ())> = BAD_CALLS
            .iter()
            .map(|matcher| (matcher, ()))
            .collect::<Vec<_>>();
    }

    sema.def_map_local(file_id)
        .get_functions()
        .for_each(|(_arity, def)| {
            check_function(diagnostics, sema, def, &BAD_CALLS_MFAS, file_id);
        });
}

fn check_function(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    mfas: &[(&FunctionMatch, ())],
    file_id: FileId,
) {
    let source_file = sema.parse(file_id);
    find_call_in_function(
        diags,
        sema,
        def,
        mfas,
        &move |CheckCallCtx { parents, .. }: CheckCallCtx<'_, ()>| {
            let call_expr_id = parents.last().cloned();
            Some(call_expr_id)
        },
        &move |MatchCtx {
                   sema,
                   range_mf_only,
                   def_fb,
                   extra,
                   ..
               }| {
            if let Some(hir::fold::ParentId::HirIdx(hir_idx)) = &extra {
                if let Some(expr_id) = hir_idx.as_expr_id() {
                    let body_map = def_fb.get_body_map();
                    let in_file_ast_ptr = body_map.expr(expr_id)?;
                    let expr_ast = in_file_ast_ptr.to_node(&source_file)?;
                    let range = statement_range(&expr_ast);
                    make_diagnostic(sema, def.file.file_id, range_mf_only, range)
                } else {
                    None
                }
            } else {
                None
            }
        },
    );
}

fn make_diagnostic(
    sema: &Semantic,
    file_id: FileId,
    range_mf_only: Option<TextRange>,
    range: TextRange,
) -> Option<Diagnostic> {
    let range_mf_only = range_mf_only?;
    let diagnostic = Diagnostic::new(DIAGNOSTIC_CODE, DIAGNOSTIC_MESSAGE, range_mf_only)
        .with_severity(DIAGNOSTIC_SEVERITY)
        .with_fixes(Some(vec![remove_fix(file_id, range)]))
        .with_ignore_fix(sema, file_id);
    Some(diagnostic)
}

fn remove_fix(file_id: FileId, range: TextRange) -> Assist {
    let mut builder = SourceChangeBuilder::new(file_id);
    builder.delete(range);
    let source_change = builder.finish();
    crate::fix(REMOVE_FIX_ID, REMOVE_FIX_LABEL, source_change, range)
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::diagnostics::debugging_function::REMOVE_FIX_LABEL;
    use crate::tests;

    #[test]
    fn test_redbug_usage() {
        tests::check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-export([error/0, noerror/0]).

error() ->
    redbug:start("io:format/2->return", []),
%%  ^^^^^^^^^^^^ 💡 error: Debugging functions should only be used during local debugging and usages should not be checked in.
    redbug:stop().
%%  ^^^^^^^^^^^ 💡 error: Debugging functions should only be used during local debugging and usages should not be checked in.

noerror() ->
    redbug(),
    start(redbug),
    what:redbug(not_exist).

redbug() ->
    ok.

start(_) ->
    ok.
 
//- /src/redbug.erl
-module(redbug).
-export([start/2, stop/0]).

start(_, _) ->
    ok.

stop() ->
    ok.

//- /src/what.erl
-module(what).
-export([redbug/1]).

redbug(_) ->
    ok.
            "#,
        )
    }

    #[test]
    fn test_redbug_ignore_fix() {
        tests::check_specific_fix(
            "Ignore problem",
            r#"
//- /src/main.erl
-module(main).
-export([main/0]).

main() ->
    re~dbug:start("io:format/2->return", []).
%%  ^^^^^^^^^^^^ 💡 error: Debugging functions should only be used during local debugging and usages should not be checked in.
//- /src/redbug.erl
-module(redbug).
-export([start/2]).

start(_, _) ->
    ok.
            "#,
            expect![
                r#"
-module(main).
-export([main/0]).

main() ->
    % elp:ignore W0041 (debugging_function)
    redbug:start("io:format/2->return", []).
"#
            ],
        )
    }

    #[test]
    fn test_redbug_remove_fix() {
        tests::check_specific_fix(
            REMOVE_FIX_LABEL,
            r#"
//- /src/main.erl
-module(main).
-export([main/0]).

main() ->
    re~dbug:start("io:format/2->return", []),
%%  ^^^^^^^^^^^^ 💡 error: Debugging functions should only be used during local debugging and usages should not be checked in.
    ok.
//- /src/redbug.erl
-module(redbug).
-export([start/2]).

start(_, _) ->
    ok.
            "#,
            expect![
                r#"
-module(main).
-export([main/0]).

main() ->
    ok.
"#
            ],
        )
    }
}
