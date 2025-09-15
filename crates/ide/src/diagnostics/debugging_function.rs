/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_assists::Assist;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_text_edit::TextRange;
use hir::Semantic;

use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::MatchCtx;
use crate::codemod_helpers::statement_range;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
// @fb-only
use crate::lazy_function_matches;

pub(crate) struct NoDebuggingFunctionLinter;

impl Linter for NoDebuggingFunctionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::DebuggingFunction
    }
    fn description(&self) -> String {
        "Debugging functions should only be used during local debugging and usages should not be checked in.".to_string()
    }
    fn severity(&self) -> Severity {
        Severity::WeakWarning
    }
    fn cli_severity(&self) -> Severity {
        Severity::Error
    }
    fn should_process_generated_files(&self) -> bool {
        true
    }
}

impl FunctionCallLinter for NoDebuggingFunctionLinter {
    type Context = Option<hir::fold::ParentId>;

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![
            vec![FunctionMatch::m("redbug")]
                .into_iter()
                // @fb-only
                .collect::<Vec<_>>()
        ]
    }

    fn is_match_valid(
        &self,
        context: &CheckCallCtx<'_, ()>,
        _sema: &Semantic,
        _file_id: FileId,
    ) -> Option<Self::Context> {
        let call_expr_id = context.parents.last().cloned();
        Some(call_expr_id)
    }

    fn fixes(
        &self,
        match_context: &MatchCtx<Self::Context>,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let source_file = sema.parse(file_id);
        if let Some(hir::fold::ParentId::HirIdx(hir_idx)) = &match_context.extra {
            let expr_id = hir_idx.as_expr_id()?;
            let body_map = match_context.def_fb.get_body_map();
            let in_file_ast_ptr = body_map.expr(expr_id)?;
            let expr_ast = in_file_ast_ptr.to_node(&source_file)?;
            let range = statement_range(&expr_ast);
            Some(vec![remove_fix(file_id, range)])
        } else {
            None
        }
    }
}

pub static LINTER: NoDebuggingFunctionLinter = NoDebuggingFunctionLinter;

fn remove_fix(file_id: FileId, range: TextRange) -> Assist {
    let mut builder = SourceChangeBuilder::new(file_id);
    builder.delete(range);
    let source_change = builder.finish();
    crate::fix(
        "remove_invocation",
        "Remove invocation",
        source_change,
        range,
    )
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

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
%%  ^^^^^^^^^^^^ 💡 weak: Debugging functions should only be used during local debugging and usages should not be checked in.
    redbug:stop().
%%  ^^^^^^^^^^^ 💡 weak: Debugging functions should only be used during local debugging and usages should not be checked in.

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
%%  ^^^^^^^^^^^^ 💡 weak: Debugging functions should only be used during local debugging and usages should not be checked in.
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
            "Remove invocation",
            r#"
//- /src/main.erl
-module(main).
-export([main/0]).

main() ->
    re~dbug:start("io:format/2->return", []),
%%  ^^^^^^^^^^^^ 💡 weak: Debugging functions should only be used during local debugging and usages should not be checked in.
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
