/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: cannot-evaluate-ct-callbacks (W0021)
//
// Warning when Common Test callback functions (all/0, groups/0) cannot be
// evaluated by the erlang service. When this happens, no code lenses for
// tests will be available.

use elp_ide_db::common_test::CommonTestInfo;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::AstNode;
use hir::NameArity;
use hir::Semantic;
use hir::known;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

pub(crate) struct CannotEvaluateCTCallbacksLinter;

impl Linter for CannotEvaluateCTCallbacksLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::CannotEvaluateCTCallbacks
    }

    fn description(&self) -> &'static str {
        "Could not evaluate function. No code lenses for tests will be available."
    }

    fn runs_on_save_only(&self) -> bool {
        true
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.db.file_kind(file_id) == FileKind::TestModule
    }
}

impl GenericLinter for CannotEvaluateCTCallbacksLinter {
    type Context = ();

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<()>>> {
        match &*ctx.ct_info() {
            CommonTestInfo::EvalError(_) => {}
            _ => return None,
        }

        let def_map = ctx.sema.db.def_map(ctx.file_id);
        let results: Vec<_> = [
            NameArity::new(*known::all, 0),
            NameArity::new(*known::groups, 0),
        ]
        .into_iter()
        .filter_map(|name| {
            let def = def_map.get_function(&name)?;
            let clause_name = def.first_clause_name(ctx.sema.db.upcast())?;
            Some(GenericLinterMatchContext {
                range: FileRange {
                    file_id: ctx.file_id,
                    range: clause_name.syntax().text_range(),
                },
                context: (),
            })
        })
        .collect();

        if results.is_empty() {
            None
        } else {
            Some(results)
        }
    }
}

pub(crate) static LINTER: CannotEvaluateCTCallbacksLinter = CannotEvaluateCTCallbacksLinter;

#[cfg(test)]
mod tests {
    use crate::tests::check_diagnostics;

    #[test]
    fn test_cannot_eval_all() {
        check_diagnostics(
            r#"
//- /my_app/test/cannot_eval_all_SUITE.erl extra:test
   -module(cannot_eval_all_SUITE).
   -export([all/0]).
   -export([a/1, b/1, c/1]).
   all() -> my_external_helper:all().
%% ^^^ warning: W0021: Could not evaluate function. No code lenses for tests will be available.
%%   | 💡 <suppression>
   a(_Config) ->
     ok.
   b(_Config) ->
     ok.
   c(_Config) ->
     ok.
            "#,
        );
    }

    #[test]
    fn test_cannot_eval_groups() {
        check_diagnostics(
            r#"
//- /my_app/test/cannot_eval_all_groups_SUITE.erl extra:test
   -module(cannot_eval_all_groups_SUITE).
   -export([all/0, groups/0]).
   -export([a/1, b/1, c/1]).
   all() -> [a].
%% ^^^ warning: W0021: Could not evaluate function. No code lenses for tests will be available.
%%   | 💡 <suppression>
   groups() -> my_external_helper:groups().
%% ^^^^^^ warning: W0021: Could not evaluate function. No code lenses for tests will be available.
%%      | 💡 <suppression>
   a(_Config) ->
     ok.
   b(_Config) ->
     ok.
   c(_Config) ->
     ok.
            "#,
        );
    }
}
