/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unexported-function

use std::borrow::Cow;

use elp_ide_assists::Assist;
use elp_ide_assists::helpers;
use elp_ide_assists::helpers::ExportForm;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_syntax::SmolStr;
use hir::FunctionDef;
use hir::Semantic;

use super::DiagnosticCode;
use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::MatchCtx;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
// @fb-only
use crate::fix;
use crate::lazy_function_matches;

pub(crate) struct UnexportedFunctionLinter;

impl Linter for UnexportedFunctionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnexportedFunction
    }
    fn description(&self) -> &'static str {
        "Function is not exported."
    }
    fn should_process_generated_files(&self) -> bool {
        true
    }
    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        true // @oss-only
        // @fb-only
    }
}

impl FunctionCallLinter for UnexportedFunctionLinter {
    type Context = Option<(SmolStr, FunctionDef)>;

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![vec![FunctionMatch::any()]]
    }

    fn check_match(&self, context: &CheckCallCtx<'_, ()>) -> Option<Self::Context> {
        match context.target {
            hir::CallTarget::Remote { .. } => {
                let def_fb = context.in_clause;
                let arity = context.args.arity();
                let sema = context.in_clause.sema;
                if let Some(fun_def) =
                    context
                        .target
                        .resolve_call(arity, sema, def_fb.file_id(), &def_fb.body())
                    && !sema
                        .def_map(fun_def.file.file_id)
                        .is_function_exported(&fun_def.name)
                {
                    let label = context.target.label(arity, sema, &def_fb.body())?;
                    return Some(Some((label, fun_def)));
                }
            }
            hir::CallTarget::Local { .. } => (),
        }
        None
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        match context {
            None => Cow::Borrowed(self.description()),
            Some((label, _target_def)) => {
                Cow::Owned(format!("Function '{label}' is not exported."))
            }
        }
    }

    fn fixes(
        &self,
        match_context: &MatchCtx<Self::Context>,
        sema: &Semantic,
        _file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let (_label, target_def) = match_context.extra.as_ref()?;
        let target_file_id = target_def.file.file_id;
        let mut builder = SourceChangeBuilder::new(target_file_id);
        helpers::ExportBuilder::new(
            sema,
            target_file_id,
            ExportForm::Functions,
            std::slice::from_ref(&target_def.name),
            &mut builder,
        )
        .finish();
        let function_name = &target_def.name;
        Some(vec![fix(
            "export_function",
            format!("Export the function `{function_name}`").as_str(),
            builder.finish(),
            match_context.range.range,
        )])
    }
}
pub static LINTER: UnexportedFunctionLinter = UnexportedFunctionLinter;

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_nth_fix;

    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().disable(elp_ide_db::DiagnosticCode::NoSize);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn test_private() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main() ->
    dependency:exists(),
    dependency:private().
%%  ^^^^^^^^^^^^^^^^^^ 💡 warning: Function 'dependency:private/0' is not exported.
  exists() -> ok.
//- /src/dependency.erl
  -module(dependency).
  -export([exists/0]).
  exists() -> ok.
  private() -> ok.
            "#,
        )
    }

    #[test]
    fn test_private_same_module() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  main() ->
    ?MODULE:private(),
%%  ^^^^^^^^^^^^^^^ 💡 warning: Function 'main:private/0' is not exported.
    main:private().
%%  ^^^^^^^^^^^^ 💡 warning: Function 'main:private/0' is not exported.

  private() -> ok.
            "#,
        )
    }

    #[test]
    fn remote_call_to_header() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo() -> main:bar().
%%       ^^^^^^^^ 💡 warning: Function 'main:bar/0' is not exported.

//- /src/header.hrl
  bar() -> ok.
"#,
        );
    }

    #[test]
    fn test_export_fix_ignore() {
        check_nth_fix(
            1,
            r#"
//- /src/main.erl
-module(main).

main() ->
  dep:exists(),
  dep:pr~ivate().

//- /src/dep.erl
-module(dep).
-export([exists/0]).
exists() -> ok.
private() -> ok.
"#,
            expect![[r#"
-module(main).

main() ->
  dep:exists(),
  % elp:ignore W0026 (unexported_function)
  dep:private().

"#]],
            DiagnosticsConfig::default().set_experimental(true),
            &vec![],
            crate::tests::IncludeCodeActionAssists::Yes,
        )
    }

    #[test]
    fn test_export_fix() {
        check_nth_fix(
            0,
            r#"
//- /src/main.erl
-module(main).

main() ->
  dep:exists(),
  dep:pr~ivate().

exists() -> ok.
//- /src/dep.erl
-module(dep).
-export([exists/0]).
exists() -> ok.
private() -> ok.
"#,
            expect![[r#"
-module(dep).
-export([exists/0, private/0]).
exists() -> ok.
private() -> ok.
"#]],
            DiagnosticsConfig::default().set_experimental(true),
            &vec![],
            crate::tests::IncludeCodeActionAssists::Yes,
        )
    }
}
