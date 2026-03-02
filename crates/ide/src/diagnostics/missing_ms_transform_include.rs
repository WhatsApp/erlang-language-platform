/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: missing-ms-transform-include
//
// Return a warning if a module calls `ets:fun2ms/1` without
// `-include_lib("stdlib/include/ms_transform.hrl").`

use std::borrow::Cow;

use elp_ide_completion::IncludeFile;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_ide_db::text_edit::TextRange;
use elp_ide_db::text_edit::TextSize;
use hir::Semantic;

use crate::Assist;
use crate::codemod_helpers::FunctionMatch;
use crate::codemod_helpers::find_call_in_function;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::fix;

const MS_TRANSFORM_APP: &str = "stdlib";
const MS_TRANSFORM_INCLUDE: &str = "stdlib/include/ms_transform.hrl";

pub(crate) struct MissingMsTransformIncludeLinter;

impl Linter for MissingMsTransformIncludeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::MissingMsTransformInclude
    }

    fn description(&self) -> &'static str {
        "Missing `-include_lib(\"stdlib/include/ms_transform.hrl\").`"
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        let file_kind = sema.db.file_kind(file_id);
        file_kind.is_module()
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    include: Option<IncludeFile>,
    insert_offset: TextSize,
}

impl GenericLinter for MissingMsTransformIncludeLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let include = IncludeFile {
            include_lib: true,
            path: MS_TRANSFORM_INCLUDE.to_string(),
            app_name: MS_TRANSFORM_APP.to_string(),
        };

        // Check if the file already includes ms_transform.hrl (and if not, where it could go), since that should be quite cheap
        if let Some(insert_position) = include.insert_position_if_needed(sema, file_id) {
            // If there is no include, find ets:fun2ms/1 calls
            let mfa = FunctionMatch::mfa("ets", "fun2ms", 1);
            let mfas: Vec<(&FunctionMatch, ())> = vec![(&mfa, ())];
            let empty: Vec<(&FunctionMatch, ())> = vec![];
            let mut call_ranges: Vec<TextRange> = Vec::new();

            let insert_offset = insert_position.offset;

            sema.def_map_local(file_id)
                .get_functions()
                .for_each(|(_, def)| {
                    find_call_in_function(
                        &mut call_ranges,
                        sema,
                        def,
                        &mfas,
                        &empty,
                        &move |_ctx| Some(()),
                        &move |ctx| Some(ctx.range.range),
                    );
                });

            let res: Vec<_> = call_ranges
                .into_iter()
                .map(|range| GenericLinterMatchContext {
                    range,
                    context: Context {
                        include: Some(include.clone()),
                        insert_offset,
                    },
                })
                .collect();

            if res.is_empty() {
                return None;
            }

            Some(res)
        } else {
            None
        }
    }

    fn match_description(&self, _context: &Self::Context) -> Cow<'_, str> {
        Cow::Borrowed("Call to `ets:fun2ms/1` requires inclusion of `ms_transform.hrl`")
    }

    fn fixes(
        &self,
        context: &Self::Context,
        range: TextRange,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let edit = TextEdit::insert(
            context.insert_offset,
            context.include.as_ref()?.as_attribute(),
        );
        Some(vec![fix(
            "add_ms_transform_include",
            "Add `-include_lib(\"stdlib/include/ms_transform.hrl\").`",
            SourceChange::from_text_edit(file_id, edit),
            range,
        )])
    }
}

pub static LINTER: MissingMsTransformIncludeLinter = MissingMsTransformIncludeLinter;

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix_with_config;

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::UnusedInclude);
        check_diagnostics_with_config(config, fixture)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: expect_test::Expect) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::UnusedInclude);
        check_fix_with_config(config, fixture_before, fixture_after)
    }

    #[test]
    fn missing_ms_transform_include() {
        check_diagnostics(
            r#"
//- /src/foo.erl
-module(foo).

select_all() ->
    ets:fun2ms(fun({K, V}) -> {K, V} end).
%%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0067: Call to `ets:fun2ms/1` requires inclusion of `ms_transform.hrl`
"#,
        );
    }

    #[test]
    fn has_ms_transform_include() {
        check_diagnostics(
            r#"
//- /stdlib/include/ms_transform.hrl include_path:/include app:stdlib
    -compile({parse_transform,ms_transform}).

//- /src/foo.erl app:my_app
-module(foo).
-include_lib("stdlib/include/ms_transform.hrl").

select_all_keys() ->
    ets:fun2ms(fun({K, _V}) -> {K, true} end).
"#,
        );
    }

    #[test]
    fn no_fun2ms_calls() {
        check_diagnostics(
            r#"
//- /src/foo.erl
-module(foo).

bar() -> ok.
"#,
        );
    }

    #[test]
    fn fix_inserts_before_first_include() {
        check_fix(
            r#"
//- /include/other.hrl include_path:/include
  -define(X, 1).
//- /src/foo.erl
-module(foo).
-include("other.hrl").

select_all() ->
    ets:fun~2ms(fun({K, V}) -> {K, V} end).
"#,
            expect![[r#"
                -module(foo).
                -include("other.hrl").
                -include_lib("stdlib/include/ms_transform.hrl").

                select_all() ->
                    ets:fun2ms(fun({K, V}) -> {K, V} end).
            "#]],
        );
    }

    #[test]
    fn fix_inserts_before_first_function_when_no_includes() {
        check_fix(
            r#"
//- /src/foo.erl
-module(foo).

select_all() ->
    ets:fun~2ms(fun({K, V}) -> {K, V} end).
"#,
            expect![[r#"
                -module(foo).
                -include_lib("stdlib/include/ms_transform.hrl").

                select_all() ->
                    ets:fun2ms(fun({K, V}) -> {K, V} end).
            "#]],
        );
    }

    #[test]
    fn multiple_missing_ms_transform_include() {
        check_diagnostics(
            r#"
//- /src/foo.erl
-module(foo).

select_all() ->
    MS1 = ets:fun2ms(fun({K, _V}) -> {K, true} end),
%%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0067: Call to `ets:fun2ms/1` requires inclusion of `ms_transform.hrl`

    MS2 = ets:fun2ms(fun({K, V}) -> {ok, {K, V}} end),
%%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0067: Call to `ets:fun2ms/1` requires inclusion of `ms_transform.hrl`
    {MS1, MS2}.
"#,
        );
    }

    #[test]
    fn fix_inserts_before_first_include_with_multiple_fun2ms_calls() {
        check_fix(
            r#"
//- /include/other.hrl include_path:/include
  -define(X, 1).
//- /src/foo.erl
-module(foo).
-include("other.hrl").

select_all_specs() ->
    MS1 = ets:fun2ms(fun({K, _V}) -> {K, true} end),
    MS2 = ets:fun~2ms(fun({K, V}) -> {ok, {K, V}} end),
    {MS1, MS2}.
"#,
            expect![[r#"
                -module(foo).
                -include("other.hrl").
                -include_lib("stdlib/include/ms_transform.hrl").

                select_all_specs() ->
                    MS1 = ets:fun2ms(fun({K, _V}) -> {K, true} end),
                    MS2 = ets:fun2ms(fun({K, V}) -> {ok, {K, V}} end),
                    {MS1, MS2}.
            "#]],
        );
    }

    #[test]
    fn fix_inserts_in_alphabetical_order() {
        check_fix(
            r#"
//- /aaa/include/aaa.hrl app:aaa include_path:/aaa/include
  -define(AAA, 1).
//- /zzz/include/zzz.hrl app:zzz include_path:/zzz/include
  -define(ZZZ, 1).
//- /src/foo.erl app:my_app
-module(foo).
-include_lib("aaa/include/aaa.hrl").
-include_lib("zzz/include/zzz.hrl").

select_all() ->
    ets:fun~2ms(fun({K, V}) -> {K, V} end).
"#,
            expect![[r#"
                -module(foo).
                -include_lib("aaa/include/aaa.hrl").
                -include_lib("zzz/include/zzz.hrl").
                -include_lib("stdlib/include/ms_transform.hrl").

                select_all() ->
                    ets:fun2ms(fun({K, V}) -> {K, V} end).
            "#]],
        );
    }

    #[test]
    fn fix_inserts_before_all_includes() {
        check_fix(
            r#"
//- /zzz/include/zzz.hrl app:zzz include_path:/zzz/include
  -define(ZZZ, 1).
//- /src/foo.erl app:my_app
-module(foo).
-include_lib("zzz/include/zzz.hrl").

select_all() ->
    ets:fun~2ms(fun({K, V}) -> {K, V} end).
"#,
            expect![[r#"
                -module(foo).
                -include_lib("zzz/include/zzz.hrl").
                -include_lib("stdlib/include/ms_transform.hrl").

                select_all() ->
                    ets:fun2ms(fun({K, V}) -> {K, V} end).
            "#]],
        );
    }

    #[test]
    fn fix_inserts_honours_include_groups() {
        check_fix(
            r#"
//- /aaa/include/aaa.hrl app:aaa include_path:/aaa/include
  -define(AAA, 1).
//- /xxx/include/xxx.hrl app:xxx include_path:/xxx/include
  -define(XXX, 1).
//- /src/foo.erl app:my_app
-module(foo).
-include_lib("aaa/include/aaa.hrl").

-include_lib("xxx/include/xxx.hrl").

select_all() ->
    ets:fun~2ms(fun({K, V}) -> {K, V} end).
"#,
            expect![[r#"
                -module(foo).
                -include_lib("aaa/include/aaa.hrl").

                -include_lib("xxx/include/xxx.hrl").
                -include_lib("stdlib/include/ms_transform.hrl").

                select_all() ->
                    ets:fun2ms(fun({K, V}) -> {K, V} end).
            "#]],
        );
    }
}
