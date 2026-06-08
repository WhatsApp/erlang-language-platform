/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: deprecated-function
//
// Return a warning if a function is marked as deprecated.
// This functionality is similar to the one provided by the XRef tool which comes with OTP.
// In fact, it leverages the same `-deprecated` attribute that XRef uses. The attribute
// allows the user to specify a "third" field to explain the reason of a deprecation, but
// XRef itself ignores that field, which is intended to be used by other tools.
// This diagnostic does just that and shows the message at the call-site.

use std::borrow::Cow;

use elp_ide_assists::Assist;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_ide_db::text_edit::TextSize;
use elp_syntax::AstNode;

use super::DiagnosticCode;
use super::DiagnosticTag;
use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::MatchCtx;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::fix;
use crate::lazy_function_matches;

pub(crate) struct DeprecatedFunctionLinter;

#[derive(Clone, Debug, PartialEq, Default)]
pub(crate) struct DeprecatedContext {
    function_label: String,
    deprecated_desc: Option<String>,
    target_module: String,
    target_name: String,
    target_arity: u32,
}

impl Linter for DeprecatedFunctionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::DeprecatedFunction
    }

    fn description(&self) -> &'static str {
        "Function is deprecated."
    }

    fn should_process_generated_files(&self) -> bool {
        true
    }
}

impl FunctionCallLinter for DeprecatedFunctionLinter {
    type Context = DeprecatedContext;

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![vec![FunctionMatch::any()]]
    }

    fn diagnostic_tag(&self, _ctx: &Self::Context) -> Option<DiagnosticTag> {
        Some(DiagnosticTag::Deprecated)
    }

    fn match_description(&self, ctx: &Self::Context) -> Cow<'_, str> {
        let base_message = format!("Function '{}' is deprecated.", ctx.function_label);
        match &ctx.deprecated_desc {
            Some(desc) => Cow::Owned(format!("{base_message}\n{desc}")),
            None => Cow::Owned(base_message),
        }
    }

    fn check_match(&self, ctx: &CheckCallCtx<'_, ()>) -> Option<Self::Context> {
        let arity = ctx.args.arity();
        let target_def = ctx.target.resolve_call(
            arity,
            ctx.sema,
            ctx.in_clause.file_id(),
            &ctx.in_clause.body(),
        )?;

        if !target_def.deprecated {
            return None;
        }

        let function_label = target_def.name.to_string();
        let deprecated_desc = target_def.deprecated_desc.as_ref().map(|desc| {
            let desc = desc.to_string();
            strip_quotes(&desc).to_string()
        });
        let target_module = ctx
            .sema
            .module_name(target_def.file.file_id)
            .expect("module should have a name")
            .as_str()
            .to_string();
        let target_name = target_def.name.name().to_string();
        let target_arity = target_def.name.arity();

        Some(DeprecatedContext {
            function_label,
            deprecated_desc,
            target_module,
            target_name,
            target_arity,
        })
    }

    fn fixes(
        &self,
        match_ctx: &MatchCtx<Self::Context>,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let source = match_ctx.sema.parse(ctx.file_id).value;
        let form_list = match_ctx.sema.form_list(ctx.file_id);

        let offset = if let Some(module_attr) = form_list.module_attribute() {
            let module_attr_range = module_attr.form_id.get(&source).syntax().text_range();
            module_attr_range.end() + TextSize::from(1)
        } else {
            TextSize::from(0)
        };

        let text = format!(
            "-ignore_xref([{{{}, {}, {}}}]).\n",
            match_ctx.extra.target_module,
            match_ctx.extra.target_name,
            match_ctx.extra.target_arity,
        );
        let mut edit_builder = TextEdit::builder();
        edit_builder.insert(offset, text);
        let edit = edit_builder.finish();
        let source_change = SourceChange::from_text_edit(ctx.file_id, edit);

        Some(vec![fix(
            "xref_ignore",
            "Add xref ignore for all calls to this function",
            source_change,
            match_ctx.range.range,
        )])
    }
}

pub static LINTER: DeprecatedFunctionLinter = DeprecatedFunctionLinter;

fn strip_quotes(s: &str) -> &str {
    let quote = "\"";
    let s = s.strip_prefix(quote).unwrap_or(s);
    s.strip_suffix(quote).unwrap_or(s)
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn test_deprecated_function_local() {
        check_diagnostics(
            r#"
  -module(main).
  -deprecated({not_ok_to_use, 0}).
  not_ok_to_use() ->
    ok.
  main() ->
    not_ok_to_use().
%%  ^^^^^^^^^^^^^ 💡 warning: W0016: Function 'not_ok_to_use/0' is deprecated.
            "#,
        )
    }

    #[test]
    fn test_deprecated_function_remote() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([not_ok_to_use/0]).
  -deprecated({not_ok_to_use, 0}).
  not_ok_to_use() ->
    ok.
//- /src/a.erl
  -module(a).

  main() ->
    b:not_ok_to_use().
%%  ^^^^^^^^^^^^^^^ 💡 warning: W0016: Function 'not_ok_to_use/0' is deprecated.
            "#,
        )
    }

    #[test]
    fn test_deprecated_function_in_macro() {
        check_diagnostics(
            r#"
  -module(main).
  -define(LAZY, fun(X) -> X end).
  -deprecated({do, 0}).
  main() ->
    do(),
%%  ^^ 💡 warning: W0016: Function 'do/0' is deprecated.
    ?LAZY(do()).
%%  ^^^^^^^^^^^ 💡 warning: W0016: Function 'do/0' is deprecated.
  do() ->
    ok.
            "#,
        )
    }

    #[test]
    fn test_deprecated_function_description() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([not_ok_to_use/0]).
  -deprecated({not_ok_to_use, 0, "Cause I said so."}).
  not_ok_to_use() ->
    ok.
//- /src/a.erl
  -module(a).

  main() ->
    b:not_ok_to_use().
%%  ^^^^^^^^^^^^^^^ 💡 warning: W0016: Function 'not_ok_to_use/0' is deprecated.
%%                | Cause I said so.
            "#,
        )
    }

    #[test]
    fn test_deprecated_function_no_auto_import() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  -compile({no_auto_import, [now/0]}).
  -export([now/0, later/0]).
  now() ->
    really_now.
  later() ->
    {now(), but_later}.
            "#,
        )
    }

    #[test]
    fn test_xref_ignore_fix() {
        check_fix(
            r#"
//- /src/b.erl
-module(b).
-export([not_ok_to_use/0]).
-deprecated({not_ok_to_use, 0, "Cause I said so."}).
not_ok_to_use() ->
  ok.

//- /src/a.erl
-module(a).

main() ->
  b:no~t_ok_to_use().
"#,
            expect![[r#"
-module(a).
-ignore_xref([{b, not_ok_to_use, 0}]).

main() ->
  b:not_ok_to_use().
"#]],
        )
    }
}
