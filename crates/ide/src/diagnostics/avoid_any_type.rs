/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint/fix: avoid_any_type
//!
//! Return a diagnostic for uses of the `any()` type, in any type position
//! (`-spec`, `-type`, `-opaque`, `-callback` and `-record` field types).
//! `term()` is the preferred spelling for the same type.
//!
//! An `any()` reaching a type position through a macro is reported once per
//! macro call site, following the same convention as the function call
//! linters (see `find_call_in_function_with_matchers`).

use elp_ide_assists::Assist;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_ide_db::text_edit::TextRange;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::Body;
use hir::CallTarget;
use hir::Callback;
use hir::InFile;
use hir::Record;
use hir::Semantic;
use hir::Spec;
use hir::Strategy;
use hir::TypeAlias;
use hir::TypeExpr;
use hir::fold::AnyCallBackCtx;
use hir::fold::Fold;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::known;

use crate::FxHashSet;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::fix;

const STRATEGY: Strategy = Strategy {
    macros: MacroStrategy::ExpandButIncludeMacroCall,
    parens: ParenStrategy::InvisibleParens,
};

#[derive(Debug, Clone)]
pub enum Context {
    /// The `any()` is spelled out in the type position.
    Direct,
    /// The `any()` came from a macro expansion, so the diagnostic is anchored
    /// on the macro call. No fix is offered: rewriting the call site would
    /// delete the macro use, and the macro body is not known to be used only
    /// in type positions, so rewriting the definition can break other uses.
    ViaMacro,
}

pub(crate) struct AvoidAnyTypeLinter;

impl Linter for AvoidAnyTypeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::AvoidAnyType
    }

    fn description(&self) -> &'static str {
        "Prefer `term()` over `any()`."
    }

    fn is_enabled(&self) -> bool {
        false
    }
}

impl GenericLinter for AvoidAnyTypeLinter {
    type Context = Context;

    fn matches(
        &self,
        ctx: &LinterContext,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let form_list = sema.form_list(file_id);
        let mut res = Vec::new();

        for (spec_id, _spec) in form_list.specs() {
            let spec_id = InFile::new(file_id, spec_id);
            let spec_body = sema.db.spec_body(spec_id);
            Spec::fold(sema, STRATEGY, spec_id, (), &mut |_acc, ctx| {
                check_any_type(&mut res, sema, &ctx, &spec_body.body);
            });
        }

        for (type_alias_id, _type_alias) in form_list.type_aliases() {
            let type_alias_id = InFile::new(file_id, type_alias_id);
            let type_body = sema.db.type_body(type_alias_id);
            TypeAlias::fold(sema, STRATEGY, type_alias_id, (), &mut |_acc, ctx| {
                check_any_type(&mut res, sema, &ctx, &type_body.body);
            });
        }

        for (callback_id, _callback) in form_list.callback_attributes() {
            let callback_id = InFile::new(file_id, callback_id);
            let callback_body = sema.db.callback_body(callback_id);
            Callback::fold(sema, STRATEGY, callback_id, (), &mut |_acc, ctx| {
                check_any_type(&mut res, sema, &ctx, &callback_body.body);
            });
        }

        for (record_id, _record) in form_list.records() {
            let record_id = InFile::new(file_id, record_id);
            let record_body = sema.db.record_body(record_id);
            Record::fold(sema, STRATEGY, record_id, (), &mut |_acc, ctx| {
                check_any_type(&mut res, sema, &ctx, &record_body.body);
            });
        }

        // A single macro call reports once, even if the macro body mentions
        // `any()` more than once: the diagnostics would be indistinguishable,
        // and their suppression fixes are insertions, which `TextEdit::union`
        // does not dedup.
        let mut seen = FxHashSet::default();
        res.retain(|matched| seen.insert(matched.range));

        Some(res)
    }

    fn fixes(
        &self,
        context: &Self::Context,
        range: TextRange,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        if matches!(context, Context::ViaMacro) {
            return None;
        }
        let mut builder = TextEdit::builder();
        builder.replace(range, "term".to_string());
        Some(vec![fix(
            "replace_any_with_term",
            "Replace `any()` with `term()`",
            SourceChange::from_text_edit(ctx.file_id, builder.finish()),
            range,
        )])
    }
}

/// Report the `any` name only, not the whole call target. For the bare
/// `any()` spelling the two coincide, since HIR lowers built-in types to
/// remote calls on a synthetic `erlang` module with no source range. For an
/// explicit `erlang:any()` it keeps the fix to `erlang:term()`.
fn check_any_type(
    matches: &mut Vec<GenericLinterMatchContext<Context>>,
    sema: &Semantic,
    ctx: &AnyCallBackCtx<'_>,
    body: &Body,
) -> Option<()> {
    let AnyExpr::TypeExpr(TypeExpr::Call { target, args }) = &ctx.item else {
        return None;
    };
    if !args.is_empty() {
        return None;
    }
    let CallTarget::Remote { module, name, .. } = target else {
        return None;
    };
    if body[*module].as_atom()?.as_name() != *known::erlang
        || body[*name].as_atom()?.as_name() != *known::any
    {
        return None;
    }
    let name_range = body.range_for_any(sema, AnyExprId::TypeExpr(*name));
    let (range, context) = match ctx.in_macro {
        Some((macro_call, _)) => {
            let macro_range = body.range_for_any(sema, macro_call.idx)?;
            // An `any()` passed as a macro *argument* is written at the call
            // site, so it is reported and fixed in place. Only one coming from
            // the macro body is anchored on the call.
            match name_range {
                Some(name_range)
                    if name_range.file_id == macro_range.file_id
                        && macro_range.range.contains_range(name_range.range) =>
                {
                    (name_range, Context::Direct)
                }
                _ => (macro_range, Context::ViaMacro),
            }
        }
        None => (name_range?, Context::Direct),
    };
    matches.push(GenericLinterMatchContext { range, context });
    Some(())
}

pub static LINTER: AvoidAnyTypeLinter = AvoidAnyTypeLinter;

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;
    use expect_test::Expect;
    use expect_test::expect;

    use crate::DiagnosticsConfig;
    use crate::tests;

    fn config() -> DiagnosticsConfig {
        DiagnosticsConfig::default().enable(DiagnosticCode::AvoidAnyType)
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_diagnostics_with_config(config(), fixture);
    }

    #[track_caller]
    fn check_fix(before: &str, after: Expect) {
        tests::check_fix_with_config(config(), before, after);
    }

    #[test]
    fn avoid_any_type_in_spec() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([test/1]).

            -spec test(any()) -> any().
            %%         ^^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            %%                   ^^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            test(X) -> X.
            "#,
        )
    }

    #[test]
    fn avoid_any_type_in_type_alias_and_record() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).

            -type my_type() :: any().
            %%                 ^^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            -opaque my_opaque() :: {ok, any()} | error.
            %%                          ^^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            -record(my_record, {field :: any()}).
            %%                           ^^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            "#,
        )
    }

    #[test]
    fn avoid_any_type_in_callback() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).

            -callback handle(any()) -> ok.
            %%               ^^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            "#,
        )
    }

    #[test]
    fn avoid_any_type_in_header() {
        check_diagnostics(
            r#"
            //- /src/main.hrl
            -record(my_record, {field :: any()}).
            %%                           ^^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            "#,
        )
    }

    #[test]
    fn avoid_any_type_not_reported() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([test/1, any/0]).

            %% `term()` is the preferred spelling, `any/1` is a different type,
            %% and a function or atom called `any` is not a type at all.
            -spec test(term()) -> {atom(), ok}.
            test(_) -> {any, any()}.

            any() -> ok.
            "#,
        )
    }

    #[test]
    fn avoid_any_type_via_macro_reported_per_use() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -define(T, any()).
            -spec test(?T) -> ?T.
            %%         ^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            %%                ^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            test(X) -> X.
            "#,
        )
    }

    #[test]
    fn avoid_any_type_via_macro_from_header() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -include("defs.hrl").
            -spec test(?T) -> ?T.
            %%         ^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            %%                ^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            test(X) -> X.
            //- /src/defs.hrl
            -define(T, any()).
            "#,
        )
    }

    #[test]
    fn avoid_any_type_via_macro_reported_once_per_call() {
        // The macro body mentions `any()` twice, but a call site is a single
        // range, so it is reported once.
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -define(PAIR, {any(), any()}).
            -spec test() -> ?PAIR.
            %%              ^^^^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            test() -> {ok, ok}.
            "#,
        )
    }

    #[test]
    fn avoid_any_type_in_macro_argument_still_reported() {
        // Written by the user at the call site, so it keeps its fix.
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -define(WRAP(X), X).
            -spec test(?WRAP(any())) -> ok.
            %%               ^^^ 💡 warning: W0082: Prefer `term()` over `any()`.
            test(_) -> ok.
            "#,
        )
    }

    #[test]
    fn fix_replaces_any_with_term() {
        check_fix(
            r#"
//- /src/main.erl
-module(main).
-export([test/1]).

-spec test(an~y()) -> ok.
test(_) -> ok.
"#,
            expect![[r#"
-module(main).
-export([test/1]).

-spec test(term()) -> ok.
test(_) -> ok.
"#]],
        )
    }
}
