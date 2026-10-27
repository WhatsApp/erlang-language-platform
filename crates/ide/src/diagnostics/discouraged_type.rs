/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: discouraged_type
//!
//! Return a diagnostic for every type position (`-spec`, `-type`, `-opaque`,
//! `-nominal`, `-callback` and `-record` field types) holding one of the types
//! listed in the `[discouraged_type]` section of `.elp_lint.toml`:
//!
//! ```toml
//! [discouraged_type]
//! types = ["dynamic()", "map()", "#{_ => _}", "{error, term()}", "term()"]
//! ```
//!
//! Types are compared structurally after lowering, so spacing, parentheses
//! and an explicit `erlang:` qualifier on a built-in type do not matter, and
//! a type reaching a type position through a macro is reported on the macro
//! call. There is no notion of subtyping: listing `map()` does not report
//! `#{atom() => binary()}`. When matches nest, only the outermost is reported, so listing both
//! `{error, term()}` and `term()` reports the `term()` inside `{error, term()}`
//! once, as the former. The linter is a no-op unless `types` is configured.

use std::borrow::Cow;
use std::sync::Arc;

use elp_ide_db::RootDatabase;
use elp_ide_db::elp_base_db::FileRange;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::Body;
use hir::Callback;
use hir::InFile;
use hir::Record;
use hir::Semantic;
use hir::Spec;
use hir::Strategy;
use hir::TypeAlias;
use hir::TypeBody;
use hir::TypeExpr;
use hir::TypeExprId;
use hir::db::InternDatabase;
use hir::fold::AnyCallBackCtx;
use hir::fold::Fold;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use serde::Deserialize;
use serde::Serialize;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

const STRATEGY: Strategy = Strategy {
    macros: MacroStrategy::ExpandButIncludeMacroCall,
    parens: ParenStrategy::InvisibleParens,
};

/// Configuration for the `discouraged_type` (W0087) linter, read from the
/// `[discouraged_type]` section of `.elp_lint.toml`.
#[derive(Deserialize, Serialize, Default, Debug, Clone)]
#[serde(
    try_from = "RawDiscouragedTypeConfig",
    into = "RawDiscouragedTypeConfig"
)]
pub struct DiscouragedTypeConfig {
    types: Vec<TypePattern>,
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct RawDiscouragedTypeConfig {
    #[serde(default)]
    types: Vec<String>,
}

/// A configured type, as written and in the canonical form it is compared in.
#[derive(Debug, Clone, PartialEq, Eq)]
struct TypePattern {
    text: String,
    canonical: String,
}

impl DiscouragedTypeConfig {
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }
}

impl TryFrom<RawDiscouragedTypeConfig> for DiscouragedTypeConfig {
    type Error = String;

    fn try_from(raw: RawDiscouragedTypeConfig) -> Result<Self, Self::Error> {
        let db = RootDatabase::default();
        let types = raw
            .types
            .into_iter()
            .map(|text| {
                let body = TypeBody::lower_standalone(&db, &text)
                    .ok_or_else(|| format!("invalid type '{text}' in [discouraged_type]"))?;
                let canonical = canonical(&db, &body.body, body.ty);
                Ok(TypePattern { text, canonical })
            })
            .collect::<Result<_, String>>()?;
        Ok(DiscouragedTypeConfig { types })
    }
}

impl From<DiscouragedTypeConfig> for RawDiscouragedTypeConfig {
    fn from(config: DiscouragedTypeConfig) -> Self {
        RawDiscouragedTypeConfig {
            types: config.types.into_iter().map(|t| t.text).collect(),
        }
    }
}

fn canonical(db: &dyn InternDatabase, body: &Body, ty: TypeExprId) -> String {
    body.print_any_expr(db, AnyExprId::TypeExpr(ty))
        .trim()
        .to_string()
}

pub(crate) struct DiscouragedTypeLinter;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Context {
    text: String,
}

impl Linter for DiscouragedTypeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::DiscouragedType
    }

    fn description(&self) -> &'static str {
        "Type is discouraged."
    }
}

impl GenericLinter for DiscouragedTypeLinter {
    type Context = Context;

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!("`{}` is a discouraged type.", context.text))
    }

    fn matches(
        &self,
        ctx: &LinterContext,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let config = &ctx.lint_config()?.discouraged_type;
        if config.is_empty() {
            return None;
        }

        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let form_list = sema.form_list(file_id);
        let mut res = Vec::new();

        for (spec_id, _) in form_list.specs() {
            let spec_id = InFile::new(file_id, spec_id);
            let body = sema.db.spec_body(spec_id).body.clone();
            Spec::fold(sema, STRATEGY, spec_id, (), &mut |_acc, ctx| {
                check_type(&mut res, sema, config, &ctx, &body);
            });
        }
        for (type_alias_id, _) in form_list.type_aliases() {
            let type_alias_id = InFile::new(file_id, type_alias_id);
            let body = sema.db.type_body(type_alias_id).body.clone();
            TypeAlias::fold(sema, STRATEGY, type_alias_id, (), &mut |_acc, ctx| {
                check_type(&mut res, sema, config, &ctx, &body);
            });
        }
        for (callback_id, _) in form_list.callback_attributes() {
            let callback_id = InFile::new(file_id, callback_id);
            let body = sema.db.callback_body(callback_id).body.clone();
            Callback::fold(sema, STRATEGY, callback_id, (), &mut |_acc, ctx| {
                check_type(&mut res, sema, config, &ctx, &body);
            });
        }
        for (record_id, _) in form_list.records() {
            let record_id = InFile::new(file_id, record_id);
            let body = sema.db.record_body(record_id).body.clone();
            Record::fold(sema, STRATEGY, record_id, (), &mut |_acc, ctx| {
                check_type(&mut res, sema, config, &ctx, &body);
            });
        }

        Some(res)
    }
}

/// Folds are pre-order, so an enclosing match is always recorded before the
/// ones it contains, which are then dropped. A macro call holding several
/// matches reports once, since they all share its range.
fn check_type(
    matches: &mut Vec<GenericLinterMatchContext<Context>>,
    sema: &Semantic,
    config: &DiscouragedTypeConfig,
    ctx: &AnyCallBackCtx<'_>,
    body: &Arc<Body>,
) -> Option<()> {
    let AnyExpr::TypeExpr(ty) = &ctx.item else {
        return None;
    };
    let AnyExprId::TypeExpr(ty_id) = ctx.item_id else {
        return None;
    };
    if matches!(ty, TypeExpr::MacroCall { .. }) {
        // Its expansion is visited next, as part of the macro.
        return None;
    }
    let canonical = canonical(sema.db, body, ty_id);
    let pattern = config.types.iter().find(|t| t.canonical == canonical)?;

    let range = match_range(sema, ctx, body, ty_id)?;
    if matches
        .iter()
        .any(|m| m.range.file_id == range.file_id && m.range.range.contains_range(range.range))
    {
        return None;
    }
    matches.push(GenericLinterMatchContext {
        range,
        context: Context {
            text: pattern.text.clone(),
        },
    });
    Some(())
}

/// A type written at a macro call site, as a macro argument, is reported in
/// place; one coming from the macro body is reported on the macro call.
fn match_range(
    sema: &Semantic,
    ctx: &AnyCallBackCtx<'_>,
    body: &Body,
    ty_id: TypeExprId,
) -> Option<FileRange> {
    let range = body.range_for_any(sema, AnyExprId::TypeExpr(ty_id));
    let Some((macro_call, _)) = ctx.in_macro else {
        return range;
    };
    let macro_range = body.range_for_any(sema, macro_call.idx)?;
    match range {
        Some(range)
            if range.file_id == macro_range.file_id
                && macro_range.range.contains_range(range.range) =>
        {
            Some(range)
        }
        _ => Some(macro_range),
    }
}

pub static LINTER: DiscouragedTypeLinter = DiscouragedTypeLinter;

#[cfg(test)]
mod tests {
    use elp_ide_db::DiagnosticCode;

    use super::DiscouragedTypeConfig;
    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::diagnostics::LintConfig;
    use crate::tests::check_filtered_diagnostics_with_config;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::DiscouragedType
    }

    fn config_with(types: &[&str]) -> DiagnosticsConfig {
        let section = format!(
            "[discouraged_type]\ntypes = [{}]\n",
            types
                .iter()
                .map(|t| format!("'{t}'"))
                .collect::<Vec<_>>()
                .join(", ")
        );
        let lint_config: LintConfig =
            toml::from_str(&section).expect("test config should be valid");
        DiagnosticsConfig {
            lint_config: Some(lint_config),
            ..Default::default()
        }
    }

    #[track_caller]
    fn check_diagnostics_with(types: &[&str], fixture: &str) {
        check_filtered_diagnostics_with_config(config_with(types), &vec![], fixture, &filter)
    }

    const TYPES: &[&str] = &[
        "dynamic()",
        "map()",
        "#{_ => _}",
        "#{_ := _}",
        "list()",
        "tuple()",
        "fun()",
        "{error, term()}",
        "term()",
    ];

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        check_diagnostics_with(TYPES, fixture)
    }

    #[test]
    fn reports_configured_types_in_type_positions() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-export([test/1]).
-type t() :: dynamic().
%%           ^^^^^^^^^ warning: W0087: `dynamic()` is a discouraged type.
%%                   | 💡 <suppression>
-record(r, {f :: map()}).
%%               ^^^^^ warning: W0087: `map()` is a discouraged type.
%%                   | 💡 <suppression>
-callback cb(list()) -> tuple().
%%           ^^^^^^ warning: W0087: `list()` is a discouraged type.
%%                | 💡 <suppression>
%%                      ^^^^^^^ warning: W0087: `tuple()` is a discouraged type.
%%                            | 💡 <suppression>
-spec test(fun()) -> #{_ => _}.
%%         ^^^^^ warning: W0087: `fun()` is a discouraged type.
%%             | 💡 <suppression>
%%                   ^^^^^^^^^ warning: W0087: `#{_ => _}` is a discouraged type.
%%                           | 💡 <suppression>
test(F) -> F().
"#,
        )
    }

    #[test]
    fn outermost_match_wins() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-export([test/0]).
-spec test() -> ok | {error, term()}.
%%                   ^^^^^^^^^^^^^^^ warning: W0087: `{error, term()}` is a discouraged type.
%%                                 | 💡 <suppression>
test() -> ok.
"#,
        )
    }

    #[test]
    fn matches_structurally() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-type a() :: erlang:dynamic().
%%           ^^^^^^^^^^^^^^^^ warning: W0087: `dynamic()` is a discouraged type.
%%                          | 💡 <suppression>
-type b() :: {  error ,(term())  }.
%%           ^^^^^^^^^^^^^^^^^^^^^ warning: W0087: `{error, term()}` is a discouraged type.
%%                               | 💡 <suppression>
"#,
        )
    }

    #[test]
    fn not_reported() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-export([test/1, dynamic/0]).
%% Comments, strings, expressions and other types are not type positions
%% holding a configured type: dynamic(), map().
-type k() :: #{atom() => binary()}.
-type v(K, V) :: #{K => V}.
-type f() :: fun((atom()) -> ok).
-type my_map() :: other:map().
-spec test(k()) -> {ok, binary()}.
test(_) -> {ok, <<"dynamic()">>}.
dynamic() -> map.
"#,
        )
    }

    #[test]
    fn via_macro() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("defs.hrl").
-define(WRAP(X), X).
-spec test(?T) -> ?WRAP(map()).
%%         ^^ warning: W0087: `dynamic()` is a discouraged type.
%%          | 💡 <suppression>
%%                ^^^^^^^^^^^^ warning: W0087: `map()` is a discouraged type.
%%                           | 💡 <suppression>
test(X) -> X.
//- /src/defs.hrl
-define(T, dynamic()).
"#,
        )
    }

    #[test]
    fn in_header() {
        check_diagnostics(
            r#"
//- /src/main.hrl
-record(r, {f :: dynamic()}).
%%               ^^^^^^^^^ warning: W0087: `dynamic()` is a discouraged type.
%%                       | 💡 <suppression>
"#,
        )
    }

    #[test]
    fn no_op_without_config() {
        check_diagnostics_with(
            &[],
            r#"
//- /src/main.erl
-module(main).
-type t() :: dynamic().
"#,
        )
    }

    #[test]
    fn rejects_invalid_types() {
        for (case, types) in [
            ("not a type", "types = ['{error,']"),
            ("macro", "types = ['?T']"),
            ("two forms", "types = ['a(). -type b() :: c()']"),
        ] {
            let err = toml::from_str::<DiscouragedTypeConfig>(types)
                .expect_err(case)
                .to_string();
            assert!(
                err.contains("invalid type"),
                "{case}: expected an invalid type error, got: {err}"
            );
        }
    }

    #[test]
    fn round_trips() {
        let config: DiscouragedTypeConfig =
            toml::from_str("types = ['dynamic()', '#{_ => _}']").unwrap();
        expect_test::expect![[r##"
            types = ["dynamic()", "#{_ => _}"]
        "##]]
        .assert_eq(&toml::to_string(&config).unwrap());
    }
}
