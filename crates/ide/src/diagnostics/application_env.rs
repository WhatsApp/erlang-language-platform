/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: application-env
//
// Diagnostic for unsafe usages of an applications environment.
// The originial motivation and discussion is in T107133234

use std::borrow::Cow;
use std::sync::LazyLock;

use elp_ide_db::elp_base_db::FileId;
use hir::AnyExprId;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFunctionClauseBody;
use hir::Semantic;

use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
// @fb-only: use crate::diagnostics;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;

pub(crate) struct ApplicationEnvLinter;

impl Linter for ApplicationEnvLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::ApplicationGetEnv
    }

    fn description(&self) -> &'static str {
        "Unsafe usage of an application's environment"
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::Warning
    }

    fn should_process_generated_files(&self) -> bool {
        true
    }

    fn should_process_test_files(&self) -> bool {
        false
    }
}

impl FunctionCallLinter for ApplicationEnvLinter {
    type Context = String;

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(context.clone())
    }

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        all_function_matches()
    }

    fn check_match(&self, context: &CheckCallCtx<'_, ()>) -> Option<Self::Context> {
        let action = find_action(context.mfa)?;
        let def = context.in_clause.value;
        match &action {
            BadEnvCallAction::AppArg(arg_index) => {
                let arg = context.args.get(*arg_index)?;
                check_valid_application(context.sema, context.in_clause, &arg, def)
            }
            BadEnvCallAction::OptionsArg { arg_index, tag } => {
                let arg = context.args.get(*arg_index)?;
                match &context.in_clause[arg] {
                    hir::Expr::List { exprs, tail: _ } => {
                        exprs
                            .iter()
                            .find_map(|expr| match &context.in_clause[*expr] {
                                hir::Expr::Tuple { exprs } => {
                                    let key = exprs.first()?;
                                    let val = exprs.get(1)?;
                                    let key_name = context.in_clause.as_atom_name(key)?;
                                    if tag == key_name.as_str() {
                                        check_tuple(context.in_clause, val, context.sema, def)
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            })
                    }
                    _ => None,
                }
            }
            BadEnvCallAction::MapArg { arg_index, keys } => {
                let arg = context.args.get(*arg_index)?;
                match &context.in_clause[arg] {
                    hir::Expr::Map { fields: _ } => {
                        if let Some(AnyExprId::Expr(rhs)) = context
                            .in_clause
                            .body()
                            .lookup_map_path(AnyExprId::Expr(arg), keys)
                        {
                            check_tuple(context.in_clause, &rhs, context.sema, def)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
        }
    }
}

pub static LINTER: ApplicationEnvLinter = ApplicationEnvLinter;

#[derive(Debug, Clone)]
pub struct BadEnvCall {
    mfa: FunctionMatch,
    action: BadEnvCallAction,
}

impl BadEnvCall {
    pub(crate) fn new(
        m: &str,
        f: &str,
        arity: Vec<u32>,
        action: BadEnvCallAction,
    ) -> Vec<BadEnvCall> {
        arity
            .into_iter()
            .map(|a| BadEnvCall {
                mfa: FunctionMatch::mfa(m, f, a),
                action: action.clone(),
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
#[allow(clippy::enum_variant_names)]
pub(crate) enum BadEnvCallAction {
    AppArg(usize), // Zero-based argument index
    /// Matches a config argument of the form {tag, {Arg0, Arg1, ...}}
    /// The `tag` must match, and we check the `index`th part of the
    /// second tuple element
    /// e.g. `our_mod:a_fun(Context, Location, [..., {cfg, {Application, Flag}}, ...], Format, Args)
    #[allow(dead_code)]
    OptionsArg {
        /// Which argument contains the list of options to be checked
        arg_index: usize,
        /// The option tag we are looking for
        tag: String,
    },
    #[allow(dead_code)] // @oss-only
    MapArg {
        /// Which argument contains the map of options to be checked
        arg_index: usize,
        /// The sequence of map keys we are looking for. we chain
        /// through, expecting each to return a map that we check the
        /// next in
        keys: Vec<String>,
    },
}

static BAD_MATCHES: LazyLock<Vec<BadEnvCall>> = LazyLock::new(|| {
    vec![
        BadEnvCall::new(
            "application",
            "get_env",
            vec![2, 3],
            BadEnvCallAction::AppArg(0),
        ),
        // @fb-only: diagnostics::meta_only::application_env_bad_matches(),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
});

fn all_function_matches() -> Vec<FunctionMatch> {
    BAD_MATCHES.iter().map(|b| b.mfa.clone()).collect()
}

fn find_action(mfa: &FunctionMatch) -> Option<&'static BadEnvCallAction> {
    BAD_MATCHES
        .iter()
        .find(|b| &b.mfa == mfa)
        .map(|b| &b.action)
}

fn check_tuple(
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    val: &ExprId,
    sema: &Semantic,
    def: &FunctionDef,
) -> Option<String> {
    if let hir::Expr::Tuple { exprs } = &in_clause[*val] {
        let app = exprs.first()?;
        check_valid_application(sema, in_clause, app, def)
    } else {
        None
    }
}

fn check_valid_application(
    sema: &Semantic,
    def_fb: &hir::InFunctionClauseBody<&FunctionDef>,
    arg: &ExprId,
    def: &FunctionDef,
) -> Option<String> {
    let arg_name = def_fb.as_atom_name(arg)?;
    let form_list = sema.form_list(def.file.file_id);
    // We need the app from the calling function location.
    let app = sema.db.file_app_name(def_fb.file_id())?;

    if arg_name.as_str() == app.as_str() {
        None
    } else {
        let module_attribute = form_list.module_attribute()?;
        let module = module_attribute.name.clone();
        Some(format!(
            "module `{module}` belongs to app `{app}`, but reads env for `{arg_name}`"
        ))
    }
}

#[cfg(test)]
mod tests {
    use elp_ide_db::DiagnosticCode;

    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    #[track_caller]
    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().disable(DiagnosticCode::NoNoWarnSuppressions);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn get_env_basic() {
        check_diagnostics(
            r#"
            //- /my_app/src/main.erl app:my_app
            -module(main).

            get_mine() ->
                application:get_env(misc, key).
            %%  ^^^^^^^^^^^^^^^^^^^ 💡 warning: W0011: module `main` belongs to app `my_app`, but reads env for `misc`

            get_mine3() ->
                application:get_env(misc, key, def).
            %%  ^^^^^^^^^^^^^^^^^^^ 💡 warning: W0011: module `main` belongs to app `my_app`, but reads env for `misc`

            //- /my_app/src/application.erl
            -module(application).
            -export([get_env/2, get_env/3]).
            get_env(App,Key) -> {App,Key}.
            get_env(App,Key,Def) -> {App,Key,Def}.
            "#,
        )
    }

    #[test]
    fn get_env_2() {
        // TODO: the Scala version also reports for the header file
        // (https://fburl.com/code/ohea18zy), but that was introduced
        // by D37262963 when tweaking the Glean indexing.  We
        // currently do not have any functions defined in header files
        // for WASERVER
        check_diagnostics(
            r#"
            //- /misc/src/app_env.erl app:misc
            -module(app_env).

            -compile([export_all, nowarn_export_all]).

            get_key_dynamic(App) ->
                application:get_env(App, key).

            get_mine() ->
                application:get_env(misc, key).

            steal() ->
                application:get_env(debug, key).
            %%  ^^^^^^^^^^^^^^^^^^^ 💡 warning: W0011: module `app_env` belongs to app `misc`, but reads env for `debug`

            //- /misc/src/application.erl app:misc
            -module(application).
            -export([get_env/2, get_env/3]).
            get_env(App,Key) -> {App,Key}.
            get_env(App,Key,Def) -> {App,Key,Def}.
            "#,
        )
    }

    #[test]
    fn get_env_macro() {
        check_diagnostics(
            r#"
            //- /my_app/src/main.erl app:my_app
            -module(main).

            -include_lib("my_app/include/my_header.hrl").

            get_mine() ->
                ?get(misc, key).
            %%  ^^^^^^^^^^^^^^^ 💡 warning: W0011: module `main` belongs to app `my_app`, but reads env for `misc`

            //- /my_app/include/my_header.hrl app:my_app
            -define(get(K,V), application:get_env(K,V)).

            //- /my_app/src/application.erl app:my_app
            -module(application).
            -export([get_env/2]).
            get_env(App,Key) -> {App,Key}.

            "#,
        )
    }
}
