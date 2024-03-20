/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Diagnostic: application-env
//
// Diagnostic for unsafe usages of an applications environment.
// The originial motivation and discussion is in T107133234

use elp_ide_db::elp_base_db::FileId;
use hir::AnyExprId;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFunctionClauseBody;
use hir::Semantic;
use lazy_static::lazy_static;

use super::Diagnostic;
use crate::codemod_helpers::find_call_in_function;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::FunctionMatch;
use crate::codemod_helpers::MakeDiagCtx;
// @fb-only: use crate::diagnostics;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;

pub(crate) fn application_env(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    sema.def_map(file_id)
        .get_functions()
        .for_each(|(_, def)| check_function(diags, sema, def));
}

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

pub(crate) fn check_function(diags: &mut Vec<Diagnostic>, sema: &Semantic, def: &FunctionDef) {
    lazy_static! {
        static ref BAD_MATCHES: Vec<BadEnvCall> = vec![BadEnvCall::new(
            "application",
            "get_env",
            vec![2, 3],
            BadEnvCallAction::AppArg(0)),
            // @fb-only: diagnostics::meta_only::application_env_bad_matches(),
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    }

    process_badmatches(diags, sema, def, &BAD_MATCHES);
}

pub(crate) fn process_badmatches(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    bad: &[BadEnvCall],
) {
    let mfas = bad.iter().map(|b| (&b.mfa, &b.action)).collect::<Vec<_>>();
    find_call_in_function(
        diags,
        sema,
        def,
        &mfas,
        &move |CheckCallCtx {
                   t, args, in_clause, ..
               }: CheckCallCtx<'_, &BadEnvCallAction>| match t {
            BadEnvCallAction::AppArg(arg_index) => {
                let arg = args.get(*arg_index)?;
                check_valid_application(sema, in_clause, arg, def)
            }
            BadEnvCallAction::OptionsArg { arg_index, tag } => {
                let arg = args.get(*arg_index)?;
                match &in_clause[*arg] {
                    hir::Expr::List { exprs, tail: _ } => {
                        exprs.iter().find_map(|expr| match &in_clause[*expr] {
                            hir::Expr::Tuple { exprs } => {
                                let key = exprs.get(0)?;
                                let val = exprs.get(1)?;
                                let key_name = in_clause.as_atom_name(key)?;
                                if tag == key_name.as_str() {
                                    check_tuple(in_clause, val, sema, def)
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
                // We expect the `arg_index`'th argument to contain a
                // map.  We chase through the keys, expecting each to
                // return another map, which we look up the next key
                // in. At the end we expect a tuple, and check the
                // first arg of that.
                let arg = args.get(*arg_index)?;
                match &in_clause[*arg] {
                    hir::Expr::Map { fields: _ } => {
                        if let Some(AnyExprId::Expr(rhs)) = in_clause.body().lookup_map_path(
                            sema.db.upcast(),
                            AnyExprId::Expr(*arg),
                            keys,
                        ) {
                            check_tuple(in_clause, &rhs, sema, def)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
        },
        &move |MakeDiagCtx {
                   sema,
                   def_fb,
                   extra,
                   range,
                   ..
               }: MakeDiagCtx<'_, String>|
              -> Option<Diagnostic> {
            let diag = Diagnostic::new(DiagnosticCode::ApplicationGetEnv, extra, range)
                .with_severity(Severity::Warning)
                .with_ignore_fix(sema, def_fb.file_id());
            Some(diag)
        },
    );
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
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    #[track_caller]
    pub(crate) fn check_diagnostics(ra_fixture: &str) {
        let mut config = DiagnosticsConfig::default();
        config
            .disabled
            .insert(DiagnosticCode::MissingCompileWarnMissingSpec);
        check_diagnostics_with_config(config, ra_fixture)
    }

    #[test]
    fn get_env_basic() {
        check_diagnostics(
            r#"
            //- /my_app/src/main.erl app:my_app
            -module(main).

            get_mine() ->
                application:get_env(misc, key).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: module `main` belongs to app `my_app`, but reads env for `misc`

            get_mine3() ->
                application:get_env(misc, key, def).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: module `main` belongs to app `my_app`, but reads env for `misc`

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

            -include_lib("misc/include/my_header.hrl").

            get_key_dynamic(App) ->
                application:get_env(App, key).

            get_mine() ->
                application:get_env(misc, key).

            steal() ->
                application:get_env(debug, key).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: module `app_env` belongs to app `misc`, but reads env for `debug`

            //- /misc/src/application.erl app:misc
            -module(application).
            -export([get_env/2, get_env/3]).
            get_env(App,Key) -> {App,Key}.
            get_env(App,Key,Def) -> {App,Key,Def}.

            //- /misc/include/my_header.hrl
            get_check_key() ->
                application:get_env(check, key).
            "#,
        )
    }

    #[test]
    fn get_env_macro() {
        check_diagnostics(
            r#"
            //- /my_app/src/main.erl app:my_app
            -module(main).

            -include("my_app/include/my_header.hrl").

            get_mine() ->
                ?get(misc, key).
            %%  ^^^^^^^^^^^^^^^ 💡 warning: module `main` belongs to app `my_app`, but reads env for `misc`

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
