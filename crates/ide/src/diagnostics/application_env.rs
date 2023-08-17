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
use hir::ExprId;
use hir::FunctionDef;
use hir::Semantic;

use super::Diagnostic;
use crate::codemod_helpers::find_call_in_function;
use crate::codemod_helpers::FunctionMatch;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;

pub(crate) fn application_env(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    sema.def_map(file_id)
        .get_functions()
        .iter()
        .for_each(|(_arity, def)| check_function(diags, sema, def));
}

#[derive(Debug, Clone)]
pub(crate) struct BadEnvCall {
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
    // @oss-only #[allow(dead_code)]
    OptionsArg {
        /// Which argument contains the list of options to be checked
        arg_index: usize,
        /// The option tag we are looking for
        tag: String,
    },
}

pub(crate) fn check_function(diags: &mut Vec<Diagnostic>, sema: &Semantic, def: &FunctionDef) {
    let bad_matches = vec![BadEnvCall::new(
        "application",
        "get_env",
        vec![2, 3],
        BadEnvCallAction::AppArg(0),
    )]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>();

    process_badmatches(diags, sema, def, &bad_matches);
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
        &move |_mfa, action, _target, args, def_fb| match action {
            BadEnvCallAction::AppArg(arg_index) => {
                let arg = args.get(*arg_index)?;
                check_valid_application(sema, def_fb, arg, def)
            }
            BadEnvCallAction::OptionsArg { arg_index, tag } => {
                let arg = args.get(*arg_index)?;
                match &def_fb[*arg] {
                    hir::Expr::List { exprs, tail: _ } => {
                        exprs.iter().find_map(|expr| match &def_fb[*expr] {
                            hir::Expr::Tuple { exprs } => {
                                let key = exprs.get(0)?;
                                let val = exprs.get(1)?;
                                let key_name = def_fb.as_atom_name(sema.db, key)?;
                                if tag == key_name.as_str() {
                                    if let hir::Expr::Tuple { exprs } = &def_fb[*val] {
                                        let app = exprs.get(0)?;
                                        check_valid_application(sema, def_fb, app, def)
                                    } else {
                                        None
                                    }
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
        },
        move |_sema, mut _def_fb, _target, _call_id, extra_info, range| {
            let diag = Diagnostic::new(DiagnosticCode::ApplicationGetEnv, extra_info, range)
                .severity(Severity::Warning);
            Some(diag)
        },
    );
}

fn check_valid_application(
    sema: &Semantic,
    def_fb: &hir::InFunctionBody<&FunctionDef>,
    arg: &ExprId,
    def: &FunctionDef,
) -> Option<String> {
    let arg_name = def_fb.as_atom_name(sema.db, arg)?;
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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: module `main` belongs to app `my_app`, but reads env for `misc`

            get_mine3() ->
                application:get_env(misc, key, def).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: module `main` belongs to app `my_app`, but reads env for `misc`

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
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: module `app_env` belongs to app `misc`, but reads env for `debug`

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
            %%  ^^^^^^^^^^^^^^^ warning: module `main` belongs to app `my_app`, but reads env for `misc`

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
