/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: redundant-ct-callback (W0094)
//
// Return a warning if a Common Test suite defines `init_per_*`/`end_per_*`
// callbacks that only do what Common Test does when they are missing.

use std::borrow::Cow;
use std::cell::LazyCell;

use elp_ide_db::SearchScope;
use elp_ide_db::SymbolDefinition;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::AstNode;
use fxhash::FxHashSet;
use hir::Expr;
use hir::FunctionDef;
use hir::Literal;
use hir::Name;
use hir::NameArity;
use hir::Pat;
use hir::Semantic;
use hir::known;

use crate::common_test;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::DiagnosticTag;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::diagnostics::Severity;

pub(crate) struct RedundantCtCallbackLinter;

impl Linter for RedundantCtCallbackLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::RedundantCtCallback
    }

    fn description(&self) -> &'static str {
        "Common Test callback does what Common Test does when it is missing."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::Warning
    }

    fn should_process_headers(&self) -> bool {
        false
    }

    fn should_process_generated_files(&self) -> bool {
        false
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.module_name(file_id)
            .is_some_and(|name| common_test::is_suite(&name))
    }
}

#[derive(Clone, Copy)]
enum Kind {
    /// Returns its `Config` argument unchanged.
    Init,
    /// Returns `ok`.
    End,
}

/// The sets of callbacks that can only be removed together. Common Test calls
/// its suite and group defaults only when a suite defines neither callback of
/// the pair, and fails with `undef` when it defines just one. It skips a
/// missing `init_per_testcase/2` or `end_per_testcase/2` on its own, whatever
/// the other one does.
fn callback_sets() -> [Vec<(NameArity, Kind)>; 4] {
    let callback = |name: &Name, arity, kind| (NameArity::new(name.clone(), arity), kind);
    [
        vec![
            callback(&known::init_per_suite, 1, Kind::Init),
            callback(&known::end_per_suite, 1, Kind::End),
        ],
        vec![
            callback(&known::init_per_group, 2, Kind::Init),
            callback(&known::end_per_group, 2, Kind::End),
        ],
        vec![callback(&known::init_per_testcase, 2, Kind::Init)],
        vec![callback(&known::end_per_testcase, 2, Kind::End)],
    ]
}

impl GenericLinter for RedundantCtCallbackLinter {
    type Context = NameArity;

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<NameArity>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let def_map = sema.def_map_local(file_id);
        // Only computed when some callback set is trivial, which most
        // suites have none of.
        let other_callbacks = LazyCell::new(|| other_behaviour_callbacks(sema, file_id));
        let remote_scope = LazyCell::new(|| remote_scope(sema, file_id));

        let res = callback_sets()
            .into_iter()
            .flat_map(|callbacks| {
                let defined: Vec<(&FunctionDef, Kind)> = callbacks
                    .into_iter()
                    .filter_map(|(name, kind)| Some((def_map.get_function(&name)?, kind)))
                    .collect();
                // The syntactic check of every callback comes first: the
                // usage searches are only needed for trivial sets.
                let redundant = !defined.is_empty()
                    && defined
                        .iter()
                        .all(|(fun_def, kind)| is_default(sema, fun_def, *kind))
                    && defined.iter().all(|(fun_def, _)| {
                        !other_callbacks.contains(&fun_def.name)
                            && !has_usages(sema, file_id, fun_def, &remote_scope)
                    });
                defined
                    .into_iter()
                    .filter(move |_| redundant)
                    .filter_map(move |(fun_def, _)| {
                        Some(GenericLinterMatchContext {
                            range: FileRange {
                                file_id,
                                range: fun_def.name_range(sema.db)?,
                            },
                            context: fun_def.name.clone(),
                        })
                    })
            })
            .collect();
        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!(
            "Callback {} only does what Common Test does when it is missing.",
            context
        ))
    }

    fn tag(&self, _context: &Self::Context) -> Option<DiagnosticTag> {
        Some(DiagnosticTag::Unused)
    }
}

pub static LINTER: RedundantCtCallbackLinter = RedundantCtCallbackLinter;

/// Callbacks of behaviours other than `ct_suite`, which may require them.
fn other_behaviour_callbacks(sema: &Semantic, file_id: FileId) -> FxHashSet<NameArity> {
    sema.def_map(file_id)
        .get_behaviours()
        .iter()
        .filter(|name| **name != *known::ct_suite)
        .filter_map(|name| sema.resolve_behaviour(file_id, name))
        .flat_map(|(_, callbacks)| callbacks.into_keys())
        .collect()
}

/// Whether the callback has a single unguarded clause whose arguments are all
/// variables and which does what the Common Test default does.
fn is_default(sema: &Semantic, fun_def: &FunctionDef, kind: Kind) -> bool {
    let fun_body = fun_def.in_function_body(sema, fun_def);
    let mut clauses = fun_body.clauses();
    let (Some((clause_id, clause)), None) = (clauses.next(), clauses.next()) else {
        return false;
    };
    let clause = &clause.clause;
    let [expr_id] = clause.exprs.as_slice() else {
        return false;
    };
    if !clause.guards.is_empty() {
        return false;
    }
    let in_clause = fun_body.in_clause(clause_id);
    let vars: Option<Vec<_>> = clause
        .pats
        .iter()
        .map(|pat_id| match &in_clause[*pat_id] {
            Pat::Var(var) => Some(*var),
            _ => None,
        })
        .collect();
    let Some(vars) = vars else {
        return false;
    };
    match (kind, &in_clause[*expr_id]) {
        (Kind::Init, Expr::Var(var)) => vars.last() == Some(var),
        (Kind::End, Expr::Literal(Literal::Atom(_))) => in_clause
            .as_atom_name(expr_id)
            .is_some_and(|name| name == *known::ok),
        _ => false,
    }
}

/// Files other than `file_id` that name its module, where a remote call to
/// one of its callbacks could be.
fn remote_scope(sema: &Semantic, file_id: FileId) -> Option<SearchScope> {
    let project = sema.db.file_app(file_id)?.project(sema.db);
    let module_name = sema.module_name(file_id)?;
    Some(
        SearchScope::project(sema.db, project)
            .exclude_file(file_id)
            .retain_files_naming_module(sema.db, project, &module_name),
    )
}

/// Whether anything calls or references the callback, besides its export
/// entries and spec.
fn has_usages(
    sema: &Semantic,
    file_id: FileId,
    fun_def: &FunctionDef,
    remote_scope: &Option<SearchScope>,
) -> bool {
    let baseline =
        export_entry_count(sema, file_id, &fun_def.name) + usize::from(fun_def.spec.is_some());
    let file_scope = SearchScope::single_file(file_id, None);
    let mut local = SymbolDefinition::Function(fun_def.clone()).usages(sema);
    local.set_scope(&file_scope);
    if local.at_least_n(baseline + 1) {
        return true;
    }
    let Some(remote_scope) = remote_scope else {
        return false;
    };
    let mut remote = SymbolDefinition::Function(fun_def.clone()).usages(sema);
    remote.set_scope(remote_scope);
    remote.at_least_one()
}

/// How many entries of the export attributes name `name`. A function can be
/// exported more than once, and each entry is a reference to it.
fn export_entry_count(sema: &Semantic, file_id: FileId, name: &NameArity) -> usize {
    let source = sema.parse(file_id).value;
    let name = name.to_string();
    sema.form_list(file_id)
        .exports()
        .flat_map(|(_, export)| export.form_id.get(&source).funs())
        .filter(|fa| fa.syntax().text() == name.as_str())
        .count()
}

#[cfg(test)]
mod tests {
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    fn config() -> DiagnosticsConfig {
        DiagnosticsConfig::default()
            .enable(DiagnosticCode::RedundantCtCallback)
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::UnnecessaryFunctionArg)
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        check_diagnostics_with_config(config(), fixture)
    }

    #[test]
    fn default_pairs_are_reported() {
        check_diagnostics(
            r#"
//- /test/main_SUITE.erl
-module(main_SUITE).
-export([all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([a/1]).

all() -> [{group, g}].
groups() -> [{g, [a]}].

   init_per_suite(Config) -> Config.
%% ^^^^^^^^^^^^^^ warning: W0094: Callback init_per_suite/1 only does what Common Test does when it is missing.
%%              | 💡 <suppression>
   end_per_suite(_Config) -> ok.
%% ^^^^^^^^^^^^^ warning: W0094: Callback end_per_suite/1 only does what Common Test does when it is missing.
%%             | 💡 <suppression>

   init_per_group(_Group, Config) -> Config.
%% ^^^^^^^^^^^^^^ warning: W0094: Callback init_per_group/2 only does what Common Test does when it is missing.
%%              | 💡 <suppression>
   end_per_group(_, _) -> ok.
%% ^^^^^^^^^^^^^ warning: W0094: Callback end_per_group/2 only does what Common Test does when it is missing.
%%             | 💡 <suppression>

   init_per_testcase(_TestCase, Config) -> Config.
%% ^^^^^^^^^^^^^^^^^ warning: W0094: Callback init_per_testcase/2 only does what Common Test does when it is missing.
%%                 | 💡 <suppression>
   end_per_testcase(_TestCase, _Config) -> ok.
%% ^^^^^^^^^^^^^^^^ warning: W0094: Callback end_per_testcase/2 only does what Common Test does when it is missing.
%%                | 💡 <suppression>

a(_Config) -> ok.
            "#,
        );
    }

    #[test]
    fn lone_default_callback_is_reported() {
        check_diagnostics(
            r#"
//- /test/main_SUITE.erl
-module(main_SUITE).
-export([all/0, init_per_testcase/2, a/1]).

all() -> [a].

   init_per_testcase(_TestCase, Config) -> Config.
%% ^^^^^^^^^^^^^^^^^ warning: W0094: Callback init_per_testcase/2 only does what Common Test does when it is missing.
%%                 | 💡 <suppression>

a(_Config) -> ok.
            "#,
        );
    }

    #[test]
    fn default_end_kept_when_init_does_work() {
        check_diagnostics(
            r#"
//- /test/main_SUITE.erl
-module(main_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1, a/1]).

all() -> [a].

init_per_suite(Config) -> [{key, value} | Config].
end_per_suite(_Config) -> ok.

a(_Config) -> ok.
            "#,
        );
    }

    #[test]
    fn testcase_callbacks_are_reported_on_their_own() {
        check_diagnostics(
            r#"
//- /test/main_SUITE.erl
-module(main_SUITE).
-export([all/0, init_per_testcase/2, end_per_testcase/2, a/1]).

all() -> [a].

init_per_testcase(_TestCase, Config) -> [{key, value} | Config].
   end_per_testcase(_TestCase, _Config) -> ok.
%% ^^^^^^^^^^^^^^^^ warning: W0094: Callback end_per_testcase/2 only does what Common Test does when it is missing.
%%                | 💡 <suppression>

a(_Config) -> ok.
            "#,
        );
    }

    #[test]
    fn non_default_callbacks_are_not_reported() {
        check_diagnostics(
            r#"
//- /test/main_SUITE.erl
-module(main_SUITE).
-export([all/0, init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

all() -> [].

init_per_group(my_group, Config) -> Config.
end_per_group(_Group, Config) -> Config.

init_per_testcase(_TestCase, Config) when is_list(Config) -> Config;
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, Config) -> ok = cleanup(Config), ok.

cleanup(_) -> ok.
            "#,
        );
    }

    #[test]
    fn init_returning_another_variable_is_not_reported() {
        check_diagnostics(
            r#"
//- /test/main_SUITE.erl
-module(main_SUITE).
-export([all/0, init_per_group/2]).

all() -> [].

init_per_group(Config, _Group) -> Config.
            "#,
        );
    }

    #[test]
    fn callback_exported_twice_is_reported() {
        check_diagnostics(
            r#"
//- /test/main_SUITE.erl
-module(main_SUITE).
-export([all/0, end_per_testcase/2, a/1]).
-export([end_per_testcase/2]).

all() -> [a].

-spec end_per_testcase(atom(), list()) -> ok.
   end_per_testcase(_TestCase, _Config) -> ok.
%% ^^^^^^^^^^^^^^^^ warning: W0094: Callback end_per_testcase/2 only does what Common Test does when it is missing.
%%                | 💡 <suppression>

a(_Config) -> ok.
            "#,
        );
    }

    #[test]
    fn called_callback_is_not_reported() {
        check_diagnostics(
            r#"
//- /test/main_SUITE.erl
-module(main_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2]).

all() -> [].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_group(_Group, Config) -> init_per_suite(Config).
end_per_group(_Group, Config) -> end_per_suite(Config).
            "#,
        );
    }

    #[test]
    fn callback_called_from_another_module_is_not_reported() {
        check_diagnostics(
            r#"
//- /test/main_SUITE.erl
-module(main_SUITE).
-export([all/0, init_per_suite/1]).

all() -> [].

init_per_suite(Config) -> Config.

//- /test/helper.erl
-module(helper).
-export([setup/0]).

setup() -> main_SUITE:init_per_suite([]).
            "#,
        );
    }

    #[test]
    fn non_suite_module_is_not_reported() {
        check_diagnostics(
            r#"
//- /test/helper.erl
-module(helper).
-export([init_per_suite/1, end_per_suite/1]).

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
            "#,
        );
    }
}
