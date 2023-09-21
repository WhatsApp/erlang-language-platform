/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// This module implements native support for the Common Test testing framework in ELP.
// The main use case is to provide code lenses so that users can run testcases
// directly from the IDE.
//
// In a Common Test suite, tests are defined via a callback function: `all/0`.
// Tests can also be grouped together and groups definitions are provided via an
// additional callback function: `groups/0`.
//
// For more information about Common Test and the structure of a test suite,
// please see:
//
//   * https://www.erlang.org/doc/apps/common_test/introduction.html
//   * https://www.erlang.org/doc/man/ct_suite.html
//
// We currently parse test and group definitions, without evaluating those functions.
// This means that, for the time being, only tests specified as literals are supported.
// This limitation can be solved by leveraging the Erlang Service in ELP to evaluate
// those functions before processing them.

use elp_ide_db::elp_base_db::FileId;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::known;
use hir::Body;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFile;
use hir::InFunctionBody;
use hir::Literal;
use hir::Name;
use hir::NameArity;
use hir::Semantic;
use lazy_static::lazy_static;

use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;
use crate::navigation_target::ToNav;
use crate::runnables::RunnableKind;
use crate::Runnable;

const SUFFIX: &str = "_SUITE";

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum GroupName {
    NoGroup, // Used to allow tests to run outside of a group
    Name(Name),
}

impl GroupName {
    pub fn name(&self) -> String {
        match self {
            GroupName::NoGroup => "".to_string(),
            GroupName::Name(name) => name.to_string(),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct GroupDef {
    name: Name,
    content: Vec<TestDef>,
}

impl GroupDef {
    pub(crate) fn new(
        sema: &Semantic,
        body: &Body,
        group_name: ExprId,
        group_content: ExprId,
    ) -> Option<Self> {
        let group_name = &body[group_name].as_atom()?;
        let group_name = sema.db.lookup_atom(*group_name);
        let group_content = parse_group_content(sema, body, group_content).ok()?;
        Some(GroupDef {
            name: group_name,
            content: group_content,
        })
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum TestDef {
    TestName(Name),
    GroupName(Name),
    GroupDef(Name, Vec<TestDef>),
}

pub fn unreachable_test(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    let exported_test_ranges = exported_test_ranges(sema, file_id);
    if let Ok(runnable_names) = runnable_names(sema, file_id) {
        for (name, range) in exported_test_ranges {
            if !runnable_names.contains(&name) {
                let d = Diagnostic::new(
                    DiagnosticCode::UnreachableTest,
                    format!("Unreachable test ({name})"),
                    range,
                )
                .severity(Severity::Warning)
                .with_ignore_fix(sema, file_id);
                diagnostics.push(d);
            }
        }
    }
}

fn runnable_names(sema: &Semantic, file_id: FileId) -> Result<FxHashSet<NameArity>, ()> {
    runnables(sema, file_id).map(|runnables| {
        runnables
            .into_iter()
            .filter_map(|runnable| match runnable.kind {
                RunnableKind::Test { name, .. } => Some(name),
                RunnableKind::Suite => None,
            })
            .collect()
    })
}

fn exported_test_ranges(sema: &Semantic, file_id: FileId) -> FxHashMap<NameArity, TextRange> {
    let mut res = FxHashMap::default();
    let def_map = sema.db.def_map(file_id);
    let functions = def_map.get_functions();
    for (name_arity, def) in functions {
        if def.exported && !KNOWN_FUNCTIONS_ARITY_1.contains(name_arity) {
            if let Some(name) = def.source(sema.db.upcast()).name() {
                if name_arity.arity() == 1 {
                    res.insert(name_arity.clone(), name.syntax().text_range());
                }
            }
        }
    }
    res
}

lazy_static! {
    static ref KNOWN_FUNCTIONS_ARITY_1: FxHashSet<NameArity> = {
        let mut res = FxHashSet::default();
        for name in vec![known::end_per_suite, known::init_per_suite, known::group] {
            res.insert(NameArity::new(name.clone(), 1));
        }
        res
    };
}

// Populate the list of runnables for a Common Test test suite
pub fn runnables(sema: &Semantic, file_id: FileId) -> Result<Vec<Runnable>, ()> {
    let mut res = Vec::new();
    if let Some(module_name) = sema.module_name(file_id) {
        if module_name.ends_with(SUFFIX) {
            // Add a runnable for the entire test suite
            if let Some(suite_runnable) = suite_to_runnable(sema, file_id) {
                res.push(suite_runnable);
            }
            // Parse and expand the content of the groups/0 function
            let groups = groups(sema, file_id)?;
            // Parse the content of the all/0 function
            let all = all(sema, file_id)?;
            // Finally produce the list of runnables
            let test_defs = Vec::from_iter(all);
            runnables_for_test_defs(
                &mut res,
                sema,
                file_id,
                &test_defs,
                FxHashSet::default(),
                &groups,
            );
        }
    }
    Ok(res)
}

fn runnables_for_test_defs(
    res: &mut Vec<Runnable>,
    sema: &Semantic,
    file_id: FileId,
    test_defs: &Vec<TestDef>,
    group_names: FxHashSet<GroupName>,
    group_defs: &FxHashMap<Name, GroupDef>,
) {
    for test_def in test_defs {
        match test_def {
            TestDef::TestName(testcase_name) => {
                if group_names.is_empty() {
                    if let Some(runnable) =
                        runnable(sema, file_id, testcase_name.clone(), GroupName::NoGroup)
                    {
                        res.push(runnable);
                    }
                } else {
                    for group_name in group_names.clone() {
                        if let Some(runnable) =
                            runnable(sema, file_id, testcase_name.clone(), group_name)
                        {
                            res.push(runnable);
                        }
                    }
                }
            }
            TestDef::GroupName(group_name) => {
                if !group_names.contains(&GroupName::Name(group_name.clone())) {
                    runnables_for_group_def(
                        res,
                        sema,
                        file_id,
                        group_name,
                        group_names.clone(),
                        group_defs,
                    )
                }
            }
            TestDef::GroupDef(group_name, group_test_defs) => {
                if !group_names.contains(&GroupName::Name(group_name.clone())) {
                    let mut new_group_names = group_names.clone();
                    new_group_names.insert(GroupName::Name(group_name.clone()));
                    runnables_for_test_defs(
                        res,
                        sema,
                        file_id,
                        group_test_defs,
                        new_group_names,
                        group_defs,
                    )
                }
            }
        }
    }
}

fn runnables_for_group_def(
    res: &mut Vec<Runnable>,
    sema: &Semantic,
    file_id: FileId,
    group_name: &Name,
    group_names: FxHashSet<GroupName>,
    group_defs: &FxHashMap<Name, GroupDef>,
) {
    if let Some(GroupDef { name, content }) = group_defs.get(group_name) {
        let mut new_group_names = group_names.clone();
        new_group_names.insert(GroupName::Name(name.clone()));
        runnables_for_test_defs(res, sema, file_id, content, new_group_names, group_defs)
    }
}

// A testcase is runnable if:
//   * A corresponding function with arity 1 exists
//   * That function is exported
fn runnable(
    sema: &Semantic,
    file_id: FileId,
    name: Name,
    group_name: GroupName,
) -> Option<Runnable> {
    let def_map = sema.def_map(file_id);
    let name_arity = NameArity::new(name, 1);
    let def = def_map.get_function(&name_arity)?;
    if def.exported {
        def_to_runnable(sema, def, group_name)
    } else {
        None
    }
}

// Return a runnable for the given test suite
fn suite_to_runnable(sema: &Semantic, file_id: FileId) -> Option<Runnable> {
    let suite = sema.module_name(file_id)?.to_string();
    let module = sema.resolve_module_name(file_id, suite.as_str())?;
    let nav = module.to_nav(sema.db);
    Some(Runnable {
        nav,
        kind: RunnableKind::Suite,
    })
}

// Return a runnable for the given function definition
fn def_to_runnable(sema: &Semantic, def: &FunctionDef, group: GroupName) -> Option<Runnable> {
    let nav = def.to_nav(sema.db);
    let app_name = sema.db.file_app_name(def.file.file_id)?;
    let suite = sema.module_name(def.file.file_id)?.to_string();
    let name = def.function.name.clone();
    let kind = RunnableKind::Test {
        name: name.clone(),
        app_name,
        suite,
        case: name.name().to_string(),
        group,
    };
    Some(Runnable { nav, kind })
}

// Extract the test definitions from the content of the all/0 function
fn all(sema: &Semantic, file_id: FileId) -> Result<FxHashSet<TestDef>, ()> {
    let mut res = FxHashSet::default();

    if let Some(expr) = top_level_expression(sema, file_id, known::all, 0) {
        let body = expr.body();
        match &body[expr.value] {
            Expr::List { exprs, tail: _ } => {
                for expr_id in exprs {
                    parse_test_definition(&mut res, sema, &body, *expr_id)?
                }
            }
            _ => return Err(()),
        }
    }

    Ok(res)
}

// Parse each entry from the `all/0` function.
// See https://www.erlang.org/doc/man/ct_suite.html#Module:all-0 for details
fn parse_test_definition(
    res: &mut FxHashSet<TestDef>,
    sema: &Semantic,
    body: &Body,
    expr_id: ExprId,
) -> Result<(), ()> {
    match &body[expr_id] {
        Expr::Literal(Literal::Atom(testcase_name)) => {
            let testcase_name = sema.db.lookup_atom(*testcase_name);
            res.insert(TestDef::TestName(testcase_name));
        }
        Expr::Tuple { exprs } => match exprs[..] {
            [first, second] => match (&body[first], &body[second]) {
                (
                    Expr::Literal(Literal::Atom(group_tag)),
                    Expr::Literal(Literal::Atom(group_name)),
                ) => {
                    if sema.db.lookup_atom(*group_tag) == known::group {
                        // {group, Group}
                        let group_name = sema.db.lookup_atom(*group_name);
                        res.insert(TestDef::GroupName(group_name));
                    }
                }
                _ => return Err(()),
            },
            [first, second, _third] => match (&body[first], &body[second]) {
                (Expr::Literal(Literal::Atom(first)), Expr::Literal(Literal::Atom(second))) => {
                    if sema.db.lookup_atom(*first) == known::group {
                        // {group, Group, _Properties}
                        let group_name = sema.db.lookup_atom(*second);
                        res.insert(TestDef::GroupName(group_name));
                    } else if sema.db.lookup_atom(*first) == known::testcase {
                        // {testcase, Testcase, _Properties}
                        let testcase_name = sema.db.lookup_atom(*second);
                        res.insert(TestDef::TestName(testcase_name));
                    }
                }
                _ => return Err(()),
            },
            [first, second, _third, _fourth] => {
                match (&body[first], &body[second]) {
                    (Expr::Literal(Literal::Atom(first)), Expr::Literal(Literal::Atom(second))) => {
                        if sema.db.lookup_atom(*first) == known::group {
                            // {group, Group, _Properties, _SubGroupsProperties}
                            let group_name = sema.db.lookup_atom(*second);
                            res.insert(TestDef::GroupName(group_name));
                        }
                    }
                    _ => return Err(()),
                }
            }
            _ => return Err(()),
        },
        _ => return Err(()),
    };
    Ok(())
}

// Given a function expressed in terms of name and arity, return the top-level expression for that function.
// This is used to parse the content of the all/0 and groups/0 functions.
// The function must be exported.
fn top_level_expression(
    sema: &Semantic,
    file_id: FileId,
    name: Name,
    arity: u32,
) -> Option<InFunctionBody<ExprId>> {
    let def_map = sema.def_map(file_id);
    let exported_functions = def_map.get_exported_functions();
    let name_arity = exported_functions.get(&NameArity::new(name, arity))?;
    let body = def_map.get_function(name_arity)?;
    let function_id = InFile::new(file_id, body.function_id);
    let function_body = sema.to_function_body(function_id);
    let (_clause_idx, clause) = function_body.clauses().next()?;
    let expr_id = clause.exprs.first()?;
    Some(function_body.with_value(*expr_id))
}

// Extract the group definitions from the content of the groups/0 function.
fn groups(sema: &Semantic, file_id: FileId) -> Result<FxHashMap<Name, GroupDef>, ()> {
    let mut res = FxHashMap::default();

    if let Some(expr) = top_level_expression(sema, file_id, known::groups, 0) {
        let body = expr.body();
        match &body[expr.value] {
            Expr::List { exprs, tail: _ } => {
                for expr_id in exprs {
                    match parse_group(sema, &body, *expr_id) {
                        Some(group_def) => {
                            res.insert(group_def.name.clone(), group_def);
                        }
                        None => return Err(()),
                    }
                }
            }
            _ => return Err(()),
        }
    }

    Ok(res)
}

// Parse each entry from the `groups/0` function.
// See https://www.erlang.org/doc/man/ct_suite.html#Module:groups-0 for details
fn parse_group(sema: &Semantic, body: &Body, expr_id: ExprId) -> Option<GroupDef> {
    match &body[expr_id] {
        Expr::Tuple { exprs } => match exprs[..] {
            [group_name, _properties, group_content] => {
                GroupDef::new(sema, body, group_name, group_content)
            }
            [group_name, group_content] => GroupDef::new(sema, body, group_name, group_content),
            _ => None,
        },
        _ => None,
    }
}

fn parse_group_content(
    sema: &Semantic,
    body: &Body,
    group_content: ExprId,
) -> Result<Vec<TestDef>, ()> {
    let mut res = Vec::new();
    match &body[group_content] {
        Expr::List { exprs, tail: _ } => {
            for expr in exprs {
                parse_group_content_entry(&mut res, sema, body.clone(), *expr)?
            }
        }
        _ => return Err(()),
    };
    Ok(res)
}

fn parse_group_content_entry(
    res: &mut Vec<TestDef>,
    sema: &Semantic,
    body: &Body,
    expr_id: ExprId,
) -> Result<(), ()> {
    match &body[expr_id] {
        Expr::Literal(Literal::Atom(testcase_name)) => {
            let testcase_name = sema.db.lookup_atom(*testcase_name);
            res.push(TestDef::TestName(testcase_name));
        }
        Expr::Tuple { exprs } => match exprs[..] {
            [group_tag, group_name] => match (&body[group_tag], &body[group_name]) {
                (
                    Expr::Literal(Literal::Atom(group_tag)),
                    Expr::Literal(Literal::Atom(group_name)),
                ) => {
                    if sema.db.lookup_atom(*group_tag) == known::group {
                        let group_name = sema.db.lookup_atom(*group_name);
                        res.push(TestDef::GroupName(group_name));
                    }
                }
                _ => return Err(()),
            },
            [group_name, _properties, group_content] => {
                if let Some(group_name) = &body[group_name].as_atom() {
                    let group_name = sema.db.lookup_atom(*group_name);
                    let content = parse_group_content(sema, body, group_content)?;
                    res.push(TestDef::GroupDef(group_name, content))
                }
            }
            _ => return Err(()),
        },
        _ => return Err(()),
    };
    Ok(())
}

#[cfg(test)]
mod tests {

    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix;

    #[track_caller]
    pub(crate) fn check_diagnostics(ra_fixture: &str) {
        let config =
            DiagnosticsConfig::default().disable(DiagnosticCode::MissingCompileWarnMissingSpec);
        check_diagnostics_with_config(config, ra_fixture)
    }

    #[test]
    fn test_unreachable_test() {
        check_diagnostics(
            r#"
//- /my_app/test/my_SUITE.erl
   -module(my_SUITE).
   -export([all/0]).
   -export([a/1, b/1]).
   all() -> [a].
   a(_Config) ->
     ok.
   b(_Config) ->
%% ^ 💡 warning: Unreachable test (b/1)
     ok.
            "#,
        );
    }

    #[test]
    fn test_unreachable_test_init_end() {
        check_diagnostics(
            r#"
//- /my_app/test/my_SUITE.erl
   -module(my_SUITE).
   -export([all/0]).
   -export([init_per_suite/1, end_per_suite/1]).
   -export([a/1, b/1]).
   all() -> [a].
   init_per_suite(Config) -> Config.
   end_per_suite(_Config) -> ok.
   a(_Config) ->
     ok.
   b(_Config) ->
%% ^ 💡 warning: Unreachable test (b/1)
     ok.
            "#,
        );
    }

    #[test]
    fn test_unreachable_test_unparsable_all() {
        check_diagnostics(
            r#"
//- /my_app/test/my_SUITE.erl
   -module(my_SUITE).
   -export([all/0]).
   -export([init_per_suite/1, end_per_suite/1]).
   -export([a/1, b/1]).
   all() -> do_all().
   do_all() -> [a].
   init_per_suite(Config) -> Config.
   end_per_suite(_Config) -> ok.
   a(_Config) ->
     ok.
   b(_Config) ->
     ok.
            "#,
        );
    }

    #[test]
    fn test_unreachable_test_ignore() {
        check_diagnostics(
            r#"
//- /my_app/test/my_SUITE.erl
   -module(my_SUITE).
   -export([all/0]).
   -export([a/1, b/1, c/1]).
   all() -> [a].
   a(_Config) ->
     ok.
   % elp:ignore W0008
   b(_Config) ->
     ok.
   c(_Config) ->
%% ^ 💡 warning: Unreachable test (c/1)
     ok.
            "#,
        );
    }
    #[test]
    fn test_unreachable_test_ignore_by_label() {
        check_diagnostics(
            r#"
//- /my_app/test/my_SUITE.erl
   -module(my_SUITE).
   -export([all/0]).
   -export([a/1, b/1, c/1]).
   all() -> [a].
   a(_Config) ->
     ok.
   % elp:ignore unreachable_test
   b(_Config) ->
     ok.
   c(_Config) ->
%% ^ 💡 warning: Unreachable test (c/1)
     ok.
            "#,
        );
    }

    #[test]
    fn test_unreachable_test_fix() {
        check_diagnostics(
            r#"
 //- /my_app/test/my_SUITE.erl
    -module(my_SUITE).
    -export([all/0]).
    -export([a/1, b/1, c/1]).
    all() -> [a].
    a(_Config) ->
      ok.
    b(_Config) ->
 %% ^ 💡 warning: Unreachable test (b/1)
      ok.
    c(_Config) ->
 %% ^ 💡 warning: Unreachable test (c/1)
      ok.
     "#,
        );
        check_fix(
            r#"
//- /my_app/test/my_SUITE.erl
-module(my_SUITE).
-export([all/0]).
-export([a/1, b/1, c/1]).
all() -> [a].
a(_Config) ->
  ok.
b(_Config) ->
  ok.
c~(_Config) ->
  ok.
     "#,
            r#"
-module(my_SUITE).
-export([all/0]).
-export([a/1, b/1, c/1]).
all() -> [a].
a(_Config) ->
  ok.
b(_Config) ->
  ok.
% elp:ignore W0008 (unreachable_test)
c(_Config) ->
  ok.
     "#,
        );
    }
}
