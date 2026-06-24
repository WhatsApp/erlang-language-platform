/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
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

use std::sync::LazyLock;

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::ModuleName;
use elp_ide_db::erlang_service::TestDef;
use elp_ide_db::erlang_service::common_test::GroupDef;
use elp_syntax::AstNode;
use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::FunctionDef;
use hir::Name;
use hir::NameArity;
use hir::Semantic;
use hir::known;

use crate::Runnable;
use crate::navigation_target::ToNav;
use crate::runnables::RunnableKind;

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

pub fn runnable_names(
    sema: &Semantic,
    file_id: FileId,
    all: &FxHashSet<TestDef>,
    groups: &FxHashMap<SmolStr, GroupDef>,
) -> Result<FxHashSet<NameArity>, ()> {
    runnables(sema, file_id, all, groups).map(|runnables| {
        runnables
            .into_iter()
            .filter_map(|runnable| match runnable.kind {
                RunnableKind::Test { name, .. } => Some(name),
                RunnableKind::Suite { .. } => None,
            })
            .collect()
    })
}

pub(crate) fn exported_test_ranges(
    sema: &Semantic,
    file_id: FileId,
) -> FxHashMap<NameArity, TextRange> {
    let mut res = FxHashMap::default();
    let def_map = sema.db.def_map_local(file_id);
    let functions = def_map.get_functions();
    let excludes = sema.resolve_callbacks(file_id);
    for (name_arity, def) in functions {
        if def.exported
            && !KNOWN_FUNCTIONS_ARITY_1.contains(name_arity)
            && !excludes.contains(name_arity)
            && let Some(name) = def.source(sema.db.upcast()).first().and_then(|f| f.name())
            && name_arity.arity() == 1
        {
            res.insert(name_arity.clone(), name.syntax().text_range());
        }
    }
    res
}

static KNOWN_FUNCTIONS_ARITY_1: LazyLock<FxHashSet<NameArity>> = LazyLock::new(|| {
    let mut res = FxHashSet::default();
    for name in [known::end_per_suite, known::init_per_suite, known::group] {
        res.insert(NameArity::new(name.clone(), 1));
    }
    res
});

// Populate the list of runnables for a Common Test test suite
pub fn runnables(
    sema: &Semantic,
    file_id: FileId,
    all: &FxHashSet<TestDef>,
    groups: &FxHashMap<SmolStr, GroupDef>,
) -> Result<Vec<Runnable>, ()> {
    let mut res = Vec::new();
    if let Some(module_name) = sema.module_name(file_id)
        && is_suite(&module_name)
    {
        // Add a runnable for the entire test suite
        if let Some(suite_runnable) = suite_to_runnable(sema, file_id) {
            res.push(suite_runnable);
        }
        runnables_for_test_defs(
            &mut res,
            sema,
            file_id,
            all.iter(),
            FxHashSet::default(),
            groups,
        );
    }
    Ok(res)
}

fn runnables_for_test_defs<'a>(
    res: &mut Vec<Runnable>,
    sema: &Semantic,
    file_id: FileId,
    test_defs: impl Iterator<Item = &'a TestDef>,
    group_names: FxHashSet<GroupName>,
    group_defs: &FxHashMap<SmolStr, GroupDef>,
) {
    for test_def in test_defs {
        match test_def {
            TestDef::TestName(testcase_name) => {
                if group_names.is_empty() {
                    if let Some(runnable) = runnable(
                        sema,
                        file_id,
                        Name::from_erlang_service(testcase_name),
                        GroupName::NoGroup,
                    ) {
                        res.push(runnable);
                    }
                } else {
                    for group_name in group_names.clone() {
                        if let Some(runnable) = runnable(
                            sema,
                            file_id,
                            Name::from_erlang_service(testcase_name),
                            group_name,
                        ) {
                            res.push(runnable);
                        }
                    }
                }
            }
            TestDef::GroupName(group_name) => {
                if !group_names.contains(&GroupName::Name(Name::from_erlang_service(group_name))) {
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
                if !group_names.contains(&GroupName::Name(Name::from_erlang_service(group_name))) {
                    let mut new_group_names = group_names.clone();
                    new_group_names.insert(GroupName::Name(Name::from_erlang_service(group_name)));
                    runnables_for_test_defs(
                        res,
                        sema,
                        file_id,
                        group_test_defs.iter(),
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
    group_name: &SmolStr,
    group_names: FxHashSet<GroupName>,
    group_defs: &FxHashMap<SmolStr, GroupDef>,
) {
    if let Some(GroupDef { name, content }) = group_defs.get(group_name) {
        let mut new_group_names = group_names;
        new_group_names.insert(GroupName::Name(Name::from_erlang_service(name)));
        runnables_for_test_defs(
            res,
            sema,
            file_id,
            content.iter(),
            new_group_names,
            group_defs,
        )
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
        kind: RunnableKind::Suite { suite },
    })
}

// Return a runnable for the given function definition
fn def_to_runnable(sema: &Semantic, def: &FunctionDef, group: GroupName) -> Option<Runnable> {
    let nav = def.to_nav(sema.db);
    let app_name = sema.db.file_app_name(def.file.file_id)?;
    let suite = sema.module_name(def.file.file_id)?.to_string();
    let case = def.name.name().to_string();
    let name = def.name.clone();
    let kind = RunnableKind::Test {
        name,
        app_name,
        suite,
        case,
        group,
    };
    Some(Runnable { nav, kind })
}

pub fn is_suite(module_name: &ModuleName) -> bool {
    module_name.ends_with(SUFFIX)
}
