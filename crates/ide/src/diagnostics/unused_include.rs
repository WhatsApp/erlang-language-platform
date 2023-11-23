/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Diagnostic: unused include (L1500)
//
// Return a warning if nothing is used from an include file

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::SearchScope;
use elp_ide_db::SymbolDefinition;
use elp_syntax::ast::AstNode;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::db::MinDefDatabase;
use hir::InFile;
use hir::IncludeAttribute;
use hir::Semantic;
use text_edit::TextEdit;

use super::Diagnostic;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;
use crate::fix;

pub(crate) fn unused_includes(
    sema: &Semantic,
    db: &dyn MinDefDatabase,
    diagnostics: &mut Vec<Diagnostic>,
    file_id: FileId,
) {
    let form_list = db.file_form_list(file_id);
    let mut cache = Default::default();
    for (include_idx, attr) in form_list.includes() {
        let in_file = InFile::new(file_id, include_idx);
        if let Some(include_file_id) = db.resolve_include(in_file) {
            if is_file_used(sema, db, include_file_id, file_id, &mut cache) {
                continue;
            }

            let path = match attr {
                IncludeAttribute::Include { path, .. } => path,
                IncludeAttribute::IncludeLib { path, .. } => path,
            };

            let source_file = db.parse(file_id);
            let inc_text_rage = attr
                .form_id()
                .get(&source_file.tree())
                .syntax()
                .text_range();

            let mut edit_builder = TextEdit::builder();
            edit_builder.delete(inc_text_rage);
            let edit = edit_builder.finish();

            let diagnostic = Diagnostic::new(
                DiagnosticCode::UnusedInclude,
                format!("Unused file: {}", path),
                inc_text_rage,
            )
            .with_severity(Severity::Warning)
            .with_fixes(Some(vec![fix(
                "remove_unused_include",
                "Remove unused include",
                SourceChange::from_text_edit(file_id, edit),
                inc_text_rage,
            )]));

            log::debug!("Found unused include {:?}", path);

            diagnostics.push(diagnostic);
        }
    }
}

fn is_file_used(
    sema: &Semantic,
    db: &dyn MinDefDatabase,
    include_file_id: FileId,
    target: FileId,
    cache: &mut FxHashMap<FileId, bool>,
) -> bool {
    if let Some(used) = cache.get(&include_file_id) {
        return *used;
    }

    let mut todo = FxHashSet::default();
    todo.insert(include_file_id);
    let scope = SearchScope::single_file(target, None);
    while let Some(file_id) = todo.iter().next().cloned() {
        todo.remove(&file_id);

        let list = db.file_form_list(file_id);
        for (include_idx, _) in list.includes() {
            let in_file = InFile::new(file_id, include_idx);
            if let Some(include_file_id) = db.resolve_include(in_file) {
                match cache.get(&include_file_id) {
                    None => todo.insert(include_file_id),
                    Some(true) => return true,
                    _ => false,
                };
            }
        }

        let def_map = db.local_def_map(file_id);
        if def_map.parse_transform {
            cache.insert(file_id, true);
            return true;
        }
        if !def_map.get_callbacks().is_empty() {
            cache.insert(file_id, true);
            return true;
        }

        if !def_map.get_exported_functions().is_empty() {
            cache.insert(file_id, true);
            return true;
        }

        if !def_map.get_exported_types().is_empty() {
            cache.insert(file_id, true);
            return true;
        }

        //TODO use find usages for that after it will work
        if !def_map.get_imports().is_empty() {
            cache.insert(file_id, true);
            return true;
        }

        for (_, fun_def) in def_map.get_functions() {
            if SymbolDefinition::Function(fun_def.clone())
                .usages(sema)
                .set_scope(&scope)
                .at_least_one()
            {
                cache.insert(file_id, true);
                return true;
            }
        }

        for type_def in def_map.get_types().values() {
            if SymbolDefinition::Type(type_def.clone())
                .usages(sema)
                .set_scope(&scope)
                .at_least_one()
            {
                cache.insert(file_id, true);
                return true;
            }
        }

        for record_def in def_map.get_records().values() {
            if SymbolDefinition::Record(record_def.clone())
                .usages(sema)
                .set_scope(&scope)
                .at_least_one()
            {
                cache.insert(file_id, true);
                return true;
            }
        }

        for macro_def in def_map.get_macros().values() {
            if SymbolDefinition::Define(macro_def.clone())
                .usages(sema)
                .set_scope(&scope)
                .at_least_one()
            {
                cache.insert(file_id, true);
                return true;
            }
        }

        cache.insert(file_id, false);
    }

    false
}

#[cfg(test)]
mod tests {

    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    #[track_caller]
    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn optimise_includes_unused_include_with_macro() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -define(FOO,3).
//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
%%^^^^^^^^^^^^^^^^^^^^ 💡 warning: Unused file: foo.hrl
        "#,
        );
    }

    #[test]
    fn optimise_includes_used_include_with_macro() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -define(FOO,3).
//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
  foo() -> ?FOO.
        "#,
        );
    }

    #[test]
    fn optimise_includes_unused_include_with_function() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  foo() -> bar.
//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
%%^^^^^^^^^^^^^^^^^^^^ 💡 warning: Unused file: foo.hrl
        "#,
        );
    }

    #[test]
    fn optimise_includes_used_include_with_function() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  foo() -> bar.
//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
  baz() -> foo().
        "#,
        );
    }

    #[test]
    fn optimise_includes_unused_include_with_type() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -type orddict(Key, Val) :: [{Key, Val}].
//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
%%^^^^^^^^^^^^^^^^^^^^ 💡 warning: Unused file: foo.hrl
        "#,
        );
    }

    #[test]
    fn optimise_includes_used_include_with_type() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -type orddict(Key, Val) :: [{Key, Val}].
//- /src/bar.erl
  -module(bar).
  -include("foo.hrl").
  -spec foo() -> orddict(integer(), integer()).
  foo() -> orddict(1, 2).
        "#,
        );
    }

    #[test]
    fn optimise_includes_unused_include_with_record() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -record(person, {name :: string(), height :: pos_integer()}).
//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
%%^^^^^^^^^^^^^^^^^^^^ 💡 warning: Unused file: foo.hrl
        "#,
        );
    }

    #[test]
    fn optimise_includes_used_include_exported_type() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -type orddict(Key, Val) :: [{Key, Val}].
  -export_type([orddict/2]).

//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
        "#,
        );
    }

    #[test]
    fn optimise_includes_used_include_exported_function() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  foo() -> bar.
  -export([foo/0]).

//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
        "#,
        );
    }

    #[test]
    fn optimise_includes_used_include_callback() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -callback terminate() -> 'ok'.

//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
        "#,
        );
    }

    #[test]
    fn optimise_includes_used_include_import() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -import(lists, [all/2]).

//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
        "#,
        );
    }

    #[test]
    fn optimise_includes_used_include_bug() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
-define(line,).
lol(A, _B) -> A.
-define(enum, lol).

//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
  bar() -> ?enum(1, [a,b]).
        "#,
        );
    }

    #[test]
    fn optimise_includes_used_transitive() {
        check_diagnostics(
            r#"
//- /include/header0.hrl include_path:/include
  bar() -> ok.
//- /include/header1.hrl include_path:/include
  -include("header0.hrl").
//- /include/header2.hrl include_path:/include
  -include("header1.hrl").

//- /src/foo.erl
  -module(foo).
  -include("header2.hrl").

  foo() -> bar().
        "#,
        );
    }

    #[test]
    fn optimise_includes_used_remote_transitive() {
        check_diagnostics(
            r#"
//- /kernel/include/logger.hrl include_path:/include app:kernel
    -define(LOG_WARNING, true).

//- /include/do_log.hrl include_path:/include app:lol

    -include_lib("kernel/include/logger.hrl").


//- /src/foo.erl app:lol
  -module(foo).
  -include("do_log.hrl").

  get_all_logs(_LogsDirectory) ->
    ?LOG_WARNING.
"#,
        );
    }

    #[test]
    fn used_for_parse_transform() {
        check_diagnostics(
            r#"
//- /stdlib/include/ms_transform.hrl include_path:/include app:stdlib
    -compile({parse_transform,ms_transform}).


//- /src/foo.erl app:lol
  -module(foo).
  -include_lib("stdlib/include/ms_transform.hrl").

  select_all() ->
      Match = ets:fun2ms(fun(#corp{login = L, props = P}) when L /= 'unknown' -> {L, P} end),
      Match.
"#,
        );
    }
}
