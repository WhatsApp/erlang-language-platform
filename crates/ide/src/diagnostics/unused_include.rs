/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unused include
//
// Return a warning if nothing is used from an include file

use std::borrow::Cow;

use elp_ide_assists::helpers::extend_range;
use elp_ide_db::SearchScope;
use elp_ide_db::SymbolDefinition;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use elp_syntax::ast::AstNode;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::FormIdx;
use hir::InFile;
use hir::IncludeAttribute;
use hir::Name;
use hir::Semantic;
use hir::db::DefDatabase;
use hir::known;
use lazy_static::lazy_static;

use crate::Assist;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::fix;

lazy_static! {
    static ref EXCLUDES: FxHashSet<SmolStr> = ["common_test/include/ct.hrl"]
        .iter()
        .map(SmolStr::new)
        .collect();
}

pub(crate) struct UnusedIncludeLinter;

impl Linter for UnusedIncludeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnusedInclude
    }

    fn description(&self) -> &'static str {
        "Unused include file"
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        let file_kind = sema.db.file_kind(file_id);
        file_kind.is_module()
    }

    fn should_process_generated_files(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
    path: SmolStr,
    extended_range: TextRange,
}

impl Default for Context {
    fn default() -> Self {
        Context {
            path: SmolStr::new(""),
            extended_range: TextRange::default(),
        }
    }
}

impl GenericLinter for UnusedIncludeLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let db = sema.db;
        let form_list = db.file_form_list(file_id);
        let mut cache = Default::default();
        let source_file = db.parse(file_id);
        let mut res = Vec::new();

        for (include_idx, attr) in form_list.includes() {
            if !EXCLUDES.contains(attr.path()) {
                let in_file = InFile::new(file_id, include_idx);
                if let Some(include_file_id) = db.resolve_include(in_file) {
                    if is_file_used(sema, db, include_file_id, file_id, &mut cache) {
                        continue;
                    }

                    let path = match attr {
                        IncludeAttribute::Include { path, .. } => path,
                        IncludeAttribute::IncludeLib { path, .. } => path,
                    };
                    let attribute = attr.form_id().get(&source_file.tree());
                    let attribute_syntax = attribute.syntax();
                    let attribute_range = attribute_syntax.text_range();
                    let extended_attribute_range = extend_range(attribute_syntax);

                    log::debug!("Found unused include {path:?}");

                    res.push(GenericLinterMatchContext {
                        range: attribute_range,
                        context: Context {
                            path: path.clone(),
                            extended_range: extended_attribute_range,
                        },
                    });
                }
            }
        }
        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!("Unused file: {}", context.path))
    }

    fn fixes(
        &self,
        context: &Self::Context,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let mut edit_builder = TextEdit::builder();
        edit_builder.delete(context.extended_range);
        let edit = edit_builder.finish();

        Some(vec![fix(
            "remove_unused_include",
            "Remove unused include",
            SourceChange::from_text_edit(file_id, edit),
            context.extended_range,
        )])
    }
}

pub static LINTER: UnusedIncludeLinter = UnusedIncludeLinter;

fn is_file_used(
    sema: &Semantic,
    db: &dyn DefDatabase,
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

        let form_list = db.file_form_list(file_id);
        for (include_idx, _) in form_list.includes() {
            let in_file = InFile::new(file_id, include_idx);
            if let Some(include_file_id) = db.resolve_include(in_file) {
                match cache.get(&include_file_id) {
                    None => todo.insert(include_file_id),
                    Some(true) => return true,
                    _ => false,
                };
            }
        }

        let def_map = db.def_map_local(file_id);
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

        for &form in form_list.forms() {
            match form {
                FormIdx::ModuleAttribute(_) => return true,
                FormIdx::Export(_) => return true,
                FormIdx::Import(_) => return true,
                FormIdx::TypeExport(_) => return true,
                FormIdx::Behaviour(_) => return true,
                FormIdx::Callback(_) => return true,
                FormIdx::OptionalCallbacks(_) => return true,
                FormIdx::Attribute(idx) => {
                    let attr = &form_list[idx];
                    if !NO_MARK_USED_ATTRIBUTES.contains(&attr.name) {
                        return true;
                    }
                }
                FormIdx::CompileOption(_) => return true,
                FormIdx::DeprecatedAttribute(_) => return true,
                FormIdx::FeatureAttribute(_) => return true,
                FormIdx::ModuleDocAttribute(_) => {}
                FormIdx::ModuleDocMetadataAttribute(_) => {}
                FormIdx::DocAttribute(_) => {}
                FormIdx::DocMetadataAttribute(_) => {}
                FormIdx::FunctionClause(_) => {}
                FormIdx::PPDirective(_) => {}
                FormIdx::PPCondition(_) => {}
                FormIdx::TypeAlias(_) => {}
                FormIdx::Spec(_) => {}
                FormIdx::Record(_) => {}
                FormIdx::SsrDefinition(_) => {}
            }
        }

        cache.insert(file_id, false);
    }

    false
}

lazy_static! {
    /// Attribute names that can occur in a header file without
    /// regarding the file as being used.
    static ref NO_MARK_USED_ATTRIBUTES: FxHashSet<Name> =
        FxHashSet::from_iter([known::author, known::oncall]);
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix;

    #[track_caller]
    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::UnspecificInclude)
            .disable(DiagnosticCode::NoDialyzerAttribute);
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
%%^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0020: Unused file: foo.hrl
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
%%^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0020: Unused file: foo.hrl
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
%%^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0020: Unused file: foo.hrl
        "#,
        );
    }

    #[test]
    fn optimise_includes_used_include_with_type() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -type orddict(Key, Val) :: [{Key, Val}].
//- /src/bar1u.erl
  -module(bar1u).
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
%%^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0020: Unused file: foo.hrl
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

    #[test]
    fn used_for_is_record_2() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo(Payload) when is_record(Payload, rec) -> ok.

//- /src/header.hrl
-record(rec, {}).

//- /src/erlang.erl
-module(erlang).
-export([is_record/2]).
is_record(_Term,_RecordTag) -> false.
"#,
        )
    }

    #[test]
    fn used_for_is_record_3() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo(Payload) when is_record(Payload, rec, 0) -> ok.

//- /src/header.hrl
-record(rec, {}).

//- /src/erlang.erl
-module(erlang).
-export([is_record/3]).
is_record(_Term,_RecordTag, _Size) -> false.
"#,
        )
    }

    #[test]
    fn used_for_compile_attribute() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo() -> ok.

//- /src/header.hrl
-compile(export_all).

"#,
        )
    }

    #[test]
    fn used_for_dialyzer_attribute() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo() -> ok.

//- /src/header.hrl
-dialyzer({nowarn_function, delete_at/3}).

"#,
        )
    }

    #[test]
    fn used_for_broken_attribute() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo() -> ok.

//- /src/header.hrl
%% The following shows up as a wild attribute, which we regard as being used.
   -defin e(X, 1).
%%  ^^^^^ 💡 error: W0013: misspelled attribute, saw 'defin' but expected 'define'

-def ine(Y, 2).
"#,
        )
    }

    #[test]
    fn not_used_for_author_attribute() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
  -include("header.hrl").
%%^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0020: Unused file: header.hrl

foo() -> ok.

//- /src/header.hrl
-author("mary").
"#,
        )
    }

    #[test]
    fn fixes_unused_include() {
        check_fix(
            r#"
//- /src/main.erl
-module(main).
-incl~ude("header.hrl").
%%<^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Unused file: header.hrl

foo() -> ok.

//- /src/header.hrl
-oncall("mary").
"#,
            expect![[r#"
-module(main).

foo() -> ok.

"#]],
        )
    }

    #[test]
    fn record_in_macro() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

-define(RECORD_NAME, record_name).
foo(?RECORD_NAME) -> ok.

//- /src/header.hrl
-record(record_name, {field :: string()}).
"#,
        )
    }

    #[test]
    fn ct_hrl_exception() {
        check_diagnostics(
            r#"
//- /test/main_SUITE.erl
-module(main_SUITE).
-include_lib("common_test/include/ct.hrl").

//- /opt/lib/common_test-1.27.1/include/ct.hrl otp_app:/opt/lib/common_test-1.27.1
//- /src/header.hrl
"#,
        )
    }
}
