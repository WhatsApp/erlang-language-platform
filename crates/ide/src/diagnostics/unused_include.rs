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
use std::sync::Arc;
use std::sync::LazyLock;

use elp_ide_assists::helpers::extend_range;
use elp_ide_db::SearchScope;
use elp_ide_db::SymbolDefinition;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use elp_syntax::ast::AstNode;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::InFile;
use hir::IncludeAttribute;
use hir::MacroEnvironment;
use hir::MacroName;
use hir::Name;
use hir::NameArity;
use hir::Semantic;
use hir::db::DefDatabase;

use crate::Assist;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::fix;

static EXCLUDES: LazyLock<FxHashSet<SmolStr>> = LazyLock::new(|| {
    ["common_test/include/ct.hrl"]
        .iter()
        .map(SmolStr::new)
        .collect()
});

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
        ctx: &LinterContext,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let db = ctx.sema.db;
        let form_list = db.file_form_list(ctx.file_id);
        let source_file = db.parse(ctx.file_id);
        let scope = SearchScope::single_file(ctx.file_id, None);
        let mut searched = FxHashMap::default();
        let mut res = Vec::new();

        // The preprocessor pass over the module walks its includes in source
        // order, so `include_env` gives the macros in scope where each one is
        // expanded -- including the guards its predecessors have already
        // defined. An include with no entry was not reached at all.
        let env = db.project_macro_environment(ctx.file_id);
        let analysis = db.file_preprocessor_analysis(ctx.file_id, Arc::clone(&env));

        for (include_idx, attr) in form_list.includes() {
            if !EXCLUDES.contains(attr.path()) {
                let in_file = InFile::new(ctx.file_id, include_idx);
                let Some(include_env) = analysis.include_env(include_idx) else {
                    continue;
                };
                if let Some(include_file_id) =
                    db.resolve_include(db.app_data_id_by_file(ctx.file_id), in_file)
                {
                    if is_include_used(
                        ctx.sema,
                        db,
                        include_file_id,
                        Arc::clone(include_env),
                        ctx.file_id,
                        &scope,
                        &mut searched,
                    ) {
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
                        range: FileRange {
                            file_id: ctx.file_id,
                            range: attribute_range,
                        },
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
        _range: TextRange,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let file_id = ctx.file_id;
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

/// Identifies a definition reached through the module's includes. A definition
/// can sit in the closure of several includes, and the usage search is the
/// expensive part, so each is searched at most once per module.
#[derive(PartialEq, Eq, Hash)]
enum DefKey {
    Function(FileId, NameArity),
    Type(FileId, NameArity),
    Record(FileId, Name),
    Macro(FileId, MacroName),
}

/// Whether `include_file_id`, expanded under `env`, gives `target` a reason to
/// include it.
///
/// `def_map_with_env` has already done the work of expanding it the way the
/// preprocessor would: each nested include is followed under the environment
/// recorded for it, and each file's forms are filtered by the conditions that
/// environment settles. A header behind a guard the module has already
/// tripped therefore merges in empty, and contributes nothing here.
fn is_include_used(
    sema: &Semantic,
    db: &dyn DefDatabase,
    include_file_id: FileId,
    env: Arc<MacroEnvironment>,
    target: FileId,
    scope: &SearchScope,
    searched: &mut FxHashMap<DefKey, bool>,
) -> bool {
    let def_map = db.def_map_with_env(include_file_id, env);

    // Cheap first: forms that count by being present at all.
    if def_map.has_effectful_form {
        return true;
    }

    let target_def_map = db.def_map(target);
    for (name_arity, fun_def) in def_map.get_functions() {
        if target_def_map.is_function_exported(name_arity) {
            return true;
        }
        let key = DefKey::Function(fun_def.file.file_id, name_arity.clone());
        if is_used(sema, scope, searched, key, || {
            SymbolDefinition::Function(fun_def.clone())
        }) {
            return true;
        }
    }

    for (name_arity, type_def) in def_map.get_types() {
        let key = DefKey::Type(type_def.file.file_id, name_arity.clone());
        if is_used(sema, scope, searched, key, || {
            SymbolDefinition::Type(type_def.clone())
        }) {
            return true;
        }
    }

    for (name, record_def) in def_map.get_records() {
        let key = DefKey::Record(record_def.file.file_id, name.clone());
        if is_used(sema, scope, searched, key, || {
            SymbolDefinition::Record(record_def.clone())
        }) {
            return true;
        }
    }

    for (name, macro_def) in def_map.get_macros() {
        let key = DefKey::Macro(macro_def.file.file_id, name.clone());
        if is_used(sema, scope, searched, key, || {
            SymbolDefinition::Define(macro_def.clone())
        }) {
            return true;
        }
    }

    false
}

fn is_used(
    sema: &Semantic,
    scope: &SearchScope,
    searched: &mut FxHashMap<DefKey, bool>,
    key: DefKey,
    definition: impl FnOnce() -> SymbolDefinition,
) -> bool {
    if let Some(used) = searched.get(&key) {
        return *used;
    }
    let used = definition().usages(sema).set_scope(scope).at_least_one();
    searched.insert(key, used);
    used
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix;

    // The `-type` declarations in these header fixtures are incidental; keep the
    // tests focused on W0020.
    #[track_caller]
    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::AvoidTypeDefsInHeader)
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
    fn used_include_function_exported_by_module() {
        // Header defines functions that are exported by the including
        // module (not by the header itself).  This pattern is common
        // for behaviour-callback delegation headers.
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  foo() -> bar.
//- /src/foo.erl
  -module(foo).
  -export([foo/0]).
  -include("foo.hrl").
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
    fn used_transitively_and_included_directly() {
        // Walking `outer.hrl` passes through `inner.hrl` and establishes only
        // that `inner.hrl` declares nothing of its own. That is not an answer
        // for `inner.hrl` as an include in its own right, which `macros.hrl`
        // makes used.
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("outer.hrl").
-include("inner.hrl").

foo() -> ?MACRO.

//- /src/outer.hrl
-include("inner.hrl").

//- /src/inner.hrl
-include("macros.hrl").

//- /src/macros.hrl
-define(MACRO, 3).
"#,
        )
    }

    #[test]
    fn unused_alongside_a_transitively_used_include() {
        // The counterpart of the above: proving `outer.hrl` used must not
        // spill over onto an include that nothing refers to. `spare.hrl` sits
        // inside the closure the successful walk visits, so an answer recorded
        // for the whole of that closure rather than for its seed would reach
        // it and silence the warning below.
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("outer.hrl").
  -include("spare.hrl").
%%^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0020: Unused file: spare.hrl

foo() -> ?MACRO.

//- /src/outer.hrl
-include("macros.hrl").
-include("spare.hrl").

//- /src/macros.hrl
-define(MACRO, 3).

//- /src/spare.hrl
-define(SPARE, 4).
"#,
        )
    }

    #[test]
    fn conditional_on_a_macro_from_an_earlier_include() {
        // `cond.hrl` supplies the record only because `feature.hrl`, included
        // ahead of it, defined `?FEATURE`. Judging `cond.hrl` in isolation
        // would evaluate the `-ifdef` against a project environment where
        // `FEATURE` is undefined, lose the record, and report a header the
        // module cannot compile without.
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("feature.hrl").
-include("cond.hrl").

foo(X) -> {?FEATURE, X#r.a}.

//- /src/feature.hrl
-define(FEATURE, true).

//- /src/cond.hrl
-ifdef(FEATURE).
-record(r, {a}).
-endif.
"#,
        )
    }

    #[test]
    fn macro_read_only_by_another_headers_condition() {
        // Records current behaviour, which is wrong. `feature.hrl` is what
        // makes `cond.hrl` supply the record, so removing it costs the module
        // its `#r` definition -- but `?FEATURE` is read by a condition in
        // another header rather than by anything in `main`, and the usage
        // search is scoped to `main`. A macro consumed only by preprocessor
        // conditions therefore looks unused. Fixing this needs the search to
        // count condition reads, which is a change in its own right.
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
  -include("feature.hrl").
%%^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0020: Unused file: feature.hrl
-include("cond.hrl").

foo(X) -> X#r.a.

//- /src/feature.hrl
-define(FEATURE, true).

//- /src/cond.hrl
-ifdef(FEATURE).
-record(r, {a}).
-endif.
"#,
        )
    }

    #[test]
    fn guarded_include_already_in_scope() {
        // The `whatsapp/server` shape: `outer.hrl` reaches the record only
        // through `guarded.hrl`, whose guard `main` has already tripped via
        // `direct.hrl`. So `outer.hrl` expands to nothing that `main` uses and
        // is genuinely unused, while `direct.hrl` -- which is what actually
        // brings the record into scope -- is not.
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("direct.hrl").
  -include("outer.hrl").
%%^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0020: Unused file: outer.hrl

foo(X) -> X#rec.field.

//- /src/direct.hrl
-include("guarded.hrl").

//- /src/outer.hrl
-include("guarded.hrl").

//- /src/guarded.hrl
-ifndef(GUARDED_HRL).
-define(GUARDED_HRL, true).
-record(rec, {field}).
-endif.
"#,
        )
    }

    #[test]
    fn guarded_include_reached_first_is_used() {
        // The same shape with the two includes swapped: whichever include
        // brings the guarded header into scope first is the one that is used.
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("outer.hrl").
  -include("direct.hrl").
%%^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0020: Unused file: direct.hrl

foo(X) -> X#rec.field.

//- /src/direct.hrl
-include("guarded.hrl").

//- /src/outer.hrl
-include("guarded.hrl").

//- /src/guarded.hrl
-ifndef(GUARDED_HRL).
-define(GUARDED_HRL, true).
-record(rec, {field}).
-endif.
"#,
        )
    }

    #[test]
    fn unguarded_include_reached_twice_stays_used() {
        // Without a guard the second expansion really does re-supply the
        // record, so neither include may be reported. (Erlang rejects the
        // duplicate definition, but that is `L1252`'s business, not W0020's.)
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("direct.hrl").
-include("outer.hrl").

foo(X) -> X#rec.field.

//- /src/direct.hrl
-include("unguarded.hrl").

//- /src/outer.hrl
-include("unguarded.hrl").

//- /src/unguarded.hrl
-record(rec, {field}).
"#,
        )
    }

    #[test]
    fn shadowed_definition_does_not_speak_for_the_winner() {
        // `-undef` lets two active headers declare the same name, and the
        // usage search asks about a definition rather than a name: `?X`
        // resolves to `b.hrl`'s, so `a.hrl`'s is genuinely unreferenced. Only
        // `a.hrl` may be reported -- removing `b.hrl` would silently change
        // `?X` from 2 to 1.
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
  -include("a.hrl").
%%^^^^^^^^^^^^^^^^^^ 💡 warning: W0020: Unused file: a.hrl
-include("b.hrl").

foo() -> ?X.

//- /src/a.hrl
-define(X, 1).

//- /src/b.hrl
-undef(X).
-define(X, 2).
"#,
        )
    }

    #[test]
    fn two_headers_sharing_a_transitively_used_include() {
        // Both headers reach the macro through `mid.hrl`. Walking `a.hrl`
        // leaves `mid.hrl` recorded as declaring nothing itself, so pruning
        // `b.hrl`'s walk on that -- rather than on a settled answer for
        // `mid.hrl`'s whole closure -- would lose the macro and report
        // `b.hrl` as unused.
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-include("a.hrl").
-include("b.hrl").

foo() -> ?MACRO.

//- /src/a.hrl
-include("mid.hrl").

//- /src/b.hrl
-include("mid.hrl").

//- /src/mid.hrl
-include("macros.hrl").

//- /src/macros.hrl
-define(MACRO, 3).
"#,
        )
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
-dialyzer(no_return).

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
