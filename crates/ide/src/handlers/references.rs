/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This module implements a reference search.
//! First, the element at the cursor position must be either an `ast::Atom`
//! or `ast::Var`. We first try to resolve it as if it was a definition of a symbol
//! (e.g. module attribute), and if that fails, we try to treat it as a reference.
//! Either way, we obtain element's HIR.
//! After that, we collect files that might contain references and look
//! for text occurrences of the identifier. If there's an `ast::Name`
//! at the index that the match starts at and its tree parent is
//! resolved to the search element SymbolDefinition, we get a reference.

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::find_best_token;
use elp_ide_db::SymbolClass;
use elp_ide_db::SymbolDefinition;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use hir::Semantic;

use crate::FilePosition;
use crate::NavigationTarget;
use crate::ToNav;

#[derive(Debug, Clone)]
pub struct ReferenceSearchResult {
    pub declaration: NavigationTarget,
    pub references: FxHashMap<FileId, Vec<TextRange>>,
}

// Feature: Find All References
//
// Shows all references of the item at the cursor location
//
// |===
// | Editor  | Shortcut
//
// | VS Code | kbd:[Shift+Alt+F12]
// |===
pub(crate) fn find_all_refs(
    sema: &Semantic<'_>,
    position: FilePosition,
) -> Option<Vec<ReferenceSearchResult>> {
    let _p = profile::span("find_all_refs");
    let search = move |def: SymbolDefinition| {
        let declaration = def.to_nav(sema.db);
        let usages = match def {
            SymbolDefinition::Function(_) => def.usages(sema).direct_only().all(),
            _ => def.usages(sema).all(),
        };

        let references = usages
            .into_iter()
            .map(|(file_id, refs)| {
                (
                    file_id,
                    refs.into_iter()
                        .map(|name| name.syntax().text_range())
                        .collect(),
                )
            })
            .collect();

        ReferenceSearchResult {
            declaration,
            references,
        }
    };

    let token = find_best_token(sema, position)?;

    match SymbolClass::classify(sema, token)? {
        SymbolClass::Definition(def) => Some(vec![search(def)]),
        SymbolClass::Reference { refs, typ: _ } => Some(refs.iter().map(search).collect()),
    }
}

#[cfg(test)]
mod tests {
    use elp_ide_db::elp_base_db::FileRange;

    use crate::fixture;
    use crate::tests::check_file_ranges;

    fn check(fixture: &str) {
        let (analysis, pos, _diagnostics_enabled, _guard, mut annos) =
            fixture::annotations(fixture);
        if let Ok(Some(resolved)) = analysis.find_all_refs(pos) {
            for res in resolved {
                let def_name = match annos
                    .iter()
                    .position(|(range, _)| range == &res.declaration.file_range())
                {
                    Some(idx) => annos.remove(idx).1,
                    None => panic!(
                        "definition not found for:\n{:#?}\nsearching:\n{:#?}",
                        res, annos
                    ),
                };
                let key = def_name
                    .strip_prefix("def")
                    .expect("malformed definition key");

                let expected = take_by(&mut annos, |(_, name)| name == key);
                let found_ranges = res
                    .references
                    .into_iter()
                    .flat_map(|(file_id, ranges)| {
                        ranges
                            .into_iter()
                            .map(move |range| FileRange { file_id, range })
                    })
                    .collect();
                check_file_ranges(found_ranges, expected)
            }
        }
        assert!(annos.is_empty());

        fn take_by<T>(vec: &mut Vec<T>, by: impl Fn(&T) -> bool) -> Vec<T> {
            let mut res = vec![];
            let mut i = 0;

            while i < vec.len() {
                if by(&vec[i]) {
                    let found = vec.swap_remove(i);
                    res.push(found);
                } else {
                    i += 1;
                }
            }

            res
        }
    }

    #[test]
    fn test_module_expr() {
        check(
            r#"
//- /src/main.erl
  -module(main).
%%^^^^^^^^^^^^^^def

main() -> main~.
%%        ^^^^
"#,
        );
    }

    #[test]
    fn test_module_type() {
        check(
            r#"
//- /src/main.erl
  -module(main).
%%^^^^^^^^^^^^^^def

-type main() :: main~.
%%              ^^^^
"#,
        );
    }

    #[test]
    fn test_module_from_definition() {
        check(
            r#"
//- /src/main.erl
  -module(main~).
%%^^^^^^^^^^^^^^def

//- /src/type.erl
-type main() :: main.
%%              ^^^^

//- /src/function.erl
main() -> main.
%%        ^^^^
"#,
        );
    }

    #[test]
    fn test_module_across_files() {
        check(
            r#"
//- /src/another.erl
  -module(another).
%%^^^^^^^^^^^^^^^^^def

//- /src/main.erl
foo() -> another~.
%%       ^^^^^^^

//- /src/third.erl
foo() -> another.
%%       ^^^^^^^
"#,
        );
    }

    #[test]
    fn test_type() {
        check(
            r#"
//- /src/main.erl
-type foo~() :: foo.
%%    ^^^^^def

-type bar() :: {foo(), baz(), foo(integer())}.
%%              ^^^
"#,
        );

        check(
            r#"
//- /src/main.erl
-type foo() :: foo.
%%    ^^^^^def

-type bar() :: {foo~(), baz(), foo(integer())}.
%%              ^^^
"#,
        );

        check(
            r#"
//- /src/main.erl
-export_type([foo/0]).
%%            ^^^

-type foo() :: foo.
%%    ^^^^^def

-type bar() :: foo~().
%%             ^^^

//- /src/another.erl
-type bar() :: {main:foo(), baz(), main:foo(integer())}.
%%                   ^^^
"#,
        )
    }

    #[test]
    fn test_record() {
        check(
            r#"
//- /src/main.erl
-record(foo~, {}).
%%      ^^^def

-type foo() :: #foo{}.
%%              ^^^


foo(#foo{}) ->
%%   ^^^
    #foo{},
%%   ^^^
    Var#foo{},
%%      ^^^
    Var#foo.field,
%%      ^^^
    #foo.field.
%%   ^^^
"#,
        );

        check(
            r#"
//- /src/main.erl
-record(foo, {}).
%%      ^^^def

-type foo() :: #foo~{}.
%%              ^^^

foo() -> #foo{}.
%%        ^^^
"#,
        );

        check(
            r#"
//- /src/main.hrl
-record(foo, {}).
%%      ^^^def

//- /src/main.erl
-include("main.hrl").
-type foo() :: #foo~{}.
%%              ^^^

//- /src/another.erl
-include("main.hrl").

foo() -> #foo{}.
%%        ^^^

//- /src/different_record.erl
-record(foo, {}).

should_not_match() -> #foo{}.
"#,
        );
    }

    #[test]
    fn test_record_transitive() {
        check(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo() -> #r~ec{}.
%%        ^^^

//- /src/other.erl
-module(other).
-include("header.hrl").

bar() -> #rec{}.
%%        ^^^


//- /src/header.hrl
-include("header_deep.hrl").

//- /src/header_deep.hrl
-record(rec, {}).
%%      ^^^def
"#,
        );
    }

    #[test]
    fn test_record_field() {
        check(
            r#"
//- /src/main.erl
-record(foo, {a, b~}).
%%               ^def

-type foo() :: #foo{b :: integer()}.
%%                  ^

foo(#foo{a = _, b = _}) ->
%%              ^
    #foo{a = 1, b = 2},
%%              ^
    Var#foo{b = 3},
%%          ^
    Var#foo.b,
%%          ^
    #foo.b.
%%       ^
"#,
        );

        check(
            r#"
//- /src/main.erl
-record(foo, {a}).
%%            ^def

-type foo() :: #foo{a = 1}.
%%                  ^

foo() -> #foo{a~ = 2}.
%%            ^
"#,
        );

        check(
            r#"
//- /src/main.hrl
-record(foo, {a}).
%%            ^def

//- /src/main.erl
-include("main.hrl").
-type foo() :: #foo{a~ :: integer()}.
%%                  ^

//- /src/another.erl
-include("main.hrl").

foo() -> #foo{a = 1}.
%%            ^

//- /src/different_record.erl
-record(foo, {a}).

should_not_match() -> #foo{a = 1}.
"#,
        );
    }

    #[test]
    fn test_function() {
        check(
            r#"
//- /src/main.erl

-export([foo/0]).

  foo~() -> ok.
%%^^^def

bar() -> foo().
%%       ^^^
baz() -> foo(1).

//- /src/another.erl

-import(main, [foo/0]).
"#,
        );

        check(
            r#"
//- /src/main.erl

-export([foo/0]).

  foo() -> ok.
%%^^^def

bar() -> foo().
%%       ^^^
baz() -> foo(1).

//- /src/another.erl

-import(main, [foo/0]).

baz() -> main:foo~().
%%            ^^^
"#,
        );

        check(
            r#"
-export([foo/0]).

-spec foo~() -> ok.
  foo() -> ok.
%%^^^def
"#,
        );

        // Finding reference works from any clause,
        // the first clause is considered the "definition"
        check(
            r#"
-export([foo/1]).

  foo~(1) -> ok;
%%^^^def
foo(2) -> error.
"#,
        );

        check(
            r#"
-export([foo/1]).

  foo(1) -> ok;
%%^^^def
foo~(2) -> error.
"#,
        );
    }

    #[test]
    fn test_functions_import_1() {
        check(
            r#"
//- /foo/src/main.erl app:foo
-module(main).
-import(another, [baz/1]).
foo() ->
    ba~z(3).
%%  ^^^
//- /foo/src/another.erl app:foo
   -module(another).
   -export([baz/1]).
   baz(0) -> zero.
%% ^^^def
"#,
        );
    }

    #[test]
    fn test_functions_import_2() {
        check(
            r#"
//- /foo/src/main.erl app:foo
-module(main).
foo() -> another:imp~orted().

//- /foo/src/another.erl app:foo
   -module(another).
   -import(baz, [imported/0]).

//- /foo/src/baz.erl app:foo
   -module(baz).

   -export([imported/0]).

   imported() -> ok.

"#,
        );
    }

    #[test]
    fn test_macro() {
        check(
            r#"
//- /src/main.erl
-define(FOO~(X), X).
%%      ^^^^^^def

-type foo() :: ?FOO(integer()).
%%              ^^^

foo(?FOO(_), FOO) -> FOO, ?FOO(1).
%%   ^^^
%%                         ^^^

-ifdef(FOO).
%%     ^^^
-ifndef(FOO).
%%      ^^^
"#,
        );

        check(
            r#"
//- /src/main.hrl
-define(FOO, 1).
%%      ^^^def

//- /src/main.erl
-include("main.hrl").

foo() -> ?FOO~.
%%        ^^^

//- /src/another.erl
-include("main.hrl").

-type foo() :: ?FOO.
%%              ^^^

//- /src/no_include.erl
foo() -> ?FOO.

//- /src/another_macro.erl
-define(FOO, 2).

foo() -> ?FOO.
"#,
        );
    }

    #[test]
    fn test_var() {
        check(
            r#"
foo(Var~) ->
%%  ^^^def
    Var.
%%  ^^^
"#,
        );

        check(
            r#"
foo(Var) ->
%%  ^^^def
    Var~,
%%  ^^^
    Var.
%%  ^^^
"#,
        );

        check(
            r#"
foo() ->
    case {} of
        Var -> ok;
%%      ^^^def1
        _ ->
            Var = 1
%%          ^^^def2
    end,
    Var~.
%%  ^^^1
%%  ^^^2
"#,
        );
    }

    #[test]
    fn test_callback() {
        check(
            r#"
-callback foo~(integer()) -> ok.
%%        ^^^def
-callback foo() -> ok.

-optional_callbacks([foo/1]).
%%                   ^^^
-optional_callbacks([foo/0]).
"#,
        );
    }

    #[test]
    fn test_headers() {
        check(
            r#"
//- /foo/include/main.hrl app:foo include_path:/foo/include
%% ^file def
//- /foo/src/main.erl app:foo
-include("main.hrl~").
%%       ^^^^^^^^^^
//- /bar/src/another.erl app:bar
-include_lib("foo/include/main.hrl").
%%           ^^^^^^^^^^^^^^^^^^^^^^
"#,
        );
    }

    #[test]
    fn test_functions_only() {
        // T137651044: Reference results for function should only include functions
        check(
            r#"
//- /foo/src/main.erl app:foo
-module(main).
-import(another, [baz/1]).
foo() ->
    baz(3),
%%  ^^^
    another:b~az(5).
%%          ^^^
//- /foo/src/another.erl app:foo
   -module(another).
   -export([baz/1]).
   -spec baz(any()) -> any().
   baz(0) -> zero;
%% ^^^def
   baz(_) -> ok.

   other() -> baz(2).
%%            ^^^
"#,
        );
    }
}
