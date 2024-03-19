/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::find_best_token;
use elp_ide_db::RootDatabase;
use elp_ide_db::SymbolClass;
use hir::Semantic;

use crate::navigation_target::NavigationTarget;
use crate::navigation_target::ToNav;
use crate::RangeInfo;

pub(crate) fn goto_definition(
    db: &RootDatabase,
    position: FilePosition,
) -> Option<RangeInfo<Vec<NavigationTarget>>> {
    let sema = Semantic::new(db);
    let token = find_best_token(&sema, position)?;
    let targets = SymbolClass::classify(&sema, token.clone())?
        .iter()
        .map(|def| def.to_nav(db))
        .collect();
    Some(RangeInfo::new(token.value.text_range(), targets))
}

#[cfg(test)]
mod tests {
    use crate::fixture;
    use crate::tests::check_navs;
    use crate::tests::check_no_parse_errors;

    #[track_caller]
    fn check_expect_parse_error(fixture: &str) {
        check_worker(fixture, false)
    }

    #[track_caller]
    fn check(fixture: &str) {
        check_worker(fixture, true)
    }

    #[track_caller]
    fn check_worker(fixture: &str, check_parse_error: bool) {
        let (analysis, position, _diagnostics_enabled, _guard, expected) =
            fixture::annotations(fixture);
        if check_parse_error {
            check_no_parse_errors(&analysis, position.file_id);
        }

        let navs = analysis
            .goto_definition(position)
            .unwrap()
            .expect("no definition found")
            .info;

        if navs.is_empty() {
            panic!("got some with empty navs!");
        }

        check_navs(navs, expected);
    }

    fn check_unresolved(fixture: &str) {
        let (analysis, position, _) = fixture::position(fixture);
        check_no_parse_errors(&analysis, position.file_id);

        match analysis.goto_definition(position).unwrap() {
            Some(navs) if !navs.info.is_empty() => {
                panic!("didn't expect this to resolve anywhere: {:?}", navs)
            }
            Some(_) => {
                panic!("got some with empty navs!");
            }
            None => {}
        }
    }

    #[test]
    fn module_name() {
        check(
            r#"
//- /src/main.erl
-module(main).

foo() -> anoth~er.

//- /src/another.erl
  -module(another).
%%^^^^^^^^^^^^^^^^^
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

foo(anoth~er) -> ok.

//- /src/another.erl
  -module(another).
%%^^^^^^^^^^^^^^^^^
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-type x() :: anoth~er.

//- /src/another.erl
  -module(another).
%%^^^^^^^^^^^^^^^^^
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-compile({parse_transform, anoth~er}).

//- /src/another.erl
  -module(another).
%%^^^^^^^^^^^^^^^^^
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-attr(anoth~er).

//- /src/another.erl
  -module(another).
%%^^^^^^^^^^^^^^^^^
"#,
        );
    }

    #[test]
    fn module_name_inside_otp() {
        check(
            r#"
//- /src/main.erl
-module(main).

foo() -> l~ists.

//- /opt/lib/stdlib-3.17/src/lists.erl otp_app:/opt/lib/stdlib-3.17
  -module(lists).
%%^^^^^^^^^^^^^^^
"#,
        );
    }

    #[test]
    fn module_name_no_module_attr() {
        check(
            r#"
//- /src/main.erl
-module(main).

foo() -> anoth~er.

//- /src/another.erl
%%^file
foo(1) -> ok.
bar(2) -> ok.
"#,
        )
    }

    #[test]
    fn module_name_unresolved() {
        check_unresolved(
            r#"
//- /src/main.erl
-module(main).

foo() -> anoth~er.
"#,
        )
    }

    #[test]
    fn local_call() {
        check(
            r#"
//- /src/main.erl
-module(main).

foo() -> b~ar().

  bar() -> ok.
%%^^^
"#,
        )
    }

    #[test]
    fn local_call_from_record_def() {
        check(
            r#"
//- /src/main.erl
-module(main).

-record(test, {init = i~nit() :: atom()}).

  init() -> #test.init.
%%^^^^
"#,
        )
    }

    #[test]
    fn local_call_to_header() {
        check(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo() -> b~ar().

//- /src/header.hrl
  bar() -> ok.
%%^^^
"#,
        )
    }

    #[test]
    fn local_call_to_inner_header() {
        check(
            r#"
//- /src/main.erl
-module(main).
-include("header1.hrl").
foo() -> b~ar().

//- /src/header1.hrl
-include("header2.hrl").

//- /src/header2.hrl
  bar() -> ok.
%%^^^
"#,
        )
    }

    #[test]
    fn cyclic_header() {
        check(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").
foo() -> b~ar().

//- /src/header.hrl
-include("header.hrl").

  bar() -> ok.
%%^^^
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).
-include("header1.hrl").
foo() -> b~ar().

//- /src/header1.hrl
-include("header2.hrl").

  bar() -> ok.
%%^^^

//- /src/header2.hrl
-include("header1.hrl").
"#,
        );

        check_unresolved(
            r#"
//- /src/main.erl
-module(main).
-include("header1.hrl").
foo() -> b~ar().
baz() -> ko().

//- /src/header1.hrl
-include("header2.hrl").
  ko() -> err.

//- /src/header2.hrl
-include("header1.hrl").

  bar() -> ok.
"#,
        );
    }

    #[test]
    fn local_call_erlang_function() {
        check(
            r#"
//- /src/main.erl
-module(main).

foo() ->
  Pid = se~lf(),
  Pid.

//- /src/erlang.erl
  -module(erlang).
  self() -> ok.
%%^^^^
  spawn(F) -> {F}.
"#,
        )
    }

    #[test]
    fn local_call_imported_fun() {
        check(
            r#"
//- /src/main.erl
  -module(main).
  -include("blah.hrl").

  -import(foo, [bar/0]).

  baz() -> b~ar(),?FOO.

//- /src/foo.erl
  -module(foo).
  -export([bar/0]).
  bar() -> ok.
%%^^^
//- /src/blah.hrl
-define(FOO,ok).
"#,
        )
    }

    #[test]
    fn local_type_alias() {
        // BinaryOpExpr
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type bar() :: f~oo() + atom().
"#,
        );

        // Call
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type bar() :: f~oo().
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-opaque bar() :: f~oo().
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type id(X) :: X.
-type bar() :: id(f~oo()).
"#,
        );

        // MapExpr
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-opaque bar() :: #{atom() => f~oo()}.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type id(X) :: X.
-type bar() :: #{id(f~oo()) => atom()} | [].
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type id(X) :: X.
-type bar() :: #{id(f~oo()) => foo()}.
"#,
        );

        // Pipe
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-opaque bar() :: binary() | f~oo().
"#,
        );

        // RangeType
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: 1.
%%    ^^^^^
-type bar() :: f~oo() .. 3.
"#,
        );

        // Record
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-record(rec, {foo}).
-type rec() :: #rec{foo :: f~oo()}.
"#,
        );

        // UnaryOpExpr
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type bar() :: - f~oo().
"#,
        );

        // FunType
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type bar() :: fun((f~oo()) -> foo()).
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type bar() :: fun((...) -> f~oo()).
"#,
        );

        // List
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type bar() :: [fun((f~oo()) -> foo())].
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type bar() :: [fun((f~oo()) -> foo()), ...].
"#,
        );

        // Paren
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type bar(A) :: ([fun((f~oo()) -> A), ...]).
"#,
        );

        // Tuple
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type bar(A) :: {f~oo(), A}.
"#,
        );
    }

    #[test]
    fn local_type_to_header() {
        check(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

-type bar() :: f~oo().

//- /src/header.hrl
-type foo() :: number().
%%    ^^^^^
"#,
        );
    }

    #[test]
    fn remote_type_alias() {
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: number().
%%    ^^^^^
-type bar() :: main:f~oo().
"#,
        );

        check(
            r#"
//- /src/mod1.erl
-module(mod1).
-type foo() :: number().
%%    ^^^^^

//- /src/mod2.erl
-module(mod2).
-type bar() :: mod1:f~oo().
"#,
        );

        check(
            r#"
//- /src/mod1.erl
-module(mod1).

-type foo() :: number().
%%    ^^^^^

//- /src/mod2.erl
-module(mod2).
-opaque bar() :: binary() | mod1:f~oo().
"#,
        );

        check(
            r#"
//- /src/mod1.erl
-module(mod1).

-type foo() :: number().
%%    ^^^^^

//- /src/mod2.erl
-module(mod2).
-opaque bar() :: #{atom() => mod1:f~oo()}.
"#,
        );
    }

    #[test]
    fn remote_opaque_to_header() {
        check(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

-type bar() :: main:f~oo().

//- /src/header.hrl
-type foo() :: number().
%%    ^^^^^
"#,
        );
    }

    #[test]
    fn local_opaque() {
        check(
            r#"
//- /src/main.erl
-module(main).

-opaque foo() :: number().
%%      ^^^^^
-type bar() :: f~oo().
"#,
        )
    }

    #[test]
    fn remote_opaque() {
        check(
            r#"
//- /src/mod1.erl
-module(mod1).
-opaque foo() :: number().
%%      ^^^^^

//- /src/mod2.erl
-module(mod2).
-type bar() :: mod1:f~oo().
"#,
        )
    }

    #[test]
    fn local_type_alias_from_record_def() {
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo(X) :: [X].
%%    ^^^^^^

-record(foo_container, {foo :: f~oo(ok)}).
main() -> #foo_container.foo.
"#,
        )
    }

    #[test]
    fn local_type_alias_from_spec() {
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo(X) :: [X].
%%    ^^^^^^

-spec bar(f~oo(atom())) -> foo(pid()).
bar(X) -> X.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-type foo(X) :: [X].
%%    ^^^^^^

-spec bar(foo(atom())) -> atom() | f~oo(pid()).
bar(X) -> X.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: [].
%%    ^^^^^

-spec bar() -> XX when XX :: f~oo().
bar() -> ok.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: [].
%%    ^^^^^

-spec bar(In :: f~oo()) -> Out :: foo().
bar(X) -> X.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: [].
%%    ^^^^^

-spec bar(In :: foo()) -> Out :: f~oo().
bar(X) -> X.
"#,
        );
    }

    #[test]
    fn local_type_alias_from_callback() {
        check(
            r#"
//- /src/main.erl
-module(main).

-type foo(X) :: [X].
%%    ^^^^^^

-callback bar(f~oo(atom())) -> foo(pid()).
"#,
        );
    }

    #[test]
    fn record_from_spec() {
        check(
            r#"
//- /src/main.erl
-module(main).

-record(foo, {}).
%%      ^^^

-spec bar(#f~oo{}) -> pid().
bar(X) -> X.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-record(foo, {}).
%%      ^^^

-spec bar(atom()) -> atom() | #f~oo{}.
bar(X) -> X.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-type foo() :: [].
%%    ^^^^^

-spec bar() -> XX when XX :: f~oo().
bar() -> ok.
"#,
        );
    }

    #[test]
    fn remote_call() {
        check(
            r#"
//- /src/main.erl
-module(main).

foo() -> main:b~ar().

  bar() -> ok.
%%^^^
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

foo() -> another:b~ar().

//- /src/another.erl
-module(another).
  bar() -> ok.
%%^^^
"#,
        );

        check_unresolved(
            r#"
//- /src/main.erl
-module(main).

foo() -> Another:b~ar().
"#,
        )
    }

    #[test]
    fn remote_call_to_header() {
        check(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo() -> main:b~ar().

//- /src/header.hrl
  bar() -> ok.
%%^^^
"#,
        );
    }

    #[test]
    fn remote_call_module() {
        check(
            r#"
//- /src/main.erl
-module(main).

foo() -> a~nother:bar().

//- /src/another.erl
  -module(another).
%%^^^^^^^^^^^^^^^^^
"#,
        )
    }

    #[test]
    fn behaviour_attribute() {
        check(
            r#"
//- /src/main.erl
-module(main).
-behaviour(a~nother).

//- /src/another.erl
  -module(another).
%%^^^^^^^^^^^^^^^^^
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).
-behavior(a~nother).

//- /src/another.erl
  -module(another).
%%^^^^^^^^^^^^^^^^^
"#,
        )
    }

    #[test]
    fn import_attribute() {
        check(
            r#"
//- /src/main.erl
-module(main).

-import(a~nother, []).

//- /src/another.erl
  -module(another).
%%^^^^^^^^^^^^^^^^^
"#,
        )
    }

    #[test]
    fn internal_fun() {
        check(
            r#"
//- /src/main.erl
-module(main).

foo() -> fun a~nother/0.

  another() -> ok.
%%^^^^^^^
"#,
        )
    }

    #[test]
    fn external_fun() {
        check(
            r#"
//- /src/main.erl
-module(main).

foo() -> fun another:f~oo/0.

//- /src/another.erl
-module(another).

  foo() -> ok.
%%^^^
"#,
        )
    }

    #[test]
    fn external_fun_module() {
        check(
            r#"
//- /src/main.erl
-module(main).

foo() -> fun a~nother:foo/0.

//- /src/another.erl
  -module(another).
%%^^^^^^^^^^^^^^^^^
"#,
        )
    }

    #[test]
    fn spec() {
        check(
            r#"
//- /src/main.erl
-module(main).

-spec f~oo() -> ok.
  foo() -> ok.
%%^^^
"#,
        )
    }

    #[test]
    fn record_name() {
        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {}).
%%      ^^^

foo() -> #r~ec{}.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {}).
%%      ^^^

foo() -> Expr#r~ec{}.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {}).
%%      ^^^

foo() -> Expr#r~ec.field.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {}).
%%      ^^^

foo() -> #r~ec.field.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {}).
%%      ^^^

foo(#r~ec{}) -> ok.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {}).
%%      ^^^

foo(#r~ec.field) -> ok.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {}).
%%      ^^^

-type rec() :: #r~ec{}.
"#,
        );
    }

    #[test]
    fn record_name_to_header() {
        check(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo() -> #r~ec{}.

//- /src/header.hrl
-record(rec, {}).
%%      ^^^
"#,
        );
    }

    #[test]
    fn is_record_2() {
        check(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo(Payload) when is_record(Payload, re~c) -> ok.

//- /src/header.hrl
-record(rec, {}).
%%      ^^^

//- /src/erlang.erl
-module(erlang).
is_record(_Term,_RecordTag) -> false.
"#,
        );
    }

    #[test]
    fn not_is_record_2() {
        check_unresolved(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {}).
foo(Payload) when bar_is_record(Payload, re~c) -> ok.

//- /src/erlang.erl
-module(erlang).
is_record(_Term,_RecordTag) -> false.
"#,
        );
    }

    #[test]
    fn is_record_3_matches() {
        check(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo(Payload) when is_record(Payload, re~c, 1) -> ok.

//- /src/header.hrl
-record(rec, {bar}).
%%      ^^^

//- /src/erlang.erl
-module(erlang).
is_record(_Term,_RecordTag, _Size) -> false.
"#,
        );
    }

    #[test]
    fn is_record_3_no_match() {
        check_unresolved(
            r#"
//- /src/main.erl
-module(main).
-record(rec, {bar}).
foo(Payload) when is_record(Payload, re~c, 2) -> ok.
baz() -> #rec.bar.

//- /src/erlang.erl
-module(erlang).
is_record(_Term,_RecordTag, _Size) -> false.
"#,
        );
    }

    #[test]
    fn record_field_name() {
        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {field}).
%%            ^^^^^

foo() -> Expr#rec.f~ield.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {field}).
%%            ^^^^^

foo() -> #rec.f~ield.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {field}).
%%            ^^^^^

foo(#rec.f~ield) -> ok.
"#,
        );
    }

    #[test]
    fn record_field() {
        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {field}).
%%            ^^^^^

foo() -> #rec{f~ield = ok}.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {field}).
%%            ^^^^^

foo(Expr) -> Expr#rec{f~ield = ok}.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {field}).
%%            ^^^^^

foo(#rec{f~ield = ok}) -> ok.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-record(rec, {field}).
%%            ^^^^^

-type rec() :: #rec{f~ield :: ok}.
"#,
        );
    }

    #[test]
    fn record_field_to_header() {
        check(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

foo() -> #rec{field1 = 1, f~ield3 = ok, field2 = ""}.

//- /src/header.hrl
-record(rec, {field1, field2, field3}).
%%                            ^^^^^^
"#,
        );
    }

    #[test]
    fn export_entry() {
        check(
            r#"
//- /src/main.erl
-module(main).

-export([f~oo/1]).

  foo(_) -> ok.
%%^^^
"#,
        )
    }

    #[test]
    fn import_entry() {
        check(
            r#"
//- /src/main.erl
-module(main).

-import(another, [f~oo/1]).

//- /src/another.erl
-module(another).

  foo(_) -> ok.
%%^^^
"#,
        )
    }

    #[test]
    fn export_type_entry() {
        check(
            r#"
//- /src/main.erl
-module(main).

-export_type([f~oo/1]).

-type foo(_) :: ok.
%%    ^^^^^^
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-export_type([f~oo/1]).

-opaque foo(_) :: ok.
%%      ^^^^^^
"#,
        );
    }

    #[test]
    fn optional_callbacks_entry() {
        check(
            r#"
//- /src/main.erl
-module(main).

-optional_callbacks([f~oo/1]).

-callback foo(integer()) -> integer().
%%        ^^^
"#,
        );
    }

    #[test]
    fn optional_callbacks_entry_to_header() {
        check(
            r#"
//- /src/main.erl
-module(main).
-include("header.hrl").

-optional_callbacks([f~oo/1]).

//- /src/header.hrl
-callback foo(integer()) -> integer().
%%        ^^^
"#,
        );
    }

    #[test]
    fn macro_call() {
        check(
            r#"
//- /src/main.erl
-module(main).

-define(FOO, 1).
%%      ^^^

foo() -> ?F~OO.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-define(FOO(), 1).
%%      ^^^^^

foo(?F~OO()) -> ok.
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-define(FOO(X), X).
%%      ^^^^^^

-type foo() :: ?F~OO(integer()).
"#,
        );
    }

    #[test]
    fn macro_name() {
        check(
            r#"
//- /src/main.erl
-module(main).

-define(FOO, 1).
%%      ^^^

-ifdef(F~OO).
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-define(FOO(), 1).
%%      ^^^^^

-ifndef(F~OO).
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-define(FOO(X), X).
%%      ^^^^^^

-undef(F~OO).
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-define(foo, 1).
%%      ^^^
-define(foo(), 2).
%%      ^^^^^
-define(foo(X), 3).
%%      ^^^^^^

-ifndef(f~oo).
"#,
        );
    }

    #[test]
    fn macro_undef() {
        check_unresolved(
            r#"
//- /src/main.erl
-module(main).

-define(FOO, 1).

-undef(FOO).

foo() -> ?F~OO.
"#,
        );

        check_unresolved(
            r#"
//- /src/main.erl
-module(main).

-define(foo, 1).
-define(foo(), 2).

-undef(foo).

foo() -> ?f~oo().
"#,
        );
    }

    #[test]
    fn macro_in_header() {
        check(
            r#"
//- /src/include.hrl

-define(FOO, 1).
%%      ^^^

//- /src/main.erl
-module(main).

-include("include.hrl").

foo() -> ?F~OO.
"#,
        );

        check(
            r#"
//- /src/include.hrl

-define(FOO(X), X).
%%      ^^^^^^

//- /src/main.erl
-module(main).

-include("include.hrl").

foo() -> ?F~OO(1).
"#,
        );
    }

    #[test]
    fn include() {
        check(
            r#"
//- /src/main.erl
-module(main).

-include("h~eader.hrl").
//- /src/header.hrl
%% ^file
-import(lists, [all/2]).
"#,
        );

        check(
            r#"
//- /src/main.erl include_path:/include
-module(main).

-include("h~eader.hrl").
//- /include/header.hrl
%% ^file
-import(lists, [all/2]).
"#,
        );
    }

    #[test]
    fn include_lib() {
        check(
            r#"
//- /main/src/main.erl app:main
-module(main).

-include_lib("a~nother/include/header.hrl").
//- /another-app/include/header.hrl app:another
%% ^file
-import(lists, [all/2]).
"#,
        );
    }

    #[test]
    fn var() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

foo(Var) -> V~ar.
%%  ^^^
"#,
        );

        check(
            r#"
//- /main/src/main.erl
-module(main).

foo() ->
   Var = 1,
%% ^^^
   V~ar.
"#,
        );
    }

    #[test]
    fn var_self() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

foo() ->
   V~ar = 1,
%% ^^^
   Var.
"#,
        );
    }

    #[test]
    fn case() {
        check_expect_parse_error(
            r#"
//- /main/src/main.erl
-module(main).

bar(X) ->
   Var = 3,
%% ^^^
   case X of
     V~ar -> none
   end.
"#,
        );

        check_expect_parse_error(
            r#"
//- /main/src/main.erl
-module(main).

bar(X) ->
   Var = 3,
%% ^^^
   case X of
     {V~ar, 6} -> none
   end.
"#,
        );
    }

    #[test]
    fn case2() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(X) ->
   case X of
     {Var, 5} -> Var;
     {Var, 6} -> V~ar
%%    ^^^
   end.
"#,
        );
    }

    #[test]
    fn case3() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(X) ->
   case X of
     {Var, 5} -> Var;
     {V~ar, 6} -> Var
%%    ^^^
   end.
"#,
        );
    }

    #[test]
    fn case4() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(XX) ->
    case XX of
        1 -> ZZ = 1, YY = 0, g(YY);
%%           ^^
        2 -> ZZ = 2
%%           ^^
    end,
    Z~Z.
"#,
        );
    }

    #[test]
    fn case5() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(XX) ->
    case XX of
        1 -> ZZ = 1, YY = 0;
%%                   ^^
        2 -> ZZ = 2
    end,
    g(ZZ),
    Y~Y.
"#,
        );
    }

    #[test]
    fn case6() {
        check_unresolved(
            r#"
//- /main/src/main.erl
-module(main).

bar(XX) ->
    case XX of
        1 -> ZZ = 1, g(ZZ);
        2 -> Z~Z
    end.
"#,
        );
    }

    #[test]
    fn anonymous_fun_1() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar() ->
    F = fun
          (X, true) -> XX = X+1, X~X;
%%                     ^^
          (X, _)    -> XX = X+1, XX
        end,
    F(42, true).
"#,
        );
    }

    #[test]
    fn anonymous_fun_2() {
        check_expect_parse_error(
            r#"
//- /main/src/main.erl
-module(main).

bar() ->
    XX = 1,
%%  ^^
    F = fun
          (X, true) -> XX = X+1, X~X;
          (X, _)    -> XX = X+1, XX
        end,
    ok.
"#,
        );
    }

    #[test]
    fn binary_comprehension1() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(Bytes) ->
    Byte = 1,
    << B~yte || <<Byte>> <= Bytes, Byte >= 5>>,
%%               ^^^^
    Byte.
"#,
        );
    }

    #[test]
    fn binary_comprehension2() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(Bytes) ->
    By~te = 1,
%%  ^^^^
    << Byte || <<Byte>> <= Bytes, Byte >= 5>>,
    Byte.
"#,
        );
    }

    #[test]
    fn binary_comprehension3() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(Bytes) ->
    Byte = 1,
%%  ^^^^
    << Byte || <<Byte>> <= Bytes, Byte >= 5>>,
    By~te.
"#,
        );
    }

    #[test]
    fn binary_comprehension_chained() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(Bytes) ->
    BB = 1,
    << BB || <<Byte>> <= Bytes, <<BB>> <= By~te>>,
%%             ^^^^
    BB.
"#,
        );
    }

    #[test]
    fn list_comprehension1() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
    XX = 1,
    [XX || XX <- List, X~X >= 5],
%%         ^^
    XX.
"#,
        );
    }

    #[test]
    fn list_comprehension2() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
    X~X = 1,
%%  ^^
    [XX || XX <- List, XX >= 5],
    XX.
"#,
        );
    }

    #[test]
    fn tuple1() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
    XX = 1,
%%  ^^
    {X~X,List}.
"#,
        );
    }

    #[test]
    fn tuple2() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar() ->
    {XX = 1, 2},
%%   ^^
    X~X.
"#,
        );
    }

    #[test]
    fn list1() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
    XX = 1,
%%  ^^
    [2,X~X,4|List].
"#,
        );
    }

    #[test]
    fn list2() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(_List) ->
    XX = [1],
%%  ^^
    [2,XX,4|X~X].
"#,
        );
    }

    #[test]
    fn list3() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar() ->
    [2,XX = 1,4],
%%     ^^
    X~X.
"#,
        );
    }

    #[test]
    fn unary_op1() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(_List) ->
    XX = 1,
%%  ^^
    -X~X.
"#,
        );
    }

    #[test]
    fn record() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(_List) ->
    XX = 1,
%%  ^^
    #record{field = X~X}.
"#,
        );
    }

    #[test]
    fn record_update1() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
    XX = 1,
%%  ^^
    List#record{field = X~X}.
"#,
        );
    }

    #[test]
    fn record_update2() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
%%  ^^^^
    XX = 1,
    Li~st#record{field = XX}.
"#,
        );
    }

    #[test]
    fn record_field_expr() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
%%  ^^^^
    XX = 1,
    g(XX),
    Li~st#record.field.
"#,
        );
    }

    #[test]
    fn record_map1() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(_List) ->
    XX = 1,
%%  ^^
    #{foo => X~X}.
"#,
        );
    }

    #[test]
    fn record_map2() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(_List) ->
    XX = 1,
%%  ^^
    #{X~X => 1}.
"#,
        );
    }

    #[test]
    fn record_map_update1() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
    XX = 1,
%%  ^^
    List#{foo => X~X}.
"#,
        );
    }

    #[test]
    fn record_map_update2() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
    XX = 1,
%%  ^^
    List#{X~X => 1}.
"#,
        );
    }

    #[test]
    fn record_map_update3() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
%%  ^^^^
    XX = foo,
    Li~st#{XX => 1}.
"#,
        );
    }

    #[test]
    fn catch() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(_List) ->
    XX = foo,
%%  ^^
    catch X~X.
"#,
        );
    }

    #[test]
    fn binary1() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(_List) ->
    XX = 1,
%%  ^^
    <<2,X~X,4>>.
"#,
        );
    }

    #[test]
    fn binary2() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
    XX = 1,
%%  ^^
    <<2,List:X~X,4>>.
"#,
        );
    }

    #[test]
    fn binary3() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
    <<Size, Data:Si~ze>> = List.
%%    ^^^^
"#,
        );
    }

    #[test]
    fn binary4() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(List) ->
    <<Size, Data:Si~ze/binary>> = List.
%%    ^^^^
"#,
        );
    }

    #[test]
    fn call1() {
        check(
            r#"
//- /main/src/main.erl
-module(main).

bar(FunName) ->
%%  ^^^^^^^
    Fun~Name().
"#,
        );
    }

    #[test]
    fn call2() {
        check(
            r#"
//- /main/src/module.erl
    -module(module).
%%  ^^^^^^^^^^^^^^^^

bar(FunName) ->
    mo~dule:FunName().
"#,
        );
    }

    #[test]
    fn call3() {
        check(
            r#"
//- /main/src/module.erl
-module(module).

bar(FunName) ->
%%  ^^^^^^^
    module:Fun~Name().
"#,
        );
    }

    #[test]
    fn call4() {
        check(
            r#"
//- /main/src/module.erl
-module(module).

bar(ModName) ->
%%  ^^^^^^^
    Mod~Name:foo().
"#,
        );
    }

    #[test]
    fn call5() {
        check(
            r#"
//- /main/src/module.erl
-module(module).

bar(FunName) ->
%%  ^^^^^^^
    module:foo(Fun~Name).
"#,
        );
    }

    #[test]
    fn block1() {
        check(
            r#"
//- /main/src/module.erl
-module(module).

bar(FunName) ->
%%  ^^^^^^^
  begin
    foo(Fu~nName)
  end.
"#,
        );
    }

    #[test]
    fn block2() {
        check(
            r#"
//- /main/src/module.erl
-module(module).

bar() ->
  begin
    XX = 1
%%  ^^
  end,
  X~X.
"#,
        );
    }

    #[test]
    fn if1() {
        check(
            r#"
//- /main/src/module.erl
-module(module).

bar(XX) ->
%%  ^^
  if
    X~X -> ok;
    true -> not_ok
  end.
"#,
        );
    }

    #[test]
    fn if2() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
      if
        XX-> YY = 1, ZZ = 2, g(ZZ);
    %%       ^^
        true -> YY = 0
    %%          ^^
      end,
      Y~Y.
    "#,
        );
    }

    #[test]
    fn if3() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
      if
        XX-> YY = 1, ZZ = 2;
    %%               ^^
        true -> YY = 0
      end,
      foo(YY),
      Z~Z.
    "#,
        );
    }

    #[test]
    fn if4() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
    %%  ^^
      if
        X~X-> YY = 1, ZZ = 2, g(ZZ);
        true -> YY = 0
      end,
      YY.
    "#,
        );
    }

    #[test]
    fn receive1() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
    %%  ^^
      receive
        X~X-> YY = 1, ZZ = 2, g(ZZ);
        true -> YY = 0
      after TimeOut -> timeout
      end,
      YY.
    "#,
        );
    }

    #[test]
    fn receive2() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
      receive
        XX-> YY = 1, ZZ = 2, g(ZZ);
    %%       ^^
        true -> YY = 0
    %%          ^^
      after TimeOut -> timeout
      end,
      Y~Y.
    "#,
        );
    }

    #[test]
    fn receive3() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
      receive
        XX-> YY = 1, ZZ = 2;
    %%               ^^
        true -> YY = 0
      after TimeOut -> timeout
      end,
      g(YY),
      Z~Z.
    "#,
        );
    }

    #[test]
    fn receive4() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
      TimeOut = 45,
    %%^^^^^^^
      receive
        XX-> YY = 1, ZZ = 2;
        true -> YY = 0
      after Tim~eOut -> timeout
      end,
      g(YY),
      ZZ.
    "#,
        );
    }

    #[test]
    fn receive5() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
      TimeOut = 45,
      receive
        XX-> YY = 1, ZZ = 2;
    %%               ^^
        true -> YY = 0
      after TimeOut -> ZZ = 4
    %%                 ^^
      end,
      g(YY),
      Z~Z.
    "#,
        );
    }

    #[test]
    fn receive6() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar() ->
      receive
      after XX = 1 -> ok
      %%    ^^
      end,
      X~X.
    "#,
        );
    }

    #[test]
    fn try1() {
        check_expect_parse_error(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
    %%  ^^
        try 1, 2 of
            X~X -> ok
        catch
            YY when true -> ok;
            error:undef:Stack -> Stack
        after
            ok
        end.
    "#,
        );
    }

    #[test]
    fn try2() {
        check_expect_parse_error(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
        try 1, 2 of
            XX -> YY = 1, ok
    %%            ^^
        catch
            Cond when true -> ok;
            error:undef:Stack -> Stack
        after
            ok
        end,
        Y~Y.
    "#,
        );
    }

    #[test]
    fn try3() {
        check_expect_parse_error(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
        try 1, 2 of
            XX -> YY = 1, ok
        catch
            Cond when true -> ok;
        %%  ^^^^
            error:undef:Stack -> Stack
        after
            ok
        end,
        %% This will generate an error diagnostic for an unsafe usage, but we resolve anyway
        Co~nd.
    "#,
        );
    }

    #[test]
    fn try4() {
        check_expect_parse_error(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
        try 1, 2 of
            XX -> YY = 1, ok
        catch
            Cond when true -> ok;
            error:undef:Stack -> Sta~ck
        %%              ^^^^^
        after
            ok
        end,
        Cond.
    "#,
        );
    }

    #[test]
    fn try5() {
        check_expect_parse_error(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
        try 1, 2 of
            XX -> YY = 1, ok
        catch
            Cond when true -> ok;
            error:undef:Stack -> Stack
        %%              ^^^^^
        after
            ok
        end,
        %% This will generate an error diagnostic for an unsafe usage, but we resolve anyway
        St~ack.
    "#,
        );
    }

    #[test]
    fn try6() {
        check_expect_parse_error(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
        try 1, 2 of
            XX -> YY = 1, ok
        catch
            Cond when true -> ok;
            error:Undef:Stack -> Stack
        %%        ^^^^^
        after
            ok
        end,
        %% This will generate an error diagnostic for an unsafe usage, but we resolve anyway
        Und~ef.
    "#,
        );
    }

    #[test]
    fn try7() {
        check_expect_parse_error(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
        try 1, 2 of
            XX -> YY = 1, ok
        catch
            Cond when true -> ok;
            Error:Undef:Stack -> Stack
        %%  ^^^^^
        after
            ok
        end,
        %% This will generate an error diagnostic for an unsafe usage, but we resolve anyway
        Err~or.
    "#,
        );
    }

    #[test]
    fn try8() {
        check_expect_parse_error(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
    %%  ^^
        try 1, 2 of
            XX -> YY = 1, ok
        catch
            Cond when X~X -> ok;
            error:undef:Stack -> Stack
        after
            ok
        end,
        %% This will generate an error diagnostic for an unsafe usage, but we resolve anyway
        Stack.
    "#,
        );
    }

    #[test]
    fn try9() {
        check_expect_parse_error(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
        try ZZ = 1, 2 of
    %%      ^^
            XX -> YY = 1, ok
        catch
            Cond when XX -> ok;
            error:undef:Stack -> Stack
        after
            ok
        end,
        Z~Z.
    "#,
        );
    }

    #[test]
    fn capture_fun1() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
    %%  ^^
      YY = fun modu:fun/X~X,
      YY.
    "#,
        );
    }

    #[test]
    fn capture_fun2() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
    %%  ^^
      YY = fun modu:X~X/0,
      YY.
    "#,
        );
    }

    #[test]
    fn capture_fun3() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
    %%  ^^
      YY = fun X~X:foo/0,
      YY.
    "#,
        );
    }

    #[test]
    fn capture_fun4() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    bar(XX) ->
    %%  ^^
      YY = fun X~X/0,
      YY.
    "#,
        );
    }

    #[test]
    fn pat_match1() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    foo(AA = BB) ->
    %%  ^^
        A~A + BB.
    "#,
        );
    }

    #[test]
    fn pat_match2() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    foo(AA = BB) ->
    %%       ^^
        AA + B~B.
    "#,
        );
    }

    #[test]
    fn pat_list1() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    foo([AA, BB]) ->
    %%   ^^
        A~A + BB.
    "#,
        );
    }

    #[test]
    fn pat_list2() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    foo([AA | BB]) ->
      %%      ^^
        AA + B~B.
    "#,
        );
    }

    #[test]
    fn pat_unary_op1() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    foo(AA = not BB) ->
    %%           ^^
        AA + B~B.
    "#,
        );
    }

    #[test]
    fn pat_binary_op1() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    foo(AA andalso BB) ->
    %%  ^^
        A~A + BB.
    "#,
        );
    }

    #[test]
    fn pat_binary_op2() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    foo(AA andalso BB) ->
    %%             ^^
        AA + B~B.
    "#,
        );
    }

    #[test]
    fn pat_record1() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    foo(#record{field = AA}) ->
    %%                  ^^
        A~A.
    "#,
        );
    }

    #[test]
    fn pat_map1() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    foo(#{foo := BB}) ->
    %%           ^^
        [AA] = B~B.
    "#,
        );
    }

    #[test]
    fn macro_module_name() {
        check(
            r#"
//- /src/main.erl
-module(main).

   foo() ->
%% ^^^
    ?MODULE:f~oo().
"#,
        );

        check(
            r#"
//- /src/main.erl
-module(main).

-define(ANOTHER, another).
foo() ->
    ?ANOTHER:f~oo() .

//- /src/another.erl
-module(another).

   foo() -> ok.
%% ^^^
    "#,
        );
    }

    #[test]
    fn anonymous_fun_as_variable_1() {
        check_expect_parse_error(
            r#"
    //- /main/src/module.erl
    -module(module).

    main(_) ->
        fun FF() ->
    %%      ^^
            case rand:uniform(2) of
                1 -> F~F();
                _ -> ok
            end
        end().
    "#,
        );
    }

    #[test]
    fn anonymous_fun_as_variable_2() {
        check_expect_parse_error(
            r#"
    //- /main/src/module.erl
    -module(module).

    main(_) ->
        fun FF() ->
    %%      ^^
            {_, _} = catch F~F = 3
        end().
    "#,
        );
    }

    #[test]
    fn test_class_variable() {
        check_expect_parse_error(
            r#"
//- /main/src/module.erl
-module(module).

main(_) ->
    Ty = error,
%%  ^^
    try ok
    catch
        T~y:_ -> ok
    end.
"#,
        )
    }

    #[test]
    fn variable_in_guard_1() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    foo(LowerBound, UpperBound)
    %%  ^^^^^^^^^^
        when LowerB~ound < UpperBound, is_integer(LowerBound), is_integer(UpperBound) ->
        Range = UpperBound - LowerBound,
        LowerBound + foo(Range).
    "#,
        );
    }

    #[test]
    fn single_clause_for_variable() {
        check(
            r#"
    //- /main/src/module.erl
    -module(module).

    foo(0) ->
        X~0 = 1,
    %%  ^^
        X0;
    foo(Y) ->
        [X0] = Y,
        X0.
    "#,
        );
    }

    #[test]
    fn define_func_include_file() {
        check(
            r#"
//- /include/foo.hrl include_path:/include
-define(enum, m:f).
%%      ^^^^
//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").

  bar() -> ?e~num(1, [a,b]).
        "#,
        );
    }

    // Unresolved with atom being an identifier and not "just" an expression
    // navigating to a module
    mod unresolved_atoms_with_special_meaning {
        use super::*;

        #[test]
        fn module_attribute() {
            check(
                r#"
//- /src/main.erl
  -module(ma~in).
%%^^^^^^^^^^^^^^

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn fa() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

-export([a~nother/1]).

//- /src/another.erl
  -module(another).
"#,
            );

            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

-import(x, [a~nother/1]).

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn type_name() {
            check(
                r#"
//- /src/main.erl
-module(main).

-type a~nother() :: _.
%%    ^^^^^^^^^

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn record_decl() {
            check(
                r#"
//- /src/main.erl
-module(main).

-record(a~nother, {}).
%%      ^^^^^^^

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn spec() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

-spec a~nother() -> _.

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn callback() {
            check(
                r#"
//- /src/main.erl
-module(main).

-callback a~nother() -> _.
%%        ^^^^^^^

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn attr_name() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

-a~nother(ok).

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn function_clause() {
            check(
                r#"
//- /src/main.erl
-module(main).

  a~nother() -> ok.
%%^^^^^^^
//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn bit_type_list() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

foo() -> <<1/a~nother>>.

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn record_name() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

foo() -> #a~nother{}.

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn record_field_name() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

foo() -> #x.a~nother.

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn record_field() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

foo() -> #x{a~nother = ok}.

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn external_fun() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

foo() -> fun foo:a~nother/0.

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn internal_fun() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

foo() -> fun a~nother/0.

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn try_class() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

foo() -> try ok catch a~nother:_ -> ok end.

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn macro_lhs() {
            check(
                r#"
//- /src/main.erl
-module(main).

-define(a~nother, 4).
%%      ^^^^^^^
foo() -> ?another.

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn macro_call_expr() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

foo() -> ?a~nother().

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn pp_undef() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

-undef(a~nother).

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn pp_ifdef() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

-ifdef(a~nother).

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn pp_ifndef() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

-ifndef(a~nother).

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn remote() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

foo() -> foo:a~nother().

//- /src/another.erl
  -module(another).
"#,
            )
        }

        #[test]
        fn call() {
            check_unresolved(
                r#"
//- /src/main.erl
-module(main).

foo() -> a~nother().

//- /src/another.erl
  -module(another).
"#,
            )
        }
    }

    mod maybe {
        use super::*;

        #[test]
        fn maybe() {
            check(
                r#"
//- /main/src/module.erl
-module(module).

foo() ->
    maybe
        {ok, AA} = ok_a(),
        %%   ^^
        {ok, BB} = ok_b(),
        {A~A, BB}
    end.
            "#,
            );
        }

        #[test]
        fn maybe_cond_match_simple() {
            check(
                r#"
//- /main/src/module.erl
-module(module).

foo() ->
    maybe
        {ok, AA} ?= ok_a(),
    %%       ^^
        {ok, BB} = ok_b(),
        {A~A, BB}
    end.
            "#,
            );
        }

        #[test]
        fn maybe_cond_match() {
            check(
                r#"
//- /main/src/module.erl
-module(module).

foo() ->
    maybe
        AA = 1,
    %%  ^^
        {ok, AA} ?= ok_a(),
        {ok, BB} = ok_b(),
        {A~A, BB}
    end.
            "#,
            );
        }

        #[test]
        fn maybe_else() {
            check(
                r#"
//- /main/src/module.erl
-module(module).

foo() ->
    maybe
        A = 1,
        A
    else
        Err ->
    %%  ^^^
            {error, E~rr}
    end.
            "#,
            );
        }

        #[test]
        fn maybe_unresolved() {
            check_unresolved(
                r#"
//- /main/src/module.erl
-module(module).

foo() ->
    maybe
        AA = 1,
        AA
    else
        2 -> {error, A~A}
    end.
            "#,
            );
        }
    }
    mod map_comp {
        use super::*;

        #[test]
        fn simple_go_to_key() {
            check(
                r#"
//- /main/src/module.erl
-module(module).

foo(Map) ->
    #{K~K => VV
        || KK := VV <- Map}.
%%         ^^
            "#,
            );
        }

        #[test]
        fn simple_go_to_val() {
            check(
                r#"
//- /main/src/module.erl
-module(module).

foo() ->
    #{KK => V~V
        || KK := VV <- #{1 => 2, 3 => 4}}.
%%               ^^
            "#,
            );
        }

        #[test]
        fn go_to_key_in_filter() {
            check(
                r#"
//- /main/src/module.erl
-module(module).

foo() ->
    #{KK => V
        || KK := V <- #{1 => 2, 3 => 4}, K~K > 1}.
%%         ^^
            "#,
            );
        }

        #[test]
        fn go_to_key_with_list_generator() {
            check(
                r#"
//- /main/src/module.erl
-module(module).

foo() ->
    #{KK => K~K + 1
        || KK <- [1, 2, 3]}.
%%         ^^
            "#,
            );
        }

        #[test]
        fn go_to_val_with_binary_generator() {
            check(
                r#"
//- /main/src/module.erl
-module(module).

foo() ->
    #{KK => V~V + 1
        || <<KK, VV>> <= <<1, 2, 3, 4>>}.
%%               ^^
            "#,
            );
        }

        #[test]
        fn list_comp_with_map_generator() {
            check(
                r#"
//- /main/src/module.erl
-module(module).

foo() ->
    [{K, V~V}
        || K := VV <- #{1 => 2, 3 => 4}, K>1].
%%              ^^
            "#,
            );
        }
    }

    #[test]
    fn local_call_from_macro_rhs() {
        check(
            r#"
   //- /src/main.erl
      -module(main).
      -export([main/0]).
      -define(MY_MACRO(), my_f~unction()).
      main() -> ?MY_MACRO().
      my_function() -> ok.
   %% ^^^^^^^^^^^
"#,
        )
    }

    #[test]
    fn remote_call_from_macro_rhs() {
        check(
            r#"
   //- /src/main.erl
      -module(main).
      -export([main/0]).
      -define(MY_MACRO(), ?MODULE:my_f~unction()).
      main() -> ?MY_MACRO().
      my_function() -> ok.
   %% ^^^^^^^^^^^
"#,
        )
    }

    #[test]
    fn remote_call_from_included_macro_rhs() {
        check(
            r#"
   //- /include/main.hrl
   -define(MY_MACRO(), main:my_f~unction()).
   //- /src/main.erl
      -module(main).
      -export([main/0]).
      my_function() -> ok.
   %% ^^^^^^^^^^^
"#,
        )
    }

    #[test]
    fn built_in_macro_function_name_1() {
        check(
            r#"
         //- /src/main.erl
            -module(main).

            foo() ->
         %% ^^^
               ?FUNCT~ION_NAME().
            "#,
        )
    }

    #[test]
    fn built_in_macro_function_name_2() {
        check_unresolved(
            r#"
         //- /src/main.erl
            -module(main).

            foo() ->
               ?FUNCT~ION_NAME.
            "#,
        )
    }

    #[test]
    fn built_in_macro_function_name_3() {
        check_unresolved(
            r#"
         //- /src/main.erl
            -module(main).

            foo(X) ->
               ?FUNCT~ION_NAME(X,1).
            "#,
        )
    }

    #[test]
    fn built_in_macro_function_name_4() {
        check(
            r#"
         //- /src/main.erl
            -module(main).

            foo(X) ->
               ?FUNCT~ION_NAME(X,1).

            foo(X,Y) ->
         %% ^^^
               {X,Y}.
            "#,
        )
    }
}
