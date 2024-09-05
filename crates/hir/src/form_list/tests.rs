/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_base_db::fixture::WithFixture;
use expect_test::expect;
use expect_test::Expect;

use crate::db::DefDatabase;
use crate::test_db::TestDB;

fn check(ra_fixture: &str, expect: Expect) {
    let (db, file_id) = TestDB::with_single_file(ra_fixture);
    let form_list = db.file_form_list(file_id);
    let pretty = form_list.pretty_print();
    expect.assert_eq(pretty.trim_start());
}

#[test]
fn empty() {
    check(r#""#, expect![[r#""#]]);
}

#[test]
fn module_attribute() {
    check(
        r#"
-module( 'foo' ).
"#,
        expect![[r#"
            -module(foo). %% cond: None
        "#]],
    )
}

#[test]
fn include_attribute() {
    check(
        r#"
-include("foo.hrl").
-include_lib("kernel" "/include/file.hrl").
"#,
        expect![[r#"
            -include("foo.hrl"). %% cond: None

            -inlcude_lib("kernel/include/file.hrl"). %% cond: None
        "#]],
    )
}

#[test]
fn feature_attribute() {
    check(
        r#"
-feature(maybe_expr, enable).
"#,
        expect![[r#"
            -feature(...). %% cond: None
        "#]],
    )
}

#[test]
fn module_doc_attribute() {
    check(
        r#"
-moduledoc "
Convenience functions for encoding and decoding from base64.
".
"#,
        expect![[r#"
            -moduledoc(...). %% cond: None
        "#]],
    )
}

#[test]
fn doc_attribute() {
    check(
        r#"
-doc "Example".
"#,
        expect![[r#"
            -doc(...). %% cond: None
        "#]],
    )
}

#[test]
fn function() {
    check(
        r#"
fun1() -> ok.
fun2(1, 2, 3) -> error;
fun2(A, B, C) -> ok.
"#,
        expect![[r#"
            fun1() -> .... %% cond: None

            fun2(_, _, _) -> .... %% cond: None

            fun2(_, _, _) -> .... %% cond: None
        "#]],
    )
}

#[test]
fn define_undef() {
    check(
        r#"
-define(FOO, 1).
-define(BAR(), 2).
-define(BAZ(X), 3).
-undef(XXX).
"#,
        expect![[r#"
            -define(FOO, ...). %% cond: None

            -define(BAR(), ...). %% cond: None

            -define(BAZ(_), ...). %% cond: None

            -undef(XXX). %% cond: None
        "#]],
    )
}

#[test]
fn macro_function() {
    check(
        r#"
-define(FOO, foo).
-define(INCOMPATIBLE, 1 + 1).
?FOO() -> ok.
?UNDEFINED() -> ok.
?INCOMPATIBLE() -> ok.
"#,
        expect![[r#"
            -define(FOO, ...). %% cond: None

            -define(INCOMPATIBLE, ...). %% cond: None

            foo() -> .... %% cond: None

            [missing name]() -> .... %% cond: None

            [missing name]() -> .... %% cond: None
        "#]],
    )
}

#[test]
fn var_function() {
    check(
        r#"
VAR() -> ok.
"#,
        expect![[r#"
            [missing name]() -> .... %% cond: None
        "#]],
    )
}

#[test]
fn ifdef() {
    check(
        r#"
-ifdef(FOO).
-ifdef(bar).
-endif.
-endif.
"#,
        expect![[r#"
            -ifdef(FOO). %% 0, cond: None

            -ifdef(bar). %% 1, cond: Some(0)

            -endif. %% prev: 1

            -endif. %% prev: 0
        "#]],
    )
}

#[test]
fn ifndef() {
    check(
        r#"
-ifndef(FOO).
-ifndef(bar).
-endif.
-endif.
"#,
        expect![[r#"
            -ifndef(FOO). %% 0, cond: None

            -ifndef(bar). %% 1, cond: Some(0)

            -endif. %% prev: 1

            -endif. %% prev: 0
        "#]],
    )
}

#[test]
fn pp_if() {
    check(
        r#"
-if(?FOO > 0).
-if(?bar < 100).
-endif.
-endif.
"#,
        expect![[r#"
            -if(...). %% 0, cond: None

            -if(...). %% 1, cond: Some(0)

            -endif. %% prev: 1

            -endif. %% prev: 0
        "#]],
    )
}

#[test]
fn pp_elif() {
    check(
        r#"
-if(?FOO > 0).
-elif(?bar < 100).
-elif(?bar < 1).
-endif.
"#,
        expect![[r#"
            -if(...). %% 0, cond: None

            -elif(...). %% 1, prev: 0

            -elif(...). %% 2, prev: 1

            -endif. %% prev: 2
        "#]],
    )
}

#[test]
fn pp_else() {
    check(
        r#"
-if(?FOO > 0).
-else.
foo() -> ok.
-endif.
"#,
        expect![[r#"
            -if(...). %% 0, cond: None

            -else. %% 1, prev: 0

            foo() -> .... %% cond: Some(1)

            -endif. %% prev: 1
        "#]],
    )
}

#[test]
fn export() {
    check(
        r#"
-export([]).
-export([foo/1, bar/0]).
"#,
        expect![[r#"
            -export([]). %% cond: None

            -export([ %% cond: None
                foo/1,
                bar/0
            ]).
        "#]],
    )
}

#[test]
fn import() {
    check(
        r#"
-import(, []).
-import(foo, []).
-import(foo, [foo/1]).
"#,
        expect![[r#"
            -import([missing name], []). %% cond: None

            -import(foo, []). %% cond: None

            -import(foo, [ %% cond: None
                foo/1
            ]).
        "#]],
    )
}

#[test]
fn type_export() {
    check(
        r#"
-export_type([]).
-export_type([foo/1]).
"#,
        expect![[r#"
            -export_type([]). %% cond: None

            -export_type([ %% cond: None
                foo/1
            ]).
        "#]],
    )
}

#[test]
fn behaviour() {
    check(
        r#"
-behaviour(foo).
-behavior(foo).
"#,
        expect![[r#"
            -behaviour(foo). %% cond: None

            -behaviour(foo). %% cond: None
        "#]],
    )
}

#[test]
fn type_alias() {
    check(
        r#"
-type foo() :: ok.
-type bar(A) :: ok.
"#,
        expect![[r#"
            -type foo() :: .... %% cond: None

            -type bar(_) :: .... %% cond: None
        "#]],
    )
}

#[test]
fn opaque() {
    check(
        r#"
-opaque foo() :: ok.
-opaque bar(A) :: ok.
"#,
        expect![[r#"
            -opaque foo() :: .... %% cond: None

            -opaque bar(_) :: .... %% cond: None
        "#]],
    )
}

#[test]
fn optional_callbacks() {
    check(
        r#"
-optional_callbacks([]).
-optional_callbacks([foo/1]).
"#,
        expect![[r#"
            -optional_callbacks([]). %% cond: None

            -optional_callbacks([ %% cond: None
                foo/1
            ]).
        "#]],
    )
}

#[test]
fn specs() {
    check(
        r#"
-spec bar() -> ok.
-spec foo(integer()) -> integer();
         (float()) -> float().
"#,
        expect![[r#"
            -spec bar() -> .... %% cond: None

            -spec foo(_) -> .... %% cond: None
        "#]],
    )
}

#[test]
fn spec_with_matched_module() {
    check(
        r#"
-module(foo).
-spec foo:bar() -> ok.
"#,
        expect![[r#"
            -module(foo). %% cond: None

            -spec bar() -> .... %% cond: None
        "#]],
    )
}

#[test]
fn spec_with_default_module() {
    check(
        r#"
-spec main:bar() -> ok.
"#,
        expect![[r#"
            -spec bar() -> .... %% cond: None
        "#]],
    )
}

#[test]
fn spec_with_mismatched_module() {
    check(
        r#"
-module(main).
-spec other:bar() -> ok.
"#,
        expect![[r#"
            -module(main). %% cond: None

            -spec bar() -> .... %% cond: None
        "#]],
    )
}

#[test]
fn callbacks() {
    check(
        r#"
-callback bar() -> ok.
-callback foo(integer()) -> integer();
             (float()) -> float().
"#,
        expect![[r#"
            -callback bar() -> .... %% cond: None

            -callback foo(_) -> .... %% cond: None
        "#]],
    )
}

#[test]
fn record() {
    check(
        r#"
-record(foo, {}).
"#,
        expect![[r#"
            -record(foo, {}). %% cond: None
        "#]],
    )
}

#[test]
fn record_fields() {
    check(
        r#"
-record(foo, {a, b = b, c :: c, d = d :: d}).
"#,
        expect![[r#"
            -record(foo, { %% cond: None
                a,
                b,
                c,
                d
            }).
        "#]],
    )
}

#[test]
fn compile_option() {
    check(
        r#"
-compile([export_all]).
"#,
        expect![[r#"
            -compile(...). %% cond: None
        "#]],
    )
}

#[test]
fn attributes() {
    check(
        r#"
-'foobar'(a).
-attribute([]).
"#,
        expect![[r#"
            -foobar(...). %% cond: None

            -attribute(...). %% cond: None
        "#]],
    )
}

#[test]
fn deprecated() {
    check(
        r#"
                    -deprecated(module).
                    -deprecated({foo, 1, "desc"}).
                    -deprecated({foo, '_', "desc"}).
                    -deprecated({foo, '_', atom}).
                    -deprecated({foo, '_'}).
                    -deprecated({foo, 1}).
                    -deprecated({'_', '_'}).
                    -deprecated({'_', '_', atom}).
                    -deprecated({'_', '_', "desc"}).
                    -deprecated([{foo, 1, "desc"}, {foo, '_', "desc"}, {foo, '_', atom}, {foo, '_'}, {foo, 1}, {'_', '_'}, {'_', '_', atom}, {'_', '_', "desc"}]).
"#,
        expect![[r#"
                            -deprecated(module). %% cond: None

                            -deprecated({foo, 1, "desc"}). %% cond: None

                            -deprecated({foo, '_', "desc"}). %% cond: None

                            -deprecated({foo, '_', atom}). %% cond: None

                            -deprecated({foo, '_'}). %% cond: None

                            -deprecated({foo, 1}). %% cond: None

                            -deprecated({'_', '_'}). %% cond: None

                            -deprecated({'_', '_', atom}). %% cond: None

                            -deprecated({'_', '_', "desc"}). %% cond: None

                            -deprecated([{foo, 1, "desc"},{foo, '_', "desc"},{foo, '_', atom},{foo, '_'},{foo, 1},{'_', '_'},{'_', '_', atom},{'_', '_', "desc"}]). %% cond: None
        "#]],
    )
}
