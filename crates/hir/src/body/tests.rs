/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Write;

use elp_base_db::OTP_VERSION;
use elp_base_db::assert_eq_expected;
use elp_base_db::fixture::WithFixture;
use expect_test::Expect;
use expect_test::expect;

use crate::AnyAttribute;
use crate::CallTarget;
use crate::Expr;
use crate::FormIdx;
use crate::FunctionDefId;
use crate::InFile;
use crate::Literal;
use crate::PPDirective;
use crate::SpecOrCallback;
use crate::Strategy;
use crate::TypeExpr;
use crate::db::DefDatabase;
use crate::fold::MacroStrategy;
use crate::fold::ParenStrategy;
use crate::test_db::TestDB;

#[track_caller]
fn check(ra_fixture: &str, expect: Expect) {
    let (db, file_ids, _) = TestDB::with_many_files(ra_fixture);
    let file_id = file_ids[0];
    let form_list = db.file_form_list(file_id);
    let pretty = form_list
        .forms()
        .iter()
        .flat_map(|&form_idx| match form_idx {
            FormIdx::FunctionClause(function_id) => {
                // We now have only one clause per function, with the
                // FunctionDefId derived from the FunctionId of the
                // first one. So print the whole thing when we have a
                // valid FunctionDefid.
                let def_map = db.def_map(file_id);
                let function_def_id = InFile::new(file_id, FunctionDefId::new(function_id));
                if let Some(_fun_def) = def_map.get_by_function_id(&function_def_id) {
                    let function = &form_list[function_id];
                    let body = db.function_body(function_def_id);
                    Some(body.print(&db, function))
                } else {
                    None
                }
            }
            FormIdx::TypeAlias(type_alias_id) => {
                let type_alias = &form_list[type_alias_id];
                let body = db.type_body(InFile::new(file_id, type_alias_id));
                Some(body.print(&db, type_alias))
            }
            FormIdx::Spec(spec_id) => {
                let spec = SpecOrCallback::Spec(form_list[spec_id].clone());
                let body = db.spec_body(InFile::new(file_id, spec_id));
                Some(body.print(&db, spec))
            }
            FormIdx::Callback(callback_id) => {
                let spec = SpecOrCallback::Callback(form_list[callback_id].clone());
                let body = db.callback_body(InFile::new(file_id, callback_id));
                Some(body.print(&db, spec))
            }
            FormIdx::Record(record_id) => {
                let body = db.record_body(InFile::new(file_id, record_id));
                Some(body.print(&db, &form_list, record_id))
            }
            FormIdx::Attribute(attribute_id) => {
                let attribute = AnyAttribute::Attribute(form_list[attribute_id].clone());
                let body = db.attribute_body(InFile::new(file_id, attribute_id));
                Some(body.print(&db, attribute))
            }
            FormIdx::CompileOption(attribute_id) => {
                let attribute = AnyAttribute::CompileOption(form_list[attribute_id].clone());
                let body = db.compile_body(InFile::new(file_id, attribute_id));
                Some(body.print(&db, attribute))
            }
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("");
    expect.assert_eq(pretty.trim_start());
}

#[track_caller]
fn check_ast(ra_fixture: &str, expect: Expect) {
    let (db, file_ids, _) = TestDB::with_many_files(ra_fixture);
    let file_id = file_ids[0];
    let form_list = db.file_form_list(file_id);
    let strategy = Strategy {
        macros: MacroStrategy::ExpandButIncludeMacroCall,
        parens: ParenStrategy::VisibleParens,
    };
    let pretty = form_list
        .forms()
        .iter()
        .flat_map(|&form_idx| match form_idx {
            FormIdx::FunctionClause(function_id) => {
                // We now have only one clause per function, with the
                // FunctionDefId derived from the FunctionId of the
                // first one. So print the whole thing when we have a
                // valid FunctionDefid.
                let def_map = db.def_map(file_id);
                let function_def_id = InFile::new(file_id, FunctionDefId::new(function_id));
                if let Some(_fun_def) = def_map.get_by_function_id(&function_def_id) {
                    let body = db.function_body(function_def_id);
                    Some(body.tree_print(strategy))
                } else {
                    None
                }
            }
            FormIdx::TypeAlias(type_alias_id) => {
                let type_alias = &form_list[type_alias_id];
                let body = db.type_body(InFile::new(file_id, type_alias_id));
                Some(body.tree_print(type_alias))
            }
            FormIdx::Spec(spec_id) => {
                let spec = SpecOrCallback::Spec(form_list[spec_id].clone());
                let body = db.spec_body(InFile::new(file_id, spec_id));
                Some(body.tree_print(spec))
            }
            FormIdx::Callback(callback_id) => {
                let spec = SpecOrCallback::Callback(form_list[callback_id].clone());
                let body = db.callback_body(InFile::new(file_id, callback_id));
                Some(body.tree_print(spec))
            }
            FormIdx::Record(record_id) => {
                let body = db.record_body(InFile::new(file_id, record_id));
                Some(body.tree_print(&form_list[record_id]))
            }
            FormIdx::Attribute(attribute_id) => {
                let attribute = AnyAttribute::Attribute(form_list[attribute_id].clone());
                let body = db.attribute_body(InFile::new(file_id, attribute_id));
                Some(body.tree_print(attribute))
            }
            FormIdx::CompileOption(attribute_id) => {
                let attribute = AnyAttribute::CompileOption(form_list[attribute_id].clone());
                let body = db.compile_body(InFile::new(file_id, attribute_id));
                Some(body.tree_print(attribute))
            }
            FormIdx::PPDirective(idx) => match &form_list[idx] {
                PPDirective::Define(define_id) => {
                    let define = &form_list[*define_id];
                    let body = db.define_body(InFile::new(file_id, *define_id));
                    Some(body.tree_print(define))
                }
                _ => None,
            },
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("");
    expect.assert_eq(pretty.trim_start());
}

#[test]
fn simple() {
    check(
        r#"
foo(ok) -> ok.
"#,
        expect![[r#"
            foo(ok) ->
                ok.
        "#]],
    );
}

#[test]
fn char() {
    check(
        r#"
foo($a) -> $b.
"#,
        expect![[r#"
            foo($a) ->
                $b.
        "#]],
    );
}

#[test]
fn float() {
    check(
        r#"
foo(0.1) -> 1.2.
"#,
        expect![[r#"
            foo(0.1) ->
                1.2.
        "#]],
    );
}

#[test]
fn based_float() {
    check(
        r#"
foo(10#0.1) -> 10#1.2.
"#,
        expect![[r#"
            foo([missing]) ->
                [missing].
        "#]],
    );
}

#[test]
fn integer() {
    check(
        r#"
foo(42) -> 4_2.
"#,
        expect![[r#"
            foo(42) ->
                42.
        "#]],
    );
}

#[test]
fn integer_with_base() {
    check(
        r#"
foo(10#42) ->
  {10#42, 2#1011011, 16#1FFF}.
"#,
        expect![[r#"
            foo(42) ->
                {
                    42,
                    91,
                    8191
                }.
        "#]],
    );
}

#[test]
fn very_large_integer_with_base() {
    check(
        r#"
foo() ->
  { 16#1000000000000000000000000000000000000,
    1000000000000000000000000000000000000000
  }.
"#,
        expect![[r#"
            foo() ->
                {
                    [missing],
                    [missing]
                }.
        "#]],
    );
}

#[test]
fn string() {
    check(
        r#"
foo("") -> "abc\s\x61".
"#,
        expect![[r#"
            foo("") ->
                "abc a".
        "#]],
    );
}

#[test]
fn concat() {
    check(
        r#"
foo("" "") -> "a" "b" "c" "\141".
"#,
        expect![[r#"
            foo("") ->
                "abca".
        "#]],
    );
}

#[test]
fn var() {
    check(
        r#"
foo(Foo) -> Foo.
"#,
        expect![[r#"
            foo(Foo) ->
                Foo.
        "#]],
    );
}

#[test]
fn tuple() {
    check(
        r#"
foo({a, b}) -> {1, 2, 3}.
"#,
        expect![[r#"
            foo({
                a,
                b
            }) ->
                {
                    1,
                    2,
                    3
                }.
        "#]],
    );
}

#[test]
fn list() {
    check(
        r#"
foo([a, b]) -> [1, 2, 3].
"#,
        expect![[r#"
            foo([
                a,
                b
            ]) ->
                [
                    1,
                    2,
                    3
                ].
        "#]],
    );

    check(
        r#"
foo([a | b]) -> [1, 2 | 3].
"#,
        expect![[r#"
            foo([
                a
                | b
            ]) ->
                [
                    1,
                    2
                    | 3
                ].
        "#]],
    );
}

// A macro whose body is a `,`-separated list of expressions (parsed as
// `ReplacementGuardAnd`) must splice its elements into a surrounding
// list — both as a regular element and as the LHS of a `|` pipe. The
// same applies to list patterns.

#[test]
fn list_expr_with_multi_element_macro() {
    check(
        r#"
-define(ELEMS, 1, 2, 3).

foo() -> [start, ?ELEMS, finish].
"#,
        expect![[r#"
            foo() ->
                [
                    start,
                    1,
                    2,
                    3,
                    finish
                ].
        "#]],
    );
}

#[test]
fn list_expr_with_multi_element_macro_before_pipe() {
    // `?ELEMS` is the LHS of a `|`. Its expansion must splice the
    // elements before the tail.
    check(
        r#"
-define(ELEMS, 1, 2, 3).

foo(Tail) -> [?ELEMS | Tail].
"#,
        expect![[r#"
            foo(Tail) ->
                [
                    1,
                    2,
                    3
                    | Tail
                ].
        "#]],
    );
}

#[test]
fn list_pat_with_multi_element_macro() {
    check(
        r#"
-define(ELEMS, _, _).

foo([?ELEMS, last]) -> ok.
"#,
        expect![[r#"
            foo([
                _,
                _,
                last
            ]) ->
                ok.
        "#]],
    );
}

#[test]
fn list_pat_with_multi_element_macro_before_pipe() {
    // Same splice happens at pattern positions, including before `|`.
    check(
        r#"
-define(ELEMS, _, _).

foo([?ELEMS | Tail]) -> Tail.
"#,
        expect![[r#"
            foo([
                _,
                _
                | Tail
            ]) ->
                Tail.
        "#]],
    );
}

#[test]
fn list_expr_with_wrapper_multi_element_macro() {
    // OUTER is a single Expr that calls into the multi-element INNER.
    // Exercises the `MacroDefReplacement::Expr(MacroCallExpr)` arm of
    // `try_expand_list_expr_macro`.
    check(
        r#"
-define(INNER, 1, 2).
-define(OUTER, ?INNER).

foo() -> [head, ?OUTER, tail].
"#,
        expect![[r#"
            foo() ->
                [
                    head,
                    1,
                    2,
                    tail
                ].
        "#]],
    );
}

// A macro whose body is a `,`-separated list of expressions (parsed as
// `ReplacementGuardAnd`) must splice its elements into a surrounding
// tuple expression or tuple pattern.

#[test]
fn tuple_expr_with_multi_element_macro() {
    check(
        r#"
-define(ELEMS, a, b, c).

foo() -> {first, ?ELEMS, last}.
"#,
        expect![[r#"
            foo() ->
                {
                    first,
                    a,
                    b,
                    c,
                    last
                }.
        "#]],
    );
}

#[test]
fn tuple_expr_with_only_multi_element_macro() {
    check(
        r#"
-define(ELEMS, a, b, c).

foo() -> {?ELEMS}.
"#,
        expect![[r#"
            foo() ->
                {
                    a,
                    b,
                    c
                }.
        "#]],
    );
}

#[test]
fn tuple_expr_with_wrapper_multi_element_macro() {
    check(
        r#"
-define(INNER, 1, 2).
-define(OUTER, ?INNER).

foo() -> {head, ?OUTER, tail}.
"#,
        expect![[r#"
            foo() ->
                {
                    head,
                    1,
                    2,
                    tail
                }.
        "#]],
    );
}

#[test]
fn tuple_expr_with_macro_inside_multi_macro() {
    check(
        r#"
-define(INNER, 1, 2).
-define(OUTER, first, ?INNER).

foo() -> {?OUTER, last}.
"#,
        expect![[r#"
            foo() ->
                {
                    first,
                    1,
                    2,
                    last
                }.
        "#]],
    );
}

#[test]
fn tuple_pat_with_multi_element_macro() {
    check(
        r#"
-define(ELEMS, _, _).

foo({first, ?ELEMS, last}) -> ok.
"#,
        expect![[r#"
            foo({
                first,
                _,
                _,
                last
            }) ->
                ok.
        "#]],
    );
}

#[test]
fn tuple_pat_with_only_multi_element_macro() {
    check(
        r#"
-define(ELEMS, _, _, _).

foo({?ELEMS}) -> ok.
"#,
        expect![[r#"
            foo({
                _,
                _,
                _
            }) ->
                ok.
        "#]],
    );
}

#[test]
fn tuple_pat_with_wrapper_multi_element_macro() {
    check(
        r#"
-define(INNER, _, _).
-define(OUTER, ?INNER).

foo({head, ?OUTER, tail}) -> ok.
"#,
        expect![[r#"
            foo({
                head,
                _,
                _,
                tail
            }) ->
                ok.
        "#]],
    );
}

// A macro whose body is a `,`-separated list of expressions (parsed as
// `ReplacementGuardAnd`) must splice its elements into function call
// arguments — both local and remote calls.

#[test]
fn call_args_with_multi_element_macro() {
    check(
        r#"
-define(ARGS, 1, 2, 3).

foo() -> bar(first, ?ARGS, last).
"#,
        expect![[r#"
            foo() ->
                bar(
                    first,
                    1,
                    2,
                    3,
                    last
                ).
        "#]],
    );
}

#[test]
fn call_args_with_only_multi_element_macro() {
    check(
        r#"
-define(ARGS, a, b, c).

foo() -> bar(?ARGS).
"#,
        expect![[r#"
            foo() ->
                bar(
                    a,
                    b,
                    c
                ).
        "#]],
    );
}

#[test]
fn remote_call_args_with_multi_element_macro() {
    check(
        r#"
-define(ARGS, 1, 2).

foo() -> mod:func(head, ?ARGS, tail).
"#,
        expect![[r#"
            foo() ->
                mod:func(
                    head,
                    1,
                    2,
                    tail
                ).
        "#]],
    );
}

#[test]
fn call_args_with_wrapper_multi_element_macro() {
    check(
        r#"
-define(INNER, 1, 2).
-define(OUTER, ?INNER).

foo() -> bar(head, ?OUTER, tail).
"#,
        expect![[r#"
            foo() ->
                bar(
                    head,
                    1,
                    2,
                    tail
                ).
        "#]],
    );
}

#[test]
fn r#match() {
    check(
        r#"
foo(A = B) -> A = B.
"#,
        expect![[r#"
            foo(A = B) ->
                A = B.
        "#]],
    );
}

#[test]
fn unary_op() {
    check(
        r#"
foo(+ A, -B) -> bnot A, not C.
"#,
        expect![[r#"
            foo((+ A), (- B)) ->
                (bnot A),
                (not C).
        "#]],
    );
}

#[test]
fn binary_op() {
    check(
        r#"
foo(A ++ B, C + D) -> E andalso F, G ! H.
"#,
        expect![[r#"
            foo((A ++ B), (C + D)) ->
                (E andalso F),
                (G ! H).
        "#]],
    );
}

#[test]
fn map() {
    check(
        r#"
foo(#{1 + 2 := 3 + 4}) -> #{a => b}.
"#,
        expect![[r#"
            foo(#{
                (1 + 2) := (3 + 4)
            }) ->
                #{
                    a => b
                }.
        "#]],
    );
}

#[test]
fn map_update() {
    check(
        r#"
foo() -> #{a => b}#{a := b, c => d}.
"#,
        expect![[r#"
            foo() ->
                #{
                    a => b
                }#{
                    a := b,
                    c => d
                }.
        "#]],
    );
}

#[test]
fn record_index() {
    check(
        r#"
foo(#record.field) -> #record.field.
"#,
        expect![[r##"
            foo(#record.field) ->
                #record.field.
        "##]],
    );
}

#[test]
fn record() {
    check(
        r#"
//- expect_parse_errors
foo1(#record{field = 1}) -> #record{field = A + B}.
foo2(#record{field}) -> #record{field = }.
"#,
        expect![[r##"
            foo1(#record{
                field = 1
            }) ->
                #record{
                    field = (A + B)
                }.

            foo2(#record{
                field = [missing]
            }) ->
                #record{
                    field = [missing]
                }.
        "##]],
    );
}

#[test]
fn record_update() {
    check(
        r#"
//- expect_parse_errors
foo1() -> Expr#record{field = undefined}.
foo2() -> Expr#record{field = ok, missing = }.
"#,
        expect![[r#"
            foo1() ->
                Expr#record{
                    field = undefined
                }.

            foo2() ->
                Expr#record{
                    field = ok,
                    missing = [missing]
                }.
        "#]],
    );
}

// `#r{_ = V}` is Erlang's wildcard record-field initialiser: it sets
// every otherwise-unset field of the record to `V`. The field name is
// represented as `None` in HIR; the pretty printer surfaces it as `_`.

#[test]
fn record_wildcard_construction() {
    check(
        r#"
foo() -> #record{_ = undefined}.
"#,
        expect![[r#"
            foo() ->
                #record{
                    _ = undefined
                }.
        "#]],
    );
}

#[test]
fn record_wildcard_in_pattern() {
    check(
        r#"
foo(#record{_ = _}) -> ok.
"#,
        expect![[r#"
            foo(#record{
                _ = _
            }) ->
                ok.
        "#]],
    );
}

#[test]
fn record_wildcard_mixed_with_named_fields() {
    // Mixing named fields with the wildcard: `a` is set explicitly,
    // every other field is initialised to `0`.
    check(
        r#"
foo() -> #record{a = 1, _ = 0}.
"#,
        expect![[r#"
            foo() ->
                #record{
                    a = 1,
                    _ = 0
                }.
        "#]],
    );
}

#[test]
fn record_wildcard_hir_structure() {
    // Verify the HIR distinguishes wildcard `_` (None) from named
    // fields (Some(Atom)) via tree_print.
    check_ast(
        r#"
foo() -> #record{a = 1, _ = 0}.
foo(#record{_ = _}) -> ok.
"#,
        expect![[r#"
            function: foo/0
            Clause {
                pats
                guards
                exprs
                    Expr<6>:Expr::Record {
                        name: Atom('record')
                        fields
                            Atom('a'):
                                Expr<2>:Literal(Integer(1)),
                        default_field
                            Expr<4>:Literal(Integer(0)),
                    },
            }.
            function: foo/1
            Clause {
                pats
                    Pat<1>:Pat::Record {
                        name: Atom('record')
                        fields
                        default_field
                            Pat<0>:Pat::Var(_),
                    },
                guards
                exprs
                    Expr<3>:Literal(Atom('ok')),
            }.
        "#]],
    );
}

#[test]
fn record_field() {
    check(
        r#"
foo() -> Expr#record.field.
"#,
        expect![[r##"
            foo() ->
                Expr#record.field.
        "##]],
    );
}

#[test]
fn binary() {
    check(
        r#"
foo(<<Size, Data:Size/binary>>) -> <<+1/integer-little-unit:8>>.
"#,
        expect![[r#"
            foo(<<
                Size,
                Data:Size/binary
            >>) ->
                <<
                    (+ 1)/integer-little-unit:8
                >>.
        "#]],
    );
}

// A macro whose body is a `,`-separated list of values must splice its
// elements into a surrounding `<<...>>` binary as separate segments —
// in both expression and pattern positions. Macros with their own size
// or type specifier must NOT trigger the splice.

#[test]
fn binary_expr_with_multi_element_macro() {
    check(
        r#"
-define(PREFIX, 13, 10, 13, 10).

foo(Rest) -> <<?PREFIX, Rest/binary>>.
"#,
        expect![[r#"
            foo(Rest) ->
                <<
                    13,
                    10,
                    13,
                    10,
                    Rest/binary
                >>.
        "#]],
    );
}

#[test]
fn binary_pat_with_multi_element_macro() {
    check(
        r#"
-define(PREFIX, 13, 10, 13, 10).

foo(<<?PREFIX, Rest/binary>>) -> Rest.
"#,
        expect![[r#"
            foo(<<
                13,
                10,
                13,
                10,
                Rest/binary
            >>) ->
                Rest.
        "#]],
    );
}

#[test]
fn binary_expr_with_only_multi_element_macro() {
    // Macro fills the whole binary.
    check(
        r#"
-define(PAIR, 1, 2).

foo() -> <<?PAIR>>.
"#,
        expect![[r#"
            foo() ->
                <<
                    1,
                    2
                >>.
        "#]],
    );
}

#[test]
fn binary_expr_with_wrapper_multi_element_macro() {
    // OUTER is a single Expr that calls into the multi-element INNER —
    // exercises the `MacroDefReplacement::Expr(MacroCallExpr)` arm of
    // `try_expand_binary_expr_macro`.
    check(
        r#"
-define(INNER, 1, 2, 3).
-define(OUTER, ?INNER).

foo() -> <<?OUTER, 4>>.
"#,
        expect![[r#"
            foo() ->
                <<
                    1,
                    2,
                    3,
                    4
                >>.
        "#]],
    );
}

#[test]
fn binary_expr_macro_with_size_specifier_does_not_expand() {
    // `?PREFIX:8` has a size specifier — splicing wouldn't make sense
    // (size can't apply to multiple segments), so the macro is lowered
    // as a single segment whose element comes from the standard path.
    // With a multi-element macro body, that path lowers it as `Missing`
    // (the existing pre-fix behaviour for un-spliceable cases).
    check(
        r#"
-define(PREFIX, 1, 2).

foo() -> <<?PREFIX:8, 0>>.
"#,
        expect![[r#"
            foo() ->
                <<
                    [missing]:8,
                    0
                >>.
        "#]],
    );
}

#[test]
fn catch() {
    check(
        r#"
foo() -> catch 1 + 2.
"#,
        expect![[r#"
            foo() ->
                (catch (1 + 2)).
        "#]],
    );
}

#[test]
fn begin_block() {
    check(
        r#"
foo() -> begin 1, 2 end.
"#,
        expect![[r#"
            foo() ->
                begin
                    1,
                    2
                end.
        "#]],
    );
}

#[test]
fn case() {
    check(
        r#"
foo() ->
    case 1 + 2 of
        X when X andalso true; X =< 100, X >= 5 -> ok;
        _ -> error
    end.
"#,
        expect![[r#"
            foo() ->
                case (1 + 2) of
                    X when
                        (X andalso true);
                        (X =< 100),
                        (X >= 5)
                    ->
                        ok;
                    _ ->
                        error
                end.
        "#]],
    );
}

#[test]
fn receive() {
    check(
        r#"
foo() ->
    receive
        ok when true -> ok;
        _ -> error
    after Timeout -> timeout
    end.
"#,
        expect![[r#"
            foo() ->
                receive
                    ok when
                        true
                    ->
                        ok;
                    _ ->
                        error
                after Timeout ->
                    timeout
                end.
        "#]],
    );
}

#[test]
fn call() {
    check(
        r#"
foo() ->
    foo(),
    size(A),
    size(),
    foo:bar(A).
"#,
        expect![[r#"
            foo() ->
                foo(),
                erlang:size(
                    A
                ),
                size(),
                foo:bar(
                    A
                ).
        "#]],
    );
}

#[test]
fn capture_fun() {
    check(
        r#"
foo() ->
    fun foo/1,
    fun halt/0,
    fun mod:foo/1,
    fun Mod:Foo/Arity.
"#,
        expect![[r#"
            foo() ->
                fun foo/1,
                fun erlang:halt/0,
                fun mod:foo/1,
                fun Mod:Foo/Arity.
        "#]],
    );
}

#[test]
fn if_expr() {
    check(
        r#"
foo() ->
    if is_atom(X) -> ok;
       true -> error
    end.
"#,
        expect![[r#"
            foo() ->
                if
                    erlang:is_atom(
                        X
                    ) ->
                        ok;
                    true ->
                        error
                end.
        "#]],
    );
}

#[test]
fn catch_clause_alias_match_in_reason_pattern() {
    // In a catch clause, the reason pattern can be an alias `E = Subpat`.
    // The grammar parses this as a `match_expr`, so the existing `MatchExpr`
    // lowering path produces `Pat::Match` directly.
    check(
        r#"
foo() ->
    try 1 of
        _ -> ok
    catch
        throw:E = {bad, _}:_ -> E
    end.
"#,
        expect![[r#"
            foo() ->
                try
                    1
                of
                    _ ->
                        ok
                catch
                    throw:E = {
                        bad,
                        _
                    }:_ ->
                        E
                end.
        "#]],
    );
}

#[test]
fn try_expr() {
    check(
        r#"
foo() ->
    try 1, 2 of
        _ -> ok
    catch
        Pat when true -> ok;
        error:undef:Stack -> Stack
    after
        ok
    end.
"#,
        expect![[r#"
            foo() ->
                try
                    1,
                    2
                of
                    _ ->
                        ok
                catch
                    Pat when
                        true
                    ->
                        ok;
                    error:undef:Stack ->
                        Stack
                after
                    ok
                end.
        "#]],
    );
}

#[test]
fn comprehensions() {
    check(
        r#"
foo() ->
    [X || X <- List, X >= 5],
    << Byte || <<Byte>> <= Bytes, Byte >= 5>>,
    #{KK => VV || KK := VV <- Map}.
"#,
        expect![[r#"
            foo() ->
                [
                    X
                ||
                    X <- List,
                    (X >= 5)
                ],
                <<
                    Byte
                ||
                    <<
                        Byte
                    >> <= Bytes,
                    (Byte >= 5)
                >>,
                #{
                    KK => VV
                ||
                    KK := VV <- Map
                }.
        "#]],
    );
}

#[test]
fn strict_comprehensions() {
    check(
        r#"
foo() ->
    [X || X <:- List, X >= 5],
    << Byte || <<Byte>> <:= Bytes, Byte >= 5>>,
    #{KK => VV || KK := VV <:- Map}.
"#,
        expect![[r#"
            foo() ->
                [
                    X
                ||
                    X <:- List,
                    (X >= 5)
                ],
                <<
                    Byte
                ||
                    <<
                        Byte
                    >> <:= Bytes,
                    (Byte >= 5)
                >>,
                #{
                    KK => VV
                ||
                    KK := VV <:- Map
                }.
        "#]],
    );
}

#[test]
fn zip_comprehensions() {
    check(
        r#"
foo() ->
    [{X, Y} || X <- [1,2,3] && Y <- [4,5,6]].
"#,
        expect![[r#"
            foo() ->
                [
                    {
                        X,
                        Y
                    }
                ||
                        X <- [
                            1,
                            2,
                            3
                        ]
                    &&
                        Y <- [
                            4,
                            5,
                            6
                        ]
                ].
        "#]],
    );
}

#[test]
fn fun() {
    check(
        r#"
foo() ->
    fun (ok) -> ok;
        (error) -> error
    end,
    fun Named() -> Named() end.
"#,
        expect![[r#"
            foo() ->
                fun
                    (ok) ->
                        ok;
                    (error) ->
                        error
                end,
                fun
                    Named() ->
                        Named()
                end.
        "#]],
    );
}

#[test]
fn parens() {
    check(
        r#"
//- expect_parse_errors
foo((ok), ()) ->
    (ok),
    ().
"#,
        expect![[r#"
            foo(ok, [missing]) ->
                ok,
                [missing].
        "#]],
    );
}

#[test]
fn invalid_ann_type() {
    check(
        r#"
foo(A :: {}) -> A :: {}.
"#,
        expect![[r#"
            foo([missing]) ->
                [missing].
        "#]],
    );
}

#[test]
fn invalid_dotdotdot() {
    check(
        r#"
foo(...) -> ....
"#,
        expect![[r#"
            foo([missing]) ->
                [missing].
        "#]],
    );
}

#[test]
fn invalid_call() {
    check(
        r#"
foo(bar()) -> ok.
"#,
        expect![[r#"
            foo([missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_catch() {
    check(
        r#"
foo(catch 1) -> ok.
"#,
        expect![[r#"
            foo([missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_map_update() {
    check(
        r#"
foo(X#{}) -> ok.
"#,
        expect![[r#"
            foo([missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_pipe() {
    check(
        r#"
foo(X | Y) -> X | Y.
"#,
        expect![[r#"
            foo([missing]) ->
                [missing].
        "#]],
    );
}

#[test]
fn invalid_range() {
    check(
        r#"
foo(X..Y) -> X..Y.
"#,
        expect![[r#"
            foo([missing]) ->
                [missing].
        "#]],
    );
}

#[test]
fn invalid_record_field() {
    check(
        r#"
foo(X#foo.bar) -> ok.
"#,
        expect![[r#"
            foo([missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_record_update() {
    check(
        r#"
foo(X#foo{}) -> ok.
"#,
        expect![[r#"
            foo([missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_remote() {
    check(
        r#"
foo(a:b) -> a:b.
"#,
        expect![[r#"
            foo([missing]) ->
                [missing].
        "#]],
    );
}

#[test]
fn invalid_fun() {
    check(
        r#"
foo(fun() -> ok end) -> ok.
"#,
        expect![[r#"
            foo([missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_comprehension() {
    check(
        r#"
//- expect_parse_errors
foo(<<Byte || Byte <- List>>, [Byte || Byte <- List]]) -> ok.
"#,
        expect![[r#"
            foo([missing], [missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_block() {
    check(
        r#"
foo(begin foo end) -> ok.
"#,
        expect![[r#"
            foo([missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_case() {
    check(
        r#"
foo(case X of _ -> ok end) -> ok.
"#,
        expect![[r#"
            foo([missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_capture() {
    check(
        r#"
foo(fun erlang:self/0, fun foo/2) -> ok.
"#,
        expect![[r#"
            foo([missing], [missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_fun_type() {
    check(
        r#"
foo(fun()) -> fun().
"#,
        expect![[r#"
            foo([missing]) ->
                [missing].
        "#]],
    );
}

#[test]
fn invalid_if() {
    check(
        r#"
foo(if true -> ok end) -> ok.
"#,
        expect![[r#"
            foo([missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_macro_string() {
    check(
        r#"
foo(??X) -> ??X.
"#,
        expect![[r#"
            foo([missing]) ->
                [missing].
        "#]],
    );
}

#[test]
fn macro_string_simple_atom() {
    // `??X` inside a macro body stringifies the macro argument.
    check(
        r#"
-define(STR(X), ??X).
foo() -> ?STR(hello).
"#,
        expect![[r#"
            foo() ->
                "hello".
        "#]],
    );
}

#[test]
fn macro_string_compound_expression() {
    // Tokens are joined with single spaces, mirroring the Erlang
    // preprocessor; whitespace and comments are stripped as trivia.
    check(
        r#"
-define(STR(X), ??X).
foo() -> ?STR(1 + 2).
"#,
        expect![[r#"
            foo() ->
                "1 + 2".
        "#]],
    );
}

#[test]
fn macro_string_in_pattern() {
    // `??X` resolves at pattern positions as well.
    check(
        r#"
-define(STR(X), ??X).
foo(?STR(hello)) -> ok.
"#,
        expect![[r#"
            foo("hello") ->
                ok.
        "#]],
    );
}

#[test]
fn macro_string_unknown_param_falls_back_to_missing() {
    // `??Z` where `Z` is not a parameter of the surrounding macro must
    // still produce `Missing`, matching the pre-fix behaviour for the
    // unresolved case.
    check(
        r#"
-define(STR(X), ??Z).
foo() -> ?STR(hello).
"#,
        expect![[r#"
            foo() ->
                [missing].
        "#]],
    );
}

#[test]
fn macro_string_through_nested_macro_forwarding() {
    // OUTER passes its parameter `X` straight to INNER (without `??`),
    // and INNER stringifies its own parameter `Y`.  Resolving `??Y`
    // must walk up the macro stack: Y is bound to the variable `X` in
    // OUTER's frame, which is in turn bound to the original argument
    // tokens in the call site's frame.
    check(
        r#"
-define(INNER(Y), ??Y).
-define(OUTER(X), ?INNER(X)).

foo() -> ?OUTER(hello).
"#,
        expect![[r#"
            foo() ->
                "hello".
        "#]],
    );
}

#[test]
fn macro_string_through_three_levels_of_forwarding() {
    // Walking up the stack must work for arbitrary nesting depth.
    check(
        r#"
-define(INNER(Z), ??Z).
-define(MIDDLE(Y), ?INNER(Y)).
-define(OUTER(X), ?MIDDLE(X)).

foo() -> ?OUTER(deep_arg).
"#,
        expect![[r#"
            foo() ->
                "deep_arg".
        "#]],
    );
}

#[test]
fn macro_string_through_nested_with_compound_arg() {
    // Compound arguments must be stringified verbatim (whitespace
    // collapsed) once the walk-up finds the original tokens.
    check(
        r#"
-define(INNER(Y), ??Y).
-define(OUTER(X), ?INNER(X)).

foo() -> ?OUTER(1 + 2).
"#,
        expect![[r#"
            foo() ->
                "1 + 2".
        "#]],
    );
}

#[test]
fn macro_string_simple_variable_argument() {
    // When the macro argument is itself a plain variable from the call
    // site (not a parameter being forwarded from an enclosing macro),
    // the walk-up should NOT cross into the parent frame just because
    // the parent has no binding for that name. The variable's text is
    // stringified verbatim.
    check(
        r#"
-define(STR(X), ??X).

foo(SomeVar) -> ?STR(SomeVar).
"#,
        expect![[r#"
            foo(SomeVar) ->
                "SomeVar".
        "#]],
    );
}

#[test]
fn macro_string_variable_at_top_level_call_site() {
    // Same as above but with a longer variable name and no enclosing
    // macro, so the parent frame lookup definitely fails.
    check(
        r#"
-define(STR(X), ??X).

foo(LongVariableName) -> ?STR(LongVariableName).
"#,
        expect![[r#"
            foo(LongVariableName) ->
                "LongVariableName".
        "#]],
    );
}

#[test]
fn macro_string_nested_forwarding_still_works_after_fix() {
    // Sanity: the parent walk-up still works when the inner Var really
    // does name a parameter in the parent frame. (Same shape as the
    // existing `macro_string_through_nested_macro_forwarding` test, kept
    // adjacent here to make the contrast with the simple-variable case
    // obvious.)
    check(
        r#"
-define(INNER(Y), ??Y).
-define(OUTER(X), ?INNER(X)).

foo() -> ?OUTER(stuff).
"#,
        expect![[r#"
            foo() ->
                "stuff".
        "#]],
    );
}

#[test]
fn macro_string_walkup_depth_limit() {
    // The walk-up loop in resolve_macro_string is guarded by a depth
    // counter.  Even though truly infinite recursion shouldn't happen
    // (the macro_stack is finite), this confirms we degrade gracefully
    // to `[missing]` rather than spinning for a long time on a
    // pathological macro chain.
    //
    // We can't construct 100+ levels of real macro nesting easily, but
    // we verify that a moderate chain (3 levels) still resolves correctly,
    // giving confidence the counter doesn't interfere with normal operation.
    check(
        r#"
-define(L3(Z), ??Z).
-define(L2(Y), ?L3(Y)).
-define(L1(X), ?L2(X)).

foo() -> ?L1(works).
"#,
        expect![[r#"
            foo() ->
                "works".
        "#]],
    );
}

// `??Param` stringification normalizes atom quoting: atoms whose source
// text is quoted but don't actually need quoting have the quotes stripped,
// matching `io_lib:write/1`. Atoms that DO need quoting (reserved words,
// special chars) keep their quotes.

#[test]
fn macro_string_strips_unneeded_atom_quotes() {
    // `'undefined'` is a quoted atom that doesn't need to be quoted, so
    // stringification renders it as `undefined`.
    check(
        r#"
-define(STR(X), ??X).

foo() -> ?STR('undefined').
"#,
        expect![[r#"
            foo() ->
                "undefined".
        "#]],
    );
}

#[test]
fn macro_string_preserves_quotes_on_reserved_word_atom() {
    // `'and'` is a reserved word and must remain quoted.
    check(
        r#"
-define(STR(X), ??X).

foo() -> ?STR('and').
"#,
        expect![[r#"
            foo() ->
                "'and'".
        "#]],
    );
}

#[test]
fn macro_string_preserves_quotes_on_special_char_atom() {
    // An atom containing whitespace must remain quoted.
    check(
        r#"
-define(STR(X), ??X).

foo() -> ?STR('foo bar').
"#,
        expect![[r#"
            foo() ->
                "'foo bar'".
        "#]],
    );
}

#[test]
fn macro_string_unquoted_atom_unchanged() {
    // Sanity: an already-unquoted atom is rendered verbatim.
    check(
        r#"
-define(STR(X), ??X).

foo() -> ?STR(plain_atom).
"#,
        expect![[r#"
            foo() ->
                "plain_atom".
        "#]],
    );
}

#[test]
fn macro_string_atom_normalization_in_compound_arg() {
    // Atom normalization also applies when the atom is one token among
    // many in a compound argument.
    check(
        r#"
-define(STR(X), ??X).

foo() -> ?STR({'undefined', 'and', plain}).
"#,
        expect![[r#"
            foo() ->
                "{ undefined , 'and' , plain }".
        "#]],
    );
}

// `??Param` stringification of a complex argument now substitutes any
// VAR tokens inside that argument with the actual values they were
// bound to in an enclosing macro's frame. Previously the VAR text was
// emitted literally, producing surprising results like `foo ( Y )`
// instead of `foo ( 42 )`.

#[test]
fn macro_string_compound_arg_substitutes_parent_vars() {
    // OUTER(42) calls INNER with `foo(Y)` where Y is OUTER's parameter
    // bound to 42. Stringifying `??X` inside INNER must walk up and
    // substitute Y with 42, producing "foo ( 42 )".
    check(
        r#"
-define(INNER(X), ??X).
-define(OUTER(Y), ?INNER(foo(Y))).

run() -> ?OUTER(42).
"#,
        expect![[r#"
            run() ->
                "foo ( 42 )".
        "#]],
    );
}

#[test]
fn macro_string_compound_arg_substitutes_multiple_parent_vars() {
    // Two parent vars both substituted in the same compound argument.
    check(
        r#"
-define(INNER(X), ??X).
-define(OUTER(A, B), ?INNER([A, B, A])).

run() -> ?OUTER(1, 2).
"#,
        expect![[r#"
            run() ->
                "[ 1 , 2 , 1 ]".
        "#]],
    );
}

#[test]
fn macro_string_compound_arg_substitutes_through_three_levels() {
    // Three levels of nesting: each VAR substitution walks one step up
    // the macro stack until the original argument tokens are found.
    check(
        r#"
-define(C(X), ??X).
-define(B(Y), ?C({wrap, Y})).
-define(A(Z), ?B(Z)).

run() -> ?A(deep).
"#,
        expect![[r#"
            run() ->
                "{ wrap , deep }".
        "#]],
    );
}

#[test]
fn macro_string_compound_arg_var_with_no_parent_binding_stays_literal() {
    // A VAR inside the compound argument that doesn't name a parent
    // parameter is stringified verbatim (the recursion's
    // `var_map.get(&var)` lookup falls through).
    check(
        r#"
-define(STR(X), ??X).

run(LocalVar) -> ?STR({tag, LocalVar}).
"#,
        expect![[r#"
            run(LocalVar) ->
                "{ tag , LocalVar }".
        "#]],
    );
}

#[test]
fn invalid_receive() {
    check(
        r#"
foo(receive _ -> ok after X -> timeout end) -> ok.
"#,
        expect![[r#"
            foo([missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_try() {
    check(
        r#"
foo(try 1 of _ -> ok catch _ -> error end) -> ok.
"#,
        expect![[r#"
            foo([missing]) ->
                ok.
        "#]],
    );
}

#[test]
fn invalid_concat() {
    check(
        r#"
foo("a" B "c") -> "a" B "c".
"#,
        expect![[r#"
            foo([missing]) ->
                [missing].
        "#]],
    );
}

// Macro resolution inside string concatenations: a `?MACRO` whose
// definition is (or recursively expands to) a string literal can be
// folded into the surrounding concatenation. Non-string macros still
// fall back to `Missing`.

#[test]
fn concat_with_macro_string_in_expr() {
    check(
        r#"
-define(GREETING, "hello").

foo() -> ?GREETING " world".
"#,
        expect![[r#"
            foo() ->
                "hello world".
        "#]],
    );
}

#[test]
fn concat_with_two_macro_strings() {
    check(
        r#"
-define(A, "ab").
-define(B, "cd").

foo() -> ?A ?B.
"#,
        expect![[r#"
            foo() ->
                "abcd".
        "#]],
    );
}

#[test]
fn concat_with_recursive_macro_concat() {
    // Macro body is itself a concatenation (`"head" " mid"`); the outer
    // concat must recursively resolve through it.
    check(
        r#"
-define(WRAPPED, "head" " mid").

foo() -> ?WRAPPED " tail".
"#,
        expect![[r#"
            foo() ->
                "head mid tail".
        "#]],
    );
}

#[test]
fn concat_with_non_string_macro_falls_back_to_missing() {
    // Macro expands to an atom, not a string — concatenation cannot
    // be folded, so the whole expression becomes `Missing`.
    check(
        r#"
-define(NOT_A_STRING, hello).

foo() -> ?NOT_A_STRING " world".
"#,
        expect![[r#"
            foo() ->
                [missing].
        "#]],
    );
}

#[test]
fn concat_with_macro_string_in_pattern() {
    // Same resolution must work at pattern positions.
    check(
        r#"
-define(GREETING, "hello").

foo(?GREETING " world") -> ok.
"#,
        expect![[r#"
            foo("hello world") ->
                ok.
        "#]],
    );
}

#[test]
fn concat_with_macro_string_in_term() {
    // Same resolution must work in term/attribute positions.
    check(
        r#"
-define(GREETING, "hello").

-foo(?GREETING " world").
"#,
        expect![[r#"
            -foo("hello world").
        "#]],
    );
}

// `??Param` may appear inside a string concatenation alongside literal
// strings. The stringified macro argument is folded into the resulting
// concatenation buffer.

#[test]
fn concat_with_macro_string_stringification_trailing() {
    check(
        r#"
-define(STR(X), ??X " tail").

foo() -> ?STR(hello).
"#,
        expect![[r#"
            foo() ->
                "hello tail".
        "#]],
    );
}

#[test]
fn concat_with_macro_string_stringification_leading() {
    check(
        r#"
-define(STR(X), "head " ??X).

foo() -> ?STR(world).
"#,
        expect![[r#"
            foo() ->
                "head world".
        "#]],
    );
}

#[test]
fn concat_with_macro_string_stringification_surrounded() {
    check(
        r#"
-define(STR(X), "[" ??X "]").

foo() -> ?STR(item).
"#,
        expect![[r#"
            foo() ->
                "[item]".
        "#]],
    );
}

#[test]
fn concat_with_macro_string_stringification_compound_arg() {
    // Compound argument tokens are joined with single spaces and folded
    // into the surrounding concatenation.
    check(
        r#"
-define(STR(X), "got: " ??X).

foo() -> ?STR(1 + 2).
"#,
        expect![[r#"
            foo() ->
                "got: 1 + 2".
        "#]],
    );
}

#[test]
fn concat_with_macro_string_stringification_in_pattern() {
    // Same resolution applies in pattern position.
    check(
        r#"
-define(STR(X), ??X " mark").

foo(?STR(hello)) -> ok.
"#,
        expect![[r#"
            foo("hello mark") ->
                ok.
        "#]],
    );
}

// A bare macro-parameter variable inside a Concatables — e.g.
// `-define(FMT(F), "head " F).` — must resolve via the macro stack
// instead of producing `Missing`.

#[test]
fn concat_with_var_bound_to_string_literal() {
    check(
        r#"
-define(FMT(F), "head " F).

foo() -> ?FMT("body").
"#,
        expect![[r#"
            foo() ->
                "head body".
        "#]],
    );
}

#[test]
fn concat_with_var_bound_to_string_surrounded() {
    check(
        r#"
-define(FMT(F), "[" F "]").

foo() -> ?FMT("body").
"#,
        expect![[r#"
            foo() ->
                "[body]".
        "#]],
    );
}

#[test]
fn concat_with_var_bound_to_concat_expression() {
    // Argument is itself a concatenation of two string literals; the
    // recursive `lower_concat_with_macros` call resolves it.
    check(
        r#"
-define(FMT(F), "head " F).

foo() -> ?FMT("a" "b").
"#,
        expect![[r#"
            foo() ->
                "head ab".
        "#]],
    );
}

#[test]
fn concat_with_var_forwarded_through_nested_macro() {
    // Outer macro forwards its parameter into an inner macro that uses
    // the parameter inside a concatenation. The walk through the macro
    // stack must reach the original string argument.
    check(
        r#"
-define(INNER(F), "[" F "]").
-define(OUTER(X), ?INNER(X)).

foo() -> ?OUTER("body").
"#,
        expect![[r#"
            foo() ->
                "[body]".
        "#]],
    );
}

#[test]
fn concat_with_var_bound_to_non_string_falls_back_to_missing() {
    // Variable bound to an atom (not a string-shaped expression) leaves
    // the concatenation unresolvable, falling back to `Missing`.
    check(
        r#"
-define(FMT(F), "head " F).

foo() -> ?FMT(some_atom).
"#,
        expect![[r#"
            foo() ->
                [missing].
        "#]],
    );
}

#[test]
fn concat_with_var_bound_to_string_in_pattern() {
    check(
        r#"
-define(FMT(F), "head " F).

foo(?FMT("body")) -> ok.
"#,
        expect![[r#"
            foo("head body") ->
                ok.
        "#]],
    );
}

// Macro-to-macro chaining inside string concatenation: when a macro's
// body is itself a `?OTHER` reference, `resolve_macro_concat_string`
// must recurse through the inner `MacroCallExpr` to find the literal
// string, instead of bailing out as `Missing`.

#[test]
fn concat_with_macro_alias_chain() {
    // `?OUTER` is an alias for `?INNER` which is `"value"`. The outer
    // expansion's body is a single MacroCallExpr, which is handled by
    // the new `MacroCallExpr` arm of `resolve_macro_concat_string`.
    check(
        r#"
-define(INNER, "value").
-define(OUTER, ?INNER).

foo() -> ?OUTER " tail".
"#,
        expect![[r#"
            foo() ->
                "value tail".
        "#]],
    );
}

#[test]
fn concat_with_three_link_macro_alias_chain() {
    // Three-level alias chain: A → B → C → "deep". Recursion proves
    // the new arm chains to arbitrary depth.
    check(
        r#"
-define(C, "deep").
-define(B, ?C).
-define(A, ?B).

foo() -> "[" ?A "]".
"#,
        expect![[r#"
            foo() ->
                "[deep]".
        "#]],
    );
}

#[test]
fn concat_with_macro_alias_chain_in_pattern() {
    // Same alias chain works at pattern positions.
    check(
        r#"
-define(INNER, "value").
-define(OUTER, ?INNER).

foo(?OUTER " tail") -> ok.
"#,
        expect![[r#"
            foo("value tail") ->
                ok.
        "#]],
    );
}

// Built-in macros (`?MODULE_STRING`, `?FILE`, ...) that expand to string
// literals must fold into a surrounding concatenation. Non-string built-ins
// (`?MODULE` -> atom, `?LINE` -> integer) leave the concatenation as Missing.

#[test]
fn concat_with_module_string_builtin() {
    check(
        r#"
-module(my_mod).

foo() -> ?MODULE_STRING " is the module".
"#,
        expect![[r#"
            foo() ->
                "my_mod is the module".
        "#]],
    );
}

#[test]
fn concat_with_module_string_builtin_in_pattern() {
    check(
        r#"
-module(my_mod).

foo(?MODULE_STRING " here") -> ok.
"#,
        expect![[r#"
            foo("my_mod here") ->
                ok.
        "#]],
    );
}

#[test]
fn concat_with_module_string_surrounded() {
    check(
        r#"
-module(my_mod).

foo() -> "[" ?MODULE_STRING "]".
"#,
        expect![[r#"
            foo() ->
                "[my_mod]".
        "#]],
    );
}

#[test]
fn concat_with_atom_builtin_falls_back_to_missing() {
    // `?MODULE` is an atom (not a string) — concatenation is unresolvable.
    check(
        r#"
-module(my_mod).

foo() -> ?MODULE " trailing".
"#,
        expect![[r#"
            foo() ->
                [missing].
        "#]],
    );
}

#[test]
fn concat_with_integer_builtin_falls_back_to_missing() {
    // `?LINE` is an integer — concatenation is unresolvable.
    check(
        r#"
foo() -> ?LINE " trailing".
"#,
        expect![[r#"
            foo() ->
                [missing].
        "#]],
    );
}

#[test]
fn invalid_macro_case_clause() {
    check(
        r#"
foo() ->
    case X of
        ?MACRO();
        ok -> ok
    end.
"#,
        expect![[r#"
            foo() ->
                case X of
                    ok ->
                        ok
                end.
        "#]],
    );
}

#[test]
fn macro_exprs() {
    check(
        r#"
foo(?MACRO()) -> ?MACRO().
"#,
        expect![[r#"
            foo([missing]) ->
                [missing].
        "#]],
    );
}

#[test]
fn simple_type() {
    check(
        r#"
-type foo() :: ok.
"#,
        expect![[r#"
            -type foo() :: ok.
        "#]],
    );
}

#[test]
fn simple_nominal_type() {
    check(
        r#"
-nominal foo() :: ok.
"#,
        expect![[r#"
            -nominal foo() :: ok.
        "#]],
    );
}

#[test]
fn simple_opaque() {
    check(
        r#"
-opaque foo() :: ok.
"#,
        expect![[r#"
            -opaque foo() :: ok.
        "#]],
    );
}

#[test]
fn unary_op_type() {
    check(
        r#"
-type foo() :: -1.
"#,
        expect![[r#"
            -type foo() :: (- 1).
        "#]],
    );
}

#[test]
fn binary_op_type() {
    check(
        r#"
-type foo() :: 1 + 1.
"#,
        expect![[r#"
            -type foo() :: (1 + 1).
        "#]],
    );
}

#[test]
fn ann_type() {
    check(
        r#"
-type foo() :: A :: any().
"#,
        expect![[r#"
            -type foo() :: (A  :: erlang:any()).
        "#]],
    );
}

#[test]
fn list_type() {
    check(
        r#"
-type foo() :: [foo].
-type bar() :: [bar, ...].
"#,
        expect![[r#"
            -type foo() :: [foo].

            -type bar() :: [bar, ...].
        "#]],
    );
}

#[test]
fn tuple_type() {
    check(
        r#"
-type foo() :: {a, b, c}.
"#,
        expect![[r#"
            -type foo() :: {
                a,
                b,
                c
            }.
        "#]],
    );
}

// A macro whose body is a `,`-separated list of types (parsed as
// `ReplacementGuardAnd`) must splice its elements into a surrounding
// tuple type rather than collapsing to a single `any()`.

#[test]
fn tuple_type_with_multi_type_macro() {
    check(
        r#"
-define(PAIR, integer(), atom()).
-type foo() :: {first, ?PAIR, last}.
"#,
        expect![[r#"
            -type foo() :: {
                first,
                erlang:integer(),
                erlang:atom(),
                last
            }.
        "#]],
    );
}

#[test]
fn tuple_type_with_only_multi_type_macro() {
    // Macro is the entire body — its types fill the tuple directly.
    check(
        r#"
-define(TRIPLE, a, b, c).
-type foo() :: {?TRIPLE}.
"#,
        expect![[r#"
            -type foo() :: {
                a,
                b,
                c
            }.
        "#]],
    );
}

#[test]
fn tuple_type_with_wrapper_macro() {
    // Outer macro is a single Expr that calls into the multi-type
    // inner macro. Exercises the `MacroDefReplacement::Expr(MacroCallExpr)`
    // arm of `try_expand_type_tuple_macro`.
    check(
        r#"
-define(INNER, integer(), atom()).
-define(OUTER, ?INNER).
-type foo() :: {?OUTER}.
"#,
        expect![[r#"
            -type foo() :: {
                erlang:integer(),
                erlang:atom()
            }.
        "#]],
    );
}

#[test]
fn tuple_type_with_macro_inside_multi_macro() {
    // Outer macro's `,`-separated body contains another macro call
    // whose own body is multi-type. Exercises the recursive `flat_map`
    // arm.
    check(
        r#"
-define(INNER, integer(), atom()).
-define(OUTER, first_atom, ?INNER).
-type foo() :: {?OUTER, last}.
"#,
        expect![[r#"
            -type foo() :: {
                first_atom,
                erlang:integer(),
                erlang:atom(),
                last
            }.
        "#]],
    );
}

#[test]
fn range_type() {
    check(
        r#"
-type foo() :: 1..100.
"#,
        expect![[r#"
            -type foo() :: (1..100).
        "#]],
    );
}

#[test]
fn map_type() {
    check(
        r#"
-type foo() :: #{a => b, c := d}.
"#,
        expect![[r#"
            -type foo() :: #{
                a => b,
                c := d
            }.
        "#]],
    );
}

#[test]
fn fun_type() {
    check(
        r#"
-type foo1() :: fun().
-type foo2() :: fun(() -> ok).
-type foo3() :: fun((a, b) -> ok).
-type foo4() :: fun((...) -> ok).
"#,
        expect![[r#"
            -type foo1() :: fun().

            -type foo2() :: fun(() -> ok).

            -type foo3() :: fun((a, b) -> ok).

            -type foo4() :: fun((...) -> ok).
        "#]],
    );
}

#[test]
fn union_type() {
    check(
        r#"
-type foo1() :: a | b.
-type foo2() :: a | b | c.
-type foo3() :: (a | b) | c.
"#,
        expect![[r#"
            -type foo1() :: (
                a |
                b
            ).

            -type foo2() :: (
                a |
                b |
                c
            ).

            -type foo3() :: (
                (
                    a |
                    b
                ) |
                c
            ).
        "#]],
    );
}

#[test]
fn var_type() {
    check(
        r#"
-type foo(A) :: A.
"#,
        expect![[r#"
            -type foo(A) :: A.
        "#]],
    );
}

#[test]
fn call_type() {
    check(
        r#"
-type local(A) :: local(A | integer()).
-type remote(A) :: module:remote(A | integer()).
"#,
        expect![[r#"
            -type local(A) :: local(
                (
                    A |
                    erlang:integer()
                )
            ).

            -type remote(A) :: module:remote(
                (
                    A |
                    erlang:integer()
                )
            ).
        "#]],
    );
}

#[test]
fn call_type_erlang_bif() {
    check(
        r#"
-type remote() :: erlang:pid() | pid().
"#,
        expect![[r#"
            -type remote() :: (
                erlang:pid() |
                erlang:pid()
            ).
        "#]],
    );
}

#[test]
fn record_type() {
    check(
        r#"
//- expect_parse_errors
-type foo1() :: #record{}.
-type foo2(B) :: #record{a :: integer(), b :: B}.
-type foo3() :: #record{a ::}.
"#,
        expect![[r#"
            -type foo1() :: #record{}.

            -type foo2(B) :: #record{
                a :: erlang:integer(),
                b :: B
            }.

            -type foo3() :: #record{
                a :: [missing]
            }.
        "#]],
    );
}

#[test]
fn invalid_type() {
    check(
        r#"
-type foo() :: catch 1.
"#,
        expect![[r#"
            -type foo() :: [missing].
        "#]],
    );
}

// Binary (bitstring) type expressions: `<<>>`, `<<_:M>>`, `<<_:_*N>>`,
// and `<<_:M, _:_*N>>`. The pretty printer always emits the canonical
// two-element form `<<_:M, _:_*N>>`.

#[test]
fn binary_type_empty() {
    check(
        r#"
-type foo() :: <<>>.
"#,
        expect![[r#"
            -type foo() :: <<_:0, _:_*0>>.
        "#]],
    );
}

#[test]
fn binary_type_fixed_size() {
    check(
        r#"
-type foo() :: <<_:8>>.
"#,
        expect![[r#"
            -type foo() :: <<_:8, _:_*0>>.
        "#]],
    );
}

#[test]
fn binary_type_unit() {
    check(
        r#"
-type foo() :: <<_:_*4>>.
"#,
        expect![[r#"
            -type foo() :: <<_:0, _:_*4>>.
        "#]],
    );
}

#[test]
fn binary_type_fixed_and_unit() {
    check(
        r#"
-type foo() :: <<_:8, _:_*1>>.
"#,
        expect![[r#"
            -type foo() :: <<_:8, _:_*1>>.
        "#]],
    );
}

#[test]
fn simple_spec() {
    check(
        r#"
-spec foo() -> ok.
"#,
        expect![[r#"
            -spec foo
                () -> ok.
        "#]],
    );
}

#[test]
fn simple_callback() {
    check(
        r#"
-callback foo() -> ok.
"#,
        expect![[r#"
            -callback foo
                () -> ok.
        "#]],
    );
}

#[test]
fn multi_sig_spec() {
    check(
        r#"
-spec foo(atom()) -> atom();
         (integer()) -> integer().
"#,
        expect![[r#"
            -spec foo
                (erlang:atom()) -> erlang:atom();
                (erlang:integer()) -> erlang:integer().
        "#]],
    );
}

#[test]
fn ann_var_spec() {
    check(
        r#"
-spec foo(A :: any()) -> ok.
"#,
        expect![[r#"
            -spec foo
                ((A  :: erlang:any())) -> ok.
        "#]],
    );
}

#[test]
fn guarded_spec() {
    check(
        r#"
-spec foo(A) -> A
    when A :: any().
"#,
        expect![[r#"
            -spec foo
                (A) -> A
                    when A :: erlang:any().
        "#]],
    );
}

#[test]
fn record_definition() {
    check(
        r#"
-record(foo, {}).
-record(foo, {field}).
-record(foo, {field = value}).
-record(foo, {field :: type}).
-record(foo, {field = value :: type}).
"#,
        expect![[r#"
            -record(foo, {
            }).

            -record(foo, {
                field
            }).

            -record(foo, {
                field = value
            }).

            -record(foo, {
                field :: type
            }).

            -record(foo, {
                field = value :: type
            }).
        "#]],
    );
}

#[test]
fn simple_term() {
    check(
        r#"
//- expect_parse_errors
-foo(ok).
-missing_value().
"#,
        expect![[r#"
            -foo(ok).

            -missing_value([missing]).
        "#]],
    );
}

#[test]
fn tuple_term() {
    check(
        r#"
-foo({1, 2, ok, "abc"}).
"#,
        expect![[r#"
            -foo({
                1,
                2,
                ok,
                "abc"
            }).
        "#]],
    );
}

#[test]
fn list_term() {
    check(
        r#"
-foo([]).
-bar([1, 2]).
-baz([1 | 2]).
"#,
        expect![[r#"
            -foo([]).

            -bar([
                1,
                2
            ]).

            -baz([
                1
                | 2
            ]).
        "#]],
    );
}

#[test]
fn map_term() {
    check(
        r#"
-foo(#{1 => 2}).
"#,
        expect![[r##"
            -foo(#{
                1 => 2
            }).
        "##]],
    );
}

#[test]
fn fun_term() {
    check(
        r#"
-foo(fun erlang:is_integer/1).
"#,
        expect![[r#"
            -foo(fun erlang:is_integer/1).
        "#]],
    );
}

#[test]
fn unary_op_term() {
    check(
        r#"
-foo(-(1)).
-foo(-(1.5)).
-foo(-$a).
-foo(+1).
-foo(+1.5).
-foo(+$a).
-foo(-atom).
"#,
        expect![[r#"
            -foo(-1).

            -foo(-1.5).

            -foo(-97).

            -foo(1).

            -foo(1.5).

            -foo($a).

            -foo([missing]).
        "#]],
    );
}

#[test]
fn binary_op_term() {
    check(
        r#"
-foo(foo/1).
-compile({inline, [foo/1]}).
-compile({a/a, 1/1}).
"#,
        expect![[r#"
            -foo({
                foo,
                1
            }).

            -compile({
                inline,
                [
                    {
                        foo,
                        1
                    }
                ]
            }).

            -compile({
                [missing],
                [missing]
            }).
        "#]],
    );
}

#[test]
fn binary_term() {
    check(
        r#"
-foo(<<"abc">>).
-bar(<<"abc", "def">>).
-baz(<<$a, $b, $c>>).
-foobar(<<1, 2, 3, -1>>).
"#,
        expect![[r#"
            -foo(<<"abc"/utf8>>).

            -bar(<<"abcdef"/utf8>>).

            -baz(<<"abc"/utf8>>).

            -foobar(<<1, 2, 3, 255>>).
        "#]],
    );
}

#[test]
fn expand_macro_function_clause() {
    check(
        r#"
-define(CLAUSE, foo(_) -> ok).

foo(1) -> 1;
?CLAUSE.
"#,
        expect![[r#"
            foo(1) ->
                1;
            foo(_) ->
                ok.
        "#]],
    );
}

#[test]
fn expand_macro_function_clause_new_fun() {
    check(
        r#"
-define(CLAUSE, bar(_) -> ok).

foo(1) -> 1.
?CLAUSE.
"#,
        expect![[r#"
            foo(1) ->
                1.

            bar(_) ->
                ok.
        "#]],
    );
}

#[test]
fn expand_macro_function_clause_with_params() {
    check(
        r#"
-define(CLAUSE(Res), foo(_) -> Res).

foo(1) -> 1;
?CLAUSE(ok).
"#,
        expect![[r#"
            foo(1) ->
                1;
            foo(_) ->
                ok.
        "#]],
    );
}

#[test]
fn expand_macro_function_multiple_clauses() {
    check(
        r#"
-define(CLAUSE(Res), foo(1) -> 1;
                     foo(_) -> Res).

?CLAUSE(ok).
"#,
        // We do not have the function name in the form list, so we
        // print the macro name. In HIR this is resolved properly.
        expect![[r#"
            foo(1) ->
                1;
            foo(_) ->
                ok.
        "#]],
    );
}

#[test]
fn expand_macro_function_multiple_files() {
    check(
        r#"
//- /src/main.erl
-include("main.hrl").
?CLAUSE(ok).
//- /src/main.hrl
-define(CLAUSE(Res), foo(1) -> 1;
                     foo(_) -> Res).

"#,
        // We do not have the function name in the form list, so we
        // print the macro name. In HIR this is resolved properly.
        expect![[r#"
            foo(1) ->
                1;
            foo(_) ->
                ok.
        "#]],
    );
}

#[test]
fn expand_macro_expr() {
    check(
        r#"
-define(EXPR, 1 + 2).

foo() -> ?EXPR.
"#,
        expect![[r#"
            foo() ->
                (1 + 2).
        "#]],
    );
}

// A macro whose body is a `,`-separated list of expressions, when used
// in a clause body, contributes each expression as a separate body
// expression (matching Erlang preprocessor semantics).

#[test]
fn expand_macro_multi_expr_body() {
    check(
        r#"
-define(MULTI(X), foo(X), bar(X), baz(X)).

run() -> ?MULTI(1).
"#,
        expect![[r#"
            run() ->
                foo(
                    1
                ),
                bar(
                    1
                ),
                baz(
                    1
                ).
        "#]],
    );
}

#[test]
fn expand_macro_multi_expr_body_with_surrounding() {
    // Multi-expression macros also work alongside other clause body
    // expressions.
    check(
        r#"
-define(MULTI(X), a(X), b(X)).

run(N) -> begin_thing, ?MULTI(N), end_thing.
"#,
        expect![[r#"
            run(N) ->
                begin_thing,
                a(
                    N
                ),
                b(
                    N
                ),
                end_thing.
        "#]],
    );
}

#[test]
fn expand_macro_multi_expr_two_invocations() {
    // Two separate multi-expression macro invocations in the same
    // clause body each contribute their own expressions.
    check(
        r#"
-define(PAIR(X), p1(X), p2(X)).

run() -> ?PAIR(1), ?PAIR(2).
"#,
        expect![[r#"
            run() ->
                p1(
                    1
                ),
                p2(
                    1
                ),
                p1(
                    2
                ),
                p2(
                    2
                ).
        "#]],
    );
}

// Recursive expansion of multi-expression macros: when a wrapper macro
// expands to a single inner macro call (or has an inner macro call
// inside its `,`-separated body), the inner expansion is recursively
// flattened into the surrounding clause body instead of producing a
// `missing` atom.

#[test]
fn expand_macro_wrapper_around_multi_expr() {
    // OUTER's body is a single Expr — a call to INNER. Without the
    // recursive `MacroDefReplacement::Expr(MacroCallExpr)` arm in
    // `try_expand_body_macro`, this would lower as a single missing
    // expression. With the arm, it re-enters expansion and produces
    // INNER's two body expressions.
    check(
        r#"
-define(INNER(A, B), e1(A), e2(B)).
-define(OUTER(X), ?INNER("prefix", X)).

run() -> ?OUTER(arg).
"#,
        expect![[r#"
            run() ->
                e1(
                    "prefix"
                ),
                e2(
                    arg
                ).
        "#]],
    );
}

#[test]
fn expand_macro_multi_expr_with_nested_macro_call() {
    // The outer macro body is `,`-separated, and one of the entries is
    // itself a multi-expression macro call. The recursive `flat_map`
    // arm in `try_expand_body_macro` flattens it.
    check(
        r#"
-define(INNER(X), bar(X), baz(X)).
-define(OUTER(X), foo, ?INNER(X)).

run() -> ?OUTER(arg).
"#,
        expect![[r#"
            run() ->
                foo,
                bar(
                    arg
                ),
                baz(
                    arg
                ).
        "#]],
    );
}

#[test]
fn expand_macro_three_levels_of_wrapping() {
    // Wrapping is recursive to arbitrary depth: A → B → C, where C
    // expands to a multi-expr body. Each step of the recursion goes
    // through the new `Expr(MacroCallExpr)` arm.
    check(
        r#"
-define(C(X), c1(X), c2(X)).
-define(B(X), ?C(X)).
-define(A(X), ?B(X)).

run() -> ?A(arg).
"#,
        expect![[r#"
            run() ->
                c1(
                    arg
                ),
                c2(
                    arg
                ).
        "#]],
    );
}

// Multi-expression macros must expand the same way wherever expressions
// are sequenced. The original support was in clause bodies; this commit
// extends it to `begin ... end` blocks, `try Body of ...`, and try
// `after Body end`.

#[test]
fn expand_macro_multi_expr_in_begin_end() {
    check(
        r#"
-define(MULTI(X), a(X), b(X)).

run() -> begin ?MULTI(1) end.
"#,
        expect![[r#"
            run() ->
                begin
                    a(
                        1
                    ),
                    b(
                        1
                    )
                end.
        "#]],
    );
}

#[test]
fn expand_macro_multi_expr_in_try_body() {
    check(
        r#"
-define(MULTI(X), a(X), b(X)).

run() -> try ?MULTI(1) of _ -> ok catch _:_ -> error end.
"#,
        expect![[r#"
            run() ->
                try
                    a(
                        1
                    ),
                    b(
                        1
                    )
                of
                    _ ->
                        ok
                catch
                    _:_ ->
                        error
                end.
        "#]],
    );
}

#[test]
fn expand_macro_multi_expr_in_try_after() {
    check(
        r#"
-define(MULTI(X), a(X), b(X)).

run() ->
    try
        x
    of
        _ -> ok
    after
        ?MULTI(1)
    end.
"#,
        expect![[r#"
            run() ->
                try
                    x
                of
                    _ ->
                        ok
                after
                    a(
                        1
                    ),
                    b(
                        1
                    )
                end.
        "#]],
    );
}

#[test]
fn expand_macro_multi_expr_in_block_with_surrounding() {
    // Multi-expr macro interleaved with literal expressions inside a
    // block — proves the new helper still threads non-macro expressions
    // through correctly.
    check(
        r#"
-define(MULTI(X), a(X), b(X)).

run() -> begin start, ?MULTI(1), finish end.
"#,
        expect![[r#"
            run() ->
                begin
                    start,
                    a(
                        1
                    ),
                    b(
                        1
                    ),
                    finish
                end.
        "#]],
    );
}

#[test]
fn expand_macro_var_in_expr() {
    check(
        r#"
-define(EXPR(X), 1 + X).

foo() -> ?EXPR(2).
"#,
        expect![[r#"
            foo() ->
                (1 + 2).
        "#]],
    );
}

#[test]
fn expand_macro_function() {
    check(
        r#"
-define(NAME, name).

foo() -> ?NAME(2).
"#,
        expect![[r#"
            foo() ->
                name(
                    2
                ).
        "#]],
    );
}

#[test]
fn expand_macro_remote_function() {
    check(
        r#"
-define(NAME, module:name).

foo() -> ?NAME(2).
"#,
        expect![[r#"
            foo() ->
                module:name(
                    2
                ).
        "#]],
    );
}

#[test]
fn expand_macro_pat() {
    check(
        r#"
-define(PAT, [_]).

foo(?PAT) -> ok.
"#,
        expect![[r#"
            foo([
                _
            ]) ->
                ok.
        "#]],
    );
}

#[test]
fn expand_macro_var_in_pat() {
    check(
        r#"
-define(PAT(X), [X]).

foo(?PAT(_)) -> ok.
"#,
        expect![[r#"
            foo([
                _
            ]) ->
                ok.
        "#]],
    );
}

#[test]
fn expand_macro_type() {
    check(
        r#"
-define(TY, a | b).

-type foo() :: ?TY.
"#,
        expect![[r#"
            -type foo() :: (
                a |
                b
            ).
        "#]],
    );
}

#[test]
fn expand_macro_type_call() {
    check(
        r#"
-define(NAME, name).

-type foo() :: ?NAME().
"#,
        expect![[r#"
            -type foo() :: name().
        "#]],
    );
}

#[test]
fn expand_macro_remote_type() {
    check(
        r#"
-define(NAME, module:name).

-type foo() :: ?NAME().
"#,
        expect![[r#"
            -type foo() :: module:name().
        "#]],
    );
}

#[test]
fn expand_macro_var_in_type() {
    check(
        r#"
-define(TY(X), a | X).

-type foo() :: ?TY(b).
"#,
        expect![[r#"
            -type foo() :: (
                a |
                b
            ).
        "#]],
    );
}

#[test]
fn expand_macro_term() {
    check(
        r#"
-define(TERM, [0, 1]).

-foo(?TERM).
"#,
        expect![[r#"
            -foo([
                0,
                1
            ]).
        "#]],
    );
}

#[test]
fn expand_macro_var_in_term() {
    check(
        r#"
-define(TERM(X), [0, X]).

-foo(?TERM(1)).
"#,
        expect![[r#"
            -foo([
                0,
                1
            ]).
        "#]],
    );
}

#[test]
fn expand_macro_cr_clause() {
    check(
        r#"
-define(CLAUSE(Pat, Expr), Pat -> Expr).

foo() ->
    case bar() of
        ?CLAUSE(ok, ok);
        ?CLAUSE(_, error)
    end.
"#,
        expect![[r#"
            foo() ->
                case bar() of
                    ok ->
                        ok;
                    _ ->
                        error
                end.
        "#]],
    );
}

// `ReplacementExprGuard` macros in case-clause patterns: a macro like
// `-define(NAME, Pattern when Guard1; Guard2; ...)` expands to a pattern
// plus a list of guard alternatives. The pattern is lowered normally and
// the guards are merged with the clause's own guards via
// `pending_macro_guards`.

#[test]
fn expand_macro_cr_clause_with_guards() {
    // Macro contributes pattern + two guard alternatives. The clause has
    // no own guards, so the macro's guards become the clause's guards.
    check(
        r#"
-define(DISCONNECT_ERROR, X when X =:= timeout; X =:= shutdown).

foo(X) ->
    case X of
        ?DISCONNECT_ERROR -> ok;
        _ -> error
    end.
"#,
        expect![[r#"
            foo(X) ->
                case X of
                    X when
                        (X =:= timeout);
                        (X =:= shutdown)
                    ->
                        ok;
                    _ ->
                        error
                end.
        "#]],
    );
}

#[test]
fn expand_macro_cr_clause_with_single_guard_alternative() {
    // Macro with a single guard (no `;`): just one alternative.
    check(
        r#"
-define(POS(N), N when N > 0).

foo(X) ->
    case X of
        ?POS(M) -> M;
        _ -> 0
    end.
"#,
        expect![[r#"
            foo(X) ->
                case X of
                    M when
                        (M > 0)
                    ->
                        M;
                    _ ->
                        0
                end.
        "#]],
    );
}

#[test]
fn expand_macro_cr_clause_with_guards_and_own_guard() {
    // OTP preprocessor does lexical substitution, so `?MACRO when extra`
    // produces `Pattern when MacroGuard when extra` — a syntax error.
    // The macro guards win; the clause's own guard is discarded.
    check(
        r#"
-define(DISCONNECT_ERROR, X when X =:= timeout; X =:= shutdown).

foo(X) ->
    case X of
        ?DISCONNECT_ERROR when is_atom(X) -> ok;
        _ -> error
    end.
"#,
        expect![[r#"
            foo(X) ->
                case X of
                    X when
                        (X =:= timeout);
                        (X =:= shutdown)
                    ->
                        ok;
                    _ ->
                        error
                end.
        "#]],
    );
}

#[test]
fn expand_macro_expr_guard_in_function_clause_no_leak() {
    // A ReplacementExprGuard macro in a function-clause pattern must NOT
    // leak its guards into a subsequent case clause.
    check(
        r#"
-define(DISCONNECT_ERROR, X when X =:= timeout; X =:= shutdown).

foo(?DISCONNECT_ERROR) ->
    case ok of
        A -> A;
        _ -> error
    end.
"#,
        expect![[r#"
            foo(X) ->
                case ok of
                    A ->
                        A;
                    _ ->
                        error
                end.
        "#]],
    );
}

#[test]
fn expand_macro_expr_guard_in_try_catch_no_leak() {
    // A ReplacementExprGuard macro in a try-catch reason pattern must NOT
    // leak its guards into a subsequent case clause.
    check(
        r#"
-define(DISCONNECT_ERROR, X when X =:= timeout; X =:= shutdown).

foo() ->
    try bar() catch
        error:?DISCONNECT_ERROR -> handle_error
    end,
    case ok of
        A -> A;
        _ -> error
    end.
"#,
        expect![[r#"
            foo() ->
                try
                    bar()
                catch
                    error:X ->
                        handle_error
                end,
                case ok of
                    A ->
                        A;
                    _ ->
                        error
                end.
        "#]],
    );
}

// Macro expansion in guard context uses textual substitution semantics.
// See body/lower.rs `lower_guard_clause`.

#[test]
fn expand_macro_guard_or_alternatives() {
    // `?IS_ABC(X)` expands to a `;`-separated list, producing three
    // guard alternatives.
    check(
        r#"
-define(IS_ABC(X), X =:= a; X =:= b; X =:= c).

foo(X) when ?IS_ABC(X) -> ok.
"#,
        expect![[r#"
            foo(X) when
                (X =:= a);
                (X =:= b);
                (X =:= c)
            ->
                ok.
        "#]],
    );
}

#[test]
fn expand_macro_guard_or_with_preceding_guards() {
    // Text substitution: `is_atom(X), ?IS_AB(X)` becomes
    // `is_atom(X), X =:= a; X =:= b` — preceding expressions only
    // attach to the first OR branch.
    check(
        r#"
-define(IS_AB(X), X =:= a; X =:= b).

foo(X) when is_atom(X), ?IS_AB(X) -> ok.
"#,
        expect![[r#"
            foo(X) when
                erlang:is_atom(
                    X
                ),
                (X =:= a);
                (X =:= b)
            ->
                ok.
        "#]],
    );
}

#[test]
fn expand_macro_guard_and_conjunction() {
    // `?CHECK(X)` expands to a `,`-separated list (AND), producing a
    // single guard with multiple conjuncts.
    check(
        r#"
-define(CHECK(X), is_integer(X), X > 0).

foo(X) when ?CHECK(X) -> ok.
"#,
        expect![[r#"
            foo(X) when
                erlang:is_integer(
                    X
                ),
                (X > 0)
            ->
                ok.
        "#]],
    );
}

#[test]
fn expand_macro_guard_or_with_trailing_guards() {
    // Text substitution: `?IS_AB(X), is_atom(X)` becomes
    // `X =:= a; X =:= b, is_atom(X)` — trailing expressions only
    // attach to the last OR branch.
    check(
        r#"
-define(IS_AB(X), X =:= a; X =:= b).

foo(X) when ?IS_AB(X), is_atom(X) -> ok.
"#,
        expect![[r#"
            foo(X) when
                (X =:= a);
                (X =:= b),
                erlang:is_atom(
                    X
                )
            ->
                ok.
        "#]],
    );
}

#[test]
fn expand_macro_guard_or_nested_wrapper() {
    // `?B(X)` expands to another macro call `?A(X)`, whose own body is a
    // `;`-separated guard-OR. The wrapper must recurse so the OR structure
    // is preserved rather than collapsing to `Missing`.
    check(
        r#"
-define(A(X), X =:= a; X =:= b).
-define(B(X), ?A(X)).

foo(X) when ?B(X) -> ok.
"#,
        expect![[r#"
            foo(X) when
                (X =:= a);
                (X =:= b)
            ->
                ok.
        "#]],
    );
}

#[test]
fn expand_macro_guard_and_nested_wrapper() {
    // Same wrapper indirection, but the inner macro is a `,`-separated
    // guard-AND conjunction.
    check(
        r#"
-define(A(X), is_integer(X), X > 0).
-define(B(X), ?A(X)).

foo(X) when ?B(X) -> ok.
"#,
        expect![[r#"
            foo(X) when
                erlang:is_integer(
                    X
                ),
                (X > 0)
            ->
                ok.
        "#]],
    );
}

#[test]
fn tree_print_guard_no_macro() {
    // Baseline: plain guards with disjunction and conjunction, no macros.
    check_ast(
        r#"
foo(X) when is_atom(X), X =/= undefined; is_integer(X) -> ok.
"#,
        expect![[r#"
            function: foo/1
            Clause {
                pats
                    Pat<0>:Pat::Var(X),
                guards
                    guard
                        Expr<4>:Expr::Call {
                            target
                                CallTarget::Remote {
                                    Expr<2>:Literal(Atom('erlang'))
                                    Expr<1>:Literal(Atom('is_atom'))
                                }
                            args
                                Expr<3>:Expr::Var(X),
                        },
                        Expr<7>:Expr::BinaryOp {
                            lhs
                                Expr<5>:Expr::Var(X)
                            rhs
                                Expr<6>:Literal(Atom('undefined'))
                            op
                                CompOp(Eq { strict: true, negated: true }),
                        },
                    guard
                        Expr<10>:Expr::Call {
                            target
                                CallTarget::Remote {
                                    Expr<2>:Literal(Atom('erlang'))
                                    Expr<8>:Literal(Atom('is_integer'))
                                }
                            args
                                Expr<9>:Expr::Var(X),
                        },
                exprs
                    Expr<11>:Literal(Atom('ok')),
            }.
        "#]],
    );
}

#[test]
fn tree_print_guard_macro_or_standalone() {
    // OR macro as the only guard expression — splices into multiple
    // disjunctive guard clauses.
    check_ast(
        r#"
-define(IS_ABC(X), X =:= a; X =:= b; X =:= c).

foo(X) when ?IS_ABC(X) -> ok.
"#,
        expect![[r#"
            -define(IS_ABC/1,
                Expr<0>:Expr::Missing
            ).
            function: foo/1
            Clause {
                pats
                    Pat<0>:Pat::Var(X),
                guards
                    guard
                        Expr<3>:Expr::BinaryOp {
                            lhs
                                Expr<1>:Expr::Var(X)
                            rhs
                                Expr<2>:Literal(Atom('a'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                    guard
                        Expr<6>:Expr::BinaryOp {
                            lhs
                                Expr<4>:Expr::Var(X)
                            rhs
                                Expr<5>:Literal(Atom('b'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                    guard
                        Expr<9>:Expr::BinaryOp {
                            lhs
                                Expr<7>:Expr::Var(X)
                            rhs
                                Expr<8>:Literal(Atom('c'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                exprs
                    Expr<10>:Literal(Atom('ok')),
            }.
        "#]],
    );
}

#[test]
fn tree_print_guard_macro_or_with_trailing() {
    // Text substitution: trailing expression attaches to last OR branch only.
    check_ast(
        r#"
-define(IS_AB(X), X =:= a; X =:= b).

foo(X) when ?IS_AB(X), is_atom(X) -> ok.
"#,
        expect![[r#"
            -define(IS_AB/1,
                Expr<0>:Expr::Missing
            ).
            function: foo/1
            Clause {
                pats
                    Pat<0>:Pat::Var(X),
                guards
                    guard
                        Expr<3>:Expr::BinaryOp {
                            lhs
                                Expr<1>:Expr::Var(X)
                            rhs
                                Expr<2>:Literal(Atom('a'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                    guard
                        Expr<6>:Expr::BinaryOp {
                            lhs
                                Expr<4>:Expr::Var(X)
                            rhs
                                Expr<5>:Literal(Atom('b'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                        Expr<10>:Expr::Call {
                            target
                                CallTarget::Remote {
                                    Expr<8>:Literal(Atom('erlang'))
                                    Expr<7>:Literal(Atom('is_atom'))
                                }
                            args
                                Expr<9>:Expr::Var(X),
                        },
                exprs
                    Expr<11>:Literal(Atom('ok')),
            }.
        "#]],
    );
}

#[test]
fn tree_print_guard_macro_or_with_preceding() {
    // Text substitution: preceding expression attaches to first OR branch only.
    check_ast(
        r#"
-define(IS_AB(X), X =:= a; X =:= b).

foo(X) when is_atom(X), ?IS_AB(X) -> ok.
"#,
        expect![[r#"
            -define(IS_AB/1,
                Expr<0>:Expr::Missing
            ).
            function: foo/1
            Clause {
                pats
                    Pat<0>:Pat::Var(X),
                guards
                    guard
                        Expr<4>:Expr::Call {
                            target
                                CallTarget::Remote {
                                    Expr<2>:Literal(Atom('erlang'))
                                    Expr<1>:Literal(Atom('is_atom'))
                                }
                            args
                                Expr<3>:Expr::Var(X),
                        },
                        Expr<7>:Expr::BinaryOp {
                            lhs
                                Expr<5>:Expr::Var(X)
                            rhs
                                Expr<6>:Literal(Atom('a'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                    guard
                        Expr<10>:Expr::BinaryOp {
                            lhs
                                Expr<8>:Expr::Var(X)
                            rhs
                                Expr<9>:Literal(Atom('b'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                exprs
                    Expr<11>:Literal(Atom('ok')),
            }.
        "#]],
    );
}

#[test]
fn tree_print_guard_macro_or_with_both() {
    // Text substitution with expressions on both sides of the OR macro.
    check_ast(
        r#"
-define(IS_AB(X), X =:= a; X =:= b).

foo(X) when is_list(X), ?IS_AB(X), is_atom(X) -> ok.
"#,
        expect![[r#"
            -define(IS_AB/1,
                Expr<0>:Expr::Missing
            ).
            function: foo/1
            Clause {
                pats
                    Pat<0>:Pat::Var(X),
                guards
                    guard
                        Expr<4>:Expr::Call {
                            target
                                CallTarget::Remote {
                                    Expr<2>:Literal(Atom('erlang'))
                                    Expr<1>:Literal(Atom('is_list'))
                                }
                            args
                                Expr<3>:Expr::Var(X),
                        },
                        Expr<7>:Expr::BinaryOp {
                            lhs
                                Expr<5>:Expr::Var(X)
                            rhs
                                Expr<6>:Literal(Atom('a'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                    guard
                        Expr<10>:Expr::BinaryOp {
                            lhs
                                Expr<8>:Expr::Var(X)
                            rhs
                                Expr<9>:Literal(Atom('b'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                        Expr<13>:Expr::Call {
                            target
                                CallTarget::Remote {
                                    Expr<2>:Literal(Atom('erlang'))
                                    Expr<11>:Literal(Atom('is_atom'))
                                }
                            args
                                Expr<12>:Expr::Var(X),
                        },
                exprs
                    Expr<14>:Literal(Atom('ok')),
            }.
        "#]],
    );
}

#[test]
fn tree_print_guard_macro_and() {
    // AND macro expands into a single guard clause with multiple conjuncts.
    check_ast(
        r#"
-define(CHECK(X), is_integer(X), X > 0).

foo(X) when ?CHECK(X) -> ok.
"#,
        expect![[r#"
            -define(CHECK/1,
                Expr<0>:Expr::Missing
            ).
            function: foo/1
            Clause {
                pats
                    Pat<0>:Pat::Var(X),
                guards
                    guard
                        Expr<4>:Expr::Call {
                            target
                                CallTarget::Remote {
                                    Expr<2>:Literal(Atom('erlang'))
                                    Expr<1>:Literal(Atom('is_integer'))
                                }
                            args
                                Expr<3>:Expr::Var(X),
                        },
                        Expr<7>:Expr::BinaryOp {
                            lhs
                                Expr<5>:Expr::Var(X)
                            rhs
                                Expr<6>:Literal(Integer(0))
                            op
                                CompOp(Ord { ordering: Greater, strict: true }),
                        },
                exprs
                    Expr<8>:Literal(Atom('ok')),
            }.
        "#]],
    );
}

#[test]
fn tree_print_guard_macro_mixed_or_and_explicit() {
    // OR macro combined with an explicit disjunction in the source.
    check_ast(
        r#"
-define(IS_AB(X), X =:= a; X =:= b).

foo(X) when ?IS_AB(X); is_list(X) -> ok.
"#,
        expect![[r#"
            -define(IS_AB/1,
                Expr<0>:Expr::Missing
            ).
            function: foo/1
            Clause {
                pats
                    Pat<0>:Pat::Var(X),
                guards
                    guard
                        Expr<3>:Expr::BinaryOp {
                            lhs
                                Expr<1>:Expr::Var(X)
                            rhs
                                Expr<2>:Literal(Atom('a'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                    guard
                        Expr<6>:Expr::BinaryOp {
                            lhs
                                Expr<4>:Expr::Var(X)
                            rhs
                                Expr<5>:Literal(Atom('b'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                    guard
                        Expr<10>:Expr::Call {
                            target
                                CallTarget::Remote {
                                    Expr<8>:Literal(Atom('erlang'))
                                    Expr<7>:Literal(Atom('is_list'))
                                }
                            args
                                Expr<9>:Expr::Var(X),
                        },
                exprs
                    Expr<11>:Literal(Atom('ok')),
            }.
        "#]],
    );
}

#[test]
fn tree_print_guard_macro_two_or_macros() {
    // Two consecutive OR macros — last branch of first merges with
    // first branch of second (text substitution semantics).
    check_ast(
        r#"
-define(AB(X), X =:= a; X =:= b).
-define(CD(X), X =:= c; X =:= d).

foo(X) when ?AB(X), ?CD(X) -> ok.
"#,
        expect![[r#"
            -define(AB/1,
                Expr<0>:Expr::Missing
            ).

            -define(CD/1,
                Expr<0>:Expr::Missing
            ).
            function: foo/1
            Clause {
                pats
                    Pat<0>:Pat::Var(X),
                guards
                    guard
                        Expr<3>:Expr::BinaryOp {
                            lhs
                                Expr<1>:Expr::Var(X)
                            rhs
                                Expr<2>:Literal(Atom('a'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                    guard
                        Expr<6>:Expr::BinaryOp {
                            lhs
                                Expr<4>:Expr::Var(X)
                            rhs
                                Expr<5>:Literal(Atom('b'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                        Expr<9>:Expr::BinaryOp {
                            lhs
                                Expr<7>:Expr::Var(X)
                            rhs
                                Expr<8>:Literal(Atom('c'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                    guard
                        Expr<12>:Expr::BinaryOp {
                            lhs
                                Expr<10>:Expr::Var(X)
                            rhs
                                Expr<11>:Literal(Atom('d'))
                            op
                                CompOp(Eq { strict: true, negated: false }),
                        },
                exprs
                    Expr<13>:Literal(Atom('ok')),
            }.
        "#]],
    );
}

#[test]
fn expand_macro_arity() {
    check(
        r#"
-define(ARITY(X), X).

foo() ->
    fun local/?ARITY(1),
    fun remote:function/?ARITY(2).
"#,
        expect![[r#"
            foo() ->
                fun local/1,
                fun remote:function/2.
        "#]],
    );
}

#[test]
fn expand_macro_name() {
    check(
        r#"
-define(NAME, name).

foo() ->
    #?NAME{?NAME = ?NAME}.
"#,
        expect![[r#"
            foo() ->
                #name{
                    name = name
                }.
        "#]],
    );
}

#[test]
fn expand_nested_macro() {
    check(
        r#"
-define(M1(A, B), {m1, ?M2(A, ?M3(B))}).
-define(M2(B, A), {m2, B, ?M3(A)}).
-define(M3(A), {m3, A}).

foo() ->
    ?M1(1, 2),
    ?M1(A, B).
"#,
        expect![[r#"
            foo() ->
                {
                    m1,
                    {
                        m2,
                        1,
                        {
                            m3,
                            {
                                m3,
                                2
                            }
                        }
                    }
                },
                {
                    m1,
                    {
                        m2,
                        A,
                        {
                            m3,
                            {
                                m3,
                                B
                            }
                        }
                    }
                }.
        "#]],
    );
}

#[test]
fn expand_built_in_function_name() {
    check(
        r#"
foo(?FUNCTION_NAME) -> ?FUNCTION_NAME.
"#,
        expect![[r#"
            foo(foo) ->
                foo.
        "#]],
    );

    check(
        r#"
foo() -> ?FUNCTION_NAME().
"#,
        expect![[r#"
            foo() ->
                foo().
        "#]],
    );
}

#[test]
fn expand_built_in_function_arity() {
    check(
        r#"
foo(?FUNCTION_ARITY) -> ?FUNCTION_ARITY.
"#,
        expect![[r#"
            foo(1) ->
                1.
        "#]],
    );
}

// OTP epp.erl bug compatibility for ?FUNCTION_ARITY:
// https://github.com/erlang/otp/issues/10705
//
// `epp:update_fun_name_1` scans tokens of the first clause to compute
// arity. It only flags the first argument when it sees an `other`
// (non-bracket, non-comma) token. So if the first clause's first
// argument is made entirely of brackets/commas (`[]`, `{}`, `<<>>`,
// `[[], {}]`, ...), epp under-counts the arity by one. ELP must
// reproduce that off-by-one so abstract forms match the compiler.
// Fixed in OTP 29: https://github.com/erlang/otp/issues/10705

fn epp_has_function_arity_bug() -> bool {
    elp_base_db::epp_has_function_arity_bug()
}

#[test]
fn epp_function_arity_bracket_only_first_arg_list() {
    if epp_has_function_arity_bug() {
        check(
            r#"
foo([]) -> ?FUNCTION_ARITY.
"#,
            expect![[r#"
                foo([]) ->
                    0.
            "#]],
        );
    } else {
        check(
            r#"
foo([]) -> ?FUNCTION_ARITY.
"#,
            expect![[r#"
                foo([]) ->
                    1.
            "#]],
        );
    }
}

#[test]
fn epp_function_arity_bracket_only_first_arg_tuple() {
    if epp_has_function_arity_bug() {
        check(
            r#"
foo({}) -> ?FUNCTION_ARITY.
"#,
            expect![[r#"
                foo({}) ->
                    0.
            "#]],
        );
    } else {
        check(
            r#"
foo({}) -> ?FUNCTION_ARITY.
"#,
            expect![[r#"
                foo({}) ->
                    1.
            "#]],
        );
    }
}

#[test]
fn epp_function_arity_bracket_only_first_arg_binary() {
    if epp_has_function_arity_bug() {
        check(
            r#"
foo(<<>>) -> ?FUNCTION_ARITY.
"#,
            expect![[r#"
                foo(<<>>) ->
                    0.
            "#]],
        );
    } else {
        check(
            r#"
foo(<<>>) -> ?FUNCTION_ARITY.
"#,
            expect![[r#"
                foo(<<>>) ->
                    1.
            "#]],
        );
    }
}

#[test]
fn epp_function_arity_bracket_only_nested() {
    if epp_has_function_arity_bug() {
        check(
            r#"
foo([[], {}]) -> ?FUNCTION_ARITY.
"#,
            expect![[r#"
                foo([
                    [],
                    {}
                ]) ->
                    0.
            "#]],
        );
    } else {
        check(
            r#"
foo([[], {}]) -> ?FUNCTION_ARITY.
"#,
            expect![[r#"
                foo([
                    [],
                    {}
                ]) ->
                    1.
            "#]],
        );
    }
}

#[test]
fn epp_function_arity_bracket_only_first_of_two_args() {
    if epp_has_function_arity_bug() {
        check(
            r#"
foo([], X) -> ?FUNCTION_ARITY.
"#,
            expect![[r#"
                foo([], X) ->
                    1.
            "#]],
        );
    } else {
        check(
            r#"
foo([], X) -> ?FUNCTION_ARITY.
"#,
            expect![[r#"
                foo([], X) ->
                    2.
            "#]],
        );
    }
}

#[test]
fn epp_function_arity_non_bracket_first_arg_unaffected() {
    check(
        r#"
foo(X, []) -> ?FUNCTION_ARITY.
"#,
        expect![[r#"
            foo(X, []) ->
                2.
        "#]],
    );
}

#[test]
fn epp_function_arity_uses_first_clause_only_buggy_first() {
    if epp_has_function_arity_bug() {
        check(
            r#"
foo([], 1) -> ?FUNCTION_ARITY;
foo(X, Y) -> ?FUNCTION_ARITY.
"#,
            expect![[r#"
                foo([], 1) ->
                    1;
                foo(X, Y) ->
                    1.
            "#]],
        );
    } else {
        check(
            r#"
foo([], 1) -> ?FUNCTION_ARITY;
foo(X, Y) -> ?FUNCTION_ARITY.
"#,
            expect![[r#"
                foo([], 1) ->
                    2;
                foo(X, Y) ->
                    2.
            "#]],
        );
    }
}

#[test]
fn epp_function_arity_uses_first_clause_only_clean_first() {
    check(
        r#"
foo(X, Y) -> ?FUNCTION_ARITY;
foo([], 1) -> ?FUNCTION_ARITY.
"#,
        expect![[r#"
            foo(X, Y) ->
                2;
            foo([], 1) ->
                2.
        "#]],
    );
}

#[test]
fn expand_built_in_line() {
    check(
        r#"
-type foo() :: ?LINE.

foo(?LINE) -> ?LINE.
"#,
        expect![[r#"
            -type foo() :: 0.

            foo(0) ->
                0.
        "#]],
    );
}

#[test]
fn expand_built_in_machine() {
    check(
        r#"
-type foo() :: ?MACHINE.

foo(?MACHINE) -> ?MACHINE.
"#,
        expect![[r#"
            -type foo() :: 'ELP'.

            foo('ELP') ->
                'ELP'.
        "#]],
    );
}

#[test]
fn expand_built_in_otp_release() {
    // OTP_RELEASE expands to the real OTP version if available, otherwise 2000
    let version = OTP_VERSION
        .as_ref()
        .and_then(|v| v.parse::<u32>().ok())
        .unwrap_or(2000);

    let (db, file_ids, _) = TestDB::with_many_files(
        r#"
-type foo() :: ?OTP_RELEASE.

foo(?OTP_RELEASE) -> ?OTP_RELEASE.
"#,
    );
    let file_id = file_ids[0];
    let form_list = db.file_form_list(file_id);
    let pretty: String = form_list
        .forms()
        .iter()
        .flat_map(|&form_idx| match form_idx {
            FormIdx::FunctionClause(function_id) => {
                let def_map = db.def_map(file_id);
                let function_def_id = InFile::new(file_id, FunctionDefId::new(function_id));
                if let Some(_fun_def) = def_map.get_by_function_id(&function_def_id) {
                    let function = &form_list[function_id];
                    let body = db.function_body(function_def_id);
                    Some(body.print(&db, function))
                } else {
                    None
                }
            }
            FormIdx::TypeAlias(type_alias_id) => {
                let type_alias = &form_list[type_alias_id];
                let body = db.type_body(InFile::new(file_id, type_alias_id));
                Some(body.print(&db, type_alias))
            }
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("");

    let expected = format!(
        "-type foo() :: {version}.

foo({version}) ->
    {version}."
    );
    assert_eq_expected!(expected.trim_end(), pretty.trim_start().trim_end());
}

#[test]
fn expand_built_in_module_no_attribute() {
    check(
        r#"
-type foo() :: ?MODULE.

foo(?MODULE) -> ?MODULE.
"#,
        expect![[r#"
            -type foo() :: [missing].

            foo([missing]) ->
                [missing].
        "#]],
    );
}

#[test]
fn expand_built_in_module() {
    check(
        r#"
-module(foobar).

-type foo() :: ?MODULE.

foo(?MODULE) -> ?MODULE.
"#,
        expect![[r#"
            -type foo() :: foobar.

            foo(foobar) ->
                foobar.
        "#]],
    );
}

#[test]
fn expand_built_in_module_string() {
    check(
        r#"
-module(foobar).

-type foo() :: ?MODULE_STRING.

foo(?MODULE_STRING) -> ?MODULE_STRING.
"#,
        expect![[r#"
            -type foo() :: "foobar".

            foo("foobar") ->
                "foobar".
        "#]],
    );
}

#[test]
fn expand_built_in_file() {
    check(
        r#"
-module(foobar).

-type foo() :: ?FILE.

foo(?FILE) -> ?FILE.
"#,
        expect![[r#"
            -type foo() :: "foobar.erl".

            foo("foobar.erl") ->
                "foobar.erl".
        "#]],
    );
}

#[test]
fn expand_recursive_macro() {
    check(
        r#"
-define(FOO, ?FOO).

foo(?FOO) -> ?FOO.
"#,
        expect![[r#"
            foo([missing]) ->
                [missing].
        "#]],
    );
}

#[test]
fn expand_mutually_recursive_macro() {
    check(
        r#"
-define(FOO, ?BAR).
-define(BAR, ?FOO).

foo(?FOO) -> ?BAR.
"#,
        expect![[r#"
            foo([missing]) ->
                [missing].
        "#]],
    );
}

#[test]
fn expand_aparently_argument_recursive_macro() {
    check(
        r#"
-define(FOO(X), X).

foo(?FOO(?FOO(1))) -> ?FOO(?FOO(1)).
"#,
        expect![[r#"
            foo(1) ->
                1.
        "#]],
    );

    check(
        r#"
-define(FOO(X), ?BAR(X)).
-define(BAR(X), X).

foo(?FOO(?BAR(?FOO(?BAR(1))))) -> ?FOO(?BAR(?FOO(?BAR(1)))).
"#,
        expect![[r#"
            foo(1) ->
                1.
        "#]],
    );
}

#[test]
fn maybe_simple() {
    check(
        r#"
foo() ->
maybe
    {ok, A} ?= a(),
    true = A >= 0,
    {ok, B} ?= b(),
    A + B
end."#,
        expect![[r#"
            foo() ->
                maybe
                    {
                        ok,
                        A
                    } ?= a(),
                    true = (A >= 0),
                    {
                        ok,
                        B
                    } ?= b(),
                    (A + B)
                end.
        "#]],
    );
}

#[test]
fn maybe_else() {
    check(
        r#"
foo() ->
maybe
    {ok, A} ?= a(),
    true = A >= 0,
    A
else
    error -> error;
    Other when Other == 0 -> error
end."#,
        expect![[r#"
            foo() ->
                maybe
                    {
                        ok,
                        A
                    } ?= a(),
                    true = (A >= 0),
                    A
                else
                    error ->
                        error;
                    Other when
                        (Other == 0)
                    ->
                        error
                end.
        "#]],
    );
}

#[test]
fn fundecl_clauses_1() {
    check(
        r#"
        foo(0) -> ok;
        foo(_) -> not_ok.
        "#,
        expect![[r#"
            foo(0) ->
                ok;
            foo(_) ->
                not_ok.
        "#]],
    );
}

#[test]
fn triple_quoted_strings_1() {
    check(
        r#"
        foo() ->
                 """
                 hello
                 there
                 """.
        "#,
        expect![[r#"
            foo() ->
                "\"\"\"\n         hello\n         there\n         \"\"\"".
        "#]],
    );
}

#[test]
fn triple_quoted_strings_2() {
    check(
        r#"
        foo() ->
                 """"""
                 hello
                  """
                 there
                 """""".
        "#,
        expect![[r#"
            foo() ->
                "\"\"\"\"\"\"\n         hello\n          \"\"\"\n         there\n         \"\"\"\"\"\"".
        "#]],
    );
}

// -------------------------------------

// Direct copy of expect_test::expect! macro, but allowing an expr not
// just a literal.
// This allows
// - Nice diffs on test failure
// - Proper location reporting for failing tests
macro_rules! my_expect {
    [$data:literal] => { $crate::expect![[$data]] };
    [[$data:expr]] => {expect_test::Expect {
        position: expect_test::Position {
            file: file!(),
            line: line!(),
            column: column!(),
        },
        data: $data,
        indent: true,
    }};
    [] => { expect_test::expect![[""]] };
    [[]] => { expect_test::expect![[""]] };
}

// -------------------------------------
// This section based on QB tests in
// https://github.com/erlang/otp/pull/7684/files#diff-c10f10e80ad43db595859b195d163b88a51785fdefaa66e191ecfdde5eab4448R60-R65

const QUOTED_BINARY_EXPECT: &str = r#"
            f() ->
                <<
                    "ab\"c\"\d"/utf8
                >>.
        "#;

#[test]
fn quoted_binary_no_sigil() {
    check(
        r#"
        f() -> <<"ab\"c\"\d"/utf8>>.

        "#,
        my_expect![[QUOTED_BINARY_EXPECT]],
    );
}

#[test]
fn quoted_binary_default_sigil() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        f() -> \~"ab\"c\"\d".
        "#,
        my_expect![[QUOTED_BINARY_EXPECT]],
    );
}

#[test]
fn quoted_binary_explicit_sigil() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        f() -> \~b"ab\"c\"\d".
        "#,
        my_expect![[QUOTED_BINARY_EXPECT]],
    );
}

#[test]
fn quoted_binary_explicit_sigil_in_tq_string() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        f() -> \~b"""
                ab"c"\d
                """.
        "#,
        my_expect![[QUOTED_BINARY_EXPECT]],
    );
}

// -------------------------------------
// This section based on VB tests in
// https://github.com/erlang/otp/pull/7684/files#diff-c10f10e80ad43db595859b195d163b88a51785fdefaa66e191ecfdde5eab4448R66-R73

const VERBATIM_BINARY_EXPECT: &str = r#"
        f() ->
            <<
                "ab\"c\"\\d"/utf8
            >>.
    "#;

#[test]
fn verbatim_binary_no_sigil() {
    check(
        r#"
        f() ->  <<"ab\"c\"\\d"/utf8>>.
        "#,
        my_expect![[VERBATIM_BINARY_EXPECT]],
    );
}

#[test]
fn verbatim_binary_with_sigil() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        f() -> \~B"ab\"c\"\d".
        "#,
        my_expect![[VERBATIM_BINARY_EXPECT]],
    );
}

#[test]
fn verbatim_binary_in_default_sigil_tq_string() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        f() -> \~"""
                ab"c"\d
                """.
        "#,
        my_expect![[VERBATIM_BINARY_EXPECT]],
    );
}

#[test]
fn verbatim_binary_in_verbatim_sigil_tq_string() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        f() -> \~B"""
                ab"c"\d
                """.
        "#,
        my_expect![[VERBATIM_BINARY_EXPECT]],
    );
}

// -------------------------------------
// This section based on QS tests in
// https://github.com/erlang/otp/pull/7684/files#diff-c10f10e80ad43db595859b195d163b88a51785fdefaa66e191ecfdde5eab4448R75-R79

const QUOTED_STRING_EXPECT: &str = r#"
            f() ->
                "ab\"c\"\d".
        "#;

#[test]
fn quoted_string_no_sigil() {
    check(
        r#"
        f() -> "ab\"c\"\d".
        "#,
        my_expect![[QUOTED_STRING_EXPECT]],
    );
}

#[test]
fn quoted_string_with_sigil() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        f() -> \~s"ab\"c\"\d".
        "#,
        my_expect![[QUOTED_STRING_EXPECT]],
    );
}

#[test]
fn quoted_string_with_sigil_in_tq_string() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        f() -> \~s"""
                ab"c"\d
                """.
        "#,
        my_expect![[QUOTED_STRING_EXPECT]],
    );
}

#[test]
fn quoted_s_sigil_does_not_double_unescape() {
    // Source `\\n` is two chars (backslash + n). With single-pass
    // unescape, that's what the runtime string contains, so the pretty
    // printer re-escapes to `\\n` (visible backslash + n). With the
    // pre-fix double-pass, the literal would have been a newline,
    // which pretty-prints as `\n`.
    check(
        r#"
        f() -> \~s"\\n".
        "#,
        expect![[r#"
            f() ->
                "\\n".
        "#]],
    );
}

#[test]
fn quoted_default_binary_sigil_does_not_double_unescape() {
    // Same regression for `~"..."` (default binary sigil).
    check(
        r#"
        f() -> \~"\\n".
        "#,
        expect![[r#"
            f() ->
                <<
                    "\\n"/utf8
                >>.
        "#]],
    );
}

#[test]
fn quoted_b_sigil_does_not_double_unescape() {
    // Same regression for `~b"..."` (explicit binary sigil).
    check(
        r#"
        f() -> \~b"\\n".
        "#,
        expect![[r#"
            f() ->
                <<
                    "\\n"/utf8
                >>.
        "#]],
    );
}

#[test]
fn quoted_s_sigil_double_backslash_is_one_backslash() {
    // `\\` in the source is one backslash in the runtime string —
    // single-pass unescape stops there. The pretty printer re-escapes
    // that single backslash to `\\`.
    check(
        r#"
        f() -> \~s"a\\b".
        "#,
        expect![[r#"
            f() ->
                "a\\b".
        "#]],
    );
}

#[test]
fn tq_b_sigil_still_unescapes() {
    // Triple-quoted `~b"""..."""` contents come back verbatim from
    // `trim_quotes_and_sigils`, so the lowering does still unescape
    // them (the `is_tq` branch). `\\n` inside a tq sigil should produce
    // backslash + n (the same final value as the single-quoted case,
    // but reached via the tq-only unescape path).
    check(
        r#"
        f() -> \~b"""
                a\\nb
                """.
        "#,
        expect![[r#"
            f() ->
                <<
                    "a\\nb"/utf8
                >>.
        "#]],
    );
}

// -------------------------------------
// This section based on VS tests in
// https://github.com/erlang/otp/pull/7684/files#diff-c10f10e80ad43db595859b195d163b88a51785fdefaa66e191ecfdde5eab4448R80-R84

const VERBATIM_STRING_EXPECT: &str = r#"
            f() ->
                "ab\"c\"\\d".
        "#;

#[test]
fn verbatim_string_no_sigil() {
    check(
        r#"
        f() -> "ab\"c\"\\d".
        "#,
        my_expect![[VERBATIM_STRING_EXPECT]],
    );
}

#[test]
fn verbatim_string_with_sigil() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        f() -> \~S"ab\"c\"\d".
        "#,
        my_expect![[VERBATIM_STRING_EXPECT]],
    );
}

#[test]
fn verbatim_string_with_sigil_in_tq_string() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        f() -> \~S"""
               ab"c"\d
               """.
        "#,
        my_expect![[VERBATIM_STRING_EXPECT]],
    );

    // End of verbatim string tests
    // -------------------------------------
}

#[test]
fn verbatim_string_with_sigil_in_tq_string_multi_delimiters() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        f() -> \~S"""""
                 """"
               ab"c"\d
               """"".
        "#,
        expect![[r#"
            f() ->
                "  \"\"\"\"\nab\"c\"\\d".
        "#]],
    );

    // End of verbatim string tests
    // -------------------------------------
}

#[test]
// Since we use a generic lowering thoroughly tested for `Expr`, we
// just do an existence test for `Pat`.
fn verbatim_binary_sigil_in_pat() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        f(\~B"ab\"c\"\d") -> ok.
        "#,
        expect![[r#"
            f(<<
                "ab\"c\"\\d"/utf8
            >>) ->
                ok.
        "#]],
    );
}

#[test]
// Since we use a generic lowering thoroughly tested for `Expr`, we
// just do an existence test for `TypeExpr`.
fn verbatim_binary_sigil_in_type() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        //- expect_parse_errors
        -type foo() :: \~B"ab\"c\"\d").
        -type bar() :: "hello").
        "#,
        expect![[r#"
            -type foo() :: [missing].

            -type bar() :: "hello".
        "#]],
    );
}

#[test]
// Since we use a generic lowering thoroughly tested for `Expr`, we
// just do an existence test for `Term`.
fn verbatim_binary_sigil_in_term() {
    // Note: \~ gets replaced by ~ in the fixture parsing
    check(
        r#"
        -wild(\~B"ab\"c\"\d").
        "#,
        expect![[r#"
            -wild(<<"ab\"c\"\\d"/utf8>>).
        "#]],
    );
}

#[test]
fn lowering_with_error_nodes() {
    check(
        r#"
         //- expect_parse_errors
            f(1a) -> ok begin 1 end.
        "#,
        expect![[r#"
            f(a) ->
                ok.
        "#]],
    );
}
// ---------------------------------------------------------------------
// Tree printing starts

#[test]
fn tree_print_function() {
    check_ast(
        r#"
        foo(0) -> ok;
        foo(_) -> error.
        "#,
        expect![[r#"
            function: foo/1
            Clause {
                pats
                    Pat<0>:Literal(Integer(0)),
                guards
                exprs
                    Expr<1>:Literal(Atom('ok')),
            };
            Clause {
                pats
                    Pat<0>:Pat::Var(_),
                guards
                exprs
                    Expr<1>:Literal(Atom('error')),
            }.
        "#]],
    );
}

#[test]
fn tree_print_function_with_ranges() {
    let (db, file_ids, _) = TestDB::with_many_files(
        r#"
        foo( (((A))) ) -> ((3)).
        "#,
    );
    let file_id = file_ids[0];
    let form_list = db.file_form_list(file_id);
    let strategy = Strategy {
        macros: MacroStrategy::ExpandButIncludeMacroCall,
        parens: ParenStrategy::VisibleParens,
    };
    let pretty = form_list
        .forms()
        .iter()
        .flat_map(|&form_idx| match form_idx {
            FormIdx::FunctionClause(function_id) => {
                let def_map = db.def_map(file_id);
                let function_def_id = InFile::new(file_id, FunctionDefId::new(function_id));
                if let Some(_fun_def) = def_map.get_by_function_id(&function_def_id) {
                    let (body, source_maps) = db.function_body_with_source(function_def_id);
                    let mut out = String::new();
                    if let Some((_, clause)) = body.clauses.iter().next() {
                        if let Some(na) = clause.name.clone() {
                            writeln!(out, "function: {na}").ok();
                        }
                        if let Some(source_map) = source_maps.first() {
                            write!(
                                out,
                                "{}",
                                clause.tree_print_with_range(strategy, source_map)
                            )
                            .ok();
                        }
                    }
                    writeln!(out, ".").ok();
                    Some(out)
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("");
    expect![[r#"
        function: foo/1

        Clause {
            pats
                Pat<3>:Pat::Paren { (range: 5..12)
                    Pat<2>:Pat::Paren { (range: 6..11)
                        Pat<1>:Pat::Paren { (range: 7..10)
                            Pat<0>:Pat::Var(A)}}},
            guards
            exprs
                Expr<3>:Expr::Paren { (range: 18..23)
                    Expr<2>:Expr::Paren { (range: 19..22)
                        Expr<1>:Literal(Integer(3))
                    }
                },
        }
        .
    "#]]
    .assert_eq(pretty.trim_start());
}

#[test]
fn tree_print_type_body() {
    check_ast(
        r#"
        -type foo() :: ok | error.
        -opaque foo() :: term().
        -nominal bar() :: {ok}.
        "#,
        expect![[r#"
            -type foo() :: TypeExpr::Union {
                Literal(Atom('ok')),
                Literal(Atom('error')),
            }.

            -opaque foo() :: TypeExpr::Call {
                target
                    CallTarget::Remote {
                        Literal(Atom('erlang'))
                        Literal(Atom('term'))
                    }
                args
                    ()
            }.

            -nominal bar() :: TypeExpr::Tuple {
                Literal(Atom('ok')),
            }.
        "#]],
    );
}

#[test]
fn tree_print_binary_type() {
    check_ast(
        r#"
        -type a() :: <<>>.
        -type b() :: <<_:8>>.
        -type c() :: <<_:_*4>>.
        -type d() :: <<_:8, _:_*1>>.
        "#,
        expect![[r#"
            -type a() :: TypeExpr::Bitstring(0, 0).

            -type b() :: TypeExpr::Bitstring(8, 0).

            -type c() :: TypeExpr::Bitstring(0, 4).

            -type d() :: TypeExpr::Bitstring(8, 1).
        "#]],
    );
}

#[test]
fn tree_print_spec_or_callback() {
    check_ast(
        r#"
        -spec new(string() | binary()) -> pid().
        -callback foo() -> ok.
        "#,
        expect![[r#"
            -spec new
                (
                    TypeExpr::Union {
                        TypeExpr::Call {
                            target
                                CallTarget::Remote {
                                    Literal(Atom('erlang'))
                                    Literal(Atom('string'))
                                }
                            args
                                ()
                        },
                        TypeExpr::Call {
                            target
                                CallTarget::Remote {
                                    Literal(Atom('erlang'))
                                    Literal(Atom('binary'))
                                }
                            args
                                ()
                        },
                    }
                ) ->
                    TypeExpr::Call {
                        target
                            CallTarget::Remote {
                                Literal(Atom('erlang'))
                                Literal(Atom('pid'))
                            }
                        args
                            ()
                    }.

            -callback foo
                () ->
                    Literal(Atom('ok')).
        "#]],
    );
}

#[test]
fn tree_print_record() {
    check_ast(
        r#"
         -record(foo, {}).
         -record(foo, {field}).
         -record(foo, {field1, field2}).
         -record(foo, {field = value}).
         -record(foo, {field :: type}).
         -record(foo, {field = value :: type}).
        "#,
        expect![[r#"
            -record(foo,
                fields
            ).

            -record(foo,
                fields
                    RecordFieldBody {
                        field_id
                            Idx::<RecordField>(0)
                        expr
                        ty
                    },
            ).

            -record(foo,
                fields
                    RecordFieldBody {
                        field_id
                            Idx::<RecordField>(1)
                        expr
                        ty
                    },
                    RecordFieldBody {
                        field_id
                            Idx::<RecordField>(2)
                        expr
                        ty
                    },
            ).

            -record(foo,
                fields
                    RecordFieldBody {
                        field_id
                            Idx::<RecordField>(3)
                        expr
                            Expr<0>:Literal(Atom('value')),
                        ty
                    },
            ).

            -record(foo,
                fields
                    RecordFieldBody {
                        field_id
                            Idx::<RecordField>(4)
                        expr
                        ty
                            Literal(Atom('type')),
                    },
            ).

            -record(foo,
                fields
                    RecordFieldBody {
                        field_id
                            Idx::<RecordField>(5)
                        expr
                            Expr<0>:Literal(Atom('value')),
                        ty
                            Literal(Atom('type')),
                    },
            ).
        "#]],
    );
}

#[test]
fn tree_print_attribute() {
    // TODO: fix wild attribute parsing, T246546041, to remove expect_parse_errors
    check_ast(
        r#"
        //- expect_parse_errors
         -wild(foo, []).
         -compile({inline, [foo/1]}).
         -compile({a/a, 1/1}).
        "#,
        expect![[r#"
            -wild(
                Term::List {
                    exprs
                    tail
                }
            ).

            -compile(
                Term::Tuple {
                    Literal(Atom('inline')),
                    Term::List {
                        exprs
                            Term::Tuple {
                                Literal(Atom('foo')),
                                Literal(Integer(1)),
                            },
                        tail
                    },
                }
            ).

            -compile(
                Term::Tuple {
                    Term::Missing,
                    Term::Missing,
                }
            ).
        "#]],
    );
}

#[test]
fn tree_print_define_expr() {
    check_ast(
        r#"
         -define(FOO, ok).
        "#,
        expect![[r#"
            -define(FOO,
                Expr<0>:Literal(Atom('ok'))
            ).
        "#]],
    );
}

#[test]
fn tree_print_define_missing() {
    check_ast(
        r#"
         -define(FOO(X), is_integer(X), X > 1).
        "#,
        expect![[r#"
            -define(FOO/1,
                Expr<0>:Expr::Missing
            ).
        "#]],
    );
}

/// Demonstrates that function bodies resolve macros using point-in-file
/// state rather than end-of-file state. `foo/0` sees the first definition
/// of `?VAL` (→ `old`) while `bar/0` sees the second (→ `new`).
///
/// Without the `set_macro_defs_from_preprocessor` calls in body lowering,
/// both functions would resolve `?VAL` to the final (second) definition,
/// producing `new` in both bodies.
#[test]
fn tree_print_macro_redefine_point_in_file() {
    check_ast(
        r#"
-define(VAL, old).
foo() -> ?VAL.
-undef(VAL).
-define(VAL, new).
bar() -> ?VAL.
"#,
        expect![[r#"
            -define(VAL,
                Expr<0>:Literal(Atom('old'))
            ).
            function: foo/0
            Clause {
                pats
                guards
                exprs
                    Expr<2>:Expr::MacroCall {
                        args
                        macro_def
                            Some(InFile { file_id: FileId(0), value: Idx::<Define>(0) })
                        expansion
                            Expr<1>:Literal(Atom('old'))
                    },
            }.

            -define(VAL,
                Expr<0>:Literal(Atom('new'))
            ).
            function: bar/0
            Clause {
                pats
                guards
                exprs
                    Expr<2>:Expr::MacroCall {
                        args
                        macro_def
                            Some(InFile { file_id: FileId(0), value: Idx::<Define>(1) })
                        expansion
                            Expr<1>:Literal(Atom('new'))
                    },
            }.
        "#]],
    );
}

/// Demonstrates that type bodies resolve macros using point-in-file
/// state rather than end-of-file state. `-type foo()` sees the first
/// definition of `?T` (→ `old`) while `-type bar()` sees the second
/// (→ `new`).
///
/// Without the `set_macro_defs_from_preprocessor` call in
/// `type_body_with_source_query`, both type aliases would resolve `?T`
/// to the final (second) definition, producing `new` in both bodies.
#[test]
fn tree_print_type_macro_redefine_point_in_file() {
    check_ast(
        r#"
-define(T, old).
-type foo() :: ?T.
-undef(T).
-define(T, new).
-type bar() :: ?T.
"#,
        expect![[r#"
            -define(T,
                Expr<0>:Literal(Atom('old'))
            ).

            -type foo() :: Literal(Atom('old')).

            -define(T,
                Expr<0>:Literal(Atom('new'))
            ).

            -type bar() :: Literal(Atom('new')).
        "#]],
    );
}

/// Demonstrates that define bodies resolve macros using point-in-file
/// state rather than end-of-file state. `-define(A, ?VAL)` sees the
/// first definition of `?VAL` (→ `old`) while `-define(B, ?VAL)` sees
/// the second (→ `new`).
///
/// Without the `set_macro_defs_from_preprocessor` call in
/// `define_body_with_source_query`, both define bodies would resolve
/// `?VAL` to the final (second) definition, producing `new` in both.
#[test]
fn tree_print_define_macro_redefine_point_in_file() {
    check_ast(
        r#"
-define(VAL, old).
-define(A, ?VAL).
-undef(VAL).
-define(VAL, new).
-define(B, ?VAL).
"#,
        expect![[r#"
            -define(VAL,
                Expr<0>:Literal(Atom('old'))
            ).

            -define(A,
                Expr<1>:Literal(Atom('old'))
            ).

            -define(VAL,
                Expr<0>:Literal(Atom('new'))
            ).

            -define(B,
                Expr<1>:Literal(Atom('new'))
            ).
        "#]],
    );
}

/// Demonstrates that spec bodies resolve macros using point-in-file
/// state rather than end-of-file state. `-spec foo()` sees the first
/// definition of `?RET` (→ `old`) while `-spec bar()` sees the second
/// (→ `new`).
///
/// Without the `set_macro_defs_from_preprocessor` call in
/// `spec_body_with_source_query`, both specs would resolve `?RET` to
/// the final (second) definition, producing `new` in both.
#[test]
fn tree_print_spec_macro_redefine_point_in_file() {
    check_ast(
        r#"
-define(RET, old).
-spec foo() -> ?RET.
foo() -> ok.
-undef(RET).
-define(RET, new).
-spec bar() -> ?RET.
bar() -> ok.
"#,
        expect![[r#"
            -define(RET,
                Expr<0>:Literal(Atom('old'))
            ).

            -spec foo
                () ->
                    Literal(Atom('old')).
            function: foo/0
            Clause {
                pats
                guards
                exprs
                    Expr<1>:Literal(Atom('ok')),
            }.

            -define(RET,
                Expr<0>:Literal(Atom('new'))
            ).

            -spec bar
                () ->
                    Literal(Atom('new')).
            function: bar/0
            Clause {
                pats
                guards
                exprs
                    Expr<1>:Literal(Atom('ok')),
            }.
        "#]],
    );
}

/// Demonstrates that callback bodies resolve macros using point-in-file
/// state rather than end-of-file state. The first `-callback` sees the
/// first definition of `?RET` (→ `old`) while the second sees the
/// redefined version (→ `new`).
///
/// Without the `set_macro_defs_from_preprocessor` call in
/// `callback_body_with_source_query`, both callbacks would resolve
/// `?RET` to the final (second) definition, producing `new` in both.
#[test]
fn tree_print_callback_macro_redefine_point_in_file() {
    check_ast(
        r#"
-define(RET, old).
-callback foo() -> ?RET.
-undef(RET).
-define(RET, new).
-callback bar() -> ?RET.
"#,
        expect![[r#"
            -define(RET,
                Expr<0>:Literal(Atom('old'))
            ).

            -callback foo
                () ->
                    Literal(Atom('old')).

            -define(RET,
                Expr<0>:Literal(Atom('new'))
            ).

            -callback bar
                () ->
                    Literal(Atom('new')).
        "#]],
    );
}

/// Demonstrates that record bodies resolve macros using point-in-file
/// state rather than end-of-file state. `-record(r1, ...)` sees the
/// first definition of `?DEFAULT` (→ `old`) while `-record(r2, ...)`
/// sees the second (→ `new`).
///
/// Without the `set_macro_defs_from_preprocessor` call in
/// `record_body_with_source_query`, both records would resolve
/// `?DEFAULT` to the final (second) definition, producing `new` in
/// both.
#[test]
fn tree_print_record_macro_redefine_point_in_file() {
    check_ast(
        r#"
-define(DEFAULT, old).
-record(r1, {field = ?DEFAULT}).
-undef(DEFAULT).
-define(DEFAULT, new).
-record(r2, {field = ?DEFAULT}).
"#,
        expect![[r#"
            -define(DEFAULT,
                Expr<0>:Literal(Atom('old'))
            ).

            -record(r1,
                fields
                    RecordFieldBody {
                        field_id
                            Idx::<RecordField>(0)
                        expr
                            Expr<1>:Literal(Atom('old')),
                        ty
                    },
            ).

            -define(DEFAULT,
                Expr<0>:Literal(Atom('new'))
            ).

            -record(r2,
                fields
                    RecordFieldBody {
                        field_id
                            Idx::<RecordField>(1)
                        expr
                            Expr<1>:Literal(Atom('new')),
                        ty
                    },
            ).
        "#]],
    );
}

/// Demonstrates that attribute bodies resolve macros using point-in-file
/// state rather than end-of-file state. The first `-compile(...)` sees
/// the first definition of `?OPTS` (→ `[old]`) while the second sees
/// the redefined version (→ `[new]`).
///
/// Without the `set_macro_defs_from_preprocessor` call in
/// `compile_body_with_source_query`, both compile attributes would
/// resolve `?OPTS` to the final (second) definition, producing `[new]`
/// in both.
#[test]
fn tree_print_attribute_macro_redefine_point_in_file() {
    check_ast(
        r#"
-define(OPTS, [old]).
-compile(?OPTS).
-undef(OPTS).
-define(OPTS, [new]).
-compile(?OPTS).
"#,
        expect![[r#"
            -define(OPTS,
                Expr<1>:Expr::List {
                    exprs
                        Expr<0>:Literal(Atom('old')),
                    tail
                }
            ).

            -compile(
                Term::List {
                    exprs
                        Literal(Atom('old')),
                    tail
                }
            ).

            -define(OPTS,
                Expr<1>:Expr::List {
                    exprs
                        Expr<0>:Literal(Atom('new')),
                    tail
                }
            ).

            -compile(
                Term::List {
                    exprs
                        Literal(Atom('new')),
                    tail
                }
            ).
        "#]],
    );
}

/// Demonstrates that doc attribute bodies resolve macros using point-in-file
/// state rather than end-of-file state. The first `-doc ?DOC` sees the
/// first definition of `?DOC` (→ `"old doc"`) while the second sees the
/// redefined version (→ `"new doc"`).
///
/// Without the `set_macro_defs_from_preprocessor` call in
/// `doc_body_with_source_query`, both doc attributes would resolve `?DOC`
/// to the final (second) definition, producing `"new doc"` in both.
#[test]
fn tree_print_doc_macro_redefine_point_in_file() {
    check_ast(
        r#"
-define(DOC, "old doc").
-doc ?DOC.
foo() -> ok.
-undef(DOC).
-define(DOC, "new doc").
-doc ?DOC.
bar() -> ok.
"#,
        expect![[r#"
            -define(DOC,
                Expr<0>:Literal(String(Normal("old doc")))
            ).
            function: foo/0
            Clause {
                pats
                guards
                exprs
                    Expr<1>:Literal(Atom('ok')),
            }.

            -define(DOC,
                Expr<0>:Literal(String(Normal("new doc")))
            ).
            function: bar/0
            Clause {
                pats
                guards
                exprs
                    Expr<1>:Literal(Atom('ok')),
            }.
        "#]],
    );
}

// Tree printing ends
// ---------------------------------------------------------------------

// ---------------------------------------------------------------------
// Native records (EEP 79)

#[test]
fn native_record_qualified_create() {
    check(
        r#"
foo() -> #mod:name{a = 1}.
"#,
        expect![[r#"
            foo() ->
                #mod:name{
                    a = 1
                }.
        "#]],
    );
}

#[test]
fn native_record_qualified_update() {
    check(
        r#"
foo(X) -> X#mod:name{a = 1, b = 3}.
"#,
        expect![[r#"
            foo(X) ->
                X#mod:name{
                    a = 1,
                    b = 3
                }.
        "#]],
    );
}

#[test]
fn native_record_qualified_field_access() {
    check(
        r#"
foo(X) -> X#mod:name.field.
"#,
        expect![[r#"
            foo(X) ->
                X#mod:name.field.
        "#]],
    );
}

#[test]
fn native_record_anon_create() {
    check(
        r#"
foo() -> #_{a = 1, b = 2}.
"#,
        expect![[r#"
            foo() ->
                #_{
                    a = 1,
                    b = 2
                }.
        "#]],
    );
}

#[test]
fn native_record_anon_update() {
    check(
        r#"
foo(X) -> X#_{a = 1}.
"#,
        expect![[r#"
            foo(X) ->
                X#_{
                    a = 1
                }.
        "#]],
    );
}

#[test]
fn native_record_anon_field_access() {
    check(
        r#"
foo(X) -> X#_.field.
"#,
        expect![[r#"
            foo(X) ->
                X#_.field.
        "#]],
    );
}

#[test]
fn native_record_qualified_in_pat() {
    check(
        r#"
foo(#mod:name{a = A}) -> A.
"#,
        expect![[r#"
            foo(#mod:name{
                a = A
            }) ->
                A.
        "#]],
    );
}

#[test]
fn native_record_anon_in_pat() {
    check(
        r#"
foo(#_{a = A}) -> A.
"#,
        expect![[r#"
            foo(#_{
                a = A
            }) ->
                A.
        "#]],
    );
}

#[test]
fn native_record_qualified_in_type() {
    check(
        r#"
-type foo() :: #mod:name{a :: integer()}.
"#,
        expect![[r#"
            -type foo() :: #mod:name{
                a :: erlang:integer()
            }.
        "#]],
    );
}

#[test]
fn native_record_anon_in_type() {
    check(
        r#"
-type foo() :: #_{a :: integer()}.
"#,
        expect![[r#"
            -type foo() :: #_{
                a :: erlang:integer()
            }.
        "#]],
    );
}

// ---------------------------------------------------------------------
// CallTarget `unqualified` flag
//
// `unqualified: true` is set only when a bare local-looking call gets resolved
// to a remote call via an auto-imported BIF (`length(_)` -> `erlang:length(_)`)
// or a `-import` directive. Explicitly-qualified remote calls and ordinary
// local calls must keep `unqualified: false`.

/// Find the first `Expr::Call` in the body of the (only-clause) function `name/arity`.
#[track_caller]
fn first_call_target(
    db: &TestDB,
    file_id: elp_base_db::FileId,
    name: &str,
    arity: u32,
) -> CallTarget<crate::ExprId> {
    let def_map = db.def_map(file_id);
    let na = crate::NameArity::new(crate::Name::from_erlang_service(name), arity);
    let fun_def = def_map
        .get_function(&na)
        .unwrap_or_else(|| panic!("function {name}/{arity} not in def map"));
    let body = db.function_body(InFile::new(file_id, fun_def.function_id));
    let (_, clause) = body.clauses.iter().next().expect("function has no clause");
    clause
        .body
        .exprs
        .iter()
        .find_map(|(_, e)| match e {
            Expr::Call { target, .. } => Some(target.clone()),
            _ => None,
        })
        .unwrap_or_else(|| panic!("no Expr::Call in body of {name}/{arity}"))
}

#[track_caller]
fn atom_of(body: &crate::Body, expr_id: crate::ExprId) -> String {
    match &body.exprs[expr_id] {
        Expr::Literal(Literal::Atom(a)) => a.as_string(),
        other => panic!("expected atom literal, got {other:?}"),
    }
}

#[test]
fn call_target_unqualified_for_bif_auto_import() {
    // `length([])` is an auto-imported BIF — lowering should rewrite it to a
    // `Remote` target with `unqualified: true`.
    let (db, file_ids, _) = TestDB::with_many_files(
        r#"
            bif_call() -> length([]).
            explicit_call() -> erlang:length([]).
            local_call() -> bif_call().
        "#,
    );
    let file_id = file_ids[0];

    let bif_target = first_call_target(&db, file_id, "bif_call", 0);
    let body = {
        let def_map = db.def_map(file_id);
        let na = crate::NameArity::new(crate::Name::from_erlang_service("bif_call"), 0);
        let fun_def = def_map.get_function(&na).unwrap();
        db.function_body(InFile::new(file_id, fun_def.function_id))
    };
    let (_, clause) = body.clauses.iter().next().unwrap();
    match &bif_target {
        CallTarget::Remote {
            module,
            name,
            unqualified,
            ..
        } => {
            assert!(*unqualified, "BIF call should set unqualified = true");
            assert_eq_expected!("erlang", atom_of(&clause.body, *module));
            assert_eq_expected!("length", atom_of(&clause.body, *name));
        }
        other => panic!("expected CallTarget::Remote for BIF, got {other:?}"),
    }

    // `erlang:length([])` is written explicitly — `unqualified` must stay false.
    let explicit_target = first_call_target(&db, file_id, "explicit_call", 0);
    match &explicit_target {
        CallTarget::Remote { unqualified, .. } => {
            assert!(
                !*unqualified,
                "explicit remote call must keep unqualified = false"
            );
        }
        other => panic!("expected CallTarget::Remote for explicit call, got {other:?}"),
    }

    // A bare call to a user-defined function in the same module stays Local.
    let local_target = first_call_target(&db, file_id, "local_call", 0);
    assert!(
        matches!(local_target, CallTarget::Local { .. }),
        "user-defined local call should be CallTarget::Local, got {local_target:?}"
    );
}

#[test]
fn call_target_no_auto_import_disables_bif_rewrite() {
    // `now/0` is an auto-imported BIF (`erlang:now/0`), but the module disables
    // the auto-import via `-compile({no_auto_import, [now/0]})`, so the
    // unqualified call must lower to `CallTarget::Local` rather than a remote
    // `erlang:now/0` call.
    let (db, file_ids, _) = TestDB::with_many_files(
        r#"
            -module(main).
            -compile({no_auto_import, [now/0]}).
            -export([now/0, later/0]).
            now() -> really_now.
            later() -> {now(), but_later}.
        "#,
    );
    let file_id = file_ids[0];

    let target = first_call_target(&db, file_id, "later", 0);
    assert!(
        matches!(target, CallTarget::Local { .. }),
        "no_auto_import BIF call should be CallTarget::Local, got {target:?}"
    );
}

#[test]
fn call_target_bif_rewrite_without_no_auto_import() {
    // Without `no_auto_import`, an unqualified call to an auto-imported BIF is
    // still rewritten to a remote `erlang:F/A` call even when the module
    // defines a clashing local function. That ambiguous case is surfaced
    // separately by the compiler's "ambiguous call" warning, so the lowering
    // behaviour here is intentionally left unchanged.
    let (db, file_ids, _) = TestDB::with_many_files(
        r#"
            -module(main).
            -export([now/0, later/0]).
            now() -> really_now.
            later() -> {now(), but_later}.
        "#,
    );
    let file_id = file_ids[0];

    let target = first_call_target(&db, file_id, "later", 0);
    match &target {
        CallTarget::Remote {
            name, unqualified, ..
        } => {
            assert!(*unqualified, "BIF call should set unqualified = true");
            let body = {
                let def_map = db.def_map(file_id);
                let na = crate::NameArity::new(crate::Name::from_erlang_service("later"), 0);
                let fun_def = def_map.get_function(&na).unwrap();
                db.function_body(InFile::new(file_id, fun_def.function_id))
            };
            let (_, clause) = body.clauses.iter().next().unwrap();
            assert_eq_expected!("now", atom_of(&clause.body, *name));
        }
        other => panic!("expected CallTarget::Remote for auto-imported BIF, got {other:?}"),
    }
}

#[test]
fn call_target_unqualified_for_dash_import() {
    // A function brought in by `-import` should also be lowered as an
    // unqualified remote call.
    let (db, file_ids, _) = TestDB::with_many_files(
        r#"
            -import(lists, [reverse/1]).
            via_import() -> reverse([1, 2, 3]).
        "#,
    );
    let file_id = file_ids[0];

    let target = first_call_target(&db, file_id, "via_import", 0);
    let body = {
        let def_map = db.def_map(file_id);
        let na = crate::NameArity::new(crate::Name::from_erlang_service("via_import"), 0);
        let fun_def = def_map.get_function(&na).unwrap();
        db.function_body(InFile::new(file_id, fun_def.function_id))
    };
    let (_, clause) = body.clauses.iter().next().unwrap();
    match &target {
        CallTarget::Remote {
            module,
            name,
            unqualified,
            ..
        } => {
            assert!(*unqualified, "imported call should set unqualified = true");
            assert_eq_expected!("lists", atom_of(&clause.body, *module));
            assert_eq_expected!("reverse", atom_of(&clause.body, *name));
        }
        other => panic!("expected CallTarget::Remote for -import call, got {other:?}"),
    }
}

// Type-level analogue of the value-level `unqualified` invariant. The
// pretty printer ignores the `unqualified` field (it always prints the
// canonical `module:name(...)` form), so we walk the type body's root
// `TypeExpr::Call` directly to assert on the `unqualified` flag.

#[track_caller]
fn root_type_call_target(
    db: &TestDB,
    file_id: elp_base_db::FileId,
    name: &str,
    arity: u32,
) -> CallTarget<crate::TypeExprId> {
    let form_list = db.file_form_list(file_id);
    let na = crate::NameArity::new(crate::Name::from_erlang_service(name), arity);
    let (alias_id, _) = form_list
        .type_aliases()
        .find(|(_, ta)| ta.name() == &na)
        .unwrap_or_else(|| panic!("type alias {name}/{arity} not in form list"));
    let body = db.type_body(InFile::new(file_id, alias_id));
    match &body.body.type_exprs[body.ty] {
        TypeExpr::Call { target, .. } => target.clone(),
        other => panic!("expected root TypeExpr::Call, got {other:?}"),
    }
}

#[track_caller]
fn type_atom_of(body: &crate::Body, ty_id: crate::TypeExprId) -> String {
    match &body.type_exprs[ty_id] {
        TypeExpr::Literal(Literal::Atom(a)) => a.as_string(),
        other => panic!("expected atom literal type, got {other:?}"),
    }
}

#[test]
fn type_call_target_unqualified_for_bif() {
    // `pid()` is a built-in type — lowering rewrites it to
    // `erlang:pid()` with `unqualified: true`.
    let (db, file_ids, _) = TestDB::with_many_files(
        r#"
            -type t() :: pid().
        "#,
    );
    let file_id = file_ids[0];

    let target = root_type_call_target(&db, file_id, "t", 0);
    let body = {
        let form_list = db.file_form_list(file_id);
        let na = crate::NameArity::new(crate::Name::from_erlang_service("t"), 0);
        let (alias_id, _) = form_list
            .type_aliases()
            .find(|(_, ta)| ta.name() == &na)
            .unwrap();
        db.type_body(InFile::new(file_id, alias_id))
    };
    match &target {
        CallTarget::Remote {
            module,
            name,
            unqualified,
            ..
        } => {
            assert!(*unqualified, "BIF type call should set unqualified = true");
            assert_eq_expected!("erlang", type_atom_of(&body.body, *module));
            assert_eq_expected!("pid", type_atom_of(&body.body, *name));
        }
        other => panic!("expected CallTarget::Remote for BIF type, got {other:?}"),
    }
}

#[test]
fn type_call_target_explicit_erlang_qualification() {
    // `erlang:pid()` is written with an explicit module qualifier.
    // Even though `pid/0` resolves through the BIF auto-import on the
    // inner call target, the outer remote-with-call-fun lowering must
    // set `unqualified: false` because the user wrote the module name in
    // source.
    let (db, file_ids, _) = TestDB::with_many_files(
        r#"
            -type t() :: erlang:pid().
        "#,
    );
    let file_id = file_ids[0];

    let target = root_type_call_target(&db, file_id, "t", 0);
    let body = {
        let form_list = db.file_form_list(file_id);
        let na = crate::NameArity::new(crate::Name::from_erlang_service("t"), 0);
        let (alias_id, _) = form_list
            .type_aliases()
            .find(|(_, ta)| ta.name() == &na)
            .unwrap();
        db.type_body(InFile::new(file_id, alias_id))
    };
    match &target {
        CallTarget::Remote {
            module,
            name,
            unqualified,
            ..
        } => {
            assert!(
                !*unqualified,
                "explicit remote type call must keep unqualified = false"
            );
            assert_eq_expected!("erlang", type_atom_of(&body.body, *module));
            assert_eq_expected!("pid", type_atom_of(&body.body, *name));
        }
        other => panic!("expected CallTarget::Remote for explicit type, got {other:?}"),
    }
}

#[test]
fn type_call_target_explicit_user_module() {
    // Sanity check: explicit `m:t()` for a non-erlang module also
    // produces `unqualified: false`.
    let (db, file_ids, _) = TestDB::with_many_files(
        r#"
            -type t() :: my_mod:my_type().
        "#,
    );
    let file_id = file_ids[0];

    let target = root_type_call_target(&db, file_id, "t", 0);
    match &target {
        CallTarget::Remote { unqualified, .. } => {
            assert!(
                !*unqualified,
                "explicit user-module type call must keep unqualified = false"
            );
        }
        other => panic!("expected CallTarget::Remote, got {other:?}"),
    }
}
