/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::helpers::pick_best_token;
use elp_ide_db::RootDatabase;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::SyntaxKind;
use hir::Semantic;

use crate::FilePosition;

#[derive(Debug)]
pub struct ExpandedMacro {
    pub name: String,
    pub expansion: String,
}

// Feature: Expand Macro Recursively
//
// Shows the full macro expansion of the macro at current cursor.
//
// |===
// | Editor  | Action Name
//
// | VS Code | **Erlang: Expand Macro**
// |===
//
pub(crate) fn expand_macro(db: &RootDatabase, position: FilePosition) -> Option<ExpandedMacro> {
    let sema = Semantic::new(db);
    let source_file = sema.parse(position.file_id);

    // Temporary for T153426323
    let _pctx = stdx::panic_context::enter(format!("\nexpand_macro"));
    let tok = pick_best_token(
        source_file.value.syntax().token_at_offset(position.offset),
        |kind| match kind {
            SyntaxKind::ATOM => 1,
            SyntaxKind::VAR => 1,
            _ => 0,
        },
    )?;

    tok.parent_ancestors().find_map(|node| {
        let mac = ast::MacroCallExpr::cast(node)?;
        let (name, expansion) = sema.expand(source_file.with_value(&mac))?;
        Some(ExpandedMacro {
            name: name.to_string(),
            expansion,
        })
    })
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use expect_test::Expect;

    use crate::fixture;

    fn check(elp_fixture: &str, expect: Expect) {
        let (analysis, pos) = fixture::position(elp_fixture);

        let expansion = match analysis.expand_macro(pos).unwrap() {
            Some(it) => format!("{}{}", it.name, it.expansion),
            None => "***EXPANSION FAILED***".to_string(),
        };
        expect.assert_eq(&expansion);
    }

    #[test]
    fn macro_expand_line_macro() {
        check(
            r#"
-module(foo).
-bar() -> ?L~INE.
"#,
            expect![[r#"
                LINE
                0
            "#]],
        );
    }

    #[test]
    fn macro_expand_file_macro() {
        check(
            r#"
-module(foo).
-bar() -> ?F~ILE.
"#,
            expect![[r#"
                FILE
                "foo.erl"
            "#]],
        );
    }

    #[test]
    fn macro_expand_constant_macro() {
        check(
            r#"
-module(foo).
-define(FOO,foo).
bar() -> ?F~OO.
"#,
            expect![[r#"
                FOO
                'foo'
            "#]],
        );
    }

    #[test]
    fn macro_expand_constant_macro2() {
        check(
            r#"
-module(foo).
-define(FOO,foo + 1).
bar() -> ?F~OO.
"#,
            expect![[r#"
                FOO
                ('foo' + 1)
            "#]],
        );
    }

    #[test]
    fn macro_expand_simple_param_macro() {
        check(
            r#"
-module(foo).
-define(FOO(X),foo+X).
bar() -> ?F~OO(4).
"#,
            expect![[r#"
                FOO/1
                ('foo' + 4)
            "#]],
        );
    }

    #[test]
    fn macro_expand_simple_param_macro2() {
        check(
            r#"
-module(foo).
-define(FOO(X,Y),foo+X+Y + 1).
bar() -> ?F~OO(4,5).
"#,
            expect![[r#"
                FOO/2
                ((('foo' + 4) + 5) + 1)
            "#]],
        );
    }

    #[test]
    fn macro_expand_simple_param_macro3() {
        check(
            r#"
-module(foo).
-define(FOO(X,Y),foo+X+Y + 1).
bar() -> ?F~OO(4,(baz(42))).
"#,
            expect![[r#"
                FOO/2
                ((('foo' + 4) + 'baz'(
                    42
                )) + 1)
            "#]],
        );
    }

    #[test]
    fn macro_expand_param_macro_args_mismatch() {
        check(
            r#"
-module(foo).
-define(FOO(X),foo+X).
bar() -> ?F~OO(4,5).
"#,
            expect!["***EXPANSION FAILED***"],
        );
    }

    #[test]
    fn macro_expand_missing_macro() {
        check(
            r#"
-module(foo).
bar() -> ?F~OO.
"#,
            expect!["***EXPANSION FAILED***"],
        );
    }

    #[test]
    fn macro_expand_recursive_fail() {
        check(
            r#"
-module(foo).
-define(BAZ, ?BAZ).
-define(FOO(X),foo+X+?BAZ).
bar() -> ?F~OO(4).
"#,
            expect![[r#"
                FOO/1
                (('foo' + 4) + [missing])
            "#]],
        );
    }

    #[test]
    fn macro_expand_recursive() {
        check(
            r#"
-module(foo).
-define(BAZ, baz).
-define(FOO(X),foo+X+?BAZ).
bar() -> ?F~OO(4).
"#,
            expect![[r#"
                FOO/1
                (('foo' + 4) + 'baz')
            "#]],
        );
    }

    #[test]
    fn macro_expand_recursive_multiple_fail() {
        check(
            r#"
-module(foo).
-define(BAZ, ?BAR(6)).
-define(BAR(X), ?FOO(X)).
-define(FOO(X),foo+X+?BAZ).
bar() -> ?F~OO(4).
"#,
            expect![[r#"
                FOO/1
                (('foo' + 4) + [missing])
            "#]],
        );
    }

    #[test]
    fn macro_expand_recursive_multiple() {
        check(
            r#"
-module(foo).
-define(BAZ, ?BAR(6)).
-define(BAR(X), foo(X)).
-define(FOO(X),foo+X+?BAZ).
bar() -> ?F~OO(4).
"#,
            expect![[r#"
                FOO/1
                (('foo' + 4) + 'foo'(
                    6
                ))
            "#]],
        );
    }

    #[test]
    fn macro_expand_comment_in_rhs() {
        check(
            r#"
-module(foo).
-define(FOO,[
%% comment
1,
2
]).
bar() -> ?F~OO.
"#,
            expect![[r#"
                FOO
                [
                    1,
                    2
                ]
            "#]],
        );
    }

    #[test]
    fn macro_expand_wtf() {
        check(
            r#"
-module(foo).

-define(VAL, val).
-define(ALL, all).
-define(assertA(PARAM_A, Type, Expected), begin
    ((fun() ->
        {ok, Actual} = lookup_mod:get(a_mod:get_val(PARAM_A), Type),
        ?assertEqual(length(Expected), length(Actual)),
        DebugComment = [{actual, Actual}, {expected, Expected}],
        SortFun = fun(A, B) -> maps:get(code, A) =< maps:get(code, B) end,
        lists:foreach(
            fun({ExpectedVal, ActualVal}) ->
                ?assertEqual(maps:get(code, ExpectedVal), maps:get(code, ActualVal), DebugComment),
                ?assertEqual(maps:get(key_val, ExpectedVal, undefined), maps:get(key_val, ActualVal, undefined)),
                ExpectedType =
                    case Type of
                        ?ALL ->
                            maps:get(type, ExpectedVal, missing_expected_type);
                        _ ->
                            Type
                    end,
                ?assertEqual(ExpectedType, maps:get(type, ActualVal)),
                ?assert(maps:is_key(ctime, ActualVal))
            end,
            lists:zip(lists:sort(SortFun, Expected), lists:sort(SortFun, Actual))
        )
    end)())
end).
baz() ->
    ?asser~tA(Alice, ?VAL, []),
    ok.
"#,
            expect![[r#"
                assertA/3
                begin
                    fun
                        () ->
                            {
                                'ok',
                                Actual
                            } = 'lookup_mod':'get'(
                                'a_mod':'get_val'(
                                    Alice
                                ),
                                'val'
                            ),
                            [missing],
                            DebugComment = [
                                {
                                    'actual',
                                    Actual
                                },
                                {
                                    'expected',
                                    []
                                }
                            ],
                            SortFun = fun
                                (A, B) ->
                                    ('maps':'get'(
                                        'code',
                                        A
                                    ) =< 'maps':'get'(
                                        'code',
                                        B
                                    ))
                            end,
                            'lists':'foreach'(
                                fun
                                    ({
                                        ExpectedVal,
                                        ActualVal
                                    }) ->
                                        [missing],
                                        [missing],
                                        ExpectedType = case 'val' of
                                            'all' ->
                                                'maps':'get'(
                                                    'type',
                                                    ExpectedVal,
                                                    'missing_expected_type'
                                                );
                                            _ ->
                                                'val'
                                        end,
                                        [missing],
                                        [missing]
                                end,
                                'lists':'zip'(
                                    'lists':'sort'(
                                        SortFun,
                                        []
                                    ),
                                    'lists':'sort'(
                                        SortFun,
                                        Actual
                                    )
                                )
                            )
                    end()
                end
            "#]],
        );
    }

    #[test]
    fn macro_expand_wtf2() {
        check(
            r#"
-module(foo).

-define(WA_QR_TYPE_MESSAGE, qr_type_message).
-define(WA_QR_TYPE_ALL, qr_type_all).
-define(assertQrs(WID, Type, ExpectedQrs),
                ExpectedType =
                    case Type of
                        ?WA_QR_TYPE_ALL ->
                            maps:get(type, ExpectedQr, missing_expected_type);
                        _ ->
                            Type
                    end,
).
baz() ->
    ?asser~tQrs(AliceWID, ?WA_QR_TYPE_MESSAGE, []),
    ok.
"#,
            expect![[r#"
                assertQrs/3
                ExpectedType = case 'qr_type_message' of
                    'qr_type_all' ->
                        'maps':'get'(
                            'type',
                            ExpectedQr,
                            'missing_expected_type'
                        );
                    _ ->
                        'qr_type_message'
                end
            "#]],
        );
    }

    #[test]
    fn macro_expand_case() {
        check(
            r#"
-module(foo).

-define(FOO(X),
    case X of
        1 ->
            one;
        _ ->
            X
    end).
baz() ->
    ?F~OO(3),
    ok.
"#,
            expect![[r#"
                FOO/1
                case 3 of
                    1 ->
                        'one';
                    _ ->
                        3
                end
            "#]],
        );
    }

    #[test]
    fn macro_expand_include_file() {
        check(
            r#"
//- /include/foo.hrl include_path:/include
-define(FOO,3).
//- /src/foo.erl
-module(foo).
-include("foo.hrl").
bar() -> ?F~OO.
"#,
            expect![[r#"
                FOO
                3
            "#]],
        );
    }

    #[test]
    fn macro_expand_no_param_macro_1() {
        check(
            r#"
-module(foo).
-define(C, m:f).
f() ->
    ?~C().
"#,
            expect![[r#"
                C
                'm':'f'()
            "#]],
        );
    }
    #[test]
    fn macro_expand_no_param_macro_2() {
        check(
            r#"
-module(foo).
-define(C, m:f).
f() ->
    ?~C(1,2).
"#,
            expect![[r#"
                C
                'm':'f'(
                    1,
                    2
                )
            "#]],
        );
    }

    #[test]
    fn macro_expand_empty_param_macro() {
        check(
            r#"
-module(foo).
-define(F0(), c).
f() ->
    ?~?F0.
"#,
            expect!["***EXPANSION FAILED***"],
        );
    }

    #[test]
    fn macro_expand_multiple_arities() {
        check(
            r#"
-module(foo).
-define(C(), 0).
-define(C(X), X).
-define(C(X,Y), X+Y).
f() ->
    ?~C(1,2).
"#,
            expect![[r#"
                C/2
                (1 + 2)
            "#]],
        );
    }

    #[test]
    fn macro_expand_no_param_macro() {
        check(
            r#"
-module(foo).
-export([get_partition/1]).
-define(HASH_FUN, wa_pg2:hash).
get_partition(Who) ->
    ?~HASH_FUN(Who, 5).
"#,
            expect![[r#"
                HASH_FUN
                'wa_pg2':'hash'(
                    Who,
                    5
                )
            "#]],
        );
    }

    #[test]
    fn macro_expand_outside_expressions() {
        check(
            r#"

-module(foo).
-define(ARITY, 0).
-export([bar/?A~RITY]).
bar() -> ok.
"#,
            expect!["***EXPANSION FAILED***"],
        );
    }
}
