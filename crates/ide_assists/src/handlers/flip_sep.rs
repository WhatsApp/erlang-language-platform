/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_syntax::algo::non_trivia_sibling;
use elp_syntax::Direction;
use elp_syntax::SyntaxKind;
use fxhash::FxHashSet;

use crate::AssistContext;
use crate::Assists;

// Assist: flip_sep
//
// Flips two items around a separator.
//
// ```
//     {{1, 2}~, {3, 4}}.
// ```
// ->
// ```
//     {{3, 4}~, {1, 2}}.
// ```
//
// ```
//     f(A~, B) -> ok.
// ```
// ->
// ```
//     f(B, A) -> ok.
// ```
pub(crate) fn flip_sep(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    let pivot = ctx.find_tokens_syntax_at_offset(FxHashSet::from_iter([
        SyntaxKind::ANON_COMMA,
        SyntaxKind::ANON_SEMI,
    ]))?;

    let prev = non_trivia_sibling(pivot.clone().into(), Direction::Prev)?;
    let next = non_trivia_sibling(pivot.clone().into(), Direction::Next)?;

    acc.add(
        AssistId("flip_sep", AssistKind::RefactorRewrite),
        "Flip around separator",
        pivot.text_range(),
        None,
        |edit| {
            edit.replace(prev.text_range(), next.to_string());
            edit.replace(next.text_range(), prev.to_string());
        },
    )
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::*;

    // --- Just two elements to swap ---

    #[test]
    fn test_two_function_params() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo(Foo~, some_atom) -> ok.
"#,
            expect![[r#"
                foo(some_atom, Foo) -> ok.
            "#]],
        )
    }

    #[test]
    fn test_two_function_args() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo(Foo~, some_atom) -> ok.
"#,
            expect![[r#"
                foo(some_atom, Foo) -> ok.
            "#]],
        )
    }

    #[test]
    fn test_two_spec_params() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
-spec foo(Foo :: t()~, some_atom) -> ok.
"#,
            expect![[r#"
                -spec foo(some_atom, Foo :: t()) -> ok.
            "#]],
        )
    }

    #[test]
    fn test_two_statements() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo(Foo, some_atom) ->
    lists:reverse([1,2,3])~,
    ok.
"#,
            expect![[r#"
                foo(Foo, some_atom) ->
                    ok,
                    lists:reverse([1,2,3]).
            "#]],
        )
    }

    #[test]
    fn test_two_cases() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo(Foo) ->
    case Foo of
        a -> x~;
        b -> y
    end.
"#,
            expect![[r#"
                foo(Foo) ->
                    case Foo of
                        b -> y;
                        a -> x
                    end.
            "#]],
        )
    }

    #[test]
    fn test_two_intersected_specs() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
-spec foo(Foo :: t(), some_atom) -> a~; (Bar :: r(), other_atom) -> b.
"#,
            expect![[r#"
                -spec foo(Bar :: r(), other_atom) -> b; (Foo :: t(), some_atom) -> a.
            "#]],
        )
    }

    #[test]
    fn test_two_function_clauses() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo({X, Y}) -> X ++ Y~;
foo(XY) -> XY.
"#,
            expect![[r#"
                foo(XY) -> XY;
                foo({X, Y}) -> X ++ Y.
            "#]],
        )
    }

    #[test]
    fn test_two_tuple_elements() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo({X~, Y}) -> X ++ Y.
"#,
            expect![[r#"
                foo({Y, X}) -> X ++ Y.
            "#]],
        )
    }

    #[test]
    fn test_two_list_elements() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo() -> [1~, 2].
"#,
            expect![[r#"
                foo() -> [2, 1].
            "#]],
        )
    }

    #[test]
    fn test_comma_in_string_not_a_separator() {
        check_assist_not_applicable(
            flip_sep,
            r#"
foo() ->
    "This is not a pivot~, I think".
    "#,
        );
    }

    #[test]
    fn test_semicolon_in_string_not_a_separator() {
        check_assist_not_applicable(
            flip_sep,
            r#"
foo() ->
    "This is not a pivot~; I think".
    "#,
        );
    }

    #[test]
    fn test_comma_in_atom_not_a_separator() {
        check_assist_not_applicable(
            flip_sep,
            r#"
foo() ->
    'quoted~,atom'.
    "#,
        );
    }

    #[test]
    fn test_comma_in_semicolon_not_a_separator() {
        check_assist_not_applicable(
            flip_sep,
            r#"
foo() ->
    'quoted~;atom'.
    "#,
        );
    }

    // --- Multiple elements, of which only two should be swapped ---

    #[test]
    fn test_multiple_function_params() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo(Bar, Foo~, some_atom) -> ok.
"#,
            expect![[r#"
                foo(Bar, some_atom, Foo) -> ok.
            "#]],
        )
    }

    #[test]
    fn test_multiple_function_args() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo(Bar, Foo~, some_atom) -> ok.
"#,
            expect![[r#"
                foo(Bar, some_atom, Foo) -> ok.
            "#]],
        )
    }

    #[test]
    fn test_multiple_spec_params() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
-spec foo(Bar :: boolean(), Foo :: string()~, some_atom) -> ok.
"#,
            expect![[r#"
                -spec foo(Bar :: boolean(), some_atom, Foo :: string()) -> ok.
            "#]],
        )
    }

    #[test]
    fn test_multiple_statements() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo(Foo, some_atom) ->
    [a,b,c],
    lists:reverse([1,2,3])~,
    ok,
    {error, "reason"}.
"#,
            expect![[r#"
                foo(Foo, some_atom) ->
                    [a,b,c],
                    ok,
                    lists:reverse([1,2,3]),
                    {error, "reason"}.
            "#]],
        )
    }

    #[test]
    fn test_multiple_cases() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo(Foo) ->
    case Foo of
        {a,b} -> w;
        a -> x~;
        b -> y;
        _ -> z
    end.
"#,
            expect![[r#"
                foo(Foo) ->
                    case Foo of
                        {a,b} -> w;
                        b -> y;
                        a -> x;
                        _ -> z
                    end.
            "#]],
        )
    }

    #[test]
    fn test_multiple_intersected_specs() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
-spec foo(Baz :: s(), another_atom) -> c; (Foo :: t(), some_atom) -> a~; (Bar :: r(), other_atom) -> b.
"#,
            expect![[r#"
                -spec foo(Baz :: s(), another_atom) -> c; (Bar :: r(), other_atom) -> b; (Foo :: t(), some_atom) -> a.
            "#]],
        )
    }

    #[test]
    fn test_multiple_function_clauses() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo([X | Ys]) -> Ys;
foo({X, Y}) -> X ++ Y~;
foo(XY) -> XY.
"#,
            expect![[r#"
                foo([X | Ys]) -> Ys;
                foo(XY) -> XY;
                foo({X, Y}) -> X ++ Y.
            "#]],
        )
    }

    #[test]
    fn test_multiple_tuple_elements() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo({W, X~, Y, Z}) -> W ++ X ++ Y ++ Z.
"#,
            expect![[r#"
                foo({W, Y, X, Z}) -> W ++ X ++ Y ++ Z.
            "#]],
        )
    }

    #[test]
    fn test_multiple_list_elements() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo() -> [0, 1~, 2, 3].
"#,
            expect![[r#"
                foo() -> [0, 2, 1, 3].
            "#]],
        )
    }

    #[test]
    fn test_multiple_binary_elements() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo() ->
    A = 1,
    B = 17,
    C = 42,
    D = 9,
    <<A, B~, C:16, D/utf8>>.
"#,
            expect![[r#"
                foo() ->
                    A = 1,
                    B = 17,
                    C = 42,
                    D = 9,
                    <<A, C:16, B, D/utf8>>.
            "#]],
        );
    }

    #[test]
    fn test_list_comprehension_generators_and_filters_are_flippable() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo() ->
    [ X || X <- [1,2,c,4], is_integer(X)~, X > 1 ].
"#,
            expect![[r#"
            foo() ->
                [ X || X <- [1,2,c,4], X > 1, is_integer(X) ].
            "#]],
        )
    }

    #[test]
    fn test_list_comprehension_values_are_flippable() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo() ->
    [{X~, Y} || X <- [1,2,3], Y <- [a,b]].
"#,
            expect![[r#"
            foo() ->
                [{Y, X} || X <- [1,2,3], Y <- [a,b]].
            "#]],
        )
    }

    #[test]
    fn test_binary_comprehension_generators_and_filters_are_flippable() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo() ->
    [ X || X <= <<1,2,3,4>>, is_integer(X)~, X > 1 ].
"#,
            expect![[r#"
            foo() ->
                [ X || X <= <<1,2,3,4>>, X > 1, is_integer(X) ].
            "#]],
        )
    }

    #[test]
    fn test_binary_comprehension_values_are_flippable() {
        check_assist(
            flip_sep,
            "Flip around separator",
            r#"
foo() ->
    [ {X~, X + 1} || X <= <<1,2,3,4>>, is_integer(X), X > 1 ].
"#,
            expect![[r#"
            foo() ->
                [ {X + 1, X} || X <= <<1,2,3,4>>, is_integer(X), X > 1 ].
            "#]],
        )
    }
}
