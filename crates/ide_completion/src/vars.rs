/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::iter;

use elp_syntax::SyntaxToken;
use fxhash::FxHashSet;

use crate::Completion;
use crate::Contents;
use crate::Ctx;
use crate::DoneFlag;
use crate::Kind;

pub(crate) fn add_completions(
    acc: &mut Vec<Completion>,
    Ctx {
        sema: _,
        trigger,
        file_position,
        previous_tokens,
        ..
    }: &Ctx,
) -> DoneFlag {
    use elp_syntax::SyntaxKind as K;
    let default = vec![];
    let previous_tokens: &[_] = previous_tokens.as_ref().unwrap_or(&default);
    match previous_tokens {
        // Local variables
        [.., (K::VAR, var)] if trigger.is_none() => {
            let mut completions = FxHashSet::default();
            // Scan backward until the end of the prior function
            // (recognised by '.'), and forward to the end of this
            // one.  We could optimise to look for clause boundaries,
            // but a ';' location is harder to disambiguate in broken
            // code.
            if var.text_range().end() == file_position.offset {
                // We are on the end of the var, not in whitespace past it
                iter::successors(var.prev_token(), |t| t.prev_token())
                    .take_while(|tok| tok.text() != ".")
                    .for_each(|tok| {
                        complete_var(var, &tok, &mut completions);
                    });
                iter::successors(var.next_token(), |t| t.next_token())
                    .take_while(|tok| tok.text() != ".")
                    .for_each(|tok| {
                        complete_var(var, &tok, &mut completions);
                    });
                acc.extend(completions);
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

fn complete_var(var: &SyntaxToken, candidate: &SyntaxToken, acc: &mut FxHashSet<Completion>) {
    if candidate.text().starts_with(var.text()) {
        acc.insert(Completion {
            label: candidate.text().to_string(),
            kind: Kind::Variable,
            contents: Contents::SameAsLabel,
            position: None,
            sort_text: None,
            deprecated: false,
            additional_edit: None,
        });
    }
}

#[cfg(test)]
mod test {
    use expect_test::Expect;
    use expect_test::expect;

    use crate::Kind;
    use crate::tests::get_completions;
    use crate::tests::render_completions;

    // keywords are filtered out to avoid noise
    fn check(code: &str, trigger_character: Option<char>, expect: Expect) {
        let completions = get_completions(code, trigger_character)
            .into_iter()
            .filter(|c| c.kind != Kind::Keyword)
            .collect();
        let actual = &render_completions(completions);
        expect.assert_eq(actual);
    }

    #[test]
    fn test_local_variables_1() {
        check(
            r#"
    //- expect_parse_errors
    //- /src/sample1.erl
    -module(sample1).
    test(AnArg1,Blah) ->
        case An~
        Another = 42,
    "#,
            None,
            expect![[r#"
                {label:AnArg1, kind:Variable, contents:SameAsLabel, position:None}
                {label:Another, kind:Variable, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_local_variables_limit_to_current_function() {
        check(
            r#"
    //- expect_parse_errors
    //- /src/sample1.erl
    -module(sample1).
    another(AnArgNotMatched) ->
        AnotherNotMatched = 4.
    test(AnArg1,Blah) ->
        case An~
        Another = 42,
    later_fun(AnArgLater) ->
        AnotherLater = 4.
    later_fun2(AnArgEvenLater) ->
        AnotherEvenLater = 4.
    "#,
            None,
            expect![[r#"
                {label:AnArg1, kind:Variable, contents:SameAsLabel, position:None}
                {label:AnArgLater, kind:Variable, contents:SameAsLabel, position:None}
                {label:Another, kind:Variable, contents:SameAsLabel, position:None}
                {label:AnotherLater, kind:Variable, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_local_variables_none_if_space() {
        check(
            r#"
    //- expect_parse_errors
    //- /src/sample1.erl
    -module(sample1).
    test(AnArg1,Blah) ->
        case An ~
        Another = 42,
    "#,
            None,
            expect!["{label:sample1, kind:Module, contents:SameAsLabel, position:None}"],
        );
    }

    #[test]
    fn test_local_variables_no_duplicates() {
        check(
            r#"
    //- expect_parse_errors
    //- /src/sample1.erl
    -module(sample1).
    handle_update(Config, Contents) ->
        gen_server:cast(?MODULE, {update_config, Config, Contents}),
        Co~
        valid.
    "#,
            None,
            expect![[r#"
                {label:Config, kind:Variable, contents:SameAsLabel, position:None}
                {label:Contents, kind:Variable, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_local_variables_in_macro_arg() {
        check(
            r#"
        -module(main).
        -define(MY_MACRO(A, B), A + B).
        main(This, That) ->
            ?MY_MACRO(Th~)
                "#,
            None,
            expect![[r#"
                {label:That, kind:Variable, contents:SameAsLabel, position:None}
                {label:This, kind:Variable, contents:SameAsLabel, position:None}"#]],
        );
    }
}
