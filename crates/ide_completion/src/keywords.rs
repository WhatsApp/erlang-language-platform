/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use lazy_static::lazy_static;

use crate::Completion;
use crate::Contents;
use crate::Ctx;
use crate::DoneFlag;

lazy_static! {
    // adapted from https://github.com/erlang-ls/erlang_ls d067267b906239c883fed6e0f9e69c4eb94dd580
    static ref KEYWORDS: Vec<Completion> = [
        "case",
        "after",
        "and",
        "andalso",
        "band",
        "begin",
        "bnot",
        "bor",
        "bsl",
        "bsr",
        "bxor",
        "case",
        "catch",
        "cond",
        "div",
        "end",
        "fun",
        "if",
        "let",
        "not",
        "of",
        "or",
        "orelse",
        "receive",
        "rem",
        "try",
        "when",
        "xor",
        // Add some common atoms too
        "ok",
        "undefined",
        "true",
        "false"
    ].iter().map(|label| Completion{ label: label.to_string(), kind: crate::Kind::Keyword, contents: Contents::SameAsLabel, position: None, sort_text: None, deprecated: false}).collect();
}

pub(crate) fn add_completions(acc: &mut Vec<Completion>, Ctx { trigger, .. }: &Ctx) -> DoneFlag {
    if trigger.is_some() {
        return false;
    }
    acc.append(&mut KEYWORDS.clone());
    false
}

#[cfg(test)]
mod test {
    use expect_test::expect;
    use expect_test::Expect;

    use crate::tests::get_completions;
    use crate::tests::render_completions;

    fn check(code: &str, trigger_character: Option<char>, expect: Expect) {
        let completions = get_completions(code, trigger_character);
        let actual = &render_completions(completions);
        expect.assert_eq(actual);
    }

    #[test]
    fn test_keywords() {
        assert_eq!(
            serde_json::to_string(&lsp_types::CompletionItemKind::KEYWORD).unwrap(),
            "14"
        );
        assert_eq!(
            serde_json::to_string(&lsp_types::InsertTextFormat::PLAIN_TEXT).unwrap(),
            "1"
        );
        check(
            r#"
    -module(sample).
    test(X) ->
        a~
    "#,
            None,
            expect![[r#"
                {label:after, kind:Keyword, contents:SameAsLabel, position:None}
                {label:and, kind:Keyword, contents:SameAsLabel, position:None}
                {label:andalso, kind:Keyword, contents:SameAsLabel, position:None}
                {label:band, kind:Keyword, contents:SameAsLabel, position:None}
                {label:begin, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bnot, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bor, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bsl, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bsr, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bxor, kind:Keyword, contents:SameAsLabel, position:None}
                {label:case, kind:Keyword, contents:SameAsLabel, position:None}
                {label:case, kind:Keyword, contents:SameAsLabel, position:None}
                {label:catch, kind:Keyword, contents:SameAsLabel, position:None}
                {label:cond, kind:Keyword, contents:SameAsLabel, position:None}
                {label:div, kind:Keyword, contents:SameAsLabel, position:None}
                {label:end, kind:Keyword, contents:SameAsLabel, position:None}
                {label:fun, kind:Keyword, contents:SameAsLabel, position:None}
                {label:if, kind:Keyword, contents:SameAsLabel, position:None}
                {label:let, kind:Keyword, contents:SameAsLabel, position:None}
                {label:not, kind:Keyword, contents:SameAsLabel, position:None}
                {label:of, kind:Keyword, contents:SameAsLabel, position:None}
                {label:or, kind:Keyword, contents:SameAsLabel, position:None}
                {label:orelse, kind:Keyword, contents:SameAsLabel, position:None}
                {label:receive, kind:Keyword, contents:SameAsLabel, position:None}
                {label:rem, kind:Keyword, contents:SameAsLabel, position:None}
                {label:try, kind:Keyword, contents:SameAsLabel, position:None}
                {label:when, kind:Keyword, contents:SameAsLabel, position:None}
                {label:xor, kind:Keyword, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_no_keywords() {
        check(
            r#"
    -module(sample).
    test(X) ->
        a~
    "#,
            Some(':'),
            expect![""],
        );
        check(
            r#"
    -module(sample).
    test(~X) ->
        X.
    "#,
            None,
            expect![""],
        );

        check(
            r#"
    -module(m~).
    "#,
            None,
            expect![""],
        );

        check(
            r#"
    -module(m).
    -~
    "#,
            None,
            expect![""],
        );

        check(
            r#"
    -module(m).
    -type foo() :: ~.
    "#,
            None,
            expect![[r#"
                {label:foo/0, kind:Type, contents:Snippet("foo()"), position:None}
                {label:main, kind:Module, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_keywords_error_recovery() {
        check(
            r#"
    -module(sample).
    test(X) ->
        X ~
    "#,
            None,
            expect![[r#"
                {label:after, kind:Keyword, contents:SameAsLabel, position:None}
                {label:and, kind:Keyword, contents:SameAsLabel, position:None}
                {label:andalso, kind:Keyword, contents:SameAsLabel, position:None}
                {label:band, kind:Keyword, contents:SameAsLabel, position:None}
                {label:begin, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bnot, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bor, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bsl, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bsr, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bxor, kind:Keyword, contents:SameAsLabel, position:None}
                {label:case, kind:Keyword, contents:SameAsLabel, position:None}
                {label:case, kind:Keyword, contents:SameAsLabel, position:None}
                {label:catch, kind:Keyword, contents:SameAsLabel, position:None}
                {label:cond, kind:Keyword, contents:SameAsLabel, position:None}
                {label:div, kind:Keyword, contents:SameAsLabel, position:None}
                {label:end, kind:Keyword, contents:SameAsLabel, position:None}
                {label:fun, kind:Keyword, contents:SameAsLabel, position:None}
                {label:if, kind:Keyword, contents:SameAsLabel, position:None}
                {label:let, kind:Keyword, contents:SameAsLabel, position:None}
                {label:main, kind:Module, contents:SameAsLabel, position:None}
                {label:not, kind:Keyword, contents:SameAsLabel, position:None}
                {label:of, kind:Keyword, contents:SameAsLabel, position:None}
                {label:or, kind:Keyword, contents:SameAsLabel, position:None}
                {label:orelse, kind:Keyword, contents:SameAsLabel, position:None}
                {label:receive, kind:Keyword, contents:SameAsLabel, position:None}
                {label:rem, kind:Keyword, contents:SameAsLabel, position:None}
                {label:try, kind:Keyword, contents:SameAsLabel, position:None}
                {label:when, kind:Keyword, contents:SameAsLabel, position:None}
                {label:xor, kind:Keyword, contents:SameAsLabel, position:None}"#]],
        );

        check(
            r#"
    -module(sample).
    test(X) ->
        ~
    "#,
            None,
            expect![[r#"
                {label:after, kind:Keyword, contents:SameAsLabel, position:None}
                {label:and, kind:Keyword, contents:SameAsLabel, position:None}
                {label:andalso, kind:Keyword, contents:SameAsLabel, position:None}
                {label:band, kind:Keyword, contents:SameAsLabel, position:None}
                {label:begin, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bnot, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bor, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bsl, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bsr, kind:Keyword, contents:SameAsLabel, position:None}
                {label:bxor, kind:Keyword, contents:SameAsLabel, position:None}
                {label:case, kind:Keyword, contents:SameAsLabel, position:None}
                {label:case, kind:Keyword, contents:SameAsLabel, position:None}
                {label:catch, kind:Keyword, contents:SameAsLabel, position:None}
                {label:cond, kind:Keyword, contents:SameAsLabel, position:None}
                {label:div, kind:Keyword, contents:SameAsLabel, position:None}
                {label:end, kind:Keyword, contents:SameAsLabel, position:None}
                {label:fun, kind:Keyword, contents:SameAsLabel, position:None}
                {label:if, kind:Keyword, contents:SameAsLabel, position:None}
                {label:let, kind:Keyword, contents:SameAsLabel, position:None}
                {label:main, kind:Module, contents:SameAsLabel, position:None}
                {label:not, kind:Keyword, contents:SameAsLabel, position:None}
                {label:of, kind:Keyword, contents:SameAsLabel, position:None}
                {label:or, kind:Keyword, contents:SameAsLabel, position:None}
                {label:orelse, kind:Keyword, contents:SameAsLabel, position:None}
                {label:receive, kind:Keyword, contents:SameAsLabel, position:None}
                {label:rem, kind:Keyword, contents:SameAsLabel, position:None}
                {label:try, kind:Keyword, contents:SameAsLabel, position:None}
                {label:when, kind:Keyword, contents:SameAsLabel, position:None}
                {label:xor, kind:Keyword, contents:SameAsLabel, position:None}"#]],
        );
    }
}
