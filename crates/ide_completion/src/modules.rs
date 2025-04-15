/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::Completion;
use crate::Contents;
use crate::Ctx;
use crate::DoneFlag;
use crate::Kind;
use crate::helpers;

pub(crate) fn add_completions(
    acc: &mut Vec<Completion>,
    Ctx {
        file_position,
        parsed,
        sema,
        trigger,
        ..
    }: &Ctx,
) -> DoneFlag {
    if trigger.is_some() {
        return false;
    }
    let prefix = &helpers::atom_value(parsed, file_position.offset).unwrap_or_default();
    if let Some(modules) = sema.resolve_module_names(file_position.file_id) {
        let completions = modules.into_iter().filter_map(|m| {
            if m.starts_with(prefix) {
                Some(Completion {
                    label: m.to_string(),
                    kind: Kind::Module,
                    contents: Contents::SameAsLabel,
                    position: None,
                    sort_text: None,
                    deprecated: false,
                })
            } else {
                None
            }
        });

        acc.extend(completions)
    }
    false
}

#[cfg(test)]
mod test {
    use expect_test::Expect;
    use expect_test::expect;

    use crate::Kind;
    use crate::tests::get_completions;
    use crate::tests::render_completions;

    // completions filtered to avoid noise
    fn check(code: &str, expect: Expect) {
        let completions = get_completions(code, None)
            .into_iter()
            .filter(|c| c.kind != Kind::Keyword)
            .collect();
        let actual = &render_completions(completions);
        expect.assert_eq(actual);
    }

    #[test]
    fn test_modules() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::MODULE).unwrap() == "9");

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    -spec foo() -> s~.
    foo() ->
        ok.
    //- /src/sample2.erl
    -module(sample2).
    "#,
            expect![[r#"
                {label:sample1, kind:Module, contents:SameAsLabel, position:None}
                {label:sample2, kind:Module, contents:SameAsLabel, position:None}"#]],
        );

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    foo() ->
        s~.
    //- /src/sample2.erl
    -module(sample2).
    "#,
            expect![[r#"
                {label:sample1, kind:Module, contents:SameAsLabel, position:None}
                {label:sample2, kind:Module, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_module_from_otp() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::MODULE).unwrap() == "9");

        check(
            r#"
//- /src/sample1.erl
-module(sample1).
foo() ->
    s~.
//- /src/sample2.erl
-module(sample2).
//- /opt/lib/stdlib-3.17/src/sets.erl otp_app:/opt/lib/stdlib-3.17
-module(sets).
    "#,
            expect![[r#"
                {label:sample1, kind:Module, contents:SameAsLabel, position:None}
                {label:sample2, kind:Module, contents:SameAsLabel, position:None}
                {label:sets, kind:Module, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_no_modules() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::MODULE).unwrap() == "9");

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    -export([
        s~
    ]).
    //- /src/sample2.erl
    -module(sample2).
    "#,
            expect![""],
        );
    }

    #[test]
    fn test_modules_prefix_filtering() {
        check(
            r#"
    //- /src/sample.erl
    -module(sample).
    test(X) ->
        ma~
    //- /src/match1.erl
    -module(match1).
    //- /src/match2.erl
    -module(match).
    //- /src/no_prefix_match.erl
    -module(no_prefix_match).
    "#,
            expect![[r#"
                {label:match1, kind:Module, contents:SameAsLabel, position:None}
                {label:match2, kind:Module, contents:SameAsLabel, position:None}"#]],
        );
    }
}
