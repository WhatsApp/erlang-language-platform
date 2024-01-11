/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_syntax::algo;
use elp_syntax::ast;
use elp_syntax::AstNode;
use hir::InFile;

use crate::helpers;
use crate::Args;
use crate::Completion;
use crate::Kind;

pub(crate) fn add_completions(
    acc: &mut Vec<Completion>,
    Args {
        file_position,
        parsed,
        sema,
        trigger,
        previous_tokens,
        ..
    }: &Args,
) {
    use elp_syntax::SyntaxKind as K;
    let default = vec![];
    let previous_tokens: &[_] = previous_tokens.as_ref().unwrap_or(&default);
    match previous_tokens {
        [
            ..,
            (K::ANON_DASH, _),
            (K::ANON_SPEC, _),
            (K::ATOM, spec_fun_prefix),
        ] if trigger.is_none() => {
            if let Some(spec) =
                algo::find_node_at_offset::<ast::Spec>(parsed.value.syntax(), file_position.offset)
            {
                if let Some(sp) = sema.find_form::<ast::Spec>(InFile::new(parsed.file_id, &spec)) {
                    if let Some(completion) = helpers::name_slash_arity_completion(
                        &sp.name,
                        spec_fun_prefix.text(),
                        Kind::Function,
                    ) {
                        acc.push(completion);
                    }
                }
            }

            let def_map = sema.def_map(file_position.file_id);

            let completions = def_map.get_functions().filter_map(|(na, _)| {
                helpers::name_slash_arity_completion(na, spec_fun_prefix.text(), Kind::Function)
            });
            acc.extend(completions);
            true
        }
        _ => false,
    };
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
    fn test_spec() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::INTERFACE).unwrap() == "8");

        check(
            r#"
        -module(sample).

        frog() -> ok.

        -spec f~
        foo() -> ok.
        "#,
            None,
            expect![[r#"
                {label:foo/0, kind:Function, contents:SameAsLabel, position:None}
                {label:frog/0, kind:Function, contents:SameAsLabel, position:None}"#]],
        );
    }
}
