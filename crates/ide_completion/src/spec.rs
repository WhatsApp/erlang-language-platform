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

pub(crate) fn add_completions(
    acc: &mut Vec<Completion>,
    Args {
        file_position,
        parsed,
        sema,
        trigger,
        previous_tokens,
        next_token,
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
                    if let Some(mut completion) = helpers::name_arity_to_call_completion(
                        sema,
                        file_position.file_id,
                        &sp.name,
                        spec_fun_prefix.text(),
                        next_token,
                    ) {
                        fun_completion_to_spec(&mut completion);
                        acc.push(completion);
                    }
                }
            }

            let def_map = sema.def_map(file_position.file_id);

            let completions = def_map.get_functions().filter_map(|(na, _)| {
                if let Some(mut completion) = helpers::name_arity_to_call_completion(
                    sema,
                    file_position.file_id,
                    na,
                    spec_fun_prefix.text(),
                    next_token,
                ) {
                    fun_completion_to_spec(&mut completion);
                    Some(completion)
                } else {
                    None
                }
            });
            acc.extend(completions);
            true
        }
        _ => false,
    };
}

fn fun_completion_to_spec(completion: &mut Completion) {
    match &completion.contents {
        crate::Contents::SameAsLabel => {}
        crate::Contents::String(s) => {
            completion.contents = crate::Contents::String(format!("{s} -> return_type()."));
        }
        crate::Contents::Snippet(s) => {
            completion.contents = crate::Contents::Snippet(format!("{s} -> return_type()."));
        }
    }
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
                {label:foo/0, kind:Function, contents:Snippet("foo() -> return_type()."), position:None}
                {label:frog/0, kind:Function, contents:Snippet("frog() -> return_type()."), position:Some(FilePosition { file_id: FileId(0), offset: 18 })}"#]],
        );
    }

    #[test]
    fn test_spec_with_args() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::INTERFACE).unwrap() == "8");

        check(
            r#"
        -module(sample).

        frog(A, B) -> {A, B}.

        -spec f~
        foo(X, Y) -> {X,Y}.
        "#,
            None,
            expect![[r#"
                {label:foo/2, kind:Function, contents:Snippet("foo(${1:Arg1}, ${2:Arg2}) -> return_type()."), position:None}
                {label:frog/2, kind:Function, contents:Snippet("frog(${1:A}, ${2:B}) -> return_type()."), position:Some(FilePosition { file_id: FileId(0), offset: 18 })}"#]],
        );
    }
}
