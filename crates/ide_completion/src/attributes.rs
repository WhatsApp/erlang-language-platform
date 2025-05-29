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

pub(crate) fn add_completions(
    acc: &mut Vec<Completion>,
    Ctx {
        sema,
        previous_tokens,
        file_position,
        trigger,
        ..
    }: &Ctx,
) -> DoneFlag {
    use elp_syntax::SyntaxKind as K;
    let default = vec![];
    let previous_tokens: &[_] = previous_tokens.as_ref().unwrap_or(&default);
    match previous_tokens {
        // -behavior(behavior_name_prefix~
        [
            ..,
            (K::ANON_DASH, _),
            (K::ANON_BEHAVIOR | K::ANON_BEHAVIOUR, _),
            (K::ANON_LPAREN, _),
            (K::ATOM, behavior_name_prefix),
        ] if trigger.is_none() => || -> _ {
            let modules = sema.resolve_module_names(file_position.file_id)?;
            let completions = modules.into_iter().filter_map(|m| {
                if m.starts_with(behavior_name_prefix.text()) {
                    let module = sema.resolve_module_name(file_position.file_id, &m)?;
                    let def_map = sema.def_map(module.file.file_id);
                    if def_map.get_callbacks().is_empty() {
                        None
                    } else {
                        Some(Completion {
                            label: m.to_string(),
                            kind: Kind::Behavior,
                            contents: Contents::SameAsLabel,
                            position: None,
                            sort_text: None,
                            deprecated: false,
                            additional_edit: None,
                        })
                    }
                } else {
                    None
                }
            });

            acc.extend(completions);
            Some(true)
        }()
        .unwrap_or_default(),

        [.., (K::ANON_DASH, _), (K::ATOM, attr_name)] if matches!(trigger, Some('-') | None) => {
            if "module".starts_with(attr_name.text()) {
                if let Some(module) = sema.module_name(file_position.file_id) {
                    acc.push(Completion {
                        kind: Kind::Attribute,
                        label: format!("-module({}).", module.to_quoted_string()),
                        contents: Contents::Snippet(format!(
                            "module({}).",
                            module.to_quoted_string()
                        )),
                        position: None,
                        sort_text: None,
                        deprecated: false,
                        additional_edit: None,
                    });
                    true
                } else {
                    false
                }
            } else if "typing".starts_with(attr_name.text()) {
                acc.push(Completion {
                    kind: Kind::Attribute,
                    label: "-typing([eqwalizer]).".to_string(),
                    contents: Contents::Snippet("typing([eqwalizer]).".to_string()),
                    position: None,
                    sort_text: None,
                    deprecated: false,
                    additional_edit: None,
                });
                true
            } else {
                false
            }
        }
        // A common VSCode extension already has snippets for most attributes, so no need to include those here
        _ => false,
    }
}

#[cfg(test)]
mod test {
    use expect_test::Expect;
    use expect_test::expect;

    use crate::tests::get_completions;
    use crate::tests::render_completions;

    fn check(code: &str, trigger: Option<char>, expect: Expect) {
        let completions = get_completions(code, trigger);
        let actual = &render_completions(completions);
        expect.assert_eq(actual);
    }

    #[test]
    fn test_user_defined_behaviors() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::INTERFACE).unwrap() == "8");

        check(
            r#"
    //- /src/sample.erl
    -module(sample1).
    -behavior(gen~).
    //- /src/gen_book.erl
    -module(gen_book).
    -callback bookit(term()) -> term().
    //- /src/gen_look.erl
    -module(gen_look).
    -callback lookit(term()) -> term().
    //- /src/other_behavior.erl
    -module(other_behavior).
    -callback other(term()) -> term().
    //- /src/gen_no_behavior.erl
    % should not show up in completions
    -module(gen_no_behavior).
    "#,
            None,
            expect![[r#"
                {label:gen_book, kind:Behavior, contents:SameAsLabel, position:None}
                {label:gen_look, kind:Behavior, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_otp_behaviors() {
        check(
            r#"
//- /src/sample1.erl
-module(sample1).
-behavior(gen~).
//- /opt/lib/stdlib-3.17/src/gen_server.erl otp_app:/opt/lib/stdlib-3.17
-module(gen_server).
-callback init(term()) -> term().
"#,
            None,
            expect!["{label:gen_server, kind:Behavior, contents:SameAsLabel, position:None}"],
        );
    }

    #[test]
    fn test_error_recovery() {
        check(
            r#"
            //- /src/sample.erl
            -module(sample1).
            % U.S. English
            -behavior(gen~
            //- /src/gen_book.erl
            -module(gen_book).
            -callback bookit(term()) -> term().
        "#,
            None,
            expect!["{label:gen_book, kind:Behavior, contents:SameAsLabel, position:None}"],
        );

        check(
            r#"
            //- /src/sample.erl
            -module(sample1).
            % U.K. English
            -behaviour(gen~
            //- /src/gen_book.erl
            -module(gen_book).
            -callback bookit(term()) -> term().
        "#,
            None,
            expect!["{label:gen_book, kind:Behavior, contents:SameAsLabel, position:None}"],
        );
    }

    #[test]
    fn test_typing_attribute() {
        check(
            r#"
        -module(sample).
        -typ~
        "#,
            None,
            expect![[
                r#"{label:-typing([eqwalizer])., kind:Attribute, contents:Snippet("typing([eqwalizer])."), position:None}"#
            ]],
        );
    }

    #[test]
    fn test_module_attribute() {
        check(
            r#"
        -mod~
        "#,
            None,
            expect![[
                r#"{label:-module(main)., kind:Attribute, contents:Snippet("module(main)."), position:None}"#
            ]],
        );
    }

    #[test]
    fn test_module_attribute_hyphen() {
        check(
            r#"
        //- /src/my-module.erl
        -mod~
        "#,
            None,
            expect![[
                r#"{label:-module('my-module')., kind:Attribute, contents:Snippet("module('my-module')."), position:None}"#
            ]],
        );
    }

    #[test]
    fn test_module_attribute_at() {
        check(
            r#"
        //- /src/my@module.erl
        -mod~
        "#,
            None,
            expect![[
                r#"{label:-module(my@module)., kind:Attribute, contents:Snippet("module(my@module)."), position:None}"#
            ]],
        );
    }

    #[test]
    fn test_module_attribute_underscore() {
        check(
            r#"
        //- /src/my_module.erl
        -mod~
        "#,
            None,
            expect![[
                r#"{label:-module(my_module)., kind:Attribute, contents:Snippet("module(my_module)."), position:None}"#
            ]],
        );
    }

    #[test]
    fn test_module_attribute_uppercase() {
        check(
            r#"
        //- /src/Module.erl
        -mod~
        "#,
            None,
            expect![[
                r#"{label:-module('Module')., kind:Attribute, contents:Snippet("module('Module')."), position:None}"#
            ]],
        );
    }

    #[test]
    fn test_module_attribute_uppercase_middle() {
        check(
            r#"
        //- /src/moDule.erl
        -mod~
        "#,
            None,
            expect![[
                r#"{label:-module(moDule)., kind:Attribute, contents:Snippet("module(moDule)."), position:None}"#
            ]],
        );
    }
}
