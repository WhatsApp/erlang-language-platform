/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_base_db::FileId;
use elp_base_db::FilePosition;
use elp_syntax::AstNode;
use elp_syntax::SyntaxToken;
use hir::sema::to_def::resolve_module_name;
use hir::Semantic;

use crate::helpers;
use crate::Completion;
use crate::Ctx;
use crate::DoneFlag;
use crate::Kind;

pub(crate) fn add_completions(
    acc: &mut Vec<Completion>,
    Ctx {
        sema,
        trigger,
        file_position,
        previous_tokens,
        next_token,
        ctx_kind: ctx,
        ..
    }: &Ctx,
) -> DoneFlag {
    use elp_syntax::SyntaxKind as K;
    let default = vec![];
    let previous_tokens: &[_] = previous_tokens.as_ref().unwrap_or(&default);
    match previous_tokens {
        // fun function_name_prefix~
        [.., (K::ANON_FUN, _), (K::ATOM, function_prefix)] if trigger.is_none() => {
            let def_map = sema.def_map(file_position.file_id);

            let completions = def_map.get_functions().filter_map(|(na, _)| {
                helpers::name_slash_arity_completion(na, function_prefix.text(), Kind::Function)
            });
            acc.extend(completions);
            true
        }
        // fun mod:function_name_prefix~
        [
            ..,
            (K::ANON_FUN, _),
            (K::ATOM, module_name),
            (K::ANON_COLON, _),
            (K::ATOM, function_prefix),
        ] if matches!(trigger, Some(':') | None) => {
            if let Some(module) =
                sema.resolve_module_name(file_position.file_id, module_name.text())
            {
                let def_map = sema.def_map(module.file.file_id);
                let completions = def_map.get_exported_functions().iter().filter_map(|na| {
                    helpers::name_slash_arity_completion(na, function_prefix.text(), Kind::Function)
                });
                acc.extend(completions);
                true
            } else {
                false
            }
        }
        // mod:function_name_prefix~
        [
            ..,
            (K::ATOM, module),
            (K::ANON_COLON, _),
            (K::ATOM, name_prefix),
        ] if matches!(trigger, Some(':') | None) => {
            complete_remote_function_call(
                sema,
                file_position.file_id,
                module.text(),
                name_prefix.text(),
                next_token,
                acc,
            );
            true
        }
        // ?MODULE:function_name_prefix~
        [
            ..,
            (K::ANON_QMARK, _),
            (K::VAR, module),
            (K::ANON_COLON, _),
            (K::ATOM, name_prefix),
        ] if matches!(trigger, Some(':') | None) && module.text() == "MODULE" => {
            if let Some(module_name) = sema.module_name(file_position.file_id) {
                complete_remote_function_call(
                    sema,
                    file_position.file_id,
                    module_name.as_str(),
                    name_prefix.text(),
                    next_token,
                    acc,
                );
                true
            } else {
                false
            }
        }
        // mod:
        [.., (K::ATOM, module), (K::ANON_COLON, _)] if matches!(trigger, Some(':') | None) => {
            complete_remote_function_call(
                sema,
                file_position.file_id,
                module.text(),
                "",
                next_token,
                acc,
            );
            true
        }
        // ?MODULE:
        [.., (K::ANON_QMARK, _), (K::VAR, module), (K::ANON_COLON, _)]
            if matches!(trigger, Some(':') | None) && module.text() == "MODULE" =>
        {
            if let Some(module_name) = sema.module_name(file_position.file_id) {
                complete_remote_function_call(
                    sema,
                    file_position.file_id,
                    module_name.as_str(),
                    "",
                    next_token,
                    acc,
                );
                true
            } else {
                false
            }
        }
        // foo
        [.., (K::ATOM, function_prefix)] if trigger.is_none() => {
            let def_map = sema.def_map(file_position.file_id);
            let completions = def_map
                .get_functions_in_scope()
                .filter(|(na, _)| na.name().starts_with(function_prefix.text()))
                .filter_map(|(na, module)| {
                    let function_name = na.name();
                    let module_file_id = module
                        .and_then(|module| {
                            Some(
                                resolve_module_name(sema, file_position.file_id, module.as_str())?
                                    .file
                                    .file_id,
                            )
                        })
                        .unwrap_or(file_position.file_id);
                    let def_map = sema.db.def_map(module_file_id);
                    let def = def_map.get_function(&na)?;
                    let fun_decl_ast = def.source(sema.db.upcast());
                    let deprecated = def_map.is_deprecated(na);
                    match ctx {
                        crate::ctx::CtxKind::Dialyzer => helpers::name_slash_arity_completion(
                            na,
                            function_prefix.text(),
                            Kind::Function,
                        ),
                        _ => {
                            let contents = helpers::function_contents(
                                sema.db.upcast(),
                                &def,
                                function_name,
                                helpers::should_include_args(next_token),
                            )?;
                            Some(Completion {
                                label: na.to_string(),
                                kind: Kind::Function,
                                contents,
                                position: Some(FilePosition {
                                    file_id: def.file.file_id,
                                    offset: fun_decl_ast.first()?.syntax().text_range().start(),
                                }),
                                sort_text: None,
                                deprecated,
                            })
                        }
                    }
                });
            acc.extend(completions);
            false
        }
        _ => false,
    }
}

fn complete_remote_function_call<'a>(
    sema: &'a Semantic,
    from_file: FileId,
    module_name: &'a str,
    fun_prefix: &'a str,
    next_token: &Option<SyntaxToken>,
    acc: &mut Vec<Completion>,
) {
    || -> Option<_> {
        let module = sema.resolve_module_name(from_file, module_name)?;
        let def_map = sema.def_map(module.file.file_id);
        let completions = def_map.get_exported_functions().iter().filter_map(|na| {
            helpers::name_arity_to_call_completion(
                sema,
                module.file.file_id,
                na,
                fun_prefix,
                next_token,
            )
        });
        acc.extend(completions);
        Some(())
    }();
}

#[cfg(test)]
mod test {
    use expect_test::expect;
    use expect_test::Expect;

    use crate::tests::get_completions;
    use crate::tests::render_completions;
    use crate::Kind;

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
    fn test_remote_calls_with_trigger() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::FUNCTION).unwrap() == "3");

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        sample2:~.
    //- /src/sample2.erl
    -module(sample2).
    -export([foo/0]).
    -export([foon/2]).
    -export([bar/2]).
    foo() -> ok.
    foon(A, B) -> ok.
    bar(A, B, C) -> ok.
    "#,
            Some(':'),
            expect![[r#"
                {label:bar/2, kind:Function, contents:Snippet("bar(${1:Arg1}, ${2:Arg2})"), position:None}
                {label:foo/0, kind:Function, contents:Snippet("foo()"), position:Some(FilePosition { file_id: FileId(1), offset: 73 })}
                {label:foon/2, kind:Function, contents:Snippet("foon(${1:A}, ${2:B})"), position:Some(FilePosition { file_id: FileId(1), offset: 86 })}"#]],
        );

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        sample2:f~.
    //- /src/sample2.erl
    -module(sample2).
    -export([foo/0]).
    -export([foon/2]).
    -export([bar/2]).
    foo() -> ok.
    foon(A, B) -> ok.
    bar(A, B, C) -> ok.
    "#,
            Some(':'),
            expect![[r#"
                {label:foo/0, kind:Function, contents:Snippet("foo()"), position:Some(FilePosition { file_id: FileId(1), offset: 73 })}
                {label:foon/2, kind:Function, contents:Snippet("foon(${1:A}, ${2:B})"), position:Some(FilePosition { file_id: FileId(1), offset: 86 })}"#]],
        );
    }

    #[test]
    fn test_otp_calls_no_trigger() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::FUNCTION).unwrap() == "3");

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        len~.

    //- /opt/lib/stdlib-3.17/src/erlang.erl otp_app:/opt/lib/stdlib-3.17
       -module(erlang).
       -export([length/1]).
       length(_) -> 1.
    "#,
            None,
            expect![[
                r#"{label:length/1, kind:Function, contents:Snippet("length(${1:Arg1})"), position:Some(FilePosition { file_id: FileId(1), offset: 47 })}"#
            ]],
        );
    }

    #[test]
    fn test_import_calls_no_trigger() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::FUNCTION).unwrap() == "3");

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    -import(other, [a_fun/1]).
    local() ->
        a_f~.

    //- /src/other.erl
       -module(other).
       -export([a_fun/1]).
       a_fun(_) -> 1.
    "#,
            None,
            expect![[
                r#"{label:a_fun/1, kind:Function, contents:Snippet("a_fun(${1:Arg1})"), position:Some(FilePosition { file_id: FileId(1), offset: 45 })}"#
            ]],
        );
    }

    #[test]
    fn test_remote_calls_no_trigger() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::MODULE).unwrap() == "9");

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        sample2:~.
    //- /src/sample2.erl
    -module(sample2).
    -export([foo/0]).
    -export([foon/2]).
    -export([bar/2]).
    foo() -> ok.
    foon(A, B) -> ok.
    bar(A, B, C) -> ok.
    "#,
            None,
            expect![[r#"
                {label:bar/2, kind:Function, contents:Snippet("bar(${1:Arg1}, ${2:Arg2})"), position:None}
                {label:foo/0, kind:Function, contents:Snippet("foo()"), position:Some(FilePosition { file_id: FileId(1), offset: 73 })}
                {label:foon/2, kind:Function, contents:Snippet("foon(${1:A}, ${2:B})"), position:Some(FilePosition { file_id: FileId(1), offset: 86 })}"#]],
        );

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        sample2:f~.
    //- /src/sample2.erl
    -module(sample2).
    -export([foo/0]).
    -export([foon/2]).
    -export([bar/2]).
    foo() -> ok.
    foon(A, B) -> ok.
    bar(A, B, C) -> ok.
    "#,
            None,
            expect![[r#"
                {label:foo/0, kind:Function, contents:Snippet("foo()"), position:Some(FilePosition { file_id: FileId(1), offset: 73 })}
                {label:foon/2, kind:Function, contents:Snippet("foon(${1:A}, ${2:B})"), position:Some(FilePosition { file_id: FileId(1), offset: 86 })}"#]],
        );
    }
    #[test]
    fn test_remote_calls_deprecated() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::MODULE).unwrap() == "9");

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        sample2:~.
    //- /src/sample2.erl
    -module(sample2).
    -deprecated({foon, 2, "Don't use me!"}).
    -export([foo/0]).
    -export([foon/2]).
    -export([bar/2]).
    foo() -> ok.
    foon(A, B) -> ok.
    bar(A, B, C) -> ok.
    "#,
            None,
            expect![[r#"
                {label:bar/2, kind:Function, contents:Snippet("bar(${1:Arg1}, ${2:Arg2})"), position:None}
                {label:foo/0, kind:Function, contents:Snippet("foo()"), position:Some(FilePosition { file_id: FileId(1), offset: 114 })}
                {label:foon/2, kind:Function, contents:Snippet("foon(${1:A}, ${2:B})"), position:Some(FilePosition { file_id: FileId(1), offset: 127 }), deprecated:true}"#]],
        );
    }

    #[test]
    fn test_remote_calls_multiple_deprecated() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::MODULE).unwrap() == "9");

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        sample2:~.
    //- /src/sample2.erl
    -module(sample2).
    -deprecated([{foon, 2, "Don't use me!"}, {foo, 0}]).
    -export([foo/0]).
    -export([foon/2]).
    -export([bar/2]).
    foo() -> ok.
    foon(A, B) -> ok.
    bar(A, B, C) -> ok.
    "#,
            None,
            expect![[r#"
                {label:bar/2, kind:Function, contents:Snippet("bar(${1:Arg1}, ${2:Arg2})"), position:None}
                {label:foo/0, kind:Function, contents:Snippet("foo()"), position:Some(FilePosition { file_id: FileId(1), offset: 126 }), deprecated:true}
                {label:foon/2, kind:Function, contents:Snippet("foon(${1:A}, ${2:B})"), position:Some(FilePosition { file_id: FileId(1), offset: 139 }), deprecated:true}"#]],
        );
    }

    #[test]
    fn test_remote_module_deprecated() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::MODULE).unwrap() == "9");

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        sample2:~.
    //- /src/sample2.erl
    -module(sample2).
    -deprecated(module).
    -export([foo/0]).
    -export([bar/2]).
    foo() -> ok.
    bar(A, B, C) -> ok.
    "#,
            None,
            expect![[r#"
                    {label:bar/2, kind:Function, contents:Snippet("bar(${1:Arg1}, ${2:Arg2})"), position:None, deprecated:true}
                    {label:foo/0, kind:Function, contents:Snippet("foo()"), position:Some(FilePosition { file_id: FileId(1), offset: 75 }), deprecated:true}"#]],
        );
    }

    #[test]
    fn test_remote_module_incorrect_deprecation() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::MODULE).unwrap() == "9");

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        sample2:~.
    //- /src/sample2.erl
    -module(sample2).
    -deprecated(incorrect).
    -export([foo/0]).
    foo() -> ok.
    "#,
            None,
            expect![[r#"
                    {label:foo/0, kind:Function, contents:Snippet("foo()"), position:Some(FilePosition { file_id: FileId(1), offset: 60 })}"#]],
        );
    }

    #[test]
    fn test_no_function_completions() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::MODULE).unwrap() == "9");
        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        sample2:bar(a, s~)
    //- /src/sample2.erl
    -module(sample2).
    -export([bar/0]).
    -export([bar/2]).
    -export([baz/3]).
    bar() -> ok.
    bar(A, B) -> ok.
    baz(A, B, C) -> ok.
    "#,
            None,
            expect![[r#"
                {label:sample1, kind:Module, contents:SameAsLabel, position:None}
                {label:sample2, kind:Module, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn test_local_calls_1() {
        check(
            r#"
    -module(sample1).
    foo() -> id(b~).
    id(X) -> X.
    bar() -> ok.
    bar(X) -> X.
    baz(X, _) -> X.
    "#,
            None,
            expect![[r#"
                {label:bar/0, kind:Function, contents:Snippet("bar()"), position:Some(FilePosition { file_id: FileId(0), offset: 46 })}
                {label:bar/1, kind:Function, contents:Snippet("bar(${1:X})"), position:Some(FilePosition { file_id: FileId(0), offset: 59 })}
                {label:baz/2, kind:Function, contents:Snippet("baz(${1:X}, ${2:Arg2})"), position:Some(FilePosition { file_id: FileId(0), offset: 72 })}"#]],
        );
    }

    #[test]
    fn test_local_calls_2() {
        check(
            r#"
    -module(sample1).
    foo() ->
        b~.
    bar() -> ok.
    baz(X) -> X.
    "#,
            None,
            expect![[r#"
                {label:bar/0, kind:Function, contents:Snippet("bar()"), position:Some(FilePosition { file_id: FileId(0), offset: 34 })}
                {label:baz/1, kind:Function, contents:Snippet("baz(${1:X})"), position:Some(FilePosition { file_id: FileId(0), offset: 47 })}"#]],
        );
    }

    #[test]
    fn test_local_calls_3() {
        check(
            r#"
    -module(sample).
    test() ->
        try 1
        of
            1 -> ok
        catch
            b~ -> ok
        end.
    bar() -> ok.
    baz(X) -> X.
            "#,
            None,
            expect![""],
        );
    }

    #[test]
    fn test_local_call_arg_names() {
        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    foo(X, Y, Y, _, _, {W, 3}, [H], [], 99, Z, _) -> ok.
    main(_) ->
        fo~
    "#,
            None,
            expect![[
                r#"{label:foo/11, kind:Function, contents:Snippet("foo(${1:X}, ${2:Y}, ${3:Y}, ${4:Arg4}, ${5:Arg5}, ${6:Arg6}, ${7:Arg7}, ${8:Arg8}, ${9:Arg9}, ${10:Z}, ${11:Arg11})"), position:Some(FilePosition { file_id: FileId(0), offset: 18 })}"#
            ]],
        );
    }

    #[test]
    fn test_remote_fun_exprs_with_trigger() {
        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    main(_) ->
        lists:map(fun sample2:foo/~), [])
    foo(_, _, _) -> ok.
    //- /src/sample2.erl
    -module(sample2).
    -export([foo/0]).
    -export([foo/2]).
    -export([bar/2]).
    foo() -> ok.
    foo(A, B) -> ok.
    bar(A, B) -> ok.
    "#,
            Some(':'),
            expect![""],
        );
    }

    #[test]
    fn test_local_fun_exprs_with_trigger() {
        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    foo() -> ok.
    main(_) ->
        fun fo~
    "#,
            Some(':'),
            expect![""],
        );
    }

    #[test]
    fn test_local_fun_exprs_no_trigger() {
        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    foo() -> ok.
    main(_) ->
        fun fo~
    "#,
            None,
            expect!["{label:foo/0, kind:Function, contents:SameAsLabel, position:None}"],
        );
    }

    #[test]
    fn function_error_recovery() {
        check(
            r#"
    -module(sample1).
    foo() ->
        b~
    bar() -> ok.
    baz(X) -> X.
    "#,
            None,
            expect![[r#"
                {label:bar/0, kind:Function, contents:Snippet("bar()"), position:Some(FilePosition { file_id: FileId(0), offset: 33 })}
                {label:baz/1, kind:Function, contents:Snippet("baz(${1:X})"), position:Some(FilePosition { file_id: FileId(0), offset: 46 })}"#]],
        );
        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        lists:map(fun sample2:f~, [])
    //- /src/sample2.erl
    -module(sample2).
    -export([foo/0]).
    foo() -> ok.
    "#,
            Some(':'),
            expect!["{label:foo/0, kind:Function, contents:SameAsLabel, position:None}"],
        );
    }

    #[test]
    fn test_local_and_remote() {
        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    samba() -> ok.
    main(_) ->
        lists:map(fun sa~), [])
    //- /src/sample2.erl
    -module(sample2).
    -export([foo/0]).
    foo() -> ok.

    "#,
            None,
            expect!["{label:samba/0, kind:Function, contents:SameAsLabel, position:None}"],
        );
    }

    #[test]
    fn test_remote_call_broken_case() {
        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    test() ->
        case sample2:m~
        A = 42,
    //- /src/sample2.erl
    -module(sample2).
    -export([main/0]).
    main() -> ok.
    "#,
            None,
            expect![[
                r#"{label:main/0, kind:Function, contents:Snippet("main()"), position:Some(FilePosition { file_id: FileId(1), offset: 37 })}"#
            ]],
        );
    }

    #[test]
    fn test_local_call_broken_case() {
        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    foo() -> ok.
    test() ->
        case fo~
        A = 42,
    "#,
            None,
            expect![[
                r#"{label:foo/0, kind:Function, contents:Snippet("foo()"), position:Some(FilePosition { file_id: FileId(0), offset: 18 })}"#
            ]],
        );
    }

    #[test]
    fn test_local_call_macro_rhs() {
        check(
            r#"
    -module(main).
    -define(MY_MACRO(), my_f~unction()).
    my_function() -> ok.
    "#,
            None,
            expect![[
                r#"{label:my_function/0, kind:Function, contents:Snippet("my_function()"), position:Some(FilePosition { file_id: FileId(0), offset: 51 })}"#
            ]],
        );
    }

    #[test]
    fn test_remote_call_header_macro_rhs() {
        check(
            r#"
    //- /include/main.hrl
    -define(MY_MACRO(), main:my_f~unction()).
    //- /src/main.erl
    -module(main).
    -export([my_function/0]).
    my_function() -> ok.
    "#,
            None,
            expect![[
                r#"{label:my_function/0, kind:Function, contents:Snippet("my_function()"), position:Some(FilePosition { file_id: FileId(1), offset: 41 })}"#
            ]],
        );
    }

    #[test]
    fn test_local_call_parentheses() {
        check(
            r#"
    -module(sample).
    test() ->
        fo~(something).
    foo(X) -> ok.
            "#,
            None,
            expect![[
                r#"{label:foo/1, kind:Function, contents:Snippet("foo"), position:Some(FilePosition { file_id: FileId(0), offset: 46 })}"#
            ]],
        );
    }

    #[test]
    fn test_remote_call_parentheses() {
        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        sample2:fo~().
    //- /src/sample2.erl
    -module(sample2).
    -export([foo/0]).
    -export([foon/2]).
    -export([bar/2]).
    foo() -> ok.
    foon(A, B) -> ok.
    bar(A, B, C) -> ok.
            "#,
            None,
            expect![[r#"
                {label:foo/0, kind:Function, contents:Snippet("foo"), position:Some(FilePosition { file_id: FileId(1), offset: 73 })}
                {label:foon/2, kind:Function, contents:Snippet("foon"), position:Some(FilePosition { file_id: FileId(1), offset: 86 })}"#]],
        );
    }

    #[test]
    fn test_remote_call_with_spec() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::MODULE).unwrap() == "9");

        check(
            r#"
    //- /src/sample1.erl
    -module(sample1).
    local() ->
        sample2:~.
    //- /src/sample2.erl
    -module(sample2).
    -export([foo/1, foo/2]).
    -spec foo(Override :: integer()) -> ok.
    foo(X) -> ok.
    foo(X, Y) -> ok.
    "#,
            None,
            expect![[r#"
                {label:foo/1, kind:Function, contents:Snippet("foo(${1:Override})"), position:Some(FilePosition { file_id: FileId(1), offset: 83 })}
                {label:foo/2, kind:Function, contents:Snippet("foo(${1:X}, ${2:Y})"), position:Some(FilePosition { file_id: FileId(1), offset: 97 })}"#]],
        );
        check(
            r#"
//- /src/sample1.erl
-module(sample1).
local() ->
    sample2:~.
//- /src/sample2.erl
-module(sample2).
-export([foo/1, foo/2]).
-spec foo(Override :: integer()) -> ok.
foo(X) -> ok.
-spec foo(Override :: integer(), integer()) -> ok.
foo(X, Y) -> ok.
"#,
            None,
            expect![[r#"
            {label:foo/1, kind:Function, contents:Snippet("foo(${1:Override})"), position:Some(FilePosition { file_id: FileId(1), offset: 83 })}
            {label:foo/2, kind:Function, contents:Snippet("foo(${1:Override}, ${2:Arg2})"), position:Some(FilePosition { file_id: FileId(1), offset: 148 })}"#]],
        );
        check(
            r#"
//- /src/sample1.erl
-module(sample1).
local() ->
    sample2:~.
//- /src/sample2.erl
-module(sample2).
-export([foo/1, foo/2]).
-spec foo(integer()) -> ok.
foo(X) -> ok.
-spec foo(Override :: integer(), integer()) -> ok.
foo(X, Y) -> ok.
"#,
            None,
            expect![[r#"
            {label:foo/1, kind:Function, contents:Snippet("foo(${1:X})"), position:Some(FilePosition { file_id: FileId(1), offset: 71 })}
            {label:foo/2, kind:Function, contents:Snippet("foo(${1:Override}, ${2:Arg2})"), position:Some(FilePosition { file_id: FileId(1), offset: 136 })}"#]],
        );
    }

    #[test]
    fn test_remote_call_same_module() {
        check(
            r#"
    -module(main).
    -export([main/0, do_not_use_me/0]).
    -deprecated({do_not_use_me, 0, "Because I said so"}).
    main() ->
        main:~
        ok.
    do_not_use_me() ->
        i_am_useless.
            "#,
            None,
            expect![[r#"
                {label:do_not_use_me/0, kind:Function, contents:Snippet("do_not_use_me()"), position:Some(FilePosition { file_id: FileId(0), offset: 133 }), deprecated:true}
                {label:main/0, kind:Function, contents:Snippet("main()"), position:Some(FilePosition { file_id: FileId(0), offset: 105 })}"#]],
        );
    }

    #[test]
    fn test_remote_call_same_module_macro() {
        check(
            r#"
    -module(main).
    -export([main/0, do_not_use_me/0]).
    -deprecated({do_not_use_me, 0, "Because I said so"}).
    main() ->
        ?MODULE:~
        ok.
    do_not_use_me() ->
        i_am_useless.
            "#,
            None,
            expect![[r#"
                {label:do_not_use_me/0, kind:Function, contents:Snippet("do_not_use_me()"), position:Some(FilePosition { file_id: FileId(0), offset: 136 }), deprecated:true}
                {label:main/0, kind:Function, contents:Snippet("main()"), position:Some(FilePosition { file_id: FileId(0), offset: 105 })}"#]],
        );
    }

    #[test]
    fn test_remote_call_same_module_macro_prefix() {
        check(
            r#"
    -module(main).
    -export([main/0, do_not_use_me/0]).
    -deprecated({do_not_use_me, 0, "Because I said so"}).
    main() ->
        ?MODULE:do~
        ok.
    do_not_use_me() ->
        i_am_useless.
            "#,
            None,
            expect![[
                r#"{label:do_not_use_me/0, kind:Function, contents:Snippet("do_not_use_me()"), position:Some(FilePosition { file_id: FileId(0), offset: 138 }), deprecated:true}"#
            ]],
        );
    }

    #[test]
    fn test_in_dialyzer_attribute() {
        check(
            r#"
    -module(main).
    -export([
        foo/1,
        is_internal/1
    ]).
    foo(X) when is_binary(X) ->
        X.
    -dialyzer({nowarn, is~})
    is_internal(Y) ->
        is_internal_impl(foo(Y)).
    "#,
            None,
            expect!["{label:is_internal/1, kind:Function, contents:SameAsLabel, position:None}"],
        );
    }
}
