/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_syntax::AstNode;
use elp_syntax::algo;
use elp_syntax::ast;
use elp_syntax::ast::Atom;
use hir::AtomDef;
use hir::InFile;
use hir::NameArity;

use crate::Completion;
use crate::Ctx;
use crate::DoneFlag;
use crate::Kind;
use crate::helpers;

pub(crate) fn add_completions(acc: &mut Vec<Completion>, args: &Ctx) -> DoneFlag {
    add_remote(acc, args) || add_local(acc, args)
}

pub(crate) fn add_remote(
    acc: &mut Vec<Completion>,
    args @ Ctx {
        file_position,
        parsed,
        ..
    }: &Ctx,
) -> DoneFlag {
    let node = parsed.value.syntax();
    match algo::find_node_at_offset::<ast::Remote>(node, file_position.offset) {
        None => false,
        Some(remote) => {
            || -> Option<()> {
                let (module_atom, name) = &helpers::split_remote(&remote)?;
                complete_remote_name(acc, args, module_atom, name)?;
                Some(())
            }();
            true
        }
    }
}

fn complete_remote_name(
    acc: &mut Vec<Completion>,
    Ctx {
        file_position,
        sema,
        trigger,
        ..
    }: &Ctx,
    module_atom: &Atom,
    fun_prefix: &str,
) -> Option<()> {
    match trigger {
        Some(':') | None => (),
        _ => return None,
    };
    match sema.to_def(InFile::new(file_position.file_id, module_atom)) {
        Some(AtomDef::Module(module)) => {
            let def_map = sema.def_map(module.file.file_id);
            let completions = def_map
                .get_exported_types()
                .iter()
                .filter(|na| na.name().starts_with(fun_prefix))
                .map(create_call_completion);
            acc.extend(completions);
            Some(())
        }
        _ => None,
    }
}

pub(crate) fn add_local(
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
    let def_map = sema.def_map(file_position.file_id);
    let completions = def_map.get_types().iter().filter_map(|(name_arity, _)| {
        if name_arity.name().starts_with(prefix) {
            Some(create_call_completion(name_arity))
        } else {
            None
        }
    });
    acc.extend(completions);
    false
}

fn create_call_completion(name_arity: &NameArity) -> Completion {
    let contents = helpers::format_call(name_arity.name(), name_arity.arity());
    Completion {
        label: name_arity.to_string(),
        kind: Kind::Type,
        contents,
        position: None,
        sort_text: None,
        deprecated: false,
        additional_edit: None,
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
    fn user_defined_local() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::INTERFACE).unwrap() == "8");
        check(
            r#"
        //- /src/sample.erl
        -module(sample).
        -type alias() :: ok.
        -type alias(T) :: T.
        -opaque alias_opaque() :: secret.
        -opaque alias_opaque(T) :: T.
        -nominal alias_nominal() :: secret.
        -nominal alias_nominal(T) :: T.
        -spec foo() -> a~.
        foo() -> ok.
        //- /src/another_module.erl
        -module(another_module).
        -export_type([alias/0]).
        -type alias() :: ok.
        "#,
            None,
            expect![[r#"
                {label:alias/0, kind:Type, contents:Snippet("alias()"), position:None}
                {label:alias/1, kind:Type, contents:Snippet("alias(${1:Arg1})"), position:None}
                {label:alias_nominal/0, kind:Type, contents:Snippet("alias_nominal()"), position:None}
                {label:alias_nominal/1, kind:Type, contents:Snippet("alias_nominal(${1:Arg1})"), position:None}
                {label:alias_opaque/0, kind:Type, contents:Snippet("alias_opaque()"), position:None}
                {label:alias_opaque/1, kind:Type, contents:Snippet("alias_opaque(${1:Arg1})"), position:None}
                {label:another_module, kind:Module, contents:SameAsLabel, position:None}"#]],
        );
    }

    #[test]
    fn user_defined_remote() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::INTERFACE).unwrap() == "8");

        check(
            r#"
        //- /src/sample.erl
        -module(sample).
        -type alias() :: ok.
        -type alias(T) :: T.
        -spec foo() -> sample2:al~.
        foo() -> ok.
        //- /src/sample2.erl
        -module(sample2).
        -export_type([alias2/0, alias_opaque2/0, alias_opaque2/1, alias_nominal2/0, alias_nominal2/1, bar2/0]).
        -type alias2() :: ok.
        -opaque alias_opaque2() :: secret.
        -opaque alias_opaque2(T) :: T.
        -nominal alias_nominal2() :: secret.
        -nominal alias_nominal2(T) :: T.
        -type ba2() :: ok.
        -type alias_not_exported2() :: ok.
        "#,
            Some(':'),
            expect![[r#"
                {label:alias2/0, kind:Type, contents:Snippet("alias2()"), position:None}
                {label:alias_nominal2/0, kind:Type, contents:Snippet("alias_nominal2()"), position:None}
                {label:alias_nominal2/1, kind:Type, contents:Snippet("alias_nominal2(${1:Arg1})"), position:None}
                {label:alias_opaque2/0, kind:Type, contents:Snippet("alias_opaque2()"), position:None}
                {label:alias_opaque2/1, kind:Type, contents:Snippet("alias_opaque2(${1:Arg1})"), position:None}"#]],
        );

        check(
            r#"
        //- /src/sample.erl
        -module(sample).
        -type alias() :: ok.
        -type alias(T) :: T.
        -spec foo() -> sample2:al~.
        foo() -> ok.
        //- /src/sample2.erl
        -module(sample2).
        -export_type([alias2/0, alias_opaque2/0, alias_opaque2/1, alias_nominal2/0, alias_nominal2/1, bar2/0]).
        -type alias2() :: ok.
        -opaque alias_opaque2() :: secret.
        -opaque alias_opaque2(T) :: T.
        -nominal alias_nominal2() :: secret.
        -nominal alias_nominal2(T) :: T.
        -type ba2() :: ok.
        -type alias_not_exported2() :: ok.
        "#,
            None,
            expect![[r#"
                {label:alias2/0, kind:Type, contents:Snippet("alias2()"), position:None}
                {label:alias_nominal2/0, kind:Type, contents:Snippet("alias_nominal2()"), position:None}
                {label:alias_nominal2/1, kind:Type, contents:Snippet("alias_nominal2(${1:Arg1})"), position:None}
                {label:alias_opaque2/0, kind:Type, contents:Snippet("alias_opaque2()"), position:None}
                {label:alias_opaque2/1, kind:Type, contents:Snippet("alias_opaque2(${1:Arg1})"), position:None}"#]],
        );

        check(
            r#"
        //- /src/sample.erl
        -module(sample).
        -spec foo() -> sample2:~.
        foo() -> ok.
        //- /src/sample2.erl
        -module(sample2).
        -export_type([alias2/0]).
        -type alias2() :: ok.
        "#,
            None,
            expect![[
                r#"{label:alias2/0, kind:Type, contents:Snippet("alias2()"), position:None}"#
            ]],
        );
    }

    #[test]
    fn in_module_part() {
        // really unlikely the user will try to
        // complete in there, but ensure we do
        // something reasonable (show nothing)
        check(
            r#"
        //- /src/sample.erl
        -module(sample).
        -spec foo() -> sample~:.
        foo() -> ok.
        //- /src/sample2.erl
        -module(sample2).
        -export_type([alias2/0]).
        -type alias2() :: ok.
        "#,
            Some(':'),
            expect![""],
        );

        check(
            r#"
        //- /src/sample.erl
        -module(sample).
        -spec foo() -> sample~:.
        foo() -> ok.
        //- /src/sample2.erl
        -module(sample2).
        -export_type([alias2/0]).
        -type alias2() :: ok.
        "#,
            None,
            expect![""],
        );
    }
}
