/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::max;

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_syntax::ast::BehaviourAttribute;
use elp_syntax::AstNode;
use hir::Callback;
use hir::CallbackId;
use hir::InFile;
use hir::Module;
use hir::NameArity;
use text_edit::TextRange;
use text_edit::TextSize;

use crate::assist_context::AssistContext;
use crate::assist_context::Assists;
use crate::helpers;

// Assist: implement_behaviour
//
// Implement and export all callbacks when on behaviour attribute
//
// ```
// -behaviour(gen_server).
//
// ->
// ```
// -behaviour(gen_server).
//
// %% Callbacks for `gen_server`
// -export([init/1, handle_call/3, handle_cast/2]).
//
// init(Args) ->
//     erlang:error(not_implemented).
//
// handle_call(Request,From,State) ->
//     erlang:error(not_implemented).
//
// handle_cast(Request,State) ->
//     erlang:error(not_implemented).
// ```

pub(crate) fn implement_behaviour(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    let behaviour_ast = ctx.find_node_at_offset::<BehaviourAttribute>()?;
    let behaviour = ctx
        .sema
        .to_def(InFile::new(ctx.file_id(), &behaviour_ast))?;
    let our_def_map = ctx.sema.def_map(ctx.file_id());
    let our_forms = ctx.sema.form_list(ctx.file_id());
    let module_def_map = ctx.sema.def_map(behaviour.file.file_id);
    let behaviour_forms = ctx.sema.form_list(behaviour.file.file_id);
    let mut existing_callback = None;
    let mut existing_optional_callback = None;
    let mut additions = Vec::default();
    let mut optional_additions = Vec::default();
    behaviour_forms
        .callback_attributes()
        .for_each(|(idx, callback)| {
            let implemented = our_forms.functions().find(|(_, f)| f.name == callback.name);
            match implemented {
                Some((_idx, fun)) => {
                    if module_def_map.is_callback_optional(&callback.name) {
                        if existing_optional_callback.is_none() {
                            existing_optional_callback = Some(callback.name.clone());
                        }
                        if !our_def_map.is_function_exported(&fun.name) {
                            optional_additions.push((idx, Kind::ExportOnly(&fun.name)));
                        }
                    } else {
                        if existing_callback.is_none() {
                            existing_callback = Some(callback.name.clone());
                        }
                        if !our_def_map.is_function_exported(&fun.name) {
                            additions.push((idx, Kind::ExportOnly(&fun.name)));
                        }
                    }
                }
                None => {
                    if module_def_map.is_callback_optional(&callback.name) {
                        optional_additions.push((idx, Kind::CallBack(callback)));
                    } else {
                        additions.push((idx, Kind::CallBack(callback)));
                    }
                }
            };
        });

    let attr_range = behaviour_ast.syntax().text_range();
    let export_range = our_forms.exports().last().map(|(_idx, export)| {
        export
            .form_id
            .get_ast(ctx.sema.db, ctx.file_id())
            .syntax()
            .text_range()
    });
    let insert_start = match export_range {
        Some(range) => max(range.end(), attr_range.end()),
        None => attr_range.end(),
    };
    let insert_at = insert_start + TextSize::from(1);

    let mut implement_callbacks =
        ImplementCallbacks::new(ctx, &behaviour, attr_range, insert_at, acc);

    if !additions.is_empty() {
        let id = AssistId("implement_callbacks", AssistKind::QuickFix);
        let message = format!(
            "Create callbacks for '{}'",
            behaviour.name(ctx.sema.db).as_str()
        );
        let comment = format!("Callbacks for `{}`", behaviour.name(ctx.db()));
        implement_callbacks.add_assist(id, additions, message, comment, existing_callback);
    }

    if !optional_additions.is_empty() {
        let id = AssistId("implement_optional_callbacks", AssistKind::QuickFix);
        let message = format!(
            "Create optional callbacks for '{}'",
            behaviour.name(ctx.sema.db)
        );
        let comment = format!("Optional callbacks for `{}`", behaviour.name(ctx.db()));
        implement_callbacks.add_assist(
            id,
            optional_additions,
            message,
            comment,
            existing_optional_callback,
        );
    }
    Some(())
}

enum Kind<'a> {
    CallBack(&'a Callback),
    ExportOnly(&'a NameArity),
}

struct ImplementCallbacks<'a> {
    ctx: &'a AssistContext<'a>,
    behaviour: &'a Module,
    acc: &'a mut Assists,
    attr_range: TextRange,
    insert_at: TextSize,
}

impl<'a> ImplementCallbacks<'a> {
    fn new(
        ctx: &'a AssistContext<'a>,
        behaviour: &'a Module,
        attr_range: TextRange,
        insert_at: TextSize,
        acc: &'a mut Assists,
    ) -> ImplementCallbacks<'a> {
        ImplementCallbacks {
            ctx,
            behaviour,
            attr_range,
            insert_at,
            acc,
        }
    }

    fn add_assist(
        &mut self,
        id: AssistId,
        additions: Vec<(CallbackId, Kind)>,
        message: String,
        comment: String,
        existing_callback: Option<NameArity>,
    ) {
        let (funs, texts) = build_assist(self.ctx, self.behaviour, additions);
        self.acc.add(id, message, self.attr_range, None, |builder| {
            let mut export_builder =
                helpers::ExportBuilder::new(&self.ctx.sema, self.ctx.file_id(), &funs, builder)
                    .insert_at(self.insert_at)
                    .with_comment(comment);
            if let Some(existing) = existing_callback {
                export_builder = export_builder.group_with(existing)
            }
            export_builder.finish();
            builder.edit_file(self.ctx.frange.file_id);
            let mut text = texts.join("\n");
            text.push('\n');
            builder.insert(self.insert_at, text)
        });
    }
}

fn build_assist(
    ctx: &AssistContext<'_>,
    behaviour: &Module,
    additions: Vec<(CallbackId, Kind)>,
) -> (Vec<NameArity>, Vec<String>) {
    let (funs, texts): (Vec<NameArity>, Vec<String>) = additions
        .into_iter()
        .filter_map(|(idx, callback)| make_implementation(ctx, behaviour, idx, callback))
        .fold((vec![], vec![]), |(mut funs, mut msgs), (fun, msg)| {
            funs.push(fun);
            if let Some(msg) = msg {
                msgs.push(msg);
            }
            (funs, msgs)
        });
    (funs, texts)
}

fn make_implementation(
    ctx: &AssistContext<'_>,
    behaviour: &Module,
    idx: CallbackId,
    callback: Kind,
) -> Option<(NameArity, Option<String>)> {
    let callback_body = ctx
        .sema
        .db
        .callback_body(InFile::new(behaviour.file.file_id, idx));

    match callback {
        Kind::CallBack(callback) => {
            let function_name = callback.name.name();
            if let Some(sig) = callback_body.sigs.first() {
                let function_args =
                    ctx.create_function_args_from_types(&sig.args, &callback_body.body);
                let addition = (
                    callback.name.clone(),
                    Some(format!(
                        "\n{}({}) ->\n    erlang:error(not_implemented).",
                        function_name, function_args
                    )),
                );
                Some(addition)
            } else {
                None
            }
        }
        Kind::ExportOnly(name) => Some((name.clone(), None)),
    }
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::*;

    #[test]
    fn implement_behaviour_from_scratch() {
        check_assist(
            implement_behaviour,
            "Create callbacks for 'supervisor'",
            r#"
            //- /src/main.erl
            -module(main).
            -behaviour(sup~ervisor).

            //- /opt/lib/stdlib-4.31/src/supervisor.erl otp_app:/opt/lib/stdlib-4.31
            -module(supervisor).
            -callback init(Args :: term()) ->
                {ok, {SupFlags :: sup_flags(), [ChildSpec :: child_spec()]}}
                | ignore.
                        -module(lists).
                        -export([reverse/1]).
                        reverse([]) -> [].
             "#,
            expect![[r#"
                -module(main).
                -behaviour(supervisor).

                %% Callbacks for `supervisor`
                -export([init/1]).

                init(Args) ->
                    erlang:error(not_implemented).

            "#]],
        )
    }

    #[test]
    fn implement_behaviour_from_scratch_2() {
        check_assist(
            implement_behaviour,
            "Create callbacks for 'gen_server'",
            r#"
            //- /src/main.erl
            -module(main).
            -behaviour(gen_s~erver).

            existing_fun() -> ok.

            //- /opt/lib/stdlib-4.31/src/gen_server.erl otp_app:/opt/lib/stdlib-4.31
            -module(gen_server).
            -callback init(Args :: term()) ->
                {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate | {continue, term()}} |
                {stop, Reason :: term()} | ignore.
            -callback handle_call(Request :: term(), From :: from(),
                                  State :: term()) ->
                {reply, Reply :: term(), NewState :: term()} |
                {reply, Reply :: term(), NewState :: term(), timeout() | hibernate | {continue, term()}} |
                {noreply, NewState :: term()} |
                {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
                {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                {stop, Reason :: term(), NewState :: term()}.
             "#,
            expect![[r#"
                -module(main).
                -behaviour(gen_server).

                %% Callbacks for `gen_server`
                -export([init/1, handle_call/3]).

                init(Args) ->
                    erlang:error(not_implemented).

                handle_call(Request,From,State) ->
                    erlang:error(not_implemented).

                existing_fun() -> ok.

            "#]],
        )
    }

    #[test]
    fn implement_balance_of_behaviour_callbacks_1() {
        check_assist(
            implement_behaviour,
            "Create callbacks for 'my_behaviour'",
            r#"
            //- /src/main.erl
            -module(main).
            -behaviour(my_b~ehaviour).
            -export([init/1]).

            init(_) -> already_done,ok.

            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback init(Args :: term()) -> ok.
            -callback another() -> ok.
             "#,
            expect![[r#"
                -module(main).
                -behaviour(my_behaviour).
                -export([init/1, another/0]).

                another() ->
                    erlang:error(not_implemented).

                init(_) -> already_done,ok.

            "#]],
        )
    }

    #[test]
    fn implement_balance_of_behaviour_callbacks_2() {
        check_assist(
            implement_behaviour,
            "Create callbacks for 'my_behaviour'",
            r#"
            //- /src/main.erl
            -module(main).
            -behaviour(my_b~ehaviour).

            -export([foo/0]).

            %% Callbacks for `my_behaviour`
            -export([init/1]).

            init(_) -> already_done,ok.

            foo() -> ok.

            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback init(Args :: term()) -> ok.
            -callback another() -> ok.
             "#,
            expect![[r#"
                -module(main).
                -behaviour(my_behaviour).

                -export([foo/0]).

                %% Callbacks for `my_behaviour`
                -export([init/1, another/0]).

                another() ->
                    erlang:error(not_implemented).

                init(_) -> already_done,ok.

                foo() -> ok.

            "#]],
        )
    }

    #[test]
    fn implement_balance_of_behaviour_callbacks_3() {
        check_assist(
            implement_behaviour,
            "Create callbacks for 'my_behaviour'",
            r#"
            //- /src/main.erl
            -module(main).
            -export([foo/0]).

            -behaviour(my_b~ehaviour).

            init(_) -> already_done,ok.

            foo() -> ok.

            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback init(Args :: term()) -> ok.
            -callback another() -> ok.
             "#,
            expect![[r#"
                -module(main).
                -export([foo/0]).

                -behaviour(my_behaviour).

                %% Callbacks for `my_behaviour`
                -export([init/1, another/0]).

                another() ->
                    erlang:error(not_implemented).

                init(_) -> already_done,ok.

                foo() -> ok.

            "#]],
        )
    }

    #[test]
    fn optional_callbacks_skipped() {
        check_assist(
            implement_behaviour,
            "Create callbacks for 'my_behaviour'",
            r#"
            //- /src/main.erl
            -module(main).
            -behaviour(my_b~ehaviour).

            init(_) -> already_done,ok.

            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback init(Args :: term()) -> ok.
            -callback another() -> ok.
            -callback optional() -> ok.
            -optional_callbacks([
                init/1, optional/0
            ]).
             "#,
            expect![[r#"
                -module(main).
                -behaviour(my_behaviour).

                %% Callbacks for `my_behaviour`
                -export([another/0]).

                another() ->
                    erlang:error(not_implemented).

                init(_) -> already_done,ok.

            "#]],
        )
    }

    #[test]
    fn optional_callbacks_assist() {
        check_assist(
            implement_behaviour,
            "Create optional callbacks for 'my_behaviour'",
            r#"
            //- /src/main.erl
            -module(main).
            -behaviour(my_b~ehaviour).

            init(_) -> already_done,ok.

            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback init(Args :: term()) -> ok.
            -callback another() -> ok.
            -callback optional() -> ok.
            -optional_callbacks([
                init/1, optional/0
            ]).
             "#,
            expect![[r#"
                -module(main).
                -behaviour(my_behaviour).

                %% Optional callbacks for `my_behaviour`
                -export([init/1, optional/0]).

                optional() ->
                    erlang:error(not_implemented).

                init(_) -> already_done,ok.

            "#]],
        )
    }

    #[test]
    fn existing_fun_missing_export() {
        check_assist(
            implement_behaviour,
            "Create callbacks for 'my_behaviour'",
            r#"
            //- /src/main.erl
            -module(main).
            -behaviour(my_b~ehaviour).

            init(_) -> already_done,ok.

            //- /src/my_behaviour.erl
            -module(my_behaviour).
            -callback init(Args :: term()) -> ok.
            -callback another() -> ok.
             "#,
            expect![[r#"
                -module(main).
                -behaviour(my_behaviour).

                %% Callbacks for `my_behaviour`
                -export([init/1, another/0]).

                another() ->
                    erlang:error(not_implemented).

                init(_) -> already_done,ok.

            "#]],
        )
    }
}
