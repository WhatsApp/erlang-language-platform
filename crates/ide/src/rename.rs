/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Renaming functionality.

use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::rename::format_err;
use elp_ide_db::rename::rename_error;
use elp_ide_db::rename::RenameError;
use elp_ide_db::rename::RenameResult;
use elp_ide_db::rename::SafetyChecks;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::ReferenceClass;
use elp_ide_db::RootDatabase;
use elp_ide_db::SymbolClass;
use elp_ide_db::SymbolDefinition;
use elp_syntax::algo;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::SyntaxNode;
use hir::InFile;
use hir::Semantic;

// Feature: Rename
//
// Renames the item below the cursor and all of its references
//
// |===
// | Editor  | Shortcut
//
// | VS Code | kbd:[F2]
// |===
//
pub(crate) fn rename(
    db: &RootDatabase,
    position: FilePosition,
    new_name: &str,
) -> RenameResult<SourceChange> {
    let sema = Semantic::new(db);
    let file_id = position.file_id;
    let source_file = sema.parse(file_id);
    let syntax = source_file.value.syntax();
    let new_name = new_name.trim();

    let defs = find_definitions(&sema, syntax, position)?;

    let ops: RenameResult<Vec<SourceChange>> = defs
        .iter()
        .map(|def| def.rename(&sema, &|_| new_name.to_string(), SafetyChecks::Yes))
        .collect();

    ops?.into_iter()
        .reduce(|acc, elem| acc.merge(elem))
        .ok_or_else(|| format_err!("No references found at position"))
}

fn find_definitions(
    sema: &Semantic,
    syntax: &SyntaxNode,
    position: FilePosition,
) -> RenameResult<Vec<SymbolDefinition>> {
    let symbols =
        if let Some(name_like) = algo::find_node_at_offset::<ast::Name>(syntax, position.offset) {
            let res = match &name_like {
                ast::Name::Var(var) => {
                    let def = sema.to_def::<ast::Var>(InFile {
                        file_id: position.file_id,
                        value: var,
                    });
                    if let Some(defs) = def {
                        match defs {
                            hir::DefinitionOrReference::Definition(def) => {
                                Some(Ok(vec![SymbolDefinition::Var(def)]))
                            }
                            hir::DefinitionOrReference::Reference(defs) => Some(Ok(defs
                                .into_iter()
                                .map(SymbolDefinition::Var)
                                .collect::<Vec<_>>())),
                        }
                    } else {
                        None
                    }
                }
                ast::Name::Atom(atom) => {
                    if let Some(token) = atom.syntax().first_token() {
                        let location = InFile {
                            file_id: position.file_id,
                            value: token.clone(),
                        };
                        match SymbolClass::classify(sema, location) {
                            Some(SymbolClass::Definition(def)) => Some(Ok(vec![def])),
                            Some(SymbolClass::Reference { refs, typ: _ }) => match refs {
                                ReferenceClass::Definition(def) => Some(Ok(vec![def])),
                                ReferenceClass::MultiVar(defs) => Some(Ok(defs
                                    .into_iter()
                                    .map(SymbolDefinition::Var)
                                    .collect::<Vec<_>>())),
                                ReferenceClass::MultiMacro(_) => None,
                            },
                            None => None,
                        }
                    } else {
                        None
                    }
                }
                ast::Name::MacroCallExpr(_) => None,
            };
            res
        } else {
            rename_error!("No references found at position")
        };

    if let Some(res) = symbols {
        res
    } else {
        rename_error!("No references found at position")
    }
}

#[cfg(test)]
mod tests {
    use elp_ide_db::elp_base_db::assert_eq_text;
    use elp_ide_db::elp_base_db::test_fixture::trim_indent;
    use text_edit::TextEdit;

    use crate::fixture;

    #[track_caller]
    fn check(new_name: &str, fixture_before: &str, fixture_after_str: &str) {
        let fixture_after_str = &trim_indent(fixture_after_str);
        let analysis_after = fixture::multi_file(fixture_after_str);

        let (analysis, position) = fixture::position(fixture_before);
        let rename_result = analysis
            .rename(position, new_name)
            .unwrap_or_else(|err| panic!("Rename to '{}' was cancelled: {}", new_name, err));
        match rename_result {
            Ok(source_change) => {
                for edit in source_change.source_file_edits {
                    let mut text_edit_builder = TextEdit::builder();
                    let file_id = edit.0;
                    for indel in edit.1.into_iter() {
                        text_edit_builder.replace(indel.delete, indel.insert);
                    }
                    let mut result = analysis.file_text(file_id).unwrap().to_string();
                    let edit = text_edit_builder.finish();
                    edit.apply(&mut result);
                    let expected = analysis_after.file_text(file_id).unwrap().to_string();
                    assert_eq_text!(&*expected, &*result);
                }
            }
            Err(err) => {
                if fixture_after_str.starts_with("error:") {
                    let error_message = fixture_after_str
                        .chars()
                        .into_iter()
                        .skip("error:".len())
                        .collect::<String>();
                    assert_eq!(error_message.trim(), err.to_string());
                } else {
                    panic!("Rename to '{}' failed unexpectedly: {}", new_name, err)
                }
            }
        };
    }

    #[test]
    fn test_rename_var_1() {
        check("Y", r#"main() -> I~ = 1."#, r#"main() -> Y = 1."#);
    }

    #[test]
    fn test_rename_var_2() {
        check(
            "Y",
            r#"main() ->
                   I~ = 1,
                   I + 2."#,
            r#"main() ->
                   Y = 1,
                   Y + 2."#,
        );
    }

    #[test]
    fn test_rename_var_3() {
        check(
            "Y",
            r#"main(X) ->
                   case X of
                     1 -> Z = 2;
                     2 -> Z = 3
                   end,
                   ~Z + 2."#,
            r#"main(X) ->
                   case X of
                     1 -> Y = 2;
                     2 -> Y = 3
                   end,
                   Y + 2."#,
        );
    }

    #[test]
    fn test_rename_var_4() {
        check(
            "Y",
            r#"testz() ->
                   case rand:uniform(2) of
                       1 ->
                           Z = 1;
                       2 ->
                           ~Z = 2;
                       Z ->
                           ok
                   end,
                   Z."#,
            r#"testz() ->
                   case rand:uniform(2) of
                       1 ->
                           Y = 1;
                       2 ->
                           Y = 2;
                       Y ->
                           ok
                   end,
                   Y."#,
        );
    }

    #[test]
    fn test_rename_var_5() {
        check(
            "YY",
            r#"main(_) ->
                   Y = 5,
                   AssertIs5 = fun (X) ->
                       ~Y = X,
                       erlang:display(Y)
                   end,
                   AssertIs5(2),
                   erlang:display(Y),
                   ok."#,
            r#"main(_) ->
                   YY = 5,
                   AssertIs5 = fun (X) ->
                       YY = X,
                       erlang:display(YY)
                   end,
                   AssertIs5(2),
                   erlang:display(YY),
                   ok."#,
        );
    }

    #[test]
    fn test_rename_var_6() {
        check(
            "ZZ",
            r#"main(_) ->
                   Z = 2,
                   case 3 of
                       3 -> ~Z = 2
                   end."#,
            r#"main(_) ->
                   ZZ = 2,
                   case 3 of
                       3 -> ZZ = 2
                   end."#,
        );
    }

    #[test]
    fn test_rename_var_7() {
        check(
            "Y",
            r#"main() ->
                   I = 1,
                   I~ + 2."#,
            r#"main() ->
                   Y = 1,
                   Y + 2."#,
        );
    }

    #[test]
    fn test_rename_var_name_clash_1() {
        check(
            "Y",
            r#"main(Y) ->
                   I~ = 1,
                   I + Y."#,
            r#"error: Name 'Y' already in scope"#,
        );
    }

    #[test]
    fn test_rename_var_but_not_shadowed() {
        check(
            "Z",
            r#"triples( Self, X, Y, none )->
                   [ Result || Result = { X~, Y, _} <- Self ]."#,
            r#"triples( Self, X, Y, none )->
                   [ Result || Result = { Z, Y, _} <- Self ]."#,
        );
    }

    // -----------------------------------------------------------------

    #[test]
    fn test_rename_local_function_no_calls() {
        check(
            "new_fun",
            r#"trip~les( Self, X, Y, none )->
                   [ Result || Result = { X, Y, _} <- Self ]."#,
            r#"new_fun( Self, X, Y, none )->
                   [ Result || Result = { X, Y, _} <- Self ]."#,
        );
    }

    #[test]
    fn test_rename_local_function_with_calls_1() {
        check(
            "new_fun",
            r#"fo~o() -> ok.
               bar() -> foo()."#,
            r#"new_fun() -> ok.
               bar() -> new_fun()."#,
        );
    }

    #[test]
    fn test_rename_local_function_with_calls_2() {
        check(
            "new_fun",
            r#"fo~o() -> ok.
               bar() -> baz(),foo()."#,
            r#"new_fun() -> ok.
               bar() -> baz(),new_fun()."#,
        );
    }

    #[test]
    fn test_rename_local_function_with_calls_3() {
        check(
            "new_fun",
            r#"fo~o(0) -> 0;
               foo(X) -> foo(X - 1).
               bar() -> foo(3)."#,
            r#"new_fun(0) -> 0;
               new_fun(X) -> new_fun(X - 1).
               bar() -> new_fun(3)."#,
        );
    }

    #[test]
    fn test_rename_local_function_with_calls_4() {
        check(
            "new_fun",
            r#"test1() ->
                   ok.
               foo() -> te~st1()."#,
            r#"new_fun() ->
                   ok.
               foo() -> new_fun()."#,
        );
    }

    #[test]
    fn test_rename_local_function_fails_name_clash_1() {
        check(
            "new_fun",
            r#"fo~o() -> ok.
               new_fun() -> ok."#,
            r#"error: Function 'new_fun/0' already in scope"#,
        );
    }

    #[test]
    fn test_rename_local_function_fails_name_clash_2() {
        check(
            "foo",
            r#"foo() -> ok.
               b~ar() -> ok."#,
            r#"error: Function 'foo/0' already in scope"#,
        );
    }

    #[test]
    fn test_rename_local_function_fails_name_clash_checks_arity() {
        check(
            "new_fun",
            r#"fo~o() -> ok.
               new_fun(X) -> ok."#,
            r#"new_fun() -> ok.
               new_fun(X) -> ok."#,
        );
    }

    #[test]
    fn test_rename_local_function_fails_name_clash_imported_function() {
        check(
            "new_fun",
            r#"-import(bar, [new_fun/0]).
               fo~o() -> ok."#,
            r#"error: Function 'new_fun/0' already in scope"#,
        );
    }

    #[test]
    fn test_rename_local_function_fails_name_clash_erlang_function() {
        check(
            "alias",
            r#"fo~o() -> ok."#,
            r#"error: Function 'alias/0' already in scope"#,
        );
    }

    #[test]
    fn test_rename_local_function_also_name_in_macro() {
        check(
            "new",
            r#"-define(FOO, foo).
               fo~o() -> ok.
               bar() -> ?FOO()"#,
            r#"-define(FOO, foo).
               new() -> ok.
               bar() -> ?FOO()"#,
        );
    }

    #[test]
    fn test_rename_local_var_trims_surrounding_spaces() {
        check("  Aaa  ", r#"foo() -> V~ar = 3."#, r#"foo() -> Aaa = 3."#);
    }

    #[test]
    fn test_rename_local_function_trims_surrounding_spaces() {
        check("  aaa  ", r#"fo~o() -> Var = 3."#, r#"aaa() -> Var = 3."#);
    }

    #[test]
    fn test_rename_local_var_fails_invalid_var_name() {
        check(
            "aaa",
            r#"foo() -> V~ar = 3."#,
            r#"error: Invalid new variable name: 'aaa'"#,
        );
    }

    #[test]
    fn test_rename_local_function_fails_invalid_function_name_1() {
        check(
            "Foo",
            r#"fo~o() -> ok."#,
            r#"error: Invalid new function name: 'Foo'"#,
        );
    }

    #[test]
    fn test_rename_local_function_fails_invalid_function_name_2() {
        check(
            "TT TTT",
            r#"fo~o() -> ok."#,
            r#"error: Invalid new function name: 'TT TTT'"#,
        );
    }

    #[test]
    fn test_rename_local_function_fails_invalid_function_name_3() {
        check(
            "TTT",
            r#"fo~o() -> ok."#,
            r#"error: Invalid new function name: 'TTT'"#,
        );
    }

    #[test]
    fn test_rename_var_d39578003_case_5() {
        check(
            "_G",
            r#"main(_) ->
                  fun F() ->
                      case rand:uniform(2) of
                          1 -> F();
                          _ -> ok
                      end,
                      {_, _} = catch ~F = 3
                  end()."#,
            r#"main(_) ->
                  fun _G() ->
                      case rand:uniform(2) of
                          1 -> _G();
                          _ -> ok
                      end,
                      {_, _} = catch _G = 3
                  end()."#,
        );
    }

    #[test]
    fn test_rename_in_spawn_1() {
        check(
            "new_name",
            r#"foo() ->
                 Pid = erlang:spawn(fun noop_~group_leader/0),
                 ok.

               noop_group_leader() -> ok."#,
            r#"foo() ->
                 Pid = erlang:spawn(fun new_name/0),
                 ok.

               new_name() -> ok."#,
        );
    }

    #[test]
    fn test_rename_in_spawn_2() {
        check(
            "new_name",
            r#"foo() ->
                 Pid = erlang:spawn(fun noop_group_leader/0),
                 ok.

               noop_group_~leader() -> ok."#,
            r#"foo() ->
                 Pid = erlang:spawn(fun new_name/0),
                 ok.

               new_name() -> ok."#,
        );
    }

    #[test]
    fn test_rename_in_apply_1() {
        check(
            "new_name",
            r#"
               //- /src/baz.erl
               -module(baz).
               foo() ->
                   apply(?MODULE, bar, []).

               b~ar() ->
                    ok."#,
            r#"
               -module(baz).
               foo() ->
                   apply(?MODULE, new_name, []).

               new_name() ->
                    ok."#,
        );
    }

    #[test]
    fn test_rename_in_apply_2() {
        check(
            "new_name",
            r#"
               //- /src/baz.erl
               -module(baz).
               foo() ->
                   apply(baz, b~ar, []).
               bar() ->
                    ok."#,
            r#"
               -module(baz).
               foo() ->
                   apply(baz, new_name, []).
               new_name() ->
                    ok."#,
        );
    }

    #[test]
    fn test_rename_in_apply_args_1() {
        check(
            "new_name",
            r#"
               //- /src/baz.erl
               -module(baz).
               foo() ->
                   apply(baz, b~ar, [1]).
               bar(X) ->
                    ok.
               bar() ->
                    ok."#,
            r#"
               -module(baz).
               foo() ->
                   apply(baz, new_name, [1]).
               new_name(X) ->
                    ok.
               bar() ->
                    ok."#,
        );
    }

    #[test]
    fn test_rename_in_apply_args_2() {
        check(
            "new_name",
            r#"
               //- /src/baz.erl
               -module(baz).
               foo(XS) ->
                   apply(baz, b~ar, [1|XS]).
               bar(X) ->
                    ok.
               bar() ->
                    ok."#,
            r#"error: No references found at position"#,
        );
    }

    #[test]
    fn test_rename_in_apply_3() {
        check(
            "new_name",
            r#"
               //- /src/baz.erl
               -module(baz).
               foo() ->
                   X = bar(),
                   apply(b~ar, []).
               bar() ->
                    ok."#,
            r#"
               -module(baz).
               foo() ->
                   X = new_name(),
                   apply(new_name, []).
               new_name() ->
                    ok."#,
        );
    }

    #[test]
    fn test_rename_in_apply_4() {
        check(
            "new_name",
            r#"
               //- /src/baz.erl
               -module(baz).
               foo() ->
                   erlang:apply(?MODULE, b~ar, []),
                   erlang:apply(bar, []).
               bar() ->
                    ok."#,
            r#"
               -module(baz).
               foo() ->
                   erlang:apply(?MODULE, new_name, []),
                   erlang:apply(new_name, []).
               new_name() ->
                    ok."#,
        );
    }

    #[test]
    fn test_rename_in_apply_5() {
        check(
            "new_name",
            r#"
               //- /src/baz.erl
               -module(baz).
               foo() ->
                   other_mod:apply(b~ar, []).
               bar() ->
                    ok."#,
            r#"error: No references found at position"#,
        );
    }

    #[test]
    fn test_rename_function_with_spec_1() {
        check(
            "new_name",
            r#"
               -spec foo() -> ok.
               fo~o() ->
                    ok."#,
            r#"
               -spec new_name() -> ok.
               new_name() ->
                    ok."#,
        );
    }

    #[test]
    fn test_rename_function_with_spec_2() {
        check(
            "new_name",
            r#"
               -spec f~oo() -> ok.
               foo() ->
                    ok."#,
            r#"
               -spec new_name() -> ok.
               new_name() ->
                    ok."#,
        );
    }

    #[test]
    fn test_rename_function_with_spec_3() {
        check(
            "new_name",
            r#"
               -spec f~oo(any()) -> ok.
               foo(1) -> ok;
               foo(_) -> oops."#,
            r#"
               -spec new_name(any()) -> ok.
               new_name(1) -> ok;
               new_name(_) -> oops."#,
        );
    }

    #[test]
    fn test_rename_underscore_1() {
        check(
            "NewName",
            r#"
               foo() ->
                    ~_ = foo(),
                    ok."#,
            r#"error: No references found at position"#,
        );
    }

    #[test]
    fn test_rename_underscore_2() {
        check(
            "NewName",
            r#"
               foo(X) ->
                    ~_ = foo(1),
                    _ = foo(2),
                    X."#,
            r#"error: No references found at position"#,
        );
    }

    #[test]
    fn test_rename_case_1() {
        check(
            "XX",
            r#"
               foo(X) ->
                 X2 = case X of
                   [X~0, X1] -> X0 + X1;
                   [] -> X0 = 1
                 end,
                 X0 + X2."#,
            r#"
               foo(X) ->
                 X2 = case X of
                   [XX, X1] -> XX + X1;
                   [] -> XX = 1
                 end,
                 XX + X2."#,
        );
    }

    #[test]
    fn test_rename_case_2() {
        check(
            "XX",
            r#"
              foo() ->
                X~0 = 42,
                X1 = X0 + 1,
                [ X0 || X0 <- [X1] ]."#,
            r#"
              foo() ->
                XX = 42,
                X1 = XX + 1,
                [ X0 || X0 <- [X1] ]."#,
        );
    }

    #[test]
    fn test_rename_case_3() {
        check(
            "XX",
            r#"
              foo() ->
                [ X1 || X1 <- [X~0 = 2] ],
                X1 = 3."#,
            r#"
              foo() ->
                [ X1 || X1 <- [XX = 2] ],
                X1 = 3."#,
        );
    }

    #[test]
    fn rename_export_function() {
        check(
            "new_name",
            r#"
               //- /src/baz.erl
               -module(baz).
               -export([foo/0]).
               foo() -> ok.
               bar() -> f~oo().

               //- /src/bar.erl
               -module(bar).
               another_fun() ->
                  baz:foo(),
                  ok.
            "#,
            r#"
               //- /src/baz.erl
               -module(baz).
               -export([new_name/0]).
               new_name() -> ok.
               bar() -> new_name().

               //- /src/bar.erl
               -module(bar).
               another_fun() ->
                  baz:new_name(),
                  ok.
             "#,
        );
    }

    #[test]
    fn rename_export_function_2() {
        check(
            "new_name",
            r#"
               //- /src/baz.erl
               -module(baz).
               -export([foo/0]).
               foo() -> ok.
               bar() -> f~oo().

               //- /src/bar.erl
               -module(bar).
               -import(baz, [foo/0]).
               another_fun() ->
                  foo(),
                  ok.
            "#,
            r#"
               //- /src/baz.erl
               -module(baz).
               -export([new_name/0]).
               new_name() -> ok.
               bar() -> new_name().

               //- /src/bar.erl
               -module(bar).
               -import(baz, [new_name/0]).
               another_fun() ->
                  new_name(),
                  ok.
             "#,
        );
    }

    #[test]
    fn rename_export_function_fails() {
        check(
            "new_name",
            r#"
               //- /src/baz.erl
               -module(baz).
               -export([foo/0]).
               foo() -> ok.
               bar() -> f~oo().

               //- /src/bar.erl
               -module(bar).
               -import(baz, [foo/0]).
               another_fun() ->
                  foo(),
                  ok.
               new_name() -> ok.
            "#,
            r#"error: Function 'new_name/0' already in scope in module 'bar'"#,
        );
    }

    #[test]
    fn rename_function_in_include() {
        // We do not have functions defined in our header files, but
        // confirm it does the right thing anyway
        check(
            "new_name",
            r#"
             //- /src/main.hrl
             %% main.hrl
             -spec bar() -> ok.
             bar() -> ok.

             //- /src/main.erl
             %% main.erl
             -include("main.hrl").
             baz() -> ba~r().

             //- /src/another.erl
             %% another.erl
             -include("main.hrl").

             foo() -> bar().

             //- /src/different_bar.erl
             %% different_bar.erl
             bar() -> different.

             should_not_match() -> bar().
             "#,
            r#"
             //- /src/main.hrl
             %% main.hrl
             -spec new_name() -> ok.
             new_name() -> ok.

             //- /src/main.erl
             %% main.erl
             -include("main.hrl").
             baz() -> new_name().

             //- /src/another.erl
             %% another.erl
             -include("main.hrl").

             foo() -> new_name().

             //- /src/different_bar.erl
             %% different_bar.erl
             bar() -> different.

             should_not_match() -> bar().
             "#,
        );
    }

    #[test]
    fn test_rename_in_macro_rhs_1() {
        check(
            "new_name",
            r#"
               -define(BAR(X), foo(X)).
               baz() -> ?BAR(3).
               fo~o(X) ->
                    X."#,
            r#"
               -define(BAR(X), new_name(X)).
               baz() -> ?BAR(3).
               new_name(X) ->
                    X."#,
        );
    }

    #[test]
    fn rename_with_macro() {
        check(
            "NewName",
            r#"
             //- /src/main.hrl
             %% main.hrl
             -define(config,test_server:lookup_config).

             //- /src/main.erl
             %% main.erl
             -include("main.hrl").
             start_apps(Config) ->
                 PrivDir = ?config(priv_dir, Config),
                 {ok, A~pps} = {ok, [foo]},
                 [{apps, Apps}].
             "#,
            r#"
             //- /src/main.hrl
             %% main.hrl
             -define(config,test_server:lookup_config).

             //- /src/main.erl
             %% main.erl
             -include("main.hrl").
             start_apps(Config) ->
                 PrivDir = ?config(priv_dir, Config),
                 {ok, NewName} = {ok, [foo]},
                 [{apps, NewName}].
             "#,
        );
    }

    // Document that at the moment there are a few corner cases
    // where we incorrectly rename type definitions within a macro.
    // See T157498333
    #[test]
    fn rename_function_with_macro_type() {
        check(
            "newFoo",
            r#"
            -module(main).
            -define(TY, foo()).
            -spec x(?TY) -> ok.
            x(_) -> foo().
            fo~o() -> ok.
             "#,
            r#"
            -module(main).
            -define(TY, newFoo()).
            -spec x(?TY) -> ok.
            x(_) -> newFoo().
            newFoo() -> ok.
             "#,
        );
    }
}
