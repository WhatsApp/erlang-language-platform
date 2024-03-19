/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::find_best_token;
use elp_ide_db::ReferenceCategory;
use elp_ide_db::SearchScope;
use elp_ide_db::SymbolClass;
use elp_ide_db::SymbolDefinition;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::NodeOrToken;
use elp_syntax::TextRange;
use hir::Semantic;

use crate::navigation_target::ToNav;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct HighlightedRange {
    pub range: TextRange,
    pub category: Option<ReferenceCategory>,
}

// Feature: Highlight Related
//
// Highlights constructs related to the thing under the cursor.
//
pub(crate) fn highlight_related(
    sema: &Semantic,
    position: FilePosition,
) -> Option<Vec<HighlightedRange>> {
    let _p = profile::span("highlight_related");
    find_local_refs(sema, position)
}

/// This function is based on `references::find_all_refs()` but limits
/// its search to the current file, does not request direct only, and
/// returns highlight results
fn find_local_refs(sema: &Semantic<'_>, position: FilePosition) -> Option<Vec<HighlightedRange>> {
    let _p = profile::span("find_local_refs");
    let search = move |def: SymbolDefinition| -> Vec<HighlightedRange> {
        let declaration = def.to_nav(sema.db);
        let (ref_category, decl_category) = match def {
            SymbolDefinition::Var(_) => (
                Some(ReferenceCategory::Read),
                Some(ReferenceCategory::Write),
            ),
            _ => (None, None),
        };
        let file_scope = SearchScope::single_file(position.file_id, None);
        let usages = def.usages(sema).set_scope(&file_scope).all();

        let mut references: Vec<_> = usages
            .into_iter()
            .flat_map(|(file_id, refs)| {
                if file_id == position.file_id {
                    refs.into_iter()
                        .map(|name| HighlightedRange {
                            range: name.syntax().text_range(),
                            category: ref_category,
                        })
                        .collect::<Vec<_>>()
                } else {
                    Vec::default()
                }
            })
            .collect();
        if declaration.file_id == position.file_id {
            references.push(HighlightedRange {
                range: declaration.focus_range.unwrap_or(declaration.full_range),
                category: decl_category,
            });
        };

        references
    };

    let token = find_best_token(sema, position)?;
    match SymbolClass::classify(sema, token.clone()) {
        Some(SymbolClass::Definition(def)) => Some(search(def)),
        Some(SymbolClass::Reference { refs, typ: _ }) => {
            Some(refs.iter().flat_map(search).collect())
        }
        None => {
            let atom = ast::Atom::cast(token.value.parent()?)?;
            let escaped_name = atom.text();
            // Simply find all matching atoms in the file, using normalised names.
            // Possibly limit it to +- X lines, since it is for display in a viewport?
            // Possibly make use of the search.rs functionality? But that needs a SymbolDefinition
            let source = sema.parse(position.file_id);
            let references: Vec<_> = source
                .value
                .syntax()
                .descendants_with_tokens()
                .filter_map(|n| match n {
                    NodeOrToken::Node(n) => {
                        let atom = ast::Atom::cast(n)?;
                        if atom.text() == escaped_name {
                            Some(HighlightedRange {
                                range: atom.syntax().text_range(),
                                category: None,
                            })
                        } else {
                            None
                        }
                    }
                    NodeOrToken::Token(_) => None,
                })
                .collect();

            Some(references)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture;

    #[track_caller]
    fn check(fixture_str: &str) {
        let (analysis, pos, _diagnostics_enabled, _guard, annotations) =
            fixture::annotations(fixture_str);
        fixture::check_no_parse_errors(&analysis, pos.file_id);

        let hls = analysis.highlight_related(pos).unwrap().unwrap_or_default();

        let mut expected = annotations
            .into_iter()
            .map(|(r, access)| (r.range, (!access.is_empty()).then_some(access)))
            .collect::<Vec<_>>();

        let mut actual = hls
            .into_iter()
            .map(|hl| {
                (
                    hl.range,
                    hl.category.map(|it| {
                        match it {
                            ReferenceCategory::Read => "read",
                            ReferenceCategory::Write => "write",
                        }
                        .to_string()
                    }),
                )
            })
            .collect::<Vec<_>>();
        actual.sort_by_key(|(range, _)| range.start());
        expected.sort_by_key(|(range, _)| range.start());

        assert_eq!(expected, actual);
    }

    // Note: Test names are based on those in erlang_ls
    #[test]
    fn application_local() {
        check(
            r#"
            -module(main).
            -export([ function_a/0, function_b/0, function_g/1 ]).
            %%                      ^^^^^^^^^^

            -define(MACRO_A, macro_a).
            -define(MACRO_A(X), erlang:display(X)).

            function_a() ->
              fun~ction_b(),
           %% ^^^^^^^^^^
              #record_a{}.

            function_b() ->
         %% ^^^^^^^^^^
              ?MACRO_A.

            function_g(X) ->
              F = fun function_b/0,
            %%        ^^^^^^^^^^
              G = { },
              {F, G}.

"#,
        );
    }

    #[test]
    fn application_remote_module() {
        check(
            r#"
          //- /src/main.erl
            -module(main).
            -export([ function_c/0, function_g/1 ]).

            function_c() ->
              code_n~avigation_extra:do(test),
          %%  ^^^^^^^^^^^^^^^^^^^^^
              A = #record_a{ field_a = a },
              _X = A#record_a.field_a, _Y = A#record_a.field_a,
              length([1, 2, 3]).

            function_g(X) ->
              F = fun function_b/0,
              G = {fun code_navigation_extra:do/1 },
          %%           ^^^^^^^^^^^^^^^^^^^^^
              {F, G}.


          //- /src/code_navigation_extra.erl
            -module(code_navigation_extra).
            -export([do/1]).
            do(X) -> ok.
"#,
        );
    }

    #[test]
    fn application_remote_function() {
        check(
            r#"
          //- /src/main.erl
            -module(main).
            -export([ function_c/0, function_g/1 ]).

            function_c() ->
              code_navigation_extra:d~o(test),
          %%                        ^^
              A = #record_a{ field_a = a },
              _X = A#record_a.field_a, _Y = A#record_a.field_a,
              length([1, 2, 3]).

            function_g(X) ->
              F = fun function_b/0,
              G = {fun code_navigation_extra:do/1 },
          %%                                 ^^
              {F, G}.


          //- /src/code_navigation_extra.erl
            -module(code_navigation_extra).
            -export([do/1]).
            do(X) -> ok.
"#,
        );
    }

    #[test]
    fn application_imported() {
        check(
            r#"
          //- /src/main.erl
            -module(main).
            -export([ function_c/0, function_g/1 ]).
            -import(lists, [length/1]).
          %%                ^^^^^^

            function_c() ->
              code_navigation_extra:do(test),
              A = #record_a{ field_a = a },
              _X = A#record_a.field_a, _Y = A#record_a.field_a,
              len~gth([1, 2, 3]).
          %%  ^^^^^^

            function_g(X) ->
              F = fun function_b/0,
              G = {fun code_navigation_extra:do/1 },
              {F, G}.


          //- /opt/lib/stdlib-3.17/src/lists.erl otp_app:/opt/lib/stdlib-3.17
            -module(lists).
            -export([length/1]).
            length(X) -> ok.
"#,
        );
    }

    #[test]
    fn function_definition() {
        check(
            r#"
            -module(main).
            -export([ function_a/0, function_b/0, function_g/1 ]).
            %%                      ^^^^^^^^^^

            -define(MACRO_A, macro_a).
            -define(MACRO_A(X), erlang:display(X)).

            function_a() ->
              function_b(),
           %% ^^^^^^^^^^
              #record_a{}.

            funct~ion_b() ->
         %% ^^^^^^^^^^
              ?MACRO_A.

            function_g(X) ->
              F = fun function_b/0,
            %%        ^^^^^^^^^^
              G = { },
              {F, G}.

"#,
        );
    }

    #[test]
    fn fun_local() {
        check(
            r#"
            -module(main).
            -export([ function_a/0, function_b/0, function_g/1 ]).
            %%                      ^^^^^^^^^^

            -define(MACRO_A, macro_a).
            -define(MACRO_A(X), erlang:display(X)).

            function_a() ->
              function_b(),
           %% ^^^^^^^^^^
              #record_a{}.

            function_b() ->
         %% ^^^^^^^^^^
              ?MACRO_A.

            function_g(X) ->
              F = fun func~tion_b/0,
            %%        ^^^^^^^^^^
              G = { },
              {F, G}.

"#,
        );
    }

    #[test]
    fn fun_remote_module() {
        check(
            r#"
          //- /src/main.erl
            -module(main).
            -export([ function_c/0, function_g/1 ]).

            function_c() ->
              code_navigation_extra:do(test),
          %%  ^^^^^^^^^^^^^^^^^^^^^
              A = #record_a{ field_a = a },
              _X = A#record_a.field_a, _Y = A#record_a.field_a,
              length([1, 2, 3]).

            function_g(X) ->
              F = fun function_b/0,
              G = {fun code_navig~ation_extra:do/1 },
          %%           ^^^^^^^^^^^^^^^^^^^^^
              {F, G}.


          //- /src/code_navigation_extra.erl
            -module(code_navigation_extra).
            -export([do/1]).
            do(X) -> ok.
"#,
        );
    }

    #[test]
    fn fun_remote_function() {
        check(
            r#"
          //- /src/main.erl
            -module(main).
            -export([ function_c/0, function_g/1 ]).

            function_c() ->
              code_navigation_extra:do(test),
          %%                        ^^
              A = #record_a{ field_a = a },
              _X = A#record_a.field_a, _Y = A#record_a.field_a,
              length([1, 2, 3]).

            function_g(X) ->
              F = fun function_b/0,
              G = {fun code_navigation_extra:d~o/1 },
          %%                                 ^^
              {F, G}.


          //- /src/code_navigation_extra.erl
            -module(code_navigation_extra).
            -export([do/1]).
            do(X) -> ok.
"#,
        );
    }

    #[test]
    fn atom() {
        check(
            r#"
            -module(main).
            -export([ function_a/0, function_b/0, function_g/1 ]).

            -record(record_a, {field_a, field_b, 'Field C'}).
           %%                  ^^^^^^^

            -define(MACRO_A, macro_a).
            -define(MACRO_A(X), erlang:display(X)).

            function_a() ->
              function_b(),
              #record_a{}.

            function_c() ->
              code_navigation_extra:do(test),
              A = #record_a{ field_a = a },
           %%                ^^^^^^^
              _X = A#record_a.field_a, _Y = A#record_a.field_a,
           %%                 ^^^^^^^
           %%                                          ^^^^^^^
              length([1, 2, 3]).


            function_b() ->
              ?MACRO_A.

            function_g(X) ->
              F = fun function_b/0,
              G = { },
              {F, G}.

            %% atom highlighting and completion includes record fields
            function_o() ->
              {fie~ld_a, incl}.
           %%  ^^^^^^^

            %% [#1052] ?MODULE macro as record name
            -record(?MODULE, {field_a, field_b}).
           %%                 ^^^^^^^

            -spec function_q() -> {#?MODULE{field_a :: integer()}, any()}.
           %%                               ^^^^^^^
            function_q() ->
              X = #?MODULE{},
              {X#?MODULE{field_a = 42}, X#?MODULE.field_a}.
           %%            ^^^^^^^
           %%                                     ^^^^^^^


"#,
        );
    }

    #[test]
    fn quoted_atom_1() {
        check(
            r#"
            -module(main).
            -export([ 'PascalCaseFunction'/1 ]).
            %%        ^^^^^^^^^^^^^^^^^^^^

            %% quoted atoms
            -spec 'PascalCaseFunction'(T) -> 'Code.Navigation.Elixirish':'Type'(T).
            %%    ^^^^^^^^^^^^^^^^^^^^
            'PascalC~aseFunction'(R) ->
         %% ^^^^^^^^^^^^^^^^^^^^
              _ = R#record_a.'Field C',
              F = fun 'Code.Navigation.Elixirish':do/1,
              F('Atom with whitespaces, "double quotes" and even some \'single quotes\'').

"#,
        );
    }

    #[test]
    fn quoted_atom_2() {
        check(
            r#"
            -module(main).
            -export([ 'PascalCaseFunction'/1 ]).

            %% quoted atoms
            -spec 'PascalCaseFunction'(T) -> 'Code.Navigation.Elixirish':'Type'(T).
            'PascalCaseFunction'(R) ->
              _ = R#record_a.'Field C',
              F = fun 'Code.Navigation.Elixirish':do/1,
              F('Atom with whi~tespaces, "double quotes" and even some \'single quotes\'').
             %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

"#,
        );
    }

    #[test]
    fn quoted_atom_3() {
        check(
            r#"
            -module(main).
            -export([ 'PascalCaseFunction'/1 ]).

            -record(record_a, {field_a, field_b, 'Field C'}).
             %%                                  ^^^^^^^^^

            %% quoted atoms
            -spec 'PascalCaseFunction'(T) -> 'Code.Navigation.Elixirish':'Type'(T).
            'PascalCaseFunction'(R) ->
              _ = R#record_a.'Fie~ld C',
             %%              ^^^^^^^^^
              F = fun 'Code.Navigation.Elixirish':do/1,
              F('Atom with whitespaces, "double quotes" and even some \'single quotes\'').

"#,
        );
    }

    #[test]
    fn quoted_atom_4() {
        check(
            r#"
            -module(main).
            -export([ 'PascalCaseFunction'/1 ]).

            -record(record_a, {field_a, field_b, 'Field C'}).

            %% quoted atoms
            -spec 'PascalCaseFunction'(T) -> 'Code.Navigation.Elixirish':'Type'(T).
             %%                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
            'PascalCaseFunction'(R) ->
              _ = R#record_a.'Field C',
              F = fun 'Code.Naviga~tion.Elixirish':do/1,
             %%       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
              F('Atom with whitespaces, "double quotes" and even some \'single quotes\'').

"#,
        );
    }

    #[test]
    fn quoted_atom_5() {
        check(
            r#"
            -module(main).
            -export([ 'PascalCaseFunction'/1 ]).

            -record(record_a, {field_a, field_b, 'Field C'}).

            %% quoted atoms
            -spec 'PascalCaseFunction'(T) -> 'Code.Na~vigation.Elixirish':'Type'(T).
             %%                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
            'PascalCaseFunction'(R) ->
              _ = R#record_a.'Field C',
              F = fun 'Code.Navigation.Elixirish':do/1,
             %%       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
              F('Atom with whitespaces, "double quotes" and even some \'single quotes\'').

"#,
        );
    }

    #[test]
    fn quoted_atom_6() {
        check(
            r#"
            -module(main).
            -export([ 'PascalCaseFunction'/1 ]).

            -record(record_a, {field_a, field_b, 'Field C'}).

            %% quoted atoms
            -spec 'PascalCaseFunction'(T) -> 'Code.Na~vigation.Elixirish':'Type'(T).
             %%                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
            'PascalCaseFunction'(R) ->
              _ = R#record_a.'Field C',
              F = fun 'Code.Navigation.Elixirish':do/1,
             %%       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
              F('Atom with whitespaces, "double quotes" and even some \'single quotes\'').

"#,
        );
    }

    #[test]
    fn record_expr() {
        check(
            r#"
            -module(main).
            -export([ 'PascalCaseFunction'/1 ]).

            -record(record_a, {field_a, field_b, 'Field C'}).
            %%      ^^^^^^^^

            function_a() ->
              function_b(),
              #rec~ord_a{}.
            %% ^^^^^^^^

            function_c() ->
              code_navigation_extra:do(test),
              A = #record_a{ field_a = a },
            %%     ^^^^^^^^
              _X = A#record_a.field_a, _Y = A#record_a.field_a,
            %%       ^^^^^^^^
            %%                                ^^^^^^^^
              length([1, 2, 3]).

            %% quoted atoms
            -spec 'PascalCaseFunction'(T) -> 'Code.Navigation.Elixirish':'Type'(T).
            'PascalCaseFunction'(R) ->
              _ = R#record_a.'Field C',
            %%      ^^^^^^^^
              F = fun 'Code.Navigation.Elixirish':do/1,
              F('Atom with whitespaces, "double quotes" and even some \'single quotes\'').

"#,
        );
    }

    #[test]
    fn record_access() {
        check(
            r#"
            -module(main).
            -export([ 'PascalCaseFunction'/1 ]).

            -record(record_a, {field_a, field_b, 'Field C'}).
            %%      ^^^^^^^^

            function_a() ->
              function_b(),
              #record_a{}.
            %% ^^^^^^^^

            function_c() ->
              code_navigation_extra:do(test),
              A = #record_a{ field_a = a },
            %%     ^^^^^^^^
              _X = A#re~cord_a.field_a, _Y = A#record_a.field_a,
            %%       ^^^^^^^^
            %%                                ^^^^^^^^
              length([1, 2, 3]).

            %% quoted atoms
            -spec 'PascalCaseFunction'(T) -> 'Code.Navigation.Elixirish':'Type'(T).
            'PascalCaseFunction'(R) ->
              _ = R#record_a.'Field C',
            %%      ^^^^^^^^
              F = fun 'Code.Navigation.Elixirish':do/1,
              F('Atom with whitespaces, "double quotes" and even some \'single quotes\'').

"#,
        );
    }

    #[test]
    fn record_def() {
        check(
            r#"
            -module(main).
            -export([ 'PascalCaseFunction'/1 ]).

            -record(rec~ord_a, {field_a, field_b, 'Field C'}).
            %%      ^^^^^^^^

            function_a() ->
              function_b(),
              #record_a{}.
            %% ^^^^^^^^

            function_c() ->
              code_navigation_extra:do(test),
              A = #record_a{ field_a = a },
            %%     ^^^^^^^^
              _X = A#record_a.field_a, _Y = A#record_a.field_a,
            %%       ^^^^^^^^
            %%                                ^^^^^^^^
              length([1, 2, 3]).

            %% quoted atoms
            -spec 'PascalCaseFunction'(T) -> 'Code.Navigation.Elixirish':'Type'(T).
            'PascalCaseFunction'(R) ->
              _ = R#record_a.'Field C',
            %%      ^^^^^^^^
              F = fun 'Code.Navigation.Elixirish':do/1,
              F('Atom with whitespaces, "double quotes" and even some \'single quotes\'').

"#,
        );
    }

    #[test]
    fn record_field() {
        check(
            r#"
            -module(main).
            -export([ 'PascalCaseFunction'/1 ]).

            -record(record_a, {fie~ld_a, field_b, 'Field C'}).
            %%                 ^^^^^^^

            function_a() ->
              function_b(),
              #record_a{}.

            function_c() ->
              code_navigation_extra:do(test),
              A = #record_a{ field_a = a },
            %%               ^^^^^^^
              _X = A#record_a.field_a, _Y = A#record_a.field_a,
            %%                ^^^^^^^
            %%                                         ^^^^^^^
              length([1, 2, 3]).

            %% quoted atoms
            -spec 'PascalCaseFunction'(T) -> 'Code.Navigation.Elixirish':'Type'(T).
            'PascalCaseFunction'(R) ->
              _ = R#record_a.'Field C',
              F = fun 'Code.Navigation.Elixirish':do/1,
              F('Atom with whitespaces, "double quotes" and even some \'single quotes\'').

"#,
        );
    }

    #[test]
    fn export() {
        // Not duplicating this functionality from erlang_ls, it does not make sense.
        check(
            r#"
            -module(main).
            -expo~rt([ 'PascalCaseFunction'/1 ]).

            %% quoted atoms
            -spec 'PascalCaseFunction'(T) -> 'Code.Navigation.Elixirish':'Type'(T).
            'PascalCaseFunction'(R) ->
              _ = R#record_a.'Field C',
              F = fun 'Code.Navigation.Elixirish':do/1,
              F('Atom with whitespaces, "double quotes" and even some \'single quotes\'').

"#,
        );
    }

    #[test]
    fn export_entry() {
        check(
            r#"
            -module(main).
            -export([ function_a/0, func~tion_b/0, function_g/1 ]).
            %%                      ^^^^^^^^^^

            -define(MACRO_A, macro_a).
            -define(MACRO_A(X), erlang:display(X)).

            function_a() ->
              function_b(),
           %% ^^^^^^^^^^
              #record_a{}.

            function_b() ->
         %% ^^^^^^^^^^
              ?MACRO_A.

            function_g(X) ->
              F = fun function_b/0,
            %%        ^^^^^^^^^^
              G = { },
              {F, G}.

"#,
        );
    }

    #[test]
    fn export_type_entry() {
        check(
            r#"
            -module(main).

            -type type_a() :: atom().
            %%    ^^^^^^^^

            -export_type([ typ~e_a/0 ]).
            %%             ^^^^^^

"#,
        );
    }

    #[test]
    fn import() {
        // Not duplicating this functionality from erlang_ls, it does not make sense.
        // And is broken in erlang_ls
        check(
            r#"

          //- /src/main.erl
            -module(main).
            -imp~ort(lists, [length/1]).

            foo() -> length([1]).

          //- /opt/lib/stdlib-3.17/src/lists.erl otp_app:/opt/lib/stdlib-3.17
            -module(lists).
            -export([length/1]).
            length(X) -> ok.
"#,
        );
    }

    #[test]
    fn import_entry() {
        check(
            r#"

          //- /src/main.erl
            -module(main).
            -import(lists, [le~ngth/1]).
            %%              ^^^^^^

            foo() -> length([1]).
            %%       ^^^^^^

          //- /opt/lib/stdlib-3.17/src/lists.erl otp_app:/opt/lib/stdlib-3.17
            -module(lists).
            -export([length/1]).
            length(X) -> ok.
"#,
        );
    }

    #[test]
    fn type_def() {
        check(
            r#"
          //- /src/main.erl
            -module(main).
            -type ty~pe_a() :: any().
            %%    ^^^^^^^^

            -spec function_h() -> type_a() | undefined_type_a() | file:fd().
            %%                    ^^^^^^
            function_h() ->
              function_i().

"#,
        );
    }

    #[test]
    fn type_application_module() {
        check(
            r#"
          //- /src/main.erl
            -module(main).
            -type type_a() :: any().

            -spec function_h() -> type_a() | undefined_type_a() | fi~le:fd().
            %%                                                    ^^^^
            function_h() ->
              function_i().

"#,
        );
    }

    #[test]
    fn type_application_type() {
        check(
            r#"
          //- /src/main.erl
            -module(main).
            -type type_a() :: any().

            -spec function_h() -> type_a() | undefined_type_a() | file:f~d().
            %%                                                         ^^
            function_h() ->
              function_i().

"#,
        );
    }

    #[test]
    fn opaque() {
        check(
            r#"
          //- /src/main.erl
            -module(main).
            -opaque opaque_t~ype_a() :: atom().
            %%      ^^^^^^^^^^^^^^^

            -export_type([ opaque_type_a/0 ]).
            %%             ^^^^^^^^^^^^^

            -type user_type_a() :: type_a() | opaque_type_a().
            %%                                ^^^^^^^^^^^^^

"#,
        );
    }

    #[test]
    fn macro_define() {
        check(
            r#"
          //- /src/main.erl
            -module(main).
            -define(MA~CRO_A, macro_a).
            %%      ^^^^^^^
            -define(MACRO_A(X), erlang:display(X)).

            function_b() ->
              ?MACRO_A.
            %% ^^^^^^^

            function_d() ->
              ?MACRO_A(d).

            %% [#333] Record field accessors assumed to be atoms
            function_k() ->
              X#included_record_a.?MACRO_A,
            %%                     ^^^^^^^
              <<"foo:">>.
"#,
        );
    }

    #[test]
    fn macro_use() {
        check(
            r#"
          //- /src/main.erl
            -module(main).
            -define(MACRO_A, macro_a).
            %%      ^^^^^^^
            -define(MACRO_A(X), erlang:display(X)).

            function_b() ->
              ?MAC~RO_A.
            %% ^^^^^^^

            function_d() ->
              ?MACRO_A(d).

            %% [#333] Record field accessors assumed to be atoms
            function_k() ->
              X#included_record_a.?MACRO_A,
            %%                     ^^^^^^^
              <<"foo:">>.
"#,
        );
    }

    #[test]
    fn spec() {
        // Not duplicating this functionality from erlang_ls, it does not make sense.
        // So we duplicate just the type, not the whole attribute
        check(
            r#"

          //- /src/main.erl
            -module(main).
            -spec func~tion_h() -> type_a() | undefined_type_a() | file:fd().
            %%    ^^^^^^^^^^

"#,
        );
    }

    #[test]
    fn behaviour() {
        // Not duplicating this functionality from erlang_ls, it does not make sense.
        // So we duplicate just the name, not the whole attribute
        check(
            r#"

          //- /src/main.erl
            -module(main).
            -behaviour(behav~iour_a).
            %%         ^^^^^^^^^^^

"#,
        );
    }

    #[test]
    fn callback() {
        // Not duplicating this functionality from erlang_ls, it does not make sense.
        // So we duplicate just the name, not the whole attribute
        check(
            r#"

          //- /src/main.erl
            -module(main).

            -callback rena~me_me(any()) -> ok.
            %%        ^^^^^^^^^

"#,
        );
    }

    #[test]
    fn local_variables_1() {
        check(
            r#"
          //- /src/main.erl
            -module(main).

            foo(X~X,YY) ->
           %%   ^^write
              XX + YY.
           %% ^^read

"#,
        );
    }

    #[test]
    fn argument_used_in_macro() {
        check(
            r#"
          //- /src/main.erl
            -module(main).

            -define(a_macro(Expr), ok).
            get_aclink_state_test_helper(Ar~gs) ->
            %%                           ^^^^write
                ?a_macro(Args).
            %%           ^^^^read

"#,
        );
    }
}
