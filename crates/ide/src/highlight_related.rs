/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::ReferenceCategory;
use elp_ide_db::ReferenceType;
use elp_ide_db::SearchScope;
use elp_ide_db::SymbolClass;
use elp_ide_db::SymbolDefinition;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::find_best_token;
use elp_syntax::AstNode;
use elp_syntax::NodeOrToken;
use elp_syntax::TextRange;
use elp_syntax::algo;
use elp_syntax::ast;
use hir::CallTarget;
use hir::Expr;
use hir::InFile;
use hir::Literal;
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
    let _p = tracing::info_span!("highlight_related").entered();
    if let Some(highlights) = find_format_string_highlights(sema, position) {
        return Some(highlights);
    }
    find_local_refs(sema, position)
}

/// This function is based on `references::find_all_refs()` but limits
/// its search to the current file, does not request direct only, and
/// returns highlight results
fn find_local_refs(sema: &Semantic<'_>, position: FilePosition) -> Option<Vec<HighlightedRange>> {
    let _p = tracing::info_span!("find_local_refs").entered();
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
        if declaration.file_id == position.file_id
            && let Some(highlight_ranges) = declaration.highlight_ranges
        {
            for range in highlight_ranges {
                references.push(HighlightedRange {
                    range,
                    category: decl_category,
                });
            }
        };

        references
    };

    let token = find_best_token(sema, position)?;
    match SymbolClass::classify(sema, token.clone()) {
        Some(SymbolClass::Definition(def)) => Some(search(def)),
        Some(SymbolClass::Reference {
            refs: _,
            typ: ReferenceType::Fuzzy,
        }) => None,
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

/// Find format string ↔ argument highlight pairs.
///
/// When the cursor is on a format specifier (e.g., `~p`) inside a format string,
/// highlight the specifier and the corresponding argument in the args list.
/// When the cursor is on an argument in the args list, highlight the argument
/// and the corresponding format specifier. Arguments can be any expression
/// (variables, function calls, etc.) — the cursor just needs to be anywhere
/// inside the expression's source range.
fn find_format_string_highlights(
    sema: &Semantic,
    position: FilePosition,
) -> Option<Vec<HighlightedRange>> {
    let source_file = sema.parse(position.file_id);
    let syntax = source_file.value.syntax();

    // The cursor might be inside a nested expression (e.g., a function call)
    // that is an argument to a format function. Walk up ancestor Call nodes
    // until we find a format function call.
    let mut call = algo::find_node_at_offset::<ast::Call>(syntax, position.offset)?;
    loop {
        if let result @ Some(_) = try_format_call_highlights(sema, position, &call) {
            return result;
        }
        call = call
            .syntax()
            .ancestors()
            .skip(1)
            .find_map(ast::Call::cast)?;
    }
}

/// Try to produce format string highlights for a specific Call node.
/// Returns `Some(highlights)` if this call is a format function and
/// highlights are found, or `None` if it isn't a format function
/// (or if no highlights are applicable at this cursor position).
fn try_format_call_highlights(
    sema: &Semantic,
    position: FilePosition,
    call: &ast::Call,
) -> Option<Vec<HighlightedRange>> {
    use crate::format_string;

    // Convert AST Call to HIR to analyze it semantically
    let call_expr = sema.to_expr(InFile::new(
        position.file_id,
        &ast::Expr::Call(call.clone()),
    ))?;

    // Check if this is a remote call to a format function
    let hir_expr = &call_expr[call_expr.value];
    let (module_id, name_id, args) = match hir_expr {
        Expr::Call {
            target: CallTarget::Remote { module, name, .. },
            args,
        } => (*module, *name, args.as_slice()),
        _ => return None,
    };

    let module_atom = call_expr[module_id].as_atom()?;
    let name_atom = call_expr[name_id].as_atom()?;
    let module_name = sema.db.lookup_atom(module_atom);
    let function_name = sema.db.lookup_atom(name_atom);

    let info = format_string::match_format_function(&module_name, &function_name, args.len())?;

    let fmt_expr_id = args[info.format_arg_index];
    let args_list_id = args[info.args_list_index];

    // Only handle literal format strings
    match &call_expr[fmt_expr_id] {
        Expr::Literal(Literal::String(_)) => {}
        _ => return None,
    }

    // Get format string source range and raw text
    let body_map = call_expr.get_body_map();
    let fmt_source = body_map.expr(fmt_expr_id)?;
    let fmt_file_range = fmt_source.range();
    if fmt_file_range.file_id != position.file_id {
        return None;
    }
    let string_range = fmt_file_range.range;
    let file_text = sema.db.file_text(position.file_id);
    let fmt_src = format_string::parse_format_source(&file_text, string_range)?;
    let parsed = &fmt_src.parsed;

    if parsed.specifiers.is_empty() {
        return None;
    }

    // Determine which argument expressions are in the args list
    let arg_expr_ids: Option<Vec<_>> = match &call_expr[args_list_id] {
        Expr::List { exprs, tail: None } => Some(exprs.clone()),
        _ => None,
    };

    // Determine if cursor is on a specifier or on an argument
    let cursor_in_format_string = string_range.contains(position.offset);

    let content_offset = fmt_src.content_offset;

    if cursor_in_format_string {
        // Cursor is inside the format string - find which specifier
        let cursor_byte_offset = usize::from(position.offset - string_range.start());
        if cursor_byte_offset < content_offset {
            return None;
        }
        let cursor_content_offset = cursor_byte_offset - content_offset;

        let spec_idx = parsed
            .specifiers
            .iter()
            .position(|s| s.range.contains(&cursor_content_offset))?;

        let spec = &parsed.specifiers[spec_idx];
        let spec_range = format_string::specifier_source_range(string_range, content_offset, spec);

        let mut highlights = vec![HighlightedRange {
            range: spec_range,
            category: None,
        }];

        // Find the corresponding argument(s)
        if let Some(ref arg_ids) = arg_expr_ids {
            // Find which argument indices map to this specifier
            for (arg_idx, &si) in parsed.arg_to_specifier.iter().enumerate() {
                if si == spec_idx
                    && let Some(&arg_id) = arg_ids.get(arg_idx)
                    && let Some(arg_source) = body_map.expr(arg_id)
                {
                    let arg_range = arg_source.range();
                    if arg_range.file_id == position.file_id {
                        highlights.push(HighlightedRange {
                            range: arg_range.range,
                            category: Some(ReferenceCategory::Read),
                        });
                    }
                }
            }
        }

        Some(highlights)
    } else {
        // Cursor might be on an argument in the args list.
        // The cursor can be anywhere inside an expression (e.g., inside
        // a function call like `foo(X)`) — we match if the cursor falls
        // within the top-level arg expression's source range.
        let arg_ids = arg_expr_ids?;

        // Find which argument the cursor is on
        let arg_idx = arg_ids.iter().position(|&arg_id| {
            body_map
                .expr(arg_id)
                .map(|s| {
                    s.range().file_id == position.file_id
                        && s.range().range.contains(position.offset)
                })
                .unwrap_or(false)
        })?;

        // Find which specifier corresponds to this argument index
        let &spec_idx = parsed.arg_to_specifier.get(arg_idx)?;
        let spec = &parsed.specifiers[spec_idx];
        let spec_range = format_string::specifier_source_range(string_range, content_offset, spec);

        let arg_source = body_map.expr(arg_ids[arg_idx])?;
        let arg_range = arg_source.range();
        if arg_range.file_id != position.file_id {
            return None;
        }

        let mut highlights = vec![
            HighlightedRange {
                range: spec_range,
                category: None,
            },
            HighlightedRange {
                range: arg_range.range,
                category: Some(ReferenceCategory::Read),
            },
        ];

        // If the specifier consumes multiple args (e.g., ~W, ~P), highlight all of them
        for (other_arg_idx, &si) in parsed.arg_to_specifier.iter().enumerate() {
            if si == spec_idx
                && other_arg_idx != arg_idx
                && let Some(&other_arg_id) = arg_ids.get(other_arg_idx)
                && let Some(other_source) = body_map.expr(other_arg_id)
            {
                let other_range = other_source.range();
                if other_range.file_id == position.file_id {
                    highlights.push(HighlightedRange {
                        range: other_range.range,
                        category: Some(ReferenceCategory::Read),
                    });
                }
            }
        }

        Some(highlights)
    }
}

#[cfg(test)]
mod tests {
    use elp_ide_db::elp_base_db::assert_eq_expected;
    use elp_syntax::TextSize;

    use super::*;
    use crate::fixture;

    #[track_caller]
    fn check(fixture_str: &str) {
        let (analysis, fixture) = fixture::with_fixture(fixture_str);
        let annotations = fixture.annotations();
        fixture::check_no_parse_errors(&analysis, fixture.file_id());

        let hls = analysis
            .highlight_related(fixture.position())
            .unwrap()
            .unwrap_or_default();

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

        assert_eq_expected!(expected, actual);
    }

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
    fn application_local_multiple_clauses() {
        check(
            r#"
            -module(main).
            -export([ function_a/0, function_b/1, function_g/1 ]).
            %%                      ^^^^^^^^^^

            -define(MACRO_A, macro_a).
            -define(MACRO_A(X), erlang:display(X)).

            function_a() ->
              fun~ction_b(42),
           %% ^^^^^^^^^^
              #record_a{}.

            function_b(X) when is_atom(X) ->
         %% ^^^^^^^^^^
              ?MACRO_A;
            function_b(X) ->
         %% ^^^^^^^^^^
              X.

            function_g(X) ->
              F = fun function_b/1,
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
    fn nominal() {
        check(
            r#"
          //- /src/main.erl
            -module(main).
            -nominal nominal_t~ype_a() :: atom().
            %%       ^^^^^^^^^^^^^^^^

            -export_type([ nominal_type_a/0 ]).
            %%             ^^^^^^^^^^^^^^

            -type user_type_a() :: type_a() | nominal_type_a().
            %%                                ^^^^^^^^^^^^^^

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

    // Helper for format string highlight tests. Uses \~ in fixture text
    // for tilde characters (the fixture parser converts \~ back to ~).
    // `cursor_on` is searched in the processed file text to find cursor position.
    // `expected` contains (text, category) pairs for comparison.
    #[track_caller]
    fn check_format(fixture_str: &str, cursor_on: &str, expected: &[(&str, Option<&str>)]) {
        let (analysis, fixture) = fixture::with_fixture(fixture_str);
        let file_id = fixture.file_id();
        fixture::check_no_parse_errors(&analysis, file_id);
        let file_text = analysis.file_text(file_id).unwrap();
        let cursor_offset = file_text
            .find(cursor_on)
            .unwrap_or_else(|| panic!("cursor_on text '{cursor_on}' not found in file"));
        let position = FilePosition {
            file_id,
            offset: TextSize::from(cursor_offset as u32),
        };
        let hls = analysis
            .highlight_related(position)
            .unwrap()
            .unwrap_or_default();
        let mut actual_with_pos: Vec<(TextSize, String, Option<&str>)> = hls
            .iter()
            .map(|hl| {
                let text = file_text[usize::from(hl.range.start())..usize::from(hl.range.end())]
                    .to_string();
                let cat = hl.category.map(|c| match c {
                    ReferenceCategory::Read => "read",
                    ReferenceCategory::Write => "write",
                });
                (hl.range.start(), text, cat)
            })
            .collect();
        actual_with_pos.sort_by_key(|(start, _, _)| *start);
        let actual: Vec<(String, Option<&str>)> = actual_with_pos
            .into_iter()
            .map(|(_, text, cat)| (text, cat))
            .collect();
        let expected_vec: Vec<(String, Option<&str>)> =
            expected.iter().map(|(t, c)| (t.to_string(), *c)).collect();
        assert_eq!(actual, expected_vec, "Format string highlights mismatch");
    }

    #[test]
    fn format_string_cursor_on_arg() {
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~p \~s", [This, That]).
            "#,
            "This",
            &[("~p", None), ("This", Some("read"))],
        );
    }

    #[test]
    fn format_string_cursor_on_specifier() {
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~p \~s", [This, That]).
            "#,
            "~p",
            &[("~p", None), ("This", Some("read"))],
        );
    }

    #[test]
    fn format_string_cursor_on_second_specifier() {
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~p \~s", [This, That]).
            "#,
            "~s",
            &[("~s", None), ("That", Some("read"))],
        );
    }

    #[test]
    fn format_string_cursor_on_second_arg() {
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~p \~s", [This, That]).
            "#,
            "That",
            &[("~s", None), ("That", Some("read"))],
        );
    }

    #[test]
    fn format_string_io_format_3() {
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format(user, "\~w \~n \~s", [Arg1, Arg2]).
            "#,
            "Arg1",
            &[("~w", None), ("Arg1", Some("read"))],
        );
    }

    #[test]
    fn format_string_non_consuming_specifier() {
        // Cursor on ~n (which doesn't consume an arg) should highlight only the specifier
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~p\~n\~\~", [A]).
            "#,
            "~n",
            &[("~n", None)],
        );
    }

    #[test]
    fn format_string_io_lib_format() {
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io_lib:format("\~p", [Arg]).
            "#,
            "Arg",
            &[("~p", None), ("Arg", Some("read"))],
        );
    }

    #[test]
    fn format_string_io_fwrite() {
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:fwrite("\~p \~s", [This, That]).
            "#,
            "This",
            &[("~p", None), ("This", Some("read"))],
        );
    }

    #[test]
    fn format_string_io_lib_fwrite() {
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io_lib:fwrite("\~w", [Val]).
            "#,
            "Val",
            &[("~w", None), ("Val", Some("read"))],
        );
    }

    #[test]
    fn format_string_multi_arg_specifier_cursor_on_first_arg() {
        // ~W consumes 2 args; cursor on first arg should highlight specifier + both args
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~W", [Data, Depth]).
            "#,
            "Data",
            &[
                ("~W", None),
                ("Data", Some("read")),
                ("Depth", Some("read")),
            ],
        );
    }

    #[test]
    fn format_string_multi_arg_specifier_cursor_on_second_arg() {
        // ~W consumes 2 args; cursor on second arg should also highlight all
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~W", [Data, Depth]).
            "#,
            "Depth",
            &[
                ("~W", None),
                ("Data", Some("read")),
                ("Depth", Some("read")),
            ],
        );
    }

    #[test]
    fn format_string_multi_arg_specifier_cursor_on_specifier() {
        // Cursor on ~W should highlight specifier + both consumed args
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~W", [Data, Depth]).
            "#,
            "~W",
            &[
                ("~W", None),
                ("Data", Some("read")),
                ("Depth", Some("read")),
            ],
        );
    }

    #[test]
    fn format_string_cursor_outside_specifier() {
        // Cursor on plain text in format string (not on any specifier) should not highlight
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("hello \~p world", [A]).
            "#,
            "hello",
            &[],
        );
    }

    #[test]
    fn format_string_cursor_on_call_arg() {
        // Cursor on a function call arg: should highlight the whole call expression
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            bar(X) -> X.
            foo() ->
                io:format("\~p \~s", [bar(ok), That]).
            "#,
            "bar(ok)",
            &[("~p", None), ("bar(ok)", Some("read"))],
        );
    }

    #[test]
    fn format_string_cursor_inside_call_arg() {
        // Cursor on `ok` inside `bar(ok)`: should still highlight ~p and bar(ok)
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            bar(X) -> X.
            foo() ->
                io:format("\~p \~s", [bar(ok), That]).
            "#,
            "ok)",
            &[("~p", None), ("bar(ok)", Some("read"))],
        );
    }

    #[test]
    fn format_string_cursor_on_arithmetic_arg() {
        // Cursor on an arithmetic expression arg
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~p", [1 + 2]).
            "#,
            "+ 2",
            &[("~p", None), ("1 + 2", Some("read"))],
        );
    }

    #[test]
    fn format_string_specifier_highlights_call_arg() {
        // Cursor on specifier ~p should highlight the call expression arg
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            bar(X) -> X.
            foo() ->
                io:format("\~p", [bar(ok)]).
            "#,
            "~p",
            &[("~p", None), ("bar(ok)", Some("read"))],
        );
    }

    #[test]
    fn format_string_nested_format_call_cursor_on_inner_arg() {
        // Arg is itself a format function call. Cursor inside the inner
        // io_lib:format should match the INNER format call, not the outer.
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~p", [io_lib:format("\~s", [Name])]).
            "#,
            "Name",
            &[("~s", None), ("Name", Some("read"))],
        );
    }

    #[test]
    fn format_string_nested_format_call_cursor_on_outer_specifier() {
        // Cursor on the outer ~p should highlight the inner io_lib:format call as the arg
        check_format(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~p", [io_lib:format("\~s", [Name])]).
            "#,
            "~p",
            &[
                ("~p", None),
                ("io_lib:format(\"~s\", [Name])", Some("read")),
            ],
        );
    }
}
