/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Diagnostic: edoc
use std::sync::LazyLock;

use elp_ide_assists::helpers::extend_range;
use elp_ide_assists::helpers::extend_range_to_adjacent_newline_skip_inline_comment;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_syntax::AstNode;
use fxhash::FxHashSet;
use hir::edoc::EdocHeader;
use hir::edoc::EdocHeaderKind;
use hir::known;
use hir::Attribute;
use hir::FormList;
use hir::Name;
use hir::Semantic;
use text_edit::TextRange;
use text_edit::TextSize;

use super::Diagnostic;
use super::DiagnosticCode;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;

const DIAGNOSTIC_CODE: DiagnosticCode = DiagnosticCode::OldEdocSyntax;
const DIAGNOSTIC_MESSAGE: &str = "EDoc style comments are deprecated. Please use Markdown instead.";
const DIAGNOSTIC_SEVERITY: Severity = Severity::Warning;
const CONVERT_FIX_ID: &str = "convert_to_markdown";
const CONVERT_FIX_LABEL: &str = "Convert to Markdown";

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: true,
    },
    checker: &|diags, sema, file_id, _ext| {
        check(diags, sema, file_id);
    },
};

fn check(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    if let Some(comments) = sema.file_edoc_comments(file_id) {
        for header in comments.values() {
            if let Some(doc) = &header.doc {
                if let Some(doc_start) = header.start() {
                    diagnostics.push(old_edoc_syntax_diagnostic(
                        sema, file_id, doc.range, &header, doc_start,
                    ));
                }
            } else if let Some(equiv) = &header.equiv {
                if let Some(doc_start) = header.start() {
                    diagnostics.push(old_edoc_syntax_diagnostic(
                        sema,
                        file_id,
                        equiv.range,
                        &header,
                        doc_start,
                    ));
                }
            } else if let Some(hidden) = &header.hidden {
                if let Some(doc_start) = header.start() {
                    diagnostics.push(old_edoc_syntax_diagnostic(
                        sema,
                        file_id,
                        hidden.range,
                        &header,
                        doc_start,
                    ));
                }
            }
        }
    }
}

fn old_edoc_syntax_diagnostic(
    sema: &Semantic,
    file_id: FileId,
    show_range: TextRange,
    header: &EdocHeader,
    start_offset: TextSize,
) -> Diagnostic {
    let eep59_insert_offset = match header.kind {
        EdocHeaderKind::Module => module_doc_insert_offset(sema, file_id).unwrap_or(start_offset),
        EdocHeaderKind::Function => start_offset,
    };
    let authors_insert_offset = match header.kind {
        EdocHeaderKind::Module => {
            author_tags_insert_offset(sema, file_id).unwrap_or(eep59_insert_offset)
        }
        EdocHeaderKind::Function => eep59_insert_offset,
    };
    let mut builder = SourceChangeBuilder::new(file_id);
    for comment in header.comments() {
        builder.delete(extend_range(comment.to_ast(sema.db.upcast()).syntax()));
    }
    if let Some(prev_divider) = header.prev_divider(sema.db.upcast()) {
        builder.delete(extend_range(&prev_divider));
    }
    if let Some(next_divider) = header.next_divider(sema.db.upcast()) {
        builder.delete(extend_range(&next_divider));
    }
    if let Some(copyright_comment) = header.copyright_comment() {
        builder.insert(start_offset, copyright_comment);
    }
    let existing_authors = existing_authors(sema, file_id);
    for author in &header.authors {
        let author_description = author.description();
        if !author_exists(&author_description, &existing_authors) {
            builder.insert(
                authors_insert_offset,
                format!("-author(\"{}\").\n", author_description),
            );
        }
    }
    builder.insert(eep59_insert_offset, header.to_eep59());
    let source_change = builder.finish();
    let fix = crate::fix(CONVERT_FIX_ID, CONVERT_FIX_LABEL, source_change, show_range);
    Diagnostic::new(DIAGNOSTIC_CODE, DIAGNOSTIC_MESSAGE, show_range)
        .with_severity(DIAGNOSTIC_SEVERITY)
        .with_fixes(Some(vec![fix]))
}

fn author_exists(author: &str, authors: &FxHashSet<String>) -> bool {
    authors
        .iter()
        .any(|a| a.contains(author) || author.contains(a))
}

fn author_tags_insert_offset(sema: &Semantic, file_id: FileId) -> Option<TextSize> {
    let form_list = sema.form_list(file_id);
    Some(
        form_list
            .attributes()
            .skip_while(|(_idx, attr)| attr.name != known::author)
            .map(|(_idx, attr)| attr)
            .last()?
            .form_id
            .get_ast(sema.db, file_id)
            .syntax()
            .text_range()
            .start(),
    )
}

fn existing_authors(sema: &Semantic, file_id: FileId) -> FxHashSet<String> {
    let form_list = sema.form_list(file_id);
    form_list
        .attributes()
        .filter_map(|(_idx, attr)| {
            if attr.name == known::author {
                author_name(sema, file_id, attr)
            } else {
                None
            }
        })
        .collect()
}

fn author_name(sema: &Semantic, file_id: FileId, attribute: &Attribute) -> Option<String> {
    let wild_attribute = attribute.form_id.get_ast(sema.db, file_id);
    let value = wild_attribute.value()?.syntax().text().to_string();
    let value = value.trim_start_matches('(').trim_end_matches(')');
    Some(value.trim_matches(|c| c == '"' || c == '\'').to_string())
}

fn module_doc_insert_offset(sema: &Semantic, file_id: FileId) -> Option<TextSize> {
    let form_list = sema.form_list(file_id);
    let module_attribute = form_list.module_attribute()?;
    let range = match last_significant_attribute(&form_list) {
        Some(attribute) => extend_range_to_adjacent_newline_skip_inline_comment(
            attribute.form_id.get_ast(sema.db, file_id).syntax(),
        ),
        None => extend_range_to_adjacent_newline_skip_inline_comment(
            module_attribute.form_id.get_ast(sema.db, file_id).syntax(),
        ),
    };
    Some(range.end())
}

fn last_significant_attribute(form_list: &FormList) -> Option<&Attribute> {
    static SIGNIFICANT_ATTRIBUTES: LazyLock<FxHashSet<Name>> =
        LazyLock::new(|| FxHashSet::from_iter([known::author, known::compile, known::oncall]));

    form_list
        .attributes()
        .take_while(|(_idx, attr)| SIGNIFICANT_ATTRIBUTES.contains(&attr.name))
        .last()
        .map(|(_idx, attr)| attr)
}

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;
    use expect_test::expect;
    use expect_test::Expect;

    use crate::tests;
    use crate::DiagnosticsConfig;

    fn config() -> DiagnosticsConfig {
        DiagnosticsConfig::default().enable(DiagnosticCode::OldEdocSyntax)
    }

    fn check_diagnostics(fixture: &str) {
        tests::check_diagnostics_with_config(config(), fixture);
    }

    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        tests::check_fix_with_config(config(), fixture_before, fixture_after);
    }

    #[test]
    fn test_module_doc() {
        check_diagnostics(
            r#"
    %% Copyright (c) Meta Platforms, Inc. and affiliates.
    %%
    %% This is some license text.
    %%%-------------------------------------------------------------------
    %% @doc This is the module documentation.
    %% ^^^^ 💡 warning: EDoc style comments are deprecated. Please use Markdown instead.
    %%      With some more text.
    %%      And some more lines.
    %% @end
    %%%-------------------------------------------------------------------
    %%% % @format
    -module(main).

    main() ->
      dep().

    dep() -> ok.
        "#,
        )
    }

    #[test]
    fn test_function_doc() {
        check_diagnostics(
            r#"
    -module(main).
    %% @doc This is the main function documentation.
    %% ^^^^ 💡 warning: EDoc style comments are deprecated. Please use Markdown instead.
    main() ->
      dep().

    dep() -> ok.
        "#,
        )
    }

    #[test]
    fn test_function_doc_different_arities() {
        check_diagnostics(
            r#"
    -module(main).
    -export([main/0, main/2]).

    %% @doc This is the main function documentation.
    %% ^^^^ 💡 warning: EDoc style comments are deprecated. Please use Markdown instead.
    %% @see main/2 for more information.
    -spec main() -> tuple().
    main() ->
      main([], []).

    %% @doc This is the main function with two arguments documentation.
    %% ^^^^ 💡 warning: EDoc style comments are deprecated. Please use Markdown instead.
    -spec main(any(), any()) -> tuple().
    main(A, B) ->
      {A, B}.
        "#,
        )
    }

    #[test]
    fn test_incorrect_type_doc() {
        check_diagnostics(
            r#"
    -module(main).
    -export([main/2]).
    -export_type([my_integer/0]).

    %% @doc This is an incorrect type doc
    %% ^^^^ 💡 warning: EDoc style comments are deprecated. Please use Markdown instead.
    -type my_integer() :: integer().

    -type my_integer2() :: integer().

    -spec main(any(), any()) -> ok.
    main(A, B) ->
        dep().

    dep() -> ok.
        "#,
        )
    }

    #[test]
    fn test_incorrect_type_doc_followed_by_function_docs() {
        check_diagnostics(
            r#"
    -module(main).
    -export([main/2]).
    -export_type([my_integer/0]).

    %% @doc This is an incorrect type doc
    -type my_integer() :: integer().

    -type my_integer2() :: integer().

    %% @doc These are docs for the main function
    %% ^^^^ 💡 warning: EDoc style comments are deprecated. Please use Markdown instead.
    -spec main(any(), any()) -> ok.
    main(A, B) ->
        dep().

    dep() -> ok.
        "#,
        )
    }

    #[test]
    fn test_function_doc_with_multiline_tag() {
        check_diagnostics(
            r#"
    -module(main).
    -export([main/0, main/2]).

    %% @doc This is the main function documentation.
    %% ^^^^ 💡 warning: EDoc style comments are deprecated. Please use Markdown instead.
    %% @see main/2 which is a great function to look at
    %% with a very long description that goes on and on
    -spec main() -> tuple().
    main() ->
        main([], []).

    %% @doc This is the main function with two arguments documentation.
    %% ^^^^ 💡 warning: EDoc style comments are deprecated. Please use Markdown instead.
    -spec main(any(), any()) -> tuple().
    main(A, B) ->
        {A, B}.
        "#,
        )
    }

    #[test]
    fn test_module_doc_fix() {
        check_fix(
            r#"
%% @d~oc This is the module documentation.
%%       With an extra line.
-module(main).
main() ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-moduledoc """
This is the module documentation.
With an extra line.
""".
main() ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_module_doc_fix_significant_attributes() {
        check_fix(
            r#"
%% @d~oc This is the module documentation.
%%       With an extra line.
-module(main).
-oncall(my_oncall).
-author(my_author).

-export([main/0]).

main() ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-oncall(my_oncall).
-author(my_author).
-moduledoc """
This is the module documentation.
With an extra line.
""".

-export([main/0]).

main() ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_module_doc_fix_significant_attributes_with_comment() {
        check_fix(
            r#"
%% @d~oc This is the module documentation.
%%       With an extra line.
-module(main).
-oncall(my_oncall).
-author(my_author). %% And this is a comment

-export([main/0]).

main() ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-oncall(my_oncall).
-author(my_author). %% And this is a comment
-moduledoc """
This is the module documentation.
With an extra line.
""".

-export([main/0]).

main() ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_fix() {
        check_fix(
            r#"
-module(main).
%% @d~oc This is the main function documentation.
main() ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-doc """
This is the main function documentation.
""".
main() ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_fix_verbatim_quoting_single_quotes() {
        check_fix(
            r#"
-module(main).
%% @d~oc This is about `Foo' and `Bar'.
main() ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-doc """
This is about `Foo` and `Bar`.
""".
main() ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_fix_verbatim_quoting_triple_quotes() {
        check_fix(
            r#"
-module(main).
%% @d~oc This is some code:
%% ```
%%    awesome code here
%% '''
main() ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-doc """
This is some code:
```
   awesome code here
```
""".
main() ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_fix_verbatim_quoting_combined_quotes() {
        check_fix(
            r#"
-module(main).
%% @d~oc This is some `code':
%% ```
%%    awesome code here
%% '''
main() ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-doc """
This is some `code`:
```
   awesome code here
```
""".
main() ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_fix_empty_doc_slogan() {
        check_fix(
            r#"
-module(main).
%% @d~oc
%% This is some doc
%% @end
main() ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-doc """
This is some doc
""".
main() ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_fix_empty_doc_slogan_trailing_spaces() {
        check_fix(
            r#"
-module(main).
%% @d~oc   
%% This is some doc
%% @end
main() ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-doc """
This is some doc
""".
main() ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_fix_html_entities() {
        check_fix(
            r#"
-module(main).
%% @d~oc
%% In Erlang &lt;&lt;&gt;&gt; represents an empty binary.
%% @end
main() ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-doc """
In Erlang <<>> represents an empty binary.
""".
main() ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_fix_params() {
        check_fix(
            r#"
-module(main).
%% @d~oc
%% This is the main doc
%% @param A is a param
%% @param B is another param
%% @end
main(A, B) ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-doc """
This is the main doc
""".
-doc #{params => #{"A" => "is a param", "B" => "is another param"}}.
main(A, B) ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_fix_spec_in_between() {
        check_fix(
            r#"
-module(main).
%% @d~oc
%% This is the main doc

-spec main(any(), any()) -> ok.
%% These are some extra lines of doc
main(A, B) ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-doc """
This is the main doc
""".

-spec main(any(), any()) -> ok.
%% These are some extra lines of doc
main(A, B) ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_fix_return() {
        check_fix(
            r#"
-module(main).

%% @d~oc This is the main function
%% @returns ok
main(A, B) ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).

-doc """
This is the main function
### Returns
ok
""".
main(A, B) ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_fix_incorrect_type_docs() {
        check_fix(
            r#"
-module(main).
-export([main/2]).
-export_type([my_integer/0]).

%% @d~oc This is an incorrect type doc
-type my_integer() :: integer().

-type my_integer2() :: integer().

-spec main(any(), any()) -> ok.
main(A, B) ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).
-export_type([my_integer/0]).

-doc """
This is an incorrect type doc
""".
-type my_integer() :: integer().

-type my_integer2() :: integer().

-spec main(any(), any()) -> ok.
main(A, B) ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_fix_incorrect_type_docs_with_function_docs() {
        check_fix(
            r#"
-module(main).
-export([main/2]).
-export_type([my_integer/0]).

%% @doc This is an incorrect type doc
-type my_integer() :: integer().

-type my_integer2() :: integer().

%% @d~oc These are docs for the main function
-spec main(any(), any()) -> ok.
main(A, B) ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).
-export_type([my_integer/0]).

-doc """
These are docs for the main function
""".
%% @doc This is an incorrect type doc
-type my_integer() :: integer().

-type my_integer2() :: integer().

-spec main(any(), any()) -> ok.
main(A, B) ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_module_doc_divider() {
        check_fix(
            r#"
%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% Some license info
%%%------------------------------------------------------------------
%% @d~oc Some docs
%%      with some more text.
%% @end
%%%------------------------------------------------------------------
%%% % @format
-module(main).
-export([main/2]).
-export_type([my_integer/0]).

-spec main(any(), any()) -> ok.
main(A, B) ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% Some license info
%%% % @format
-module(main).
-moduledoc """
Some docs
with some more text.
""".
-export([main/2]).
-export_type([my_integer/0]).

-spec main(any(), any()) -> ok.
main(A, B) ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_multi_line_returns() {
        check_fix(
            r#"
-module(main).
-export([main/2]).

%% @d~oc These are docs for the main function
%% @returns Some multi line
%%          explanation
-spec main(any(), any()) -> ok.
main(A, B) ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).

-doc """
These are docs for the main function
### Returns
Some multi line
explanation
""".
-spec main(any(), any()) -> ok.
main(A, B) ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_multi_line_params() {
        check_fix(
            r#"
-module(main).
-export([main/2]).

%% @d~oc These are docs for the main function
%% @param A Is a param
%%        with a long explanation
%% @param B Is also a param
%%        with a long explanation
-spec main(any(), any()) -> ok.
main(A, B) ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).

-doc """
These are docs for the main function
""".
-doc #{params => #{"A" => "Is a param with a long explanation", "B" => "Is also a param with a long explanation"}}.
-spec main(any(), any()) -> ok.
main(A, B) ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_generic_tag() {
        check_fix(
            r#"
-module(main).
-export([main/2]).

%% @d~oc These are docs for the main function
%% @something First line
%%            Second line
%% @else Third line
%%       Fourth line
-spec main(any(), any()) -> ok.
main(A, B) ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).

-doc """
These are docs for the main function
### Something
First line
Second line
### Else
Third line
Fourth line
""".
-spec main(any(), any()) -> ok.
main(A, B) ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_equiv() {
        check_fix(
            r#"
-module(main).
-export([main/2]).

%% @eq~uiv main(A, B, undefined)
-spec main(any(), any()) -> ok.
main(A, B) ->
    main(A, B, undefined).

-spec main(any(), any(), any()) -> ok.
main(A, B, C) ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).

-doc #{equiv => main(A, B, undefined)}.
-spec main(any(), any()) -> ok.
main(A, B) ->
    main(A, B, undefined).

-spec main(any(), any(), any()) -> ok.
main(A, B, C) ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_equiv_combined() {
        check_fix(
            r#"
-module(main).
-export([main/2]).

%% @do~c This is the main function
%% @equiv main(A, B, undefined)
-spec main(any(), any()) -> ok.
main(A, B) ->
    main(A, B, undefined).

-spec main(any(), any(), any()) -> ok.
main(A, B, C) ->
    dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).

-doc """
This is the main function
""".
-doc #{equiv => main(A, B, undefined)}.
-spec main(any(), any()) -> ok.
main(A, B) ->
    main(A, B, undefined).

-spec main(any(), any(), any()) -> ok.
main(A, B, C) ->
    dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_module_doc_author() {
        check_fix(
            r#"
%%%-----------------------------------------------------------------------------
%%% @author Some Author <some@email.com>
%%% @d~oc
%%% Some description
%%% @end
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-author("Some Author <some@email.com>").
-moduledoc """
Some description
""".
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_module_doc_author_existing_different() {
        check_fix(
            r#"
%%%-----------------------------------------------------------------------------
%%% @author Another Author <some@email.com>
%%% @d~oc
%%% Some description
%%% @end
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-author("Some Author <some@email.com>").
-oncall(something).
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-author("Some Author <some@email.com>").
-author("Another Author <some@email.com>").
-oncall(something).
-moduledoc """
Some description
""".
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_module_doc_author_existing_same() {
        check_fix(
            r#"
%%%-----------------------------------------------------------------------------
%%% @author Some Author <some@email.com>
%%% @d~oc
%%% Some description
%%% @end
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-author("Some Author <some@email.com>").
-oncall(something).
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-author("Some Author <some@email.com>").
-oncall(something).
-moduledoc """
Some description
""".
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_module_doc_author_existing_contains_one() {
        check_fix(
            r#"
%%%-----------------------------------------------------------------------------
%%% @author Some Author
%%% @d~oc
%%% Some description
%%% @end
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-author("Some Author <some@email.com>").
-oncall(something).
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-author("Some Author <some@email.com>").
-oncall(something).
-moduledoc """
Some description
""".
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_module_doc_author_existing_contains_two() {
        check_fix(
            r#"
%%%-----------------------------------------------------------------------------
%%% @author Some Author <some@email.com>
%%% @d~oc
%%% Some description
%%% @end
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-author('some@email.com').
-oncall(something).
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-author('some@email.com').
-oncall(something).
-moduledoc """
Some description
""".
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_module_doc_copyright() {
        check_fix(
            r#"
%%%-----------------------------------------------------------------------------
%%% @author Some Author <some@email.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @d~oc
%%% Some description
%%% @end
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
%%% Copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-author("Some Author <some@email.com>").
-moduledoc """
Some description
""".
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_module_doc_copyright_redundant() {
        check_fix(
            r#"
%%%-----------------------------------------------------------------------------
%%% Copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @author Some Author <some@email.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @d~oc
%%% Some description
%%% @end
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
%%%-----------------------------------------------------------------------------
%%% Copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% Some extra info
%%%-----------------------------------------------------------------------------
-module(main).
-author("Some Author <some@email.com>").
-moduledoc """
Some description
""".
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_private() {
        check_fix(
            r#"
-module(main).
-export([main/2]).

%% @d~oc
%% @private
-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).

-doc hidden.
-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_private_local() {
        check_fix(
            r#"
-module(main).
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

%% @d~oc
%% @private
dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_module_doc_private() {
        check_fix(
            r#"
%% @d~oc
%% @private
-module(main).
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-moduledoc hidden.
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_module_doc_hidden() {
        check_fix(
            r#"
%% @d~oc
%% @hidden
-module(main).
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-moduledoc hidden.
-export([main/2]).

-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_private_standalone() {
        check_fix(
            r#"
-module(main).
-export([main/2]).

%% @p~rivate
-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).

-doc hidden.
-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_private_with_doc() {
        check_fix(
            r#"
-module(main).
-export([main/2]).

%% @d~oc This function has some documentation
%% @private
-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).

-doc """
This function has some documentation
""".
%% -doc hidden. https://github.com/erlang/otp/issues/9672
-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_unknown() {
        check_fix(
            r#"
-module(main).
-export([main/2]).

%% @d~oc
%% @unknown Some unknown tag
-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).

-doc """
### Unknown
Some unknown tag
""".
-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_ascii_art() {
        check_fix(
            r#"
-module(main).
-export([main/2]).

%% @d~oc This is a test
%%   +---+
%%    \ /
%%     +
-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#,
            expect![[r#"
-module(main).
-export([main/2]).

-doc """
This is a test
+---+
 \ /
  +
""".
-spec main(any(), any()) -> ok.
main(A, B) ->
  dep().

dep() -> ok.
"#]],
        )
    }

    #[test]
    fn test_function_doc_see() {
        check_fix(
            r#"
-module(main).
-export([main/0, main/2]).

%% @d~oc This is the main function documentation.
%% @see main/2 for more information.
-spec main() -> tuple().
main() ->
    main([], []).

%% @doc This is the main function with two arguments documentation.
-spec main(any(), any()) -> tuple().
main(A, B) ->
    {A, B}.
"#,
            expect![[r#"
-module(main).
-export([main/0, main/2]).

-doc """
This is the main function documentation.
See `main/2` for more information.
""".
-spec main() -> tuple().
main() ->
    main([], []).

%% @doc This is the main function with two arguments documentation.
-spec main(any(), any()) -> tuple().
main(A, B) ->
    {A, B}.
"#]],
        )
    }

    #[test]
    fn test_function_doc_see_end() {
        check_fix(
            r#"
-module(main).
-export([main/0, main/2]).

%% @d~oc This is the main function documentation.
%% @see main/2
%% @end
-spec main() -> tuple().
main() ->
    main([], []).

%% @doc This is the main function with two arguments documentation.
-spec main(any(), any()) -> tuple().
main(A, B) ->
    {A, B}.
"#,
            expect![[r#"
-module(main).
-export([main/0, main/2]).

-doc """
This is the main function documentation.
See `main/2`
""".
-spec main() -> tuple().
main() ->
    main([], []).

%% @doc This is the main function with two arguments documentation.
-spec main(any(), any()) -> tuple().
main(A, B) ->
    {A, B}.
"#]],
        )
    }

    #[test]
    fn test_function_doc_see_multiline() {
        check_fix(
            r#"
-module(main).
-export([main/0, main/2]).

%% @d~oc This is the main function documentation.
%% @see main/2 which is a great function to look at
%% with a very long description that goes on and on
-spec main() -> tuple().
main() ->
    main([], []).

-spec main(any(), any()) -> tuple().
main(A, B) ->
    {A, B}.
"#,
            expect![[r#"
-module(main).
-export([main/0, main/2]).

-doc """
This is the main function documentation.
See `main/2` which is a great function to look at
with a very long description that goes on and on
""".
-spec main() -> tuple().
main() ->
    main([], []).

-spec main(any(), any()) -> tuple().
main(A, B) ->
    {A, B}.
"#]],
        )
    }

    #[test]
    fn test_function_doc_last_comment_is_divider() {
        check_fix(
            r#"
-module(main).
-export([main/0]).

%%--------------------------------------------------------------------
%% @d~oc
%% Handling cast messages
%%--------------------------------------------------------------------
-spec main() -> tuple().
main() ->
    {}.
"#,
            expect![[r#"
-module(main).
-export([main/0]).

-doc """
Handling cast messages
""".
-spec main() -> tuple().
main() ->
    {}.
"#]],
        )
    }

    #[test]
    fn test_function_doc_big_divider() {
        check_fix(
            r#"
-module(main).
-export([main/0]).

%%====================================================================
%% API
%%====================================================================
%% @d~oc
%% The main function
%%--------------------------------------------------------------------
-spec main() -> tuple().
main() ->
    {}.
"#,
            expect![[r#"
-module(main).
-export([main/0]).

%%====================================================================
%% API
%%====================================================================
-doc """
The main function
""".
-spec main() -> tuple().
main() ->
    {}.
"#]],
        )
    }
}
