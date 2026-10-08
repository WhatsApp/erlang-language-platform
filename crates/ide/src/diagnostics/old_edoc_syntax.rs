/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: old_edoc_syntax

use elp_ide_db::elp_base_db::FileRange;

use super::DiagnosticCode;
use super::GenericLinter;
use super::GenericLinterMatchContext;
use super::Linter;
use super::LinterContext;

pub(crate) struct OldEdocSyntaxLinter;

impl Linter for OldEdocSyntaxLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::OldEdocSyntax
    }

    fn description(&self) -> &'static str {
        "EDoc style comments are deprecated. Please use Markdown instead."
    }
}

impl GenericLinter for OldEdocSyntaxLinter {
    type Context = ();

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<()>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let mut res = Vec::new();
        if let Some(comments) = sema.file_edoc_comments(file_id) {
            for header in comments.values() {
                if let Some(doc) = &header.doc {
                    if header.start().is_some() {
                        res.push(GenericLinterMatchContext {
                            range: FileRange {
                                file_id,
                                range: doc.range,
                            },
                            context: (),
                        });
                    }
                } else if let Some(equiv) = &header.equiv {
                    if header.start().is_some() {
                        res.push(GenericLinterMatchContext {
                            range: FileRange {
                                file_id,
                                range: equiv.range,
                            },
                            context: (),
                        });
                    }
                } else if let Some(deprecated) = &header.deprecated {
                    if header.start().is_some() {
                        res.push(GenericLinterMatchContext {
                            range: FileRange {
                                file_id,
                                range: deprecated.range,
                            },
                            context: (),
                        });
                    }
                } else if let Some(hidden) = &header.hidden
                    && header.start().is_some()
                {
                    res.push(GenericLinterMatchContext {
                        range: FileRange {
                            file_id,
                            range: hidden.range,
                        },
                        context: (),
                    });
                }
            }
        }
        Some(res)
    }
}

pub static LINTER: OldEdocSyntaxLinter = OldEdocSyntaxLinter;

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;

    use crate::DiagnosticsConfig;
    use crate::tests;

    fn config() -> DiagnosticsConfig {
        DiagnosticsConfig::default().enable(DiagnosticCode::OldEdocSyntax)
    }

    fn check_diagnostics(fixture: &str) {
        tests::check_diagnostics_with_config(config(), fixture);
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
    %% ^^^^ warning: W0038: EDoc style comments are deprecated. Please use Markdown instead.
    %%    | 💡 <suppression>
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
    %% ^^^^ warning: W0038: EDoc style comments are deprecated. Please use Markdown instead.
    %%    | 💡 <suppression>
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
    %% ^^^^ warning: W0038: EDoc style comments are deprecated. Please use Markdown instead.
    %%    | 💡 <suppression>
    %% @see main/2 for more information.
    -spec main() -> tuple().
    main() ->
      main([], []).

    %% @doc This is the main function with two arguments documentation.
    %% ^^^^ warning: W0038: EDoc style comments are deprecated. Please use Markdown instead.
    %%    | 💡 <suppression>
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
    %% ^^^^ warning: W0038: EDoc style comments are deprecated. Please use Markdown instead.
    %%    | 💡 <suppression>
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
    %% ^^^^ warning: W0038: EDoc style comments are deprecated. Please use Markdown instead.
    %%    | 💡 <suppression>
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
    %% ^^^^ warning: W0038: EDoc style comments are deprecated. Please use Markdown instead.
    %%    | 💡 <suppression>
    %% @see main/2 which is a great function to look at
    %% with a very long description that goes on and on
    -spec main() -> tuple().
    main() ->
        main([], []).

    %% @doc This is the main function with two arguments documentation.
    %% ^^^^ warning: W0038: EDoc style comments are deprecated. Please use Markdown instead.
    %%    | 💡 <suppression>
    -spec main(any(), any()) -> tuple().
    main(A, B) ->
        {A, B}.
        "#,
        )
    }
}
