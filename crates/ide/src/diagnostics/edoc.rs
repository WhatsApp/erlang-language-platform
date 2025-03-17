/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Diagnostic: edoc

use elp_ide_assists::helpers::extend_range;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_syntax::AstNode;
use fxhash::FxHashSet;
use hir::edoc::EdocHeader;
use hir::edoc::EdocHeaderKind;
use hir::edoc::Tag;
use hir::known;
use hir::Attribute;
use hir::Name;
use hir::Semantic;
use lazy_static::lazy_static;
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
        for (_form, header) in comments {
            if let Some(doc) = &header.doc {
                if let Some(show_range) = doc.tag_range(sema.db.upcast()) {
                    diagnostics.push(old_edoc_syntax_diagnostic(
                        sema, file_id, show_range, &header,
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
) -> Diagnostic {
    let fix = match header.kind {
        EdocHeaderKind::Module => {
            let insert_offset =
                module_doc_insert_offset(sema, file_id).unwrap_or(header.range.end());
            let mut builder = SourceChangeBuilder::new(file_id);
            for comment in header.comments() {
                builder.delete(extend_range(comment.to_ast(sema.db.upcast()).syntax()));
            }
            builder.insert(insert_offset, header.to_markdown());
            let source_change = builder.finish();
            crate::fix(CONVERT_FIX_ID, CONVERT_FIX_LABEL, source_change, show_range)
        }
        EdocHeaderKind::Function => {
            let mut builder = SourceChangeBuilder::new(file_id);
            builder.replace(header.range, header.to_markdown());
            let source_change = builder.finish();
            crate::fix(CONVERT_FIX_ID, CONVERT_FIX_LABEL, source_change, show_range)
        }
    };
    Diagnostic::new(DIAGNOSTIC_CODE, DIAGNOSTIC_MESSAGE, show_range)
        .with_severity(DIAGNOSTIC_SEVERITY)
        .with_fixes(Some(vec![fix]))
}

fn module_doc_insert_offset(sema: &Semantic, file_id: FileId) -> Option<TextSize> {
    let form_list = sema.form_list(file_id);
    let module_attribute = form_list.module_attribute()?;
    let source = sema.parse(file_id);
    let syntax = match last_significant_attribute(sema, file_id) {
        Some(attribute) => attribute.form_id.get(&source.value).syntax().clone(),
        None => module_attribute.form_id.get(&source.value).syntax().clone(),
    };
    Some(syntax.text_range().end())
}

lazy_static! {
    static ref SIGNIFICANT_ATTRIBUTES: FxHashSet<Name> =
        FxHashSet::from_iter([known::author, known::compile, known::oncall]);
}

fn last_significant_attribute(sema: &Semantic, file_id: FileId) -> Option<Attribute> {
    sema.form_list(file_id)
        .attributes()
        .take_while(|(_idx, attr)| is_significant_attribute(attr))
        .last()
        .map(|(_idx, attr)| attr.clone())
}

fn is_significant_attribute(attribute: &Attribute) -> bool {
    SIGNIFICANT_ATTRIBUTES.contains(&attribute.name)
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
  - @param A is a param
  - @param B is another param
""".
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
}
