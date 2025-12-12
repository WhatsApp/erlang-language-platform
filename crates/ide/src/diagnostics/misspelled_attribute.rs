/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_syntax::ast::AstNode;
use hir::Semantic;

use super::DiagnosticCode;
use super::GenericLinter;
use super::GenericLinterMatchContext;
use super::Linter;
use crate::Assist;
use crate::TextRange;
use crate::TextSize;
use crate::diagnostics::Severity;
use crate::fix;

// Diagnostic: misspelled_attribute
//
// Finds attributes with names similar to "known" attributes,
// and suggests replacing them
//
// ```
// -include_lob("/foo/bar/baz.hrl").
// ```
// ->
// ```
// -include_lib("/foo/bar/baz.hrl").
// ```

pub(crate) struct MisspelledAttributeLinter;

impl Linter for MisspelledAttributeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::MisspelledAttribute
    }

    fn description(&self) -> &'static str {
        "misspelled attribute"
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> super::Severity {
        Severity::Error
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context {
    attr_name: String,
    suggested_rename: String,
}

impl GenericLinter for MisspelledAttributeLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let form_list = sema.db.file_form_list(file_id);
        let parsed_file = sema.db.parse(file_id);
        let mut res = Vec::new();

        for (_id, attr) in form_list.attributes() {
            if let Some(suggested_rename) = looks_like_misspelling(attr) {
                let attr_form = attr.form_id.get(&parsed_file.tree());
                if let Some(attr_name_node) = attr_form.name() {
                    let attr_name_range_with_hyphen = attr_name_node.syntax().text_range();
                    let attr_name_range = TextRange::new(
                        attr_name_range_with_hyphen
                            .start()
                            .checked_add(TextSize::of('-'))
                            .unwrap(),
                        attr_name_range_with_hyphen.end(),
                    );

                    res.push(GenericLinterMatchContext {
                        range: attr_name_range,
                        context: Context {
                            attr_name: attr.name.to_string(),
                            suggested_rename: suggested_rename.to_string(),
                        },
                    });
                }
            }
        }
        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> std::borrow::Cow<'_, str> {
        format!(
            "misspelled attribute, saw '{}' but expected '{}'",
            context.attr_name, context.suggested_rename
        )
        .into()
    }

    fn fixes(
        &self,
        context: &Self::Context,
        range: TextRange,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let edit = TextEdit::replace(range, context.suggested_rename.clone());
        let msg = format!("Change to '{}'", context.suggested_rename);
        Some(vec![fix(
            "fix_misspelled_attribute",
            &msg,
            SourceChange::from_text_edit(file_id, edit),
            range,
        )])
    }
}

const KNOWN_ATTRIBUTES: &[&str] = &[
    "include_lib",
    "export_type",
    "behaviour",
    "moduledoc",
    "callback",
    "dialyzer",
    "behavior",
    "on_load",
    "include",
    "feature",
    "compile",
    "record",
    "oncall",
    "module",
    "import",
    "export",
    "define",
    "opaque",
    "typing",
    "author",
    "type",
    "spec",
    "nifs",
    "file",
    "vsn",
    "doc",
];

fn looks_like_misspelling(attr: &hir::Attribute) -> Option<&str> {
    let mut suggestions: Vec<(&str, f64)> = KNOWN_ATTRIBUTES
        .iter()
        .filter(|&known| &attr.name != known)
        .filter(|&known| {
            let close_enough: usize = (attr.name.len() / 3).clamp(1, 3);
            triple_accel::levenshtein::rdamerau(attr.name.as_str().as_bytes(), known.as_bytes())
                <= u32::try_from(close_enough).unwrap()
        })
        .map(|&known| (known, strsim::jaro_winkler(&attr.name, known)))
        .collect::<Vec<(&str, f64)>>();
    suggestions.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
    suggestions
        .first()
        .map(|(suggestion, _similarity)| suggestion)
        .copied()
}

pub static LINTER: MisspelledAttributeLinter = MisspelledAttributeLinter;

// To run the tests via cargo
// cargo test --package elp_ide --lib
#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::diagnostics::DiagnosticsConfig;
    use crate::fixture;
    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn test_can_find_and_fix_misspelling() {
        check_diagnostics(
            r#"
    -module(main).
    -dyalizer({nowarn_function, f/0}).
 %%% ^^^^^^^^ 💡 error: W0013: misspelled attribute, saw 'dyalizer' but expected 'dialyzer'
            "#,
        );
        check_fix(
            r#"
    -module(main).
    -dyalize~r({nowarn_function, f/0}).
            "#,
            expect![[r#"
    -module(main).
    -dialyzer({nowarn_function, f/0}).
            "#]],
        );
    }

    #[test]
    fn test_can_ignore_valid_spelling() {
        let (analysis, fixture) = fixture::with_fixture(
            r#"
    -module(main).
    -di~alyzer({nowarn_function, f/0}).
            "#,
        );
        let config =
            DiagnosticsConfig::default().disable(elp_ide_db::DiagnosticCode::NoDialyzerAttribute);
        let diags = analysis
            .native_diagnostics(&config, &vec![], fixture.file_id())
            .unwrap();
        assert!(
            diags.is_empty(),
            "didn't expect diagnostic errors in files: {diags:?}"
        );
    }

    #[test]
    fn test_does_not_consider_the_names_of_records() {
        let (analysis, fixture) = fixture::with_fixture(
            r#"
    -module(main).
    -re~cord(dyalizer, {field = "foo"}).

    f(#dyalizer{field = Bar}) -> Bar.
            "#,
        );
        let config = DiagnosticsConfig::default();
        let diags = analysis
            .native_diagnostics(&config, &vec![], fixture.file_id())
            .unwrap();
        assert!(
            diags.is_empty(),
            "didn't expect diagnostic errors in files: {diags:?}"
        );
    }

    #[test]
    fn test_module_doc() {
        check_diagnostics(
            r#"
    -module(main).
    -module_doc """
%%%  ^^^^^^^^^^ 💡 error: W0013: misspelled attribute, saw 'module_doc' but expected 'moduledoc'
    Hola
    """.
            "#,
        );
    }

    #[test]
    fn test_docs() {
        check_diagnostics(
            r#"
    -module(main).
    -docs """
%%%  ^^^^ 💡 error: W0013: misspelled attribute, saw 'docs' but expected 'doc'
    Hola
    """.
    foo() -> ok.
            "#,
        );
    }
}
