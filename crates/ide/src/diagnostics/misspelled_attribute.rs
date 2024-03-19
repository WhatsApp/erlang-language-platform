/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_syntax::ast::AstNode;
use elp_syntax::ast::WildAttribute;
use hir::Attribute;
use hir::Semantic;
use text_edit::TextEdit;

use super::Diagnostic;
use crate::diagnostics::RelatedInformation;
use crate::fix;
use crate::TextRange;
use crate::TextSize;

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
pub(crate) fn misspelled_attribute(
    sema: &Semantic,
    diagnostics: &mut Vec<Diagnostic>,
    file_id: FileId,
) {
    let form_list = sema.db.file_form_list(file_id);
    let potential_misspellings = form_list.attributes().filter_map(|(id, attr)| {
        looks_like_misspelling(attr).map(|suggested_rename| (id, attr, suggested_rename))
    });
    potential_misspellings.for_each(|(_id, attr, suggested_rename)| {
        let parsed_file = sema.db.parse(file_id);
        let attr_form = attr.form_id.get(&parsed_file.tree());
        diagnostics.push(make_diagnostic(
            sema,
            file_id,
            attr,
            attr_form,
            suggested_rename,
        ))
    })
}

const KNOWN_ATTRIBUTES: &[&str] = &[
    "include_lib",
    "export_type",
    "behaviour",
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
];

fn looks_like_misspelling(attr: &Attribute) -> Option<&str> {
    let mut suggestions: Vec<(&str, f64)> = KNOWN_ATTRIBUTES
        .iter()
        .filter(|&known| &attr.name != known)
        .filter(|&known| {
            let close_enough: usize = std::cmp::max(1, std::cmp::min(3, attr.name.len() / 3));
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

fn make_diagnostic(
    sema: &Semantic,
    file_id: FileId,
    attr: &Attribute,
    attr_form: WildAttribute,
    suggested_rename: &str,
) -> Diagnostic {
    // Includes the '-', e.g. "-dialyzer" from `-dialyzer([]).`, but we don't
    // want to apply another `.name()`, because for attributes with special
    // meanings like `-record(foo, ...).` we would get "foo"
    let attr_name_range_with_hyphen = attr_form.name().unwrap().syntax().text_range();
    let attr_name_range = TextRange::new(
        attr_name_range_with_hyphen
            .start()
            .checked_add(TextSize::of('-'))
            .unwrap(),
        attr_name_range_with_hyphen.end(),
    );

    let edit = TextEdit::replace(attr_name_range, suggested_rename.to_string());

    Diagnostic::new(
        super::DiagnosticCode::MisspelledAttribute,
        format!(
            "misspelled attribute, saw '{}' but expected '{}'",
            attr.name, suggested_rename
        ),
        attr_name_range,
    )
    .with_related(Some(vec![RelatedInformation {
        range: attr_name_range,
        message: "Misspelled attribute".to_string(),
    }]))
    .with_fixes(Some(vec![fix(
        "fix_misspelled_attribute",
        format!("Change misspelled attribute to '{}'", suggested_rename).as_str(),
        SourceChange::from_text_edit(file_id, edit),
        attr_name_range,
    )]))
    .with_ignore_fix(sema, file_id)
}

// To run the tests via cargo
// cargo test --package elp_ide --lib
#[cfg(test)]
mod tests {
    use crate::diagnostics::DiagnosticCode;
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
 %%% ^^^^^^^^ 💡 error: misspelled attribute, saw 'dyalizer' but expected 'dialyzer'
            "#,
        );
        check_fix(
            r#"
    -module(main).
    -dyalize~r({nowarn_function, f/0}).
            "#,
            r#"
    -module(main).
    -dialyzer({nowarn_function, f/0}).
            "#,
        );
    }

    #[test]
    fn test_can_ignore_valid_spelling() {
        let (analysis, position, _, _, _) = fixture::annotations(
            r#"
    -module(main).
    -di~alyzer({nowarn_function, f/0}).
            "#,
        );
        let mut config = DiagnosticsConfig::default();
        config
            .disabled
            .insert(DiagnosticCode::MissingCompileWarnMissingSpec);
        let diags = analysis
            .native_diagnostics(&config, position.file_id)
            .unwrap();
        assert!(
            diags.is_empty(),
            "didn't expect diagnostic errors in files: {:?}",
            diags
        );
    }

    #[test]
    fn test_does_not_consider_the_names_of_records() {
        let (analysis, position, _, _, _) = fixture::annotations(
            r#"
    -module(main).
    -re~cord(dyalizer, {field = "foo"}).

    f(#dyalizer{field = Bar}) -> Bar.
            "#,
        );
        let mut config = DiagnosticsConfig::default();
        config
            .disabled
            .insert(DiagnosticCode::MissingCompileWarnMissingSpec);
        let diags = analysis
            .native_diagnostics(&config, position.file_id)
            .unwrap();
        assert!(
            diags.is_empty(),
            "didn't expect diagnostic errors in files: {:?}",
            diags
        );
    }
}
