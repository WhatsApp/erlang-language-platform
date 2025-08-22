/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint/fix: replace_in_spec
//!
//! Return a diagnostic for the spec of a given function, which has a
//! specified type replacement in it

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_syntax::SmolStr;
use elp_text_edit::TextEdit;
use fxhash::FxHashSet;
use hir::AnyExpr;
use hir::InFile;
use hir::Semantic;
use hir::Spec;
use hir::Strategy;
use hir::TypeExpr;
use hir::fold::Fold;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use serde::Deserialize;
use serde::Serialize;

use super::Diagnostic;
use super::Severity;
use crate::MFA;
use crate::fix;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum TypeReplacement {
    TypeAliasWithString { from: MFA, to: String },
}

#[allow(unused_variables)]
pub fn replace_in_spec(
    functions: &[MFA],
    action_from: &MFA,
    action_to: &str,
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
    let def_map = sema.def_map(file_id);
    if let Some(module_name) = sema.module_name(file_id) {
        let possibles: FxHashSet<_> = functions
            .iter()
            .filter_map(
                |MFA {
                     module,
                     name,
                     arity,
                 }| {
                    if module == module_name.as_str() {
                        Some((name, arity))
                    } else {
                        None
                    }
                },
            )
            .collect();
        def_map.get_functions().for_each(|(na, def)| {
            if possibles.contains(&(&na.name().to_string(), &na.arity()))
                && let Some(spec) = &def.spec
            {
                let spec_id = InFile::new(spec.file.file_id, spec.spec_id);
                let spec = sema.db.spec_body(spec_id);
                Spec::fold(
                    sema,
                    Strategy {
                        macros: MacroStrategy::Expand,
                        parens: ParenStrategy::InvisibleParens,
                    },
                    spec_id,
                    (),
                    &mut |_acc, ctx| {
                        if let AnyExpr::TypeExpr(TypeExpr::Call { target, ref args }) = ctx.item {
                            let arity = args.len();
                            let type_label = target.label(arity as u32, sema, &spec.body);
                            let from_label: SmolStr = action_from.label().into();
                            if type_label == Some(from_label)
                                && let Some(range) = spec.body.range_for_any(sema, ctx.item_id)
                                && range.file_id == file_id
                            {
                                let mut edit_builder = TextEdit::builder();
                                edit_builder.replace(range.range, action_to.to_string());
                                let edit = edit_builder.finish();

                                let diag_label = format!(
                                    "Replace '{}' with '{}'",
                                    &action_from.label(),
                                    action_to
                                );

                                let diag = Diagnostic::new(
                                    DiagnosticCode::AdHoc(action_from.label()),
                                    diag_label.clone(),
                                    range.range,
                                )
                                .with_severity(Severity::WeakWarning)
                                .experimental()
                                .with_fixes(Some(vec![fix(
                                    "replace_type",
                                    &diag_label,
                                    SourceChange::from_text_edit(file_id, edit),
                                    range.range,
                                )]));
                                diags.push(diag);
                            }
                        };
                    },
                )
            }
        });
    }
}

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;
    use expect_test::Expect;
    use expect_test::expect;

    use super::*;
    use crate::DiagnosticsConfig;
    use crate::diagnostics::AdhocSemanticDiagnostics;
    use crate::tests::check_diagnostics_with_config_and_ad_hoc;
    use crate::tests::check_fix_with_config_and_adhoc;

    #[track_caller]
    pub(crate) fn check_fix_with_ad_hoc_semantics(
        ad_hoc_semantic_diagnostics: Vec<&dyn AdhocSemanticDiagnostics>,
        fixture_before: &str,
        fixture_after: Expect,
    ) {
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::UndefinedFunction);
        check_fix_with_config_and_adhoc(
            config,
            &ad_hoc_semantic_diagnostics,
            fixture_before,
            fixture_after,
        )
    }

    #[track_caller]
    pub(crate) fn check_diagnostics_with_ad_hoc_semantics(
        ad_hoc_semantic_diagnostics: Vec<&dyn AdhocSemanticDiagnostics>,
        fixture: &str,
    ) {
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::UndefinedFunction);
        check_diagnostics_with_config_and_ad_hoc(config, &ad_hoc_semantic_diagnostics, fixture)
    }

    #[test]
    fn check_replace_in_spec_matches() {
        check_diagnostics_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                replace_in_spec(
                    &["modu:fn/1".try_into().unwrap()],
                    &"modu:one/0".try_into().unwrap(),
                    "modu:other()",
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/modu.erl
            -module(modu).

            -type one() :: one.
            -spec fn(integer()) -> modu:one().
            %%                     ^^^^^^^^^^ 💡 weak: Replace 'modu:one/0' with 'modu:other()'
            fn(0) -> one.

            "#,
        )
    }

    #[test]
    fn check_fix_replace_in_spec() {
        check_fix_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                replace_in_spec(
                    &["modu:fn/1".try_into().unwrap()],
                    &"modu:one/0".try_into().unwrap(),
                    "modu:other()",
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/modu.erl
            -module(modu).

            -type one() :: one.
            -spec fn(integer()) -> mo~du:one().
            fn(0) -> one.
            "#,
            expect![[r#"
            -module(modu).

            -type one() :: one.
            -spec fn(integer()) -> modu:other().
            fn(0) -> one.
            "#]],
        )
    }
}
