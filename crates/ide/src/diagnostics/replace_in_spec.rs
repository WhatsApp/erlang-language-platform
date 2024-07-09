/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint/fix: replace_in_spec
//!
//! Return a diagnostic for the spec of a given function, which has a
//! specified type replacement in it

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::DiagnosticCode;
use elp_syntax::SmolStr;
use fxhash::FxHashSet;
use hir::fold::Fold;
use hir::AnyExpr;
use hir::InFile;
use hir::Semantic;
use hir::Spec;
use hir::Strategy;
use hir::TypeExpr;
use serde::Deserialize;
use serde::Serialize;

use super::Diagnostic;
use super::Severity;
use crate::MFA;

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
            .into_iter()
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
            if possibles.contains(&(&na.name().to_string(), &na.arity())) {
                if let Some(spec) = &def.spec {
                    let spec_id = InFile::new(spec.file.file_id, spec.spec_id);
                    let spec = sema.db.spec_body(spec_id);
                    Spec::fold(
                        sema,
                        Strategy::InvisibleMacros,
                        spec_id,
                        (),
                        &mut |_acc, ctx| {
                            match ctx.item {
                                AnyExpr::TypeExpr(TypeExpr::Call { target, ref args }) => {
                                    let arity = args.len();
                                    let type_label = target.label(arity as u32, sema, &spec.body);
                                    let from_label: SmolStr = action_from.label().into();
                                    if &type_label == &Some(from_label) {
                                        if let Some(range) =
                                            spec.body.range_for_any(sema, ctx.item_id)
                                        {
                                            diags.push(
                                                Diagnostic::new(
                                                    DiagnosticCode::AdHoc(action_from.label()),
                                                    format!(
                                                        "Replace '{}' with '{}'",
                                                        &action_from.label(),
                                                        action_to
                                                    ),
                                                    range,
                                                )
                                                .with_severity(Severity::WeakWarning)
                                                .experimental(),
                                            );
                                        }
                                    }
                                }
                                _ => {}
                            };
                        },
                    )
                }
            }
        });
    }
}

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;

    use super::*;
    use crate::diagnostics::AdhocSemanticDiagnostics;
    use crate::tests::check_diagnostics_with_config;
    use crate::DiagnosticsConfig;

    #[track_caller]
    pub(crate) fn check_diagnostics_with_ad_hoc_semantics<'a>(
        ad_hoc_semantic_diagnostics: Vec<&'a dyn AdhocSemanticDiagnostics>,
        fixture: &str,
    ) {
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::UndefinedFunction)
            .set_ad_hoc_semantic_diagnostics(ad_hoc_semantic_diagnostics);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn check_replace_in_spec_matches() {
        check_diagnostics_with_ad_hoc_semantics(
            vec![&|acc, sema, file_id, _ext| {
                replace_in_spec(
                    &vec!["modu:fn/1".try_into().unwrap()],
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
            %%                     ^^^^^^^^^^ weak: Replace 'modu:one/0' with 'modu:other()'
            fn(0) -> one.

            "#,
        )
    }
}
