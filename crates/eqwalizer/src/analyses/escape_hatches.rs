/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_types_db::eqwalizer::form::ExternalForm;
use elp_types_db::eqwalizer::visitor::Visitor;
use elp_types_db::eqwalizer::EqwalizerDiagnostic;
use elp_types_db::eqwalizer::TextRange;

use super::EqwalizerAnalysesDatabase;
use crate::ast::Pos;

struct EscapeHatchesVisitor<'a> {
    diagnostics: &'a mut Vec<EqwalizerDiagnostic>,
}

impl<'a> Visitor<'a, ()> for EscapeHatchesVisitor<'a> {
    fn visit_form(&mut self, form: &'a ExternalForm) -> Result<(), ()> {
        match form {
            ExternalForm::ElpMetadata(meta) => {
                for fixme in &meta.fixmes {
                    if fixme.is_ignore {
                        self.diagnostics.push(ignore_diagnostic(&fixme.comment))
                    } else {
                        self.diagnostics.push(fixme_diagnostic(&fixme.comment))
                    }
                }
            }
            ExternalForm::EqwalizerNowarnFunction(attr) => {
                nowarn_diagnostic(&attr.location).map(|d| self.diagnostics.push(d));
            }
            _ => (),
        }
        Ok(())
    }
}

fn fixme_diagnostic(range: &TextRange) -> EqwalizerDiagnostic {
    EqwalizerDiagnostic {
        range: range.clone().into(),
        message: "%eqwalizer:fixme escape hatch".into(),
        uri: "https://fb.me/eqwalizer_stats#eqwalizer_fixme".into(),
        code: "eqwalizer_fixme".into(),
        expression: None,
        explanation: None,
        diagnostic: None,
    }
}

fn ignore_diagnostic(range: &TextRange) -> EqwalizerDiagnostic {
    EqwalizerDiagnostic {
        range: range.clone().into(),
        message: "%eqwalizer:ignore escape hatch".into(),
        uri: "https://fb.me/eqwalizer_stats#eqwalizer_ignore".into(),
        code: "eqwalizer_ignore".into(),
        expression: None,
        explanation: None,
        diagnostic: None,
    }
}

fn nowarn_diagnostic(pos: &Pos) -> Option<EqwalizerDiagnostic> {
    if let Pos::TextRange(range) = pos {
        Some(EqwalizerDiagnostic {
            range: range.clone().into(),
            message: "-eqwalizer({nowarn_function, ...}) escape hatch".into(),
            uri: "https://fb.me/eqwalizer_stats#eqwalizer_nowarn".into(),
            code: "eqwalizer_nowarn".into(),
            expression: None,
            explanation: None,
            diagnostic: None,
        })
    } else {
        None
    }
}

pub(crate) fn escape_hatches(
    db: &dyn EqwalizerAnalysesDatabase,
    diagnostics: &mut Vec<EqwalizerDiagnostic>,
    project_id: ProjectId,
    module: ModuleName,
) {
    if let Ok(ast) = db.converted_ast(project_id, module) {
        let mut visitor = EscapeHatchesVisitor { diagnostics };
        let _ = visitor.visit_ast(&ast);
    }
}
