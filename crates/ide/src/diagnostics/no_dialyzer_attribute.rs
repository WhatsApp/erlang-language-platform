/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: no-dialyzer-attribute
use elp_ide_db::elp_base_db::FileId;
use hir::Semantic;
use hir::known;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;

pub(crate) struct NoDialyzerAttributeLinter;

impl Linter for NoDialyzerAttributeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::NoDialyzerAttribute
    }

    fn description(&self) -> &'static str {
        "Avoid using the -dialyzer attribute."
    }

    fn should_process_test_files(&self) -> bool {
        false
    }
}

impl GenericLinter for NoDialyzerAttributeLinter {
    type Context = ();

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let mut res = Vec::new();
        let form_list = sema.db.file_form_list(file_id);
        form_list.attributes().for_each(|(_idx, attr)| {
            if attr.name == known::dialyzer
                && let Some(range) = attr.name_range(sema.db, file_id)
            {
                res.push(GenericLinterMatchContext { range, context: () })
            }
        });
        Some(res)
    }
}

pub static LINTER: NoDialyzerAttributeLinter = NoDialyzerAttributeLinter;

#[cfg(test)]
mod tests {

    use crate::tests;

    #[test]
    fn test_dialyzer_attribute() {
        tests::check_diagnostics(
            r#"
    -module(main).
    -dialyzer({nowarn_function, foo/0}).
 %% ^^^^^^^^^ 💡 warning: W0048: Avoid using the -dialyzer attribute.
              "#,
        )
    }
}
