/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unused-macro
//
// Return a warning if a macro defined in an .erl file has no references to it

use std::borrow::Cow;

use elp_ide_assists::Assist;
use elp_ide_assists::helpers::extend_range;
use elp_ide_db::SymbolDefinition;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use hir::Semantic;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::DiagnosticTag;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::fix;

pub(crate) struct UnusedMacroLinter;

impl Linter for UnusedMacroLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnusedMacro
    }
    fn description(&self) -> &'static str {
        "Unused macro."
    }
    fn should_process_generated_files(&self) -> bool {
        true
    }
    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.db.file_kind(file_id).is_module()
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    delete_range: TextRange,
    name: String,
}

impl GenericLinter for UnusedMacroLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let mut res = Vec::new();
        let def_map = sema.def_map_local(file_id);
        for (name, def) in def_map.get_macros() {
            if !SymbolDefinition::Define(def.clone())
                .usages(sema)
                .at_least_one()
                && let Some(range) = def.name_range(sema.db)
            {
                let context = Context {
                    delete_range: extend_range(def.source(sema.db).syntax()),
                    name: name.to_string(),
                };
                res.push(GenericLinterMatchContext { range, context });
            }
        }
        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!("Unused macro ({})", context.name))
    }

    fn tag(&self, _context: &Self::Context) -> Option<super::DiagnosticTag> {
        Some(DiagnosticTag::Unused)
    }

    fn fixes(&self, context: &Context, _sema: &Semantic, file_id: FileId) -> Option<Vec<Assist>> {
        Some(vec![delete_unused_macro(
            file_id,
            context.delete_range,
            &context.name,
        )])
    }
}

pub static LINTER: UnusedMacroLinter = UnusedMacroLinter;

fn delete_unused_macro(file_id: FileId, range: TextRange, name: &str) -> Assist {
    let mut builder = TextEdit::builder();
    builder.delete(range);
    let edit = builder.finish();
    fix(
        "delete_unused_macro",
        &format!("Delete unused macro ({name})"),
        SourceChange::from_text_edit(file_id, edit),
        range,
    )
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix;

    #[track_caller]
    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::HirUnresolvedMacro);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn test_unused_macro() {
        check_diagnostics(
            r#"
-module(main).
-define(MEANING_OF_LIFE, 42).
    %%  ^^^^^^^^^^^^^^^ 💡 warning: W0002: Unused macro (MEANING_OF_LIFE)
            "#,
        );
        check_fix(
            r#"
-module(main).
-define(MEA~NING_OF_LIFE, 42).
            "#,
            expect![[r#"
-module(main).
            "#]],
        )
    }

    #[test]
    fn test_unused_macro_not_applicable() {
        check_diagnostics(
            r#"
-module(main).
-define(MEANING_OF_LIFE, 42).
main() ->
  ?MEANING_OF_LIFE.
            "#,
        );
    }

    #[test]
    fn test_unused_macro_not_applicable_for_hrl_file() {
        check_diagnostics(
            r#"
//- /include/foo.hrl
-define(MEANING_OF_LIFE, 42).
            "#,
        );
    }

    #[test]
    fn test_unused_macro_with_arg() {
        check_diagnostics(
            r#"
-module(main).
-define(USED_MACRO, used_macro).
-define(UNUSED_MACRO, unused_macro).
     %% ^^^^^^^^^^^^ 💡 warning: W0002: Unused macro (UNUSED_MACRO)
-define(UNUSED_MACRO_WITH_ARG(C), C).
     %% ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0002: Unused macro (UNUSED_MACRO_WITH_ARG/1)

main() ->
  ?MOD:foo(),
  ?USED_MACRO.
            "#,
        );
    }

    #[test]
    fn test_unused_macro_dynamic_call() {
        // Ported from issue #1021 in Erlang LS
        check_diagnostics(
            r#"
-module(main).
-define(MOD, module). %% MOD
main() ->
  ?MOD:foo().
            "#,
        );
    }

    #[test]
    fn test_unused_macro_include() {
        check_diagnostics(
            r#"
//- /src/foo.hrl
-define(A, a).
-define(B, b).
//- /src/foo.erl
-module(foo).
-include("foo.hrl").
-define(BAR, 42).
     %% ^^^ 💡 warning: W0002: Unused macro (BAR)
main() ->
  ?A.
        "#,
        );
    }
}
