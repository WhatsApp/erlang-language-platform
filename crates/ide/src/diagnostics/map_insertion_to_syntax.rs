/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint: map_function_to_syntax
//!
//! offer to rewrite basic maps function calls to the equivalent syntax
//! e.g. maps:put(K, V, M) becomes M#{K => V}
//!      maps:update(K, V, M) becomes M#{K := V}

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_db::DiagnosticCode;
use elp_ide_ssr::match_pattern_in_file;
use elp_ide_ssr::Match;
use elp_ide_ssr::PlaceholderMatch;
use elp_ide_ssr::SubId;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::Strategy;
use hir::AnyExprId;
use hir::Expr;
use hir::Semantic;

use crate::diagnostics::Category;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticConditions;
use crate::diagnostics::DiagnosticDescriptor;
use crate::diagnostics::Severity;
use crate::fix;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|acc, sema, file_id, _ext| {
        map_put_to_syntax_ssr(acc, sema, file_id);
        map_update_to_syntax_ssr(acc, sema, file_id);
    },
};

static KEY_VAR: &str = "_@Key";
static VALUE_VAR: &str = "_@Value";
static MAP_VAR: &str = "_@Map";

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum MapInsertionFunction {
    Put,
    Update,
}

impl MapInsertionFunction {
    pub fn to_fix_id(&self) -> &'static str {
        match self {
            MapInsertionFunction::Put => "maps_put_function_rather_than_syntax",
            MapInsertionFunction::Update => "maps_update_function_rather_than_syntax",
        }
    }
    pub fn to_fix_label(&self) -> &'static str {
        match self {
            MapInsertionFunction::Put => "Rewrite to use map put syntax",
            MapInsertionFunction::Update => "Rewrite to use map update syntax",
        }
    }
    pub fn to_code(&self) -> DiagnosticCode {
        match self {
            MapInsertionFunction::Put => DiagnosticCode::MapsPutFunctionRatherThanSyntax,
            MapInsertionFunction::Update => DiagnosticCode::MapsUpdateFunctionRatherThanSyntax,
        }
    }
    pub fn to_replacement_str(&self, sema: &Semantic, m: &Match) -> Option<String> {
        let map = m.placeholder_text(sema, MAP_VAR)?;
        let key = m.placeholder_text(sema, KEY_VAR)?;
        let value = m.placeholder_text(sema, VALUE_VAR)?;
        Some(match self {
            MapInsertionFunction::Put => format!("{map}#{{{key} => {value}}}",),
            MapInsertionFunction::Update => format!("{map}#{{{key} := {value}}}"),
        })
    }
}

fn map_put_to_syntax_ssr(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    let matches = match_pattern_in_file(
        sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        file_id,
        format!("ssr: maps:put({},{},{}).", KEY_VAR, VALUE_VAR, MAP_VAR).as_str(),
    );
    matches.matches.iter().for_each(|m| {
        if let Some(map_match) = m.get_placeholder_match(sema, MAP_VAR) {
            if is_var(sema, m, map_match) {
                if let Some(diagnostic) = make_diagnostic(sema, m, MapInsertionFunction::Put) {
                    diags.push(diagnostic)
                }
            }
        }
    });
}

fn map_update_to_syntax_ssr(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    let matches = match_pattern_in_file(
        sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        file_id,
        format!("ssr: maps:update({},{},{}).", KEY_VAR, VALUE_VAR, MAP_VAR).as_str(),
    );
    matches.matches.iter().for_each(|m| {
        if let Some(map_match) = m.get_placeholder_match(sema, MAP_VAR) {
            if is_var(sema, m, map_match) {
                if let Some(diagnostic) = make_diagnostic(sema, m, MapInsertionFunction::Update) {
                    diags.push(diagnostic)
                }
            }
        }
    });
}

fn is_var(sema: &Semantic, m: &Match, matched: PlaceholderMatch) -> bool {
    let body = &m.matched_node_body.get_body(sema);
    body.as_ref().map_or(false, |b| match matched.code_id {
        SubId::AnyExprId(AnyExprId::Expr(expr_id)) => match b[expr_id] {
            Expr::Var(_) => true,
            _ => false,
        },
        _ => false,
    })
}

fn make_diagnostic(
    sema: &Semantic,
    matched: &Match,
    op: MapInsertionFunction,
) -> Option<Diagnostic> {
    let file_id = matched.range.file_id;
    let map_function_call_range = matched.range.range;
    let message = "Consider using map syntax rather than a function call.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    let map_syntax = op.to_replacement_str(sema, matched)?;
    builder.replace(map_function_call_range, map_syntax);
    let fixes = vec![fix(
        op.to_fix_id(),
        op.to_fix_label(),
        builder.finish(),
        map_function_call_range,
    )];
    Some(
        Diagnostic::new(op.to_code(), message, map_function_call_range)
            .with_severity(Severity::Information)
            .with_ignore_fix(sema, file_id)
            .with_fixes(Some(fixes))
            .add_categories([Category::SimplificationRule]),
    )
}

#[cfg(test)]
mod tests {

    use expect_test::expect;
    use expect_test::Expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::MapsPutFunctionRatherThanSyntax
            || d.code == DiagnosticCode::MapsUpdateFunctionRatherThanSyntax
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_filtered_diagnostics(fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        tests::check_fix(fixture_before, fixture_after)
    }

    #[test]
    fn ignores_map_put_function_when_map_is_not_a_var() {
        check_diagnostics(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V) -> maps:put(K, V, #{}).
            "#,
        )
    }

    #[test]
    fn ignores_map_update_function_when_map_is_not_a_var() {
        check_diagnostics(
            r#"
         //- /src/map_update_function.erl
         -module(map_update_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V) -> maps:update(K, V, #{a => b}).
            "#,
        )
    }

    #[test]
    fn detects_map_put_function() {
        check_diagnostics(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> maps:put(K, V, Map).
         %%               ^^^^^^^^^^^^^^^^^^^💡 information: Consider using map syntax rather than a function call.
            "#,
        )
    }

    #[test]
    fn detects_map_update_function() {
        check_diagnostics(
            r#"
         //- /src/map_update_function.erl
         -module(map_update_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> maps:update(K, V, Map).
         %%               ^^^^^^^^^^^^^^^^^^^^^^💡 information: Consider using map syntax rather than a function call.
            "#,
        )
    }

    #[test]
    fn rewrites_map_put_function_to_use_syntax() {
        check_fix(
            r#"
         //- /src/map_put_function.erl
         -module(map_put_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> maps:p~ut(K, V, Map).
            "#,
            expect![[r#"
         -module(map_put_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> Map#{K => V}.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_update_function_to_use_syntax() {
        check_fix(
            r#"
         //- /src/map_update_function.erl
         -module(map_update_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> maps:up~date(K, V, Map).
            "#,
            expect![[r#"
         -module(map_update_function).

         % elp:ignore W0017 (undefined_function)
         fn(K, V, Map) -> Map#{K := V}.
            "#]],
        )
    }
}
