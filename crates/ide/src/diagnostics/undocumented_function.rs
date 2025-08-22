/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_assists::helpers::unwrap_parens;
// Diagnostic: undocumented-function
use elp_ide_db::elp_base_db::FileId;
use elp_syntax::ast;
use elp_syntax::ast::Atom;
use fxhash::FxHashSet;
use hir::AsName;
use hir::FunctionDef;
use hir::NameArity;
use hir::Semantic;
use hir::form_list::ModuleDocAttribute;
use hir::known;

use super::Diagnostic;
use super::DiagnosticCode;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;

const DIAGNOSTIC_CODE: DiagnosticCode = DiagnosticCode::UndocumentedFunction;
const DIAGNOSTIC_MESSAGE: &str = "The function is non-trivial, exported, but not documented.";
const DIAGNOSTIC_SEVERITY: Severity = Severity::WeakWarning;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: false,
        include_tests: false,
        default_disabled: true,
    },
    checker: &|diags, sema, file_id, _ext| {
        check(diags, sema, file_id);
    },
};

fn check(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    let callbacks = sema.resolve_callbacks(file_id);
    if !contains_moduledoc_hidden_attribute(sema, file_id) {
        sema.def_map_local(file_id)
            .get_functions()
            .for_each(|(_arity, def)| check_function(diagnostics, sema, def, &callbacks));
    }
}

fn contains_moduledoc_hidden_attribute(sema: &Semantic, file_id: FileId) -> bool {
    sema.form_list(file_id)
        .moduledoc_attributes()
        .any(|(_idx, attribute)| is_hidden_moduledoc(sema, file_id, attribute) == Some(true))
}

fn is_hidden_moduledoc(
    sema: &Semantic,
    file_id: FileId,
    attribute: &ModuleDocAttribute,
) -> Option<bool> {
    let value = &attribute.form_id.get_ast(sema.db, file_id).value()?;
    let value = &unwrap_parens(value)?;
    match value {
        ast::Expr::ExprMax(ast::ExprMax::Atom(atom)) => Some(is_hidden_or_false(atom)),
        _ => None,
    }
}

fn is_hidden_or_false(atom: &Atom) -> bool {
    atom.as_name() == known::hidden || atom.as_name() == known::false_name
}

fn function_should_be_checked(
    sema: &Semantic,
    def: &FunctionDef,
    callbacks: &FxHashSet<NameArity>,
) -> bool {
    const CODE_COMPLEXITY_CAP: usize = 5;
    def.exported
        && !callbacks.contains(&def.name)
        && def.code_complexity(sema, Some(CODE_COMPLEXITY_CAP)).score >= CODE_COMPLEXITY_CAP
}

fn check_function(
    diagnostics: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    callbacks: &FxHashSet<NameArity>,
) {
    if function_should_be_checked(sema, def, callbacks)
        && def.doc_id.is_none()
        && def.doc_metadata_id.is_none()
        && def.edoc_comments(sema.db).is_none()
        && let Some(name_range) = def.name_range(sema.db)
    {
        let diagnostic = Diagnostic::new(DIAGNOSTIC_CODE, DIAGNOSTIC_MESSAGE, name_range)
            .with_severity(DIAGNOSTIC_SEVERITY);
        diagnostics.push(diagnostic);
    }
}

#[cfg(test)]
mod tests {

    use elp_ide_db::DiagnosticCode;

    use crate::DiagnosticsConfig;
    use crate::tests;

    fn config() -> DiagnosticsConfig {
        DiagnosticsConfig::default()
            .enable(DiagnosticCode::UndocumentedFunction)
            .disable(DiagnosticCode::OldEdocSyntax)
    }

    fn check_diagnostics(fixture: &str) {
        tests::check_diagnostics_with_config(config(), fixture);
    }

    #[test]
    fn test_exported_function() {
        check_diagnostics(
            r#"
    -module(main).
    -export([main/0]).
    main() ->
 %% ^^^^ weak: The function is non-trivial, exported, but not documented.
      [ok,
       ok,
       ok,
       ok,
       ok].
        "#,
        )
    }

    #[test]
    fn test_only_exported_function() {
        check_diagnostics(
            r#"
    -module(main).
    -export([main/0]).
    main() ->
 %% ^^^^ weak: The function is non-trivial, exported, but not documented.
      [ok,
       ok,
       ok,
       ok,
       ok].

    local() ->
      [ok,
       ok,
       ok,
       ok,
       ok].
        "#,
        )
    }

    #[test]
    fn test_exported_function_with_doc_attribute() {
        check_diagnostics(
            r#"
    -module(main).
    -export([main/0]).
    -doc """
    Some documentation
    """.
    main() ->
      [ok,
       ok,
       ok,
       ok,
       ok].
        "#,
        )
    }

    #[test]
    fn test_exported_function_with_doc_hidden() {
        check_diagnostics(
            r#"
    -module(main).
    -export([main/0]).
    -doc hidden.
    main() ->
      [ok,
       ok,
       ok,
       ok,
       ok].
        "#,
        )
    }

    #[test]
    fn test_exported_function_with_doc_false() {
        check_diagnostics(
            r#"
    -module(main).
    -export([main/0]).
    -doc false.
    main() ->
      [ok,
       ok,
       ok,
       ok,
       ok].
        "#,
        )
    }

    #[test]
    fn test_exported_function_with_edoc() {
        check_diagnostics(
            r#"
    -module(main).
    -export([main/0]).
    %% @doc Some documentation
    main() ->
      [ok,
       ok,
       ok,
       ok,
       ok].
        "#,
        )
    }

    #[test]
    fn test_exported_function_with_moduledoc_false() {
        check_diagnostics(
            r#"
    -module(main).
    -moduledoc false.
    -export([main/0]).
    
    main() ->
      [ok,
       ok,
       ok,
       ok,
       ok].
        "#,
        )
    }

    #[test]
    fn test_exported_function_with_moduledoc_false_parentheses() {
        check_diagnostics(
            r#"
    -module(main).
    -moduledoc(false).
    -export([main/0]).
    
    main() ->
      [ok,
       ok,
       ok,
       ok,
       ok].
        "#,
        )
    }

    #[test]
    fn test_exported_function_with_moduledoc_hidden() {
        check_diagnostics(
            r#"
    -module(main).
    -moduledoc hidden.
    -export([main/0]).
    
    main() ->
      [ok,
       ok,
       ok,
       ok,
       ok].
        "#,
        )
    }

    #[test]
    fn test_exported_function_with_moduledoc_hidden_parentheses() {
        check_diagnostics(
            r#"
    -module(main).
    -moduledoc(hidden).
    -export([main/0]).
    
    main() ->
      [ok,
       ok,
       ok,
       ok,
       ok].
        "#,
        )
    }

    #[test]
    fn test_exported_function_callback() {
        check_diagnostics(
            r#"
    //- /src/main.erl
    -module(main).
    -behaviour(gen_server).
    -export([main/0]).
    -export([handle_call/1]).
    
    main() ->
    %%<^ weak: The function is non-trivial, exported, but not documented.
      [ok,
       ok,
       ok,
       ok,
       ok].

    handle_call(_X) ->
      [ok,
       ok,
       ok,
       ok,
       ok].

    //- /src/gen_server.erl
    -module(gen_server).
    -callback handle_call(term()) -> term().
        "#,
        )
    }

    #[test]
    fn test_exported_function_simple_function() {
        check_diagnostics(
            r#"
    -module(main).
    -export([simple/0, complex/0]).
    
    simple() ->
      ok.

    complex() ->
    %%<^^^^ weak: The function is non-trivial, exported, but not documented.
      [ok,
       ok,
       ok,
       ok,
       ok].
        "#,
        )
    }

    #[test]
    fn test_exported_function_complex_function_multiple_clauses() {
        check_diagnostics(
            r#"
    -module(main).
    -export([simple/0, complex/1]).
    
    simple() ->
      ok.

    complex(a) ->
    %%<^^^^ weak: The function is non-trivial, exported, but not documented.
      [ok];
    complex(b) ->
      [ok,
       ok].
        "#,
        )
    }
}
