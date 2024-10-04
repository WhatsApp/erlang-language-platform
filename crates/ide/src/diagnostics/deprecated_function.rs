/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Diagnostic: deprecated-function
//
// Return a warning if a function is marked as deprecated.
// This functionality is similar to the one provided by the XRef tool which comes with OTP.
// In fact, it leverages the same `-deprecated` attribute that XRef uses. The attribute
// allows the user to specify a "third" field to explain the reason of a deprecation, but
// XRef itself ignores that field, which is intended to be used by other tools.
// This diagnostic does just that and shows the message at the call-site.
// It also provides a mechanism to augment deprecation information with extra information, such as a URI.

use elp_ide_assists::Assist;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_syntax::AstNode;
use hir::AnyExpr;
use hir::Expr;
use hir::FunctionDef;
use hir::Semantic;
use hir::Strategy;
use lazy_static::lazy_static;
use text_edit::TextEdit;
use text_edit::TextRange;
use text_edit::TextSize;

use super::Diagnostic;
use super::DiagnosticCode;
use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use super::Severity;
use crate::codemod_helpers::FunctionMatch;
use crate::codemod_helpers::FunctionMatcher;
// @fb-only
use crate::fix;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: true,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, _ext| {
        deprecated_function(diags, sema, file_id);
    },
};

#[derive(Debug, Clone)]
pub struct DeprecationDetails {
    severity: Severity,
    uri: Option<String>,
    message: Option<String>,
}

#[allow(dead_code)] // @oss-only
impl DeprecationDetails {
    pub fn new() -> Self {
        DeprecationDetails {
            severity: Severity::Warning,
            uri: None,
            message: None,
        }
    }

    pub fn with_uri(mut self, uri: Option<String>) -> Self {
        self.uri = uri;
        self
    }

    pub fn with_message(mut self, message: Option<String>) -> Self {
        self.message = message;
        self
    }
}

fn deprecated_function(diagnostics: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    lazy_static! {
        static ref DEPRECATED_FUNCTIONS: Vec<(FunctionMatch, DeprecationDetails)> = {
            let matches: Vec<Vec<(FunctionMatch, DeprecationDetails)>>  = vec![
                // @fb-only
            ];
            matches.into_iter()
        .flatten()
        .collect::<Vec<_>>()
        };
    }
    let matches = &DEPRECATED_FUNCTIONS;
    let matches = matches
        .iter()
        .map(|(m, d)| (m, d.clone()))
        .collect::<Vec<_>>();
    sema.for_each_function(file_id, |def| {
        check_function(diagnostics, sema, def, &matches)
    });
}

fn check_function(
    diagnostics: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    matches: &[(&FunctionMatch, DeprecationDetails)],
) {
    let matcher = FunctionMatcher::new(matches);
    let def_fb = def.in_function_body(sema, def);
    def_fb
        .clone()
        .fold_function(Strategy::VisibleMacros, (), &mut |acc, clause_id, ctx| {
            if let AnyExpr::Expr(Expr::Call { target, args }) = ctx.item {
                let arity = args.len() as u32;
                if let Some(target_def) =
                    target.resolve_call(arity, sema, def_fb.file_id(), &def_fb.body(clause_id))
                {
                    let match_result = matcher.get_match(
                        &target,
                        args.len() as u32,
                        Some(&args),
                        sema,
                        &def_fb.body(clause_id),
                    );
                    let details = match_result.map(|(_match, details)| details.clone());
                    if target_def.deprecated || match_result.is_some() {
                        let expr_id = if let Some(expr_id) = ctx.in_macro {
                            expr_id.idx
                        } else {
                            ctx.item_id
                        };
                        if let Some(range) = def_fb.range_for_any(clause_id, expr_id) {
                            let d = make_diagnostic(range, &target_def, details)
                                .with_fixes(Some(vec![fix_xref_ignore(
                                    sema,
                                    def_fb.file_id(),
                                    &target_def,
                                    range,
                                )]))
                                .with_ignore_fix(sema, def_fb.file_id());
                            diagnostics.push(d)
                        }
                    }
                }
            };
            acc
        });
}

fn make_diagnostic(
    range: TextRange,
    def: &FunctionDef,
    details: Option<DeprecationDetails>,
) -> Diagnostic {
    let base_message = format!("Function '{}' is deprecated.", def.name);
    let base_message = match &def.deprecated_desc {
        Some(desc) => {
            let desc = desc.to_string();
            let desc = strip_quotes(desc.as_str());
            format!("{base_message}\n{desc}")
        }
        None => base_message,
    };
    let (severity, uri, message) = match details {
        Some(DeprecationDetails {
            severity,
            uri,
            message,
        }) => {
            let message = match message {
                Some(message) => format!("{base_message}\n{message}"),
                None => base_message,
            };
            (severity, uri, message)
        }
        None => (Severity::Warning, None, base_message),
    };
    Diagnostic::new(DiagnosticCode::DeprecatedFunction, message, range)
        .with_severity(severity)
        .with_uri(uri)
        .experimental()
}

fn strip_quotes(s: &str) -> &str {
    let quote = "\"";
    let s = s.strip_prefix(quote).unwrap_or(s);
    s.strip_suffix(quote).unwrap_or(s)
}

fn fix_xref_ignore(
    sema: &Semantic,
    file_id: FileId,
    def: &FunctionDef,
    range: TextRange,
) -> Assist {
    let source = sema.parse(file_id).value;
    let form_list = sema.form_list(file_id);

    let offset = if let Some(module_attr) = form_list.module_attribute() {
        let module_attr_range = module_attr.form_id.get(&source).syntax().text_range();
        module_attr_range.end() + TextSize::from(1)
    } else {
        TextSize::from(0)
    };

    let module = sema.module_name(def.file.file_id).unwrap();

    let text = format!(
        "-ignore_xref([{{{}, {}, {}}}]).\n",
        module.as_str(),
        def.name.name(),
        def.name.arity()
    );
    let mut edit_builder = TextEdit::builder();
    edit_builder.insert(offset, text);
    let edit = edit_builder.finish();
    let source_change = SourceChange::from_text_edit(file_id, edit);

    fix(
        "xref_ignore",
        "Add xref ignore for all calls to this function",
        source_change,
        range,
    )
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn test_deprecated_function_local() {
        check_diagnostics(
            r#"
  -module(main).
  -deprecated({not_ok_to_use, 0}).
  not_ok_to_use() ->
    ok.
  main() ->
    not_ok_to_use().
%%  ^^^^^^^^^^^^^^^ 💡 warning: Function 'not_ok_to_use/0' is deprecated.
            "#,
        )
    }

    #[test]
    fn test_deprecated_function_remote() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([not_ok_to_use/0]).
  -deprecated({not_ok_to_use, 0}).
  not_ok_to_use() ->
    ok.
//- /src/a.erl
  -module(a).

  main() ->
    b:not_ok_to_use().
%%  ^^^^^^^^^^^^^^^^^ 💡 warning: Function 'not_ok_to_use/0' is deprecated.
            "#,
        )
    }

    #[test]
    fn test_deprecated_function_in_macro() {
        check_diagnostics(
            r#"
  -module(main).
  -define(LAZY, fun(X) -> X end).
  -deprecated({do, 0}).
  main() ->
    do(),
%%  ^^^^ 💡 warning: Function 'do/0' is deprecated.
    ?LAZY(do()).
%%  ^^^^^^^^^^^ 💡 warning: Function 'do/0' is deprecated.
  do() ->
    ok.
            "#,
        )
    }

    #[test]
    fn test_deprecated_function_description() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([not_ok_to_use/0]).
  -deprecated({not_ok_to_use, 0, "Cause I said so."}).
  not_ok_to_use() ->
    ok.
//- /src/a.erl
  -module(a).

  main() ->
    b:not_ok_to_use().
%%  ^^^^^^^^^^^^^^^^^ 💡 warning: Function 'not_ok_to_use/0' is deprecated.
%%                  | Cause I said so.
            "#,
        )
    }

    #[test]
    fn test_xref_ignore_fix() {
        check_fix(
            r#"
//- /src/b.erl
-module(b).
-export([not_ok_to_use/0]).
-deprecated({not_ok_to_use, 0, "Cause I said so."}).
not_ok_to_use() ->
  ok.

//- /src/a.erl
-module(a).

main() ->
  b:no~t_ok_to_use().
"#,
            expect![[r#"
-module(a).
-ignore_xref([{b, not_ok_to_use, 0}]).

main() ->
  b:not_ok_to_use().
"#]],
        )
    }
}
