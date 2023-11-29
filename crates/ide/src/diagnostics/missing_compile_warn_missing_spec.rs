/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint/fix: missing_compile_warn_missing_spec
//!
//! Return a diagnostic if a the file does not have
//! `warn_missing_spec(_all)` in a compile attribute
//! Add this as a fix.
//!

use elp_ide_assists::helpers::add_compile_option;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_syntax::AstNode;
use fxhash::FxHashSet;
use hir::known;
use hir::AnyExpr;
use hir::FoldCtx;
use hir::FormIdx;
use hir::InFile;
use hir::Literal;
use hir::Name;
use hir::Semantic;
use hir::Strategy;
use hir::Term;
use lazy_static::lazy_static;
use regex::Regex;
use text_edit::TextRange;

use super::Diagnostic;
use crate::fix;

pub(crate) fn missing_compile_warn_missing_spec(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
    if sema.db.is_generated(file_id) || Some(false) == is_in_src_dir(sema.db.upcast(), file_id) {
        return;
    }
    let form_list = sema.form_list(file_id);
    if form_list.compile_attributes().next().is_none() {
        report_diagnostic(sema, None, file_id, diags);
    }
    let attributes = form_list
        .compile_attributes()
        .map(|(idx, compile_attribute)| {
            let co = sema.db.compile_body(InFile::new(file_id, idx));
            let is_present = FoldCtx::fold_term(
                &co.body,
                Strategy::InvisibleMacros,
                FormIdx::CompileOption(idx),
                co.value,
                false,
                &mut |acc, ctx| match &ctx.item {
                    AnyExpr::Term(Term::Literal(Literal::Atom(atom))) => {
                        let name = sema.db.lookup_atom(*atom);
                        if MISSING_SPEC_OPTIONS.contains(&name) {
                            true
                        } else {
                            acc
                        }
                    }
                    _ => acc,
                },
            );
            (is_present, compile_attribute)
        })
        .collect::<Vec<_>>();
    if !attributes.iter().any(|(present, _)| *present) {
        // Report on first compile attribute only
        if let Some((_, compile_attribute)) = attributes.first() {
            let range = compile_attribute
                .form_id
                .get_ast(sema.db, file_id)
                .syntax()
                .text_range();
            report_diagnostic(sema, Some(range), file_id, diags)
        }
    }
}

lazy_static! {
    static ref MISSING_SPEC_OPTIONS: FxHashSet<Name> = {
        let mut res = FxHashSet::default();
        for name in vec![
            known::warn_missing_spec,
            known::nowarn_missing_spec,
            known::warn_missing_spec_all,
            known::nowarn_missing_spec_all,
        ] {
            res.insert(name);
        }
        res
    };
}

fn is_in_src_dir(db: &dyn SourceDatabase, file_id: FileId) -> Option<bool> {
    let root_id = db.file_source_root(file_id);
    let root = db.source_root(root_id);
    let path = root.path_for_file(&file_id)?.as_path()?.as_ref().to_str()?;
    Some(is_srcdir_path(path))
}

fn is_srcdir_path(s: &str) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^.*/erl/[^/]+/src/.*\.erl$").unwrap();
    }
    RE.is_match(s)
}

fn report_diagnostic(
    sema: &Semantic,
    range: Option<TextRange>,
    file_id: FileId,
    diags: &mut Vec<Diagnostic>,
) {
    let range = range.unwrap_or(TextRange::empty(0.into()));

    let mut builder = SourceChangeBuilder::new(file_id);
    add_compile_option(sema, file_id, "warn_missing_spec", None, &mut builder);
    let edit = builder.finish();
    let d = Diagnostic::new(
        crate::diagnostics::DiagnosticCode::MissingCompileWarnMissingSpec,
            "Please add \"-compile(warn_missing_spec).\" or \"-compile(warn_missing_spec_all).\" to the module. If exported functions are not all specced, they need to be specced.".to_string(),
        range,
    ).with_fixes(Some(vec![fix("add_warn_missing_spec",
                               "Add compile option 'warn_missing_spec'",
                               edit, range)]));
    diags.push(d);
}

#[cfg(test)]
mod tests {

    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix_with_config;

    #[track_caller]
    pub(crate) fn check_fix(fixture_before: &str, fixture_after: &str) {
        let config = DiagnosticsConfig::default();
        check_fix_with_config(config, fixture_before, fixture_after)
    }

    #[track_caller]
    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default();
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn no_compile_attribute() {
        check_diagnostics(
            r#"
            //- /erl/my_app/src/main.erl
            %% <<< 💡 error: Please add "-compile(warn_missing_spec)." or "-compile(warn_missing_spec_all)." to the module. If exported functions are not all specced, they need to be specced.

            -module(main).

            "#,
        )
    }

    #[test]
    fn compile_attribute_missing_setting() {
        check_diagnostics(
            r#"
         //- /erl/my_app/src/main.erl
            -module(main).

            -compile([export_all, nowarn_export_all]).
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 error: Please add "-compile(warn_missing_spec)." or "-compile(warn_missing_spec_all)." to the module. If exported functions are not all specced, they need to be specced.

            "#,
        )
    }

    #[test]
    fn warn_missing_spec_ok() {
        check_diagnostics(
            r#"
            //- /erl/my_app/src/main.erl
            -module(main).

            -compile(warn_missing_spec).

            "#,
        )
    }

    #[test]
    fn nowarn_missing_spec_ok() {
        check_diagnostics(
            r#"
            //- /erl/my_app/src/main.erl
            -module(main).

            -compile(nowarn_missing_spec).

            "#,
        )
    }

    #[test]
    fn warn_missing_spec_all_ok() {
        check_diagnostics(
            r#"
            //- /erl/my_app/src/main.erl
            -module(main).

            -compile(warn_missing_spec_all).

            "#,
        )
    }

    #[test]
    fn nowarn_missing_spec_all_ok() {
        check_diagnostics(
            r#"
            //- /erl/my_app/src/main.erl
            -module(main).

            -compile(nowarn_missing_spec_all).

            "#,
        )
    }

    #[test]
    fn more_than_one_compile_attribute_1() {
        check_diagnostics(
            r#"
            //- /erl/my_app/src/main.erl
            -module(main).

            -compile(warn_missing_spec).
            -compile([export_all, nowarn_export_all]).
            "#,
        )
    }

    #[test]
    fn more_than_one_compile_attribute_2() {
        check_diagnostics(
            r#"
         //- /erl/my_app/src/main.erl
            -module(main).

            -compile(export_all).
         %% ^^^^^^^^^^^^^^^^^^^^^ 💡 error: Please add "-compile(warn_missing_spec)." or "-compile(warn_missing_spec_all)." to the module. If exported functions are not all specced, they need to be specced.
            -compile(nowarn_export_all).
            "#,
        )
    }

    #[test]
    fn more_than_one_compile_attribute_3() {
        check_diagnostics(
            r#"
            //- /erl/my_app/src/main.erl
            -module(main).
            -compile({nowarn_deprecated_function, {erlang,get_stacktrace,0}}).
            -compile([
                warn_missing_spec_all,
                export_all,
                nowarn_export_all
                ]).

            "#,
        )
    }

    #[test]
    fn not_in_generated_file() {
        check_diagnostics(
            r#"
            //- /erl/my_app/src/main.erl
            %% -*- coding: utf-8 -*-
            %% Automatically generated, do not edit
            %% @generated from blah
            %% To generate, see targets and instructions in local Makefile
            %% Version source: git
            -module(main).
            -eqwalizer(ignore).

            "#,
        )
    }

    #[test]
    fn not_in_test_or_extra_file() {
        check_diagnostics(
            r#"
            //- /erl/my_app/test/my_SUITE.erl extra:test
               -module(my_SUITE).
               -export([all/0]).
               -export([a/1]).
               all() -> [a].
               a(_Config) ->
                 ok.
            "#,
        )
    }

    #[test]
    fn applies_fix_no_attribute() {
        check_fix(
            r#"
         //- /erl/my_app/src/main.erl
         ~-module(main).

         %% a comment"#,
            r#"
            -module(main).

            -compile([warn_missing_spec]).

            %% a comment"#,
        );
    }

    #[test]
    fn applies_fix_existing_attribute_list() {
        check_fix(
            r#"
            //- /erl/my_app/src/main.erl
            -module(main).

            -c~ompile([export_all, nowarn_export_all]).

            "#,
            r#"
            -module(main).

            -compile([export_all, nowarn_export_all, warn_missing_spec]).

            "#,
        );
    }

    #[test]
    fn applies_fix_existing_attribute_atom() {
        check_fix(
            r#"
            //- /erl/my_app/src/main.erl
            -module(main).

            -c~ompile(export_all).

            "#,
            r#"
            -module(main).

            -compile([export_all, warn_missing_spec]).

            "#,
        );
    }

    #[test]
    fn applies_fix_existing_attribute_tuple() {
        check_fix(
            r#"
            //- /erl/my_app/src/main.erl
            -module(main).

            -c~ompile({foo, bar}).

            "#,
            r#"
            -module(main).

            -compile([{foo, bar}, warn_missing_spec]).

            "#,
        );
    }
}
