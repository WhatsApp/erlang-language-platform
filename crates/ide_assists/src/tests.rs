/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;

use elp_ide_db::assists::AssistContextDiagnostic;
use elp_ide_db::assists::AssistContextDiagnosticCode;
use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_ide_db::assists::AssistUserInput;
use elp_ide_db::assists::AssistUserInputType;
use elp_ide_db::elp_base_db::fixture::extract_annotations;
use elp_ide_db::elp_base_db::fixture::remove_annotations;
use elp_ide_db::elp_base_db::fixture::WithFixture;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide_db::helpers::SnippetCap;
use elp_ide_db::source_change::FileSystemEdit;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_db::RootDatabase;
use elp_ide_db::SymbolClass;
use elp_ide_db::SymbolDefinition;
use elp_syntax::ast;
use elp_syntax::SourceFile;
use expect_test::expect;
use expect_test::Expect;
use hir::Expr;
use hir::FunctionDefId;
use hir::InFile;
use hir::Semantic;
use stdx::format_to;

use crate::handlers::Handler;
use crate::helpers;
use crate::AssistConfig;
use crate::AssistContext;
use crate::AssistResolveStrategy;
use crate::Assists;

pub(crate) const TEST_CONFIG: AssistConfig = AssistConfig {
    snippet_cap: SnippetCap::new(true),
    allowed: None,
};

#[track_caller]
pub(crate) fn check_assist(
    assist: Handler,
    assist_label: &str,
    fixture_before: &str,
    fixture_after: Expect,
) {
    check(
        assist,
        fixture_before,
        ExpectedResult::After(fixture_after),
        Some(assist_label),
        true,
        None,
    );
}

#[track_caller]
pub(crate) fn check_assist_with_user_input(
    assist: Handler,
    assist_label: &str,
    user_input: &str,
    fixture_before: &str,
    fixture_after: Expect,
) {
    check(
        assist,
        fixture_before,
        ExpectedResult::After(fixture_after),
        Some(assist_label),
        true,
        Some(user_input),
    );
}

#[track_caller]
// We allow dead code because this function is used while debugging tests
#[allow(dead_code)]
pub(crate) fn check_assist_expect_parse_error(
    assist: Handler,
    assist_label: &str,
    fixture_before: &str,
    fixture_after: Expect,
) {
    check(
        assist,
        fixture_before,
        ExpectedResult::After(fixture_after),
        Some(assist_label),
        false,
        None,
    );
}

#[track_caller]
pub(crate) fn check_assist_not_applicable(assist: Handler, ra_fixture: &str) {
    check(
        assist,
        ra_fixture,
        ExpectedResult::NotApplicable,
        None,
        true,
        None,
    );
}

enum ExpectedResult {
    NotApplicable,
    After(Expect),
}

pub const SNIPPET_CURSOR_MARKER: &str = "$0";

#[track_caller]
fn check(
    handler: Handler,
    before: &str,
    expected: ExpectedResult,
    assist_label: Option<&str>,
    check_parse_error: bool,
    user_input: Option<&str>,
) {
    let (db, file_with_caret_id, range_or_offset) = RootDatabase::with_range_or_offset(before);

    let frange = FileRange {
        file_id: file_with_caret_id,
        range: range_or_offset.into(),
    };

    let sema = &db;
    let config = TEST_CONFIG;
    let context_diagnostics = extract_annotations(&db.file_text(file_with_caret_id));
    let mut diagnostics = vec![];
    for (range, text) in &context_diagnostics {
        if let Some((code_and_bulb, message)) = text.split_once(':') {
            if let Some(code_string) = code_and_bulb.strip_prefix("💡 ") {
                if let Ok(code) = AssistContextDiagnosticCode::from_str(code_string) {
                    let d = AssistContextDiagnostic::new(code, message.trim().to_string(), *range);
                    diagnostics.push(d)
                }
            }
        }
    }

    // Check that the fixture is syntactically valid
    if check_parse_error {
        // Check that we have a syntactically valid starting point
        let text = sema.file_text(frange.file_id);
        let parse = sema.parse(frange.file_id);
        let errors = parse.errors();
        if !errors.is_empty() {
            assert_eq!(format!("{:?}\nin\n{text}", errors), "");
        }
    }

    let mut ctx = AssistContext::new(sema, &config, frange, &diagnostics, None);
    let resolve = AssistResolveStrategy::All;
    let mut acc = Assists::new(&ctx, resolve.clone());
    handler(&mut acc, &ctx);
    let mut res = acc.finish();
    if let Some(requested_user_input) = res.get(0).and_then(|a| a.user_input.clone()) {
        let value = if let Some(input) = user_input {
            input.to_string()
        } else {
            match requested_user_input.input_type {
                AssistUserInputType::Variable => {
                    format!("{}Edited", requested_user_input.value)
                }
                AssistUserInputType::Atom => {
                    format!("{}_edited", requested_user_input.value)
                }
            }
        };
        ctx.user_input = Some(AssistUserInput {
            input_type: requested_user_input.input_type,
            value,
        });
        // Resolve the assist, with the edited result
        let mut acc = Assists::new(&ctx, resolve);
        handler(&mut acc, &ctx);
        res = acc.finish();
    }

    let assist = match assist_label {
        Some(label) => res
            .clone()
            .into_iter()
            .find(|resolved| resolved.label == label),
        None => res.clone().pop(),
    };

    match (assist, expected) {
        (Some(assist), ExpectedResult::After(after)) => {
            let source_change = assist
                .source_change
                .expect("Assist did not contain any source changes");
            assert!(!source_change.source_file_edits.is_empty());
            let skip_header = source_change.source_file_edits.len() == 1
                && source_change.file_system_edits.is_empty();

            let mut buf = String::new();
            for (file_id, edit) in source_change.source_file_edits {
                let mut text = db.file_text(file_id).as_ref().to_owned();
                edit.apply(&mut text);
                if !skip_header {
                    let sr = db.file_source_root(file_id);
                    let sr = db.source_root(sr);
                    let path = sr.path_for_file(&file_id).unwrap();
                    format_to!(buf, "//- {}\n", path)
                }
                buf.push_str(&text);
            }

            for file_system_edit in source_change.file_system_edits {
                if let FileSystemEdit::CreateFile {
                    dst,
                    initial_contents,
                } = file_system_edit
                {
                    let sr = db.file_source_root(dst.anchor);
                    let sr = db.source_root(sr);
                    let mut base = sr.path_for_file(&dst.anchor).unwrap().clone();
                    base.pop();
                    let created_file_path = format!("{}{}", base, &dst.path[1..]);
                    format_to!(buf, "//- {}\n", created_file_path);
                    buf.push_str(&initial_contents);
                }
            }

            if check_parse_error {
                // Check that we have introduced a syntactically valid result
                let text = remove_annotations(Some(SNIPPET_CURSOR_MARKER), &buf);
                let parse = SourceFile::parse_text(&text);
                let errors = parse.errors();
                if !errors.is_empty() {
                    assert_eq!(format!("{:?}\nin\n{text}", errors), "");
                }
            }

            after.assert_eq(&remove_annotations(None, &buf));
        }

        (Some(_), ExpectedResult::NotApplicable) => panic!("assist should not be applicable!"),
        (None, ExpectedResult::After(_)) => {
            match assist_label {
                Some(label) => {
                    let all = res
                        .iter()
                        .map(|resolved| resolved.label.clone())
                        .collect::<Vec<_>>();
                    panic!("Expecting \"{}\", but not found in {:?}", label, all);
                }
                None => panic!("code action is not applicable"),
            };
        }
        (None, ExpectedResult::NotApplicable) => (),
    };
}

#[test]
fn test_whitespace_skip1() {
    let before = r#"
bar() ->
  A %comment1
   ~   = 3 + B %% comment2
       ~ ,
  foo(A).
"#;

    let (db, frange) = RootDatabase::with_range(before);
    let sema = &db;
    let config = TEST_CONFIG;
    let diagnostics = vec![];
    let ctx = AssistContext::new(sema, &config, frange, &diagnostics, None);
    expect![[r#"
        FileRange {
            file_id: FileId(
                0,
            ),
            range: 26..56,
        }
    "#]]
    .assert_debug_eq(&ctx.frange);
    expect![[r#"
        29..48
    "#]]
    .assert_debug_eq(&ctx.selection_trimmed());
}

#[test]
fn test_whitespace_skip2() {
    let before = r#"
bar() ->
  A ~ %comment1
      = 3 + B ~%% comment2
        ,
  foo(A).
"#;

    let (db, frange) = RootDatabase::with_range(before);
    let sema = &db;
    let config = TEST_CONFIG;
    let diagnostics = vec![];
    let ctx = AssistContext::new(sema, &config, frange, &diagnostics, None);
    expect![[r#"
        FileRange {
            file_id: FileId(
                0,
            ),
            range: 13..38,
        }
    "#]]
    .assert_debug_eq(&ctx.frange);
    expect![[r#"
        14..37
    "#]]
    .assert_debug_eq(&ctx.selection_trimmed());
}

#[test]
fn test_function_args() {
    let fixture = r#"heavy_calculations(X) -> ~life:foo(X, X+1)~."#;

    let (db, frange) = RootDatabase::with_range(fixture);
    let sema = &db;
    let config = TEST_CONFIG;
    let diagnostics = vec![];
    let ctx = AssistContext::new(sema, &config, frange, &diagnostics, None);
    let call: ast::Call = ctx.find_node_at_offset().unwrap();
    let call_expr = ctx
        .sema
        .to_expr(InFile::new(ctx.file_id(), &ast::Expr::Call(call)))
        .unwrap();
    if let Expr::Call { target: _, args } = &call_expr[call_expr.value] {
        expect![[r#"
            "X, XN"
        "#]]
        .assert_debug_eq(&ctx.create_function_args(
            FunctionDefId::new(call_expr.function_id.value),
            args,
            &call_expr.body(),
        ));
    } else {
        panic!("Expecting Expr::Call");
    }
}

#[test]
fn test_function_args_from_type() {
    let fixture = r#"
        -callback handle_call(~Request :: term(),
                               From :: from(),
                               State :: term()~) -> ok."#;

    let (db, frange) = RootDatabase::with_range(fixture);
    let sema = &db;
    let config = TEST_CONFIG;
    let diagnostics = vec![];
    let ctx = AssistContext::new(sema, &config, frange, &diagnostics, None);
    let behaviour_forms = ctx.sema.form_list(ctx.file_id());
    if let Some((idx, _callback)) = behaviour_forms.callback_attributes().next() {
        let callback_body = ctx.sema.db.callback_body(InFile::new(ctx.file_id(), idx));
        if let Some(sig) = callback_body.sigs.first() {
            expect![[r#"
                "Request,From,State"
            "#]]
            .assert_debug_eq(&ctx.create_function_args_from_types(&sig.args, &callback_body.body));
        } else {
            panic!("Expecting Sig");
        }
    } else {
        panic!("Expecting Callback");
    };
}

#[test]
fn export_no_pre_existing() {
    fn export_function(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
        if let Some(SymbolClass::Definition(SymbolDefinition::Function(fun))) =
            ctx.classify_offset()
        {
            let function_name_arity = fun.name.clone();
            let function_range = fun.range(ctx.db().upcast()).unwrap();

            if !fun.exported {
                let id = AssistId("export_function", AssistKind::QuickFix);
                let message = format!("Export the function `{function_name_arity}`");
                acc.add(id, message, function_range, None, |builder| {
                    helpers::ExportBuilder::new(
                        &ctx.sema,
                        ctx.file_id(),
                        &[function_name_arity],
                        builder,
                    )
                    .finish();
                });
            }
        }
        Some(())
    }
    check_assist(
        export_function,
        "Export the function `heavy_calculations/1`",
        r#"
         -module(life).

         heavy_cal~culations(X) -> X.
        "#,
        expect![[r#"
                -module(life).

                -export([heavy_calculations/1]).

                heavy_calculations(X) -> X.
            "#]],
    )
}

#[test]
fn export_single_pre_existing() {
    fn export_function(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
        if let Some(SymbolClass::Definition(SymbolDefinition::Function(fun))) =
            ctx.classify_offset()
        {
            let function_name_arity = fun.name.clone();
            let function_range = fun.range(ctx.db().upcast()).unwrap();

            if !fun.exported {
                let id = AssistId("export_function", AssistKind::QuickFix);
                let message = format!("Export the function `{function_name_arity}`");
                acc.add(id, message, function_range, None, |builder| {
                    helpers::ExportBuilder::new(
                        &ctx.sema,
                        ctx.file_id(),
                        &[function_name_arity],
                        builder,
                    )
                    .finish();
                });
            }
        }
        Some(())
    }
    check_assist(
        export_function,
        "Export the function `heavy_calculations/1`",
        r#"
          -module(life).

          -export([foo/0]).

          heavy_cal~culations(X) -> X.

          foo() -> ok.
         "#,
        expect![[r#"
            -module(life).

            -export([foo/0, heavy_calculations/1]).

            heavy_calculations(X) -> X.

            foo() -> ok.
        "#]],
    )
}

#[test]
fn export_single_pre_existing_with_comment() {
    fn export_function(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
        if let Some(SymbolClass::Definition(SymbolDefinition::Function(fun))) =
            ctx.classify_offset()
        {
            let function_name_arity = fun.name.clone();
            let function_range = fun.range(ctx.db().upcast()).unwrap();

            if !fun.exported {
                let id = AssistId("export_function", AssistKind::QuickFix);
                let message = format!("Export the function `{function_name_arity}`");
                acc.add(id, message, function_range, None, |builder| {
                    helpers::ExportBuilder::new(
                        &ctx.sema,
                        ctx.file_id(),
                        &[function_name_arity],
                        builder,
                    )
                    .with_comment("header comment".to_string())
                    .finish();
                });
            }
        }
        Some(())
    }
    check_assist(
        export_function,
        "Export the function `heavy_calculations/1`",
        r#"
          -module(life).

          -export([foo/0]).

          heavy_cal~culations(X) -> X.

          foo() -> ok.
         "#,
        expect![[r#"
            -module(life).

            %% header comment
            -export([heavy_calculations/1]).

            -export([foo/0]).

            heavy_calculations(X) -> X.

            foo() -> ok.
        "#]],
    )
}

#[test]
fn export_single_group_with_overrides_comment() {
    fn export_function(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
        if let Some(SymbolClass::Definition(SymbolDefinition::Function(fun))) =
            ctx.classify_offset()
        {
            let function_name_arity = fun.name.clone();
            let function_range = fun.range(ctx.db().upcast()).unwrap();

            let forms = ctx.db().file_form_list(ctx.file_id());
            let (_, export) = forms.exports().next().unwrap();
            let fa = &export.entries.clone().into_iter().next().unwrap();
            let existing = forms[*fa].name.clone();

            if !fun.exported {
                let id = AssistId("export_function", AssistKind::QuickFix);
                let message = format!("Export the function `{function_name_arity}`");
                acc.add(id, message, function_range, None, |builder| {
                    helpers::ExportBuilder::new(
                        &ctx.sema,
                        ctx.file_id(),
                        &[function_name_arity],
                        builder,
                    )
                    .group_with(existing)
                    .with_comment("header comment".to_string())
                    .finish();
                });
            }
        }
        Some(())
    }
    check_assist(
        export_function,
        "Export the function `heavy_calculations/1`",
        r#"
          -module(life).

          -export([foo/0]).

          heavy_cal~culations(X) -> X.

          foo() -> ok.
         "#,
        expect![[r#"
            -module(life).

            -export([foo/0, heavy_calculations/1]).

            heavy_calculations(X) -> X.

            foo() -> ok.
        "#]],
    )
}

#[test]
fn export_into_specific_pre_existing_1() {
    fn export_function(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
        if let Some(SymbolClass::Definition(SymbolDefinition::Function(fun))) =
            ctx.classify_offset()
        {
            let function_name_arity = fun.name.clone();
            let function_range = fun.range(ctx.db().upcast()).unwrap();

            let forms = ctx.db().file_form_list(ctx.file_id());
            let (_, export) = forms.exports().next().unwrap();
            let fa = &export.entries.clone().into_iter().next().unwrap();
            let existing = forms[*fa].name.clone();

            if !fun.exported {
                let id = AssistId("export_function", AssistKind::QuickFix);
                let message = format!("Export the function `{function_name_arity}`");
                acc.add(id, message, function_range, None, |builder| {
                    helpers::ExportBuilder::new(
                        &ctx.sema,
                        ctx.file_id(),
                        &[function_name_arity],
                        builder,
                    )
                    .group_with(existing)
                    .finish();
                });
            }
        }
        Some(())
    }
    check_assist(
        export_function,
        "Export the function `heavy_calculations/1`",
        r#"
          -module(life).

          -export([foo/0]).
          -export([bar/0]).

          heavy_cal~culations(X) -> X.

          foo() -> ok.
          bar() -> ok.
         "#,
        expect![[r#"
            -module(life).

            -export([foo/0, heavy_calculations/1]).
            -export([bar/0]).

            heavy_calculations(X) -> X.

            foo() -> ok.
            bar() -> ok.
        "#]],
    )
}

#[test]
fn export_into_specific_pre_existing_2() {
    fn export_function(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
        if let Some(SymbolClass::Definition(SymbolDefinition::Function(fun))) =
            ctx.classify_offset()
        {
            let function_name_arity = fun.name.clone();
            let function_range = fun.range(ctx.db().upcast()).unwrap();

            let forms = ctx.db().file_form_list(ctx.file_id());
            let (_, export) = forms.exports().nth(1).unwrap();
            let fa = &export.entries.clone().into_iter().next().unwrap();
            let existing = forms[*fa].name.clone();

            if !fun.exported {
                let id = AssistId("export_function", AssistKind::QuickFix);
                let message = format!("Export the function `{function_name_arity}`");
                acc.add(id, message, function_range, None, |builder| {
                    helpers::ExportBuilder::new(
                        &ctx.sema,
                        ctx.file_id(),
                        &[function_name_arity],
                        builder,
                    )
                    .group_with(existing)
                    .finish();
                });
            }
        }
        Some(())
    }
    check_assist(
        export_function,
        "Export the function `heavy_calculations/1`",
        r#"
          -module(life).

          -export([foo/0]).
          -export([bar/0]).

          heavy_cal~culations(X) -> X.

          foo() -> ok.
          bar() -> ok.
         "#,
        expect![[r#"
            -module(life).

            -export([foo/0]).
            -export([bar/0, heavy_calculations/1]).

            heavy_calculations(X) -> X.

            foo() -> ok.
            bar() -> ok.
        "#]],
    )
}

#[test]
fn add_compile_option_1() {
    let before = r#"
             -module(foo_SUITE).

             bar() -> ok.
             "#;

    let (db, file_id) = RootDatabase::with_single_file(before);
    let sema = Semantic::new(&db);

    let mut builder = SourceChangeBuilder::new(file_id);
    helpers::add_compile_option(&sema, file_id, "blah", None, &mut builder);
    let source_change = builder.finish();
    let mut changed = db.file_text(file_id).to_string();
    for edit in source_change.source_file_edits.values() {
        edit.apply(&mut changed);
    }

    expect![[r#"
        -module(foo_SUITE).

        -compile([blah]).

        bar() -> ok.
    "#]]
    .assert_eq(&changed);
}

#[test]
fn add_to_suite_basic() {
    let before = r#"
           -module(foo_SUITE).

           bar() -> ok.
          "#;

    let (db, file_id) = RootDatabase::with_single_file(before);
    let sema = Semantic::new(&db);

    let mut builder = SourceChangeBuilder::new(file_id);
    helpers::add_suite_0_option(
        &sema,
        file_id,
        "timetrap",
        "{seconds, 10}",
        None,
        &mut builder,
    );
    let source_change = builder.finish();
    let mut changed = db.file_text(file_id).to_string();
    for edit in source_change.source_file_edits.values() {
        edit.apply(&mut changed);
    }

    expect![[r#"
        -module(foo_SUITE).

        -export([suite/0]).

        suite() ->
            [{timetrap, {seconds, 10}}].

        bar() -> ok.
    "#]]
    .assert_eq(&changed);
}

#[test]
fn add_to_suite_existing_no_match() {
    let before = r#"
           -module(foo_SUITE).

           suite() ->
               [{timetrap, {seconds, 10}}].

           bar() -> ok.
          "#;

    let (db, file_id) = RootDatabase::with_single_file(before);
    let sema = Semantic::new(&db);

    let mut builder = SourceChangeBuilder::new(file_id);
    helpers::add_suite_0_option(&sema, file_id, "require", "foo", None, &mut builder);
    let source_change = builder.finish();
    let mut changed = db.file_text(file_id).to_string();
    for edit in source_change.source_file_edits.values() {
        edit.apply(&mut changed);
    }

    // Note: also adds the missing export
    expect![[r#"
        -module(foo_SUITE).

        -export([suite/0]).

        suite() ->
            [{timetrap, {seconds, 10}}, {require, foo}].

        bar() -> ok.
    "#]]
    .assert_eq(&changed);
}

#[test]
fn add_to_suite_existing_with_match() {
    let before = r#"
           -module(foo_SUITE).

           -export([suite/0]).

           suite() ->
               [{timetrap, {seconds, 10}}].

           bar() -> ok.
          "#;

    let (db, file_id) = RootDatabase::with_single_file(before);
    let sema = Semantic::new(&db);

    let mut builder = SourceChangeBuilder::new(file_id);
    helpers::add_suite_0_option(
        &sema,
        file_id,
        "timetrap",
        "{seconds, 30}",
        None,
        &mut builder,
    );
    let source_change = builder.finish();
    let mut changed = db.file_text(file_id).to_string();
    for edit in source_change.source_file_edits.values() {
        edit.apply(&mut changed);
    }

    // Note: also adds the missing export
    expect![[r#"
        -module(foo_SUITE).

        -export([suite/0]).

        suite() ->
            [{timetrap, {seconds, 30}}].

        bar() -> ok.
    "#]]
    .assert_eq(&changed);
}

#[test]
fn add_to_suite_existing_with_comment() {
    let before = r#"
           -module(foo_SUITE).

           -export([suite/0]).

           suite() ->
               %% we do this because ...
               [{timetrap, {seconds, 10}}].

           bar() -> ok.
          "#;

    let (db, file_id) = RootDatabase::with_single_file(before);
    let sema = Semantic::new(&db);

    let mut builder = SourceChangeBuilder::new(file_id);
    helpers::add_suite_0_option(
        &sema,
        file_id,
        "timetrap",
        "{seconds, 30}",
        None,
        &mut builder,
    );
    let source_change = builder.finish();
    let mut changed = db.file_text(file_id).to_string();
    for edit in source_change.source_file_edits.values() {
        edit.apply(&mut changed);
    }

    // Note: also adds the missing export
    expect![[r#"
        -module(foo_SUITE).

        -export([suite/0]).

        suite() ->
            %% we do this because ...
            [{timetrap, {seconds, 30}}].

        bar() -> ok.
    "#]]
    .assert_eq(&changed);
}

#[test]
fn add_to_suite_grouped_export() {
    let before = r#"
           -module(foo_SUITE).

           -export([other/0]).

           -export([all/0]).

           all() -> [].

           other() -> ok.
          "#;

    let (db, file_id) = RootDatabase::with_single_file(before);
    let sema = Semantic::new(&db);

    let mut builder = SourceChangeBuilder::new(file_id);
    helpers::add_suite_0_option(
        &sema,
        file_id,
        "timetrap",
        "{seconds, 30}",
        None,
        &mut builder,
    );
    let source_change = builder.finish();
    let mut changed = db.file_text(file_id).to_string();
    for edit in source_change.source_file_edits.values() {
        edit.apply(&mut changed);
    }

    // Note: also adds the missing export
    expect![[r#"
        -module(foo_SUITE).

        -export([other/0]).

        -export([suite/0, all/0]).

        suite() ->
            [{timetrap, {seconds, 30}}].

        all() -> [].

        other() -> ok.
    "#]]
    .assert_eq(&changed);
}
