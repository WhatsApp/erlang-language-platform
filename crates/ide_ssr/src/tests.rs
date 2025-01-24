/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::fixture;
use elp_ide_db::elp_base_db::fixture::RangeOrOffset;
use elp_ide_db::elp_base_db::fixture::WithFixture;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::RootDatabase;
use expect_test::expect;
use expect_test::Expect;
use hir::Semantic;

use crate::MatchFinder;
use crate::SsrRule;

#[track_caller]
fn parse_error_text(query: &str) -> String {
    let (mut db, _file_id) = RootDatabase::with_single_file(&query);
    let pattern = SsrRule::parse_str(&mut db, query);
    format!("{}", pattern.unwrap_err())
}

#[track_caller]
fn parse_good_text(query: &str, expect: Expect) {
    let (mut db, _file_id) = RootDatabase::with_single_file(&query);
    let pattern = SsrRule::parse_str(&mut db, query);
    let actual = pattern.unwrap().tree_print(&db);
    expect.assert_eq(actual.as_str());
}

#[test]
fn parser_empty_query() {
    assert_eq!(parse_error_text(""), "Parse error: Could not lower rule");
}

#[test]
fn parser_basic_query() {
    parse_good_text(
        "ssr: V ==>> V + 1.",
        expect![[r#"

            SsrBody {
                lhs
                    Expr::Var(V)
                rhs
                    Expr::BinaryOp {
                        lhs
                            Expr::Var(V)
                        rhs
                            Literal(Integer(1))
                        op
                            ArithOp(Add),
                    }
                when
            }
        "#]],
    );
}

#[test]
fn parser_basic_query_with_placeholder() {
    parse_good_text(
        "ssr: _@V ==>> _@V + 1.",
        expect![[r#"

            SsrBody {
                lhs
                    SsrPlaceholder {var: _@V, conditions: TBD}
                rhs
                    Expr::BinaryOp {
                        lhs
                            SsrPlaceholder {var: _@V, conditions: TBD}
                        rhs
                            Literal(Integer(1))
                        op
                            ArithOp(Add),
                    }
                when
            }
        "#]],
    );
}

#[test]
fn parser_basic_query_with_cond() {
    parse_good_text(
        "ssr: V ==>> V + 1
              when is_atom(V)
         .
        ",
        expect![[r#"

            SsrBody {
                lhs
                    Expr::Var(V)
                rhs
                    Expr::BinaryOp {
                        lhs
                            Expr::Var(V)
                        rhs
                            Literal(Integer(1))
                        op
                            ArithOp(Add),
                    }
                when
                    guard
                        Expr::Call {
                            target
                                CallTarget::Remote {
                                    Literal(Atom('erlang'))
                                    Literal(Atom('is_atom'))
                                }
                            args
                                Expr::Var(V),
                        },
            }
        "#]],
    );
}

// ---------------------------------------------------------------------

/// `code` may optionally contain a cursor marker `~`. If it doesn't,
/// then the position will be the start of the file. If there's a
/// second cursor marker, then we'll return a single range.
pub(crate) fn single_file(code: &str) -> (RootDatabase, FilePosition, Vec<FileRange>) {
    let (db, file_id, range_or_offset) = if code.contains(fixture::CURSOR_MARKER) {
        RootDatabase::with_range_or_offset(code)
    } else {
        let (db, file_id) = RootDatabase::with_single_file(code);
        (db, file_id, RangeOrOffset::Offset(0.into()))
    };
    let selections;
    let position;
    match range_or_offset {
        RangeOrOffset::Range(range) => {
            position = FilePosition {
                file_id,
                offset: range.start(),
            };
            selections = vec![FileRange { file_id, range }];
        }
        RangeOrOffset::Offset(offset) => {
            position = FilePosition { file_id, offset };
            selections = vec![];
        }
    }
    (db, position, selections)
}

fn print_match_debug_info(match_finder: &MatchFinder<'_>, file_id: FileId, snippet: &str) {
    let debug_info = match_finder.debug_where_text_equal(file_id, snippet);
    println!(
        "Match debug info: {} nodes had text exactly equal to '{}'",
        debug_info.len(),
        snippet
    );
    for (index, d) in debug_info.iter().enumerate() {
        println!("Node #{index}\n{d:#?}\n");
    }
}

#[track_caller]
fn assert_matches(pattern: &str, code: &str, expected: &[&str]) {
    let (db, position, selections) = single_file(code);
    if expected.len() > 0 {
        if expected[0] == "" {
            panic!("empty expected string");
        }
    }
    let sema = Semantic::new(&db);
    let pattern = SsrRule::parse_str(sema.db, pattern).unwrap();
    let mut match_finder = MatchFinder::in_context(&sema, position.file_id, selections).unwrap();
    match_finder.add_search_pattern(pattern).unwrap();
    let matched_strings: Vec<String> = match_finder
        .matches()
        .flattened()
        .matches
        .iter()
        .map(|m| m.matched_text(&db))
        .collect();
    if matched_strings != expected && !expected.is_empty() {
        print_match_debug_info(&match_finder, position.file_id, expected[0]);
    }
    assert_eq!(matched_strings, expected);
}

// ---------------------------------------------------------------------

#[test]
fn ssr_let_stmt_in_fn_match_1() {
    assert_matches("ssr: _@A = 10.", "foo() -> X = 10, X.", &["X = 10"]);
}

#[test]
fn ssr_let_stmt_in_fn_match_2() {
    assert_matches("ssr: _@A = _@B.", "foo() -> X = 10, X.", &["X = 10"]);
}

#[test]
fn ssr_block_expr_match_1() {
    assert_matches(
        "ssr: begin _@A = _@B end.",
        "fon() -> begin X = 10 end.",
        &["begin X = 10 end"],
    );
}

#[test]
fn ssr_block_expr_match_2() {
    assert_matches(
        "ssr: begin _@A = _@B, _@C end.",
        "foo() -> begin X = 10, X end.",
        &["begin X = 10, X end"],
    );
}

#[test]
fn ssr_block_expr_match_multiple_statements() {
    assert_matches(
        "ssr: begin _@A = _@B, _@C, _@D end.",
        "foo() -> begin X = 10, Y = 20, Z = X + Y end.",
        &["begin X = 10, Y = 20, Z = X + Y end"],
    );
}

#[test]
fn ssr_expr_match_tuple() {
    assert_matches(
        "ssr: {foo, _@A, _@B, _@C, _@D}.",
        "fn() -> X = {foo, a, b, c, d}, X.",
        &["{foo, a, b, c, d}"],
    );
}

#[test]
fn ssr_expr_match_tuple_nested() {
    assert_matches(
        "ssr: {foo, {foo, 1}}.",
        "fn() -> X = {foo, {foo, 1}}.",
        &["{foo, {foo, 1}}"],
    );
    assert_matches(
        "ssr: {foo, _@A}.",
        "fn() -> X = {foo, {foo, 1}}.",
        &["{foo, {foo, 1}}", "{foo, 1}"],
    );
}

#[test]
fn ssr_record_expr_match() {
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn() -> X = #foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}, X.",
        &["#foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}"],
    );
}

#[test]
fn ssr_record_expr_match_5() {
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn() -> X = #foo{k2 = a, k3 = <<\"blah\">>, k1 = {c, d}}, X.",
        &["#foo{k2 = a, k3 = <<\"blah\">>, k1 = {c, d}}"],
    );
}

#[test]
fn ssr_record_expr_match_6() {
    // Note: HIR record only stores atom field names, so will silently
    // discard the placeholder. This will be fixed later in the stack.
    assert_matches(
        "ssr: #foo{_@K = _@A, k2 = _@B, k3 = _@C}.",
        "fn() -> X = #foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}, X.",
        &[],
    );
}

#[test]
fn ssr_record_expr_match_record() {
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn() -> X = #boo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}, X.",
        &[],
    );
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn() -> X = #foo{ka1 = a, ka2 = <<\"blah\">>, ka3 = {c, d}}, X.",
        &[],
    );
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn() -> X = #foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}, X.",
        &["#foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}"],
    );
}

#[test]
#[ignore]
fn ssr_record_expr_match_record_subset() {
    // Note: this test currently fails.
    // We need to extend the syntax to be able to say there are
    // possibly don't care extra fields.
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B}.",
        "fn() -> X = #foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}, X.",
        &["#foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}"],
    );
}

#[test]
fn ssr_record_expr_match_unordered() {
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn() -> X = #foo{k2 = a, k3 = <<\"blah\">>, k1 = {c, d}}, X.",
        &["#foo{k2 = a, k3 = <<\"blah\">>, k1 = {c, d}}"],
    );
}

#[test]
fn ssr_record_expr_match_rhs() {
    assert_matches(
        "ssr: #foo{k1 = 3, k2 = {_@A, _@B}, k3 = _@C}.",
        "fn() -> X = #foo{k1 = a, k3 = <<\"blah\">>, k2 = {c, d}}, X.",
        &[],
    );
    assert_matches(
        "ssr: #foo{k1 = 3, k2 = {_@A, _@B}, k3 = _@C}.",
        "fn() -> X = #foo{k1 = 3, k3 = <<\"blah\">>, k2 = {c, d}}, X.",
        &["#foo{k1 = 3, k3 = <<\"blah\">>, k2 = {c, d}}"],
    );
}
