/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::RootDatabase;
use elp_ide_db::elp_base_db::CURSOR_MARKER;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::RangeOrOffset;
use elp_ide_db::elp_base_db::assert_eq_expected;
use elp_ide_db::elp_base_db::fixture::WithFixture;
use expect_test::Expect;
use expect_test::expect;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

type MatchExpected<'a> = &'a [(&'a str, &'a [(&'a str, &'a [&'a str])])];
type MatchResult = Vec<(String, Vec<(String, Vec<String>)>)>;

use crate::Match;
use crate::MatchFinder;
use crate::SsrRule;
use crate::SsrSearchScope;

#[track_caller]
fn parse_error_text(query: &str) -> String {
    let (db, _file_id) = RootDatabase::with_single_file(query);
    let pattern = SsrRule::parse_str(&db, query);
    format!("{}", pattern.unwrap_err())
}

#[track_caller]
fn parse_good_text(query: &str, expect: Expect) {
    let (db, _file_id) = RootDatabase::with_single_file(query);
    let pattern = SsrRule::parse_str(&db, query);
    let actual = pattern.unwrap().tree_print();
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
                    expr
                        Expr<0>:Expr::Var(V)
                    pat
                        Pat<0>:Pat::Var(V)
                rhs
                    Expr<3>:Expr::BinaryOp {
                        lhs
                            Expr<1>:Expr::Var(V)
                        rhs
                            Expr<2>:Literal(Integer(1))
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
                    expr
                        Expr<0>:Expr::Var(_@V)
                    pat
                        Pat<0>:Pat::Var(_@V)
                rhs
                    Expr<3>:Expr::BinaryOp {
                        lhs
                            Expr<1>:Expr::Var(_@V)
                        rhs
                            Expr<2>:Literal(Integer(1))
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
              where V == foo
         .
        ",
        expect![[r#"

            SsrBody {
                lhs
                    expr
                        Expr<0>:Expr::Var(V)
                    pat
                        Pat<0>:Pat::Var(V)
                rhs
                    Expr<3>:Expr::BinaryOp {
                        lhs
                            Expr<1>:Expr::Var(V)
                        rhs
                            Expr<2>:Literal(Integer(1))
                        op
                            ArithOp(Add),
                    }
                when
                    guard
                        Expr<6>:Expr::BinaryOp {
                            lhs
                                Expr<4>:Expr::Var(V)
                            rhs
                                Expr<5>:Literal(Atom('foo'))
                            op
                                CompOp(Eq { strict: false, negated: false }),
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
    let (db, file_id, range_or_offset) = if code.contains(CURSOR_MARKER) {
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
        "Match debug info: {} nodes had text exactly equal to \"{}\"",
        debug_info.len(),
        snippet
    );
    for (index, d) in debug_info.iter().enumerate() {
        println!("Node #{index}\n{d:#?}\n");
    }
}

/// Get all placeholder information from a match as a sorted list of (name, matched_texts).
/// The list is sorted alphabetically by placeholder name.
fn get_placeholder_info(sema: &Semantic, m: &Match) -> Vec<(String, Vec<String>)> {
    let mut placeholders: Vec<(String, Vec<String>)> = m
        .placeholders_by_var
        .keys()
        .filter_map(|var| {
            let name = var.as_name().to_string();
            let texts = m.placeholder_texts(sema, &name)?;
            Some((name, texts))
        })
        .collect();
    placeholders.sort_by(|a, b| a.0.cmp(&b.0));
    placeholders
}

#[track_caller]
fn assert_matches(pattern: &str, code: &str, expected: MatchExpected) {
    assert_matches_with_strategy(
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        pattern,
        code,
        expected,
    );
}

#[track_caller]
fn assert_matches_with_strategy(
    strategy: Strategy,
    pattern: &str,
    code: &str,
    expected: MatchExpected,
) {
    let (db, position, _selections) = single_file(code);
    if !expected.is_empty() && expected[0].0.is_empty() {
        panic!("empty expected string");
    }
    let sema = Semantic::new(&db);
    let pattern = SsrRule::parse_str(sema.db, pattern).unwrap();
    let mut match_finder =
        MatchFinder::in_context(&sema, strategy, SsrSearchScope::WholeFile(position.file_id));
    match_finder.debug_print = false;
    match_finder.add_search_pattern(pattern);
    let matches = match_finder.matches().flattened();

    // Build actual results as tuples of (matched_text, placeholder_info)
    let actual: MatchResult = matches
        .matches
        .iter()
        .map(|m| {
            let matched_text = m.matched_text(&db);
            let placeholder_info = get_placeholder_info(&sema, m);
            (matched_text, placeholder_info)
        })
        .collect();

    // Convert expected to owned types for comparison
    let expected_owned: MatchResult = expected
        .iter()
        .map(|(text, placeholders)| {
            (
                text.to_string(),
                placeholders
                    .iter()
                    .map(|(name, texts)| {
                        (
                            name.to_string(),
                            texts.iter().map(|t| t.to_string()).collect(),
                        )
                    })
                    .collect(),
            )
        })
        .collect();

    if actual != expected_owned && !expected.is_empty() {
        print_match_debug_info(&match_finder, position.file_id, expected[0].0);
    }
    assert_eq_expected!(expected_owned, actual);
}

#[track_caller]
fn assert_match_placeholder(
    pattern: &str,
    code: &str,
    expected: &[&str],
    placeholder_name: &str,
    expected_val: Expect,
) {
    let (db, position, _selections) = single_file(code);
    if !expected.is_empty() && expected[0].is_empty() {
        panic!("empty expected string");
    }
    let sema = Semantic::new(&db);
    let pattern = SsrRule::parse_str(sema.db, pattern).unwrap();
    let mut match_finder = MatchFinder::in_context(
        &sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        SsrSearchScope::WholeFile(position.file_id),
    );
    match_finder.debug_print = false;
    match_finder.add_search_pattern(pattern);
    let matches = match_finder.matches();
    let match0 = &matches.matches[0];
    let placeholder_val = match0.get_placeholder_matches(placeholder_name);
    let matched_strings: Vec<String> = matches
        .flattened()
        .matches
        .iter()
        .map(|m| m.matched_text(&db))
        .collect();
    if matched_strings != expected && !expected.is_empty() {
        print_match_debug_info(&match_finder, position.file_id, expected[0]);
    }
    assert_eq_expected!(expected, matched_strings);
    expected_val.assert_debug_eq(&placeholder_val);
}

// ---------------------------------------------------------------------

#[test]
fn ssr_let_stmt_in_fn_match_1() {
    assert_matches(
        "ssr: _@A = 10.",
        "foo() -> X = 10, X.",
        &[("X = 10", &[("_@A", &["X"])])],
    );
}

#[test]
fn ssr_let_stmt_in_fn_match_2() {
    assert_matches(
        "ssr: _@A = _@B.",
        "foo() -> X = 10, X.",
        &[("X = 10", &[("_@A", &["X"]), ("_@B", &["10"])])],
    );
}

#[test]
fn ssr_block_expr_match_1() {
    assert_matches(
        "ssr: begin _@A = _@B end.",
        "fon() -> begin X = 10 end.",
        &[("begin X = 10 end", &[("_@A", &["X"]), ("_@B", &["10"])])],
    );
}

#[test]
fn ssr_block_expr_match_2() {
    assert_matches(
        "ssr: begin _@A = _@B, _@C end.",
        "foo() -> begin X = 10, X end.",
        &[(
            "begin X = 10, X end",
            &[("_@A", &["X"]), ("_@B", &["10"]), ("_@C", &["X"])],
        )],
    );
}

#[test]
fn ssr_block_expr_match_multiple_statements() {
    assert_matches(
        "ssr: begin _@A = _@B, _@C, _@D end.",
        "foo() -> begin X = 10, Y = 20, Z = X + Y end.",
        &[(
            "begin X = 10, Y = 20, Z = X + Y end",
            &[
                ("_@A", &["X"]),
                ("_@B", &["10"]),
                ("_@C", &["Y = 20"]),
                ("_@D", &["Z = X + Y"]),
            ],
        )],
    );
}

#[test]
fn ssr_expr_match_tuple() {
    assert_matches(
        "ssr: {foo, _@A, _@B, _@C, _@D}.",
        "fn() -> X = {foo, a, b, c, d}, X.",
        &[(
            "{foo, a, b, c, d}",
            &[
                ("_@A", &["a"]),
                ("_@B", &["b"]),
                ("_@C", &["c"]),
                ("_@D", &["d"]),
            ],
        )],
    );
}

#[test]
fn ssr_expr_match_tuple_nested() {
    assert_matches(
        "ssr: {foo, {foo, 1}}.",
        "fn() -> X = {foo, {foo, 1}}.",
        &[("{foo, {foo, 1}}", &[])],
    );
    assert_matches(
        "ssr: {foo, _@A}.",
        "fn() -> X = {foo, {foo, 1}}.",
        &[
            ("{foo, {foo, 1}}", &[("_@A", &["{foo, 1}"])]),
            ("{foo, 1}", &[("_@A", &["1"])]),
        ],
    );
}

#[test]
fn ssr_record_expr_match() {
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn() -> X = #foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}, X.",
        &[(
            "#foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}",
            &[
                ("_@A", &["a"]),
                ("_@B", &["<<\"blah\">>"]),
                ("_@C", &["{c, d}"]),
            ],
        )],
    );
}

#[test]
fn ssr_record_expr_match_5() {
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn() -> X = #foo{k2 = a, k3 = <<\"blah\">>, k1 = {c, d}}, X.",
        &[(
            "#foo{k2 = a, k3 = <<\"blah\">>, k1 = {c, d}}",
            &[
                ("_@A", &["{c, d}"]),
                ("_@B", &["a"]),
                ("_@C", &["<<\"blah\">>"]),
            ],
        )],
    );
}

#[test]
fn ssr_record_expr_match_6() {
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
        &[(
            "#foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}",
            &[
                ("_@A", &["a"]),
                ("_@B", &["<<\"blah\">>"]),
                ("_@C", &["{c, d}"]),
            ],
        )],
    );
}

#[test]
fn ssr_record_expr_match_record_subset() {
    // TODO(T256425463): this test should match the record, but currently fails to.
    // We need to extend the syntax to be able to say there are
    // possibly don't care extra fields.
    // Once fixed, the expected result should be:
    // &[(
    //     "#foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}",
    //     &[("_@A", &["a"]), ("_@B", &["<<\"blah\">>"])],
    // )],
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B}.",
        "fn() -> X = #foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}, X.",
        &[],
    );
}

#[test]
fn ssr_record_expr_match_unordered() {
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn() -> X = #foo{k2 = a, k3 = <<\"blah\">>, k1 = {c, d}}, X.",
        &[(
            "#foo{k2 = a, k3 = <<\"blah\">>, k1 = {c, d}}",
            &[
                ("_@A", &["{c, d}"]),
                ("_@B", &["a"]),
                ("_@C", &["<<\"blah\">>"]),
            ],
        )],
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
        &[(
            "#foo{k1 = 3, k3 = <<\"blah\">>, k2 = {c, d}}",
            &[("_@A", &["c"]), ("_@B", &["d"]), ("_@C", &["<<\"blah\">>"])],
        )],
    );
}

#[test]
fn ssr_expr_match_list() {
    assert_matches(
        "ssr: [ _@A, _@B | _@C].",
        "fn(Y) -> X = [1, 2 | [Y]].",
        &[(
            "[1, 2 | [Y]]",
            &[("_@A", &["1"]), ("_@B", &["2"]), ("_@C", &["[Y]"])],
        )],
    );
}

#[test]
fn ssr_expr_match_list_match_pipe() {
    assert_matches("ssr: [ _@A, _@B | _@C].", "fn(Y) -> X = [1, 2, [Y]].", &[]);
}

#[test]
fn ssr_expr_match_binary() {
    assert_matches(
        "ssr: << _@A, _@B>>.",
        "fn(Y) -> X=1, <<X,Y>>.",
        &[("<<X,Y>>", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
}

#[test]
fn ssr_expr_match_unary_op() {
    assert_matches(
        "ssr: not _@A.",
        "fn(Y) -> not Y.",
        &[("not Y", &[("_@A", &["Y"])])],
    );
    assert_matches("ssr: bnot _@A.", "fn(Y) -> not Y.", &[]);
    assert_matches(
        "ssr: bnot _@A.",
        "fn(Y) -> bnot Y.",
        &[("bnot Y", &[("_@A", &["Y"])])],
    );
    // Note: it is an AST, not textual match
    assert_matches("ssr: + _@A.", "fn(Y) -> +Y.", &[("+Y", &[("_@A", &["Y"])])]);
    assert_matches("ssr: -_@A.", "fn(Y) -> -Y.", &[("-Y", &[("_@A", &["Y"])])]);
}

#[test]
fn ssr_expr_binary_op() {
    assert_matches(
        "ssr: _@A + _@B.",
        "fn(X) -> Y = {X + 1}, Y.",
        &[("X + 1", &[("_@A", &["X"]), ("_@B", &["1"])])],
    );
    assert_matches("ssr: _@A - _@B.", "fn(X) -> Y = {X + 1}, Y.", &[]);
    assert_matches(
        "ssr: _@A and _@B.",
        "fn(X,Y) -> X and Y .",
        &[("X and Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A andalso _@B.",
        "fn(X,Y) -> X andalso Y .",
        &[("X andalso Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A or _@B.",
        "fn(X,Y) -> X or Y .",
        &[("X or Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A orelse _@B.",
        "fn(X,Y) -> X orelse Y .",
        &[("X orelse Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A ! _@B.",
        "fn(X,Y) -> X ! Y .",
        &[("X ! Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A == _@B.",
        "fn(X,Y) -> X == Y .",
        &[("X == Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A /= _@B.",
        "fn(X,Y) -> X /= Y .",
        &[("X /= Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A =< _@B.",
        "fn(X,Y) -> X =< Y .",
        &[("X =< Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A  < _@B.",
        "fn(X,Y) -> X  < Y .",
        &[("X  < Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A >= _@B.",
        "fn(X,Y) -> X >= Y .",
        &[("X >= Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A >  _@B.",
        "fn(X,Y) -> X >  Y .",
        &[("X >  Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A =:= _@B.",
        "fn(X,Y) -> X =:= Y .",
        &[("X =:= Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A =/= _@B.",
        "fn(X,Y) -> X =/= Y .",
        &[("X =/= Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A ++ _@B.",
        "fn(X,Y) -> X ++ Y .",
        &[("X ++ Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A -- _@B.",
        "fn(X,Y) -> X -- Y .",
        &[("X -- Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A + _@B.",
        "fn(X,Y) -> X + Y .",
        &[("X + Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A - _@B.",
        "fn(X,Y) -> X - Y .",
        &[("X - Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A bor _@B.",
        "fn(X,Y) -> X bor Y .",
        &[("X bor Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A bxor _@B.",
        "fn(X,Y) -> X bxor Y .",
        &[("X bxor Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A bsl _@B.",
        "fn(X,Y) -> X bsl Y .",
        &[("X bsl Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A bsr _@B.",
        "fn(X,Y) -> X bsr Y .",
        &[("X bsr Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A or _@B.",
        "fn(X,Y) -> X or Y .",
        &[("X or Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A / _@B.",
        "fn(X,Y) -> X / Y .",
        &[("X / Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A * _@B.",
        "fn(X,Y) -> X * Y .",
        &[("X * Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A div _@B.",
        "fn(X,Y) -> X div Y .",
        &[("X div Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A rem _@B.",
        "fn(X,Y) -> X rem Y .",
        &[("X rem Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A band _@B.",
        "fn(X,Y) -> X band Y .",
        &[("X band Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A and _@B.",
        "fn(X,Y) -> X and Y .",
        &[("X and Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
}

#[test]
fn ssr_expr_match_record_update() {
    assert_matches(
        "ssr: _@A#a_record{a_field = _@B}.",
        "bar(List) -> XX = 1, List#record{field = XX}.",
        &[],
    );
    assert_matches(
        "ssr: _@A#a_record{field = _@B}.",
        "bar(List) -> XX = 1, List#a_record{field = XX}.",
        &[(
            "List#a_record{field = XX}",
            &[("_@A", &["List"]), ("_@B", &["XX"])],
        )],
    );
}

#[test]
fn ssr_expr_match_record_index() {
    assert_matches(
        "ssr: #a_record.a_field.",
        "bar(List) -> XX = #record.field, XX.",
        &[],
    );
    assert_matches(
        "ssr: #a_record.a_field.",
        "bar(List) -> XX = #a_record.a_field, XX.",
        &[("#a_record.a_field", &[])],
    );
}

#[test]
fn ssr_expr_match_record_field() {
    assert_matches(
        "ssr: _@A#a_record.a_field.",
        "bar(List) -> XX = List#record.field, XX.",
        &[],
    );
    assert_matches(
        "ssr: _@A#record.field.",
        "bar(List) -> XX = List#record.field, XX.",
        &[("List#record.field", &[("_@A", &["List"])])],
    );
}

#[test]
fn ssr_expr_match_map() {
    // Note that the map operation is always Assoc (`=>`), as per the
    // HIR lowering
    assert_matches(
        "ssr: #{ field => _@A }.",
        "bar() -> XX = 1, #{foo => XX}.",
        &[],
    );
    assert_matches(
        "ssr: #{ field => _@A }.",
        "bar() -> XX = 1, #{field => XX}.",
        &[("#{field => XX}", &[("_@A", &["XX"])])],
    );
    assert_matches(
        "ssr: #{ field => _@A, another => _@B }.",
        "bar() -> XX = 1, #{another => 3, field => XX}.",
        &[(
            "#{another => 3, field => XX}",
            &[("_@A", &["XX"]), ("_@B", &["3"])],
        )],
    );
    assert_matches("ssr: #{ }.", "bar() -> #{}.", &[("#{}", &[])]);
}

#[test]
fn ssr_expr_match_map_update() {
    assert_matches(
        "ssr: _@A#{ foo => _@B }.",
        "bar(List) -> XX = 1, List#{foo := XX}.",
        &[],
    );
    assert_matches(
        "ssr: _@A#{ foo => _@B }.",
        "bar(List) -> XX = 1, List#{foo => XX}.",
        &[("List#{foo => XX}", &[("_@A", &["List"]), ("_@B", &["XX"])])],
    );
    assert_matches(
        "ssr: _@A#{ foo => _@B, zz => _@C }.",
        "bar(List) -> XX = 1, List#{zz => 1, foo => XX}.",
        &[(
            "List#{zz => 1, foo => XX}",
            &[("_@A", &["List"]), ("_@B", &["XX"]), ("_@C", &["1"])],
        )],
    );
    assert_matches(
        "ssr: _@A#{ foo => _@B, zz => {_@C} }.",
        "bar(List) -> XX = 1, List#{zz => 1, foo => XX}.",
        &[],
    );
}

#[test]
fn ssr_expr_match_catch() {
    assert_matches(
        "ssr: catch _@A.",
        "bar() -> XX = 1, catch XX.",
        &[("catch XX", &[("_@A", &["XX"])])],
    );
}

#[test]
fn ssr_expr_match_macro_call() {
    // TODO: fails because we do not have a visible macro call in the
    // template, only Missing
    // And it comes down to having some sort of meaningful fold option
    // that gives the surface call, and the expansion. Maybe try both?
    assert_matches_with_strategy(
        // The default, if not given
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        "ssr: ?BAR(_@AA).",
        "-define(BAR(X), {X}).
         bar() -> ?BAR(4).",
        &[],
    );
    assert_matches_with_strategy(
        Strategy {
            macros: MacroStrategy::ExpandButIncludeMacroCall,
            parens: ParenStrategy::InvisibleParens,
        },
        "ssr: ?BAR(_@AA).",
        "bar() -> ?BAR(4).",
        &[("?BAR(4)", &[("_@AA", &["4"])])],
    );
    assert_matches_with_strategy(
        Strategy {
            macros: MacroStrategy::ExpandButIncludeMacroCall,
            parens: ParenStrategy::InvisibleParens,
        },
        "ssr: ?BAR(_@AA).",
        "bar() -> ?NOT_BAR(4).",
        &[],
    );
    assert_matches_with_strategy(
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        },
        "ssr: ?BAR(_@AA).",
        "-define(BAR(X), {X}).
         bar() -> ?BAR(4).",
        &[("?BAR(4)", &[("_@AA", &["4"])])],
    );
    assert_matches_with_strategy(
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        "ssr: {_@AA}.",
        "-define(BAR(X), {X}).
         bar() -> ?BAR(4).",
        &[
            ("{X}", &[("_@AA", &["X"])]),
            ("?BAR(4)", &[("_@AA", &["4"])]),
        ],
    );
}

/// Pins current `MacroStrategy::Expand` behaviour around macro
/// references whose definition ELP cannot resolve. When the wrapping
/// macro is defined inline or in a resolved include, the inner call
/// surfaces through the expansion (1 match). When the include is
/// unresolved, the entire macro-call subtree is silently dropped and
/// the inner call is invisible (0 matches). The unresolved case is a
/// known shortcoming and should ultimately match the resolved cases.
#[test]
fn ssr_expand_unresolved_macro_drops_inner_call() {
    fn count_matches_in_main(fixture: &str, pattern: &str) -> usize {
        let (db, files, _) = RootDatabase::with_many_files(fixture);
        let main = files[0];
        let sema = Semantic::new(&db);
        let pattern = SsrRule::parse_str(sema.db, pattern).unwrap();
        let strategy = Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        };
        let mut mf = MatchFinder::in_context(&sema, strategy, SsrSearchScope::WholeFile(main));
        mf.add_search_pattern(pattern);
        mf.matches().flattened().matches.len()
    }

    // Inline define: call surfaces through the expansion.
    let inline = count_matches_in_main(
        r#"
//- /src/main.erl
-define(WRAP(BoolExpr),
    case (BoolExpr) of
        true -> ok;
        _ -> erlang:error(assertion_failed)
    end).
bar() -> ?WRAP(f(1, 2, 3)).
"#,
        "ssr: f(_@A, _@B, _@C).",
    );
    assert_eq_expected!(1, inline);

    // Resolved include: same shape, definition in an `.hrl` on the
    // include path. Still 1.
    let from_hrl_resolved = count_matches_in_main(
        r#"
//- /src/main.erl include_path:/include
-include("wrap.hrl").
bar() -> ?WRAP(f(1, 2, 3)).
//- /include/wrap.hrl
-define(WRAP(BoolExpr),
    case (BoolExpr) of
        true -> ok;
        _ -> erlang:error(assertion_failed)
    end).
"#,
        "ssr: f(_@A, _@B, _@C).",
    );
    assert_eq_expected!(1, from_hrl_resolved);

    // Unresolved include: ELP cannot see the macro definition; the
    // entire `?WRAP(...)` subtree is dropped from the fold and the
    // inner call is invisible. Today 0; once the underlying issue is
    // addressed this should also be 1.
    let from_unresolved = count_matches_in_main(
        r#"
//- /src/main.erl
-include("missing.hrl").
bar() -> ?WRAP(f(1, 2, 3)).
"#,
        "ssr: f(_@A, _@B, _@C).",
    );
    assert_eq_expected!(0, from_unresolved);
}

#[test]
fn ssr_expr_list_comprehension() {
    assert_matches(
        "ssr: [XX || XX <- _@List, _@Cond].",
        "bar() -> XX = 1, [XX || XX <- List, XX >= 5].",
        &[(
            "[XX || XX <- List, XX >= 5]",
            &[("_@Cond", &["XX >= 5"]), ("_@List", &["List"])],
        )],
    );
    assert_matches(
        "ssr: [XX || XX <:- _@List, _@Cond].",
        "bar() -> XX = 1, [XX || XX <:- List, XX >= 5].",
        &[(
            "[XX || XX <:- List, XX >= 5]",
            &[("_@Cond", &["XX >= 5"]), ("_@List", &["List"])],
        )],
    );
}

#[test]
fn ssr_expr_list_comprehension_binary_generator_pattern() {
    assert_matches(
        "ssr: <<XX || XX <= _@List, _@Cond>>.",
        "bar() -> XX = 1, [XX || XX <- List, XX >= 5].",
        &[],
    );
    assert_matches(
        "ssr: <<XX || XX <:= _@List, _@Cond>>.",
        "bar() -> XX = 1, [XX || XX <- List, XX >= 5].",
        &[],
    );
}

#[test]
fn ssr_expr_list_comprehension_binary() {
    assert_matches(
        "ssr: <<XX || XX <= _@List>>.",
        "bar(List) -> XX = 1, <<XX || XX <= List>>.",
        &[("<<XX || XX <= List>>", &[("_@List", &["List"])])],
    );
    assert_matches(
        "ssr: <<XX || XX <:= _@List>>.",
        "bar(List) -> XX = 1, <<XX || XX <:= List>>.",
        &[("<<XX || XX <:= List>>", &[("_@List", &["List"])])],
    );
}

#[test]
fn ssr_expr_map_comprehension() {
    assert_matches(
        "ssr: #{_@K => _@V || _@K := _@V <- _@Map}.",
        "bar(Map) -> #{ K => V || K := V <- Map}.",
        &[(
            "#{ K => V || K := V <- Map}",
            &[
                ("_@K", &["K", "K"]),
                ("_@Map", &["Map"]),
                ("_@V", &["V", "V"]),
            ],
        )],
    );
    assert_matches(
        "ssr: #{_@K => _@V || _@K := _@V <:- _@Map}.",
        "bar(Map) -> #{ K => V || K := V <:- Map}.",
        &[(
            "#{ K => V || K := V <:- Map}",
            &[
                ("_@K", &["K", "K"]),
                ("_@Map", &["Map"]),
                ("_@V", &["V", "V"]),
            ],
        )],
    );
}

#[test]
fn ssr_expr_multi_template_list_comprehension() {
    // Multi-template: [X, Y || X <- Xs]
    assert_matches(
        "ssr: [_@A, _@B || X <- _@List].",
        "bar(Xs) -> [X + 1, X + 2 || X <- Xs].",
        &[(
            "[X + 1, X + 2 || X <- Xs]",
            &[
                ("_@A", &["X + 1"]),
                ("_@B", &["X + 2"]),
                ("_@List", &["Xs"]),
            ],
        )],
    );
    // Single-template pattern should NOT match multi-template code
    assert_matches(
        "ssr: [_@A || X <- _@List].",
        "bar(Xs) -> [X + 1, X + 2 || X <- Xs].",
        &[],
    );
}

#[test]
fn ssr_expr_multi_template_map_comprehension() {
    // Multi-template: #{K1 => V1, K2 => V2 || ...}
    assert_matches(
        "ssr: #{_@K1 => _@V1, _@K2 => _@V2 || K := V <- _@Map}.",
        "bar(L) -> #{K1 => V1, K2 => V2 || K := V <- L}.",
        &[(
            "#{K1 => V1, K2 => V2 || K := V <- L}",
            &[
                ("_@K1", &["K1"]),
                ("_@K2", &["K2"]),
                ("_@Map", &["L"]),
                ("_@V1", &["V1"]),
                ("_@V2", &["V2"]),
            ],
        )],
    );
}

#[test]
fn ssr_expr_zip_comprehension() {
    assert_matches(
        "ssr: [_@A || XX <- _@List && YY <- _@B, _@Cond].",
        "bar() -> [{XX,YY} || XX <- List && YY <- ZZ, XX >= 5].",
        &[(
            "[{XX,YY} || XX <- List && YY <- ZZ, XX >= 5]",
            &[
                ("_@A", &["{XX,YY}"]),
                ("_@B", &["ZZ"]),
                ("_@Cond", &["XX >= 5"]),
                ("_@List", &["List"]),
            ],
        )],
    );
}

#[test]
fn ssr_expr_if() {
    assert_matches(
        "ssr: if _@Cond -> _@B end .",
        "bar(F) -> if is_atom(F) -> 22 end.",
        &[(
            "if is_atom(F) -> 22 end",
            &[("_@B", &["22"]), ("_@Cond", &["is_atom(F)"])],
        )],
    );
}

#[test]
fn ssr_expr_case() {
    assert_matches(
        "ssr: case _@XX of _@A -> _@B end .",
        "bar(F) -> XX = 1, case F of undefined -> XX end.",
        &[(
            "case F of undefined -> XX end",
            &[("_@A", &["undefined"]), ("_@B", &["XX"]), ("_@XX", &["F"])],
        )],
    );
}

#[test]
fn ssr_underscore_pattern_in_code_and_placeholder_in_ssr_do_not_match_if_atom_literal_pattens_do_no_match()
 {
    assert_matches(
        "ssr: case X of false -> _@BranchA; _@PatA -> _@BranchB end.",
        "foo(X) -> case X of true -> a; _ -> b end.",
        &[],
    );
}

#[test]
fn ssr_expr_receive() {
    assert_matches(
        "ssr: receive _@XX -> 3 end.",
        "bar(F) -> XX = 1, receive F -> 3 end.",
        &[("receive F -> 3 end", &[("_@XX", &["F"])])],
    );
    assert_matches(
        "ssr: receive _@XX -> 3 after _@MS -> ok end.",
        "bar(F) -> XX = 1, receive F -> 3 end.",
        &[],
    );
    assert_matches(
        "ssr: receive _@XX -> 3 after _@MS -> ok end.",
        "bar(F) -> XX = 1, receive F -> 3 after 1000 -> ok end.",
        &[(
            "receive F -> 3 after 1000 -> ok end",
            &[("_@MS", &["1000"]), ("_@XX", &["F"])],
        )],
    );
    assert_matches(
        "ssr: receive _@XX -> true; _@AA -> _@BB after _@MS -> timeout end.",
        "bar() -> receive all_good -> true; X -> false after 1000 -> timeout end.",
        &[(
            "receive all_good -> true; X -> false after 1000 -> timeout end",
            &[
                ("_@AA", &["X"]),
                ("_@BB", &["false"]),
                ("_@MS", &["1000"]),
                ("_@XX", &["all_good"]),
            ],
        )],
    );
}

#[test]
fn ssr_expr_try() {
    assert_matches(
        r#"ssr:
             try _@AA of
                 _@XX -> ok
             catch
                 _@YY when true -> ok;
                 error:undef:_@Stack -> _@Stack
             after
                 ok
             end."#,
        r#"bar(F) ->
             try 1 of
                 2 -> ok
             catch
                 YY when true -> ok;
                 error:undef:Stack -> Stack
             after
                 ok
             end."#,
        &[(
            r#"try 1 of
                 2 -> ok
             catch
                 YY when true -> ok;
                 error:undef:Stack -> Stack
             after
                 ok
             end"#,
            &[
                ("_@AA", &["1"]),
                ("_@Stack", &["Stack", "Stack"]),
                ("_@XX", &["2"]),
                ("_@YY", &["YY"]),
            ],
        )],
    );
}

#[test]
fn ssr_expr_capture_fun() {
    assert_matches(
        "ssr: fun _@MODU:_@FUN/_@XX.",
        "bar(XX) -> YY = fun modu:fun/XX, YY.",
        &[(
            "fun modu:fun/XX",
            &[
                ("_@FUN", &["fun"]),
                ("_@MODU", &["modu"]),
                ("_@XX", &["XX"]),
            ],
        )],
    );
}

#[test]
fn ssr_expr_closure() {
    assert_matches(
        "ssr: fun (_@XX) -> _@YY end.",
        "bar(XX) -> F = fun (3) -> 7 end, F.",
        &[("fun (3) -> 7 end", &[("_@XX", &["3"]), ("_@YY", &["7"])])],
    );
    assert_matches(
        "ssr: fun _@Name(_@XX) -> _@YY end.",
        "bar(XX) -> F = fun Bar(3) -> 7 end, F.",
        &[(
            "fun Bar(3) -> 7 end",
            &[("_@Name", &["Bar"]), ("_@XX", &["3"]), ("_@YY", &["7"])],
        )],
    );
}

#[test]
fn ssr_expr_maybe() {
    assert_matches(
        "ssr: maybe _@AA ?= _@BB end.",
        "bar() -> maybe {ok, A} ?= ok_a() end.",
        &[(
            "maybe {ok, A} ?= ok_a() end",
            &[("_@AA", &["{ok, A}"]), ("_@BB", &["ok_a()"])],
        )],
    );
}

#[test]
fn ssr_expr_maybe_bare() {
    // Note: we make a full maybe expression, need some way of saying
    // it can have anything before or after.
    // TODO: T151843175, list item
    assert_matches(
        "ssr: _@AA ?= _@BB.",
        "bar() -> maybe {ok, A} ?= ok_a() end.",
        &[(
            "maybe {ok, A} ?= ok_a() end",
            &[("_@AA", &["{ok, A}"]), ("_@BB", &["ok_a()"])],
        )],
    );
}

#[test]
fn ssr_expr_parens() {
    let invisible_parens = Strategy {
        macros: MacroStrategy::Expand,
        parens: ParenStrategy::InvisibleParens,
    };
    let visible_parens = Strategy {
        macros: MacroStrategy::Expand,
        parens: ParenStrategy::VisibleParens,
    };
    assert_matches_with_strategy(
        visible_parens,
        "ssr: ((_@AA)).",
        "bar(X) -> X = ((3)),X = (4).",
        &[("((3))", &[("_@AA", &["3"])])],
    );
    assert_matches_with_strategy(
        visible_parens,
        "ssr: ((_@AA)).",
        "bar(((X))) -> X = 3,X = (4).",
        &[("((X))", &[("_@AA", &["X"])])],
    );
    // Under InvisibleParens the fold descends through Paren, so a
    // paren-wrapped match is reported at the inner ExprId's range —
    // "3" instead of "((3))", "4" instead of "(4)", "X" instead of
    // "((X))" on the Pat side.
    assert_matches_with_strategy(
        invisible_parens,
        "ssr: ((_@AA)).",
        "bar(X) -> X = ((3)),X = (4).",
        &[
            ("X", &[("_@AA", &["X"])]),
            ("X", &[("_@AA", &["X"])]),
            ("X = ((3))", &[("_@AA", &["X = ((3))"])]),
            ("3", &[("_@AA", &["3"])]),
            ("X = (4)", &[("_@AA", &["X = (4)"])]),
            ("X", &[("_@AA", &["X"])]),
            ("4", &[("_@AA", &["4"])]),
        ],
    );
    assert_matches_with_strategy(
        invisible_parens,
        "ssr: ((_@AA)).",
        "bar(((X))) -> X = 3,X = (4).",
        &[
            ("X", &[("_@AA", &["X"])]),
            ("X = 3", &[("_@AA", &["X = 3"])]),
            ("X", &[("_@AA", &["X"])]),
            ("3", &[("_@AA", &["3"])]),
            ("X = (4)", &[("_@AA", &["X = (4)"])]),
            ("X", &[("_@AA", &["X"])]),
            ("4", &[("_@AA", &["4"])]),
        ],
    );
}

// ---------------------------------------------------------------------

#[test]
fn ssr_pat_match() {
    assert_matches(
        "ssr: _@AA = _@BB.",
        "bar({ok, A} = B) -> A.",
        &[("{ok, A} = B", &[("_@AA", &["{ok, A}"]), ("_@BB", &["B"])])],
    );
}

#[test]
fn ssr_pat_list() {
    assert_matches(
        "ssr: [ _@A, _@B | _@C].",
        "fn(X) -> [1, 2 | [Y]] = X.",
        &[(
            "[1, 2 | [Y]]",
            &[("_@A", &["1"]), ("_@B", &["2"]), ("_@C", &["[Y]"])],
        )],
    );
}

#[test]
fn ssr_pat_binary() {
    assert_matches(
        "ssr: << _@A, _@B>>.",
        "fn(Y) -> <<X,Z>> = Y.",
        &[("<<X,Z>>", &[("_@A", &["X"]), ("_@B", &["Z"])])],
    );
}

#[test]
fn ssr_pat_unary_op() {
    assert_matches(
        "ssr: not _@A.",
        "fn(Y) -> {not {X}} = Y.",
        &[("not {X}", &[("_@A", &["{X}"])])],
    );
    assert_matches("ssr: bnot _@A.", "fn(Y) -> {not {X}} = Y.", &[]);
    assert_matches(
        "ssr: + _@A.",
        "fn(Y) -> {+X} = Y.",
        &[("+X", &[("_@A", &["X"])])],
    );
    assert_matches(
        "ssr: - _@A.",
        "fn(Y) -> {-X} = Y.",
        &[("-X", &[("_@A", &["X"])])],
    );
}

#[test]
fn ssr_pat_binary_op() {
    assert_matches(
        "ssr: _@A + _@B.",
        "fn(Y) -> {X + 1} = Y.",
        &[("X + 1", &[("_@A", &["X"]), ("_@B", &["1"])])],
    );
    assert_matches("ssr: _@A - _@B.", "fn(Y) -> {X + 1} = Y.", &[]);

    assert_matches(
        "ssr: _@A + _@B.",
        "fn(X) -> Y = {X + 1} = Y.",
        &[("X + 1", &[("_@A", &["X"]), ("_@B", &["1"])])],
    );
    assert_matches("ssr: _@A - _@B.", "fn(X) -> Y = {X + 1} =  Y.", &[]);
    assert_matches(
        "ssr: _@A and _@B.",
        "fn(X,Y) -> X and Y = true.",
        &[("X and Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A andalso _@B.",
        "fn(X,Y) -> X andalso Y = true.",
        &[("X andalso Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A or _@B.",
        "fn(X,Y) -> X or Y = true.",
        &[("X or Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A orelse _@B.",
        "fn(X,Y) -> X orelse Y = true.",
        &[("X orelse Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A ! _@B.",
        "fn(X,Y,Z) -> X ! Y = Z .",
        &[("X ! Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A == _@B.",
        "fn(X,Y) -> X == Y = true.",
        &[("X == Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A /= _@B.",
        "fn(X,Y) -> X /= Y = true .",
        &[("X /= Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A =< _@B.",
        "fn(X,Y) -> X =< Y = true.",
        &[("X =< Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A  < _@B.",
        "fn(X,Y) -> X  < Y = true.",
        &[("X  < Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A >= _@B.",
        "fn(X,Y) -> X >= Y = true.",
        &[("X >= Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A >  _@B.",
        "fn(X,Y) -> X >  Y = true.",
        &[("X >  Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A =:= _@B.",
        "fn(X,Y) -> X =:= Y = true.",
        &[("X =:= Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A =/= _@B.",
        "fn(X,Y) -> X =/= Y = true.",
        &[("X =/= Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A ++ _@B.",
        "fn(X,Y) -> X ++ Y = [].",
        &[("X ++ Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A -- _@B.",
        "fn(X,Y) -> X -- Y = [].",
        &[("X -- Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A + _@B.",
        "fn(X,Y) -> X + Y = 5.",
        &[("X + Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A - _@B.",
        "fn(X,Y) -> X - Y = 3.",
        &[("X - Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A bor _@B.",
        "fn(X,Y,Z) -> X bor Y = Z.",
        &[("X bor Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A bxor _@B.",
        "fn(X,Y,Z) -> X bxor Y = Z.",
        &[("X bxor Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A bsl _@B.",
        "fn(X,Y,Z) -> X bsl Y = Z.",
        &[("X bsl Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A bsr _@B.",
        "fn(X,Y,Z) -> X bsr Y = Z.",
        &[("X bsr Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A or _@B.",
        "fn(X,Y) -> X or Y = true.",
        &[("X or Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A / _@B.",
        "fn(X,Y,Z) -> X / Y = Z.",
        &[("X / Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A * _@B.",
        "fn(X,Y) -> X * Y = 10.",
        &[("X * Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A div _@B.",
        "fn(X,Y,A) -> X div Y =Z .",
        &[("X div Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A rem _@B.",
        "fn(X,Y) -> X rem Y = 0.",
        &[("X rem Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A band _@B.",
        "fn(X,Y,Z) -> X band Y =Z .",
        &[("X band Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
    assert_matches(
        "ssr: _@A and _@B.",
        "fn(X,Y) -> X and Y = true.",
        &[("X and Y", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );

    // ---------------------------------

    assert_matches(
        "ssr: _@A + _@B.",
        "fn(Y) -> {X + 1 + 2} = Y.",
        &[
            ("X + 1 + 2", &[("_@A", &["X + 1"]), ("_@B", &["2"])]),
            ("X + 1", &[("_@A", &["X"]), ("_@B", &["1"])]),
        ],
    );
}

#[test]
fn ssr_pat_match_record() {
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn(X) -> #foo{ka1 = a, ka2 = <<\"blah\">>, ka3 = {c, d}} = X.",
        &[],
    );
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn(X) -> #boo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}} = X.",
        &[],
    );
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn(X) -> #foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}} = X.",
        &[(
            "#foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}",
            &[
                ("_@A", &["a"]),
                ("_@B", &["<<\"blah\">>"]),
                ("_@C", &["{c, d}"]),
            ],
        )],
    );
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = {_@C, _@D}}.",
        "fn(X) -> #foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}} = X.",
        &[(
            "#foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}",
            &[
                ("_@A", &["a"]),
                ("_@B", &["<<\"blah\">>"]),
                ("_@C", &["c"]),
                ("_@D", &["d"]),
            ],
        )],
    );
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = {_@C, _@D}}.",
        "fn(X) -> #foo{k3 = {c, d}, k2 = a, k1 = <<\"blah\">>} = X.",
        &[(
            "#foo{k3 = {c, d}, k2 = a, k1 = <<\"blah\">>}",
            &[
                ("_@A", &["<<\"blah\">>"]),
                ("_@B", &["a"]),
                ("_@C", &["c"]),
                ("_@D", &["d"]),
            ],
        )],
    );
}

#[test]
fn ssr_pat_record_index() {
    assert_matches(
        "ssr: #a_record.a_field.",
        "bar(XX) -> #record.field = XX.",
        &[],
    );
    assert_matches(
        "ssr: #record.field.",
        "bar(XX) -> #record.field = XX.",
        &[("#record.field", &[])],
    );
}

#[test]
fn ssr_pat_match_map() {
    assert_matches(
        "ssr: #{ foo => _@A }.",
        "bar(YY) -> #{foo := XX} = YY.",
        &[],
    );
    assert_matches(
        "ssr: #{ field := _@A }.",
        "bar(YY) -> #{foo := XX} = YY.",
        &[],
    );
    assert_matches(
        "ssr: #{ foo := _@A }.",
        "bar(YY) -> #{foo := XX} = YY.",
        &[("#{foo := XX}", &[("_@A", &["XX"])])],
    );
    assert_matches(
        "ssr: #{ a := _@A, b := _@B }.",
        "bar(YY) -> #{b := 1, a := XX} = YY.",
        &[("#{b := 1, a := XX}", &[("_@A", &["XX"]), ("_@B", &["1"])])],
    );
}

#[test]
fn ssr_pat_match_macro_call() {
    // TODO: fails because we do not have a visible macro call in the
    // template, only Missing
    // And it comes down to having some sort of meaningful fold option
    // that gives the surface call, and the expansion. Maybe try both?
    assert_matches_with_strategy(
        // The default, if not given
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        "ssr: ?BAR(_@AA).",
        "-define(BAR(X), {X}).
         bar(?BAR(4)) -> ok.",
        &[],
    );
    assert_matches_with_strategy(
        Strategy {
            macros: MacroStrategy::ExpandButIncludeMacroCall,
            parens: ParenStrategy::InvisibleParens,
        },
        "ssr: ?BAR(_@AA).",
        "bar(?BAR(4)) -> ok.",
        &[("?BAR(4)", &[("_@AA", &["4"])])],
    );
    assert_matches_with_strategy(
        Strategy {
            macros: MacroStrategy::ExpandButIncludeMacroCall,
            parens: ParenStrategy::InvisibleParens,
        },
        "ssr: ?BAR(_@AA).",
        "bar(?NOT_BAR(4)) -> ok.",
        &[],
    );
    assert_matches_with_strategy(
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        },
        "ssr: ?BAR(_@AA).",
        "-define(BAR(X), {X}).
         bar(?BAR(4)) -> ok.",
        &[("?BAR(4)", &[("_@AA", &["4"])])],
    );
    assert_matches_with_strategy(
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        "ssr: {_@AA}.",
        "-define(BAR(X), {X}).
         bar(?BAR(4)) -> ok.",
        &[
            ("{X}", &[("_@AA", &["X"])]),
            ("?BAR(4)", &[("_@AA", &["4"])]),
        ],
    );
}

// ---------------------------------------------------------------------
// Types.  Initially ensure we do not blow up on matches

#[test]
fn ssr_spec_no_blowup() {
    assert_matches("ssr: _@X = 1.", "-spec foo() -> ok.", &[]);
}

// ---------------------------------------------------------------------
// Terms.  Initially ensure we do not blow up on matches

#[test]
fn ssr_term_no_blowup() {
    assert_matches("ssr: _@X = 1.", "-wild_attr([any,thing]).", &[]);
}

// ---------------------------------------------------------------------
// Check conditions

#[test]
fn ssr_match_constant_atom_placeholder() {
    assert_matches(
        "ssr: {_@X = _@Y} where _@X == foo.",
        "foo() -> {foo = 3},{bar = 2}.",
        &[("{foo = 3}", &[("_@X", &["foo"]), ("_@Y", &["3"])])],
    );
}

#[test]
fn ssr_match_constant_negated_atom_placeholder() {
    assert_matches(
        "ssr: {_@X = _@Y} where _@X =/= foo.",
        "foo() -> {foo = 3},{bar = 2}.",
        &[("{bar = 2}", &[("_@X", &["bar"]), ("_@Y", &["2"])])],
    );
}

#[test]
fn ssr_invalid_when_condition_not_literal() {
    expect![[r#"
        "Parse error: Invalid `where` RHS, expecting a literal"
    "#]]
    .assert_debug_eq(&parse_error_text("ssr: {_@X = _@Y} where _@X == {Y}."));
}

#[test]
fn ssr_invalid_when_condition_not_compop() {
    expect![[r#"
        "Parse error: Invalid `where` condition"
    "#]]
    .assert_debug_eq(&parse_error_text("ssr: {_@X = _@Y} where _@X = foo."));
}

#[test]
fn ssr_invalid_when_guard_erlang_prefix_rejected() {
    // Only the local-call form (`is_atom(_@X)`) is supported. The
    // qualified form `erlang:is_atom(_@X)` is rejected because SSR's
    // match semantics for qualified calls differ from Erlang's.
    expect![[r#"
        "Parse error: Invalid `where` guard `erlang:is_atom/1`: only local-call form is supported, e.g. `is_atom(_@X)`"
    "#]]
    .assert_debug_eq(&parse_error_text(
        "ssr: f(_@A) where erlang:is_atom(_@A).",
    ));
}

#[test]
fn ssr_invalid_when_guard_unsupported_predicate() {
    // `is_float/1` is a legal Erlang guard BIF but not one SSR
    // recognises. Surface an error instead of silently dropping the
    // condition (previously this would compile but match anything).
    expect![[r#"
        "Parse error: Invalid `where` guard `is_float/1`: unsupported predicate"
    "#]]
    .assert_debug_eq(&parse_error_text("ssr: f(_@A) where is_float(_@A)."));
}

#[test]
fn ssr_match_constant_var_placeholder() {
    assert_matches(
        "ssr: _@X where _@X == A.",
        "foo(A) -> 3.",
        &[("A", &[("_@X", &["A"])])],
    );
    assert_matches(
        "ssr: _@X where _@X == _.",
        "foo(_) -> 3.",
        &[("_", &[("_@X", &["_"])])],
    );
}

// ---------------------------------------------------------------------

#[test]
fn ssr_retrieve_match_placeholder() {
    assert_match_placeholder(
        "ssr: {_@X = _@Y} where _@X =/= foo.",
        "foo() -> {foo = 3},{bar = 2}.",
        &["{bar = 2}"],
        "_@X",
        expect![[r#"
            Some(
                [
                    PlaceholderMatch {
                        range: FileRange {
                            file_id: FileId(
                                0,
                            ),
                            range: 20..23,
                        },
                        code_id: AnyExprId(
                            Pat(
                                Idx::<Pat>(1),
                            ),
                        ),
                        inner_matches: SsrMatches {
                            matches: [],
                        },
                    },
                ],
            )
        "#]],
    )
}

#[test]
fn ssr_retrieve_match_placeholder_multiple_matches() {
    assert_match_placeholder(
        "ssr: {_@X,_@X}.",
        "foo() -> {bar, bar}.",
        &["{bar, bar}"],
        "_@X",
        expect![[r#"
            Some(
                [
                    PlaceholderMatch {
                        range: FileRange {
                            file_id: FileId(
                                0,
                            ),
                            range: 10..13,
                        },
                        code_id: AnyExprId(
                            Expr(
                                Idx::<Expr>(1),
                            ),
                        ),
                        inner_matches: SsrMatches {
                            matches: [],
                        },
                    },
                    PlaceholderMatch {
                        range: FileRange {
                            file_id: FileId(
                                0,
                            ),
                            range: 15..18,
                        },
                        code_id: AnyExprId(
                            Expr(
                                Idx::<Expr>(2),
                            ),
                        ),
                        inner_matches: SsrMatches {
                            matches: [],
                        },
                    },
                ],
            )
        "#]],
    )
}

#[test]
fn ssr_repeated_placeholder() {
    assert_matches(
        "ssr: << _@A, _@A>>.",
        "fn(Y) -> X=1, <<X,X>>.",
        &[("<<X,X>>", &[("_@A", &["X", "X"])])],
    );
    assert_matches(
        "ssr: { _@A, _@A}.",
        "fn(Y) -> {[a,b,Y],[a,b,Y]}.",
        &[("{[a,b,Y],[a,b,Y]}", &[("_@A", &["[a,b,Y]", "[a,b,Y]"])])],
    );
    assert_matches("ssr: <<_@A, _@A>>.", "fn(Y) -> X=1, <<X,Y>>.", &[]);
}

#[test]
fn ssr_do_not_match_pattern_missing() {
    assert_matches(
        "ssr: maps:update(_@Key,_@Value,_@Map).",
        r#"
         g() ->
             <<$",$\\,194,181,$A,$">> =
                   """
                   "\\µA"
                   """.
         "#,
        &[],
    );
}

#[test]
fn ssr_complex_match() {
    assert_matches(
        "ssr: lists:foldl(fun({_@Key,_@Value}, _@Acc) -> _@Acc#{_@Key => _@Value} end, #{}, _@List).",
        r#"
         fn(List) -> lists:foldl(fun({K,V}, Acc) -> Acc#{K => V} end, #{}, List).
         "#,
        &[(
            "lists:foldl(fun({K,V}, Acc) -> Acc#{K => V} end, #{}, List)",
            &[
                ("_@Acc", &["Acc", "Acc"]),
                ("_@Key", &["K", "K"]),
                ("_@List", &["List"]),
                ("_@Value", &["V", "V"]),
            ],
        )],
    );
}

#[test]
fn ssr_matches_in_multiple_forms() {
    assert_matches(
        "ssr: {_@A, 3}.",
        r#"
         fn({a, 3}) -> ok;
         fn({b, 3}) -> ok.
         "#,
        &[
            ("{a, 3}", &[("_@A", &["a"])]),
            ("{b, 3}", &[("_@A", &["b"])]),
        ],
    );
}

#[test]
fn ssr_comments_in_match() {
    let pattern = "ssr: << _@A, _@B>>.";
    let code = r#"
                 foo() ->
                    <<{ % preceding comment
                        3},
                        { 3 % following comment
                        }>>.
                "#;
    let strategy = Strategy {
        macros: MacroStrategy::Expand,
        parens: ParenStrategy::InvisibleParens,
    };

    let (db, position, _selections) = single_file(code);
    let sema = Semantic::new(&db);
    let pattern = SsrRule::parse_str(sema.db, pattern).unwrap();
    let mut match_finder =
        MatchFinder::in_context(&sema, strategy, SsrSearchScope::WholeFile(position.file_id));
    match_finder.debug_print = false;
    match_finder.add_search_pattern(pattern);
    let matches = match_finder.matches().flattened();
    let match_ = matches.matches[0].clone(); // Its a test, panic is fine.
    expect![[r#"
        Some(
            [
                Comment {
                    syntax: COMMENT@16..35
                      COMMENT@16..35 "% preceding comment"
                    ,
                },
                Comment {
                    syntax: COMMENT@58..77
                      COMMENT@58..77 "% following comment"
                    ,
                },
            ],
        )
    "#]]
    .assert_debug_eq(&match_.comments(&sema));

    let a_match = match_.get_placeholder_match("_@A").unwrap();
    let (_body, body_map) = match_.matched_node_body.get_body_and_map(&sema).unwrap();
    expect![[r#"
        Some(
            [
                Comment {
                    syntax: COMMENT@16..35
                      COMMENT@16..35 "% preceding comment"
                    ,
                },
            ],
        )
    "#]]
    .assert_debug_eq(&a_match.comments(&sema, &body_map));

    let b_match = match_.get_placeholder_match("_@B").unwrap();
    expect![[r#"
        Some(
            [
                Comment {
                    syntax: COMMENT@58..77
                      COMMENT@58..77 "% following comment"
                    ,
                },
            ],
        )
    "#]]
    .assert_debug_eq(&b_match.comments(&sema, &body_map));
}

#[test]
fn ssr_underscore_pattern_in_code_does_not_match_atom_in_ssr_single_branch() {
    assert_matches(
        "ssr: case X of foo -> 1 end.",
        "foo(X) -> case X of _ -> 1 end.",
        &[],
    )
}

#[test]
fn ssr_underscore_pattern_in_code_does_not_match_atom_in_ssr_multi_branch() {
    assert_matches(
        "ssr: case X of true -> a; false -> b end.",
        "foo(X) -> case X of true -> a; _ -> b end.",
        &[],
    );
}

#[test]
fn ssr_patterns_and_branches_correspond() {
    assert_match_placeholder(
        "ssr: case X of a -> _@BranchA; _@PatB -> _@BranchB end.",
        "foo(X) -> case X of a -> branch_a; _ -> branch_b end.",
        &["case X of a -> branch_a; _ -> branch_b end"],
        "_@BranchA",
        expect![[r#"
            Some(
                [
                    PlaceholderMatch {
                        range: FileRange {
                            file_id: FileId(
                                0,
                            ),
                            range: 25..33,
                        },
                        code_id: AnyExprId(
                            Expr(
                                Idx::<Expr>(2),
                            ),
                        ),
                        inner_matches: SsrMatches {
                            matches: [],
                        },
                    },
                ],
            )
        "#]],
    );
    assert_match_placeholder(
        "ssr: case X of a -> _@BranchA; _@PatB -> _@BranchB end.",
        "foo(X) -> case X of a -> branch_a; _ -> branch_b end.",
        &["case X of a -> branch_a; _ -> branch_b end"],
        "_@PatB",
        expect![[r#"
            Some(
                [
                    PlaceholderMatch {
                        range: FileRange {
                            file_id: FileId(
                                0,
                            ),
                            range: 35..36,
                        },
                        code_id: AnyExprId(
                            Pat(
                                Idx::<Pat>(2),
                            ),
                        ),
                        inner_matches: SsrMatches {
                            matches: [],
                        },
                    },
                ],
            )
        "#]],
    );
    assert_match_placeholder(
        "ssr: case X of a -> _@BranchA; _@PatB -> _@BranchB end.",
        "foo(X) -> case X of a -> branch_a; _ -> branch_b end.",
        &["case X of a -> branch_a; _ -> branch_b end"],
        "_@BranchB",
        expect![[r#"
            Some(
                [
                    PlaceholderMatch {
                        range: FileRange {
                            file_id: FileId(
                                0,
                            ),
                            range: 40..48,
                        },
                        code_id: AnyExprId(
                            Expr(
                                Idx::<Expr>(3),
                            ),
                        ),
                        inner_matches: SsrMatches {
                            matches: [],
                        },
                    },
                ],
            )
        "#]],
    );
}

#[test]
fn ssr_placeholder_does_not_match_already_matched_arm() {
    assert_match_placeholder(
        "ssr: case X of foo -> bar; _@Pat -> _@Branch end.",
        "foo(X) -> case X of foo -> bar; pat -> branch end.",
        &["case X of foo -> bar; pat -> branch end"],
        "_@Pat",
        expect![[r#"
            Some(
                [
                    PlaceholderMatch {
                        range: FileRange {
                            file_id: FileId(
                                0,
                            ),
                            range: 32..35,
                        },
                        code_id: AnyExprId(
                            Pat(
                                Idx::<Pat>(2),
                            ),
                        ),
                        inner_matches: SsrMatches {
                            matches: [],
                        },
                    },
                ],
            )
        "#]],
    )
}

#[test]
fn ssr_predicates_on_match_expr_pat() {
    let pattern = "ssr: {_@S, _@V, _@A}.";
    let code = r#"
                %-compile({"str", var, atom}).
                %-type foo(V) :: {"str", V, atom}.
                foo({"hello", Var, atom}) -> {"str", Var, aa}.

                "#;
    let strategy = Strategy {
        macros: MacroStrategy::Expand,
        parens: ParenStrategy::InvisibleParens,
    };

    let (db, position, _selections) = single_file(code);
    let sema = Semantic::new(&db);
    let pattern = SsrRule::parse_str(sema.db, pattern).unwrap();
    let mut match_finder =
        MatchFinder::in_context(&sema, strategy, SsrSearchScope::WholeFile(position.file_id));
    match_finder.debug_print = false;
    match_finder.add_search_pattern(pattern);
    let matches = match_finder.matches().flattened();

    let match_pat = matches.matches[0].clone(); // Its a test, panic is fine.
    expect![[r#"
        Some(
            Normal(
                "hello",
            ),
        )
    "#]]
    .assert_debug_eq(&match_pat.placeholder_is_string(&sema, "_@S"));
    assert!(match_pat.placeholder_is_var(&sema, "_@S").is_none());
    assert!(match_pat.placeholder_is_atom(&sema, "_@S").is_none());

    // ------------

    assert!(match_pat.placeholder_is_string(&sema, "_@V").is_none());
    expect![[r#"
        Some(
            Var(
                u!("Var"),
            ),
        )
    "#]]
    .assert_debug_eq(&match_pat.placeholder_is_var(&sema, "_@V"));
    assert!(match_pat.placeholder_is_atom(&sema, "_@V").is_none());

    // ------------

    assert!(match_pat.placeholder_is_string(&sema, "_@A").is_none());
    assert!(match_pat.placeholder_is_var(&sema, "_@A").is_none());
    expect![[r#"
        Some(
            Atom(
                u!("atom"),
            ),
        )
    "#]]
    .assert_debug_eq(&match_pat.placeholder_is_atom(&sema, "_@A"));

    // ----------------------
    let match_expr = matches.matches[1].clone(); // Its a test, panic is fine.
    assert!(match_expr.placeholder_is_string(&sema, "_@V").is_none());
    expect![[r#"
        Some(
            Var(
                u!("Var"),
            ),
        )
    "#]]
    .assert_debug_eq(&match_expr.placeholder_is_var(&sema, "_@V"));
    assert!(match_expr.placeholder_is_atom(&sema, "_@V").is_none());

    // ------------

    assert!(match_expr.placeholder_is_string(&sema, "_@V").is_none());
    expect![[r#"
        Some(
            Var(
                u!("Var"),
            ),
        )
    "#]]
    .assert_debug_eq(&match_expr.placeholder_is_var(&sema, "_@V"));
    assert!(match_expr.placeholder_is_atom(&sema, "_@V").is_none());

    // ------------

    assert!(match_expr.placeholder_is_string(&sema, "_@A").is_none());
    assert!(match_expr.placeholder_is_var(&sema, "_@A").is_none());
    expect![[r#"
        Some(
            Atom(
                u!("aa"),
            ),
        )
    "#]]
    .assert_debug_eq(&match_expr.placeholder_is_atom(&sema, "_@A"));
}

// ---------------------------------------------------------------------
// Native records (EEP 79)

#[test]
fn ssr_native_record_qualified_create() {
    assert_matches(
        "ssr: #mod:name{k1 = _@A, k2 = _@B}.",
        "fn() -> X = #mod:name{k1 = a, k2 = <<\"blah\">>}, X.",
        &[(
            "#mod:name{k1 = a, k2 = <<\"blah\">>}",
            &[("_@A", &["a"]), ("_@B", &["<<\"blah\">>"])],
        )],
    );
}

#[test]
fn ssr_native_record_qualified_create_wrong_name() {
    assert_matches(
        "ssr: #mod:name{k1 = _@A}.",
        "fn() -> X = #mod:other{k1 = a}, X.",
        &[],
    );
    assert_matches(
        "ssr: #mod:name{k1 = _@A}.",
        "fn() -> X = #other:name{k1 = a}, X.",
        &[],
    );
}

#[test]
fn ssr_native_record_qualified_create_unordered() {
    assert_matches(
        "ssr: #mod:name{k1 = _@A, k2 = _@B, k3 = _@C}.",
        "fn() -> X = #mod:name{k2 = a, k3 = <<\"blah\">>, k1 = {c, d}}, X.",
        &[(
            "#mod:name{k2 = a, k3 = <<\"blah\">>, k1 = {c, d}}",
            &[
                ("_@A", &["{c, d}"]),
                ("_@B", &["a"]),
                ("_@C", &["<<\"blah\">>"]),
            ],
        )],
    );
}

#[test]
fn ssr_native_record_anon_create() {
    assert_matches(
        "ssr: #_{k1 = _@A, k2 = _@B}.",
        "fn() -> X = #_{k1 = a, k2 = <<\"blah\">>}, X.",
        &[(
            "#_{k1 = a, k2 = <<\"blah\">>}",
            &[("_@A", &["a"]), ("_@B", &["<<\"blah\">>"])],
        )],
    );
}

#[test]
fn ssr_native_record_anon_does_not_match_qualified() {
    assert_matches(
        "ssr: #_{k1 = _@A}.",
        "fn() -> X = #mod:name{k1 = a}, X.",
        &[],
    );
}

#[test]
fn ssr_native_record_qualified_update() {
    assert_matches(
        "ssr: _@A#mod:name{field = _@B}.",
        "bar(List) -> XX = 1, List#mod:name{field = XX}.",
        &[(
            "List#mod:name{field = XX}",
            &[("_@A", &["List"]), ("_@B", &["XX"])],
        )],
    );
}

#[test]
fn ssr_native_record_qualified_update_wrong_name() {
    assert_matches(
        "ssr: _@A#mod:name{field = _@B}.",
        "bar(List) -> XX = 1, List#mod:other{field = XX}.",
        &[],
    );
}

#[test]
fn ssr_native_record_anon_update() {
    assert_matches(
        "ssr: _@A#_{field = _@B}.",
        "bar(List) -> XX = 1, List#_{field = XX}.",
        &[(
            "List#_{field = XX}",
            &[("_@A", &["List"]), ("_@B", &["XX"])],
        )],
    );
}

#[test]
fn ssr_native_record_qualified_field_access() {
    assert_matches(
        "ssr: _@A#mod:name.field.",
        "bar(List) -> XX = List#mod:name.field, XX.",
        &[("List#mod:name.field", &[("_@A", &["List"])])],
    );
}

#[test]
fn ssr_native_record_qualified_field_access_wrong_name() {
    assert_matches(
        "ssr: _@A#mod:name.field.",
        "bar(List) -> XX = List#mod:other.field, XX.",
        &[],
    );
}

#[test]
fn ssr_native_record_qualified_field_access_wrong_field() {
    assert_matches(
        "ssr: _@A#mod:name.field.",
        "bar(List) -> XX = List#mod:name.other, XX.",
        &[],
    );
}

#[test]
fn ssr_native_record_anon_field_access() {
    assert_matches(
        "ssr: _@A#_.field.",
        "bar(List) -> XX = List#_.field, XX.",
        &[("List#_.field", &[("_@A", &["List"])])],
    );
}

// Native records in pattern position

#[test]
fn ssr_native_record_qualified_create_in_pat() {
    assert_matches(
        "ssr: #mod:name{k1 = _@A, k2 = _@B}.",
        "fn(#mod:name{k1 = X, k2 = Y}) -> {X, Y}.",
        &[(
            "#mod:name{k1 = X, k2 = Y}",
            &[("_@A", &["X"]), ("_@B", &["Y"])],
        )],
    );
}

#[test]
fn ssr_native_record_qualified_create_in_pat_wrong_name() {
    assert_matches(
        "ssr: #mod:name{k1 = _@A}.",
        "fn(#mod:other{k1 = X}) -> X.",
        &[],
    );
    assert_matches(
        "ssr: #mod:name{k1 = _@A}.",
        "fn(#other:name{k1 = X}) -> X.",
        &[],
    );
}

#[test]
fn ssr_native_record_qualified_create_in_pat_unordered() {
    assert_matches(
        "ssr: #mod:name{k1 = _@A, k2 = _@B}.",
        "fn(#mod:name{k2 = Y, k1 = X}) -> {X, Y}.",
        &[(
            "#mod:name{k2 = Y, k1 = X}",
            &[("_@A", &["X"]), ("_@B", &["Y"])],
        )],
    );
}

#[test]
fn ssr_native_record_anon_create_in_pat() {
    assert_matches(
        "ssr: #_{k1 = _@A, k2 = _@B}.",
        "fn(#_{k1 = X, k2 = Y}) -> {X, Y}.",
        &[("#_{k1 = X, k2 = Y}", &[("_@A", &["X"]), ("_@B", &["Y"])])],
    );
}

#[test]
fn ssr_native_record_anon_in_pat_does_not_match_qualified() {
    assert_matches("ssr: #_{k1 = _@A}.", "fn(#mod:name{k1 = X}) -> X.", &[]);
}

// ---------------------------------------------------------------------
// Per-placeholder kind constraints via `when is_<kind>(_@X)`.
//
// Recognised guard predicates: standard Erlang BIFs
// `is_atom/is_integer/is_function/is_list/is_tuple/is_map/is_binary`,
// plus SSR-only `is_var` and `is_call` for AST shapes without a native
// BIF.

#[test]
fn ssr_kind_constraint_atom_accept() {
    assert_matches(
        "ssr: f(_@A) where is_atom(_@A).",
        "fn() -> f(an_atom).",
        &[("f(an_atom)", &[("_@A", &["an_atom"])])],
    );
}

#[test]
fn ssr_kind_constraint_atom_reject_integer() {
    assert_matches("ssr: f(_@A) where is_atom(_@A).", "fn() -> f(42).", &[]);
}

#[test]
fn ssr_kind_constraint_integer_accept() {
    assert_matches(
        "ssr: f(_@A) where is_integer(_@A).",
        "fn() -> f(42).",
        &[("f(42)", &[("_@A", &["42"])])],
    );
}

#[test]
fn ssr_kind_constraint_list_accept() {
    assert_matches(
        "ssr: f(_@A) where is_list(_@A).",
        "fn() -> f([1, 2, 3]).",
        &[("f([1, 2, 3])", &[("_@A", &["[1, 2, 3]"])])],
    );
}

#[test]
fn ssr_kind_constraint_tuple_accept() {
    assert_matches(
        "ssr: f(_@A) where is_tuple(_@A).",
        "fn() -> f({x, y}).",
        &[("f({x, y})", &[("_@A", &["{x, y}"])])],
    );
}

#[test]
fn ssr_kind_constraint_call_accept_ssr_predicate() {
    // `is_call/1` has no Erlang BIF counterpart — it is an SSR-only
    // local predicate that matches any `Expr::Call` AST node.
    assert_matches(
        "ssr: f(_@A) where is_call(_@A).",
        "fn() -> f(g(1)).",
        &[("f(g(1))", &[("_@A", &["g(1)"])])],
    );
}

#[test]
fn ssr_kind_constraint_var_accept_ssr_predicate() {
    // `is_var/1` is also SSR-only — matches any `Expr::Var` /
    // `Pat::Var` AST node, regardless of binding state.
    assert_matches(
        "ssr: f(_@A) where is_var(_@A).",
        "fn(X) -> f(X).",
        &[("f(X)", &[("_@A", &["X"])])],
    );
}

#[test]
fn ssr_kind_constraint_cross_kind_mismatch() {
    assert_matches(
        "ssr: f(_@A) where is_function(_@A).",
        "fn() -> f([1, 2, 3]).",
        &[],
    );
}

/// `is_function/1` covers anonymous funs and `fun M:F/A` / `fun F/A`
/// captures alike — all lower to `Expr::Closure` or `Expr::CaptureFun`.
#[test]
fn ssr_kind_constraint_fun_accepts_anonymous() {
    assert_matches(
        "ssr: f(_@A) where is_function(_@A).",
        "fn() -> f(fun(X) -> X end).",
        &[("f(fun(X) -> X end)", &[("_@A", &["fun(X) -> X end"])])],
    );
}

#[test]
fn ssr_kind_constraint_fun_accepts_local_capture() {
    assert_matches(
        "ssr: f(_@A) where is_function(_@A).",
        "fn() -> f(fun func/1).",
        &[("f(fun func/1)", &[("_@A", &["fun func/1"])])],
    );
}

#[test]
fn ssr_kind_constraint_fun_accepts_remote_capture() {
    assert_matches(
        "ssr: f(_@A) where is_function(_@A).",
        "fn() -> f(fun mod:func/1).",
        &[("f(fun mod:func/1)", &[("_@A", &["fun mod:func/1"])])],
    );
}

/// `is_tuple/1` is position-agnostic: matches a tuple in either
/// expression or pattern position.
#[test]
fn ssr_kind_constraint_tuple_in_pat_position() {
    assert_matches(
        "ssr: _@A = rhs.",
        "fn() -> {x, y} = rhs.",
        &[("{x, y} = rhs", &[("_@A", &["{x, y}"])])],
    );
    assert_matches(
        "ssr: _@A = rhs where is_tuple(_@A).",
        "fn() -> {x, y} = rhs.",
        &[("{x, y} = rhs", &[("_@A", &["{x, y}"])])],
    );
}

/// Multi-placeholder AND across two different placeholders.
#[test]
fn ssr_kind_constraint_multi_placeholder_and() {
    assert_matches(
        "ssr: f(_@A, _@B) where is_tuple(_@A), is_atom(_@B).",
        "fn() -> f({x, y}, an_atom).",
        &[(
            "f({x, y}, an_atom)",
            &[("_@A", &["{x, y}"]), ("_@B", &["an_atom"])],
        )],
    );
    assert_matches(
        "ssr: f(_@A, _@B) where is_tuple(_@A), is_atom(_@B).",
        "fn() -> f({x, y}, [1, 2]).",
        &[],
    );
}

/// `Kind` and `Literal` conditions on different placeholders AND together.
#[test]
fn ssr_kind_constraint_ands_with_when_literal_diff_placeholders() {
    assert_matches(
        "ssr: f(_@A, _@B) where is_tuple(_@A), _@B == foo.",
        "fn() -> f({x}, foo).",
        &[("f({x}, foo)", &[("_@A", &["{x}"]), ("_@B", &["foo"])])],
    );
    assert_matches(
        "ssr: f(_@A, _@B) where is_tuple(_@A), _@B == foo.",
        "fn() -> f({x}, bar).",
        &[],
    );
}

/// `Kind` and `Literal` on the *same* placeholder AND together — both
/// must pass. Regression for a former `insert`-based bug that would
/// drop one of the two conditions.
#[test]
fn ssr_kind_constraint_ands_with_when_literal_same_placeholder() {
    assert_matches(
        "ssr: f(_@A) where is_atom(_@A), _@A == foo.",
        "fn() -> f(foo).",
        &[("f(foo)", &[("_@A", &["foo"])])],
    );
    assert_matches(
        "ssr: f(_@A) where is_atom(_@A), _@A == foo.",
        "fn() -> f(bar).",
        &[],
    );
    assert_matches(
        "ssr: f(_@A) where is_tuple(_@A), _@A == foo.",
        "fn() -> f(foo).",
        &[],
    );
}

/// `not is_<kind>(_@A)` negates the kind check.
#[test]
fn ssr_kind_constraint_negation() {
    assert_matches(
        "ssr: f(_@A) where not is_atom(_@A).",
        "fn() -> f(42).",
        &[("f(42)", &[("_@A", &["42"])])],
    );
    assert_matches(
        "ssr: f(_@A) where not is_atom(_@A).",
        "fn() -> f(an_atom).",
        &[],
    );
}

// ---------------------------------------------------------------------
// `;` disjunction (Erlang guard OR), with Erlang-guard precedence:
// `G1, G2 ; G3, G4` is `(G1 ∧ G2) ∨ (G3 ∧ G4)`.

/// Single-placeholder disjunction across kinds.
#[test]
fn ssr_when_or_single_placeholder_kinds() {
    let pat = "ssr: f(_@A) where is_atom(_@A); is_integer(_@A).";
    assert_matches(
        pat,
        "fn() -> f(an_atom).",
        &[("f(an_atom)", &[("_@A", &["an_atom"])])],
    );
    assert_matches(pat, "fn() -> f(42).", &[("f(42)", &[("_@A", &["42"])])]);
    assert_matches(pat, "fn() -> f([1, 2, 3]).", &[]);
}

/// Disjunction over different placeholders: either A or B is an atom.
#[test]
fn ssr_when_or_different_placeholders() {
    let pat = "ssr: f(_@A, _@B) where is_atom(_@A); is_atom(_@B).";
    assert_matches(
        pat,
        "fn() -> f(an_atom, 1).",
        &[("f(an_atom, 1)", &[("_@A", &["an_atom"]), ("_@B", &["1"])])],
    );
    assert_matches(
        pat,
        "fn() -> f(1, an_atom).",
        &[("f(1, an_atom)", &[("_@A", &["1"]), ("_@B", &["an_atom"])])],
    );
    assert_matches(
        pat,
        "fn() -> f(foo, bar).",
        &[("f(foo, bar)", &[("_@A", &["foo"]), ("_@B", &["bar"])])],
    );
    assert_matches(pat, "fn() -> f(1, 2).", &[]);
}

/// Precedence: `,` binds tighter than `;`, so the parse is
/// `(is_atom(A) ∧ is_integer(B)) ∨ is_tuple(A)`.
#[test]
fn ssr_when_or_precedence_and_binds_tighter() {
    let pat = "ssr: f(_@A, _@B) where is_atom(_@A), is_integer(_@B); is_tuple(_@A).";
    // First conjunction wins.
    assert_matches(
        pat,
        "fn() -> f(an_atom, 42).",
        &[("f(an_atom, 42)", &[("_@A", &["an_atom"]), ("_@B", &["42"])])],
    );
    // Second conjunction wins (B can be anything).
    assert_matches(
        pat,
        "fn() -> f({x, y}, ignored).",
        &[(
            "f({x, y}, ignored)",
            &[("_@A", &["{x, y}"]), ("_@B", &["ignored"])],
        )],
    );
    // Neither: A is atom (first half of conj 1 ok), but B is atom not
    // integer (conj 1 fails). A is not a tuple (conj 2 fails). No match.
    assert_matches(pat, "fn() -> f(foo, foo).", &[]);
}

/// `;` works for plain literal conditions too (no kind constraints).
#[test]
fn ssr_when_or_literal_alternatives() {
    let pat = "ssr: _@X where _@X == foo; _@X == bar.";
    assert_matches(pat, "fn() -> foo.", &[("foo", &[("_@X", &["foo"])])]);
    assert_matches(pat, "fn() -> bar.", &[("bar", &[("_@X", &["bar"])])]);
    assert_matches(pat, "fn() -> baz.", &[]);
}

/// Mixed literal + kind across alternatives.
#[test]
fn ssr_when_or_mixed_literal_and_kind() {
    let pat = "ssr: f(_@A) where _@A == foo; is_integer(_@A).";
    assert_matches(pat, "fn() -> f(foo).", &[("f(foo)", &[("_@A", &["foo"])])]);
    assert_matches(pat, "fn() -> f(42).", &[("f(42)", &[("_@A", &["42"])])]);
    // bar is an atom but not foo, and not an integer.
    assert_matches(pat, "fn() -> f(bar).", &[]);
}
