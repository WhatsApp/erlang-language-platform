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
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::Semantic;
use hir::Strategy;

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
                        Expr<0>:SsrPlaceholder {var: _@V, }
                    pat
                        Pat<0>:SsrPlaceholder {var: _@V, }
                rhs
                    Expr<3>:Expr::BinaryOp {
                        lhs
                            Expr<1>:SsrPlaceholder {var: _@V, }
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
              when V == foo
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
        "Match debug info: {} nodes had text exactly equal to \"{}\"",
        debug_info.len(),
        snippet
    );
    for (index, d) in debug_info.iter().enumerate() {
        println!("Node #{index}\n{d:#?}\n");
    }
}

#[track_caller]
fn assert_matches(pattern: &str, code: &str, expected: &[&str]) {
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
fn assert_matches_with_strategy(strategy: Strategy, pattern: &str, code: &str, expected: &[&str]) {
    let (db, position, _selections) = single_file(code);
    if expected.len() > 0 {
        if expected[0] == "" {
            panic!("empty expected string");
        }
    }
    let sema = Semantic::new(&db);
    let pattern = SsrRule::parse_str(sema.db, pattern).unwrap();
    let mut match_finder = MatchFinder::in_context(&sema, strategy, position.file_id);
    match_finder.debug_print = false;
    match_finder.add_search_pattern(pattern);
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

#[track_caller]
fn assert_match_placeholder(
    pattern: &str,
    code: &str,
    expected: &[&str],
    placeholder_name: &str,
    expected_val: Expect,
) {
    let (db, position, _selections) = single_file(code);
    if expected.len() > 0 {
        if expected[0] == "" {
            panic!("empty expected string");
        }
    }
    let sema = Semantic::new(&db);
    let pattern = SsrRule::parse_str(sema.db, pattern).unwrap();
    let mut match_finder = MatchFinder::in_context(
        &sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        position.file_id,
    );
    match_finder.debug_print = false;
    match_finder.add_search_pattern(pattern);
    let matches = match_finder.matches();
    let match0 = &matches.matches[0];
    let placeholder_val = match0.get_placeholder_matches(&sema, placeholder_name);
    let matched_strings: Vec<String> = matches
        .flattened()
        .matches
        .iter()
        .map(|m| m.matched_text(&db))
        .collect();
    if matched_strings != expected && !expected.is_empty() {
        print_match_debug_info(&match_finder, position.file_id, expected[0]);
    }
    assert_eq!(matched_strings, expected);
    expected_val.assert_debug_eq(&placeholder_val);
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

#[test]
fn ssr_expr_match_list() {
    assert_matches(
        "ssr: [ _@A, _@B | _@C].",
        "fn(Y) -> X = [1, 2 | [Y]].",
        &["[1, 2 | [Y]]"],
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
        &["<<X,Y>>"],
    );
}

#[test]
fn ssr_expr_match_unary_op() {
    assert_matches("ssr: not _@A.", "fn(Y) -> not Y.", &["not Y"]);
    assert_matches("ssr: bnot _@A.", "fn(Y) -> not Y.", &[]);
    assert_matches("ssr: bnot _@A.", "fn(Y) -> bnot Y.", &["bnot Y"]);
    // Note: it is an AST, not textual match
    assert_matches("ssr: + _@A.", "fn(Y) -> +Y.", &["+Y"]);
    assert_matches("ssr: -_@A.", "fn(Y) -> -Y.", &["-Y"]);
}

#[test]
fn ssr_expr_binary_op() {
    assert_matches("ssr: _@A + _@B.", "fn(X) -> Y = {X + 1}, Y.", &["X + 1"]);
    assert_matches("ssr: _@A - _@B.", "fn(X) -> Y = {X + 1}, Y.", &[]);
    assert_matches("ssr: _@A and _@B.", "fn(X,Y) -> X and Y .", &["X and Y"]);
    assert_matches(
        "ssr: _@A andalso _@B.",
        "fn(X,Y) -> X andalso Y .",
        &["X andalso Y"],
    );
    assert_matches("ssr: _@A or _@B.", "fn(X,Y) -> X or Y .", &["X or Y"]);
    assert_matches(
        "ssr: _@A orelse _@B.",
        "fn(X,Y) -> X orelse Y .",
        &["X orelse Y"],
    );
    assert_matches("ssr: _@A ! _@B.", "fn(X,Y) -> X ! Y .", &["X ! Y"]);
    // Comparison operators
    assert_matches("ssr: _@A == _@B.", "fn(X,Y) -> X == Y .", &["X == Y"]);
    assert_matches("ssr: _@A /= _@B.", "fn(X,Y) -> X /= Y .", &["X /= Y"]);
    assert_matches("ssr: _@A =< _@B.", "fn(X,Y) -> X =< Y .", &["X =< Y"]);
    assert_matches("ssr: _@A  < _@B.", "fn(X,Y) -> X  < Y .", &["X  < Y"]);
    assert_matches("ssr: _@A >= _@B.", "fn(X,Y) -> X >= Y .", &["X >= Y"]);
    assert_matches("ssr: _@A >  _@B.", "fn(X,Y) -> X >  Y .", &["X >  Y"]);
    assert_matches("ssr: _@A =:= _@B.", "fn(X,Y) -> X =:= Y .", &["X =:= Y"]);
    assert_matches("ssr: _@A =/= _@B.", "fn(X,Y) -> X =/= Y .", &["X =/= Y"]);
    // List operators
    assert_matches("ssr: _@A ++ _@B.", "fn(X,Y) -> X ++ Y .", &["X ++ Y"]);
    assert_matches("ssr: _@A -- _@B.", "fn(X,Y) -> X -- Y .", &["X -- Y"]);
    // Add operators
    assert_matches("ssr: _@A + _@B.", "fn(X,Y) -> X + Y .", &["X + Y"]);
    assert_matches("ssr: _@A - _@B.", "fn(X,Y) -> X - Y .", &["X - Y"]);
    assert_matches("ssr: _@A bor _@B.", "fn(X,Y) -> X bor Y .", &["X bor Y"]);
    assert_matches("ssr: _@A bxor _@B.", "fn(X,Y) -> X bxor Y .", &["X bxor Y"]);
    assert_matches("ssr: _@A bsl _@B.", "fn(X,Y) -> X bsl Y .", &["X bsl Y"]);
    assert_matches("ssr: _@A bsr _@B.", "fn(X,Y) -> X bsr Y .", &["X bsr Y"]);
    assert_matches("ssr: _@A or _@B.", "fn(X,Y) -> X or Y .", &["X or Y"]);
    // Mult operators
    assert_matches("ssr: _@A / _@B.", "fn(X,Y) -> X / Y .", &["X / Y"]);
    assert_matches("ssr: _@A * _@B.", "fn(X,Y) -> X * Y .", &["X * Y"]);
    assert_matches("ssr: _@A div _@B.", "fn(X,Y) -> X div Y .", &["X div Y"]);
    assert_matches("ssr: _@A rem _@B.", "fn(X,Y) -> X rem Y .", &["X rem Y"]);
    assert_matches("ssr: _@A band _@B.", "fn(X,Y) -> X band Y .", &["X band Y"]);
    assert_matches("ssr: _@A and _@B.", "fn(X,Y) -> X and Y .", &["X and Y"]);
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
        &["List#a_record{field = XX}"],
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
        &["#a_record.a_field"],
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
        &["List#record.field"],
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
        &["#{field => XX}"],
    );
    assert_matches(
        "ssr: #{ field => _@A, another => _@B }.",
        "bar() -> XX = 1, #{another => 3, field => XX}.",
        &["#{another => 3, field => XX}"],
    );
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
        &["List#{foo => XX}"],
    );
    assert_matches(
        "ssr: _@A#{ foo => _@B, zz => _@C }.",
        "bar(List) -> XX = 1, List#{zz => 1, foo => XX}.",
        &["List#{zz => 1, foo => XX}"],
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
        &["catch XX"],
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
        &["?BAR(4)"],
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
        &["?BAR(4)"],
    );
    assert_matches_with_strategy(
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        "ssr: {_@AA}.",
        "-define(BAR(X), {X}).
         bar() -> ?BAR(4).",
        &["{X}", "?BAR(4)"],
    );
}

#[test]
fn ssr_expr_list_comprehension() {
    assert_matches(
        "ssr: [XX || XX <- _@List, _@Cond].",
        "bar() -> XX = 1, [XX || XX <- List, XX >= 5].",
        &["[XX || XX <- List, XX >= 5]"],
    );
    assert_matches(
        "ssr: [XX || XX <:- _@List, _@Cond].",
        "bar() -> XX = 1, [XX || XX <:- List, XX >= 5].",
        &["[XX || XX <:- List, XX >= 5]"],
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
        &["<<XX || XX <= List>>"],
    );
    assert_matches(
        "ssr: <<XX || XX <:= _@List>>.",
        "bar(List) -> XX = 1, <<XX || XX <:= List>>.",
        &["<<XX || XX <:= List>>"],
    );
}

#[test]
fn ssr_expr_map_comprehension() {
    assert_matches(
        "ssr: #{_@K => _@V || _@K := _@V <- _@Map}.",
        "bar(Map) -> #{ K => V || K := V <- Map}.",
        &["#{ K => V || K := V <- Map}"],
    );
    assert_matches(
        "ssr: #{_@K => _@V || _@K := _@V <:- _@Map}.",
        "bar(Map) -> #{ K => V || K := V <:- Map}.",
        &["#{ K => V || K := V <:- Map}"],
    );
}

#[test]
fn ssr_expr_if() {
    assert_matches(
        "ssr: if _@Cond -> _@B end .",
        "bar(F) -> if is_atom(F) -> 22 end.",
        &["if is_atom(F) -> 22 end"],
    );
}

#[test]
fn ssr_expr_case() {
    assert_matches(
        "ssr: case _@XX of _@A -> _@B end .",
        "bar(F) -> XX = 1, case F of undefined -> XX end.",
        &["case F of undefined -> XX end"],
    );
}

#[test]
fn ssr_expr_receive() {
    assert_matches(
        "ssr: receive _@XX -> 3 end.",
        "bar(F) -> XX = 1, receive F -> 3 end.",
        &["receive F -> 3 end"],
    );
    assert_matches(
        "ssr: receive _@XX -> 3 after _@MS -> ok end.",
        "bar(F) -> XX = 1, receive F -> 3 end.",
        &[],
    );
    assert_matches(
        "ssr: receive _@XX -> 3 after _@MS -> ok end.",
        "bar(F) -> XX = 1, receive F -> 3 after 1000 -> ok end.",
        &["receive F -> 3 after 1000 -> ok end"],
    );
    assert_matches(
        "ssr: receive _@XX -> true; _@AA -> _@BB after _@MS -> timeout end.",
        "bar() -> receive all_good -> true; X -> false after 1000 -> timeout end.",
        &["receive all_good -> true; X -> false after 1000 -> timeout end"],
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
        &[r#"try 1 of
                 2 -> ok
             catch
                 YY when true -> ok;
                 error:undef:Stack -> Stack
             after
                 ok
             end"#],
    );
}

#[test]
fn ssr_expr_capture_fun() {
    assert_matches(
        "ssr: fun _@MODU:_@FUN/_@XX.",
        "bar(XX) -> YY = fun modu:fun/XX, YY.",
        &["fun modu:fun/XX"],
    );
}

#[test]
fn ssr_expr_closure() {
    assert_matches(
        "ssr: fun (_@XX) -> _@YY end.",
        "bar(XX) -> F = fun (3) -> 7 end, F.",
        &["fun (3) -> 7 end"],
    );
    assert_matches(
        "ssr: fun _@Name(_@XX) -> _@YY end.",
        "bar(XX) -> F = fun Bar(3) -> 7 end, F.",
        &["fun Bar(3) -> 7 end"],
    );
}

#[test]
fn ssr_expr_maybe() {
    assert_matches(
        "ssr: maybe _@AA ?= _@BB end.",
        "bar() -> maybe {ok, A} ?= ok_a() end.",
        &["maybe {ok, A} ?= ok_a() end"],
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
        &["maybe {ok, A} ?= ok_a() end"],
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
        &["((3))"],
    );
    assert_matches_with_strategy(
        visible_parens,
        "ssr: ((_@AA)).",
        "bar(((X))) -> X = 3,X = (4).",
        &["((X))"],
    );
    assert_matches_with_strategy(
        invisible_parens,
        "ssr: ((_@AA)).",
        "bar(X) -> X = ((3)),X = (4).",
        &["X", "X = ((3))", "X", "((3))", "X", "X = (4)", "(4)"],
    );
    assert_matches_with_strategy(
        invisible_parens,
        "ssr: ((_@AA)).",
        "bar(((X))) -> X = 3,X = (4).",
        &["((X))", "X = 3", "X", "3", "X = (4)", "X", "(4)"],
    );
}

// ---------------------------------------------------------------------

#[test]
fn ssr_pat_match() {
    assert_matches(
        "ssr: _@AA = _@BB.",
        "bar({ok, A} = B) -> A.",
        &["{ok, A} = B"],
    );
}

#[test]
fn ssr_pat_list() {
    assert_matches(
        "ssr: [ _@A, _@B | _@C].",
        "fn(X) -> [1, 2 | [Y]] = X.",
        &["[1, 2 | [Y]]"],
    );
}

#[test]
fn ssr_pat_binary() {
    assert_matches("ssr: << _@A, _@B>>.", "fn(Y) -> <<X,Z>> = Y.", &["<<X,Z>>"]);
}

#[test]
fn ssr_pat_unary_op() {
    assert_matches("ssr: not _@A.", "fn(Y) -> {not {X}} = Y.", &["not {X}"]);
    assert_matches("ssr: bnot _@A.", "fn(Y) -> {not {X}} = Y.", &[]);
    assert_matches("ssr: + _@A.", "fn(Y) -> {+X} = Y.", &["+X"]);
    assert_matches("ssr: - _@A.", "fn(Y) -> {-X} = Y.", &["-X"]);
}

#[test]
fn ssr_pat_binary_op() {
    assert_matches("ssr: _@A + _@B.", "fn(Y) -> {X + 1} = Y.", &["X + 1"]);
    assert_matches("ssr: _@A - _@B.", "fn(Y) -> {X + 1} = Y.", &[]);

    assert_matches("ssr: _@A + _@B.", "fn(X) -> Y = {X + 1} = Y.", &["X + 1"]);
    assert_matches("ssr: _@A - _@B.", "fn(X) -> Y = {X + 1} =  Y.", &[]);
    assert_matches(
        "ssr: _@A and _@B.",
        "fn(X,Y) -> X and Y = true.",
        &["X and Y"],
    );
    assert_matches(
        "ssr: _@A andalso _@B.",
        "fn(X,Y) -> X andalso Y = true.",
        &["X andalso Y"],
    );
    assert_matches("ssr: _@A or _@B.", "fn(X,Y) -> X or Y = true.", &["X or Y"]);
    assert_matches(
        "ssr: _@A orelse _@B.",
        "fn(X,Y) -> X orelse Y = true.",
        &["X orelse Y"],
    );
    assert_matches("ssr: _@A ! _@B.", "fn(X,Y,Z) -> X ! Y = Z .", &["X ! Y"]);
    // Comparison operators
    assert_matches("ssr: _@A == _@B.", "fn(X,Y) -> X == Y = true.", &["X == Y"]);
    assert_matches(
        "ssr: _@A /= _@B.",
        "fn(X,Y) -> X /= Y = true .",
        &["X /= Y"],
    );
    assert_matches("ssr: _@A =< _@B.", "fn(X,Y) -> X =< Y = true.", &["X =< Y"]);
    assert_matches("ssr: _@A  < _@B.", "fn(X,Y) -> X  < Y = true.", &["X  < Y"]);
    assert_matches("ssr: _@A >= _@B.", "fn(X,Y) -> X >= Y = true.", &["X >= Y"]);
    assert_matches("ssr: _@A >  _@B.", "fn(X,Y) -> X >  Y = true.", &["X >  Y"]);
    assert_matches(
        "ssr: _@A =:= _@B.",
        "fn(X,Y) -> X =:= Y = true.",
        &["X =:= Y"],
    );
    assert_matches(
        "ssr: _@A =/= _@B.",
        "fn(X,Y) -> X =/= Y = true.",
        &["X =/= Y"],
    );
    // List operators
    assert_matches("ssr: _@A ++ _@B.", "fn(X,Y) -> X ++ Y = [].", &["X ++ Y"]);
    assert_matches("ssr: _@A -- _@B.", "fn(X,Y) -> X -- Y = [].", &["X -- Y"]);
    // Add operators
    assert_matches("ssr: _@A + _@B.", "fn(X,Y) -> X + Y = 5.", &["X + Y"]);
    assert_matches("ssr: _@A - _@B.", "fn(X,Y) -> X - Y = 3.", &["X - Y"]);
    assert_matches(
        "ssr: _@A bor _@B.",
        "fn(X,Y,Z) -> X bor Y = Z.",
        &["X bor Y"],
    );
    assert_matches(
        "ssr: _@A bxor _@B.",
        "fn(X,Y,Z) -> X bxor Y = Z.",
        &["X bxor Y"],
    );
    assert_matches(
        "ssr: _@A bsl _@B.",
        "fn(X,Y,Z) -> X bsl Y = Z.",
        &["X bsl Y"],
    );
    assert_matches(
        "ssr: _@A bsr _@B.",
        "fn(X,Y,Z) -> X bsr Y = Z.",
        &["X bsr Y"],
    );
    assert_matches("ssr: _@A or _@B.", "fn(X,Y) -> X or Y = true.", &["X or Y"]);
    // Mult operators
    assert_matches("ssr: _@A / _@B.", "fn(X,Y,Z) -> X / Y = Z.", &["X / Y"]);
    assert_matches("ssr: _@A * _@B.", "fn(X,Y) -> X * Y = 10.", &["X * Y"]);
    assert_matches(
        "ssr: _@A div _@B.",
        "fn(X,Y,A) -> X div Y =Z .",
        &["X div Y"],
    );
    assert_matches("ssr: _@A rem _@B.", "fn(X,Y) -> X rem Y = 0.", &["X rem Y"]);
    assert_matches(
        "ssr: _@A band _@B.",
        "fn(X,Y,Z) -> X band Y =Z .",
        &["X band Y"],
    );
    assert_matches(
        "ssr: _@A and _@B.",
        "fn(X,Y) -> X and Y = true.",
        &["X and Y"],
    );

    // ---------------------------------

    assert_matches(
        "ssr: _@A + _@B.",
        "fn(Y) -> {X + 1 + 2} = Y.",
        &["X + 1", "X + 1 + 2"],
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
        &["#foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}"],
    );
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = {_@C, _@D}}.",
        "fn(X) -> #foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}} = X.",
        &["#foo{k1 = a, k2 = <<\"blah\">>, k3 = {c, d}}"],
    );
    // Order independent
    assert_matches(
        "ssr: #foo{k1 = _@A, k2 = _@B, k3 = {_@C, _@D}}.",
        "fn(X) -> #foo{k3 = {c, d}, k2 = a, k1 = <<\"blah\">>} = X.",
        &["#foo{k3 = {c, d}, k2 = a, k1 = <<\"blah\">>}"],
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
        &["#record.field"],
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
        &["#{foo := XX}"],
    );
    // Order independent
    assert_matches(
        "ssr: #{ a := _@A, b := _@B }.",
        "bar(YY) -> #{b := 1, a := XX} = YY.",
        &["#{b := 1, a := XX}"],
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
        &["?BAR(4)"],
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
        &["?BAR(4)"],
    );
    assert_matches_with_strategy(
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        "ssr: {_@AA}.",
        "-define(BAR(X), {X}).
         bar(?BAR(4)) -> ok.",
        &["{X}", "?BAR(4)"],
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
        "ssr: {_@X = _@Y} when _@X == foo.",
        "foo() -> {foo = 3},{bar = 2}.",
        &["{foo = 3}"],
    );
}

#[test]
fn ssr_match_constant_negated_atom_placeholder() {
    assert_matches(
        "ssr: {_@X = _@Y} when _@X =/= foo.",
        "foo() -> {foo = 3},{bar = 2}.",
        &["{bar = 2}"],
    );
}

#[test]
fn ssr_invalid_when_condition_not_literal() {
    expect![[r#"
        "Parse error: Invalid `when` RHS, expecting a literal"
    "#]]
    .assert_debug_eq(&parse_error_text("ssr: {_@X = _@Y} when _@X == Y."));
}

#[test]
fn ssr_invalid_when_condition_not_compop() {
    expect![[r#"
        "Parse error: Invalid `when` condition"
    "#]]
    .assert_debug_eq(&parse_error_text("ssr: {_@X = _@Y} when _@X = foo."));
}

// ---------------------------------------------------------------------

#[test]
fn ssr_retrieve_match_placeholder() {
    assert_match_placeholder(
        "ssr: {_@X = _@Y} when _@X =/= foo.",
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
        &["<<X,X>>"],
    );
    assert_matches(
        "ssr: { _@A, _@A}.",
        "fn(Y) -> {[a,b,Y],[a,b,Y]}.",
        &["{[a,b,Y],[a,b,Y]}"],
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
                 ~b"""
                   "\\µA"
                   """.
         "#,
        &[],
    );
}
