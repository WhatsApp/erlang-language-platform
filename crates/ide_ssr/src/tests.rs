/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::fixture::WithFixture;
use elp_ide_db::RootDatabase;
use expect_test::expect;
use expect_test::Expect;

use crate::SsrPattern;

#[track_caller]
fn parse_error_text(query: &str) -> String {
    let (mut db, _file_id) = RootDatabase::with_single_file(&query);
    let pattern = SsrPattern::parse_str(&mut db, query);
    format!("{}", pattern.unwrap_err())
}

#[track_caller]
fn parse_good_text(query: &str, expect: Expect) {
    let (mut db, _file_id) = RootDatabase::with_single_file(&query);
    let pattern = SsrPattern::parse_str(&mut db, query);
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
