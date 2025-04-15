/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeSet;
use std::sync::LazyLock;

use elp_types_db::StringId;
use elp_types_db::eqwalizer::AST;
use elp_types_db::eqwalizer::Id;
use elp_types_db::eqwalizer::Pos;
use elp_types_db::eqwalizer::RemoteId;
use elp_types_db::eqwalizer::expr::Body;
use elp_types_db::eqwalizer::expr::Clause;
use elp_types_db::eqwalizer::expr::Expr;
use elp_types_db::eqwalizer::expr::Lambda;
use elp_types_db::eqwalizer::expr::RemoteCall;
use elp_types_db::eqwalizer::guard::Guard;
use elp_types_db::eqwalizer::guard::Test;
use elp_types_db::eqwalizer::guard::TestAtom;
use elp_types_db::eqwalizer::guard::TestBinOp;
use elp_types_db::eqwalizer::guard::TestCall;
use elp_types_db::eqwalizer::guard::TestNumber;
use elp_types_db::eqwalizer::guard::TestTuple;
use elp_types_db::eqwalizer::guard::TestUnOp;
use elp_types_db::eqwalizer::guard::TestVar;
use elp_types_db::eqwalizer::pat::Pat;
use elp_types_db::eqwalizer::pat::PatVar;
use elp_types_db::eqwalizer::transformer;
use elp_types_db::eqwalizer::transformer::Transformer;

use crate::ast;

const PREDICATES: LazyLock<BTreeSet<ast::Id>> = LazyLock::new(|| {
    BTreeSet::from_iter(
        [
            "is_atom/1",
            "is_binary/1",
            "is_bitstring/1",
            "is_boolean/1",
            "is_float/1",
            "is_function/1",
            "is_function/2",
            "is_integer/1",
            "is_list/1",
            "is_map/1",
            "is_number/1",
            "is_pid/1",
            "is_port/1",
            "is_reference/1",
            "is_tuple/1",
            "is_record/2",
            "is_record/3",
        ]
        .map(|s| s.parse().unwrap()),
    )
});

const BINOP: LazyLock<BTreeSet<StringId>> = LazyLock::new(|| {
    BTreeSet::from_iter(
        [
            "/", "*", "-", "+", "div", "rem", "band", "bor", "bxor", "bsl", "bsr", "or", "xor",
            "and", ">=", ">", "=<", "<", "/=", "=/=", "==", "=:=", "andalso", "orelse",
        ]
        .map(|s| s.into()),
    )
});

const UNOP: LazyLock<BTreeSet<StringId>> =
    LazyLock::new(|| BTreeSet::from_iter(["bnot", "+", "-", "not"].map(|s| s.into())));

fn as_test(expr: Expr) -> Option<Test> {
    match expr {
        Expr::Var(var) => Some(Test::TestVar(TestVar {
            v: var.n,
            pos: var.pos,
        })),
        Expr::AtomLit(atom) => Some(Test::TestAtom(TestAtom {
            s: atom.s,
            pos: atom.pos,
        })),
        Expr::IntLit(lit) => Some(Test::TestNumber(TestNumber {
            pos: lit.pos,
            lit: lit.value,
        })),
        Expr::RemoteCall(rcall) if PREDICATES.contains(&rcall.id.clone().into()) => {
            Some(Test::TestCall(TestCall {
                pos: rcall.pos,
                id: rcall.id.into(),
                args: as_tests(rcall.args)?,
            }))
        }
        Expr::Tuple(tuple) => Some(Test::TestTuple(TestTuple {
            pos: tuple.pos,
            elems: as_tests(tuple.elems)?,
        })),
        Expr::UnOp(unop) if UNOP.contains(&unop.op) => Some(Test::TestUnOp(TestUnOp {
            pos: unop.pos,
            op: unop.op,
            arg: Box::new(as_test(*unop.arg)?),
        })),
        Expr::BinOp(binop) if BINOP.contains(&binop.op) => Some(Test::TestBinOp(TestBinOp {
            pos: binop.pos,
            op: binop.op,
            arg_1: Box::new(as_test(*binop.arg_1)?),
            arg_2: Box::new(as_test(*binop.arg_2)?),
        })),
        _ => None,
    }
}

fn as_tests(exprs: Vec<Expr>) -> Option<Vec<Test>> {
    let mut exprs_test = vec![];
    for expr in exprs {
        if let Some(test) = as_test(expr) {
            exprs_test.push(test);
        } else {
            return None;
        }
    }
    Some(exprs_test)
}

struct Preprocessor {
    var: u32,
}

impl Preprocessor {
    fn fresh_var(&mut self) -> StringId {
        let var = self.var;
        self.var += 1;
        format!("$pp{}", var).into()
    }

    fn eta_expand_unary_predicate(&mut self, location: &Pos, name: StringId) -> Lambda {
        let var_name = self.fresh_var();
        let test_call = Test::TestCall(TestCall {
            pos: location.clone(),
            id: Id { name, arity: 1 },
            args: vec![Test::test_var(location.clone(), var_name)],
        });
        let clause_pos = Clause {
            pos: location.clone(),
            pats: vec![Pat::pat_var(location.clone(), var_name)],
            guards: vec![Guard {
                tests: vec![test_call],
            }],
            body: Body {
                exprs: vec![Expr::atom_true(location.clone())],
            },
        };
        let clause_neg = Clause {
            pos: location.clone(),
            pats: vec![Pat::pat_var(location.clone(), var_name)],
            guards: vec![],
            body: Body {
                exprs: vec![Expr::atom_false(location.clone())],
            },
        };
        Lambda {
            pos: location.clone(),
            name: None,
            clauses: vec![clause_pos, clause_neg],
        }
    }

    fn preprocess_lists_partition_arg_fun(&mut self, location: &Pos, expr: Expr) -> Expr {
        match expr {
            Expr::RemoteFun(rfun)
                if PREDICATES.contains(&rfun.id.clone().into()) && rfun.id.arity == 1 =>
            {
                Expr::Lambda(self.eta_expand_unary_predicate(location, rfun.id.name))
            }
            Expr::Lambda(lambda) if lambda.clauses.len() == 1 => {
                let clause = &lambda.clauses[0];
                if let [body] = &clause.body.exprs[..] {
                    if let [pat] = &clause.pats[..] {
                        if let Some(test) = as_test(body.clone()) {
                            return Expr::Lambda(Lambda {
                                pos: lambda.pos.clone(),
                                name: lambda.name,
                                clauses: vec![
                                    Clause {
                                        pos: clause.pos.clone(),
                                        pats: vec![pat.clone()],
                                        guards: vec![Guard { tests: vec![test] }],
                                        body: Body {
                                            exprs: vec![Expr::atom_true(clause.pos.clone())],
                                        },
                                    },
                                    Clause {
                                        pos: clause.pos.clone(),
                                        pats: vec![Pat::PatVar(PatVar {
                                            pos: clause.pos.clone(),
                                            n: self.fresh_var(),
                                        })],
                                        guards: vec![],
                                        body: Body {
                                            exprs: vec![Expr::atom_false(clause.pos.clone())],
                                        },
                                    },
                                ],
                            });
                        }
                    }
                }
                Expr::Lambda(lambda)
            }
            expr => expr,
        }
    }
}

impl Transformer<()> for Preprocessor {
    fn transform_expr(&mut self, expr: Expr) -> Result<Expr, ()> {
        match expr {
            Expr::RemoteCall(RemoteCall {
                pos: location,
                id:
                    RemoteId {
                        module,
                        name,
                        arity: 2,
                    },
                args,
            }) if module == "lists" && name == "partition" => {
                let [arg_fun, arg_list] = args.try_into().unwrap();
                let arg_trans = self.preprocess_lists_partition_arg_fun(&location, arg_fun);
                Ok(Expr::RemoteCall(RemoteCall {
                    pos: location,
                    id: RemoteId {
                        module: "lists".into(),
                        name: "partition".into(),
                        arity: 2,
                    },
                    args: vec![arg_trans, arg_list],
                }))
            }
            e => transformer::walk_expr(self, e),
        }
    }
}

pub(crate) fn preprocess(ast: AST) -> AST {
    let mut preprocessor = Preprocessor { var: 0 };
    preprocessor.transform_ast(ast).unwrap()
}
