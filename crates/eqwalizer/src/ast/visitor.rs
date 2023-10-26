/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use super::expr::BinaryElem;
use super::expr::Body;
use super::expr::Clause;
use super::expr::Expr;
use super::expr::Qualifier;
use super::expr::RecordField;
use super::guard::Guard;
use super::guard::Test;
use super::guard::TestRecordField;
use super::pat::Pat;
use super::pat::PatBinaryElem;

pub struct Visitor<'a, T> {
    expr_visitor: &'a mut dyn FnMut(&Expr) -> Result<(), T>,
    pat_visitor: &'a mut dyn FnMut(&Pat) -> Result<(), T>,
    test_visitor: &'a mut dyn FnMut(&Test) -> Result<(), T>,
}
impl<'a, T> Visitor<'a, T> {
    pub fn visit_body(&mut self, body: &Body) -> Result<(), T> {
        body.exprs.iter().try_for_each(|e| (self.expr_visitor)(e))
    }

    pub fn visit_clause(&mut self, clause: &Clause) -> Result<(), T> {
        clause.pats.iter().try_for_each(|p| (self.pat_visitor)(p))?;
        clause.guards.iter().try_for_each(|g| self.visit_guard(g))?;
        self.visit_body(&clause.body)
    }

    pub fn visit_qualifier(&mut self, qualifier: &Qualifier) -> Result<(), T> {
        match qualifier {
            Qualifier::LGenerate(g) => {
                (self.pat_visitor)(&g.pat)?;
                (self.expr_visitor)(&g.expr)
            }
            Qualifier::BGenerate(g) => {
                (self.pat_visitor)(&g.pat)?;
                (self.expr_visitor)(&g.expr)
            }
            Qualifier::MGenerate(g) => {
                (self.pat_visitor)(&g.k_pat)?;
                (self.pat_visitor)(&g.v_pat)?;
                (self.expr_visitor)(&g.expr)
            }
            Qualifier::Filter(f) => (self.expr_visitor)(&f.expr),
        }
    }

    pub fn visit_binary_elem(&mut self, elem: &BinaryElem) -> Result<(), T> {
        (self.expr_visitor)(&elem.expr)?;
        elem.size
            .as_ref()
            .map_or(Ok(()), |s| (self.expr_visitor)(s))
    }

    pub fn visit_record_field(&mut self, field: &RecordField) -> Result<(), T> {
        match field {
            RecordField::RecordFieldGen(f) => (self.expr_visitor)(&f.value),
            RecordField::RecordFieldNamed(f) => (self.expr_visitor)(&f.value),
        }
    }

    pub fn visit_expr_children(&mut self, e: &Expr) -> Result<(), T> {
        match e {
            Expr::Var(_) => Ok(()),
            Expr::AtomLit(_) => Ok(()),
            Expr::IntLit(_) => Ok(()),
            Expr::FloatLit(_) => Ok(()),
            Expr::Block(b) => self.visit_body(&b.body),
            Expr::Match(m) => {
                (self.pat_visitor)(&m.pat)?;
                (self.expr_visitor)(&m.expr)
            }
            Expr::Tuple(t) => t.elems.iter().try_for_each(|e| (self.expr_visitor)(e)),
            Expr::StringLit(_) => Ok(()),
            Expr::NilLit(_) => Ok(()),
            Expr::Cons(c) => {
                (self.expr_visitor)(&c.h)?;
                (self.expr_visitor)(&c.t)
            }
            Expr::Case(c) => {
                (self.expr_visitor)(&c.expr)?;
                c.clauses.iter().try_for_each(|c| self.visit_clause(c))
            }
            Expr::If(i) => i.clauses.iter().try_for_each(|c| self.visit_clause(c)),
            Expr::LocalCall(c) => c.args.iter().try_for_each(|e| (self.expr_visitor)(e)),
            Expr::DynCall(c) => {
                (self.expr_visitor)(&c.f)?;
                c.args.iter().try_for_each(|e| (self.expr_visitor)(e))
            }
            Expr::RemoteCall(c) => c.args.iter().try_for_each(|e| (self.expr_visitor)(e)),
            Expr::LocalFun(_) => Ok(()),
            Expr::RemoteFun(_) => Ok(()),
            Expr::DynRemoteFun(f) => {
                (self.expr_visitor)(&f.module)?;
                (self.expr_visitor)(&f.name)
            }
            Expr::DynRemoteFunArity(f) => {
                (self.expr_visitor)(&f.module)?;
                (self.expr_visitor)(&f.name)?;
                (self.expr_visitor)(&f.arity)
            }
            Expr::Lambda(l) => l.clauses.iter().try_for_each(|c| self.visit_clause(c)),
            Expr::UnOp(o) => (self.expr_visitor)(&o.arg),
            Expr::BinOp(o) => {
                (self.expr_visitor)(&o.arg_1)?;
                (self.expr_visitor)(&o.arg_2)
            }
            Expr::LComprehension(c) => {
                (self.expr_visitor)(&c.template)?;
                c.qualifiers
                    .iter()
                    .try_for_each(|q| self.visit_qualifier(q))
            }
            Expr::BComprehension(c) => {
                (self.expr_visitor)(&c.template)?;
                c.qualifiers
                    .iter()
                    .try_for_each(|q| self.visit_qualifier(q))
            }
            Expr::MComprehension(c) => {
                (self.expr_visitor)(&c.k_template)?;
                (self.expr_visitor)(&c.v_template)?;
                c.qualifiers
                    .iter()
                    .try_for_each(|q| self.visit_qualifier(q))
            }
            Expr::Binary(b) => b.elems.iter().try_for_each(|e| self.visit_binary_elem(e)),
            Expr::Catch(c) => (self.expr_visitor)(&c.expr),
            Expr::TryCatchExpr(e) => {
                self.visit_body(&e.try_body)?;
                e.catch_clauses
                    .iter()
                    .try_for_each(|c| self.visit_clause(c))?;
                e.after_body.as_ref().map_or(Ok(()), |b| self.visit_body(b))
            }
            Expr::TryOfCatchExpr(e) => {
                self.visit_body(&e.try_body)?;
                e.try_clauses
                    .iter()
                    .try_for_each(|c| self.visit_clause(c))?;
                e.catch_clauses
                    .iter()
                    .try_for_each(|c| self.visit_clause(c))?;
                e.after_body.as_ref().map_or(Ok(()), |b| self.visit_body(b))
            }
            Expr::Receive(r) => r.clauses.iter().try_for_each(|c| self.visit_clause(c)),
            Expr::ReceiveWithTimeout(r) => {
                r.clauses.iter().try_for_each(|c| self.visit_clause(c))?;
                (self.expr_visitor)(&r.timeout)?;
                self.visit_body(&r.timeout_body)
            }
            Expr::RecordCreate(r) => r.fields.iter().try_for_each(|f| self.visit_record_field(f)),
            Expr::RecordUpdate(r) => {
                (self.expr_visitor)(&r.expr)?;
                r.fields
                    .iter()
                    .try_for_each(|f| (self.expr_visitor)(&f.value))
            }
            Expr::RecordSelect(r) => (self.expr_visitor)(&r.expr),
            Expr::RecordIndex(_) => Ok(()),
            Expr::MapCreate(m) => m.kvs.iter().try_for_each(|(k, v)| {
                (self.expr_visitor)(k).and_then(|()| (self.expr_visitor)(v))
            }),
            Expr::MapUpdate(m) => {
                (self.expr_visitor)(&m.map)?;
                m.kvs.iter().try_for_each(|(k, v)| {
                    (self.expr_visitor)(k).and_then(|()| (self.expr_visitor)(v))
                })
            }
            Expr::Maybe(m) => self.visit_body(&m.body),
            Expr::MaybeElse(m) => {
                self.visit_body(&m.body)?;
                m.else_clauses.iter().try_for_each(|c| self.visit_clause(c))
            }
            Expr::MaybeMatch(m) => {
                (self.pat_visitor)(&m.pat)?;
                (self.expr_visitor)(&m.arg)
            }
        }
    }

    pub fn visit_pat_binary_elem(&mut self, elem: &PatBinaryElem) -> Result<(), T> {
        (self.pat_visitor)(&elem.pat)?;
        elem.size
            .as_ref()
            .map_or(Ok(()), |s| (self.expr_visitor)(s))
    }

    pub fn visit_pat_children(&mut self, p: &Pat) -> Result<(), T> {
        match p {
            Pat::PatWild(_) => Ok(()),
            Pat::PatMatch(m) => {
                (self.pat_visitor)(&m.pat)?;
                (self.pat_visitor)(&m.arg)
            }
            Pat::PatTuple(t) => t.elems.iter().try_for_each(|p| (self.pat_visitor)(p)),
            Pat::PatString(_) => Ok(()),
            Pat::PatNil(_) => Ok(()),
            Pat::PatCons(c) => {
                (self.pat_visitor)(&c.h)?;
                (self.pat_visitor)(&c.t)
            }
            Pat::PatInt(_) => Ok(()),
            Pat::PatNumber(_) => Ok(()),
            Pat::PatAtom(_) => Ok(()),
            Pat::PatVar(_) => Ok(()),
            Pat::PatRecord(r) => {
                r.fields
                    .iter()
                    .try_for_each(|f| (self.pat_visitor)(&f.pat))?;
                r.gen.as_ref().map_or(Ok(()), |g| (self.pat_visitor)(g))
            }
            Pat::PatRecordIndex(_) => Ok(()),
            Pat::PatUnOp(o) => (self.pat_visitor)(&o.arg),
            Pat::PatBinOp(o) => {
                (self.pat_visitor)(&o.arg_1)?;
                (self.pat_visitor)(&o.arg_2)
            }
            Pat::PatBinary(b) => b
                .elems
                .iter()
                .try_for_each(|e| self.visit_pat_binary_elem(e)),
            Pat::PatMap(m) => m
                .kvs
                .iter()
                .try_for_each(|(k, v)| (self.test_visitor)(k).and_then(|()| (self.pat_visitor)(v))),
        }
    }

    pub fn visit_guard(&mut self, g: &Guard) -> Result<(), T> {
        g.tests.iter().try_for_each(|t| (self.test_visitor)(t))
    }

    pub fn visit_test_record_field(&mut self, f: &TestRecordField) -> Result<(), T> {
        match f {
            TestRecordField::TestRecordFieldNamed(f) => (self.test_visitor)(&f.value),
            TestRecordField::TestRecordFieldGen(f) => (self.test_visitor)(&f.value),
        }
    }

    pub fn visit_test_children(&mut self, t: &Test) -> Result<(), T> {
        match t {
            Test::TestVar(_) => Ok(()),
            Test::TestAtom(_) => Ok(()),
            Test::TestNumber(_) => Ok(()),
            Test::TestTuple(t) => t.elems.iter().try_for_each(|t| (self.test_visitor)(t)),
            Test::TestString(_) => Ok(()),
            Test::TestNil(_) => Ok(()),
            Test::TestCons(c) => {
                (self.test_visitor)(&c.h)?;
                (self.test_visitor)(&c.t)
            }
            Test::TestCall(c) => c.args.iter().try_for_each(|a| (self.test_visitor)(a)),
            Test::TestRecordCreate(r) => r
                .fields
                .iter()
                .try_for_each(|f| self.visit_test_record_field(f)),
            Test::TestRecordSelect(r) => (self.test_visitor)(&r.rec),
            Test::TestRecordIndex(_) => Ok(()),
            Test::TestMapCreate(m) => m.kvs.iter().try_for_each(|(k, v)| {
                (self.test_visitor)(k).and_then(|()| (self.test_visitor)(v))
            }),
            Test::TestMapUpdate(m) => {
                (self.test_visitor)(&m.map)?;
                m.kvs.iter().try_for_each(|(k, v)| {
                    (self.test_visitor)(k).and_then(|()| (self.test_visitor)(v))
                })
            }
            Test::TestUnOp(o) => (self.test_visitor)(&o.arg),
            Test::TestBinOp(o) => {
                (self.test_visitor)(&o.arg_1)?;
                (self.test_visitor)(&o.arg_2)
            }
            Test::TestBinaryLit(_) => Ok(()),
        }
    }
}
