/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use super::AST;
use super::expr::BComprehension;
use super::expr::BGenerate;
use super::expr::BGenerateStrict;
use super::expr::BinOp;
use super::expr::Binary;
use super::expr::BinaryElem;
use super::expr::Block;
use super::expr::Body;
use super::expr::Case;
use super::expr::Catch;
use super::expr::Clause;
use super::expr::Cons;
use super::expr::DynCall;
use super::expr::DynRemoteFun;
use super::expr::DynRemoteFunArity;
use super::expr::Expr;
use super::expr::Filter;
use super::expr::If;
use super::expr::LComprehension;
use super::expr::LGenerate;
use super::expr::LGenerateStrict;
use super::expr::Lambda;
use super::expr::LocalCall;
use super::expr::MComprehension;
use super::expr::MGenerate;
use super::expr::MGenerateStrict;
use super::expr::MapCreate;
use super::expr::MapUpdate;
use super::expr::Match;
use super::expr::Maybe;
use super::expr::MaybeElse;
use super::expr::MaybeMatch;
use super::expr::Qualifier;
use super::expr::Receive;
use super::expr::ReceiveWithTimeout;
use super::expr::RecordCreate;
use super::expr::RecordField;
use super::expr::RecordFieldGen;
use super::expr::RecordSelect;
use super::expr::RecordUpdate;
use super::expr::RemoteCall;
use super::expr::TryCatchExpr;
use super::expr::TryOfCatchExpr;
use super::expr::Tuple;
use super::expr::UnOp;
use super::expr::Zip;
use super::form::ExternalForm;
use super::form::ExternalRecDecl;
use super::form::ExternalRecField;
use super::form::FunDecl;
use super::guard::Guard;
use super::guard::Test;
use super::guard::TestBinOp;
use super::guard::TestCall;
use super::guard::TestCons;
use super::guard::TestMapCreate;
use super::guard::TestMapUpdate;
use super::guard::TestRecordCreate;
use super::guard::TestRecordField;
use super::guard::TestRecordFieldGen;
use super::guard::TestRecordFieldNamed;
use super::guard::TestRecordSelect;
use super::guard::TestTuple;
use super::guard::TestUnOp;
use super::pat::Pat;
use super::pat::PatBinOp;
use super::pat::PatBinary;
use super::pat::PatBinaryElem;
use super::pat::PatCons;
use super::pat::PatMap;
use super::pat::PatMatch;
use super::pat::PatRecord;
use super::pat::PatRecordFieldNamed;
use super::pat::PatTuple;
use super::pat::PatUnOp;
use crate::eqwalizer::expr::RecordFieldNamed;

pub trait Transformer<T>: Sized {
    fn transform_ast(&mut self, ast: AST) -> Result<AST, T> {
        let forms = ast
            .forms
            .into_iter()
            .map(|form| self.transform_form(form))
            .collect::<Result<_, _>>()?;
        Ok(AST { forms, ..ast })
    }
    fn transform_expr(&mut self, expr: Expr) -> Result<Expr, T> {
        walk_expr(self, expr)
    }
    fn transform_pat(&mut self, pat: Pat) -> Result<Pat, T> {
        walk_pat(self, pat)
    }
    fn transform_test(&mut self, test: Test) -> Result<Test, T> {
        walk_test(self, test)
    }
    fn transform_clause(&mut self, clause: Clause) -> Result<Clause, T> {
        walk_clause(self, clause)
    }
    fn transform_body(&mut self, body: Body) -> Result<Body, T> {
        walk_body(self, body)
    }
    fn transform_guard(&mut self, guard: Guard) -> Result<Guard, T> {
        walk_guard(self, guard)
    }
    fn transform_form(&mut self, form: ExternalForm) -> Result<ExternalForm, T> {
        walk_form(self, form)
    }
    fn transform_qualifier(&mut self, qualifier: Qualifier) -> Result<Qualifier, T> {
        walk_qualifier(self, qualifier)
    }
    fn transform_binary_elem(&mut self, elem: BinaryElem) -> Result<BinaryElem, T> {
        walk_binary_elem(self, elem)
    }
    fn transform_pat_binary_elem(&mut self, elem: PatBinaryElem) -> Result<PatBinaryElem, T> {
        walk_pat_binary_elem(self, elem)
    }
    fn transform_record_field(&mut self, field: RecordField) -> Result<RecordField, T> {
        walk_record_field(self, field)
    }
    fn transform_test_record_field(
        &mut self,
        field: TestRecordField,
    ) -> Result<TestRecordField, T> {
        walk_test_record_field(self, field)
    }
}

pub fn walk_body<T, V: Transformer<T>>(transformer: &mut V, body: Body) -> Result<Body, T> {
    body.exprs
        .into_iter()
        .map(|e| transformer.transform_expr(e))
        .collect::<Result<Vec<_>, _>>()
        .map(|forms| Body { exprs: forms })
}

pub fn walk_clause<T, V: Transformer<T>>(transformer: &mut V, clause: Clause) -> Result<Clause, T> {
    let pats = clause
        .pats
        .into_iter()
        .map(|p| transformer.transform_pat(p))
        .collect::<Result<Vec<_>, _>>()?;
    let guards = clause
        .guards
        .into_iter()
        .map(|g| transformer.transform_guard(g))
        .collect::<Result<Vec<_>, _>>()?;
    let body = transformer.transform_body(clause.body)?;
    Ok(Clause {
        pos: clause.pos,
        pats,
        guards,
        body,
    })
}

pub fn walk_qualifier<T, V: Transformer<T>>(
    transformer: &mut V,
    qualifier: Qualifier,
) -> Result<Qualifier, T> {
    match qualifier {
        Qualifier::LGenerate(g) => Ok(Qualifier::LGenerate(LGenerate {
            pat: transformer.transform_pat(g.pat)?,
            expr: transformer.transform_expr(g.expr)?,
        })),
        Qualifier::LGenerateStrict(g) => Ok(Qualifier::LGenerateStrict(LGenerateStrict {
            pat: transformer.transform_pat(g.pat)?,
            expr: transformer.transform_expr(g.expr)?,
        })),
        Qualifier::BGenerate(g) => Ok(Qualifier::BGenerate(BGenerate {
            pat: transformer.transform_pat(g.pat)?,
            expr: transformer.transform_expr(g.expr)?,
        })),
        Qualifier::BGenerateStrict(g) => Ok(Qualifier::BGenerateStrict(BGenerateStrict {
            pat: transformer.transform_pat(g.pat)?,
            expr: transformer.transform_expr(g.expr)?,
        })),
        Qualifier::MGenerate(g) => Ok(Qualifier::MGenerate(MGenerate {
            k_pat: transformer.transform_pat(g.k_pat)?,
            v_pat: transformer.transform_pat(g.v_pat)?,
            expr: transformer.transform_expr(g.expr)?,
        })),
        Qualifier::MGenerateStrict(g) => Ok(Qualifier::MGenerateStrict(MGenerateStrict {
            k_pat: transformer.transform_pat(g.k_pat)?,
            v_pat: transformer.transform_pat(g.v_pat)?,
            expr: transformer.transform_expr(g.expr)?,
        })),
        Qualifier::Zip(zip) => Ok(Qualifier::Zip(Zip {
            generators: zip
                .generators
                .into_iter()
                .map(|q| transformer.transform_qualifier(q))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Qualifier::Filter(f) => Ok(Qualifier::Filter(Filter {
            expr: transformer.transform_expr(f.expr)?,
        })),
    }
}

pub fn walk_binary_elem<T, V: Transformer<T>>(
    transformer: &mut V,
    elem: BinaryElem,
) -> Result<BinaryElem, T> {
    Ok(BinaryElem {
        pos: elem.pos,
        specifier: elem.specifier,
        expr: transformer.transform_expr(elem.expr)?,
        size: elem
            .size
            .map_or(Ok(None), |s| transformer.transform_expr(s).map(Some))?,
    })
}

pub fn walk_record_field<T, V: Transformer<T>>(
    transformer: &mut V,
    field: RecordField,
) -> Result<RecordField, T> {
    match field {
        RecordField::RecordFieldGen(f) => Ok(RecordField::RecordFieldGen(RecordFieldGen {
            value: transformer.transform_expr(f.value)?,
        })),
        RecordField::RecordFieldNamed(f) => Ok(RecordField::RecordFieldNamed(RecordFieldNamed {
            name: f.name,
            value: transformer.transform_expr(f.value)?,
        })),
    }
}

fn walk_cons<T, V: Transformer<T>>(transformer: &mut V, c: Cons) -> Result<Expr, T> {
    let mut cons = c;
    let mut transformed_elems = vec![];
    loop {
        let h = Box::new(transformer.transform_expr(*cons.h)?);
        transformed_elems.push((cons.pos, h));
        match *cons.t {
            Expr::Cons(c) => cons = c,
            exp => {
                let transformed_t = transformer.transform_expr(exp)?;
                return Ok(transformed_elems.into_iter().rev().fold(
                    transformed_t,
                    |t, (pos, h)| {
                        Expr::Cons(Cons {
                            pos,
                            h,
                            t: Box::new(t),
                        })
                    },
                ));
            }
        }
    }
}

pub fn walk_expr<T, V: Transformer<T>>(transformer: &mut V, e: Expr) -> Result<Expr, T> {
    match e {
        Expr::Var(v) => Ok(Expr::Var(v)),
        Expr::AtomLit(a) => Ok(Expr::AtomLit(a)),
        Expr::IntLit(i) => Ok(Expr::IntLit(i)),
        Expr::FloatLit(f) => Ok(Expr::FloatLit(f)),
        Expr::Block(b) => Ok(Expr::Block(Block {
            pos: b.pos,
            body: transformer.transform_body(b.body)?,
        })),
        Expr::Match(m) => Ok(Expr::Match(Match {
            pos: m.pos,
            pat: transformer.transform_pat(m.pat)?,
            expr: Box::new(transformer.transform_expr(*m.expr)?),
        })),
        Expr::Tuple(t) => Ok(Expr::Tuple(Tuple {
            pos: t.pos,
            elems: t
                .elems
                .into_iter()
                .map(|e| transformer.transform_expr(e))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::StringLit(s) => Ok(Expr::StringLit(s)),
        Expr::NilLit(n) => Ok(Expr::NilLit(n)),
        Expr::Cons(c) => walk_cons(transformer, c),
        Expr::Case(c) => Ok(Expr::Case(Case {
            pos: c.pos,
            expr: Box::new(transformer.transform_expr(*c.expr)?),
            clauses: c
                .clauses
                .into_iter()
                .map(|c| transformer.transform_clause(c))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::If(i) => Ok(Expr::If(If {
            pos: i.pos,
            clauses: i
                .clauses
                .into_iter()
                .map(|c| transformer.transform_clause(c))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::LocalCall(c) => Ok(Expr::LocalCall(LocalCall {
            pos: c.pos,
            id: c.id,
            args: c
                .args
                .into_iter()
                .map(|e| transformer.transform_expr(e))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::DynCall(c) => Ok(Expr::DynCall(DynCall {
            pos: c.pos,
            f: Box::new(transformer.transform_expr(*c.f)?),
            args: c
                .args
                .into_iter()
                .map(|e| transformer.transform_expr(e))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::RemoteCall(c) => Ok(Expr::RemoteCall(RemoteCall {
            pos: c.pos,
            id: c.id,
            args: c
                .args
                .into_iter()
                .map(|e| transformer.transform_expr(e))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::LocalFun(f) => Ok(Expr::LocalFun(f)),
        Expr::RemoteFun(f) => Ok(Expr::RemoteFun(f)),
        Expr::DynRemoteFun(f) => Ok(Expr::DynRemoteFun(DynRemoteFun {
            pos: f.pos,
            module: Box::new(transformer.transform_expr(*f.module)?),
            name: Box::new(transformer.transform_expr(*f.name)?),
        })),
        Expr::DynRemoteFunArity(f) => Ok(Expr::DynRemoteFunArity(DynRemoteFunArity {
            pos: f.pos,
            module: Box::new(transformer.transform_expr(*f.module)?),
            name: Box::new(transformer.transform_expr(*f.name)?),
            arity: Box::new(transformer.transform_expr(*f.arity)?),
        })),
        Expr::Lambda(l) => Ok(Expr::Lambda(Lambda {
            pos: l.pos,
            clauses: l
                .clauses
                .into_iter()
                .map(|c| transformer.transform_clause(c))
                .collect::<Result<Vec<_>, _>>()?,
            name: l.name,
        })),
        Expr::UnOp(o) => Ok(Expr::UnOp(UnOp {
            pos: o.pos,
            op: o.op,
            arg: Box::new(transformer.transform_expr(*o.arg)?),
        })),
        Expr::BinOp(o) => Ok(Expr::BinOp(BinOp {
            pos: o.pos,
            op: o.op,
            arg_1: Box::new(transformer.transform_expr(*o.arg_1)?),
            arg_2: Box::new(transformer.transform_expr(*o.arg_2)?),
        })),
        Expr::LComprehension(c) => Ok(Expr::LComprehension(LComprehension {
            pos: c.pos,
            template: Box::new(transformer.transform_expr(*c.template)?),
            qualifiers: c
                .qualifiers
                .into_iter()
                .map(|q| transformer.transform_qualifier(q))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::BComprehension(c) => Ok(Expr::BComprehension(BComprehension {
            pos: c.pos,
            template: Box::new(transformer.transform_expr(*c.template)?),
            qualifiers: c
                .qualifiers
                .into_iter()
                .map(|q| transformer.transform_qualifier(q))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::MComprehension(c) => Ok(Expr::MComprehension(MComprehension {
            pos: c.pos,
            k_template: Box::new(transformer.transform_expr(*c.k_template)?),
            v_template: Box::new(transformer.transform_expr(*c.v_template)?),
            qualifiers: c
                .qualifiers
                .into_iter()
                .map(|q| transformer.transform_qualifier(q))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::Binary(b) => Ok(Expr::Binary(Binary {
            pos: b.pos,
            elems: b
                .elems
                .into_iter()
                .map(|e| transformer.transform_binary_elem(e))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::Catch(c) => Ok(Expr::Catch(Catch {
            pos: c.pos,
            expr: Box::new(transformer.transform_expr(*c.expr)?),
        })),
        Expr::TryCatchExpr(e) => Ok(Expr::TryCatchExpr(TryCatchExpr {
            pos: e.pos,
            try_body: transformer.transform_body(e.try_body)?,
            catch_clauses: e
                .catch_clauses
                .into_iter()
                .map(|c| transformer.transform_clause(c))
                .collect::<Result<Vec<_>, _>>()?,
            after_body: e
                .after_body
                .map_or(Ok(None), |b| transformer.transform_body(b).map(Some))?,
        })),
        Expr::TryOfCatchExpr(e) => Ok(Expr::TryOfCatchExpr(TryOfCatchExpr {
            pos: e.pos,
            try_body: transformer.transform_body(e.try_body)?,
            try_clauses: e
                .try_clauses
                .into_iter()
                .map(|c| transformer.transform_clause(c))
                .collect::<Result<Vec<_>, _>>()?,
            catch_clauses: e
                .catch_clauses
                .into_iter()
                .map(|c| transformer.transform_clause(c))
                .collect::<Result<Vec<_>, _>>()?,
            after_body: e
                .after_body
                .map_or(Ok(None), |b| transformer.transform_body(b).map(Some))?,
        })),
        Expr::Receive(r) => Ok(Expr::Receive(Receive {
            pos: r.pos,
            clauses: r
                .clauses
                .into_iter()
                .map(|c| transformer.transform_clause(c))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::ReceiveWithTimeout(r) => Ok(Expr::ReceiveWithTimeout(ReceiveWithTimeout {
            pos: r.pos,
            clauses: r
                .clauses
                .into_iter()
                .map(|c| transformer.transform_clause(c))
                .collect::<Result<Vec<_>, _>>()?,
            timeout: Box::new(transformer.transform_expr(*r.timeout)?),
            timeout_body: transformer.transform_body(r.timeout_body)?,
        })),
        Expr::RecordCreate(r) => Ok(Expr::RecordCreate(RecordCreate {
            pos: r.pos,
            rec_name: r.rec_name,
            fields: r
                .fields
                .into_iter()
                .map(|f| transformer.transform_record_field(f))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::RecordUpdate(r) => Ok(Expr::RecordUpdate(RecordUpdate {
            pos: r.pos,
            rec_name: r.rec_name,
            expr: Box::new(transformer.transform_expr(*r.expr)?),
            fields: r
                .fields
                .into_iter()
                .map(|f| {
                    transformer
                        .transform_expr(f.value)
                        .map(|value| RecordFieldNamed {
                            value,
                            name: f.name,
                        })
                })
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::RecordSelect(r) => Ok(Expr::RecordSelect(RecordSelect {
            pos: r.pos,
            rec_name: r.rec_name,
            field_name: r.field_name,
            expr: Box::new(transformer.transform_expr(*r.expr)?),
        })),
        Expr::RecordIndex(r) => Ok(Expr::RecordIndex(r)),
        Expr::MapCreate(m) => Ok(Expr::MapCreate(MapCreate {
            pos: m.pos,
            kvs: m
                .kvs
                .into_iter()
                .map(|(k, v)| {
                    transformer.transform_expr(k).and_then(|k_trans| {
                        transformer
                            .transform_expr(v)
                            .map(|v_trans| (k_trans, v_trans))
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::MapUpdate(m) => Ok(Expr::MapUpdate(MapUpdate {
            pos: m.pos,
            map: Box::new(transformer.transform_expr(*m.map)?),
            kvs: m
                .kvs
                .into_iter()
                .map(|(k, v)| {
                    transformer.transform_expr(k).and_then(|k_trans| {
                        transformer
                            .transform_expr(v)
                            .map(|v_trans| (k_trans, v_trans))
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::Maybe(m) => Ok(Expr::Maybe(Maybe {
            pos: m.pos,
            body: transformer.transform_body(m.body)?,
        })),
        Expr::MaybeElse(m) => Ok(Expr::MaybeElse(MaybeElse {
            pos: m.pos,
            body: transformer.transform_body(m.body)?,
            else_clauses: m
                .else_clauses
                .into_iter()
                .map(|c| transformer.transform_clause(c))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Expr::MaybeMatch(m) => Ok(Expr::MaybeMatch(MaybeMatch {
            pos: m.pos,
            pat: transformer.transform_pat(m.pat)?,
            arg: Box::new(transformer.transform_expr(*m.arg)?),
        })),
    }
}

pub fn walk_pat_binary_elem<T, V: Transformer<T>>(
    transformer: &mut V,
    elem: PatBinaryElem,
) -> Result<PatBinaryElem, T> {
    let pat = transformer.transform_pat(elem.pat)?;
    let size = elem
        .size
        .map_or(Ok(None), |s| transformer.transform_expr(s).map(Some))?;
    Ok(PatBinaryElem {
        pat,
        size,
        pos: elem.pos,
        specifier: elem.specifier,
    })
}

pub fn walk_pat<T, V: Transformer<T>>(transformer: &mut V, p: Pat) -> Result<Pat, T> {
    match p {
        Pat::PatWild(p) => Ok(Pat::PatWild(p)),
        Pat::PatMatch(m) => Ok(Pat::PatMatch(PatMatch {
            pat: Box::new(transformer.transform_pat(*m.pat)?),
            arg: Box::new(transformer.transform_pat(*m.arg)?),
            pos: m.pos,
        })),
        Pat::PatTuple(t) => Ok(Pat::PatTuple(PatTuple {
            pos: t.pos,
            elems: t
                .elems
                .into_iter()
                .map(|p| transformer.transform_pat(p))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Pat::PatString(s) => Ok(Pat::PatString(s)),
        Pat::PatNil(n) => Ok(Pat::PatNil(n)),
        Pat::PatCons(c) => Ok(Pat::PatCons(PatCons {
            pos: c.pos,
            h: Box::new(transformer.transform_pat(*c.h)?),
            t: Box::new(transformer.transform_pat(*c.t)?),
        })),
        Pat::PatInt(i) => Ok(Pat::PatInt(i)),
        Pat::PatNumber(n) => Ok(Pat::PatNumber(n)),
        Pat::PatAtom(a) => Ok(Pat::PatAtom(a)),
        Pat::PatVar(v) => Ok(Pat::PatVar(v)),
        Pat::PatRecord(r) => Ok(Pat::PatRecord(PatRecord {
            pos: r.pos,
            rec_name: r.rec_name,
            fields: r
                .fields
                .into_iter()
                .map(|f| {
                    transformer
                        .transform_pat(f.pat)
                        .map(|pat| PatRecordFieldNamed { name: f.name, pat })
                })
                .collect::<Result<Vec<_>, _>>()?,
            gen_: r.gen_.map_or(Ok(None), |g| {
                transformer.transform_pat(*g).map(|pat| Some(Box::new(pat)))
            })?,
        })),
        Pat::PatRecordIndex(r) => Ok(Pat::PatRecordIndex(r)),
        Pat::PatUnOp(o) => Ok(Pat::PatUnOp(PatUnOp {
            pos: o.pos,
            op: o.op,
            arg: Box::new(transformer.transform_pat(*o.arg)?),
        })),
        Pat::PatBinOp(o) => Ok(Pat::PatBinOp(PatBinOp {
            pos: o.pos,
            op: o.op,
            arg_1: Box::new(transformer.transform_pat(*o.arg_1)?),
            arg_2: Box::new(transformer.transform_pat(*o.arg_2)?),
        })),
        Pat::PatBinary(b) => Ok(Pat::PatBinary(PatBinary {
            pos: b.pos,
            elems: b
                .elems
                .into_iter()
                .map(|e| transformer.transform_pat_binary_elem(e))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Pat::PatMap(m) => Ok(Pat::PatMap(PatMap {
            pos: m.pos,
            kvs: m
                .kvs
                .into_iter()
                .map(|(k, v)| {
                    transformer.transform_test(k).and_then(|k_trans| {
                        transformer
                            .transform_pat(v)
                            .map(|v_trans| (k_trans, v_trans))
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
        })),
    }
}

pub fn walk_guard<T, V: Transformer<T>>(transformer: &mut V, g: Guard) -> Result<Guard, T> {
    Ok(Guard {
        tests: g
            .tests
            .into_iter()
            .map(|t| transformer.transform_test(t))
            .collect::<Result<Vec<_>, _>>()?,
    })
}

pub fn walk_test_record_field<T, V: Transformer<T>>(
    transformer: &mut V,
    f: TestRecordField,
) -> Result<TestRecordField, T> {
    match f {
        TestRecordField::TestRecordFieldNamed(f) => Ok(TestRecordField::TestRecordFieldNamed(
            TestRecordFieldNamed {
                value: transformer.transform_test(f.value)?,
                name: f.name,
            },
        )),
        TestRecordField::TestRecordFieldGen(f) => {
            Ok(TestRecordField::TestRecordFieldGen(TestRecordFieldGen {
                value: transformer.transform_test(f.value)?,
            }))
        }
    }
}

pub fn walk_test<T, V: Transformer<T>>(transformer: &mut V, t: Test) -> Result<Test, T> {
    match t {
        Test::TestVar(v) => Ok(Test::TestVar(v)),
        Test::TestAtom(a) => Ok(Test::TestAtom(a)),
        Test::TestNumber(n) => Ok(Test::TestNumber(n)),
        Test::TestTuple(t) => Ok(Test::TestTuple(TestTuple {
            pos: t.pos,
            elems: t
                .elems
                .into_iter()
                .map(|t| transformer.transform_test(t))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Test::TestString(s) => Ok(Test::TestString(s)),
        Test::TestNil(n) => Ok(Test::TestNil(n)),
        Test::TestCons(c) => Ok(Test::TestCons(TestCons {
            pos: c.pos,
            h: Box::new(transformer.transform_test(*c.h)?),
            t: Box::new(transformer.transform_test(*c.t)?),
        })),
        Test::TestCall(c) => Ok(Test::TestCall(TestCall {
            pos: c.pos,
            id: c.id,
            args: c
                .args
                .into_iter()
                .map(|a| transformer.transform_test(a))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Test::TestRecordCreate(r) => Ok(Test::TestRecordCreate(TestRecordCreate {
            pos: r.pos,
            rec_name: r.rec_name,
            fields: r
                .fields
                .into_iter()
                .map(|f| transformer.transform_test_record_field(f))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Test::TestRecordSelect(r) => Ok(Test::TestRecordSelect(TestRecordSelect {
            pos: r.pos,
            rec: Box::new(transformer.transform_test(*r.rec)?),
            rec_name: r.rec_name,
            field_name: r.field_name,
        })),
        Test::TestRecordIndex(r) => Ok(Test::TestRecordIndex(r)),
        Test::TestMapCreate(m) => Ok(Test::TestMapCreate(TestMapCreate {
            pos: m.pos,
            kvs: m
                .kvs
                .into_iter()
                .map(|(k, v)| {
                    transformer.transform_test(k).and_then(|k_trans| {
                        transformer
                            .transform_test(v)
                            .map(|v_trans| (k_trans, v_trans))
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Test::TestMapUpdate(m) => Ok(Test::TestMapUpdate(TestMapUpdate {
            pos: m.pos,
            map: Box::new(transformer.transform_test(*m.map)?),
            kvs: m
                .kvs
                .into_iter()
                .map(|(k, v)| {
                    transformer.transform_test(k).and_then(|k_trans| {
                        transformer
                            .transform_test(v)
                            .map(|v_trans| (k_trans, v_trans))
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
        })),
        Test::TestUnOp(o) => Ok(Test::TestUnOp(TestUnOp {
            pos: o.pos,
            op: o.op,
            arg: Box::new(transformer.transform_test(*o.arg)?),
        })),
        Test::TestBinOp(o) => Ok(Test::TestBinOp(TestBinOp {
            pos: o.pos,
            op: o.op,
            arg_1: Box::new(transformer.transform_test(*o.arg_1)?),
            arg_2: Box::new(transformer.transform_test(*o.arg_2)?),
        })),
        Test::TestBinaryLit(b) => Ok(Test::TestBinaryLit(b)),
    }
}

pub fn walk_form<T, V: Transformer<T>>(
    transformer: &mut V,
    form: ExternalForm,
) -> Result<ExternalForm, T> {
    match form {
        ExternalForm::Module(m) => Ok(ExternalForm::Module(m)),
        ExternalForm::CompileExportAll(c) => Ok(ExternalForm::CompileExportAll(c)),
        ExternalForm::Export(e) => Ok(ExternalForm::Export(e)),
        ExternalForm::Import(i) => Ok(ExternalForm::Import(i)),
        ExternalForm::ExportType(e) => Ok(ExternalForm::ExportType(e)),
        ExternalForm::FunDecl(decl) => Ok(ExternalForm::FunDecl(FunDecl {
            pos: decl.pos,
            id: decl.id,
            clauses: decl
                .clauses
                .into_iter()
                .map(|c| transformer.transform_clause(c))
                .collect::<Result<Vec<_>, _>>()?,
        })),
        ExternalForm::File(f) => Ok(ExternalForm::File(f)),
        ExternalForm::ElpMetadata(m) => Ok(ExternalForm::ElpMetadata(m)),
        ExternalForm::Behaviour(b) => Ok(ExternalForm::Behaviour(b)),
        ExternalForm::EqwalizerNowarnFunction(e) => Ok(ExternalForm::EqwalizerNowarnFunction(e)),
        ExternalForm::EqwalizerUnlimitedRefinement(e) => {
            Ok(ExternalForm::EqwalizerUnlimitedRefinement(e))
        }
        ExternalForm::TypingAttribute(t) => Ok(ExternalForm::TypingAttribute(t)),
        ExternalForm::ExternalTypeDecl(decl) => Ok(ExternalForm::ExternalTypeDecl(decl)),
        ExternalForm::ExternalOpaqueDecl(decl) => Ok(ExternalForm::ExternalOpaqueDecl(decl)),
        ExternalForm::ExternalFunSpec(spec) => Ok(ExternalForm::ExternalFunSpec(spec)),
        ExternalForm::ExternalCallback(cb) => Ok(ExternalForm::ExternalCallback(cb)),
        ExternalForm::ExternalOptionalCallbacks(cb) => {
            Ok(ExternalForm::ExternalOptionalCallbacks(cb))
        }
        ExternalForm::ExternalRecDecl(decl) => Ok(ExternalForm::ExternalRecDecl(ExternalRecDecl {
            pos: decl.pos,
            name: decl.name,
            fields: decl
                .fields
                .into_iter()
                .map(|f| {
                    f.default_value
                        .map_or(Ok(None), |val| transformer.transform_expr(val).map(Some))
                        .map(|default_value| ExternalRecField { default_value, ..f })
                })
                .collect::<Result<Vec<_>, _>>()?,
        })),
    }
}
