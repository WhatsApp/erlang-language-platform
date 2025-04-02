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
use super::ext_types::ExtType;
use super::form::ExternalForm;
use super::guard::Guard;
use super::guard::Test;
use super::guard::TestRecordField;
use super::pat::Pat;
use super::pat::PatBinaryElem;
use super::types::Type;
use super::AST;

pub trait Visitor<'a, T>: Sized {
    fn visit_ast(&mut self, ast: &'a AST) -> Result<(), T> {
        ast.forms.iter().try_for_each(|form| self.visit_form(form))
    }
    fn visit_expr(&mut self, expr: &'a Expr) -> Result<(), T> {
        walk_expr(self, expr)
    }
    fn visit_pat(&mut self, pat: &'a Pat) -> Result<(), T> {
        walk_pat(self, pat)
    }
    fn visit_test(&mut self, test: &'a Test) -> Result<(), T> {
        walk_test(self, test)
    }
    fn visit_clause(&mut self, clause: &'a Clause) -> Result<(), T> {
        walk_clause(self, clause)
    }
    fn visit_body(&mut self, body: &'a Body) -> Result<(), T> {
        walk_body(self, body)
    }
    fn visit_guard(&mut self, guard: &'a Guard) -> Result<(), T> {
        walk_guard(self, guard)
    }
    fn visit_type(&mut self, ty: &'a Type) -> Result<(), T> {
        walk_type(self, ty)
    }
    fn visit_ext_type(&mut self, ty: &'a ExtType) -> Result<(), T> {
        walk_ext_type(self, ty)
    }
    fn visit_form(&mut self, form: &'a ExternalForm) -> Result<(), T> {
        walk_form(self, form)
    }
    fn visit_qualifier(&mut self, qualifier: &'a Qualifier) -> Result<(), T> {
        walk_qualifier(self, qualifier)
    }
    fn visit_binary_elem(&mut self, elem: &'a BinaryElem) -> Result<(), T> {
        walk_binary_elem(self, elem)
    }
    fn visit_pat_binary_elem(&mut self, elem: &'a PatBinaryElem) -> Result<(), T> {
        walk_pat_binary_elem(self, elem)
    }
    fn visit_record_field(&mut self, field: &'a RecordField) -> Result<(), T> {
        walk_record_field(self, field)
    }
    fn visit_test_record_field(&mut self, field: &'a TestRecordField) -> Result<(), T> {
        walk_test_record_field(self, field)
    }
}

pub fn walk_body<'a, T, V: Visitor<'a, T>>(visitor: &mut V, body: &'a Body) -> Result<(), T> {
    body.exprs.iter().try_for_each(|e| visitor.visit_expr(e))
}

pub fn walk_clause<'a, T, V: Visitor<'a, T>>(visitor: &mut V, clause: &'a Clause) -> Result<(), T> {
    clause.pats.iter().try_for_each(|p| visitor.visit_pat(p))?;
    clause
        .guards
        .iter()
        .try_for_each(|g| visitor.visit_guard(g))?;
    visitor.visit_body(&clause.body)
}

pub fn walk_qualifier<'a, T, V: Visitor<'a, T>>(
    visitor: &mut V,
    qualifier: &'a Qualifier,
) -> Result<(), T> {
    match qualifier {
        Qualifier::LGenerate(g) => {
            visitor.visit_pat(&g.pat)?;
            visitor.visit_expr(&g.expr)
        }
        Qualifier::LGenerateStrict(g) => {
            visitor.visit_pat(&g.pat)?;
            visitor.visit_expr(&g.expr)
        }
        Qualifier::BGenerate(g) => {
            visitor.visit_pat(&g.pat)?;
            visitor.visit_expr(&g.expr)
        }
        Qualifier::BGenerateStrict(g) => {
            visitor.visit_pat(&g.pat)?;
            visitor.visit_expr(&g.expr)
        }
        Qualifier::MGenerate(g) => {
            visitor.visit_pat(&g.k_pat)?;
            visitor.visit_pat(&g.v_pat)?;
            visitor.visit_expr(&g.expr)
        }
        Qualifier::MGenerateStrict(g) => {
            visitor.visit_pat(&g.k_pat)?;
            visitor.visit_pat(&g.v_pat)?;
            visitor.visit_expr(&g.expr)
        }
        Qualifier::Zip(zip) => zip
            .generators
            .iter()
            .try_for_each(|q| visitor.visit_qualifier(q)),
        Qualifier::Filter(f) => visitor.visit_expr(&f.expr),
    }
}

pub fn walk_binary_elem<'a, T, V: Visitor<'a, T>>(
    visitor: &mut V,
    elem: &'a BinaryElem,
) -> Result<(), T> {
    visitor.visit_expr(&elem.expr)?;
    elem.size.as_ref().map_or(Ok(()), |s| visitor.visit_expr(s))
}

pub fn walk_record_field<'a, T, V: Visitor<'a, T>>(
    visitor: &mut V,
    field: &'a RecordField,
) -> Result<(), T> {
    match field {
        RecordField::RecordFieldGen(f) => visitor.visit_expr(&f.value),
        RecordField::RecordFieldNamed(f) => visitor.visit_expr(&f.value),
    }
}

pub fn walk_expr<'a, T, V: Visitor<'a, T>>(visitor: &mut V, e: &'a Expr) -> Result<(), T> {
    match e {
        Expr::Var(_) => Ok(()),
        Expr::AtomLit(_) => Ok(()),
        Expr::IntLit(_) => Ok(()),
        Expr::FloatLit(_) => Ok(()),
        Expr::Block(b) => visitor.visit_body(&b.body),
        Expr::Match(m) => {
            visitor.visit_pat(&m.pat)?;
            visitor.visit_expr(&m.expr)
        }
        Expr::Tuple(t) => t.elems.iter().try_for_each(|e| visitor.visit_expr(e)),
        Expr::StringLit(_) => Ok(()),
        Expr::NilLit(_) => Ok(()),
        Expr::Cons(c) => {
            visitor.visit_expr(&c.h)?;
            visitor.visit_expr(&c.t)
        }
        Expr::Case(c) => {
            visitor.visit_expr(&c.expr)?;
            c.clauses.iter().try_for_each(|c| visitor.visit_clause(c))
        }
        Expr::If(i) => i.clauses.iter().try_for_each(|c| visitor.visit_clause(c)),
        Expr::LocalCall(c) => c.args.iter().try_for_each(|e| visitor.visit_expr(e)),
        Expr::DynCall(c) => {
            visitor.visit_expr(&c.f)?;
            c.args.iter().try_for_each(|e| visitor.visit_expr(e))
        }
        Expr::RemoteCall(c) => c.args.iter().try_for_each(|e| visitor.visit_expr(e)),
        Expr::LocalFun(_) => Ok(()),
        Expr::RemoteFun(_) => Ok(()),
        Expr::DynRemoteFun(f) => {
            visitor.visit_expr(&f.module)?;
            visitor.visit_expr(&f.name)
        }
        Expr::DynRemoteFunArity(f) => {
            visitor.visit_expr(&f.module)?;
            visitor.visit_expr(&f.name)?;
            visitor.visit_expr(&f.arity)
        }
        Expr::Lambda(l) => l.clauses.iter().try_for_each(|c| visitor.visit_clause(c)),
        Expr::UnOp(o) => visitor.visit_expr(&o.arg),
        Expr::BinOp(o) => {
            visitor.visit_expr(&o.arg_1)?;
            visitor.visit_expr(&o.arg_2)
        }
        Expr::LComprehension(c) => {
            visitor.visit_expr(&c.template)?;
            c.qualifiers
                .iter()
                .try_for_each(|q| visitor.visit_qualifier(q))
        }
        Expr::BComprehension(c) => {
            visitor.visit_expr(&c.template)?;
            c.qualifiers
                .iter()
                .try_for_each(|q| visitor.visit_qualifier(q))
        }
        Expr::MComprehension(c) => {
            visitor.visit_expr(&c.k_template)?;
            visitor.visit_expr(&c.v_template)?;
            c.qualifiers
                .iter()
                .try_for_each(|q| visitor.visit_qualifier(q))
        }
        Expr::Binary(b) => b
            .elems
            .iter()
            .try_for_each(|e| visitor.visit_binary_elem(e)),
        Expr::Catch(c) => visitor.visit_expr(&c.expr),
        Expr::TryCatchExpr(e) => {
            visitor.visit_body(&e.try_body)?;
            e.catch_clauses
                .iter()
                .try_for_each(|c| visitor.visit_clause(c))?;
            e.after_body
                .as_ref()
                .map_or(Ok(()), |b| visitor.visit_body(b))
        }
        Expr::TryOfCatchExpr(e) => {
            visitor.visit_body(&e.try_body)?;
            e.try_clauses
                .iter()
                .try_for_each(|c| visitor.visit_clause(c))?;
            e.catch_clauses
                .iter()
                .try_for_each(|c| visitor.visit_clause(c))?;
            e.after_body
                .as_ref()
                .map_or(Ok(()), |b| visitor.visit_body(b))
        }
        Expr::Receive(r) => r.clauses.iter().try_for_each(|c| visitor.visit_clause(c)),
        Expr::ReceiveWithTimeout(r) => {
            r.clauses.iter().try_for_each(|c| visitor.visit_clause(c))?;
            visitor.visit_expr(&r.timeout)?;
            visitor.visit_body(&r.timeout_body)
        }
        Expr::RecordCreate(r) => r
            .fields
            .iter()
            .try_for_each(|f| visitor.visit_record_field(f)),
        Expr::RecordUpdate(r) => {
            visitor.visit_expr(&r.expr)?;
            r.fields
                .iter()
                .try_for_each(|f| visitor.visit_expr(&f.value))
        }
        Expr::RecordSelect(r) => visitor.visit_expr(&r.expr),
        Expr::RecordIndex(_) => Ok(()),
        Expr::MapCreate(m) => m
            .kvs
            .iter()
            .try_for_each(|(k, v)| visitor.visit_expr(k).and_then(|()| visitor.visit_expr(v))),
        Expr::MapUpdate(m) => {
            visitor.visit_expr(&m.map)?;
            m.kvs
                .iter()
                .try_for_each(|(k, v)| visitor.visit_expr(k).and_then(|()| visitor.visit_expr(v)))
        }
        Expr::Maybe(m) => visitor.visit_body(&m.body),
        Expr::MaybeElse(m) => {
            visitor.visit_body(&m.body)?;
            m.else_clauses
                .iter()
                .try_for_each(|c| visitor.visit_clause(c))
        }
        Expr::MaybeMatch(m) => {
            visitor.visit_pat(&m.pat)?;
            visitor.visit_expr(&m.arg)
        }
    }
}

pub fn walk_pat_binary_elem<'a, T, V: Visitor<'a, T>>(
    visitor: &mut V,
    elem: &'a PatBinaryElem,
) -> Result<(), T> {
    visitor.visit_pat(&elem.pat)?;
    elem.size.as_ref().map_or(Ok(()), |s| visitor.visit_expr(s))
}

pub fn walk_pat<'a, T, V: Visitor<'a, T>>(visitor: &mut V, p: &'a Pat) -> Result<(), T> {
    match p {
        Pat::PatWild(_) => Ok(()),
        Pat::PatMatch(m) => {
            visitor.visit_pat(&m.pat)?;
            visitor.visit_pat(&m.arg)
        }
        Pat::PatTuple(t) => t.elems.iter().try_for_each(|p| visitor.visit_pat(p)),
        Pat::PatString(_) => Ok(()),
        Pat::PatNil(_) => Ok(()),
        Pat::PatCons(c) => {
            visitor.visit_pat(&c.h)?;
            visitor.visit_pat(&c.t)
        }
        Pat::PatInt(_) => Ok(()),
        Pat::PatNumber(_) => Ok(()),
        Pat::PatAtom(_) => Ok(()),
        Pat::PatVar(_) => Ok(()),
        Pat::PatRecord(r) => {
            r.fields
                .iter()
                .try_for_each(|f| visitor.visit_pat(&f.pat))?;
            r.gen.as_ref().map_or(Ok(()), |g| visitor.visit_pat(g))
        }
        Pat::PatRecordIndex(_) => Ok(()),
        Pat::PatUnOp(o) => visitor.visit_pat(&o.arg),
        Pat::PatBinOp(o) => {
            visitor.visit_pat(&o.arg_1)?;
            visitor.visit_pat(&o.arg_2)
        }
        Pat::PatBinary(b) => b
            .elems
            .iter()
            .try_for_each(|e| visitor.visit_pat_binary_elem(e)),
        Pat::PatMap(m) => m
            .kvs
            .iter()
            .try_for_each(|(k, v)| visitor.visit_test(k).and_then(|()| visitor.visit_pat(v))),
    }
}

pub fn walk_guard<'a, T, V: Visitor<'a, T>>(visitor: &mut V, g: &'a Guard) -> Result<(), T> {
    g.tests.iter().try_for_each(|t| visitor.visit_test(t))
}

pub fn walk_test_record_field<'a, T, V: Visitor<'a, T>>(
    visitor: &mut V,
    f: &'a TestRecordField,
) -> Result<(), T> {
    match f {
        TestRecordField::TestRecordFieldNamed(f) => visitor.visit_test(&f.value),
        TestRecordField::TestRecordFieldGen(f) => visitor.visit_test(&f.value),
    }
}

pub fn walk_test<'a, T, V: Visitor<'a, T>>(visitor: &mut V, t: &'a Test) -> Result<(), T> {
    match t {
        Test::TestVar(_) => Ok(()),
        Test::TestAtom(_) => Ok(()),
        Test::TestNumber(_) => Ok(()),
        Test::TestTuple(t) => t.elems.iter().try_for_each(|t| visitor.visit_test(t)),
        Test::TestString(_) => Ok(()),
        Test::TestNil(_) => Ok(()),
        Test::TestCons(c) => {
            visitor.visit_test(&c.h)?;
            visitor.visit_test(&c.t)
        }
        Test::TestCall(c) => c.args.iter().try_for_each(|a| visitor.visit_test(a)),
        Test::TestRecordCreate(r) => r
            .fields
            .iter()
            .try_for_each(|f| visitor.visit_test_record_field(f)),
        Test::TestRecordSelect(r) => visitor.visit_test(&r.rec),
        Test::TestRecordIndex(_) => Ok(()),
        Test::TestMapCreate(m) => m
            .kvs
            .iter()
            .try_for_each(|(k, v)| visitor.visit_test(k).and_then(|()| visitor.visit_test(v))),
        Test::TestMapUpdate(m) => {
            visitor.visit_test(&m.map)?;
            m.kvs
                .iter()
                .try_for_each(|(k, v)| visitor.visit_test(k).and_then(|()| visitor.visit_test(v)))
        }
        Test::TestUnOp(o) => visitor.visit_test(&o.arg),
        Test::TestBinOp(o) => {
            visitor.visit_test(&o.arg_1)?;
            visitor.visit_test(&o.arg_2)
        }
        Test::TestBinaryLit(_) => Ok(()),
    }
}

pub fn walk_type<'a, T, V: Visitor<'a, T>>(visitor: &mut V, ty: &'a Type) -> Result<(), T> {
    ty.walk(&mut |ty| visitor.visit_type(ty))
}

pub fn walk_ext_type<'a, T, V: Visitor<'a, T>>(visitor: &mut V, ty: &'a ExtType) -> Result<(), T> {
    ty.walk(&mut |ty| visitor.visit_ext_type(ty))
}

pub fn walk_form<'a, T, V: Visitor<'a, T>>(
    visitor: &mut V,
    form: &'a ExternalForm,
) -> Result<(), T> {
    match form {
        ExternalForm::Module(_) => Ok(()),
        ExternalForm::CompileExportAll(_) => Ok(()),
        ExternalForm::Export(_) => Ok(()),
        ExternalForm::Import(_) => Ok(()),
        ExternalForm::ExportType(_) => Ok(()),
        ExternalForm::FunDecl(decl) => decl
            .clauses
            .iter()
            .try_for_each(|c| visitor.visit_clause(c)),
        ExternalForm::File(_) => Ok(()),
        ExternalForm::ElpMetadata(_) => Ok(()),
        ExternalForm::Behaviour(_) => Ok(()),
        ExternalForm::EqwalizerNowarnFunction(_) => Ok(()),
        ExternalForm::EqwalizerUnlimitedRefinement(_) => Ok(()),
        ExternalForm::TypingAttribute(_) => Ok(()),
        ExternalForm::ExternalTypeDecl(decl) => visitor.visit_ext_type(&decl.body),
        ExternalForm::ExternalOpaqueDecl(decl) => visitor.visit_ext_type(&decl.body),
        ExternalForm::ExternalFunSpec(spec) => spec.types.iter().try_for_each(|ty| {
            visitor
                .visit_ext_type(&ty.ty.res_ty)
                .and_then(|()| {
                    ty.ty
                        .arg_tys
                        .iter()
                        .try_for_each(|ty| visitor.visit_ext_type(ty))
                })
                .and_then(|()| {
                    ty.constraints
                        .iter()
                        .try_for_each(|c| visitor.visit_ext_type(&c.ty))
                })
        }),
        ExternalForm::ExternalCallback(cb) => cb.types.iter().try_for_each(|ty| {
            visitor
                .visit_ext_type(&ty.ty.res_ty)
                .and_then(|()| {
                    ty.ty
                        .arg_tys
                        .iter()
                        .try_for_each(|ty| visitor.visit_ext_type(ty))
                })
                .and_then(|()| {
                    ty.constraints
                        .iter()
                        .try_for_each(|c| visitor.visit_ext_type(&c.ty))
                })
        }),
        ExternalForm::ExternalOptionalCallbacks(_) => Ok(()),
        ExternalForm::ExternalRecDecl(decl) => decl.fields.iter().try_for_each(|f| {
            f.tp.as_ref()
                .map_or(Ok(()), |ty| visitor.visit_ext_type(ty))
                .and_then(|()| {
                    f.default_value
                        .as_ref()
                        .map_or(Ok(()), |val| visitor.visit_expr(val))
                })
        }),
    }
}
