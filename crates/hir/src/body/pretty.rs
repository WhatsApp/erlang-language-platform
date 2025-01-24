/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Write as _;
use std::str;

use super::SpecOrCallback;
use crate::db::InternDatabase;
use crate::expr::Guards;
use crate::expr::MaybeExpr;
use crate::expr::SsrPlaceholder;
use crate::expr::StringVariant;
use crate::AnyAttribute;
use crate::AttributeBody;
use crate::BinarySeg;
use crate::Body;
use crate::CallTarget;
use crate::Clause;
use crate::ComprehensionBuilder;
use crate::ComprehensionExpr;
use crate::Expr;
use crate::ExprId;
use crate::FormList;
use crate::FunType;
use crate::FunctionBody;
use crate::FunctionClause;
use crate::ListType;
use crate::Literal;
use crate::Pat;
use crate::PatId;
use crate::Record;
use crate::RecordBody;
use crate::RecordFieldBody;
use crate::SpecBody;
use crate::SpecSig;
use crate::Term;
use crate::TermId;
use crate::TypeAlias;
use crate::TypeBody;
use crate::TypeExpr;
use crate::TypeExprId;
use crate::Var;

pub fn print_function_clause(
    db: &dyn InternDatabase,
    body: &FunctionBody,
    form: &FunctionClause,
) -> String {
    let mut out = String::new();

    let mut sep = "";
    for (_idx, clause) in body.clauses.iter() {
        write!(out, "{}", sep).unwrap();
        sep = ";";
        let mut printer = Printer::new(db, &clause.body);
        printer
            .print_clause(
                &clause.clause,
                clause.name.clone().unwrap_or(form.name.clone()).name(),
            )
            .unwrap();
        write!(out, "{}", printer.to_string_raw()).unwrap();
    }
    write!(out, ".\n").unwrap();

    out
}

pub fn print_type_alias(db: &dyn InternDatabase, body: &TypeBody, form: &TypeAlias) -> String {
    let mut printer = Printer::new(db, &body.body);

    match form {
        TypeAlias::Regular { .. } => write!(printer, "-type ").unwrap(),
        TypeAlias::Opaque { .. } => write!(printer, "-opaque ").unwrap(),
    }

    printer
        .print_args(form.name().name(), &body.vars, |this, &var| {
            write!(this, "{}", db.lookup_var(var))
        })
        .unwrap();
    write!(printer, " :: ").unwrap();
    printer.print_type(&printer.body[body.ty]).unwrap();
    write!(printer, ".").unwrap();

    printer.to_string()
}

pub fn print_spec(db: &dyn InternDatabase, body: &SpecBody, form: SpecOrCallback) -> String {
    let mut printer = Printer::new(db, &body.body);

    match form {
        SpecOrCallback::Spec(spec) => writeln!(printer, "-spec {}", spec.name.name()).unwrap(),
        SpecOrCallback::Callback(cb) => writeln!(printer, "-callback {}", cb.name.name()).unwrap(),
    }
    printer.indent_level += 1;

    let mut sep = "";
    for sig in &body.sigs {
        write!(printer, "{}", sep).unwrap();
        sep = ";\n";
        printer.print_sig(sig).unwrap();
    }

    write!(printer, ".").unwrap();

    printer.to_string()
}

pub fn print_record(
    db: &dyn InternDatabase,
    body: &RecordBody,
    form: &Record,
    form_list: &FormList,
) -> String {
    let mut printer = Printer::new(db, &body.body);

    write!(printer, "-record({}, {{", form.name).unwrap();
    printer.indent_level += 1;

    let mut sep = "\n";
    for field in &body.fields {
        write!(printer, "{}", sep).unwrap();
        sep = ",\n";
        printer.print_field(field, form_list).unwrap();
    }

    printer.indent_level -= 1;
    write!(printer, "\n}}).").unwrap();

    printer.to_string()
}

pub fn print_attribute(
    db: &dyn InternDatabase,
    body: &AttributeBody,
    form: &AnyAttribute,
) -> String {
    let mut printer = Printer::new(db, &body.body);

    match form {
        AnyAttribute::CompileOption(_) => write!(printer, "-compile(").unwrap(),
        AnyAttribute::Attribute(attr) => write!(printer, "-{}(", attr.name).unwrap(),
    }
    printer.print_term(&printer.body[body.value]).unwrap();
    write!(printer, ").").unwrap();

    printer.to_string()
}

pub fn print_expr(db: &dyn InternDatabase, body: &Body, expr: ExprId) -> String {
    let mut printer = Printer::new(db, body);
    printer.print_expr(&body[expr]).unwrap();
    printer.to_string()
}

pub fn print_pat(db: &dyn InternDatabase, body: &Body, pat: PatId) -> String {
    let mut printer = Printer::new(db, body);
    printer.print_pat(&body[pat]).unwrap();
    printer.to_string()
}

pub fn print_type(db: &dyn InternDatabase, body: &Body, ty: TypeExprId) -> String {
    let mut printer = Printer::new(db, body);
    printer.print_type(&body[ty]).unwrap();
    printer.to_string()
}

pub fn print_term(db: &dyn InternDatabase, body: &Body, term: TermId) -> String {
    let mut printer = Printer::new(db, body);
    printer.print_term(&body[term]).unwrap();
    printer.to_string()
}

struct Printer<'a> {
    db: &'a dyn InternDatabase,
    body: &'a Body,
    buf: String,
    indent_level: usize,
    needs_indent: bool,
}

impl<'a> Printer<'a> {
    fn new(db: &'a dyn InternDatabase, body: &'a Body) -> Self {
        Printer {
            db,
            body,
            buf: String::new(),
            indent_level: 0,
            needs_indent: true,
        }
    }

    fn to_string(mut self) -> String {
        self.buf.truncate(self.buf.trim_end().len());
        self.buf.push('\n');
        self.buf
    }

    fn to_string_raw(mut self) -> String {
        self.buf.truncate(self.buf.trim_end().len());
        self.buf
    }

    fn print_clause(&mut self, clause: &Clause, name: &str) -> fmt::Result {
        write!(self, "{}(", name)?;
        let mut sep = "";
        for pat in clause.pats.iter().map(|pat_id| &self.body[*pat_id]) {
            write!(self, "{}", sep)?;
            sep = ", ";
            self.print_pat(pat)?;
        }
        write!(self, ")")?;

        self.print_guards(&clause.guards, true)?;
        self.print_clause_body(&clause.exprs)
    }

    fn print_sig(&mut self, sig: &SpecSig) -> fmt::Result {
        self.print_args("", &sig.args, |this, ty| this.print_type(&self.body[*ty]))?;
        write!(self, " -> ")?;
        self.print_type(&self.body[sig.result])?;
        self.print_type_guards(&sig.guards)
    }

    fn print_field(&mut self, body: &RecordFieldBody, form_list: &FormList) -> fmt::Result {
        write!(self, "{}", form_list[body.field_id].name)?;
        if let Some(expr) = body.expr {
            write!(self, " = ")?;
            self.print_expr(&self.body[expr])?;
        }
        if let Some(ty) = body.ty {
            write!(self, " :: ")?;
            self.print_type(&self.body[ty])?;
        }
        Ok(())
    }

    fn print_pat(&mut self, pat: &Pat) -> fmt::Result {
        match pat {
            Pat::Missing => write!(self, "[missing]"),
            Pat::Literal(lit) => self.print_literal(lit),
            Pat::Var(var) => write!(self, "{}", self.db.lookup_var(*var)),
            Pat::Tuple { pats } => self.print_seq(pats, None, "{", "}", ",", |this, pat| {
                this.print_pat(&this.body[*pat])
            }),
            Pat::List { pats, tail } => {
                self.print_seq(pats, tail.as_ref(), "[", "]", ",", |this, pat| {
                    this.print_pat(&this.body[*pat])
                })
            }
            Pat::Match { lhs, rhs } => {
                self.print_pat(&self.body[*lhs])?;
                write!(self, " = ")?;
                self.print_pat(&self.body[*rhs])
            }
            Pat::UnaryOp { pat, op } => {
                write!(self, "({} ", op)?;
                self.print_pat(&self.body[*pat])?;
                write!(self, ")")
            }
            Pat::BinaryOp { lhs, rhs, op } => {
                write!(self, "(")?;
                self.print_pat(&self.body[*lhs])?;
                write!(self, " {} ", op)?;
                self.print_pat(&self.body[*rhs])?;
                write!(self, ")")
            }
            Pat::Map { fields } => {
                self.print_seq(fields, None, "#{", "}", ",", |this, (key, value)| {
                    this.print_expr(&this.body[*key])?;
                    write!(this, " := ")?;
                    this.print_pat(&this.body[*value])
                })
            }
            Pat::RecordIndex { name, field } => {
                write!(
                    self,
                    "#{}.{}",
                    self.db.lookup_atom(*name),
                    self.db.lookup_atom(*field)
                )
            }
            Pat::Record { name, fields } => {
                write!(self, "#{}", self.db.lookup_atom(*name))?;
                self.print_seq(fields, None, "{", "}", ",", |this, (key, val)| {
                    write!(this, "{} = ", this.db.lookup_atom(*key))?;
                    this.print_pat(&this.body[*val])
                })
            }
            Pat::Binary { segs } => self.print_seq(segs, None, "<<", ">>", ",", |this, seg| {
                this.print_bin_segment(seg, |this, pat| this.print_pat(&this.body[pat]))
            }),
            Pat::MacroCall {
                expansion,
                args: _,
                macro_def: _,
            } => self.print_pat(&self.body[*expansion]),
            Pat::Paren { pat } => self.print_pat(&self.body[*pat]),
            Pat::SsrPlaceholder(ssr) => self.print_ssr_placeholder(ssr),
        }
    }

    fn print_guards(&mut self, guards: &Guards, when_nested: bool) -> fmt::Result {
        if !guards.is_empty() {
            if when_nested {
                self.indent_level += 1;
                writeln!(self, " when")?;
            }
            let mut sep = "";
            for guard_clause in guards {
                write!(self, "{}", sep)?;
                sep = ";\n";
                let mut sep = "";
                for expr in guard_clause {
                    write!(self, "{}", sep)?;
                    sep = ",\n";
                    self.print_expr(&self.body[*expr])?;
                }
            }
            if when_nested {
                self.indent_level -= 1;
                write!(self, "\n->")
            } else {
                write!(self, " ->")
            }
        } else {
            write!(self, " ->")
        }
    }

    fn print_type_guards(&mut self, guards: &[(Var, TypeExprId)]) -> fmt::Result {
        if !guards.is_empty() {
            self.indent_level += 1;
            write!(self, "\nwhen ")?;
            let mut sep = "";
            for (var, ty) in guards {
                write!(self, "{}{} :: ", sep, self.db.lookup_var(*var))?;
                sep = ", ";
                self.print_type(&self.body[*ty])?;
            }
            self.indent_level += 1;
        }
        Ok(())
    }

    fn print_clause_body(&mut self, exprs: &[ExprId]) -> fmt::Result {
        self.indent_level += 1;
        let mut sep = "";
        for expr_id in exprs {
            writeln!(self, "{}", sep)?;
            sep = ",";
            self.print_expr(&self.body[*expr_id])?;
        }
        self.indent_level -= 1;
        Ok(())
    }

    fn print_expr(&mut self, expr: &Expr) -> fmt::Result {
        match expr {
            Expr::Missing => write!(self, "[missing]"),
            Expr::Literal(lit) => self.print_literal(lit),
            Expr::Var(var) => write!(self, "{}", self.db.lookup_var(*var)),
            Expr::Tuple { exprs } => self.print_seq(exprs, None, "{", "}", ",", |this, expr| {
                this.print_expr(&this.body[*expr])
            }),
            Expr::List { exprs, tail } => {
                self.print_seq(exprs, tail.as_ref(), "[", "]", ",", |this, expr| {
                    this.print_expr(&this.body[*expr])
                })
            }
            Expr::Match { lhs, rhs } => {
                self.print_pat(&self.body[*lhs])?;
                write!(self, " = ")?;
                self.print_expr(&self.body[*rhs])
            }
            Expr::UnaryOp { expr, op } => {
                write!(self, "({} ", op)?;
                self.print_expr(&self.body[*expr])?;
                write!(self, ")")
            }
            Expr::BinaryOp { lhs, rhs, op } => {
                write!(self, "(")?;
                self.print_expr(&self.body[*lhs])?;
                write!(self, " {} ", op)?;
                self.print_expr(&self.body[*rhs])?;
                write!(self, ")")
            }
            Expr::Map { fields } => {
                self.print_seq(fields, None, "#{", "}", ",", |this, (key, value)| {
                    this.print_expr(&this.body[*key])?;
                    write!(this, " => ")?;
                    this.print_expr(&this.body[*value])
                })
            }
            Expr::MapUpdate { expr, fields } => {
                self.print_expr(&self.body[*expr])?;
                self.print_seq(fields, None, "#{", "}", ",", |this, (key, op, value)| {
                    this.print_expr(&this.body[*key])?;
                    write!(this, " {} ", op)?;
                    this.print_expr(&this.body[*value])
                })
            }
            Expr::RecordIndex { name, field } => {
                write!(
                    self,
                    "#{}.{}",
                    self.db.lookup_atom(*name),
                    self.db.lookup_atom(*field)
                )
            }
            Expr::Record { name, fields } => {
                write!(self, "#{}", self.db.lookup_atom(*name))?;
                self.print_seq(fields, None, "{", "}", ",", |this, (key, val)| {
                    write!(this, "{} = ", this.db.lookup_atom(*key))?;
                    this.print_expr(&this.body[*val])
                })
            }
            Expr::RecordUpdate { expr, name, fields } => {
                self.print_expr(&self.body[*expr])?;
                write!(self, "#{}", self.db.lookup_atom(*name))?;
                self.print_seq(fields, None, "{", "}", ",", |this, (key, val)| {
                    write!(this, "{} = ", this.db.lookup_atom(*key))?;
                    this.print_expr(&this.body[*val])
                })
            }
            Expr::RecordField { expr, name, field } => {
                self.print_expr(&self.body[*expr])?;
                write!(
                    self,
                    "#{}.{}",
                    self.db.lookup_atom(*name),
                    self.db.lookup_atom(*field)
                )
            }
            Expr::Binary { segs } => self.print_seq(segs, None, "<<", ">>", ",", |this, seg| {
                this.print_bin_segment(seg, |this, expr| this.print_expr(&this.body[expr]))
            }),
            Expr::Catch { expr } => {
                write!(self, "(catch ")?;
                self.print_expr(&self.body[*expr])?;
                write!(self, ")")
            }
            Expr::Block { exprs } => {
                self.print_seq(exprs, None, "begin", "end", ",", |this, expr| {
                    this.print_expr(&this.body[*expr])
                })
            }
            Expr::Case { expr, clauses } => {
                write!(self, "case ")?;
                self.print_expr(&self.body[*expr])?;
                self.print_seq(clauses, None, " of", "end", ";", |this, clause| {
                    this.print_pat(&this.body[clause.pat])?;
                    this.print_guards(&clause.guards, true)?;
                    this.print_clause_body(&clause.exprs)
                })
            }
            Expr::Receive { clauses, after } => {
                self.print_seq(clauses, None, "receive", "", ";", |this, clause| {
                    this.print_pat(&this.body[clause.pat])?;
                    this.print_guards(&clause.guards, true)?;
                    this.print_clause_body(&clause.exprs)
                })?;
                if let Some(after) = after {
                    write!(self, "after ")?;
                    self.print_expr(&self.body[after.timeout])?;
                    write!(self, " ->")?;
                    self.print_clause_body(&after.exprs)?;
                    write!(self, "\nend")
                } else {
                    write!(self, "end")
                }
            }
            Expr::MacroCall {
                expansion,
                args: _,
                macro_def: _,
            } => self.print_expr(&self.body[*expansion]),
            Expr::Call { target, args } => {
                self.print_call_target(target, |this, expr| this.print_expr(&this.body[*expr]))?;
                self.print_seq(args, None, "(", ")", ",", |this, expr| {
                    this.print_expr(&this.body[*expr])
                })
            }
            Expr::CaptureFun { target, arity } => {
                write!(self, "fun ")?;
                self.print_call_target(target, |this, expr| this.print_expr(&this.body[*expr]))?;
                write!(self, "/")?;
                self.print_expr(&self.body[*arity])
            }
            Expr::If { clauses } => {
                self.print_seq(clauses, None, "if", "end", ";", |this, clause| {
                    this.print_guards(&clause.guards, false)?;
                    this.print_clause_body(&clause.exprs)
                })
            }
            Expr::Try {
                exprs,
                of_clauses,
                catch_clauses,
                after,
            } => {
                self.print_seq(exprs, None, "try", "", ",", |this, expr| {
                    this.print_expr(&this.body[*expr])
                })?;
                if !of_clauses.is_empty() {
                    self.print_seq(of_clauses, None, "of", "", ";", |this, clause| {
                        this.print_pat(&this.body[clause.pat])?;
                        this.print_guards(&clause.guards, true)?;
                        this.print_clause_body(&clause.exprs)
                    })?;
                }
                if !catch_clauses.is_empty() {
                    self.print_seq(catch_clauses, None, "catch", "", ";", |this, clause| {
                        if let Some(class) = clause.class {
                            this.print_pat(&this.body[class])?;
                            write!(this, ":")?;
                        }
                        this.print_pat(&this.body[clause.reason])?;
                        if let Some(stack) = clause.stack {
                            write!(this, ":")?;
                            this.print_pat(&this.body[stack])?;
                        }
                        this.print_guards(&clause.guards, true)?;
                        this.print_clause_body(&clause.exprs)
                    })?;
                }
                if !after.is_empty() {
                    self.print_seq(after, None, "after", "", ",", |this, expr| {
                        this.print_expr(&this.body[*expr])
                    })?;
                }
                write!(self, "end")
            }
            Expr::Comprehension { builder, exprs } => {
                let (start, end) = match builder {
                    ComprehensionBuilder::List(_) => ("[", "]"),
                    ComprehensionBuilder::Binary(_) => ("<<", ">>"),
                    ComprehensionBuilder::Map(_, _) => ("#{", "}"),
                };
                writeln!(self, "{}", start)?;
                self.indent_level += 1;
                match builder {
                    ComprehensionBuilder::List(expr) => self.print_expr(&self.body[*expr])?,
                    ComprehensionBuilder::Binary(expr) => self.print_expr(&self.body[*expr])?,
                    ComprehensionBuilder::Map(expr1, expr2) => {
                        self.print_expr(&self.body[*expr1])?;
                        write!(self, " => ")?;
                        self.print_expr(&self.body[*expr2])?
                    }
                }
                self.indent_level -= 1;
                writeln!(self)?;
                self.print_seq(exprs, None, "||", end, ",", |this, expr| match expr {
                    ComprehensionExpr::BinGenerator { pat, expr } => {
                        this.print_pat(&this.body[*pat])?;
                        write!(this, " <= ")?;
                        this.print_expr(&this.body[*expr])
                    }
                    ComprehensionExpr::ListGenerator { pat, expr } => {
                        this.print_pat(&this.body[*pat])?;
                        write!(this, " <- ")?;
                        this.print_expr(&this.body[*expr])
                    }
                    ComprehensionExpr::Expr(expr) => this.print_expr(&this.body[*expr]),
                    ComprehensionExpr::MapGenerator { key, value, expr } => {
                        this.print_pat(&this.body[*key])?;
                        write!(this, " := ")?;
                        this.print_pat(&this.body[*value])?;
                        write!(this, " <- ")?;
                        this.print_expr(&this.body[*expr])
                    }
                })
            }
            Expr::Closure { clauses, name } => {
                let name_str = if let Some(pat_id) = name {
                    print_pat(self.db, self.body, *pat_id)
                } else {
                    "".to_string()
                };
                let name_str = name_str.trim();
                self.print_seq(clauses, None, "fun", "end", ";", |this, clause| {
                    this.print_clause(clause, name_str)
                })
            }
            Expr::Maybe {
                exprs,
                else_clauses,
            } => {
                self.print_seq(exprs, None, "maybe", "", ",", |this, expr| match expr {
                    MaybeExpr::Cond { lhs, rhs } => {
                        this.print_pat(&this.body[*lhs])?;
                        write!(this, " ?= ")?;
                        this.print_expr(&this.body[*rhs])
                    }
                    MaybeExpr::Expr(expr_id) => this.print_expr(&this.body[*expr_id]),
                })?;
                if !else_clauses.is_empty() {
                    self.print_seq(else_clauses, None, "else", "", ";", |this, clause| {
                        this.print_pat(&this.body[clause.pat])?;
                        this.print_guards(&clause.guards, true)?;
                        this.print_clause_body(&clause.exprs)
                    })?;
                }
                write!(self, "end")
            }
            Expr::Paren { expr } => self.print_expr(&self.body[*expr]),
            Expr::SsrPlaceholder(ssr) => self.print_ssr_placeholder(ssr),
        }
    }

    fn print_type(&mut self, ty: &TypeExpr) -> fmt::Result {
        match ty {
            TypeExpr::Missing => write!(self, "[missing]"),
            TypeExpr::Literal(lit) => self.print_literal(lit),
            TypeExpr::Var(var) => write!(self, "{}", self.db.lookup_var(*var)),
            TypeExpr::Tuple { args } => self.print_seq(args, None, "{", "}", ",", |this, ty| {
                this.print_type(&this.body[*ty])
            }),
            TypeExpr::List(list) => {
                write!(self, "[")?;
                match list {
                    ListType::Empty => {}
                    ListType::Regular(ty) => {
                        self.print_type(&self.body[*ty])?;
                    }
                    ListType::NonEmpty(ty) => {
                        self.print_type(&self.body[*ty])?;
                        write!(self, ", ...")?;
                    }
                }
                write!(self, "]")
            }
            TypeExpr::Map { fields } => {
                self.print_seq(fields, None, "#{", "}", ",", |this, (key, op, value)| {
                    this.print_type(&this.body[*key])?;
                    write!(this, " {} ", op)?;
                    this.print_type(&this.body[*value])
                })
            }
            TypeExpr::Record { name, fields } => {
                write!(self, "#{}", self.db.lookup_atom(*name))?;
                self.print_seq(fields, None, "{", "}", ",", |this, (key, val)| {
                    write!(this, "{} :: ", this.db.lookup_atom(*key))?;
                    this.print_type(&this.body[*val])
                })
            }
            TypeExpr::Fun(fun) => {
                write!(self, "fun(")?;
                match fun {
                    FunType::Any => {}
                    FunType::AnyArgs { result } => {
                        write!(self, "(...) -> ")?;
                        self.print_type(&self.body[*result])?;
                    }
                    FunType::Full { params, result } => {
                        self.print_args("", params, |this, ty| this.print_type(&this.body[*ty]))?;
                        write!(self, " -> ")?;
                        self.print_type(&self.body[*result])?;
                    }
                }
                write!(self, ")")
            }
            TypeExpr::UnaryOp { type_expr, op } => {
                write!(self, "({} ", op)?;
                self.print_type(&self.body[*type_expr])?;
                write!(self, ")")
            }
            TypeExpr::AnnType { var, ty } => {
                write!(self, "({} ", self.db.lookup_var(*var))?;
                write!(self, " :: ")?;
                self.print_type(&self.body[*ty])?;
                write!(self, ")")
            }
            TypeExpr::BinaryOp { lhs, rhs, op } => {
                write!(self, "(")?;
                self.print_type(&self.body[*lhs])?;
                write!(self, " {} ", op)?;
                self.print_type(&self.body[*rhs])?;
                write!(self, ")")
            }
            TypeExpr::Range { lhs, rhs } => {
                write!(self, "(")?;
                self.print_type(&self.body[*lhs])?;
                write!(self, "..")?;
                self.print_type(&self.body[*rhs])?;
                write!(self, ")")
            }
            TypeExpr::Union { types } => self.print_seq(types, None, "(", ")", " |", |this, ty| {
                this.print_type(&self.body[*ty])
            }),
            TypeExpr::Call { target, args } => {
                self.print_call_target(target, |this, ty| this.print_type(&this.body[*ty]))?;
                self.print_seq(args, None, "(", ")", ",", |this, ty| {
                    this.print_type(&this.body[*ty])
                })
            }
            TypeExpr::MacroCall {
                expansion,
                args: _,
                macro_def: _,
            } => self.print_type(&self.body[*expansion]),
            TypeExpr::SsrPlaceholder(ssr) => self.print_ssr_placeholder(ssr),
        }
    }

    fn print_term(&mut self, term: &Term) -> fmt::Result {
        match term {
            Term::Missing => write!(self, "[missing]"),
            Term::Literal(lit) => self.print_literal(lit),
            Term::Tuple { exprs } => self.print_seq(exprs, None, "{", "}", ",", |this, term| {
                this.print_term(&this.body[*term])
            }),
            Term::List { exprs, tail } => {
                self.print_seq(exprs, tail.as_ref(), "[", "]", ",", |this, term| {
                    this.print_term(&this.body[*term])
                })
            }
            Term::Map { fields } => {
                self.print_seq(fields, None, "#{", "}", ",", |this, (key, value)| {
                    this.print_term(&this.body[*key])?;
                    write!(this, " => ")?;
                    this.print_term(&this.body[*value])
                })
            }
            Term::Binary(bin) => {
                if let Ok(str) = str::from_utf8(bin) {
                    write!(self, "<<{:?}/utf8>>", str)
                } else {
                    write!(self, "<<")?;
                    let mut sep = "";
                    for byte in bin {
                        write!(self, "{}{}", sep, byte)?;
                        sep = ", ";
                    }
                    write!(self, ">>")
                }
            }
            Term::CaptureFun {
                module,
                name,
                arity,
            } => {
                write!(
                    self,
                    "fun {}:{}/{}",
                    self.db.lookup_atom(*module),
                    self.db.lookup_atom(*name),
                    arity
                )
            }
            Term::MacroCall {
                expansion,
                args: _,
                macro_def: _,
            } => self.print_term(&self.body[*expansion]),
        }
    }

    fn print_call_target<T>(
        &mut self,
        target: &CallTarget<T>,
        print: impl Fn(&mut Self, &T) -> fmt::Result,
    ) -> fmt::Result {
        match target {
            CallTarget::Local { name } => print(self, name),
            CallTarget::Remote { module, name, .. } => {
                print(self, module)?;
                write!(self, ":")?;
                print(self, name)
            }
        }
    }

    fn print_literal(&mut self, lit: &Literal) -> fmt::Result {
        match lit {
            Literal::String(StringVariant::Normal(string)) => write!(self, "{:?}", string),
            Literal::String(StringVariant::Verbatim(string)) => {
                write!(self, "{}", string)
            }
            Literal::Char(char) => write!(self, "${}", char),
            Literal::Atom(atom) => write!(self, "'{}'", self.db.lookup_atom(*atom)),
            Literal::Integer(int) => write!(self, "{}", int),
            Literal::Float(float) => write!(self, "{}", f64::from_bits(*float)),
        }
    }

    fn print_seq<T>(
        &mut self,
        exprs: &[T],
        tail: Option<&T>,
        start: &str,
        end: &str,
        base_sep: &str,
        print: impl Fn(&mut Self, &T) -> fmt::Result,
    ) -> fmt::Result {
        if exprs.is_empty() && tail.is_none() {
            write!(self, "{}{}", start, end)
        } else {
            write!(self, "{}", start)?;
            self.indent_level += 1;
            let mut sep = "";
            for expr in exprs {
                writeln!(self, "{}", sep)?;
                sep = base_sep;
                print(self, expr)?;
            }
            if let Some(tail) = tail {
                write!(self, "\n| ")?;
                print(self, tail)?;
            }
            self.indent_level -= 1;
            write!(self, "\n{}", end)
        }
    }

    fn print_args<T>(
        &mut self,
        name: &str,
        exprs: &[T],
        print: impl Fn(&mut Self, &T) -> fmt::Result,
    ) -> fmt::Result {
        write!(self, "{}(", name)?;
        let mut sep = "";
        for expr in exprs {
            write!(self, "{}", sep)?;
            sep = ", ";
            print(self, expr)?;
        }
        write!(self, ")")
    }

    fn print_bin_segment<T: Copy>(
        &mut self,
        seg: &BinarySeg<T>,
        print: fn(&mut Self, T) -> fmt::Result,
    ) -> fmt::Result {
        print(self, seg.elem)?;
        if let Some(size) = seg.size {
            write!(self, ":")?;
            self.print_expr(&self.body[size])?;
        }
        if !seg.tys.is_empty() || seg.unit.is_some() {
            write!(self, "/")?;
            let mut sep = "";
            for &ty in &seg.tys {
                write!(self, "{}{}", sep, self.db.lookup_atom(ty))?;
                sep = "-";
            }
            if let Some(unit) = seg.unit {
                write!(self, "{}unit:{}", sep, unit)?;
            }
        }
        Ok(())
    }

    fn print_ssr_placeholder(&mut self, ssr: &SsrPlaceholder) -> fmt::Result {
        write!(self, "{}", self.db.lookup_var(ssr.var))
    }
}

impl<'a> fmt::Write for Printer<'a> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for line in s.split_inclusive('\n') {
            if self.needs_indent {
                if !self.buf.ends_with('\n') {
                    self.buf.push('\n');
                }
                for _ in 0..self.indent_level {
                    self.buf.push_str("    ");
                }
                self.needs_indent = false;
            }

            self.buf.push_str(line);
            self.needs_indent = line.ends_with('\n');
        }

        Ok(())
    }
}
