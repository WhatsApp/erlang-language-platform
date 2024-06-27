/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Print a representation of the HIR AST for a given entry point into
//! the Body.

use std::fmt;
use std::fmt::Write as _;
use std::str;

use crate::db::InternDatabase;
use crate::expr::MaybeExpr;
use crate::AnyAttribute;
use crate::AttributeBody;
use crate::BinarySeg;
use crate::Body;
use crate::CRClause;
use crate::CallTarget;
use crate::CatchClause;
use crate::Clause;
use crate::ComprehensionBuilder;
use crate::ComprehensionExpr;
use crate::Expr;
use crate::ExprId;
use crate::FunType;
use crate::FunctionBody;
use crate::FunctionClauseBody;
use crate::ListType;
use crate::Literal;
use crate::Pat;
use crate::PatId;
use crate::Term;
use crate::TermId;
use crate::TypeAlias;
use crate::TypeExpr;
use crate::TypeExprId;

pub(crate) fn print_expr(db: &dyn InternDatabase, body: &Body, expr: ExprId) -> String {
    let mut printer = Printer::new(db, body);
    printer.print_expr(&body[expr]);
    printer.to_string()
}

pub(crate) fn print_pat(db: &dyn InternDatabase, body: &Body, pat: PatId) -> String {
    let mut printer = Printer::new(db, body);
    printer.print_pat(&body[pat]);
    printer.to_string()
}

pub(crate) fn print_type(db: &dyn InternDatabase, body: &Body, ty: TypeExprId) -> String {
    let mut printer = Printer::new(db, body);
    printer.print_type(&body[ty]);
    printer.to_string()
}

pub(crate) fn print_term(db: &dyn InternDatabase, body: &Body, term: TermId) -> String {
    let mut printer = Printer::new(db, body);
    printer.print_term(&body[term]);
    printer.to_string()
}

pub(crate) fn print_attribute(
    db: &dyn InternDatabase,
    body: &AttributeBody,
    form: &AnyAttribute,
) -> String {
    let mut printer = Printer::new(db, &body.body);

    match form {
        AnyAttribute::CompileOption(_) => writeln!(printer, "-compile(").unwrap(),
        AnyAttribute::Attribute(attr) => writeln!(printer, "-{}(", attr.name).unwrap(),
    }

    printer.indent();
    printer.print_term(&printer.body[body.value]);
    writeln!(printer).ok();
    printer.dedent();
    write!(printer, ").").ok();

    printer.to_string()
}

pub(crate) fn print_function(db: &dyn InternDatabase, body: &FunctionBody) -> String {
    let mut out = String::new();

    let mut sep = "";
    for (_idx, clause) in body.clauses.iter() {
        write!(out, "{}", sep).unwrap();
        sep = ";";
        let mut printer = Printer::new(db, &clause.body);
        printer.print_clause(&clause.clause);
        write!(out, "{}", printer.to_string_raw()).unwrap();
    }
    write!(out, ".\n").unwrap();

    out
}

pub(crate) fn print_function_clause(
    db: &dyn InternDatabase,
    clause: &FunctionClauseBody,
) -> String {
    let mut out = String::new();

    let mut printer = Printer::new(db, &clause.body);
    printer.print_clause(&clause.clause);
    write!(out, "{}", printer.to_string_raw()).unwrap();
    write!(out, "\n").unwrap();

    out
}

pub(crate) fn print_type_alias(
    db: &dyn InternDatabase,
    body: &crate::TypeBody,
    form: &crate::TypeAlias,
) -> String {
    let mut printer = Printer::new(db, &body.body);

    match form {
        TypeAlias::Regular { .. } => write!(printer, "-type ").unwrap(),
        TypeAlias::Opaque { .. } => write!(printer, "-opaque ").unwrap(),
    }

    printer.print_args(form.name().name(), &body.vars, |this, &var| {
        write!(this, "{}", db.lookup_var(var)).ok();
    });
    write!(printer, " :: ").ok();
    printer.print_type(&printer.body[body.ty]);
    write!(printer, ".").ok();

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

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        self.indent_level -= 1;
    }

    fn print_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Missing => {
                write!(self, "Expr::Missing").ok();
            }
            Expr::Literal(lit) => {
                write!(self, "Literal(").ok();
                self.print_literal(lit);
                write!(self, ")").ok();
            }
            Expr::Var(var) => {
                write!(self, "Expr::Var({})", self.db.lookup_var(*var)).ok();
            }
            Expr::Match { lhs, rhs } => {
                self.print_herald("Expr::Match", &mut |this| {
                    this.print_labelled("lhs", true, &mut |this| {
                        this.print_pat(&this.body[*lhs]);
                    });
                    this.print_labelled("rhs", true, &mut |this| {
                        this.print_expr(&this.body[*rhs]);
                    });
                });
            }
            Expr::Tuple { exprs } => {
                self.print_herald("Expr::Tuple", &mut |this| {
                    exprs.iter().for_each(|expr_id| {
                        this.print_expr(&this.body[*expr_id]);
                        writeln!(this, ",").ok();
                    });
                });
            }
            Expr::List { exprs, tail } => {
                self.print_herald("Expr::List", &mut |this| {
                    this.print_labelled("exprs", false, &mut |this| {
                        exprs.iter().for_each(|expr_id| {
                            this.print_expr(&this.body[*expr_id]);
                            writeln!(this, ",").ok();
                        });
                    });

                    this.print_labelled("tail", false, &mut |this| {
                        if let Some(expr_id) = tail {
                            this.print_expr(&this.body[*expr_id]);
                            writeln!(this, ",").ok();
                        }
                    });
                });
            }
            Expr::Binary { segs } => {
                self.print_herald("Expr::Binary", &mut |this| {
                    segs.iter().for_each(|seg| {
                        this.print_bin_segment(seg, |this, expr| {
                            this.print_expr(&this.body[expr]);
                        });
                        writeln!(this).ok();
                    });
                });
            }
            Expr::UnaryOp { expr, op } => {
                self.print_herald("Expr::UnaryOp", &mut |this| {
                    this.print_expr(&this.body[*expr]);
                    writeln!(this).ok();
                    writeln!(this, "{:?},", op).ok();
                });
            }
            Expr::BinaryOp { lhs, rhs, op } => {
                self.print_herald("Expr::BinaryOp", &mut |this| {
                    this.print_labelled("lhs", true, &mut |this| this.print_expr(&this.body[*lhs]));
                    this.print_labelled("rhs", true, &mut |this| this.print_expr(&this.body[*rhs]));
                    this.print_labelled("op", true, &mut |this| {
                        write!(this, "{:?},", op).ok();
                    });
                });
            }
            Expr::Record { name, fields } => {
                self.print_herald("Expr::Record", &mut |this| {
                    writeln!(this, "name: Atom('{}')", this.db.lookup_atom(*name)).ok();
                    this.print_labelled("fields", false, &mut |this| {
                        fields.iter().for_each(|(name, expr_id)| {
                            writeln!(this, "Atom('{}'):", this.db.lookup_atom(*name)).ok();
                            this.indent();
                            this.print_expr(&this.body[*expr_id]);
                            writeln!(this, ",").ok();
                            this.dedent();
                        });
                    });
                });
            }
            Expr::RecordUpdate { expr, name, fields } => {
                self.print_herald("Expr::RecordUpdate", &mut |this| {
                    this.print_labelled("expr", true, &mut |this| {
                        this.print_expr(&this.body[*expr])
                    });
                    writeln!(this, "name: Atom('{}')", this.db.lookup_atom(*name)).ok();
                    this.print_labelled("fields", false, &mut |this| {
                        fields.iter().for_each(|(name, expr_id)| {
                            writeln!(this, "Atom('{}'):", this.db.lookup_atom(*name)).ok();
                            this.indent();
                            this.print_expr(&this.body[*expr_id]);
                            writeln!(this, ",").ok();
                            this.dedent();
                        });
                    });
                });
            }
            Expr::RecordIndex { name, field } => {
                self.print_herald("Expr::RecordIndex", &mut |this| {
                    writeln!(this, "name: Atom('{}')", this.db.lookup_atom(*name)).ok();
                    writeln!(this, "field: Atom('{}')", this.db.lookup_atom(*field)).ok();
                });
            }
            Expr::RecordField { expr, name, field } => {
                self.print_herald("Expr::RecordField", &mut |this| {
                    this.print_labelled("expr", true, &mut |this| {
                        this.print_expr(&this.body[*expr])
                    });
                    writeln!(this, "name: Atom('{}')", this.db.lookup_atom(*name)).ok();
                    writeln!(this, "field: Atom('{}')", this.db.lookup_atom(*field)).ok();
                });
            }
            Expr::Map { fields } => {
                self.print_herald("Expr::Map", &mut |this| {
                    fields.iter().for_each(|(name, value)| {
                        writeln!(this, "{{").ok();
                        this.indent();
                        this.print_expr(&this.body[*name]);
                        writeln!(this, ",").ok();
                        this.print_expr(&this.body[*value]);
                        writeln!(this, ",").ok();
                        this.dedent();
                        writeln!(this, "}},").ok();
                    });
                });
            }
            Expr::MapUpdate { expr, fields } => {
                self.print_herald("Expr::MapUpdate", &mut |this| {
                    this.print_labelled("expr", true, &mut |this| {
                        this.print_expr(&this.body[*expr])
                    });
                    this.print_labelled("fields", false, &mut |this| {
                        fields.iter().for_each(|(name, op, value)| {
                            this.print_expr(&this.body[*name]);
                            this.indent();
                            writeln!(this).ok();
                            writeln!(this, "{:?}", op).ok();
                            this.print_expr(&this.body[*value]);
                            writeln!(this, ",").ok();
                            this.dedent();
                        });
                    });
                });
            }
            Expr::Catch { expr } => {
                self.print_herald("Expr::Catch", &mut |this| {
                    this.print_labelled("expr", true, &mut |this| {
                        this.print_expr(&this.body[*expr])
                    });
                });
            }
            Expr::MacroCall {
                expansion,
                args,
                macro_def: _,
            } => {
                self.print_herald("Expr::MacroCall", &mut |this| {
                    this.print_labelled("args", false, &mut |this| this.print_exprs(args));
                    this.print_labelled("expansion", true, &mut |this| {
                        this.print_expr(&this.body[*expansion])
                    });
                });
            }
            Expr::Call { target, args } => {
                self.print_herald("Expr::Call", &mut |this| {
                    this.print_labelled("target", true, &mut |this1| {
                        this1.print_call_target(target, |this, expr| {
                            this.print_expr(&this.body[*expr])
                        });
                    });
                    this.print_labelled("args", false, &mut |this| {
                        args.iter().for_each(|expr_id| {
                            this.print_expr(&this.body[*expr_id]);
                            writeln!(this, ",").ok();
                        });
                    });
                });
            }
            Expr::Comprehension { builder, exprs } => {
                self.print_herald("Expr::Comprehension", &mut |this| {
                    this.print_labelled("builder", true, &mut |this| {
                        match builder {
                            ComprehensionBuilder::List(expr) => {
                                this.print_herald("ComprehensionBuilder::List", &mut |this| {
                                    this.print_expr(&this.body[*expr]);
                                    writeln!(this).ok();
                                });
                            }
                            ComprehensionBuilder::Binary(expr) => {
                                this.print_herald("ComprehensionBuilder::Binary", &mut |this| {
                                    this.print_expr(&this.body[*expr]);
                                    writeln!(this).ok();
                                });
                            }
                            ComprehensionBuilder::Map(expr1, expr2) => {
                                this.print_herald("ComprehensionBuilder::Map", &mut |this| {
                                    this.print_expr(&this.body[*expr1]);
                                    writeln!(this).ok();
                                    writeln!(this, "=>").ok();
                                    this.print_expr(&this.body[*expr2]);
                                    writeln!(this).ok();
                                });
                            }
                        };
                    });

                    this.print_labelled("exprs", false, &mut |this| {
                        exprs.iter().for_each(|expr| {
                            match expr {
                                ComprehensionExpr::BinGenerator { pat, expr } => {
                                    this.print_herald(
                                        "ComprehensionExpr::BinGenerator",
                                        &mut |this| {
                                            this.print_pat(&this.body[*pat]);
                                            writeln!(this).ok();
                                            this.print_expr(&this.body[*expr]);
                                            writeln!(this).ok();
                                        },
                                    );
                                }
                                ComprehensionExpr::ListGenerator { pat, expr } => {
                                    this.print_herald(
                                        "ComprehensionExpr::ListGenerator",
                                        &mut |this| {
                                            this.print_pat(&this.body[*pat]);
                                            writeln!(this).ok();
                                            this.print_expr(&this.body[*expr]);
                                            writeln!(this).ok();
                                        },
                                    );
                                }
                                ComprehensionExpr::Expr(expr) => {
                                    this.print_herald("ComprehensionExpr::Expr", &mut |this| {
                                        this.print_expr(&this.body[*expr]);
                                        writeln!(this).ok();
                                    });
                                }
                                ComprehensionExpr::MapGenerator { key, value, expr } => {
                                    this.print_herald(
                                        "ComprehensionExpr::MapGenerator",
                                        &mut |this| {
                                            this.print_pat(&this.body[*key]);
                                            writeln!(this, " :=").ok();
                                            this.print_pat(&this.body[*value]);
                                            writeln!(this, " <-").ok();
                                            this.print_expr(&this.body[*expr]);
                                            writeln!(this).ok();
                                        },
                                    );
                                }
                            };
                            writeln!(this, ",").ok();
                        });
                    });
                });
            }
            Expr::Block { exprs } => {
                self.print_herald("Expr::Block", &mut |this| {
                    exprs.iter().for_each(|expr_id| {
                        this.print_expr(&this.body[*expr_id]);
                        writeln!(this, ",").ok();
                    });
                });
            }
            Expr::If { clauses } => {
                self.print_herald("Expr::If", &mut |this| {
                    clauses.iter().for_each(|clause| {
                        this.print_herald("IfClause", &mut |this| {
                            this.print_labelled("guards", false, &mut |this| {
                                this.print_guards(&clause.guards)
                            });
                            this.print_labelled("exprs", false, &mut |this| {
                                this.print_exprs(&clause.exprs)
                            });
                        });
                        writeln!(this).ok();
                    });
                });
            }
            Expr::Case { expr, clauses } => {
                self.print_herald("Expr::Case", &mut |this| {
                    this.print_labelled("expr", true, &mut |this| {
                        this.print_expr(&this.body[*expr])
                    });
                    this.print_labelled("clauses", false, &mut |this| {
                        clauses.iter().for_each(|clause| {
                            this.print_cr_clause(clause);
                        });
                    });
                });
            }
            Expr::Receive { clauses, after } => {
                self.print_herald("Expr::Receive", &mut |this| {
                    this.print_labelled("clauses", false, &mut |this| {
                        clauses.iter().for_each(|clause| {
                            this.print_cr_clause(clause);
                        });
                    });

                    this.print_labelled("after", false, &mut |this| {
                        if let Some(after) = after {
                            this.print_herald("ReceiveAfter", &mut |this| {
                                this.print_labelled("timeout", true, &mut |this| {
                                    this.print_expr(&this.body[after.timeout])
                                });

                                this.print_labelled("exprs", false, &mut |this| {
                                    after.exprs.iter().for_each(|expr_id| {
                                        this.print_expr(&this.body[*expr_id]);
                                        writeln!(this, ",").ok();
                                    });
                                });
                            });
                        }
                    });
                });
            }
            Expr::Try {
                exprs,
                of_clauses,
                catch_clauses,
                after,
            } => {
                self.print_herald("Expr::Try", &mut |this| {
                    this.print_labelled("exprs", false, &mut |this| {
                        exprs.iter().for_each(|expr_id| {
                            this.print_expr(&this.body[*expr_id]);
                            writeln!(this, ",").ok();
                        });
                    });

                    this.print_labelled("of_clauses", false, &mut |this| {
                        of_clauses.iter().for_each(|clause| {
                            this.print_cr_clause(clause);
                        });
                    });
                    this.print_labelled("catch_clauses", false, &mut |this| {
                        catch_clauses.iter().for_each(|clause| {
                            this.print_catch_clause(clause);
                            writeln!(this, ",").ok();
                        });
                    });

                    this.print_labelled("after", false, &mut |this| {
                        after.iter().for_each(|expr_id| {
                            this.print_expr(&this.body[*expr_id]);
                            writeln!(this, ",").ok();
                        });
                    });
                });
            }
            Expr::CaptureFun { target, arity } => {
                self.print_herald("Expr::CaptureFun", &mut |this| {
                    this.print_labelled("target", true, &mut |this1| {
                        this1.print_call_target(target, |this, expr| {
                            this.print_expr(&this.body[*expr])
                        });
                    });

                    this.print_labelled("arity", true, &mut |this| {
                        this.print_expr(&this.body[*arity])
                    });
                });
            }
            Expr::Closure { clauses, name } => {
                self.print_herald("Expr::Closure", &mut |this| {
                    this.print_labelled("clauses", false, &mut |this| {
                        clauses.iter().for_each(|clause| {
                            this.print_clause(clause);
                            writeln!(this, ",").ok();
                        });
                    });
                    this.print_labelled("name", false, &mut |this| {
                        if let Some(name) = name {
                            this.print_pat(&this.body[*name]);
                            writeln!(this).ok();
                        }
                    });
                });
            }
            Expr::Maybe {
                exprs,
                else_clauses,
            } => {
                self.print_herald("Expr::Maybe", &mut |this| {
                    this.print_labelled("exprs", false, &mut |this| {
                        exprs.iter().for_each(|expr| {
                            this.print_maybe_expr(expr);
                            writeln!(this, ",").ok();
                        });
                    });
                    this.print_labelled("else_clauses", false, &mut |this| {
                        else_clauses.iter().for_each(|clause| {
                            this.print_cr_clause(clause);
                        });
                    });
                });
            }
        }
    }

    fn print_pat(&mut self, pat: &Pat) {
        match pat {
            Pat::Missing => {
                write!(self, "Pat::Missing").ok();
            }
            Pat::Literal(lit) => {
                write!(self, "Literal(").ok();
                self.print_literal(lit);
                write!(self, ")").ok();
            }
            Pat::Var(var) => {
                write!(self, "Pat::Var({})", self.db.lookup_var(*var)).ok();
            }
            Pat::Match { lhs, rhs } => {
                self.print_herald("Pat::Match", &mut |this| {
                    this.print_labelled("lhs", true, &mut |this| this.print_pat(&this.body[*lhs]));
                    this.print_labelled("rhs", true, &mut |this| this.print_pat(&this.body[*rhs]));
                });
            }
            Pat::Tuple { pats } => {
                self.print_herald("Pat::Tuple", &mut |this| {
                    pats.iter().for_each(|pat_id| {
                        this.print_pat(&this.body[*pat_id]);
                        writeln!(this, ",").ok();
                    });
                });
            }
            Pat::List { pats, tail } => {
                self.print_herald("Pat::List", &mut |this| {
                    this.print_labelled("exprs", false, &mut |this| {
                        pats.iter().for_each(|pat_id| {
                            this.print_pat(&this.body[*pat_id]);
                            writeln!(this, ",").ok();
                        });
                    });

                    this.print_labelled("tail", false, &mut |this| {
                        if let Some(pat_id) = tail {
                            this.print_pat(&this.body[*pat_id]);
                            writeln!(this, ",").ok();
                        }
                    });
                });
            }
            Pat::Binary { segs } => {
                self.print_herald("Pat::Binary", &mut |this| {
                    segs.iter().for_each(|seg| {
                        this.print_bin_segment(seg, |this, pat| this.print_pat(&this.body[pat]));
                        writeln!(this).ok();
                    });
                });
            }
            Pat::UnaryOp { pat, op } => {
                self.print_herald("Pat::UnaryOp", &mut |this| {
                    this.print_pat(&this.body[*pat]);
                    writeln!(this).ok();
                    writeln!(this, "{:?},", op).ok();
                });
            }
            Pat::BinaryOp { lhs, rhs, op } => {
                self.print_herald("Pat::BinaryOp", &mut |this| {
                    this.print_labelled("lhs", true, &mut |this| this.print_pat(&this.body[*lhs]));
                    this.print_labelled("rhs", true, &mut |this| this.print_pat(&this.body[*rhs]));
                    this.print_labelled("op", true, &mut |this| {
                        write!(this, "{:?},", op).ok();
                    });
                });
            }
            Pat::Record { name, fields } => {
                self.print_herald("Pat::Record", &mut |this| {
                    writeln!(this, "name: Atom('{}')", this.db.lookup_atom(*name)).ok();
                    this.print_labelled("fields", false, &mut |this| {
                        fields.iter().for_each(|(name, pat_id)| {
                            writeln!(this, "Atom('{}'):", this.db.lookup_atom(*name)).ok();
                            this.indent();
                            this.print_pat(&this.body[*pat_id]);
                            writeln!(this, ",").ok();
                            this.dedent();
                        });
                    });
                });
            }
            Pat::RecordIndex { name, field } => {
                self.print_herald("Pat::RecordIndex", &mut |this| {
                    writeln!(this, "name: Atom('{}')", this.db.lookup_atom(*name)).ok();
                    writeln!(this, "field: Atom('{}')", this.db.lookup_atom(*field)).ok();
                });
            }
            Pat::Map { fields } => {
                self.print_herald("Pat::Map", &mut |this| {
                    fields.iter().for_each(|(name, value)| {
                        writeln!(this, "{{").ok();
                        this.indent();
                        this.print_expr(&this.body[*name]);
                        writeln!(this, ",").ok();
                        this.print_pat(&this.body[*value]);
                        writeln!(this, ",").ok();
                        this.dedent();
                        writeln!(this, "}},").ok();
                    });
                });
            }
            Pat::MacroCall {
                expansion,
                args,
                macro_def: _,
            } => {
                self.print_herald("Pat::MacroCall", &mut |this| {
                    this.print_labelled("args", false, &mut |this| this.print_exprs(args));
                    this.print_labelled("expansion", true, &mut |this| {
                        this.print_pat(&this.body[*expansion])
                    });
                });
            }
        }
    }

    fn print_term(&mut self, term: &Term) {
        match term {
            Term::Missing => {
                write!(self, "Term::Missing").ok();
            }
            Term::Literal(lit) => {
                write!(self, "Literal(").ok();
                self.print_literal(lit);
                write!(self, ")").ok();
            }
            Term::Binary(bin) => {
                self.print_herald("Term::Binary", &mut |this| {
                    if let Ok(str) = str::from_utf8(bin) {
                        writeln!(this, "<<{:?}/utf8>>", str).ok();
                    } else {
                        write!(this, "<<").ok();
                        let mut sep = "";
                        for byte in bin {
                            write!(this, "{}{}", sep, byte).ok();
                            sep = ", ";
                        }
                        writeln!(this, ">>").ok();
                    }
                });
            }
            Term::Tuple { exprs } => {
                self.print_herald("Term::Tuple", &mut |this| {
                    exprs.iter().for_each(|term_id| {
                        this.print_term(&this.body[*term_id]);
                        writeln!(this, ",").ok();
                    });
                });
            }
            Term::List { exprs, tail } => {
                self.print_herald("Term::List", &mut |this| {
                    this.print_labelled("exprs", false, &mut |this| {
                        exprs.iter().for_each(|term_id| {
                            this.print_term(&this.body[*term_id]);
                            writeln!(this, ",").ok();
                        });
                    });

                    this.print_labelled("tail", false, &mut |this| {
                        if let Some(term_id) = tail {
                            this.print_term(&this.body[*term_id]);
                            writeln!(this, ",").ok();
                        }
                    });
                });
            }
            Term::Map { fields } => {
                self.print_herald("Term::Map", &mut |this| {
                    fields.iter().for_each(|(name, value)| {
                        writeln!(this, "{{").ok();
                        this.indent();
                        this.print_term(&this.body[*name]);
                        write!(this, " => ").ok();
                        this.print_term(&this.body[*value]);
                        writeln!(this, ",").ok();
                        this.dedent();
                        writeln!(this, "}},").ok();
                    });
                });
            }
            Term::CaptureFun {
                module,
                name,
                arity,
            } => {
                self.print_herald("Term::CaptureFun", &mut |this| {
                    write!(
                        this,
                        "fun {}:{}/{}",
                        this.db.lookup_atom(*module),
                        this.db.lookup_atom(*name),
                        arity
                    )
                    .ok();
                });
            }
            Term::MacroCall {
                expansion: _,
                args: _,
                macro_def: _,
            } => todo!(),
        }
    }

    fn print_type(&mut self, ty: &TypeExpr) {
        match ty {
            TypeExpr::AnnType { var, ty } => {
                self.print_herald("TypeExpr::AnnType", &mut |this| {
                    this.print_labelled("var", true, &mut |this| {
                        write!(this, "{}", this.db.lookup_var(*var)).ok();
                    });
                    this.print_labelled("ty", true, &mut |this| this.print_type(&this.body[*ty]));
                });
            }
            TypeExpr::BinaryOp { lhs, rhs, op } => {
                self.print_herald("TypeExpr::BinaryOp", &mut |this| {
                    this.print_labelled("lhs", true, &mut |this| this.print_type(&this.body[*lhs]));
                    this.print_labelled("rhs", true, &mut |this| this.print_type(&this.body[*rhs]));
                    this.print_labelled("op", true, &mut |this| {
                        write!(this, "{:?},", op).ok();
                    });
                });
            }
            TypeExpr::Call { target, args } => {
                self.print_herald("TypeExpr::Call", &mut |this| {
                    this.print_labelled("target", true, &mut |this1| {
                        this1
                            .print_call_target(target, |this, ty| this.print_type(&this.body[*ty]));
                    });
                    this.print_labelled("args", false, &mut |this| {
                        args.iter().for_each(|ty| {
                            this.print_type(&this.body[*ty]);
                            writeln!(this, ",").ok();
                        });
                    });
                });
            }
            TypeExpr::Fun(fun) => {
                self.print_herald("TypeExpr::Fun", &mut |this| match fun {
                    FunType::Any => {
                        writeln!(this, "FunType::Any").ok();
                    }
                    FunType::AnyArgs { result } => {
                        this.print_herald("FunType::AnyArgs", &mut |this| {
                            this.print_labelled("result", true, &mut |this| {
                                this.print_type(&this.body[*result]);
                            });
                        });
                    }
                    FunType::Full { params, result } => {
                        this.print_herald("FunType::Full", &mut |this| {
                            this.print_labelled("params", false, &mut |this| {
                                params.iter().for_each(|ty| {
                                    this.print_type(&this.body[*ty]);
                                    writeln!(this, ",").ok();
                                });
                            });
                            this.print_labelled("result", true, &mut |this| {
                                this.print_type(&this.body[*result]);
                            });
                        });
                    }
                });
            }
            TypeExpr::List(list) => {
                self.print_herald("TypeExpr::Fun", &mut |this| match list {
                    ListType::Empty => {
                        writeln!(this, "ListType::Empty").ok();
                    }
                    ListType::Regular(ty) => {
                        this.print_herald("ListType::Regular", &mut |this| {
                            this.print_type(&this.body[*ty]);
                        });
                    }
                    ListType::NonEmpty(ty) => {
                        this.print_herald("ListType::NonEmpty", &mut |this| {
                            this.print_type(&this.body[*ty]);
                        });
                    }
                });
            }
            TypeExpr::Literal(lit) => {
                write!(self, "Literal(").ok();
                self.print_literal(lit);
                write!(self, ")").ok();
            }
            TypeExpr::Map { fields } => {
                self.print_herald("TypeExpr::Map", &mut |this| {
                    fields.iter().for_each(|(name, op, value)| {
                        this.print_type(&this.body[*name]);
                        this.indent();
                        writeln!(this).ok();
                        writeln!(this, "{:?}", op).ok();
                        this.print_type(&this.body[*value]);
                        writeln!(this, ",").ok();
                        this.dedent();
                    });
                });
            }
            TypeExpr::Missing => {
                write!(self, "TypeExpr::Missing").ok();
            }
            TypeExpr::Union { types } => {
                self.print_herald("TypeExpr::Union", &mut |this| {
                    types.iter().for_each(|ty| {
                        this.print_type(&this.body[*ty]);
                        writeln!(this, ",").ok();
                    });
                });
            }
            TypeExpr::Range { lhs, rhs } => {
                self.print_herald("TypeExpr::Range", &mut |this| {
                    this.print_labelled("lhs", true, &mut |this| {
                        this.print_type(&this.body[*lhs]);
                    });
                    this.print_labelled("rhs", true, &mut |this| {
                        this.print_type(&this.body[*rhs]);
                    });
                });
            }
            TypeExpr::Record { name, fields } => {
                self.print_herald("TypeExpr::Record", &mut |this| {
                    writeln!(this, "name: Atom('{}')", this.db.lookup_atom(*name)).ok();
                    this.print_labelled("fields", false, &mut |this| {
                        fields.iter().for_each(|(name, ty)| {
                            writeln!(this, "Atom('{}'):", this.db.lookup_atom(*name)).ok();
                            this.indent();
                            this.print_type(&this.body[*ty]);
                            writeln!(this, ",").ok();
                            this.dedent();
                        });
                    });
                });
            }
            TypeExpr::Tuple { args } => {
                self.print_herald("TypeExpr::Tuple", &mut |this| {
                    args.iter().for_each(|ty| {
                        this.print_type(&this.body[*ty]);
                        writeln!(this, ",").ok();
                    });
                });
            }
            TypeExpr::UnaryOp { type_expr, op } => {
                self.print_herald("TypeExpr::UnaryOp", &mut |this| {
                    this.print_type(&this.body[*type_expr]);
                    writeln!(this).ok();
                    writeln!(this, "{:?},", op).ok();
                });
            }
            TypeExpr::Var(var) => {
                write!(self, "TypeExpr::Var({})", self.db.lookup_var(*var)).ok();
            }
            TypeExpr::MacroCall {
                expansion: _,
                args: _,
                macro_def: _,
            } => todo!(),
        }
    }

    fn print_literal(&mut self, lit: &Literal) {
        match lit {
            Literal::String(string) => write!(self, "String({:?})", string),
            Literal::Char(char) => write!(self, "Char(${})", char),
            Literal::Atom(atom) => write!(self, "Atom('{}')", self.db.lookup_atom(*atom)),
            Literal::Integer(int) => write!(self, "Integer({})", int),
            Literal::Float(float) => write!(self, "Float({})", f64::from_bits(*float)),
        }
        .ok();
    }

    fn print_clause(&mut self, clause: &Clause) {
        self.print_herald("Clause", &mut |this| {
            this.print_labelled("pats", false, &mut |this| {
                for pat in clause.pats.iter().map(|pat_id| &this.body[*pat_id]) {
                    this.print_pat(pat);
                    writeln!(this, ",").ok();
                }
            });
            this.print_labelled("guards", false, &mut |this| {
                this.print_guards(&clause.guards);
            });
            this.print_labelled("exprs", false, &mut |this| {
                this.print_exprs(&clause.exprs);
            });
        });
    }

    fn print_cr_clause(&mut self, clause: &CRClause) {
        self.print_herald("CRClause", &mut |this| {
            this.print_labelled("pat", true, &mut |this| {
                this.print_pat(&this.body[clause.pat]);
            });
            this.print_labelled("guards", false, &mut |this| {
                this.print_guards(&clause.guards);
            });
            this.print_labelled("exprs", false, &mut |this| {
                this.print_exprs(&clause.exprs);
            });
        });
        writeln!(self).ok();
    }

    fn print_catch_clause(&mut self, clause: &CatchClause) {
        self.print_herald("CatchClause", &mut |this| {
            this.print_labelled("class", false, &mut |this| {
                if let Some(class) = clause.class {
                    this.print_pat(&this.body[class]);
                    writeln!(this).ok();
                }
            });
            this.print_labelled("reason", true, &mut |this| {
                this.print_pat(&this.body[clause.reason]);
            });
            this.print_labelled("stack", false, &mut |this| {
                if let Some(stack) = clause.stack {
                    this.print_pat(&this.body[stack]);
                    writeln!(this).ok();
                }
            });
            this.print_labelled("guards", false, &mut |this| {
                this.print_guards(&clause.guards);
            });
            this.print_labelled("exprs", false, &mut |this| {
                this.print_exprs(&clause.exprs);
            });
        });
    }

    fn print_guards(&mut self, guards: &[Vec<ExprId>]) {
        if !guards.is_empty() {
            for guard_clause in guards {
                self.print_labelled("guard", false, &mut |this| {
                    for expr in guard_clause {
                        this.print_expr(&this.body[*expr]);
                        writeln!(this, ",").ok();
                    }
                });
            }
        }
    }

    fn print_exprs(&mut self, exprs: &[ExprId]) {
        for expr_id in exprs {
            self.print_expr(&self.body[*expr_id]);
            writeln!(self, ",").ok();
        }
    }

    fn print_herald(&mut self, label: &str, print: &mut dyn FnMut(&mut Self)) {
        writeln!(self, "{label} {{").ok();
        self.indent();
        print(self);
        self.dedent();
        write!(self, "}}").ok();
    }

    fn print_herald_parens(&mut self, label: &str, print: &mut dyn FnMut(&mut Self)) {
        writeln!(self, "{label}(").ok();
        self.indent();
        print(self);
        self.dedent();
        write!(self, ")").ok();
    }

    fn print_labelled(
        &mut self,
        label: &str,
        final_newline: bool,
        print: &mut dyn FnMut(&mut Self),
    ) {
        writeln!(self, "{}", label).ok();
        self.indent();
        print(self);
        if final_newline {
            writeln!(self).ok();
        }
        self.dedent();
    }

    fn print_bin_segment<T: Copy>(&mut self, seg: &BinarySeg<T>, print: fn(&mut Self, T)) {
        self.print_herald("BinarySeg", &mut |this| {
            writeln!(this, "elem").ok();
            this.indent();
            print(this, seg.elem);
            writeln!(this).ok();
            this.dedent();
            writeln!(this, "size").ok();
            this.indent();
            if let Some(size) = seg.size {
                writeln!(this, "Some(").ok();
                this.print_expr(&this.body[size]);
                writeln!(this, ")").ok();
            } else {
                writeln!(this, "None").ok();
            }
            this.dedent();
            writeln!(this, "tys").ok();
            this.indent();
            for &ty in &seg.tys {
                writeln!(this, "{},", this.db.lookup_atom(ty)).ok();
            }
            this.dedent();
            writeln!(this, "unit").ok();
            this.indent();
            if let Some(unit) = seg.unit {
                writeln!(this, "{}", unit).ok();
            };
            this.dedent();
        });
    }

    fn print_call_target<T>(&mut self, target: &CallTarget<T>, print: impl Fn(&mut Self, &T)) {
        match target {
            CallTarget::Local { name } => {
                self.print_herald("CallTarget::Local", &mut |this| {
                    print(this, name);
                    writeln!(this).ok();
                });
            }
            CallTarget::Remote { module, name } => {
                self.print_herald("CallTarget::Remote", &mut |this| {
                    print(this, module);
                    writeln!(this).ok();
                    print(this, name);
                    writeln!(this).ok();
                });
            }
        }
    }

    fn print_maybe_expr(&mut self, expr: &MaybeExpr) {
        match expr {
            MaybeExpr::Cond { lhs, rhs } => {
                self.print_herald("MaybeExpr::Cond", &mut |this| {
                    this.print_labelled("lhs", true, &mut |this| this.print_pat(&this.body[*lhs]));
                    this.print_labelled("rhs", true, &mut |this| this.print_expr(&this.body[*rhs]));
                });
            }
            MaybeExpr::Expr(expr) => {
                self.print_herald_parens("MaybeExpr::Expr", &mut |this| {
                    this.print_expr(&this.body[*expr]);
                    writeln!(this).ok();
                });
            }
        }
    }

    fn print_args<T>(&mut self, name: &str, exprs: &[T], print: impl Fn(&mut Self, &T)) {
        write!(self, "{}(", name).ok();
        let mut sep = "";
        for expr in exprs {
            write!(self, "{}", sep).ok();
            sep = ", ";
            print(self, expr);
        }
        write!(self, ")").ok();
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

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use expect_test::expect;
    use expect_test::Expect;

    use crate::db::DefDatabase;
    use crate::test_db::TestDB;
    use crate::AnyAttribute;
    use crate::FormIdx;
    use crate::FunctionDefId;
    use crate::InFile;
    use crate::SpecOrCallback;

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, file_id) = TestDB::with_single_file(fixture);
        let form_list = db.file_form_list(file_id);
        let pretty = form_list
            .forms()
            .iter()
            .flat_map(|&form_idx| -> Option<String> {
                match form_idx {
                    FormIdx::FunctionClause(function_id) => {
                        let body =
                            db.function_body(InFile::new(file_id, FunctionDefId::new(function_id)));
                        Some(body.tree_print(&db))
                    }
                    FormIdx::TypeAlias(type_alias_id) => {
                        let type_alias = &form_list[type_alias_id];
                        let body = db.type_body(InFile::new(file_id, type_alias_id));
                        Some(body.tree_print(&db, type_alias))
                    }
                    FormIdx::Spec(spec_id) => {
                        let spec = SpecOrCallback::Spec(form_list[spec_id].clone());
                        let body = db.spec_body(InFile::new(file_id, spec_id));
                        Some(body.print(&db, spec))
                    }
                    FormIdx::Callback(callback_id) => {
                        let spec = SpecOrCallback::Callback(form_list[callback_id].clone());
                        let body = db.callback_body(InFile::new(file_id, callback_id));
                        Some(body.print(&db, spec))
                    }
                    FormIdx::Record(record_id) => {
                        let body = db.record_body(InFile::new(file_id, record_id));
                        Some(body.print(&db, &form_list, record_id))
                    }
                    FormIdx::Attribute(attribute_id) => {
                        let attribute = AnyAttribute::Attribute(form_list[attribute_id].clone());
                        let body = db.attribute_body(InFile::new(file_id, attribute_id));
                        Some(body.print(&db, attribute))
                    }
                    FormIdx::CompileOption(attribute_id) => {
                        let attribute =
                            AnyAttribute::CompileOption(form_list[attribute_id].clone());
                        let body = db.compile_body(InFile::new(file_id, attribute_id));
                        Some(body.tree_print(&db, attribute))
                    }
                    _ => None,
                }
            })
            .collect::<Vec<_>>()
            .join("");
        expect.assert_eq(pretty.trim_start());
    }

    #[test]
    fn term_via_attribute_tuple() {
        check(
            r#"
            -compile({blah}).
            "#,
            expect![[r#"
                -compile(
                    Term::Tuple {
                        Literal(Atom('blah')),
                    }
                ).
            "#]],
        );
    }

    #[test]
    fn term_via_attribute_list() {
        check(
            r#"
            -compile(["blah",foo|$b]).
            "#,
            expect![[r#"
                -compile(
                    Term::List {
                        exprs
                            Literal(String(Normal("blah"))),
                            Literal(Atom('foo')),
                        tail
                            Literal(Char($b)),
                    }
                ).
            "#]],
        );
    }

    #[test]
    fn term_via_attribute_map() {
        check(
            r#"
            -compile(#{ xx => 5.3, yy => 3}).
            "#,
            expect![[r#"
                -compile(
                    Term::Map {
                        {
                            Literal(Atom('xx')) => Literal(Float(5.3)),
                        },
                        {
                            Literal(Atom('yy')) => Literal(Integer(3)),
                        },
                    }
                ).
            "#]],
        );
    }

    #[test]
    fn term_via_attribute_capture_fun() {
        check(
            r#"
            -compile({fun foo/1, fun mod:foo/1}).
            "#,
            expect![[r#"
                -compile(
                    Term::Tuple {
                        Term::Missing,
                        Term::CaptureFun {
                            fun mod:foo/1},
                    }
                ).
            "#]],
        );
    }

    #[test]
    fn term_via_attribute_capture_binary() {
        check(
            r#"
            -compile({<<"abc">>,
                      <<"abc", "def">>,
                      <<$a, $b, $c>>,
                      <<1, 2, 3, -1>>
                      }).
            "#,
            expect![[r#"
                -compile(
                    Term::Tuple {
                        Term::Binary {
                            <<"abc"/utf8>>
                        },
                        Term::Binary {
                            <<"abcdef"/utf8>>
                        },
                        Term::Binary {
                            <<"abc"/utf8>>
                        },
                        Term::Binary {
                            <<1, 2, 3, 255>>
                        },
                    }
                ).
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_tuple() {
        check(
            r#"
            foo() -> {a, 1}.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Tuple {
                            Literal(Atom('a')),
                            Literal(Integer(1)),
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_match() {
        check(
            r#"
            foo() -> A = $b.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Match {
                            lhs
                                Pat::Var(A)
                            rhs
                                Literal(Char($b))
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_list() {
        check(
            r#"
            foo() -> [A, b | 2.1].
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::List {
                            exprs
                                Expr::Var(A),
                                Literal(Atom('b')),
                            tail
                                Literal(Float(2.1)),
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_binary() {
        check(
            r#"
            foo() -> <<+1/integer-little-unit:8>>.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Binary {
                            BinarySeg {
                                elem
                                    Expr::UnaryOp {
                                        Literal(Integer(1))
                                        Plus,
                                    }
                                size
                                    None
                                tys
                                    integer,
                                    little,
                                unit
                                    8
                            }
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_record() {
        check(
            r#"
            foo() -> #record{field = 3, bar = 5 }.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Record {
                            name: Atom('record')
                            fields
                                Atom('field'):
                                    Literal(Integer(3)),
                                Atom('bar'):
                                    Literal(Integer(5)),
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_record_update() {
        check(
            r#"
            foo() -> Name#record{field = undefined}.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::RecordUpdate {
                            expr
                                Expr::Var(Name)
                            name: Atom('record')
                            fields
                                Atom('field'):
                                    Literal(Atom('undefined')),
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_record_index() {
        check(
            r#"
            foo() -> #rec.name.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::RecordIndex {
                            name: Atom('rec')
                            field: Atom('name')
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_record_field() {
        check(
            r#"
            foo() -> Name#record.field.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::RecordField {
                            expr
                                Expr::Var(Name)
                            name: Atom('record')
                            field: Atom('field')
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_map() {
        check(
            r#"
            foo() -> #{ foo => a + 3, bar => $v }.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Map {
                            {
                                Literal(Atom('foo')),
                                Expr::BinaryOp {
                                    lhs
                                        Literal(Atom('a'))
                                    rhs
                                        Literal(Integer(3))
                                    op
                                        ArithOp(Add),
                                },
                            },
                            {
                                Literal(Atom('bar')),
                                Literal(Char($v)),
                            },
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_map_update() {
        check(
            r#"
            foo() -> #{a => b}#{a := b, c => d}.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::MapUpdate {
                            expr
                                Expr::Map {
                                    {
                                        Literal(Atom('a')),
                                        Literal(Atom('b')),
                                    },
                                }
                            fields
                                Literal(Atom('a'))
                                    Exact
                                    Literal(Atom('b')),
                                Literal(Atom('c'))
                                    Assoc
                                    Literal(Atom('d')),
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_catch() {
        check(
            r#"
            foo() -> catch 1 + 2.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Catch {
                            expr
                                Expr::BinaryOp {
                                    lhs
                                        Literal(Integer(1))
                                    rhs
                                        Literal(Integer(2))
                                    op
                                        ArithOp(Add),
                                }
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_macrocall() {
        // TODO: set optional with/without MacroCall printing.
        check(
            r#"
            -define(EXPR(X), 1 + X).
            foo() -> ?EXPR(2).
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::BinaryOp {
                            lhs
                                Literal(Integer(1))
                            rhs
                                Literal(Integer(2))
                            op
                                ArithOp(Add),
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_call_mfa() {
        check(
            r#"
            foo() -> baz:bar(3,X).
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Call {
                            target
                                CallTarget::Remote {
                                    Literal(Atom('baz'))
                                    Literal(Atom('bar'))
                                }
                            args
                                Literal(Integer(3)),
                                Expr::Var(X),
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_call_fa() {
        check(
            r#"
            foo() -> bar(3,X).
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Call {
                            target
                                CallTarget::Local {
                                    Literal(Atom('bar'))
                                }
                            args
                                Literal(Integer(3)),
                                Expr::Var(X),
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_comprehension() {
        check(
            r#"
            foo() ->
                [X || X <- List, X >= 5].
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Comprehension {
                            builder
                                ComprehensionBuilder::List {
                                    Expr::Var(X)
                                }
                            exprs
                                ComprehensionExpr::ListGenerator {
                                    Pat::Var(X)
                                    Expr::Var(List)
                                },
                                ComprehensionExpr::Expr {
                                    Expr::BinaryOp {
                                        lhs
                                            Expr::Var(X)
                                        rhs
                                            Literal(Integer(5))
                                        op
                                            CompOp(Ord { ordering: Greater, strict: false }),
                                    }
                                },
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_comprehension_binary() {
        check(
            r#"
            foo() ->
                << Byte || <<Byte>> <= Bytes, Byte >= 5>>.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Comprehension {
                            builder
                                ComprehensionBuilder::Binary {
                                    Expr::Var(Byte)
                                }
                            exprs
                                ComprehensionExpr::BinGenerator {
                                    Pat::Binary {
                                        BinarySeg {
                                            elem
                                                Pat::Var(Byte)
                                            size
                                                None
                                            tys
                                            unit
                                        }
                                    }
                                    Expr::Var(Bytes)
                                },
                                ComprehensionExpr::Expr {
                                    Expr::BinaryOp {
                                        lhs
                                            Expr::Var(Byte)
                                        rhs
                                            Literal(Integer(5))
                                        op
                                            CompOp(Ord { ordering: Greater, strict: false }),
                                    }
                                },
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_map_comprehension() {
        check(
            r#"
            foo() ->
              #{KK => VV || KK := VV <- Map}.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Comprehension {
                            builder
                                ComprehensionBuilder::Map {
                                    Expr::Var(KK)
                                    =>
                                    Expr::Var(VV)
                                }
                            exprs
                                ComprehensionExpr::MapGenerator {
                                    Pat::Var(KK) :=
                                    Pat::Var(VV) <-
                                    Expr::Var(Map)
                                },
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_block() {
        check(
            r#"
            foo() -> begin 1, 2 end.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Block {
                            Literal(Integer(1)),
                            Literal(Integer(2)),
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_if() {
        check(
            r#"
            foo() ->
                if is_atom(X) -> ok;
                   true -> error
                end.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::If {
                            IfClause {
                                guards
                                    guard
                                        Expr::Call {
                                            target
                                                CallTarget::Remote {
                                                    Literal(Atom('erlang'))
                                                    Literal(Atom('is_atom'))
                                                }
                                            args
                                                Expr::Var(X),
                                        },
                                exprs
                                    Literal(Atom('ok')),
                            }
                            IfClause {
                                guards
                                    guard
                                        Literal(Atom('true')),
                                exprs
                                    Literal(Atom('error')),
                            }
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_case() {
        check(
            r#"
             foo() ->
                 case 1 + 2 of
                     X when X andalso true; X <= 100, X >= 5 -> ok;
                     _ -> error
                 end.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Case {
                            expr
                                Expr::BinaryOp {
                                    lhs
                                        Literal(Integer(1))
                                    rhs
                                        Literal(Integer(2))
                                    op
                                        ArithOp(Add),
                                }
                            clauses
                                CRClause {
                                    pat
                                        Pat::Var(X)
                                    guards
                                        guard
                                            Expr::BinaryOp {
                                                lhs
                                                    Expr::Var(X)
                                                rhs
                                                    Literal(Atom('true'))
                                                op
                                                    LogicOp(And { lazy: true }),
                                            },
                                        guard
                                            Expr::BinaryOp {
                                                lhs
                                                    Expr::Var(X)
                                                rhs
                                                    Literal(Integer(100))
                                                op
                                                    CompOp(Ord { ordering: Less, strict: true }),
                                            },
                                            Expr::BinaryOp {
                                                lhs
                                                    Expr::Var(X)
                                                rhs
                                                    Literal(Integer(5))
                                                op
                                                    CompOp(Ord { ordering: Greater, strict: false }),
                                            },
                                    exprs
                                        Literal(Atom('ok')),
                                }
                                CRClause {
                                    pat
                                        Pat::Var(_)
                                    guards
                                    exprs
                                        Literal(Atom('error')),
                                }
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_receive() {
        check(
            r#"
             foo() ->
                 receive
                     ok when true -> ok;
                     _ -> error
                 after Timeout -> timeout
                 end.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Receive {
                            clauses
                                CRClause {
                                    pat
                                        Literal(Atom('ok'))
                                    guards
                                        guard
                                            Literal(Atom('true')),
                                    exprs
                                        Literal(Atom('ok')),
                                }
                                CRClause {
                                    pat
                                        Pat::Var(_)
                                    guards
                                    exprs
                                        Literal(Atom('error')),
                                }
                            after
                                ReceiveAfter {
                                    timeout
                                        Expr::Var(Timeout)
                                    exprs
                                        Literal(Atom('timeout')),
                                }},
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_try() {
        check(
            r#"
             foo() ->
                 try 1, 2 of
                     _ -> ok
                 catch
                     Pat when true -> ok;
                     error:undef:Stack -> Stack
                 after
                     ok
                 end.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Try {
                            exprs
                                Literal(Integer(1)),
                                Literal(Integer(2)),
                            of_clauses
                                CRClause {
                                    pat
                                        Pat::Var(_)
                                    guards
                                    exprs
                                        Literal(Atom('ok')),
                                }
                            catch_clauses
                                CatchClause {
                                    class
                                    reason
                                        Pat::Var(Pat)
                                    stack
                                    guards
                                        guard
                                            Literal(Atom('true')),
                                    exprs
                                        Literal(Atom('ok')),
                                },
                                CatchClause {
                                    class
                                        Literal(Atom('error'))
                                    reason
                                        Literal(Atom('undef'))
                                    stack
                                        Pat::Var(Stack)
                                    guards
                                    exprs
                                        Expr::Var(Stack),
                                },
                            after
                                Literal(Atom('ok')),
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_capture_fun() {
        check(
            r#"
             foo() ->
                 fun foo/1,
                 fun mod:foo/1,
                 fun Mod:Foo/Arity.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::CaptureFun {
                            target
                                CallTarget::Local {
                                    Literal(Atom('foo'))
                                }
                            arity
                                Literal(Integer(1))
                        },
                        Expr::CaptureFun {
                            target
                                CallTarget::Remote {
                                    Literal(Atom('mod'))
                                    Literal(Atom('foo'))
                                }
                            arity
                                Literal(Integer(1))
                        },
                        Expr::CaptureFun {
                            target
                                CallTarget::Remote {
                                    Expr::Var(Mod)
                                    Expr::Var(Foo)
                                }
                            arity
                                Expr::Var(Arity)
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_closure() {
        check(
            r#"
             foo() ->
                 fun (ok) -> ok;
                     (error) -> error
                 end,
                 fun Named() -> Named() end.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Closure {
                            clauses
                                Clause {
                                    pats
                                        Literal(Atom('ok')),
                                    guards
                                    exprs
                                        Literal(Atom('ok')),
                                },
                                Clause {
                                    pats
                                        Literal(Atom('error')),
                                    guards
                                    exprs
                                        Literal(Atom('error')),
                                },
                            name
                        },
                        Expr::Closure {
                            clauses
                                Clause {
                                    pats
                                    guards
                                    exprs
                                        Expr::Call {
                                            target
                                                CallTarget::Local {
                                                    Expr::Var(Named)
                                                }
                                            args
                                        },
                                },
                            name
                                Pat::Var(Named)
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_maybe() {
        check(
            r#"
             foo() ->
               maybe
                   {ok, A} ?= a(),
                   true = A >= 0,
                   A
               else
                   error -> error;
                   Other when Other == 0 -> error
               end.
            "#,
            expect![[r#"
                Clause {
                    pats
                    guards
                    exprs
                        Expr::Maybe {
                            exprs
                                MaybeExpr::Cond {
                                    lhs
                                        Pat::Tuple {
                                            Literal(Atom('ok')),
                                            Pat::Var(A),
                                        }
                                    rhs
                                        Expr::Call {
                                            target
                                                CallTarget::Local {
                                                    Literal(Atom('a'))
                                                }
                                            args
                                        }
                                },
                                MaybeExpr::Expr(
                                    Expr::Match {
                                        lhs
                                            Literal(Atom('true'))
                                        rhs
                                            Expr::BinaryOp {
                                                lhs
                                                    Expr::Var(A)
                                                rhs
                                                    Literal(Integer(0))
                                                op
                                                    CompOp(Ord { ordering: Greater, strict: false }),
                                            }
                                    }
                                ),
                                MaybeExpr::Expr(
                                    Expr::Var(A)
                                ),
                            else_clauses
                                CRClause {
                                    pat
                                        Literal(Atom('error'))
                                    guards
                                    exprs
                                        Literal(Atom('error')),
                                }
                                CRClause {
                                    pat
                                        Pat::Var(Other)
                                    guards
                                        guard
                                            Expr::BinaryOp {
                                                lhs
                                                    Expr::Var(Other)
                                                rhs
                                                    Literal(Integer(0))
                                                op
                                                    CompOp(Eq { strict: false, negated: false }),
                                            },
                                    exprs
                                        Literal(Atom('error')),
                                }
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn expr_via_fun_missing() {
        check(
            r#"
             foo(a:b) -> a:b.
            "#,
            expect![[r#"
                Clause {
                    pats
                        Pat::Missing,
                    guards
                    exprs
                        Expr::Missing,
                }.
            "#]],
        );
    }

    #[test]
    fn pat_via_fun_match() {
        check(
            r#"
             foo(A = 4) -> ok.
            "#,
            expect![[r#"
                Clause {
                    pats
                        Pat::Match {
                            lhs
                                Pat::Var(A)
                            rhs
                                Literal(Integer(4))
                        },
                    guards
                    exprs
                        Literal(Atom('ok')),
                }.
            "#]],
        );
    }

    #[test]
    fn pat_via_fun_list() {
        check(
            r#"
             foo([A,4|X]) -> ok.
            "#,
            expect![[r#"
                Clause {
                    pats
                        Pat::List {
                            exprs
                                Pat::Var(A),
                                Literal(Integer(4)),
                            tail
                                Pat::Var(X),
                        },
                    guards
                    exprs
                        Literal(Atom('ok')),
                }.
            "#]],
        );
    }

    #[test]
    fn pat_via_fun_unary_op() {
        check(
            r#"
             foo(X = +1) -> ok.
            "#,
            expect![[r#"
                Clause {
                    pats
                        Pat::Match {
                            lhs
                                Pat::Var(X)
                            rhs
                                Pat::UnaryOp {
                                    Literal(Integer(1))
                                    Plus,
                                }
                        },
                    guards
                    exprs
                        Literal(Atom('ok')),
                }.
            "#]],
        );
    }

    #[test]
    fn pat_via_fun_binary_op() {
        check(
            r#"
             foo(X + 4) -> ok.
            "#,
            expect![[r#"
                Clause {
                    pats
                        Pat::BinaryOp {
                            lhs
                                Pat::Var(X)
                            rhs
                                Literal(Integer(4))
                            op
                                ArithOp(Add),
                        },
                    guards
                    exprs
                        Literal(Atom('ok')),
                }.
            "#]],
        );
    }

    #[test]
    fn pat_via_fun_record() {
        check(
            r#"
             foo(#rec{f=X, g=Y}) -> ok.
            "#,
            expect![[r#"
                Clause {
                    pats
                        Pat::Record {
                            name: Atom('rec')
                            fields
                                Atom('f'):
                                    Pat::Var(X),
                                Atom('g'):
                                    Pat::Var(Y),
                        },
                    guards
                    exprs
                        Literal(Atom('ok')),
                }.
            "#]],
        );
    }

    #[test]
    fn pat_via_fun_record_index() {
        check(
            r#"
             foo(#rec.f) -> ok.
            "#,
            expect![[r#"
                Clause {
                    pats
                        Pat::RecordIndex {
                            name: Atom('rec')
                            field: Atom('f')
                        },
                    guards
                    exprs
                        Literal(Atom('ok')),
                }.
            "#]],
        );
    }

    #[test]
    fn pat_via_fun_map() {
        check(
            r#"
             foo(#{1 + 2 := 3 + 4}) -> #{a => b}.
            "#,
            expect![[r#"
                Clause {
                    pats
                        Pat::Map {
                            {
                                Expr::BinaryOp {
                                    lhs
                                        Literal(Integer(1))
                                    rhs
                                        Literal(Integer(2))
                                    op
                                        ArithOp(Add),
                                },
                                Pat::BinaryOp {
                                    lhs
                                        Literal(Integer(3))
                                    rhs
                                        Literal(Integer(4))
                                    op
                                        ArithOp(Add),
                                },
                            },
                        },
                    guards
                    exprs
                        Expr::Map {
                            {
                                Literal(Atom('a')),
                                Literal(Atom('b')),
                            },
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn type_binary_op() {
        check(
            r#"
             -type foo() :: 1 + 1.
            "#,
            expect![[r#"
                -type foo() :: TypeExpr::BinaryOp {
                    lhs
                        Literal(Integer(1))
                    rhs
                        Literal(Integer(1))
                    op
                        ArithOp(Add),
                }.
            "#]],
        );
    }

    #[test]
    fn type_call() {
        check(
            r#"
             -type local(A) :: local(A | integer()).
             -type remote(A) :: module:remote(A | integer()).
            "#,
            expect![[r#"
                -type local(A) :: TypeExpr::Call {
                    target
                        CallTarget::Local {
                            Literal(Atom('local'))
                        }
                    args
                        TypeExpr::Union {
                            TypeExpr::Var(A),
                            TypeExpr::Call {
                                target
                                    CallTarget::Remote {
                                        Literal(Atom('erlang'))
                                        Literal(Atom('integer'))
                                    }
                                args
                            },
                        },
                }.

                -type remote(A) :: TypeExpr::Call {
                    target
                        CallTarget::Remote {
                            Literal(Atom('module'))
                            Literal(Atom('remote'))
                        }
                    args
                        TypeExpr::Union {
                            TypeExpr::Var(A),
                            TypeExpr::Call {
                                target
                                    CallTarget::Remote {
                                        Literal(Atom('erlang'))
                                        Literal(Atom('integer'))
                                    }
                                args
                            },
                        },
                }.
            "#]],
        );
    }

    #[test]
    fn type_fun() {
        check(
            r#"
            -type foo1() :: fun().
            -type foo2() :: fun(() -> ok).
            -type foo3() :: fun((a, b) -> ok).
            -type foo4() :: fun((...) -> ok).
            "#,
            expect![[r#"
                -type foo1() :: TypeExpr::Fun {
                    FunType::Any
                }.

                -type foo2() :: TypeExpr::Fun {
                    FunType::Full {
                        params
                        result
                            Literal(Atom('ok'))
                    }}.

                -type foo3() :: TypeExpr::Fun {
                    FunType::Full {
                        params
                            Literal(Atom('a')),
                            Literal(Atom('b')),
                        result
                            Literal(Atom('ok'))
                    }}.

                -type foo4() :: TypeExpr::Fun {
                    FunType::AnyArgs {
                        result
                            Literal(Atom('ok'))
                    }}.
            "#]],
        );
    }

    #[test]
    fn type_list() {
        check(
            r#"
            -type foo() :: [foo].
            -type bar() :: [bar, ...].
            -type baz() :: [].
            "#,
            expect![[r#"
                -type foo() :: TypeExpr::Fun {
                    ListType::Regular {
                        Literal(Atom('foo'))}}.

                -type bar() :: TypeExpr::Fun {
                    ListType::NonEmpty {
                        Literal(Atom('bar'))}}.

                -type baz() :: TypeExpr::Fun {
                    ListType::Empty
                }.
            "#]],
        );
    }

    #[test]
    fn type_map() {
        check(
            r#"
            -type foo() :: #{a => b, c := d}.
            "#,
            expect![[r#"
                -type foo() :: TypeExpr::Map {
                    Literal(Atom('a'))
                        Assoc
                        Literal(Atom('b')),
                    Literal(Atom('c'))
                        Exact
                        Literal(Atom('d')),
                }.
            "#]],
        );
    }

    #[test]
    fn type_range() {
        check(
            r#"
            -type foo() :: 1..100.
            "#,
            expect![[r#"
                -type foo() :: TypeExpr::Range {
                    lhs
                        Literal(Integer(1))
                    rhs
                        Literal(Integer(100))
                }.
            "#]],
        );
    }

    #[test]
    fn type_record() {
        check(
            r#"
            -type foo1() :: #record{}.
            -type foo2(B) :: #record{a :: integer(), b :: B}.
            -type foo3() :: #record{a ::}.
            "#,
            expect![[r#"
                -type foo1() :: TypeExpr::Record {
                    name: Atom('record')
                    fields
                }.

                -type foo2(B) :: TypeExpr::Record {
                    name: Atom('record')
                    fields
                        Atom('a'):
                            TypeExpr::Call {
                                target
                                    CallTarget::Remote {
                                        Literal(Atom('erlang'))
                                        Literal(Atom('integer'))
                                    }
                                args
                            },
                        Atom('b'):
                            TypeExpr::Var(B),
                }.

                -type foo3() :: TypeExpr::Record {
                    name: Atom('record')
                    fields
                        Atom('a'):
                            TypeExpr::Missing,
                }.
            "#]],
        );
    }

    #[test]
    fn type_tuple() {
        check(
            r#"
            -type foo() :: {a, b, c}.
            "#,
            expect![[r#"
                -type foo() :: TypeExpr::Tuple {
                    Literal(Atom('a')),
                    Literal(Atom('b')),
                    Literal(Atom('c')),
                }.
            "#]],
        );
    }

    #[test]
    fn type_unary_op() {
        check(
            r#"
            -type foo() :: -1.
            "#,
            expect![[r#"
                -type foo() :: TypeExpr::UnaryOp {
                    Literal(Integer(1))
                    Minus,
                }.
            "#]],
        );
    }

    #[test]
    fn type_ann_type() {
        check(
            r#"
            -type foo() :: A :: any().
            "#,
            expect![[r#"
                -type foo() :: TypeExpr::AnnType {
                    var
                        A
                    ty
                        TypeExpr::Call {
                            target
                                CallTarget::Remote {
                                    Literal(Atom('erlang'))
                                    Literal(Atom('any'))
                                }
                            args
                        }
                }.
            "#]],
        );
    }
}
