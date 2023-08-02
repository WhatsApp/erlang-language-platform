/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use eetf;
use eetf::Term;
use elp_syntax::SmolStr;
use fxhash::FxHashSet;

use super::auto_import;
use super::binary_specifier::Specifier;
use super::compiler_macro;
use super::expr::AtomLit;
use super::expr::BComprehension;
use super::expr::BGenerate;
use super::expr::BinOp;
use super::expr::Binary;
use super::expr::BinaryElem;
use super::expr::Block;
use super::expr::Body;
use super::expr::Case;
use super::expr::Catch;
use super::expr::Cons;
use super::expr::DynCall;
use super::expr::DynRemoteFun;
use super::expr::DynRemoteFunArity;
use super::expr::Expr;
use super::expr::Filter;
use super::expr::FloatLit;
use super::expr::If;
use super::expr::IntLit;
use super::expr::LComprehension;
use super::expr::LGenerate;
use super::expr::Lambda;
use super::expr::LocalCall;
use super::expr::LocalFun;
use super::expr::MComprehension;
use super::expr::MGenerate;
use super::expr::MapCreate;
use super::expr::MapUpdate;
use super::expr::Match;
use super::expr::NilLit;
use super::expr::Qualifier;
use super::expr::Receive;
use super::expr::ReceiveWithTimeout;
use super::expr::RecordCreate;
use super::expr::RecordField;
use super::expr::RecordFieldGen;
use super::expr::RecordFieldNamed;
use super::expr::RecordIndex;
use super::expr::RecordSelect;
use super::expr::RecordUpdate;
use super::expr::RemoteCall;
use super::expr::RemoteFun;
use super::expr::StringLit;
use super::expr::TryCatchExpr;
use super::expr::TryOfCatchExpr;
use super::expr::Tuple;
use super::expr::UnOp;
use super::expr::Var;
use super::ext_types::AnyArityFunExtType;
use super::ext_types::AnyListExtType;
use super::ext_types::AnyMapExtType;
use super::ext_types::AtomLitExtType;
use super::ext_types::BinOpType;
use super::ext_types::BuiltinExtType;
use super::ext_types::ConstrainedFunType;
use super::ext_types::Constraint;
use super::ext_types::ExtProp;
use super::ext_types::ExtType;
use super::ext_types::FunExtType;
use super::ext_types::IntLitExtType;
use super::ext_types::ListExtType;
use super::ext_types::LocalExtType;
use super::ext_types::MapExtType;
use super::ext_types::OptBadExtProp;
use super::ext_types::OptExtProp;
use super::ext_types::RecordExtType;
use super::ext_types::RecordRefinedExtType;
use super::ext_types::RefinedField;
use super::ext_types::RemoteExtType;
use super::ext_types::ReqBadExtProp;
use super::ext_types::ReqExtProp;
use super::ext_types::TupleExtType;
use super::ext_types::UnOpType;
use super::ext_types::UnionExtType;
use super::ext_types::VarExtType;
use super::form::BehaviourAttr;
use super::form::CompileExportAllAttr;
use super::form::ElpMetadataAttr;
use super::form::EqwalizerNowarnFunctionAttr;
use super::form::EqwalizerUnlimitedRefinementAttr;
use super::form::ExportAttr;
use super::form::ExportTypeAttr;
use super::form::ExternalCallback;
use super::form::ExternalForm;
use super::form::ExternalFunSpec;
use super::form::ExternalOpaqueDecl;
use super::form::ExternalOptionalCallbacks;
use super::form::ExternalRecDecl;
use super::form::ExternalRecField;
use super::form::ExternalTypeDecl;
use super::form::FileAttr;
use super::form::Fixme;
use super::form::FunDecl;
use super::form::ImportAttr;
use super::form::ModuleAttr;
use super::form::TypingAttribute;
use super::guard::Guard;
use super::guard::Test;
use super::guard::TestAtom;
use super::guard::TestBinOp;
use super::guard::TestBinaryLit;
use super::guard::TestCall;
use super::guard::TestCons;
use super::guard::TestMapCreate;
use super::guard::TestMapUpdate;
use super::guard::TestNil;
use super::guard::TestNumber;
use super::guard::TestRecordCreate;
use super::guard::TestRecordField;
use super::guard::TestRecordFieldGen;
use super::guard::TestRecordFieldNamed;
use super::guard::TestRecordIndex;
use super::guard::TestRecordSelect;
use super::guard::TestString;
use super::guard::TestTuple;
use super::guard::TestUnOp;
use super::guard::TestVar;
use super::pat::Pat;
use super::pat::PatAtom;
use super::pat::PatBinOp;
use super::pat::PatBinary;
use super::pat::PatBinaryElem;
use super::pat::PatCons;
use super::pat::PatInt;
use super::pat::PatMap;
use super::pat::PatMatch;
use super::pat::PatNil;
use super::pat::PatNumber;
use super::pat::PatRecord;
use super::pat::PatRecordFieldNamed;
use super::pat::PatRecordIndex;
use super::pat::PatString;
use super::pat::PatTuple;
use super::pat::PatUnOp;
use super::pat::PatVar;
use super::pat::PatWild;
use super::types::Type;
use super::ConversionError;
use super::Id;
use super::RemoteId;
use crate::ast;
use crate::ast::expr::Clause;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Converter {
    no_auto_imports: FxHashSet<ast::Id>,
    from_beam: bool,
    filter_stub: bool,
}

fn get_specifier(name: &str) -> Option<Specifier> {
    match name {
        "default" => Some(Specifier::UnsignedIntegerSpecifier),
        "integer" => Some(Specifier::UnsignedIntegerSpecifier),
        "float" => Some(Specifier::FloatSpecifier),
        "binary" => Some(Specifier::BinarySpecifier),
        "bytes" => Some(Specifier::BytesSpecifier),
        "bitstring" => Some(Specifier::BitstringSpecifier),
        "bits" => Some(Specifier::BitsSpecifier),
        "utf8" => Some(Specifier::Utf8Specifier),
        "utf16" => Some(Specifier::Utf16Specifier),
        "utf32" => Some(Specifier::Utf32Specifier),
        _ => None,
    }
}

impl Converter {
    fn make_pos(&self, start: u32, end: u32) -> ast::Pos {
        if self.from_beam {
            ast::Pos::LineAndColumn(ast::LineAndColumn {
                line: start,
                column: end,
            })
        } else {
            ast::Pos::TextRange(ast::TextRange {
                start_byte: start,
                end_byte: end,
            })
        }
    }

    fn convert_pos(&self, term: &eetf::Term) -> Result<ast::Pos, ConversionError> {
        match term {
            Term::Tuple(tuple) => {
                if let [Term::FixInteger(l), Term::FixInteger(c)] = &tuple.elements[..] {
                    return Ok(self.make_pos(l.value as u32, c.value as u32));
                }
            }
            Term::List(list) => {
                if let [Term::Tuple(_), Term::Tuple(loc)] = &list.elements[..] {
                    if let [Term::Atom(eetf::Atom { name }), pos] = &loc.elements[..] {
                        if name == "location" {
                            return self.convert_pos(pos);
                        }
                    }
                }
            }
            Term::FixInteger(l) => {
                return Ok(self.make_pos(l.value as u32, l.value as u32 + 1));
            }
            _ => (),
        };
        return Err(ConversionError::InvalidLocation);
    }

    fn convert_line(&self, line: &eetf::Term) -> Option<u32> {
        match line {
            Term::FixInteger(i) => Some(i.value as u32),
            _ => None,
        }
    }

    fn convert_id(&self, id: &eetf::Term) -> Result<ast::Id, ConversionError> {
        match id {
            Term::Tuple(tup) => {
                if let [Term::Atom(name), Term::FixInteger(arity)] = &tup.elements[..] {
                    return Ok(ast::Id {
                        name: name.name.clone().into(),
                        arity: arity.value as u32,
                    });
                }
                if let [Term::Atom(_), Term::Atom(name), Term::FixInteger(arity)] =
                    &tup.elements[..]
                {
                    return Ok(ast::Id {
                        name: name.name.clone().into(),
                        arity: arity.value as u32,
                    });
                }
            }
            _ => (),
        };
        return Err(ConversionError::InvalidID);
    }

    fn convert_ids(&self, ids: &eetf::List) -> Result<Vec<ast::Id>, ConversionError> {
        ids.elements.iter().map(|id| self.convert_id(id)).collect()
    }

    fn convert_varname(&self, v: &eetf::Term) -> Result<SmolStr, ConversionError> {
        if let Term::Tuple(var) = v {
            if let [Term::Atom(v), _, Term::Atom(name)] = &var.elements[..] {
                if v.name == "var" {
                    return Ok(name.name.clone().into());
                }
            }
        }
        return Err(ConversionError::InvalidVarName);
    }

    fn convert_name(&self, t: &eetf::Term) -> Result<SmolStr, ConversionError> {
        if let Term::Atom(atom) = t {
            return Ok(atom.name.clone().into());
        }
        return Err(ConversionError::InvalidName);
    }

    fn convert_attribute(
        &self,
        kind: &eetf::Atom,
        args: &eetf::Term,
        location: ast::Pos,
    ) -> Result<Option<ExternalForm>, ConversionError> {
        match (kind.name.as_str(), args) {
            ("module", Term::Atom(name)) => {
                return Ok(Some(ExternalForm::Module(ModuleAttr {
                    name: name.name.clone().into(),
                    location,
                })));
            }
            ("export", Term::List(ids)) => {
                return Ok(Some(ExternalForm::Export(ExportAttr {
                    funs: self.convert_ids(ids)?,
                    location,
                })));
            }
            ("import", Term::Tuple(imports)) => {
                if let [Term::Atom(m), Term::List(ids)] = &imports.elements[..] {
                    return Ok(Some(ExternalForm::Import(ImportAttr {
                        module: m.name.clone().into(),
                        funs: self.convert_ids(ids)?,
                        location,
                    })));
                }
                return Ok(None);
            }
            ("export_type", Term::List(ids)) => {
                return Ok(Some(ExternalForm::ExportType(ExportTypeAttr {
                    types: self.convert_ids(ids)?,
                    location,
                })));
            }
            ("record", Term::Tuple(rec)) => {
                if let [Term::Atom(name), Term::List(fields)] = &rec.elements[..] {
                    return Ok(Some(ExternalForm::ExternalRecDecl(ExternalRecDecl {
                        name: name.name.clone().into(),
                        fields: fields
                            .elements
                            .iter()
                            .map(|f| self.convert_rec_field_form(f))
                            .collect::<Result<Vec<_>, _>>()?,
                        location,
                    })));
                }
            }
            ("file", Term::Tuple(file)) => {
                if let [Term::List(name), line] = &file.elements[..] {
                    if let Some(line) = self.convert_line(line) {
                        let filename: String = name
                            .elements
                            .iter()
                            .flat_map(|v| match v {
                                Term::FixInteger(i) => char::from_u32(i.value as u32),
                                _ => None,
                            })
                            .collect();
                        return Ok(Some(ExternalForm::File(FileAttr {
                            file: filename.into(),
                            start: line,
                            location,
                        })));
                    }
                }
            }
            ("elp_metadata", Term::List(prop_list)) => {
                for prop in prop_list.elements.iter() {
                    if let Term::Tuple(prop) = prop {
                        if let [Term::Atom(tag), Term::List(raw_fixmes)] = &prop.elements[..] {
                            if tag.name == "eqwalizer_fixmes" {
                                let fixmes = raw_fixmes
                                    .elements
                                    .iter()
                                    .map(|f| self.convert_fixme(f))
                                    .collect::<Result<Vec<_>, _>>()?;
                                return Ok(Some(ExternalForm::ElpMetadata(ElpMetadataAttr {
                                    location,
                                    fixmes,
                                })));
                            }
                        }
                    }
                }
            }
            ("behaviour" | "behavior", Term::Atom(name)) => {
                return Ok(Some(ExternalForm::Behaviour(BehaviourAttr {
                    location,
                    name: name.name.clone().into(),
                })));
            }
            ("type", Term::Tuple(decl)) => {
                if let [Term::Atom(n), body, Term::List(vs)] = &decl.elements[..] {
                    let id = ast::Id {
                        name: n.name.clone().into(),
                        arity: vs.elements.len() as u32,
                    };
                    let body = self.convert_type(body)?;
                    let params = vs
                        .elements
                        .iter()
                        .map(|v| self.convert_varname(v))
                        .collect::<Result<Vec<_>, _>>()?;
                    return Ok(Some(ExternalForm::ExternalTypeDecl(ExternalTypeDecl {
                        location,
                        id,
                        params,
                        body,
                    })));
                }
            }
            ("opaque", Term::Tuple(decl)) => {
                if let [Term::Atom(n), body, Term::List(vs)] = &decl.elements[..] {
                    let id = ast::Id {
                        name: n.name.clone().into(),
                        arity: vs.elements.len() as u32,
                    };
                    let body = self.convert_type(body)?;
                    let params = vs
                        .elements
                        .iter()
                        .map(|v| self.convert_varname(v))
                        .collect::<Result<Vec<_>, _>>()?;
                    return Ok(Some(ExternalForm::ExternalOpaqueDecl(ExternalOpaqueDecl {
                        location,
                        id,
                        params,
                        body,
                    })));
                }
            }
            ("spec", Term::Tuple(spec)) => {
                if let [fun_id, Term::List(types)] = &spec.elements[..] {
                    let id = self.convert_id(fun_id)?;
                    let types = types
                        .elements
                        .iter()
                        .map(|s| self.convert_fun_spec(s))
                        .collect::<Result<Vec<_>, _>>()?;
                    return Ok(Some(ExternalForm::ExternalFunSpec(ExternalFunSpec {
                        location,
                        id,
                        types,
                    })));
                }
            }
            ("callback", Term::Tuple(cb)) => {
                if let [fun_id, Term::List(types)] = &cb.elements[..] {
                    let id = self.convert_id(fun_id)?;
                    let types = types
                        .elements
                        .iter()
                        .map(|s| self.convert_fun_spec(s))
                        .collect::<Result<Vec<_>, _>>()?;
                    return Ok(Some(ExternalForm::ExternalCallback(ExternalCallback {
                        location,
                        id,
                        types,
                    })));
                }
            }
            ("optional_callbacks", Term::List(ids)) => {
                let ids = self.convert_ids(ids)?;
                return Ok(Some(ExternalForm::ExternalOptionalCallbacks(
                    ExternalOptionalCallbacks { location, ids },
                )));
            }
            ("compile", Term::List(flags)) => {
                if flags.elements.iter().any(|f| self.is_export_all(f)) {
                    return Ok(Some(ExternalForm::CompileExportAll(CompileExportAllAttr {
                        location,
                    })));
                }
            }
            ("compile", Term::Atom(flag)) => {
                if flag.name == "export_all" {
                    return Ok(Some(ExternalForm::CompileExportAll(CompileExportAllAttr {
                        location,
                    })));
                }
            }
            ("typing", Term::List(elems)) => {
                let names = elems
                    .elements
                    .iter()
                    .map(|elem| self.convert_name(elem))
                    .collect::<Result<Vec<_>, _>>()?;
                return Ok(Some(ExternalForm::TypingAttribute(TypingAttribute {
                    location,
                    names,
                })));
            }
            ("eqwalizer", Term::Tuple(args)) => {
                if let [Term::Atom(pragma), args] = &args.elements[..] {
                    if pragma.name == "nowarn_function" {
                        let id = self.convert_id(args)?;
                        return Ok(Some(ExternalForm::EqwalizerNowarnFunction(
                            EqwalizerNowarnFunctionAttr { location, id },
                        )));
                    }
                    if pragma.name == "unlimited_refinement" {
                        let id = self.convert_id(args)?;
                        return Ok(Some(ExternalForm::EqwalizerUnlimitedRefinement(
                            EqwalizerUnlimitedRefinementAttr { location, id },
                        )));
                    }
                }
            }
            _ => (),
        };
        return Ok(None);
    }

    fn convert_fixme(&self, fixme: &eetf::Term) -> Result<Fixme, ConversionError> {
        if let Term::Tuple(data) = fixme {
            if let [
                Term::FixInteger(comment_start),
                Term::FixInteger(comment_end),
                Term::FixInteger(suppression_start),
                Term::FixInteger(suppression_end),
                Term::Atom(ignore),
            ] = &data.elements[..]
            {
                let comment = ast::TextRange {
                    start_byte: comment_start.value as u32,
                    end_byte: comment_end.value as u32,
                };
                let suppression = ast::TextRange {
                    start_byte: suppression_start.value as u32,
                    end_byte: suppression_end.value as u32,
                };
                let is_ignore = ignore.name == "true";
                return Ok(Fixme {
                    comment,
                    suppression,
                    is_ignore,
                });
            }
        }
        return Err(ConversionError::InvalidFixme);
    }

    fn is_export_all(&self, flag: &eetf::Term) -> bool {
        if let Term::Atom(flag) = flag {
            return flag.name == "export_all";
        }
        return false;
    }

    fn convert_function(
        &self,
        name: &eetf::Atom,
        arity: u32,
        clauses: &eetf::List,
        location: ast::Pos,
    ) -> Result<ExternalForm, ConversionError> {
        let id = ast::Id {
            name: name.name.clone().into(),
            arity,
        };
        let clauses = clauses
            .elements
            .iter()
            .map(|c| self.convert_clause(c))
            .collect::<Result<Vec<_>, _>>()?;
        return Ok(ExternalForm::FunDecl(FunDecl {
            id,
            clauses,
            location,
        }));
    }

    fn convert_clauses(&self, clauses: &eetf::List) -> Result<Vec<Clause>, ConversionError> {
        clauses
            .elements
            .iter()
            .map(|clause| self.convert_clause(clause))
            .collect()
    }

    fn convert_clause(&self, clause: &eetf::Term) -> Result<Clause, ConversionError> {
        if let Term::Tuple(cl) = clause {
            if let [
                Term::Atom(tag),
                pos,
                Term::List(pats),
                Term::List(guards),
                Term::List(exprs),
            ] = &cl.elements[..]
            {
                if tag.name == "clause" {
                    let location = self.convert_pos(pos)?;
                    let pats = pats
                        .elements
                        .iter()
                        .map(|p| self.convert_pat(p))
                        .collect::<Result<Vec<_>, _>>()?;
                    let guards = guards
                        .elements
                        .iter()
                        .map(|g| self.convert_guard(g))
                        .collect::<Result<Vec<_>, _>>()?;
                    let exprs = self.convert_exprs(exprs)?;
                    return Ok(Clause {
                        pats,
                        guards,
                        body: Body { exprs },
                        location,
                    });
                }
            }
        }
        return Err(ConversionError::InvalidClause);
    }

    fn convert_rec_field_form(
        &self,
        field: &eetf::Term,
    ) -> Result<ExternalRecField, ConversionError> {
        if let Term::Tuple(field) = field {
            match &field.elements[..] {
                [Term::Atom(kind), _pos, name_lit] if kind.name == "record_field" => {
                    let name = self.convert_atom_lit(name_lit)?;
                    return Ok(ExternalRecField {
                        name,
                        tp: None,
                        default_value: None,
                    });
                }
                [Term::Atom(kind), _pos, name_lit, expr] if kind.name == "record_field" => {
                    let name = self.convert_atom_lit(name_lit)?;
                    let default_value = self.convert_expr(expr)?;
                    return Ok(ExternalRecField {
                        name,
                        tp: None,
                        default_value: Some(default_value),
                    });
                }
                [Term::Atom(kind), field, ty] if kind.name == "typed_record_field" => {
                    let untyped_field = self.convert_rec_field_form(field)?;
                    let tp = self.convert_type(ty)?;
                    return Ok(ExternalRecField {
                        tp: Some(tp),
                        ..untyped_field
                    });
                }
                _ => (),
            }
        }
        return Err(ConversionError::InvalidRecordField);
    }

    fn convert_atom_lit(&self, atom: &eetf::Term) -> Result<SmolStr, ConversionError> {
        if let Term::Tuple(data) = atom {
            if let [Term::Atom(kind), _, Term::Atom(val)] = &data.elements[..] {
                if kind.name == "atom" {
                    return Ok(val.name.clone().into());
                }
            }
        }
        return Err(ConversionError::InvalidAtomLit);
    }

    fn convert_int_lit(&self, atom: &eetf::Term) -> Result<i32, ConversionError> {
        if let Term::Tuple(data) = atom {
            if let [Term::Atom(kind), _, Term::FixInteger(val)] = &data.elements[..] {
                if kind.name == "integer" {
                    return Ok(val.value);
                }
            }
        }
        return Err(ConversionError::InvalidIntLit);
    }

    fn convert_cons(&self, expr: &eetf::Term) -> Result<Expr, ConversionError> {
        let mut exprs = vec![];
        let mut expr = expr;
        loop {
            if let Term::Tuple(tuple) = expr {
                if let [Term::Atom(kind), pos, args @ ..] = &tuple.elements[..] {
                    match (kind.name.as_str(), args) {
                        ("cons", [he, te]) => {
                            let location = self.convert_pos(pos)?;
                            let h = self.convert_expr(he)?;
                            expr = te;
                            exprs.push((location, h));
                            continue;
                        }
                        _ => {
                            let end = self.convert_expr(expr)?;
                            return Ok(exprs.into_iter().rev().fold(end, |t, (location, h)| {
                                Expr::Cons(Cons {
                                    location,
                                    h: Box::new(h),
                                    t: Box::new(t),
                                })
                            }));
                        }
                    }
                }
            }
            return Err(ConversionError::InvalidExpr);
        }
    }

    fn convert_exprs(&self, exprs: &eetf::List) -> Result<Vec<Expr>, ConversionError> {
        exprs
            .elements
            .iter()
            .map(|expr| self.convert_expr(expr))
            .collect()
    }

    fn convert_expr(&self, expr: &eetf::Term) -> Result<Expr, ConversionError> {
        if let Term::Tuple(tuple) = expr {
            if let [Term::Atom(kind), pos, args @ ..] = &tuple.elements[..] {
                let location = self.convert_pos(pos)?;
                match (kind.name.as_str(), args) {
                    ("match", [pat, exp]) => {
                        let pat = self.convert_pat(pat)?;
                        let expr = self.convert_expr(exp)?;
                        return Ok(Expr::Match(Match {
                            location,
                            pat,
                            expr: Box::new(expr),
                        }));
                    }
                    ("var", [Term::Atom(name)]) => {
                        return Ok(Expr::Var(Var {
                            location,
                            n: name.name.clone().into(),
                        }));
                    }
                    ("tuple", [Term::List(exps)]) => {
                        let elems = self.convert_exprs(exps)?;
                        return Ok(Expr::Tuple(Tuple { location, elems }));
                    }
                    ("nil", []) => {
                        return Ok(Expr::NilLit(NilLit { location }));
                    }
                    ("cons", _) => {
                        return self.convert_cons(expr);
                    }
                    ("bin", [Term::List(bin_elems)]) => {
                        let elems = bin_elems
                            .elements
                            .iter()
                            .map(|e| self.convert_binary_elem(e))
                            .collect::<Result<Vec<_>, _>>()?;
                        return Ok(Expr::Binary(Binary { location, elems }));
                    }
                    ("op", [Term::Atom(op), e1, e2]) => {
                        let arg_1 = self.convert_expr(e1)?;
                        let arg_2 = self.convert_expr(e2)?;
                        return Ok(Expr::BinOp(BinOp {
                            location,
                            op: op.name.clone().into(),
                            arg_1: Box::new(arg_1),
                            arg_2: Box::new(arg_2),
                        }));
                    }
                    ("op", [Term::Atom(op), e]) => {
                        let arg = self.convert_expr(e)?;
                        return Ok(Expr::UnOp(UnOp {
                            location,
                            op: op.name.clone().into(),
                            arg: Box::new(arg),
                        }));
                    }
                    ("record", [Term::Atom(name), Term::List(fields)]) => {
                        return Ok(Expr::RecordCreate(RecordCreate {
                            location,
                            rec_name: name.name.clone().into(),
                            fields: fields
                                .elements
                                .iter()
                                .map(|f| self.convert_rec_field_expr(f))
                                .collect::<Result<Vec<_>, _>>()?,
                        }));
                    }
                    ("record", [expr, Term::Atom(name), Term::List(fields)]) => {
                        return Ok(Expr::RecordUpdate(RecordUpdate {
                            location,
                            expr: Box::new(self.convert_expr(expr)?),
                            rec_name: name.name.clone().into(),
                            fields: fields
                                .elements
                                .iter()
                                .map(|f| match self.convert_rec_field_expr(f) {
                                    Ok(RecordField::RecordFieldNamed(field)) => Ok(field),
                                    _ => Err(ConversionError::InvalidRecordUpdateField),
                                })
                                .collect::<Result<Vec<RecordFieldNamed>, ConversionError>>()?,
                        }));
                    }
                    ("record_index", [Term::Atom(name), field]) => {
                        return Ok(Expr::RecordIndex(RecordIndex {
                            location,
                            rec_name: name.name.clone().into(),
                            field_name: self.convert_atom_lit(field)?,
                        }));
                    }
                    ("record_field", [expr, Term::Atom(name), field]) => {
                        return Ok(Expr::RecordSelect(RecordSelect {
                            location,
                            expr: Box::new(self.convert_expr(expr)?),
                            rec_name: name.name.clone().into(),
                            field_name: self.convert_atom_lit(field)?,
                        }));
                    }
                    ("map", [Term::List(assocs)]) => {
                        return Ok(Expr::MapCreate(MapCreate {
                            location,
                            kvs: assocs
                                .elements
                                .iter()
                                .map(|kv| self.convert_create_kv(kv))
                                .collect::<Result<Vec<_>, _>>()?,
                        }));
                    }
                    ("map", [expr, Term::List(assocs)]) => {
                        let map = self.convert_expr(expr)?;
                        return Ok(Expr::MapUpdate(MapUpdate {
                            location,
                            map: Box::new(map),
                            kvs: assocs
                                .elements
                                .iter()
                                .map(|kv| self.convert_kv(kv))
                                .collect::<Result<Vec<_>, _>>()?,
                        }));
                    }
                    ("catch", [expr]) => {
                        return Ok(Expr::Catch(Catch {
                            location,
                            expr: Box::new(self.convert_expr(expr)?),
                        }));
                    }
                    ("call", [expr, Term::List(args)]) => {
                        let args: Vec<Expr> = self.convert_exprs(args)?;
                        let arity = args.len() as u32;
                        if let Term::Tuple(call) = expr {
                            match &call.elements[..] {
                                [Term::Atom(remote), _, m, f] if remote.name == "remote" => {
                                    match (self.convert_atom_lit(m), self.convert_atom_lit(f)) {
                                        (Ok(m), Ok(f)) => {
                                            let id = RemoteId {
                                                module: m,
                                                name: f,
                                                arity,
                                            };
                                            return Ok(Expr::RemoteCall(RemoteCall {
                                                location,
                                                id,
                                                args,
                                            }));
                                        }
                                        _ => (),
                                    }
                                }
                                [Term::Atom(atom), _, Term::Atom(fname)] if atom.name == "atom" => {
                                    let local_id = Id {
                                        name: fname.name.clone().into(),
                                        arity,
                                    };
                                    if compiler_macro::is_compiler_macro(&local_id) {
                                        let remote_id = RemoteId {
                                            module: compiler_macro::FAKE_MODULE.into(),
                                            name: fname.name.clone().into(),
                                            arity,
                                        };
                                        return Ok(Expr::RemoteCall(RemoteCall {
                                            location,
                                            id: remote_id,
                                            args,
                                        }));
                                    } else if auto_import::is_auto_imported(&local_id)
                                        && !self.no_auto_imports.contains(&local_id)
                                    {
                                        let remote_id = RemoteId {
                                            module: "erlang".into(),
                                            name: fname.name.clone().into(),
                                            arity,
                                        };
                                        return Ok(Expr::RemoteCall(RemoteCall {
                                            location,
                                            id: remote_id,
                                            args,
                                        }));
                                    } else {
                                        return Ok(Expr::LocalCall(LocalCall {
                                            location,
                                            id: local_id,
                                            args,
                                        }));
                                    }
                                }
                                _ => (),
                            }
                        }
                        return Ok(Expr::DynCall(DynCall {
                            location,
                            f: Box::new(self.convert_expr(expr)?),
                            args,
                        }));
                    }
                    ("lc", [template, Term::List(qualifiers)]) => {
                        return Ok(Expr::LComprehension(LComprehension {
                            location,
                            template: Box::new(self.convert_expr(template)?),
                            qualifiers: qualifiers
                                .elements
                                .iter()
                                .map(|q| self.convert_qualifier(q))
                                .collect::<Result<Vec<_>, _>>()?,
                        }));
                    }
                    ("bc", [template, Term::List(qualifiers)]) => {
                        return Ok(Expr::BComprehension(BComprehension {
                            location,
                            template: Box::new(self.convert_expr(template)?),
                            qualifiers: qualifiers
                                .elements
                                .iter()
                                .map(|q| self.convert_qualifier(q))
                                .collect::<Result<Vec<_>, _>>()?,
                        }));
                    }
                    ("mc", [template, Term::List(qualifiers)]) => {
                        let (k_template, v_template) = self.convert_create_kv(template)?;
                        return Ok(Expr::MComprehension(MComprehension {
                            location,
                            k_template: Box::new(k_template),
                            v_template: Box::new(v_template),
                            qualifiers: qualifiers
                                .elements
                                .iter()
                                .map(|q| self.convert_qualifier(q))
                                .collect::<Result<Vec<_>, _>>()?,
                        }));
                    }
                    ("block", [Term::List(exps)]) => {
                        return Ok(Expr::Block(Block {
                            location,
                            body: Body {
                                exprs: self.convert_exprs(exps)?,
                            },
                        }));
                    }
                    ("if", [Term::List(clauses)]) => {
                        return Ok(Expr::If(If {
                            location,
                            clauses: self.convert_clauses(clauses)?,
                        }));
                    }
                    ("case", [expr, Term::List(clauses)]) => {
                        return Ok(Expr::Case(Case {
                            location,
                            expr: Box::new(self.convert_expr(expr)?),
                            clauses: self.convert_clauses(clauses)?,
                        }));
                    }
                    (
                        "try",
                        [
                            Term::List(body),
                            Term::List(try_clauses),
                            Term::List(catch_clauses),
                            Term::List(after),
                        ],
                    ) => {
                        let try_body = Body {
                            exprs: self.convert_exprs(body)?,
                        };
                        let try_clauses: Vec<Clause> = self.convert_clauses(try_clauses)?;
                        let catch_clauses = self.convert_clauses(catch_clauses)?;
                        let after: Vec<Expr> = self.convert_exprs(after)?;
                        let after_body = if after.is_empty() {
                            None
                        } else {
                            Some(Body { exprs: after })
                        };
                        if try_clauses.is_empty() {
                            return Ok(Expr::TryCatchExpr(TryCatchExpr {
                                location,
                                try_body,
                                catch_clauses,
                                after_body,
                            }));
                        } else {
                            return Ok(Expr::TryOfCatchExpr(TryOfCatchExpr {
                                location,
                                try_clauses,
                                try_body,
                                catch_clauses,
                                after_body,
                            }));
                        }
                    }
                    ("receive", [Term::List(clauses)]) => {
                        return Ok(Expr::Receive(Receive {
                            location,
                            clauses: self.convert_clauses(clauses)?,
                        }));
                    }
                    ("receive", [Term::List(clauses), timeout, Term::List(defaults)]) => {
                        let timeout_exprs = self.convert_exprs(defaults)?;
                        let timeout_body = Body {
                            exprs: timeout_exprs,
                        };
                        return Ok(Expr::ReceiveWithTimeout(ReceiveWithTimeout {
                            location,
                            clauses: self.convert_clauses(clauses)?,
                            timeout: Box::new(self.convert_expr(timeout)?),
                            timeout_body,
                        }));
                    }
                    ("fun", [Term::Tuple(decl)]) => match &decl.elements[..] {
                        [Term::Atom(kind), Term::List(clauses)] if kind.name == "clauses" => {
                            return Ok(Expr::Lambda(Lambda {
                                location,
                                clauses: self.convert_clauses(clauses)?,
                                name: None,
                            }));
                        }
                        [Term::Atom(kind), Term::Atom(name), Term::FixInteger(arity)]
                            if kind.name == "function" =>
                        {
                            let local_id = Id {
                                name: name.name.clone().into(),
                                arity: arity.value as u32,
                            };
                            if auto_import::is_auto_imported(&local_id)
                                && !self.no_auto_imports.contains(&local_id)
                            {
                                let remote_id = RemoteId {
                                    module: "erlang".into(),
                                    name: name.name.clone().into(),
                                    arity: arity.value as u32,
                                };
                                return Ok(Expr::RemoteFun(RemoteFun {
                                    location,
                                    id: remote_id,
                                }));
                            } else {
                                return Ok(Expr::LocalFun(LocalFun {
                                    location,
                                    id: local_id,
                                }));
                            }
                        }
                        [Term::Atom(kind), module, name, arity] if kind.name == "function" => {
                            match (
                                self.convert_atom_lit(module),
                                self.convert_atom_lit(name),
                                self.convert_int_lit(arity),
                            ) {
                                (Ok(module), Ok(name), Ok(arity)) => {
                                    let remote_id = RemoteId {
                                        module,
                                        name,
                                        arity: arity as u32,
                                    };
                                    return Ok(Expr::RemoteFun(RemoteFun {
                                        location,
                                        id: remote_id,
                                    }));
                                }
                                _ => {
                                    return Ok(Expr::DynRemoteFunArity(DynRemoteFunArity {
                                        location,
                                        module: Box::new(self.convert_expr(module)?),
                                        name: Box::new(self.convert_expr(name)?),
                                        arity: Box::new(self.convert_expr(arity)?),
                                    }));
                                }
                            }
                        }
                        _ => (),
                    },
                    ("named_fun", [Term::Atom(name), Term::List(clauses)]) => {
                        return Ok(Expr::Lambda(Lambda {
                            location,
                            clauses: self.convert_clauses(clauses)?,
                            name: Some(name.name.clone().into()),
                        }));
                    }
                    ("atom", [Term::Atom(value)]) => {
                        return Ok(Expr::AtomLit(AtomLit {
                            location,
                            s: value.name.clone().into(),
                        }));
                    }
                    ("float", [Term::Float(_)]) => {
                        return Ok(Expr::FloatLit(FloatLit { location }));
                    }
                    ("char" | "integer", [Term::BigInteger(_)]) => {
                        return Ok(Expr::IntLit(IntLit {
                            location,
                            value: None,
                        }));
                    }
                    ("char" | "integer", [Term::FixInteger(value)]) => {
                        return Ok(Expr::IntLit(IntLit {
                            location,
                            value: Some(value.value),
                        }));
                    }
                    ("string", [Term::List(elems)]) if elems.is_nil() => {
                        return Ok(Expr::StringLit(StringLit {
                            location,
                            empty: true,
                        }));
                    }
                    ("string", [_]) => {
                        return Ok(Expr::StringLit(StringLit {
                            location,
                            empty: false,
                        }));
                    }
                    ("remote", [module, name]) => {
                        return Ok(Expr::DynRemoteFun(DynRemoteFun {
                            location,
                            module: Box::new(self.convert_expr(module)?),
                            name: Box::new(self.convert_expr(name)?),
                        }));
                    }
                    _ => (),
                }
            }
        }
        return Err(ConversionError::InvalidExpr);
    }

    fn convert_create_kv(&self, kv: &eetf::Term) -> Result<(Expr, Expr), ConversionError> {
        if let Term::Tuple(kv) = kv {
            if let [Term::Atom(atom_assoc), _, exp1, exp2] = &kv.elements[..] {
                if atom_assoc.name == "map_field_assoc" {
                    return Ok((self.convert_expr(exp1)?, self.convert_expr(exp2)?));
                }
            }
        }
        return Err(ConversionError::InvalidMapAssoc);
    }

    fn convert_kv(&self, kv: &eetf::Term) -> Result<(Expr, Expr), ConversionError> {
        if let Term::Tuple(kv) = kv {
            if let [_, _, exp1, exp2] = &kv.elements[..] {
                return Ok((self.convert_expr(exp1)?, self.convert_expr(exp2)?));
            }
        }
        return Err(ConversionError::InvalidKV);
    }

    fn convert_rec_field_expr(&self, field: &eetf::Term) -> Result<RecordField, ConversionError> {
        if let Term::Tuple(field) = field {
            if let [Term::Atom(atom_field), _, name, exp] = &field.elements[..] {
                if atom_field.name == "record_field" {
                    match self.convert_rec_field_name(name)? {
                        Some(name) => {
                            return Ok(RecordField::RecordFieldNamed(RecordFieldNamed {
                                name,
                                value: self.convert_expr(exp)?,
                            }));
                        }
                        None => {
                            return Ok(RecordField::RecordFieldGen(RecordFieldGen {
                                value: self.convert_expr(exp)?,
                            }));
                        }
                    }
                }
            }
        }
        return Err(ConversionError::InvalidRecordFieldExpr);
    }

    fn convert_rec_field_name(
        &self,
        term: &eetf::Term,
    ) -> Result<Option<SmolStr>, ConversionError> {
        if let Term::Tuple(field) = term {
            match &field.elements[..] {
                [Term::Atom(atom), _, Term::Atom(val)] if atom.name == "atom" => {
                    return Ok(Some(val.name.clone().into()));
                }
                [Term::Atom(var), _, Term::Atom(val)] if var.name == "var" && val.name == "_" => {
                    return Ok(None);
                }
                _ => (),
            }
        }
        return Err(ConversionError::InvalidRecordFieldName);
    }

    fn convert_binary_elem(&self, elem: &eetf::Term) -> Result<BinaryElem, ConversionError> {
        if let Term::Tuple(be) = elem {
            if let [Term::Atom(atom_be), pos, elem, size, spec] = &be.elements[..] {
                let location = self.convert_pos(pos)?;
                let specifier = self.convert_specifier(spec);
                let size = {
                    match size {
                        Term::Atom(a) if a.name == "default" => None,
                        e => Some(self.convert_expr(e)?),
                    }
                };
                if atom_be.name == "bin_element" {
                    return Ok(BinaryElem {
                        location,
                        size,
                        specifier,
                        expr: self.convert_expr(elem)?,
                    });
                }
            }
        }
        return Err(ConversionError::InvalidBinaryElem);
    }

    fn convert_pat(&self, pat: &eetf::Term) -> Result<Pat, ConversionError> {
        if let Term::Tuple(pat) = pat {
            if let [Term::Atom(kind), pos, args @ ..] = &pat.elements[..] {
                let location = self.convert_pos(pos)?;
                match (kind.name.as_str(), args) {
                    ("match", [pat1, pat2]) => {
                        return Ok(Pat::PatMatch(PatMatch {
                            location,
                            pat: Box::new(self.convert_pat(pat1)?),
                            arg: Box::new(self.convert_pat(pat2)?),
                        }));
                    }
                    ("var", [Term::Atom(v)]) if v.name == "_" => {
                        return Ok(Pat::PatWild(PatWild { location }));
                    }
                    ("var", [Term::Atom(v)]) => {
                        return Ok(Pat::PatVar(PatVar {
                            location,
                            n: v.name.clone().into(),
                        }));
                    }
                    ("tuple", [Term::List(pats)]) => {
                        return Ok(Pat::PatTuple(PatTuple {
                            location,
                            elems: pats
                                .elements
                                .iter()
                                .map(|p| self.convert_pat(p))
                                .collect::<Result<Vec<_>, _>>()?,
                        }));
                    }
                    ("nil", []) => {
                        return Ok(Pat::PatNil(PatNil { location }));
                    }
                    ("cons", [hpat, tpat]) => {
                        return Ok(Pat::PatCons(PatCons {
                            location,
                            h: Box::new(self.convert_pat(hpat)?),
                            t: Box::new(self.convert_pat(tpat)?),
                        }));
                    }
                    ("atom", [Term::Atom(v)]) => {
                        return Ok(Pat::PatAtom(PatAtom {
                            location,
                            s: v.name.clone().into(),
                        }));
                    }
                    ("float", [_]) => {
                        return Ok(Pat::PatNumber(PatNumber { location }));
                    }
                    ("char" | "integer", [Term::BigInteger(_)]) => {
                        return Ok(Pat::PatInt(PatInt { location }));
                    }
                    ("char" | "integer", [Term::FixInteger(_)]) => {
                        return Ok(Pat::PatInt(PatInt { location }));
                    }
                    ("string", [_]) => {
                        return Ok(Pat::PatString(PatString { location }));
                    }
                    ("bin", [Term::List(bin_elems)]) => {
                        return Ok(Pat::PatBinary(PatBinary {
                            location,
                            elems: bin_elems
                                .elements
                                .iter()
                                .map(|be| self.convert_pat_binary_elem(be))
                                .collect::<Result<Vec<_>, _>>()?,
                        }));
                    }
                    ("op", [Term::Atom(op), pat1, pat2]) => {
                        return Ok(Pat::PatBinOp(PatBinOp {
                            location,
                            op: op.name.clone().into(),
                            arg_1: Box::new(self.convert_pat(pat1)?),
                            arg_2: Box::new(self.convert_pat(pat2)?),
                        }));
                    }
                    ("op", [Term::Atom(op), pat]) => {
                        return Ok(Pat::PatUnOp(PatUnOp {
                            location,
                            op: op.name.clone().into(),
                            arg: Box::new(self.convert_pat(pat)?),
                        }));
                    }
                    ("record", [Term::Atom(name), Term::List(fields)]) => {
                        let fields_named = fields
                            .elements
                            .iter()
                            .map(|f| self.convert_pat_record_field(f))
                            .collect::<Result<Vec<_>, _>>()?
                            .into_iter()
                            .flatten()
                            .collect();
                        let gen = fields
                            .elements
                            .iter()
                            .map(|f| self.convert_pat_record_field_gen(f))
                            .collect::<Result<Vec<_>, _>>()?
                            .into_iter()
                            .flatten()
                            .next()
                            .map(|pat| Box::new(pat));
                        return Ok(Pat::PatRecord(PatRecord {
                            location,
                            rec_name: name.name.clone().into(),
                            fields: fields_named,
                            gen,
                        }));
                    }
                    ("record_index", [Term::Atom(name), field_name]) => {
                        return Ok(Pat::PatRecordIndex(PatRecordIndex {
                            location,
                            rec_name: name.name.clone().into(),
                            field_name: self.convert_atom_lit(field_name)?,
                        }));
                    }
                    ("map", [Term::List(kvs)]) => {
                        return Ok(Pat::PatMap(PatMap {
                            location,
                            kvs: kvs
                                .elements
                                .iter()
                                .map(|kv| self.convert_pat_kv(kv))
                                .collect::<Result<Vec<_>, _>>()?,
                        }));
                    }
                    _ => (),
                }
            }
        }
        return Err(ConversionError::InvalidPattern);
    }

    fn convert_pat_binary_elem(&self, pat: &eetf::Term) -> Result<PatBinaryElem, ConversionError> {
        if let Term::Tuple(pat) = pat {
            match &pat.elements[..] {
                [Term::Atom(at_bin_element), pos, elem, esize, specifier]
                    if at_bin_element.name == "bin_element" =>
                {
                    let size = {
                        match esize {
                            Term::Atom(a) if a.name == "default" => None,
                            expr => Some(self.convert_expr(expr)?),
                        }
                    };
                    let specifier = self.convert_specifier(specifier);
                    let location = self.convert_pos(pos)?;
                    return Ok(PatBinaryElem {
                        location,
                        pat: self.convert_pat(elem)?,
                        size,
                        specifier,
                    });
                }
                _ => (),
            }
        }
        return Err(ConversionError::InvalidPatBinaryElem);
    }

    fn convert_pat_record_field(
        &self,
        pat: &eetf::Term,
    ) -> Result<Option<PatRecordFieldNamed>, ConversionError> {
        if let Term::Tuple(elems) = pat {
            match &elems.elements[..] {
                [Term::Atom(at_field), _, name, pat] if at_field.name == "record_field" => {
                    let pat = self.convert_pat(pat)?;
                    return Ok(self
                        .convert_rec_field_name(name)?
                        .map(|name| PatRecordFieldNamed { name, pat }));
                }
                _ => (),
            }
        }
        return Err(ConversionError::InvalidPatRecordFieldNamed);
    }

    fn convert_pat_record_field_gen(
        &self,
        pat: &eetf::Term,
    ) -> Result<Option<Pat>, ConversionError> {
        if let Term::Tuple(elems) = pat {
            match &elems.elements[..] {
                [Term::Atom(at_field), _, name, pat] if at_field.name == "record_field" => {
                    if self.convert_rec_field_name(name)?.is_none() {
                        return Ok(Some(self.convert_pat(pat)?));
                    } else {
                        return Ok(None);
                    }
                }
                _ => (),
            }
        }
        return Err(ConversionError::InvalidPatRecordFieldGen);
    }

    fn convert_pat_kv(&self, pat: &eetf::Term) -> Result<(Test, Pat), ConversionError> {
        if let Term::Tuple(elems) = pat {
            match &elems.elements[..] {
                [Term::Atom(kind), _, test, pat]
                    if kind.name == "map_field_exact" || kind.name == "map_field_assoc" =>
                {
                    return Ok((self.convert_test(test)?, self.convert_pat(pat)?));
                }
                _ => (),
            }
        }
        return Err(ConversionError::InvalidKVPattern);
    }

    fn convert_guard(&self, guard: &eetf::Term) -> Result<Guard, ConversionError> {
        if let Term::List(tests) = guard {
            return Ok(Guard {
                tests: tests
                    .elements
                    .iter()
                    .map(|t| self.convert_test(t))
                    .collect::<Result<Vec<_>, _>>()?,
            });
        }
        return Err(ConversionError::InvalidGuard);
    }

    fn convert_test(&self, test: &eetf::Term) -> Result<Test, ConversionError> {
        if let Term::Tuple(elems) = test {
            if let [Term::Atom(kind), pos, args @ ..] = &elems.elements[..] {
                let location = self.convert_pos(pos)?;
                match (kind.name.as_str(), args) {
                    ("var", [Term::Atom(name)]) => {
                        return Ok(Test::TestVar(TestVar {
                            location,
                            v: name.name.clone().into(),
                        }));
                    }
                    ("tuple", [Term::List(tests)]) => {
                        let tests = tests
                            .elements
                            .iter()
                            .map(|t| self.convert_test(t))
                            .collect::<Result<Vec<_>, _>>()?;
                        return Ok(Test::TestTuple(TestTuple {
                            location,
                            elems: tests,
                        }));
                    }
                    ("nil", []) => {
                        return Ok(Test::TestNil(TestNil { location }));
                    }
                    ("cons", [h, t]) => {
                        return Ok(Test::TestCons(TestCons {
                            location,
                            h: Box::new(self.convert_test(h)?),
                            t: Box::new(self.convert_test(t)?),
                        }));
                    }
                    ("bin", [_]) => {
                        return Ok(Test::TestBinaryLit(TestBinaryLit { location }));
                    }
                    ("op", [Term::Atom(op), arg1, arg2]) => {
                        return Ok(Test::TestBinOp(TestBinOp {
                            location,
                            op: op.name.clone().into(),
                            arg_1: Box::new(self.convert_test(arg1)?),
                            arg_2: Box::new(self.convert_test(arg2)?),
                        }));
                    }
                    ("op", [Term::Atom(op), arg1]) => {
                        return Ok(Test::TestUnOp(TestUnOp {
                            location,
                            op: op.name.clone().into(),
                            arg: Box::new(self.convert_test(arg1)?),
                        }));
                    }
                    ("record", [Term::Atom(name), Term::List(fields)]) => {
                        let tests = fields
                            .elements
                            .iter()
                            .map(|f| self.convert_test_record_field(f))
                            .collect::<Result<Vec<_>, _>>()?;
                        return Ok(Test::TestRecordCreate(TestRecordCreate {
                            location,
                            rec_name: name.name.clone().into(),
                            fields: tests,
                        }));
                    }
                    ("record_index", [Term::Atom(name), field]) => {
                        let field_name = self.convert_atom_lit(field)?;
                        return Ok(Test::TestRecordIndex(TestRecordIndex {
                            location,
                            rec_name: name.name.clone().into(),
                            field_name,
                        }));
                    }
                    ("record_field", [test, Term::Atom(name), field]) => {
                        let field_name = self.convert_atom_lit(field)?;
                        let test = self.convert_test(test)?;
                        return Ok(Test::TestRecordSelect(TestRecordSelect {
                            location,
                            rec: Box::new(test),
                            rec_name: name.name.clone().into(),
                            field_name,
                        }));
                    }
                    ("map", [Term::List(kvs)]) => {
                        let tests = kvs
                            .elements
                            .iter()
                            .map(|kv| self.convert_test_kv(kv))
                            .collect::<Result<Vec<_>, _>>()?;
                        return Ok(Test::TestMapCreate(TestMapCreate {
                            location,
                            kvs: tests,
                        }));
                    }
                    ("map", [t, Term::List(kvs)]) => {
                        let map = self.convert_test(t)?;
                        let kvs = kvs
                            .elements
                            .iter()
                            .map(|kv| self.convert_test_kv(kv))
                            .collect::<Result<Vec<_>, _>>()?;
                        return Ok(Test::TestMapUpdate(TestMapUpdate {
                            location,
                            map: Box::new(map),
                            kvs,
                        }));
                    }
                    ("call", [Term::Tuple(expr), Term::List(args)]) => {
                        if let [Term::Atom(remote), _, module, fname] = &expr.elements[..] {
                            if remote.name == "remote" {
                                let module = self.convert_atom_lit(module)?;
                                if module == "erlang" {
                                    let fname = self.convert_atom_lit(fname)?;
                                    let id = Id {
                                        name: fname,
                                        arity: args.elements.len() as u32,
                                    };
                                    let args = args
                                        .elements
                                        .iter()
                                        .map(|t| self.convert_test(t))
                                        .collect::<Result<Vec<_>, _>>()?;
                                    return Ok(Test::TestCall(TestCall { location, id, args }));
                                }
                            }
                        }
                        if let [Term::Atom(atom), _, Term::Atom(fname)] = &expr.elements[..] {
                            if atom.name == "atom" {
                                let id = Id {
                                    name: fname.name.clone().into(),
                                    arity: args.elements.len() as u32,
                                };
                                let args = args
                                    .elements
                                    .iter()
                                    .map(|t| self.convert_test(t))
                                    .collect::<Result<Vec<_>, _>>()?;
                                return Ok(Test::TestCall(TestCall { location, id, args }));
                            }
                        }
                    }
                    ("atom", [Term::Atom(value)]) => {
                        return Ok(Test::TestAtom(TestAtom {
                            location,
                            s: value.name.clone().into(),
                        }));
                    }
                    ("float", [_]) => {
                        return Ok(Test::TestNumber(TestNumber {
                            location,
                            lit: None,
                        }));
                    }
                    ("char" | "integer", [Term::BigInteger(_)]) => {
                        return Ok(Test::TestNumber(TestNumber {
                            location,
                            lit: None,
                        }));
                    }
                    ("char" | "integer", [Term::FixInteger(v)]) => {
                        return Ok(Test::TestNumber(TestNumber {
                            location,
                            lit: Some(v.value),
                        }));
                    }
                    ("string", [_]) => {
                        return Ok(Test::TestString(TestString { location }));
                    }
                    _ => (),
                }
            }
        }
        return Err(ConversionError::InvalidTest);
    }

    fn convert_test_record_field(
        &self,
        term: &eetf::Term,
    ) -> Result<TestRecordField, ConversionError> {
        if let Term::Tuple(elems) = term {
            if let [Term::Atom(kind), _, name, val] = &elems.elements[..] {
                if kind.name == "record_field" {
                    match self.convert_rec_field_name(name)? {
                        Some(field_name) => {
                            return Ok(TestRecordField::TestRecordFieldNamed(
                                TestRecordFieldNamed {
                                    name: field_name,
                                    value: self.convert_test(val)?,
                                },
                            ));
                        }
                        None => {
                            return Ok(TestRecordField::TestRecordFieldGen(TestRecordFieldGen {
                                value: self.convert_test(val)?,
                            }));
                        }
                    }
                }
            }
        }
        return Err(ConversionError::InvalidRecordFieldTest);
    }

    fn convert_test_kv(&self, term: &eetf::Term) -> Result<(Test, Test), ConversionError> {
        if let Term::Tuple(elems) = term {
            if let [_, _, t1, t2] = &elems.elements[..] {
                return Ok((self.convert_test(t1)?, self.convert_test(t2)?));
            }
        }
        return Err(ConversionError::InvalidKVTest);
    }

    fn convert_qualifier(&self, term: &eetf::Term) -> Result<Qualifier, ConversionError> {
        if let Term::Tuple(elems) = term {
            match &elems.elements[..] {
                [Term::Atom(kind), _, pat, exp] if kind.name == "generate" => {
                    return Ok(Qualifier::LGenerate(LGenerate {
                        pat: self.convert_pat(pat)?,
                        expr: self.convert_expr(exp)?,
                    }));
                }
                [Term::Atom(kind), _, pat, exp] if kind.name == "b_generate" => {
                    return Ok(Qualifier::BGenerate(BGenerate {
                        pat: self.convert_pat(pat)?,
                        expr: self.convert_expr(exp)?,
                    }));
                }
                [Term::Atom(kind), _, Term::Tuple(m_elems), exp] if kind.name == "m_generate" => {
                    if let [Term::Atom(m_kind), _, k_pat, v_pat] = &m_elems.elements[..] {
                        if m_kind.name == "map_field_exact" {
                            return Ok(Qualifier::MGenerate(MGenerate {
                                k_pat: self.convert_pat(k_pat)?,
                                v_pat: self.convert_pat(v_pat)?,
                                expr: self.convert_expr(exp)?,
                            }));
                        }
                    }
                }
                _ => (),
            }
        }
        return Ok(Qualifier::Filter(Filter {
            expr: self.convert_expr(term)?,
        }));
    }

    fn convert_specifier(&self, term: &eetf::Term) -> Specifier {
        let unsigned_spec = {
            match term {
                Term::List(specs) => specs
                    .elements
                    .iter()
                    .flat_map(|term| match term {
                        Term::Atom(a) => get_specifier(a.name.as_str()),
                        _ => None,
                    })
                    .next()
                    .unwrap_or(Specifier::UnsignedIntegerSpecifier),
                _ => Specifier::UnsignedIntegerSpecifier,
            }
        };
        let signed = {
            match term {
                Term::List(specs) => specs.elements.iter().any(|term| match term {
                    Term::Atom(a) => a.name == "signed",
                    _ => false,
                }),
                _ => false,
            }
        };
        let spec = {
            if signed && unsigned_spec == Specifier::UnsignedIntegerSpecifier {
                Specifier::SignedIntegerSpecifier
            } else {
                unsigned_spec
            }
        };
        return spec;
    }

    fn convert_fun_spec(&self, spec: &eetf::Term) -> Result<ConstrainedFunType, ConversionError> {
        if let Term::Tuple(spec) = spec {
            if let [Term::Atom(ty), pos, Term::Atom(kind), Term::List(decl)] = &spec.elements[..] {
                if ty.name != "type" {
                    return Err(ConversionError::InvalidFunSpec);
                }
                if kind.name == "fun" {
                    if let [Term::Tuple(dom), result] = &decl.elements[..] {
                        if let [Term::Atom(ty), pos, Term::Atom(kind), Term::List(args)] =
                            &dom.elements[..]
                        {
                            if ty.name != "type" || kind.name != "product" {
                                return Err(ConversionError::InvalidFunSpec);
                            }
                            let location = self.convert_pos(pos)?;
                            let res_ty = self.convert_type(result)?;
                            let arg_tys = args
                                .elements
                                .iter()
                                .map(|t| self.convert_type(t))
                                .collect::<Result<Vec<_>, _>>()?;
                            let ty = FunExtType {
                                location: location.clone(),
                                arg_tys,
                                res_ty: Box::new(res_ty),
                            };
                            return Ok(ConstrainedFunType {
                                location,
                                ty,
                                constraints: vec![],
                            });
                        }
                    }
                } else if kind.name == "bounded_fun" {
                    let location = self.convert_pos(pos)?;
                    if let [ft, Term::List(constraints)] = &decl.elements[..] {
                        let fun_type = self.convert_type(ft)?;
                        let constraints = constraints
                            .elements
                            .iter()
                            .map(|c| self.convert_constraint(c))
                            .collect::<Result<Vec<_>, _>>()?;
                        if let ExtType::FunExtType(ty) = fun_type {
                            return Ok(ConstrainedFunType {
                                location,
                                ty,
                                constraints,
                            });
                        }
                    }
                }
            }
        }
        return Err(ConversionError::InvalidFunSpec);
    }

    fn convert_constraint(&self, cons: &eetf::Term) -> Result<Constraint, ConversionError> {
        if let Term::Tuple(cons) = cons {
            if let [Term::Atom(ty), pos, Term::Atom(cs), Term::List(decl)] = &cons.elements[..] {
                if let [is_sub, Term::List(vt)] = &decl.elements[..] {
                    if let [v, t] = &vt.elements[..] {
                        if ty.name == "type"
                            && cs.name == "constraint"
                            && self.convert_atom_lit(is_sub)? == "is_subtype".to_string()
                        {
                            let location = self.convert_pos(pos)?;
                            let t_var = self.convert_varname(v)?;
                            let ty = self.convert_type(t)?;
                            return Ok(Constraint {
                                location,
                                t_var,
                                ty,
                            });
                        }
                    }
                }
            }
        }
        return Err(ConversionError::InvalidFunConstraint);
    }

    fn convert_prop_type(&self, prop: &eetf::Term) -> Result<ExtProp, ConversionError> {
        if let Term::Tuple(prop) = prop {
            if let [Term::Atom(ty), pos, Term::Atom(kind), Term::List(kv)] = &prop.elements[..] {
                if ty.name != "type" {
                    return Err(ConversionError::InvalidPropType);
                }
                let location = self.convert_pos(pos)?;
                if let [kt, vt] = &kv.elements[..] {
                    let key_type = self.convert_type(kt)?;
                    let val_type = self.convert_type(vt)?;
                    if kind.name == "map_field_assoc" {
                        match key_type {
                            ExtType::AtomLitExtType(_) => {
                                return Ok(ExtProp::OptExtProp(OptExtProp {
                                    location,
                                    key: key_type,
                                    tp: val_type,
                                }));
                            }
                            _ => {
                                return Ok(ExtProp::OptBadExtProp(OptBadExtProp {
                                    location,
                                    key: key_type,
                                    tp: val_type,
                                }));
                            }
                        }
                    } else if kind.name == "map_field_exact" {
                        match key_type {
                            ExtType::AtomLitExtType(_) => {
                                return Ok(ExtProp::ReqExtProp(ReqExtProp {
                                    location,
                                    key: key_type,
                                    tp: val_type,
                                }));
                            }
                            _ => {
                                return Ok(ExtProp::ReqBadExtProp(ReqBadExtProp {
                                    location,
                                    key: key_type,
                                    tp: val_type,
                                }));
                            }
                        }
                    }
                }
            }
        }
        return Err(ConversionError::InvalidPropType);
    }

    fn convert_refined_field(&self, field: &eetf::Term) -> Result<RefinedField, ConversionError> {
        if let Term::Tuple(field) = field {
            match &field.elements[..] {
                [
                    Term::Atom(atom_ty),
                    _,
                    Term::Atom(atom_field_ty),
                    Term::List(field),
                ] if atom_ty.name == "type" && atom_field_ty.name == "field_type" => {
                    if let [name_lit, e_type] = &field.elements[..] {
                        return Ok(RefinedField {
                            label: self.convert_atom_lit(name_lit)?,
                            ty: self.convert_type(e_type)?,
                        });
                    }
                }
                _ => (),
            }
        }
        return Err(ConversionError::InvalidRecordRefinedField);
    }

    fn convert_type(&self, ty: &eetf::Term) -> Result<ExtType, ConversionError> {
        if let Term::Tuple(ty) = ty {
            if let [Term::Atom(kind), pos, def @ ..] = &ty.elements[..] {
                let location = self.convert_pos(pos)?;
                match (kind.name.as_str(), &def[..]) {
                    ("ann_type", [Term::List(ty)]) => {
                        if let [_, tp] = &ty.elements[..] {
                            return self.convert_type(tp);
                        }
                    }
                    ("atom", [Term::Atom(val)]) => {
                        return Ok(ExtType::AtomLitExtType(AtomLitExtType {
                            location,
                            atom: val.name.clone().into(),
                        }));
                    }
                    ("type", [Term::Atom(fun), Term::List(ty)])
                        if fun.name == "fun" && !ty.is_nil() =>
                    {
                        if let [Term::Tuple(dom), res_ty] = &ty.elements[..] {
                            let res_ty = self.convert_type(res_ty)?;
                            if let [Term::Atom(dom_ty), _, Term::Atom(dom_kind), args @ ..] =
                                &dom.elements[..]
                            {
                                if dom_ty.name == "type" && dom_kind.name == "any" {
                                    return Ok(ExtType::AnyArityFunExtType(AnyArityFunExtType {
                                        location,
                                        res_ty: Box::new(res_ty),
                                    }));
                                }
                                if dom_ty.name == "type" && dom_kind.name == "product" {
                                    if let [Term::List(args)] = &args[..] {
                                        let arg_tys = args
                                            .elements
                                            .iter()
                                            .map(|a| self.convert_type(a))
                                            .collect::<Result<Vec<_>, _>>()?;
                                        return Ok(ExtType::FunExtType(FunExtType {
                                            location,
                                            arg_tys,
                                            res_ty: Box::new(res_ty),
                                        }));
                                    }
                                }
                            }
                        }
                    }
                    ("type", [Term::Atom(kind), Term::List(range)]) if kind.name == "range" => {
                        if let [_range_first, _range_last] = &range.elements[..] {
                            return Ok(ExtType::int_ext_type(location));
                        }
                    }
                    ("type", [Term::Atom(kind), def]) if kind.name == "map" => match def {
                        Term::Atom(a) if a.name == "any" => {
                            return Ok(ExtType::AnyMapExtType(AnyMapExtType { location }));
                        }
                        Term::List(assoc) if assoc.elements.is_empty() => {
                            return Ok(ExtType::MapExtType(MapExtType {
                                props: Vec::new(),
                                location,
                            }));
                        }
                        Term::List(assoc) if assoc.elements.len() == 1 => {
                            let hd = assoc.elements.first().unwrap();
                            if let Term::Tuple(field) = hd {
                                if let [
                                    Term::Atom(prop_ty),
                                    prop_pos,
                                    Term::Atom(prop_kind),
                                    Term::List(kv),
                                ] = &field.elements[..]
                                {
                                    if prop_ty.name == "type"
                                        && prop_kind.name == "map_field_assoc"
                                        && kv.elements.len() == 2
                                    {
                                        let prop_pos = self.convert_pos(prop_pos)?;
                                        let key_type =
                                            self.convert_type(kv.elements.get(0).unwrap())?;
                                        let val_type =
                                            self.convert_type(kv.elements.get(1).unwrap())?;
                                        let prop = OptExtProp {
                                            location: prop_pos,
                                            key: key_type,
                                            tp: val_type,
                                        };
                                        return Ok(ExtType::MapExtType(MapExtType {
                                            props: vec![ExtProp::OptExtProp(prop)],
                                            location,
                                        }));
                                    }
                                }
                            }
                            return Ok(ExtType::MapExtType(MapExtType {
                                props: vec![self.convert_prop_type(&hd)?],
                                location,
                            }));
                        }
                        Term::List(assoc) => {
                            return Ok(ExtType::MapExtType(MapExtType {
                                props: assoc
                                    .elements
                                    .iter()
                                    .map(|ty| self.convert_prop_type(ty))
                                    .collect::<Result<Vec<_>, _>>()?,
                                location,
                            }));
                        }
                        _ => (),
                    },
                    ("type", [Term::Atom(kind), Term::List(decl)]) if kind.name == "record" => {
                        if let [record_name, field_tys @ ..] = &decl.elements[..] {
                            let record_name = self.convert_atom_lit(record_name)?;
                            if field_tys.is_empty() {
                                return Ok(ExtType::RecordExtType(RecordExtType {
                                    location,
                                    name: record_name,
                                }));
                            } else {
                                let refined_fields = field_tys
                                    .iter()
                                    .map(|ty| self.convert_refined_field(ty))
                                    .collect::<Result<Vec<_>, _>>()?;
                                return Ok(ExtType::RecordRefinedExtType(RecordRefinedExtType {
                                    location,
                                    name: record_name,
                                    refined_fields,
                                }));
                            }
                        }
                    }
                    ("remote_type", [Term::List(decl)]) => {
                        if let [module, name, Term::List(args)] = &decl.elements[..] {
                            let module = self.convert_atom_lit(module)?;
                            let name = self.convert_atom_lit(name)?;
                            let id = RemoteId {
                                module,
                                name,
                                arity: args.elements.len() as u32,
                            };
                            let args = args
                                .elements
                                .iter()
                                .map(|ty| self.convert_type(ty))
                                .collect::<Result<Vec<_>, _>>()?;
                            return Ok(ExtType::RemoteExtType(RemoteExtType {
                                location,
                                id,
                                args,
                            }));
                        }
                    }
                    ("user_type", [Term::Atom(name), Term::List(params)]) => {
                        let id = Id {
                            name: name.name.clone().into(),
                            arity: params.elements.len() as u32,
                        };
                        let args = params
                            .elements
                            .iter()
                            .map(|ty| self.convert_type(ty))
                            .collect::<Result<Vec<_>, _>>()?;
                        return Ok(ExtType::LocalExtType(LocalExtType { location, id, args }));
                    }
                    ("integer", [Term::BigInteger(_)]) => {
                        return Ok(ExtType::IntLitExtType(IntLitExtType { location }));
                    }
                    ("char", [Term::BigInteger(_)]) => {
                        return Ok(ExtType::char_ext_type(location));
                    }
                    ("integer", [Term::FixInteger(_)]) => {
                        return Ok(ExtType::IntLitExtType(IntLitExtType { location }));
                    }
                    ("char", [Term::FixInteger(_)]) => {
                        return Ok(ExtType::char_ext_type(location));
                    }
                    ("op", [Term::Atom(op), _]) => {
                        return Ok(ExtType::UnOpType(UnOpType {
                            location,
                            op: op.name.clone().into(),
                        }));
                    }
                    ("op", [Term::Atom(op), _, _]) => {
                        return Ok(ExtType::BinOpType(BinOpType {
                            location,
                            op: op.name.clone().into(),
                        }));
                    }
                    ("type", [Term::Atom(kind), Term::Atom(param)])
                        if kind.name == "tuple" && param.name == "any" =>
                    {
                        return Ok(ExtType::tuple_ext_type(location));
                    }
                    ("type", [Term::Atom(kind), Term::List(params)]) if kind.name == "tuple" => {
                        let arg_tys = params
                            .elements
                            .iter()
                            .map(|ty| self.convert_type(ty))
                            .collect::<Result<Vec<_>, _>>()?;
                        return Ok(ExtType::TupleExtType(TupleExtType { location, arg_tys }));
                    }
                    ("type", [Term::Atom(kind), Term::List(params)]) if kind.name == "union" => {
                        let tys = params
                            .elements
                            .iter()
                            .map(|ty| self.convert_type(ty))
                            .collect::<Result<Vec<_>, _>>()?;
                        return Ok(ExtType::UnionExtType(UnionExtType { location, tys }));
                    }
                    ("var", [Term::Atom(var)]) if var.name == "_" => {
                        return Ok(ExtType::any_ext_type(location));
                    }
                    ("var", [Term::Atom(var)]) => {
                        return Ok(ExtType::VarExtType(VarExtType {
                            location,
                            name: var.name.clone().into(),
                        }));
                    }
                    ("type", [Term::Atom(kind), Term::List(args)])
                        if (kind.name == "list" || kind.name == "nonempty_list") =>
                    {
                        if args.is_nil() {
                            return Ok(ExtType::AnyListExtType(AnyListExtType { location }));
                        } else if args.elements.len() == 1 {
                            let t = self.convert_type(args.elements.first().unwrap())?;
                            return Ok(ExtType::ListExtType(ListExtType {
                                location,
                                t: Box::new(t),
                            }));
                        }
                    }
                    ("type", [Term::Atom(kind), Term::List(args)])
                        if (kind.name == "maybe_improper_list"
                            || kind.name == "nonempty_improper_list"
                            || kind.name == "nonempty_maybe_improper_list")
                            && args.elements.len() == 2 =>
                    {
                        let t = self.convert_type(args.elements.first().unwrap())?;
                        return Ok(ExtType::ListExtType(ListExtType {
                            location,
                            t: Box::new(t),
                        }));
                    }
                    ("type", [Term::Atom(kind), Term::List(args)]) if args.is_nil() => {
                        let builtin = Type::builtin_type(kind.name.as_str());
                        if builtin.is_none() {
                            return Err(ConversionError::UnknownBuiltin(
                                kind.name.clone().into(),
                                0,
                            ));
                        } else {
                            return Ok(ExtType::BuiltinExtType(BuiltinExtType {
                                location,
                                name: kind.name.clone().into(),
                            }));
                        }
                    }
                    ("type", [Term::Atom(kind), Term::List(params)]) if kind.name == "binary" => {
                        if params.elements.len() == 1 || params.elements.len() == 2 {
                            return Ok(ExtType::binary_ext_type(location));
                        }
                    }
                    ("type", [Term::Atom(name), Term::List(params)]) => {
                        let arity = params.elements.len();
                        return Err(ConversionError::UnknownBuiltin(
                            name.name.clone().into(),
                            arity,
                        ));
                    }
                    _ => (),
                }
            }
        }
        return Err(ConversionError::InvalidType);
    }

    fn convert_form(&self, term: &eetf::Term) -> Result<Option<ExternalForm>, ConversionError> {
        if let Term::Tuple(tuple) = term {
            if let [Term::Atom(atom), _] = &tuple.elements[..] {
                if atom.name == "eof" {
                    return Ok(None);
                }
            }
            if let [Term::Atom(attr), pos, Term::Atom(kind), args] = &tuple.elements[..] {
                if attr.name == "attribute" {
                    let pos = self.convert_pos(pos)?;
                    return self.convert_attribute(kind, args, pos);
                }
            }
            if let [
                Term::Atom(fun),
                pos,
                Term::Atom(name),
                Term::FixInteger(arity),
                Term::List(clauses),
            ] = &tuple.elements[..]
            {
                if fun.name == "function" {
                    if !self.filter_stub {
                        let pos = self.convert_pos(pos)?;
                        let arity = arity.value as u32;
                        return Ok(Some(self.convert_function(name, arity, clauses, pos)?));
                    } else {
                        return Ok(None);
                    }
                }
            }
        }
        return Err(ConversionError::InvalidForm);
    }

    fn extract_no_auto_import(&self, term: &eetf::Term) -> Option<Vec<ast::Id>> {
        if let Term::Tuple(tuple) = term {
            if let [Term::Atom(attr), _, Term::Atom(kind), Term::Tuple(args)] = &tuple.elements[..]
            {
                if attr.name == "attribute" && kind.name == "compile" {
                    if let [Term::Atom(no_auto), Term::List(ids)] = &args.elements[..] {
                        if no_auto.name == "no_auto_import" {
                            return Some(
                                ids.elements
                                    .iter()
                                    .flat_map(|id| self.convert_id(id))
                                    .collect(),
                            );
                        }
                    }
                }
            }
        }
        return None;
    }
}

pub fn convert_forms(
    term: &eetf::Term,
    from_beam: bool,
    filter_stub: bool,
) -> Result<Vec<ExternalForm>, ConversionError> {
    if let Term::List(forms) = term {
        let dummy_converter = Converter {
            no_auto_imports: FxHashSet::default(),
            from_beam,
            filter_stub,
        };
        let no_auto_imports: FxHashSet<ast::Id> = forms
            .elements
            .iter()
            .flat_map(|f| dummy_converter.extract_no_auto_import(f))
            .flatten()
            .collect();
        let converter = Converter {
            no_auto_imports,
            from_beam,
            filter_stub,
        };
        return Ok(forms
            .elements
            .iter()
            .map(|f| converter.convert_form(f))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect());
    }
    return Err(ConversionError::InvalidForms);
}
