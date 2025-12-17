/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_base_db::FileId;
use elp_base_db::FileKind;
use elp_syntax::AstNode;
use elp_syntax::ast;
use elp_syntax::match_ast;

use crate::AnyExprRef;
use crate::Body;
use crate::CallTarget;
use crate::CallbackDef;
use crate::DefineDef;
use crate::Expr;
use crate::ExprId;
use crate::File;
use crate::FunctionDef;
use crate::InFile;
use crate::Literal;
use crate::Module;
use crate::Name;
use crate::NameArity;
use crate::Pat;
use crate::RecordDef;
use crate::RecordFieldDef;
use crate::ResolvedMacro;
use crate::Semantic;
use crate::Term;
use crate::TypeAliasDef;
use crate::TypeExpr;
use crate::TypeExprId;
use crate::VarDef;
use crate::macro_exp;
use crate::macro_exp::BuiltInMacro;
use crate::macro_exp::MacroExpCtx;
use crate::resolver::Resolver;
// @fb-only: use crate::sema::meta_only;

pub trait ToDef: Clone {
    type Def;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DefinitionOrReference<Definition, Reference> {
    Definition(Definition),
    Reference(Reference),
}

impl<Definition, Reference> DefinitionOrReference<Definition, Reference> {
    pub fn to_reference(self) -> Option<Reference> {
        match self {
            DefinitionOrReference::Definition(_) => None,
            DefinitionOrReference::Reference(reference) => Some(reference),
        }
    }
}

// ---------------------------------------------------------------------

#[derive(Clone, Debug)]
#[allow(clippy::large_enum_variant)]
pub enum AtomDef {
    Module(Module),
    Function(FunctionDef),
}

impl ToDef for ast::Atom {
    type Def = AtomDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let (body, body_map) = sema.find_body_and_map(ast.file_id, ast.value.syntax())?;
        let file_id = ast.file_id;
        let expr = ast.map(|atom| ast::Expr::from(ast::ExprMax::from(atom.clone())));
        let any_expr_id = body_map.any_id(expr.as_ref())?;
        let atom = match body.get_any(any_expr_id) {
            AnyExprRef::Expr(Expr::Literal(Literal::Atom(atom))) => atom,
            AnyExprRef::Pat(Pat::Literal(Literal::Atom(atom))) => atom,
            AnyExprRef::TypeExpr(TypeExpr::Literal(Literal::Atom(atom))) => atom,
            AnyExprRef::Term(Term::Literal(Literal::Atom(atom))) => atom,
            _ => return None,
        };
        let name = sema.db.lookup_atom(*atom);
        resolve_atom(sema, &name, file_id)
    }
}

fn resolve_atom(sema: &Semantic<'_>, name: &Name, file_id: FileId) -> Option<AtomDef> {
    resolve_testcase(sema, file_id, name)
        .map(AtomDef::Function)
        .or_else(|| resolve_module_name(sema, file_id, name).map(AtomDef::Module))
}

// ---------------------------------------------------------------------

impl ToDef for ast::BehaviourAttribute {
    type Def = Module;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let attr = sema.find_form(ast)?;
        let def = resolve_module_name(sema, ast.file_id, &attr.name)?;

        Some(def)
    }
}

impl ToDef for ast::ImportAttribute {
    type Def = Module;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let attr = sema.find_form(ast)?;
        let def = resolve_module_name(sema, ast.file_id, &attr.from)?;
        Some(def)
    }
}

// ---------------------------------------------------------------------

#[derive(Debug)]
pub enum CallDef {
    Function(FunctionDef),
    FuzzyFunction(FunctionDef),
    Type(TypeAliasDef),
    FuzzyType(TypeAliasDef),
}

impl ToDef for ast::Remote {
    type Def = CallDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let call = ast::Call::cast(ast.value.syntax().parent()?)?;
        ToDef::to_def(sema, ast.with_value(&call))
    }
}

impl ToDef for ast::Call {
    type Def = CallDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let (body, body_map) = sema.find_body_and_map(ast.file_id, ast.value.syntax())?;
        let file_id = ast.file_id;
        let expr = ast.map(|call| ast::Expr::from(call.clone()));
        let any_expr_id = body_map.any_id(expr.as_ref())?;
        let def = match body.get_any(any_expr_id) {
            AnyExprRef::Expr(Expr::Call { target, args }) => {
                let arity = args.len().try_into().ok()?;
                resolve_call_target(sema, target, Some(arity), file_id, &body)
                    .map(CallDef::Function)
                    .or_else(|| {
                        resolve_call_target(sema, target, None, file_id, &body)
                            .map(CallDef::FuzzyFunction)
                    })
            }
            AnyExprRef::TypeExpr(TypeExpr::Call { target, args }) => {
                let arity = args.len().try_into().ok()?;
                resolve_type_target(sema, target, Some(arity), file_id, &body)
                    .map(CallDef::Type)
                    .or_else(|| {
                        resolve_type_target(sema, target, None, file_id, &body)
                            .map(CallDef::FuzzyType)
                    })
            }
            _ => None,
        }?;
        Some(def)
    }
}

// ---------------------------------------------------------------------

impl ToDef for ast::InternalFun {
    type Def = FunctionDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let expr = ast.map(|fun| ast::Expr::from(ast::ExprMax::from(fun.clone())));
        resolve_capture(sema, expr)
    }
}

impl ToDef for ast::ExternalFun {
    type Def = FunctionDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let expr = ast.map(|fun| ast::Expr::from(ast::ExprMax::from(fun.clone())));
        resolve_capture(sema, expr)
    }
}

// ---------------------------------------------------------------------

impl ToDef for ast::Spec {
    type Def = FunctionDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let attr = sema.find_form(ast)?;
        // We ignore the `module` field - this is old syntax that is
        // not even fully supported in OTP and it's only allowed to
        // take the value of ?MODULE
        sema.db
            .def_map(ast.file_id)
            .get_function(&attr.name)
            .cloned()
    }
}

// ---------------------------------------------------------------------

impl ToDef for ast::RecordName {
    type Def = RecordDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let record_expr = ast::Expr::cast(ast.value.syntax().parent()?)?;
        let (record, _) = resolve_record(sema, ast.file_id, &record_expr, None)?;
        sema.db
            .def_map(ast.file_id)
            .get_records()
            .get(&record)
            .cloned()
    }
}

// ---------------------------------------------------------------------

impl ToDef for ast::RecordFieldName {
    type Def = RecordFieldDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let record_expr = ast::Expr::cast(ast.value.syntax().parent()?)?;
        let (record, field_name) = resolve_record(sema, ast.file_id, &record_expr, None)?;
        let field_name = field_name?;
        sema.db
            .def_map(ast.file_id)
            .get_records()
            .get(&record)?
            .find_field(sema.db, &field_name)
    }
}

// ---------------------------------------------------------------------

impl ToDef for ast::RecordField {
    type Def = DefinitionOrReference<RecordFieldDef, RecordFieldDef>;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let (idx, expr) = match_ast! {
            match (ast.value.syntax().parent()?) {
                ast::RecordExpr(rec) => (rec.fields().position(|def| &def == ast.value)?, rec.into()),
                ast::RecordUpdateExpr(rec) => (rec.fields().position(|def| &def == ast.value)?, rec.into()),
                ast::RecordDecl(rec) => {
                    let idx = rec.fields().position(|def| def == *ast.value)?;
                    let record = ToDef::to_def(sema, ast.with_value(&rec))?;
                    return Some(DefinitionOrReference::Definition(record.find_field_by_id(sema.db, idx)?));
                },
                _ => return None,
            }
        };
        let (record, field_name) = resolve_record(sema, ast.file_id, &expr, Some(idx))?;
        let field_name = field_name?;
        let def = sema
            .db
            .def_map(ast.file_id)
            .get_records()
            .get(&record)?
            .find_field(sema.db, &field_name)?;

        Some(DefinitionOrReference::Reference(def))
    }
}

// ---------------------------------------------------------------------

#[derive(Debug)]
pub enum MacroCallDef {
    Macro(DefineDef),
    Call(CallDef),
    Atom(AtomDef),
}

impl ToDef for ast::MacroCallExpr {
    type Def = MacroCallDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let name = macro_exp::macro_name(ast.value)?;
        let resolved = match sema.db.resolve_macro(ast.file_id, name.clone()) {
            Some(ResolvedMacro::User(resolved)) => resolved,
            Some(ResolvedMacro::BuiltIn(BuiltInMacro::FUNCTION_NAME)) => {
                if ast.value.args().is_some() {
                    // We have args, this represents a function call,
                    // to the name of the enclosing function, but with
                    // arity from the args.  Extract the lowered
                    // value, which deals with this.

                    let (body, body_map) =
                        sema.find_body_and_map(ast.file_id, ast.value.syntax())?;
                    let expr = ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(ast.value.clone()));
                    let expr_id = body_map.expr_id(InFile::new(ast.file_id, &expr))?;
                    match &body[expr_id] {
                        Expr::Call { target, args } => {
                            if let Some(call) =
                                target.resolve_call(args.len() as u32, sema, ast.file_id, &body)
                            {
                                return Some(MacroCallDef::Call(CallDef::Function(call)));
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    }
                } else {
                    return None;
                }
            }
            Some(ResolvedMacro::BuiltIn(BuiltInMacro::MODULE)) => {
                let form_list = sema.db.file_form_list(ast.file_id);
                let module_name = form_list.module_attribute().map(|attr| attr.name.clone());
                return module_name
                    .map(|name| resolve_atom(sema, &name, ast.file_id))
                    .and_then(|def| def.map(MacroCallDef::Atom));
            }
            Some(ResolvedMacro::BuiltIn(_)) => {
                return None;
            }
            None => {
                let name = name.with_arity(None);
                match sema.db.resolve_macro(ast.file_id, name) {
                    Some(ResolvedMacro::User(resolved)) => resolved,
                    _ => return None,
                }
            }
        };
        let form_list = sema.form_list(resolved.file_id);
        let define = form_list[resolved.value].clone();
        let file = File {
            file_id: resolved.file_id,
        };
        Some(MacroCallDef::Macro(DefineDef { file, define }))
    }
}

// ---------------------------------------------------------------------

impl ToDef for ast::PpInclude {
    type Def = File;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let idx = sema.find_form(ast)?;
        let def = sema
            .db
            .resolve_include(ast.with_value(idx))
            .map(|file_id| File { file_id })?;
        Some(def)
    }
}

// ---------------------------------------------------------------------

impl ToDef for ast::PpIncludeLib {
    type Def = File;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let idx = sema.find_form(ast)?;
        let def = sema
            .db
            .resolve_include(ast.with_value(idx))
            .map(|file_id| File { file_id })?;
        Some(def)
    }
}

// ---------------------------------------------------------------------

pub enum FaDef {
    Function(FunctionDef),
    FuzzyFunction(FunctionDef),
    Type(TypeAliasDef),
    FuzzyType(TypeAliasDef),
    Callback(CallbackDef),
}

impl ToDef for ast::Fa {
    type Def = FaDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let form_list = sema.form_list(ast.file_id);
        let parent = ast.value.syntax().parent()?;
        match_ast! {
            match (parent) {
                ast::ExportAttribute(attr) => {
                    let export_attr = sema.find_form(ast.with_value(&attr))?;
                    let idx = attr.funs().position(|fa| &fa == ast.value)? as u32;
                    let entry = export_attr
                        .entries
                        .clone()
                        .find(|&entry| form_list[entry].idx == idx)?;
                    sema.db
                        .def_map(ast.file_id)
                        .get_function(&form_list[entry].name)
                        .cloned()
                        .map(FaDef::Function)
                        .or_else(|| {
                            sema.db
                                .def_map(ast.file_id)
                                .get_function_any_arity(form_list[entry].name.name())
                                .cloned()
                                .map(FaDef::FuzzyFunction)
                        })
                },
                ast::ImportAttribute(attr) => {
                    let import_attr = sema.find_form(ast.with_value(&attr))?;
                    let idx = attr.funs().position(|fa| &fa == ast.value)? as u32;
                    let entry = import_attr
                        .entries
                        .clone()
                        .find(|&entry| form_list[entry].idx == idx)?;
                    let imported_module = ToDef::to_def(sema, InFile::new(ast.file_id, &attr))?;
                    sema.db
                        .def_map(imported_module.file.file_id)
                        .get_function(&form_list[entry].name)
                        .cloned()
                        .map(FaDef::Function)
                        .or_else(|| {
                            sema.db
                                .def_map(imported_module.file.file_id)
                                .get_function_any_arity(form_list[entry].name.name())
                                .cloned()
                                .map(FaDef::FuzzyFunction)
                        })
                },
                ast::ExportTypeAttribute(attr) => {
                    let export_attr = sema.find_form(ast.with_value(&attr))?;
                    let idx = attr.types().position(|fa| &fa == ast.value)? as u32;
                    let entry = export_attr
                        .entries
                        .clone()
                        .find(|&entry| form_list[entry].idx == idx)?;
                    sema.db
                        .def_map(ast.file_id)
                        .get_types()
                        .get(&form_list[entry].name)
                        .cloned()
                        .map(FaDef::Type)
                        .or_else(|| {
                            sema.db
                                .def_map(ast.file_id)
                                .get_type_any_arity(form_list[entry].name.name())
                                .cloned()
                                .map(FaDef::FuzzyType)
                        })
                },
                ast::OptionalCallbacksAttribute(attr) => {
                    let optional_callbacks = sema.find_form(ast.with_value(&attr))?;
                    let idx = attr.callbacks().position(|fa| &fa == ast.value)? as u32;
                    let entry = optional_callbacks
                        .entries
                        .clone()
                        .find(|&entry| form_list[entry].idx == idx)?;
                    sema.db
                        .def_map(ast.file_id)
                        .get_callbacks()
                        .get(&form_list[entry].name)
                        .cloned()
                        .map(FaDef::Callback)
                },
                _ => None,
            }
        }
    }
}

// ---------------------------------------------------------------------

impl ToDef for ast::MacroName {
    type Def = Vec<DefineDef>;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let form_list = sema.form_list(ast.file_id);
        let ctx = MacroExpCtx::new(form_list.data(), sema.db);
        let defines = ctx.find_defines_by_name(ast.value);
        let file = File {
            file_id: ast.file_id,
        };

        let resolved: Vec<_> = defines
            .into_iter()
            .map(|define| DefineDef {
                file,
                define: define.clone(),
            })
            .collect();

        if resolved.is_empty() {
            None
        } else {
            Some(resolved)
        }
    }
}

// ---------------------------------------------------------------------

impl ToDef for ast::Var {
    type Def = DefinitionOrReference<VarDef, Vec<VarDef>>;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let function_clause_id =
            sema.find_enclosing_function_clause_id(ast.file_id, ast.value.syntax())?;
        let in_clause = sema.to_function_clause_body(ast.with_value(function_clause_id));
        let (body, body_map) = sema
            .db
            .function_clause_body_with_source(ast.with_value(function_clause_id));
        let scopes = sema.db.function_clause_scopes(InFile {
            file_id: ast.file_id,
            value: function_clause_id,
        });
        let resolver = Resolver::new(scopes.clone());
        let expr = ast::Expr::ExprMax(ast::ExprMax::Var(ast.value.clone()));
        if let Some(expr_id) = body_map.expr_id(ast.with_value(&expr)) {
            let var = body.body[expr_id].as_var()?;
            in_clause.to_var_def(resolver.resolve_expr_id(&var, expr_id)?, var, None)
        } else {
            let pat_id = body_map.pat_id(ast.with_value(&expr))?;
            let var = body.body[pat_id].as_var()?;
            in_clause.to_var_def(resolver.resolve_pat_id(&var, pat_id)?, var, Some(pat_id))
        }
    }
}

// ---------------------------------------------------------------------

pub fn resolve_module_name(sema: &Semantic<'_>, file_id: FileId, name: &str) -> Option<Module> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nresolve_module_name: {file_id:?}"));
    let project_id = sema.db.file_app_data(file_id)?.project_id;
    let module_index = sema.db.module_index(project_id);
    let module_file_id = module_index.file_for_module(name)?;
    Some(Module {
        file: File {
            file_id: module_file_id,
        },
    })
}

pub fn resolve_call_target(
    sema: &Semantic<'_>,
    target: &CallTarget<ExprId>,
    arity: Option<u32>,
    file_id: FileId,
    body: &Body,
) -> Option<FunctionDef> {
    let (name, arity, file_id) = match target {
        CallTarget::Local { name } => {
            let name = sema.db.lookup_atom(body[*name].as_atom()?);
            (name, arity, file_id)
        }
        #[rustfmt::skip]
        CallTarget::Remote { module, name, .. } => {
            let module_name = sema.db.lookup_atom(body[*module].as_atom()?);
            let fn_name: Name = sema.db.lookup_atom(body[*name].as_atom()?);
            let mo =
                None; // @oss-only
                // @fb-only: meta_only::resolve_handle_call_target(sema, arity, file_id, &module_name, &fn_name);
            if let Some(r) = mo {
                r
            } else {
                (
                    fn_name,
                    arity,
                    (resolve_module_name(sema, file_id, &module_name)?
                        .file
                        .file_id),
                )
            }
        }
    };
    match arity {
        None => sema
            .db
            .def_map(file_id)
            .get_function_any_arity(&name)
            .cloned(),
        Some(arity) => {
            let name_arity = NameArity::new(name, arity);
            sema.db.def_map(file_id).get_function(&name_arity).cloned()
        }
    }
}

pub fn resolve_type_target(
    sema: &Semantic<'_>,
    target: &CallTarget<TypeExprId>,
    arity: Option<u32>,
    file_id: FileId,
    body: &Body,
) -> Option<TypeAliasDef> {
    let (file_id, type_expr) = match target {
        CallTarget::Local { name } => (file_id, *name),
        CallTarget::Remote { module, name, .. } => {
            let module = sema.db.lookup_atom(body[*module].as_atom()?);
            (
                resolve_module_name(sema, file_id, &module)?.file.file_id,
                *name,
            )
        }
    };

    let name = sema.db.lookup_atom(body[type_expr].as_atom()?);
    match arity {
        None => sema.db.def_map(file_id).get_type_any_arity(&name).cloned(),
        Some(arity) => {
            let name_arity = NameArity::new(name, arity);
            sema.db
                .def_map(file_id)
                .get_types()
                .get(&name_arity)
                .cloned()
        }
    }
}

fn resolve_capture(sema: &Semantic<'_>, fun: InFile<ast::Expr>) -> Option<FunctionDef> {
    let (body, body_map) = sema.find_body_and_map(fun.file_id, fun.value.syntax())?;
    let expr_id = body_map.expr_id(fun.as_ref())?;
    let (target, arity) = match &body[expr_id] {
        Expr::CaptureFun { target, arity } => (target, arity),
        _ => return None,
    };
    let arity = match &body[*arity] {
        Expr::Literal(Literal::Integer(int)) => int.value.try_into().ok()?,
        _ => return None,
    };
    resolve_call_target(sema, target, Some(arity), fun.file_id, &body)
}

fn resolve_record(
    sema: &Semantic<'_>,
    file_id: FileId,
    expr: &ast::Expr,
    idx: Option<usize>,
) -> Option<(Name, Option<Name>)> {
    let (body, body_map) = sema.find_body_and_map(file_id, expr.syntax())?;
    let expr = InFile::new(file_id, expr);
    let any_expr_id = body_map.any_id(expr)?;
    let (name, field) = match body.get_any(any_expr_id) {
        AnyExprRef::Expr(Expr::Record { name, fields }) => {
            (*name, idx.and_then(|idx| Some(fields.get(idx)?.0)))
        }
        AnyExprRef::Expr(Expr::RecordUpdate {
            expr: _,
            name,
            fields,
        }) => (*name, idx.and_then(|idx| Some(fields.get(idx)?.0))),
        AnyExprRef::Expr(Expr::RecordIndex { name, field }) => (*name, Some(*field)),
        AnyExprRef::Expr(Expr::RecordField {
            expr: _,
            name,
            field,
        }) => (*name, Some(*field)),
        AnyExprRef::Pat(Pat::Record { name, fields }) => {
            (*name, idx.and_then(|idx| Some(fields.get(idx)?.0)))
        }
        AnyExprRef::Pat(Pat::RecordIndex { name, field }) => (*name, Some(*field)),
        AnyExprRef::TypeExpr(TypeExpr::Record { name, fields }) => {
            (*name, idx.and_then(|idx| Some(fields.get(idx)?.0)))
        }
        _ => return None,
    };
    Some((
        sema.db.lookup_atom(name),
        field.map(|name| sema.db.lookup_atom(name)),
    ))
}

fn resolve_testcase(sema: &Semantic<'_>, file_id: FileId, name: &Name) -> Option<FunctionDef> {
    if sema.db.file_kind(file_id) != FileKind::TestModule {
        return None;
    }
    sema.db
        .def_map(file_id)
        .get_function(&NameArity::new(name.clone(), 1))
        .cloned()
}

// ---------------------------------------------------------------------

impl ToDef for ast::ModuleAttribute {
    type Def = Module;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let form = sema.find_form(ast)?;
        resolve_module_name(sema, ast.file_id, &form.name)
    }
}

impl ToDef for ast::TypeName {
    type Def = TypeAliasDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let form = sema.find_form(ast)?;
        sema.db.def_map(ast.file_id).get_type(form.name()).cloned()
    }
}

impl ToDef for ast::RecordDecl {
    type Def = RecordDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let form = sema.find_form(ast)?;
        sema.db.def_map(ast.file_id).get_record(&form.name).cloned()
    }
}

impl ToDef for ast::FunctionClause {
    type Def = FunctionDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let form_list = sema.form_list(ast.file_id);
        let idx = sema.find_enclosing_function_clause_id(ast.file_id, ast.value.syntax())?;
        let name = &form_list[idx].name;
        sema.db.def_map(ast.file_id).get_function(name).cloned()
    }
}

impl ToDef for ast::MacroLhs {
    type Def = DefineDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let define = ast::PpDefine::cast(ast.value.syntax().parent()?)?;
        let form = sema.find_form(ast.with_value(&define))?;
        Some(DefineDef {
            file: File {
                file_id: ast.file_id,
            },
            define: form,
        })
    }
}

impl ToDef for ast::Callback {
    type Def = CallbackDef;

    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let form = sema.find_form(ast)?;
        sema.db
            .def_map(ast.file_id)
            .get_callback(&form.name)
            .cloned()
    }
}

impl ToDef for ast::ExprArgs {
    type Def = CallDef;

    /// Looking specifically to pull out a function definition from a dynamic call,
    /// e.g. apply(Fun, Args) or rpc:call(Node, Module, Function, Args).
    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let (body, body_map) = sema.find_body_and_map(ast.file_id, ast.value.syntax())?;
        let call = ast::Expr::cast(ast.value.syntax().parent()?.clone())?;
        let expr = ast.with_value(call);
        let expr_id = body_map.expr_id(expr.as_ref())?;
        let def = match &body[expr_id] {
            Expr::Call { target, args } => match target {
                CallTarget::Local { name } => {
                    look_for_dynamic_call(sema, ast.file_id, None, *name, args, &body)
                }
                CallTarget::Remote { module, name, .. } => {
                    look_for_dynamic_call(sema, ast.file_id, Some(*module), *name, args, &body)
                }
            },
            _ => None,
        }?;
        Some(def)
    }
}

use fxhash::FxHashMap;
use lazy_static::lazy_static;

/// Pattern for matching dynamic call expressions (apply, rpc calls, etc.)
#[derive(Debug, Clone)]
pub(crate) struct DynamicCallPattern {
    /// Index of the target module argument (None when target function is in same module)
    pub(crate) module_arg_index: Option<usize>,
    /// Index of the target function argument
    pub(crate) function_arg_index: usize,
    /// Index of the arguments list or arity argument
    pub(crate) args_list_index: usize,
    /// Whether to extract arity directly from an integer argument (true) or from list length (false)
    pub(crate) direct_arity: bool,
}

pub(crate) type PatternKey = (Option<&'static str>, &'static str, usize);

#[macro_export]
macro_rules! add_patterns {
    ($patterns:ident, [$(($module:expr, $func:literal, $arity:literal, $module_idx:expr, $func_idx:literal, $args_idx:literal, $direct:expr)),* $(,)?]) => {
        $(
            $patterns.insert(
                ($module, $func, $arity),
                DynamicCallPattern {
                    module_arg_index: $module_idx,
                    function_arg_index: $func_idx,
                    args_list_index: $args_idx,
                    direct_arity: $direct,
                },
            );
        )*
    };
}

fn add_dynamic_call_patterns(patterns: &mut FxHashMap<PatternKey, DynamicCallPattern>) {
    // Each entry follows the format:
    //
    // (module, function, arity, module_arg_index, function_arg_index, args_list_index, direct_arity)
    //
    // Where:
    //
    // module:             Module name (Some("erlang"), Some("rpc"), etc.) or None for implicit erlang functions
    // function:           Function name as string literal (e.g., "apply", "call", "spawn")
    // arity:              Number of arguments this function pattern expects
    // module_arg_index:   Which argument position contains the target module name
    // function_arg_index: Which argument position contains the target function name
    // args_list_index:    Which argument position contains the arguments list or arity value
    // direct_arity:       true if the argument contains the arity value, false if the argument contains a list of arguments
    //
    // All indexes are 0-based.
    add_patterns!(
        patterns,
        [
            // erlang module (implicit)
            (None, "apply", 2, None, 0, 1, false),
            (None, "apply", 3, Some(0), 1, 2, false),
            (None, "function_exported", 3, Some(0), 1, 2, true),
            (None, "hibernate", 3, Some(0), 1, 2, false),
            (None, "is_builtin", 3, Some(0), 1, 2, true),
            (None, "spawn_link", 3, Some(0), 1, 2, false),
            (None, "spawn_link", 4, Some(1), 2, 3, false),
            (None, "spawn_monitor", 3, Some(0), 1, 2, false),
            (None, "spawn_monitor", 4, Some(1), 2, 3, false),
            (None, "spawn_opt", 4, Some(0), 1, 2, false),
            (None, "spawn_opt", 5, Some(1), 2, 3, false),
            (None, "spawn_request", 5, Some(1), 2, 3, false),
            (None, "spawn", 3, Some(0), 1, 2, false),
            (None, "spawn", 4, Some(1), 2, 3, false),
            // erlang module (explicit)
            (Some("erlang"), "apply", 2, None, 0, 1, false),
            (Some("erlang"), "apply", 3, Some(0), 1, 2, false),
            (Some("erlang"), "function_exported", 3, Some(0), 1, 2, true),
            (Some("erlang"), "hibernate", 3, Some(0), 1, 2, false),
            (Some("erlang"), "is_builtin", 3, Some(0), 1, 2, true),
            (Some("erlang"), "spawn_link", 3, Some(0), 1, 2, false),
            (Some("erlang"), "spawn_link", 4, Some(1), 2, 3, false),
            (Some("erlang"), "spawn_monitor", 3, Some(0), 1, 2, false),
            (Some("erlang"), "spawn_monitor", 4, Some(1), 2, 3, false),
            (Some("erlang"), "spawn_opt", 4, Some(0), 1, 2, false),
            (Some("erlang"), "spawn_opt", 5, Some(1), 2, 3, false),
            (Some("erlang"), "spawn_request", 5, Some(1), 2, 3, false),
            (Some("erlang"), "spawn", 3, Some(0), 1, 2, false),
            (Some("erlang"), "spawn", 4, Some(1), 2, 3, false),
            // rpc
            (Some("rpc"), "call", 4, Some(1), 2, 3, false),
            (Some("rpc"), "call", 5, Some(1), 2, 3, false),
            (Some("rpc"), "async_call", 4, Some(1), 2, 3, false),
            (Some("rpc"), "cast", 4, Some(1), 2, 3, false),
            (Some("rpc"), "multicall", 3, Some(0), 1, 2, false),
            (Some("rpc"), "multicall", 4, Some(0), 1, 2, false),
            (Some("rpc"), "multicall", 5, Some(1), 2, 3, false),
            // erpc
            (Some("erpc"), "call", 4, Some(1), 2, 3, false),
            (Some("erpc"), "call", 5, Some(1), 2, 3, false),
            (Some("erpc"), "cast", 4, Some(1), 2, 3, false),
            (Some("erpc"), "multicall", 4, Some(1), 2, 3, false),
            (Some("erpc"), "multicall", 5, Some(1), 2, 3, false),
            (Some("erpc"), "multicast", 4, Some(1), 2, 3, false),
            (Some("erpc"), "send_request", 6, Some(1), 2, 3, false),
        ]
    );
}

/// Specifies what forms a module argument can take.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleArgType {
    /// The argument must be a single module atom (e.g., `apply(Mod, Fun, Args)`)
    Atom,
    /// The argument must be a list of module atoms (e.g., some batch operations)
    List,
    /// The argument can be either a single module atom or a list of modules
    /// (e.g., `meck:new(Mod | [Mod], Opts)`)
    AtomOrList,
}

/// Pattern for matching module argument positions in function calls.
/// Used by rename operations to identify which argument contains a module name.
#[derive(Debug, Clone, Copy)]
pub struct ModuleArgPattern {
    /// Index of the argument containing the module name (0-based)
    pub index: usize,
    /// The type of the module argument (atom, list, or either)
    pub arg_type: ModuleArgType,
}

impl ModuleArgPattern {
    /// Creates a pattern where the argument is a single module atom.
    pub const fn atom(index: usize) -> Self {
        Self {
            index,
            arg_type: ModuleArgType::Atom,
        }
    }

    /// Creates a pattern where the argument is a list of module atoms.
    pub const fn list(index: usize) -> Self {
        Self {
            index,
            arg_type: ModuleArgType::List,
        }
    }

    /// Creates a pattern where the argument can be either a single atom or a list.
    pub const fn atom_or_list(index: usize) -> Self {
        Self {
            index,
            arg_type: ModuleArgType::AtomOrList,
        }
    }

    /// Returns true if this pattern accepts a single atom.
    pub const fn accepts_atom(&self) -> bool {
        matches!(
            self.arg_type,
            ModuleArgType::Atom | ModuleArgType::AtomOrList
        )
    }

    /// Returns true if this pattern accepts a list of atoms.
    pub const fn accepts_list(&self) -> bool {
        matches!(
            self.arg_type,
            ModuleArgType::List | ModuleArgType::AtomOrList
        )
    }
}

fn add_module_argument_patterns(patterns: &mut FxHashMap<PatternKey, ModuleArgPattern>) {
    // Each entry follows the format:
    // (module, function, arity) -> ModuleArgPattern
    //
    // Where:
    // module:           Module name (Some("meck"), Some("application"), etc.)
    // function:         Function name as string literal (e.g., "new", "get_env")
    // arity:            Number of arguments this function pattern expects
    // ModuleArgPattern: Contains the argument index and the expected type
    //
    // All indexes are 0-based.

    // meck - mocking library
    // meck:new/2 accepts either a single module atom or a list of modules
    patterns.insert((Some("meck"), "called", 3), ModuleArgPattern::atom(0));
    patterns.insert((Some("meck"), "called", 4), ModuleArgPattern::atom(0));
    patterns.insert((Some("meck"), "capture", 5), ModuleArgPattern::atom(1));
    patterns.insert((Some("meck"), "capture", 6), ModuleArgPattern::atom(1));
    patterns.insert(
        (Some("meck"), "delete", 3),
        ModuleArgPattern::atom_or_list(0),
    );
    patterns.insert(
        (Some("meck"), "delete", 4),
        ModuleArgPattern::atom_or_list(0),
    );
    patterns.insert(
        (Some("meck"), "expect", 3),
        ModuleArgPattern::atom_or_list(0),
    );
    patterns.insert(
        (Some("meck"), "expect", 4),
        ModuleArgPattern::atom_or_list(0),
    );
    patterns.insert(
        (Some("meck"), "expects", 2),
        ModuleArgPattern::atom_or_list(0),
    );
    patterns.insert((Some("meck"), "history", 1), ModuleArgPattern::atom(0));
    patterns.insert((Some("meck"), "history", 2), ModuleArgPattern::atom(0));
    patterns.insert((Some("meck"), "loop", 4), ModuleArgPattern::atom_or_list(0));
    patterns.insert((Some("meck"), "new", 1), ModuleArgPattern::atom_or_list(0));
    patterns.insert((Some("meck"), "new", 2), ModuleArgPattern::atom_or_list(0));
    patterns.insert((Some("meck"), "num_calls", 3), ModuleArgPattern::atom(0));
    patterns.insert((Some("meck"), "num_calls", 4), ModuleArgPattern::atom(0));
    patterns.insert(
        (Some("meck"), "reset", 1),
        ModuleArgPattern::atom_or_list(0),
    );
    patterns.insert(
        (Some("meck"), "sequence", 4),
        ModuleArgPattern::atom_or_list(0),
    );
    patterns.insert(
        (Some("meck"), "unload", 1),
        ModuleArgPattern::atom_or_list(0),
    );
    patterns.insert(
        (Some("meck"), "validate", 1),
        ModuleArgPattern::atom_or_list(0),
    );
    patterns.insert((Some("meck"), "wait", 4), ModuleArgPattern::atom(0));
    patterns.insert((Some("meck"), "wait", 5), ModuleArgPattern::atom(1));
    patterns.insert((Some("meck"), "wait", 6), ModuleArgPattern::atom(1));

    // code module - module loading and management
    // These functions from the Erlang stdlib take module() as their argument
    patterns.insert((Some("code"), "load_file", 1), ModuleArgPattern::atom(0));
    patterns.insert(
        (Some("code"), "ensure_loaded", 1),
        ModuleArgPattern::atom(0),
    );
    patterns.insert((Some("code"), "delete", 1), ModuleArgPattern::atom(0));
    patterns.insert((Some("code"), "purge", 1), ModuleArgPattern::atom(0));
    patterns.insert((Some("code"), "soft_purge", 1), ModuleArgPattern::atom(0));
    patterns.insert((Some("code"), "is_loaded", 1), ModuleArgPattern::atom(0));
    patterns.insert(
        (Some("code"), "get_object_code", 1),
        ModuleArgPattern::atom(0),
    );
    patterns.insert((Some("code"), "module_md5", 1), ModuleArgPattern::atom(0));
    patterns.insert((Some("code"), "is_sticky", 1), ModuleArgPattern::atom(0));
}

// Lazy static initialization for the patterns maps
lazy_static! {
    static ref DYNAMIC_CALL_PATTERNS: FxHashMap<PatternKey, DynamicCallPattern> = {
        let mut patterns = FxHashMap::default();
        add_dynamic_call_patterns(&mut patterns);
        // @fb-only: meta_only::add_dynamic_call_patterns(&mut patterns);
        patterns
    };
    static ref MODULE_ARGUMENT_PATTERNS: FxHashMap<PatternKey, ModuleArgPattern> = {
        let mut patterns = FxHashMap::default();
        add_module_argument_patterns(&mut patterns);
        // @fb-only: meta_only::add_module_argument_patterns(&mut patterns);
        patterns
    };
    /// Combined patterns for module argument positions.
    /// Merges dynamic call patterns (that have module_arg_index) with simple module argument patterns.
    /// Used by rename operations where we only care about the module argument position.
    static ref COMBINED_MODULE_ARG_PATTERNS: FxHashMap<PatternKey, ModuleArgPattern> = {
        let mut patterns: FxHashMap<PatternKey, ModuleArgPattern> = FxHashMap::default();
        // Add module_arg_index from dynamic call patterns (where present)
        for (key, pattern) in DYNAMIC_CALL_PATTERNS.iter() {
            if let Some(module_idx) = pattern.module_arg_index {
                patterns.insert(*key, ModuleArgPattern::atom(module_idx));
            }
        }
        // Add from simple module argument patterns
        for (key, module_arg_pattern) in MODULE_ARGUMENT_PATTERNS.iter() {
            patterns.insert(*key, *module_arg_pattern);
        }
        patterns
    };
}

fn get_dynamic_call_patterns() -> &'static FxHashMap<PatternKey, DynamicCallPattern> {
    &DYNAMIC_CALL_PATTERNS
}

pub fn get_module_arg_patterns() -> &'static FxHashMap<PatternKey, ModuleArgPattern> {
    &COMBINED_MODULE_ARG_PATTERNS
}

fn look_for_dynamic_call(
    sema: &Semantic,
    file_id: FileId,
    module: Option<ExprId>,
    fun: ExprId,
    args: &[ExprId],
    body: &Body,
) -> Option<CallDef> {
    let module_name = if let Some(module) = module {
        let atom = body[module].as_atom()?;
        Some(sema.db.lookup_atom(atom))
    } else {
        None
    };

    let function_atom = body[fun].as_atom()?;
    let function_name = sema.db.lookup_atom(function_atom);

    // Create pattern key and look up in HashMap for O(1) lookup
    let module_str = module_name.as_ref().map(|name| name.as_str());
    let pattern_key = (module_str, function_name.as_str(), args.len());

    let patterns_map = get_dynamic_call_patterns();
    if let Some(pattern) = patterns_map.get(&pattern_key) {
        return resolve_dynamic_call(sema, file_id, pattern, args, body);
    }

    None
}

fn resolve_dynamic_call(
    sema: &Semantic,
    file_id: FileId,
    pattern: &DynamicCallPattern,
    args: &[ExprId],
    body: &Body,
) -> Option<CallDef> {
    // Extract arity based on pattern type
    let arity = if pattern.direct_arity {
        // Extract arity directly from an integer argument
        arity_from_integer_arg(args[pattern.args_list_index], body)?
    } else {
        // Extract arity from list length
        arity_from_apply_args(args[pattern.args_list_index], body)?
    };

    // Build the call target
    let call_target = if let Some(module_idx) = pattern.module_arg_index {
        // Remote call
        CallTarget::Remote {
            module: args[module_idx],
            name: args[pattern.function_arg_index],
            parens: false,
        }
    } else {
        // Local call
        CallTarget::Local {
            name: args[pattern.function_arg_index],
        }
    };

    resolve_call_target(sema, &call_target, Some(arity), file_id, body).map(CallDef::Function)
}

/// The apply call has a last parameter being a list of arguments.
/// Given the `ExprId` of this parameter, return the length of the
/// list.
fn arity_from_apply_args(args: ExprId, body: &Body) -> Option<u32> {
    // Deal with a simple list only.
    body[args].list_length().map(|l| l as u32)
}

/// Extract arity directly from an integer argument.
/// Given the `ExprId` of an integer parameter, return its value as arity.
fn arity_from_integer_arg(arity_arg: ExprId, body: &Body) -> Option<u32> {
    match &body[arity_arg] {
        Expr::Literal(Literal::Integer(int)) => int.value.try_into().ok(),
        _ => None,
    }
}
