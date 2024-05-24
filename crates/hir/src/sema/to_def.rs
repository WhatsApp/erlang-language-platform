/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_base_db::FileId;
use elp_base_db::FileKind;
use elp_syntax::ast;
use elp_syntax::match_ast;
use elp_syntax::AstNode;

use crate::known;
use crate::macro_exp;
use crate::macro_exp::BuiltInMacro;
use crate::macro_exp::MacroExpCtx;
use crate::resolver::Resolver;
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
        resolve_testcase(sema, file_id, &name)
            .map(AtomDef::Function)
            .or_else(|| resolve_module_name(sema, file_id, &name).map(AtomDef::Module))
    }
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
    Type(TypeAliasDef),
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
                resolve_call_target(sema, target, arity, file_id, &body).map(CallDef::Function)
            }
            AnyExprRef::TypeExpr(TypeExpr::Call { target, args }) => {
                let arity = args.len().try_into().ok()?;
                resolve_type_target(sema, target, arity, file_id, &body).map(CallDef::Type)
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
    Type(TypeAliasDef),
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
                _ => return None,
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

pub(crate) fn resolve_module_expr(
    sema: &Semantic<'_>,
    body: &Body,
    file_id: FileId,
    expr_id: ExprId,
) -> Option<Module> {
    let name = sema.db.lookup_atom(body[expr_id].as_atom()?);
    resolve_module_name(sema, file_id, &name)
}

pub fn resolve_module_name(sema: &Semantic<'_>, file_id: FileId, name: &str) -> Option<Module> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nresolve_module_name: {:?}", file_id));
    let source_root_id = sema.db.file_source_root(file_id);
    let project_id = sema.db.app_data(source_root_id)?.project_id;
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
    arity: u32,
    file_id: FileId,
    body: &Body,
) -> Option<FunctionDef> {
    let (file_id, fun_expr) = match target {
        CallTarget::Local { name } => (file_id, *name),
        CallTarget::Remote { module, name } => (
            resolve_module_expr(sema, body, file_id, *module)?
                .file
                .file_id,
            *name,
        ),
    };

    let name = sema.db.lookup_atom(body[fun_expr].as_atom()?);
    let name_arity = NameArity::new(name, arity);
    sema.db.def_map(file_id).get_function(&name_arity).cloned()
}

pub fn resolve_type_target(
    sema: &Semantic<'_>,
    target: &CallTarget<TypeExprId>,
    arity: u32,
    file_id: FileId,
    body: &Body,
) -> Option<TypeAliasDef> {
    let (file_id, type_expr) = match target {
        CallTarget::Local { name } => (file_id, *name),
        CallTarget::Remote { module, name } => {
            let module = sema.db.lookup_atom(body[*module].as_atom()?);
            (
                resolve_module_name(sema, file_id, &module)?.file.file_id,
                *name,
            )
        }
    };

    let name = sema.db.lookup_atom(body[type_expr].as_atom()?);
    let name = NameArity::new(name, arity);
    sema.db.def_map(file_id).get_types().get(&name).cloned()
}

fn resolve_capture(sema: &Semantic<'_>, fun: InFile<ast::Expr>) -> Option<FunctionDef> {
    let (body, body_map) = sema.find_body_and_map(fun.file_id, fun.value.syntax())?;
    let expr_id = body_map.expr_id(fun.as_ref())?;
    let (target, arity) = match &body[expr_id] {
        Expr::CaptureFun { target, arity } => (target, arity),
        _ => return None,
    };
    let arity = match body[*arity] {
        Expr::Literal(Literal::Integer(int)) => int.try_into().ok()?,
        _ => return None,
    };
    resolve_call_target(sema, target, arity, fun.file_id, &body)
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

    /// Looking specifically to pull out a function definition from an
    /// `apply/2` or `apply/3` call.
    fn to_def(sema: &Semantic<'_>, ast: InFile<&Self>) -> Option<Self::Def> {
        let (body, body_map) = sema.find_body_and_map(ast.file_id, ast.value.syntax())?;
        let call = ast::Expr::cast(ast.value.syntax().parent()?.clone())?;
        let expr = ast.with_value(call);
        let expr_id = body_map.expr_id(expr.as_ref())?;
        let def = match &body[expr_id] {
            Expr::Call { target, args } => match target {
                CallTarget::Local { name } => {
                    look_for_apply_call(sema, ast.file_id, None, *name, args, &body)
                }
                CallTarget::Remote { module, name } => {
                    look_for_apply_call(sema, ast.file_id, Some(*module), *name, args, &body)
                }
            },
            _ => None,
        }?;
        Some(def)
    }
}

fn look_for_apply_call(
    sema: &Semantic,
    file_id: FileId,
    module: Option<ExprId>,
    fun: ExprId,
    args: &[ExprId],
    body: &Body,
) -> Option<CallDef> {
    if let Some(module) = module {
        let atom = body[module].as_atom()?;
        if sema.db.lookup_atom(atom) != known::erlang {
            return None;
        }
    };
    let atom = body[fun].as_atom()?;
    if sema.db.lookup_atom(atom) == known::apply {
        if args.len() == 2 {
            // apply/2
            let arity = arity_from_apply_args(args[1], body)?;
            let apply_target = CallTarget::Local { name: args[0] };
            resolve_call_target(sema, &apply_target, arity, file_id, body).map(CallDef::Function)
        } else if args.len() == 3 {
            // apply/3
            let arity = arity_from_apply_args(args[2], body)?;
            let apply_target = CallTarget::Remote {
                module: args[0],
                name: args[1],
            };
            resolve_call_target(sema, &apply_target, arity, file_id, body).map(CallDef::Function)
        } else {
            None
        }
    } else {
        None
    }
}

/// The apply call has a last parameter being a list of arguments.
/// Given the `ExprId` of this parameter, return the length of the
/// list.
fn arity_from_apply_args(args: ExprId, body: &Body) -> Option<u32> {
    // Deal with a simple list only.
    body[args].list_length().map(|l| l as u32)
}
