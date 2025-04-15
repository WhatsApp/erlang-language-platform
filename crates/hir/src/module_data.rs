/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use elp_base_db::FileId;
use elp_base_db::FileKind;
use elp_base_db::ModuleName;
use elp_base_db::SourceDatabase;
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::SmolStr;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use elp_syntax::ast;
use elp_syntax::ast::MapExpr;
use fxhash::FxHashMap;
use lazy_static::lazy_static;
use regex::Regex;

use crate::Callback;
use crate::DefMap;
use crate::Define;
use crate::FormIdx;
use crate::FunctionClause;
use crate::FunctionClauseId;
use crate::InFile;
use crate::InFileAstPtr;
use crate::InFunctionBody;
use crate::InFunctionClauseBody;
use crate::ModuleAttribute;
use crate::Name;
use crate::NameArity;
use crate::Record;
use crate::RecordField;
use crate::Semantic;
use crate::Spec;
use crate::SpecId;
use crate::TypeAlias;
use crate::Var;
use crate::code_complexity;
use crate::code_complexity::CodeComplexity;
use crate::db::DefDatabase;
use crate::db::InternDatabase;
use crate::def_map::FunctionDefId;
use crate::edoc::EdocHeader;
use crate::form_list::DeprecatedDesc;
use crate::form_list::DocAttributeId;

/// Represents an erlang file - header or module
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct File {
    pub file_id: FileId,
}

impl File {
    pub fn source(&self, db: &dyn SourceDatabase) -> ast::SourceFile {
        db.parse(self.file_id).tree()
    }

    pub fn kind(&self, db: &dyn SourceDatabase) -> FileKind {
        db.file_kind(self.file_id)
    }

    pub fn name(&self, db: &dyn SourceDatabase) -> SmolStr {
        // Context for T171541590
        let _ = stdx::panic_context::enter(format!("\nFile::name: {:?}", self.file_id));
        let source_root = db.source_root(db.file_source_root(self.file_id));
        if let Some((name, Some(ext))) = source_root
            .path_for_file(&self.file_id)
            .and_then(|path| path.name_and_extension())
        {
            SmolStr::new(format!("{}.{}", name, ext))
        } else {
            SmolStr::new_inline("unknown")
        }
    }

    pub fn def_map(&self, db: &dyn DefDatabase) -> Arc<DefMap> {
        db.def_map(self.file_id)
    }
}

/// Represents a module definition
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Module {
    pub file: File,
}

impl Module {
    pub fn module_attribute(&self, db: &dyn DefDatabase) -> Option<ModuleAttribute> {
        let forms = db.file_form_list(self.file.file_id);
        forms.module_attribute().cloned()
    }

    pub fn name(&self, db: &dyn DefDatabase) -> Name {
        let attr = self.module_attribute(db);
        attr.map_or(Name::MISSING, |attr| attr.name)
    }

    pub fn is_in_otp(&self, db: &dyn DefDatabase) -> bool {
        is_in_otp(self.file.file_id, db)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FunctionClauseDef {
    pub file: File,
    pub module: Option<ModuleName>,
    pub function_clause: FunctionClause,
    pub function_clause_id: FunctionClauseId,
    pub doc_id: Option<DocAttributeId>,
}

impl FunctionClauseDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> ast::FunDecl {
        let source_file = self.file.source(db);
        self.function_clause.form_id.get(&source_file)
    }

    pub fn in_clause<'a, T>(
        &self,
        sema: &'a Semantic<'a>,
        value: T,
    ) -> crate::InFunctionClauseBody<'a, T>
    where
        T: Clone,
    {
        let function_clause_body = sema.db.function_clause_body(InFile::new(
            self.file.file_id,
            self.function_clause_id.clone(),
        ));
        InFunctionClauseBody::new(
            sema,
            function_clause_body,
            InFile::new(self.file.file_id, self.function_clause_id.clone()),
            None,
            value,
        )
    }

    pub fn form_id(&self) -> FormIdx {
        FormIdx::FunctionClause(self.function_clause_id)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FunctionDef {
    pub file: File,
    pub exported: bool,
    pub deprecated: bool,
    pub deprecated_desc: Option<DeprecatedDesc>,
    pub module: Option<ModuleName>,
    pub name: NameArity,
    pub function_clauses: Vec<FunctionClause>,
    pub function_clause_ids: Vec<FunctionClauseId>,
    pub function_id: FunctionDefId,
    pub spec: Option<SpecDef>,
    pub doc_id: Option<DocAttributeId>,
}

impl FunctionDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> Vec<ast::FunDecl> {
        let source_file = self.file.source(db);
        self.function_clauses
            .iter()
            .map(|f| f.form_id.get(&source_file))
            .collect()
    }

    pub fn first_clause_name(&self, db: &dyn SourceDatabase) -> Option<ast::Name> {
        self.source(db).get(0)?.name()
    }

    pub fn range(&self, db: &dyn SourceDatabase) -> Option<TextRange> {
        let sources = self.source(db);
        let start = sources.first()?;
        let end = sources.last()?;
        Some(start.syntax().text_range().cover(end.syntax().text_range()))
    }

    pub fn name_range(&self, db: &dyn DefDatabase) -> Option<TextRange> {
        let range = self
            .function_clauses
            .first()?
            .form_id
            .get_ast(db, self.file.file_id)
            .name()?
            .syntax()
            .text_range();
        Some(range)
    }

    pub fn in_function_body<'a, T>(
        &self,
        sema: &'a Semantic<'a>,
        value: T,
    ) -> crate::InFunctionBody<'a, T>
    where
        T: Clone,
    {
        let function_body = sema
            .db
            .function_body(InFile::new(self.file.file_id, self.function_id.clone()));
        InFunctionBody::new(
            sema,
            function_body,
            InFile::new(self.file.file_id, self.function_id.clone()),
            value,
        )
    }

    pub fn is_in_otp(&self, db: &dyn DefDatabase) -> bool {
        is_in_otp(self.file.file_id, db)
    }

    pub fn edoc_comments(&self, db: &dyn DefDatabase) -> Option<EdocHeader> {
        let fun_decls = self.source(db.upcast());
        let fun_decl = fun_decls.get(0)?;
        let form = InFileAstPtr::new(
            self.file.file_id,
            AstPtr::new(&ast::Form::FunDecl(fun_decl.clone())),
        );
        let file_edoc = db.file_edoc_comments(form.file_id())?;
        file_edoc.get(&form).cloned()
    }

    pub fn first_clause_arg_names(&self) -> Option<Vec<String>> {
        let res = self
            .function_clauses
            .get(0)?
            .param_names
            .iter()
            .map(|param_name| param_name.to_string())
            .collect();
        Some(res)
    }

    pub fn arg_names(&self, db: &dyn SourceDatabase) -> Option<Vec<String>> {
        match &self.spec {
            Some(spec_def) => match spec_def.arg_names(db) {
                Some(arg_names_from_spec) => {
                    if all_spec_arg_names_are_generated(&arg_names_from_spec) {
                        self.first_clause_arg_names()
                    } else {
                        Some(
                            arg_names_from_spec
                                .iter()
                                .map(|arg_name| arg_name.name())
                                .collect(),
                        )
                    }
                }
                None => self.first_clause_arg_names(),
            },
            None => self.first_clause_arg_names(),
        }
    }

    pub fn get_parameters_doc(&self, db: &dyn DefDatabase) -> FxHashMap<String, String> {
        let parameters = self.parameters_doc_from_doc_attribute(db);
        if !parameters.is_empty() {
            parameters
        } else {
            self.parameters_doc_from_edoc(db)
        }
    }

    fn parameters_doc_from_edoc(&self, db: &dyn DefDatabase) -> FxHashMap<String, String> {
        match self.edoc_comments(db) {
            Some(edoc_header) => edoc_header
                .params
                .into_iter()
                .map(|(name, param)| (name, param.description()))
                .collect(),
            None => FxHashMap::default(),
        }
    }

    fn parameters_doc_from_doc_attribute(&self, db: &dyn DefDatabase) -> FxHashMap<String, String> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^\s*@param ([^\s]+)\s+(.*)$").unwrap();
        }
        let mut res = FxHashMap::default();
        if let Some(doc_id) = self.doc_id {
            let file_id = self.file.file_id;
            let form_list = db.file_form_list(file_id);
            let doc = &form_list[doc_id];
            let ast = doc.form_id.get_ast(db, file_id);
            if let Some(expr) = ast.value() {
                let text = expr.syntax().text().to_string();
                for line in text.split("\n") {
                    if let Some(captures) = RE.captures(&line) {
                        if captures.len() == 3 {
                            res.insert(captures[1].to_string(), captures[2].to_string());
                        }
                    }
                }
            };
        }
        res
    }

    pub fn code_complexity(&self, sema: &Semantic, score_cap: Option<usize>) -> CodeComplexity {
        code_complexity::compute(sema, self, score_cap)
    }
}

fn all_spec_arg_names_are_generated(names: &Vec<SpecArgName>) -> bool {
    !names.iter().any(|name| match name {
        SpecArgName::Name(_) => true,
        SpecArgName::Generated(_) => false,
    })
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SpecDef {
    pub file: File,
    pub spec: Spec,
    pub spec_id: SpecId,
    pub function: Option<FunctionDefId>,
}

impl SpecDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> ast::Spec {
        let source_file = self.file.source(db);
        self.spec.form_id.get(&source_file)
    }

    pub fn arg_names(&self, db: &dyn SourceDatabase) -> Option<Vec<SpecArgName>> {
        let spec = self.source(db);
        let first_sig = spec.sigs().next()?;
        Some(
            first_sig
                .args()?
                .args()
                .enumerate()
                .map(|(arg_idx, expr)| arg_name(arg_idx + 1, expr))
                .collect(),
        )
    }
}

pub enum SpecArgName {
    Name(String),
    Generated(String),
}

impl SpecArgName {
    pub fn name(&self) -> String {
        match self {
            SpecArgName::Name(name) => name.clone(),
            SpecArgName::Generated(name) => name.clone(),
        }
    }
}

fn arg_name(arg_idx: usize, expr: ast::Expr) -> SpecArgName {
    // -spec f(A) -> ok.
    //   f(A) -> ok.
    if let ast::Expr::ExprMax(ast::ExprMax::Var(var)) = expr {
        SpecArgName::Name(var.text().to_string())

    // -spec f(A :: foo()) -> ok.
    //   f(A) -> ok.
    } else if let ast::Expr::AnnType(ann) = expr {
        ann.var()
            .and_then(|var| var.var())
            .map(|var| SpecArgName::Name(var.text().to_string()))
            .unwrap_or_else(|| SpecArgName::Generated(format!("Arg{}", arg_idx)))

    // -spec f(bar()) -> ok.
    //   f(Arg1) -> ok.
    } else {
        SpecArgName::Generated(format!("Arg{}", arg_idx))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RecordDef {
    pub file: File,
    pub record: Record,
}

impl RecordDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> ast::RecordDecl {
        let source_file = self.file.source(db);
        self.record.form_id.get(&source_file)
    }

    pub fn range(&self, db: &dyn SourceDatabase) -> TextRange {
        let source = self.source(db);
        source.syntax().text_range()
    }

    pub fn fields(
        &self,
        db: &dyn DefDatabase,
    ) -> impl Iterator<Item = (Name, RecordFieldDef)> + '_ {
        let forms = db.file_form_list(self.file.file_id);
        self.record.fields.clone().map(move |f| {
            (
                forms[f].name.clone(),
                RecordFieldDef {
                    record: self.clone(),
                    field: forms[f].clone(),
                },
            )
        })
    }

    pub fn field_names(&self, db: &dyn DefDatabase) -> impl Iterator<Item = Name> {
        let forms = db.file_form_list(self.file.file_id);
        self.record
            .fields
            .clone()
            .map(move |f| forms[f].name.clone())
    }

    pub fn find_field_by_id(&self, db: &dyn DefDatabase, id: usize) -> Option<RecordFieldDef> {
        let forms = db.file_form_list(self.file.file_id);
        let field = self.record.fields.clone().nth(id)?;
        Some(RecordFieldDef {
            record: self.clone(),
            field: forms[field].clone(),
        })
    }

    pub fn find_field(&self, db: &dyn DefDatabase, name: &Name) -> Option<RecordFieldDef> {
        let forms = db.file_form_list(self.file.file_id);
        let field = self
            .record
            .fields
            .clone()
            .find(|&field| &forms[field].name == name)?;
        Some(RecordFieldDef {
            record: self.clone(),
            field: forms[field].clone(),
        })
    }
}

/// Represents a record field definition in a particular record
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RecordFieldDef {
    pub record: RecordDef,
    pub field: RecordField,
}

impl RecordFieldDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> ast::RecordField {
        let record = self.record.source(db);
        record.fields().nth(self.field.idx as usize).unwrap()
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypeAliasDef {
    pub file: File,
    pub exported: bool,
    pub type_alias: TypeAlias,
}

pub enum TypeAliasSource {
    Regular(ast::TypeAlias),
    Nominal(ast::Nominal),
    Opaque(ast::Opaque),
}

impl TypeAliasDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> TypeAliasSource {
        let source_file = self.file.source(db);
        match self.type_alias {
            TypeAlias::Opaque { form_id, .. } => TypeAliasSource::Opaque(form_id.get(&source_file)),
            TypeAlias::Regular { form_id, .. } => {
                TypeAliasSource::Regular(form_id.get(&source_file))
            }
            TypeAlias::Nominal { form_id, .. } => {
                TypeAliasSource::Nominal(form_id.get(&source_file))
            }
        }
    }

    pub fn name(&self) -> &NameArity {
        match &self.type_alias {
            TypeAlias::Regular { name, .. } => name,
            TypeAlias::Nominal { name, .. } => name,
            TypeAlias::Opaque { name, .. } => name,
        }
    }

    pub fn range(&self, db: &dyn SourceDatabase) -> Option<TextRange> {
        let source = self.source(db);
        Some(source.syntax().text_range())
    }

    /// This information is used for completion.
    /// We deliberately return nothing for an opaque type
    pub fn map_expr_for_completion(&self, db: &dyn SourceDatabase) -> Option<MapExpr> {
        let source = self.source(db);
        match source {
            TypeAliasSource::Regular(alias) => match alias.ty()? {
                ast::Expr::MapExpr(expr) => Some(expr),
                _ => None,
            },
            TypeAliasSource::Nominal(nominal_type) => match nominal_type.ty()? {
                ast::Expr::MapExpr(expr) => Some(expr),
                _ => None,
            },
            TypeAliasSource::Opaque(_) => None,
        }
    }
}

impl TypeAliasSource {
    pub fn syntax(&self) -> &SyntaxNode {
        match self {
            TypeAliasSource::Regular(type_alias) => type_alias.syntax(),
            TypeAliasSource::Nominal(nominal_type) => nominal_type.syntax(),
            TypeAliasSource::Opaque(opaque) => opaque.syntax(),
        }
    }

    pub fn type_name(&self) -> Option<ast::TypeName> {
        match self {
            TypeAliasSource::Regular(type_alias) => type_alias.name(),
            TypeAliasSource::Nominal(nominal_type) => nominal_type.name(),
            TypeAliasSource::Opaque(opaque) => opaque.name(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CallbackDef {
    pub file: File,
    pub optional: bool,
    pub callback: Callback,
}

impl CallbackDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> ast::Callback {
        let source_file = self.file.source(db);
        self.callback.form_id.get(&source_file)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DefineDef {
    pub file: File,
    pub define: Define,
}

impl DefineDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> ast::PpDefine {
        let source_file = self.file.source(db);
        self.define.form_id.get(&source_file)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VarDef {
    pub file: File,
    // Restrict access to the crate only, so we can ensure it is
    // reconstituted against the correct source.
    pub var: AstPtr<ast::Var>,
    pub hir_var: Var,
}

impl VarDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> ast::Var {
        let source_file = self.file.source(db);
        self.var.to_node(source_file.syntax())
    }

    pub fn name(&self, db: &dyn InternDatabase) -> Name {
        db.lookup_var(self.hir_var).clone()
    }
}

fn is_in_otp(file_id: FileId, db: &dyn DefDatabase) -> bool {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nis_in_otp:2: {:?}", file_id));
    match db.file_app_data(file_id) {
        Some(app_data) => {
            let project_id = app_data.project_id;
            db.project_data(project_id).otp_project_id == Some(project_id)
        }
        None => false,
    }
}
