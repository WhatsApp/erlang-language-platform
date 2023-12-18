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
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::SmolStr;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;

use crate::db::MinDefDatabase;
use crate::db::MinInternDatabase;
use crate::def_map::FunctionDefId;
use crate::edoc::EdocHeader;
use crate::form_list::DeprecatedDesc;
use crate::Callback;
use crate::DefMap;
use crate::Define;
use crate::FormIdx;
use crate::Function;
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
use crate::Spec;
use crate::SpecId;
use crate::TypeAlias;
use crate::Var;

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

    pub fn def_map(&self, db: &dyn MinDefDatabase) -> Arc<DefMap> {
        db.def_map(self.file_id)
    }
}

/// Represents a module definition
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Module {
    pub file: File,
}

impl Module {
    pub fn module_attribute(&self, db: &dyn MinDefDatabase) -> Option<ModuleAttribute> {
        let forms = db.file_form_list(self.file.file_id);
        forms.module_attribute().cloned()
    }

    pub fn name(&self, db: &dyn MinDefDatabase) -> Name {
        let attr = self.module_attribute(db);
        attr.map_or(Name::MISSING, |attr| attr.name)
    }

    pub fn is_in_otp(&self, db: &dyn MinDefDatabase) -> bool {
        is_in_otp(self.file.file_id, db)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FunctionClauseDef {
    pub file: File,
    pub module: Option<ModuleName>,
    pub function: Function,
    pub function_clause_id: FunctionClauseId,
}

impl FunctionClauseDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> ast::FunDecl {
        let source_file = self.file.source(db);
        self.function.form_id.get(&source_file)
    }

    pub fn in_clause<T>(&self, db: &dyn MinDefDatabase, value: T) -> crate::InFunctionClauseBody<T>
    where
        T: Clone,
    {
        let function_clause_body = db.function_clause_body(InFile::new(
            self.file.file_id,
            self.function_clause_id.clone(),
        ));
        InFunctionClauseBody::new(
            function_clause_body,
            InFile::new(self.file.file_id, self.function_clause_id.clone()),
            None,
            value,
        )
    }

    pub fn form_id(&self) -> FormIdx {
        FormIdx::Function(self.function_clause_id)
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
    pub function: Vec<Function>,
    pub function_clause_ids: Vec<FunctionClauseId>,
    pub function_id: FunctionDefId,
}

impl FunctionDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> Vec<ast::FunDecl> {
        let source_file = self.file.source(db);
        self.function
            .iter()
            .map(|f| f.form_id.get(&source_file))
            .collect()
    }

    pub fn range(&self, db: &dyn SourceDatabase) -> Option<TextRange> {
        let sources = self.source(db);
        let start = sources.first()?;
        let end = sources.last()?;
        Some(start.syntax().text_range().cover(end.syntax().text_range()))
    }

    pub fn in_function_body<T>(&self, db: &dyn MinDefDatabase, value: T) -> crate::InFunctionBody<T>
    where
        T: Clone,
    {
        let function_body =
            db.function_body(InFile::new(self.file.file_id, self.function_id.clone()));
        InFunctionBody::new(
            function_body,
            InFile::new(self.file.file_id, self.function_id.clone()),
            value,
        )
    }

    pub fn is_in_otp(&self, db: &dyn MinDefDatabase) -> bool {
        is_in_otp(self.file.file_id, db)
    }

    pub fn edoc_comments(&self, db: &dyn MinDefDatabase) -> Option<EdocHeader> {
        let fun_decls = self.source(db.upcast());
        let fun_decl = fun_decls.get(0)?;
        let form = InFileAstPtr::new(
            self.file.file_id,
            AstPtr::new(&ast::Form::FunDecl(fun_decl.clone())),
        );
        let file_edoc = db.file_edoc_comments(form.file_id())?;
        file_edoc.get(&form).cloned()
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SpecDef {
    pub file: File,
    pub spec: Spec,
    pub spec_id: SpecId,
}

impl SpecDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> ast::Spec {
        let source_file = self.file.source(db);
        self.spec.form_id.get(&source_file)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SpecdFunctionDef {
    pub spec_def: SpecDef,
    pub function_def: FunctionDef,
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

    pub fn fields(
        &self,
        db: &dyn MinDefDatabase,
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

    pub fn field_names(&self, db: &dyn MinDefDatabase) -> impl Iterator<Item = Name> {
        let forms = db.file_form_list(self.file.file_id);
        self.record
            .fields
            .clone()
            .map(move |f| forms[f].name.clone())
    }

    pub fn find_field_by_id(&self, db: &dyn MinDefDatabase, id: usize) -> Option<RecordFieldDef> {
        let forms = db.file_form_list(self.file.file_id);
        let field = self.record.fields.clone().nth(id)?;
        Some(RecordFieldDef {
            record: self.clone(),
            field: forms[field].clone(),
        })
    }

    pub fn find_field(&self, db: &dyn MinDefDatabase, name: &Name) -> Option<RecordFieldDef> {
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
        }
    }

    pub fn name(&self) -> &NameArity {
        match &self.type_alias {
            TypeAlias::Regular { name, .. } => name,
            TypeAlias::Opaque { name, .. } => name,
        }
    }
}

impl TypeAliasSource {
    pub fn syntax(&self) -> &SyntaxNode {
        match self {
            TypeAliasSource::Regular(type_alias) => type_alias.syntax(),
            TypeAliasSource::Opaque(opaque) => opaque.syntax(),
        }
    }

    pub fn type_name(&self) -> Option<ast::TypeName> {
        match self {
            TypeAliasSource::Regular(type_alias) => type_alias.name(),
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
    pub(crate) var: AstPtr<ast::Var>,
    pub hir_var: Var,
}

impl VarDef {
    pub fn source(&self, db: &dyn SourceDatabase) -> ast::Var {
        let source_file = self.file.source(db);
        self.var.to_node(source_file.syntax())
    }

    pub fn name(&self, db: &dyn MinInternDatabase) -> Name {
        db.lookup_var(self.hir_var).clone()
    }
}

fn is_in_otp(file_id: FileId, db: &dyn MinDefDatabase) -> bool {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nis_in_otp:2: {:?}", file_id));
    let source_root_id = db.file_source_root(file_id);
    match db.app_data(source_root_id) {
        Some(app_data) => {
            let project_id = app_data.project_id;
            db.project_data(project_id).otp_project_id == Some(project_id)
        }
        None => false,
    }
}
