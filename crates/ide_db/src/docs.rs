/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This implements the "docs on hover" logic

use std::fmt;
use std::sync::Arc;

use elp_base_db::salsa;
use elp_base_db::FileId;
use elp_base_db::SourceDatabase;
use elp_base_db::SourceDatabaseExt;
use elp_base_db::Upcast;
use elp_erlang_service::DocDiagnostic;
use elp_erlang_service::DocOrigin;
use elp_erlang_service::DocRequest;
use elp_syntax::ast;
use elp_syntax::match_ast;
use elp_syntax::AstNode;
use elp_syntax::SyntaxToken;
use fxhash::FxHashMap;
use hir::db::DefDatabase;
use hir::CallDef;
use hir::InFile;
use hir::Name;
use hir::NameArity;
use hir::Semantic;

pub trait DocLoader {
    /// when origin = eep-48:
    ///   Reads docs from beam files. Supports custom doc setups, e.g. as used by
    ///   OTP, but at the cost of requiring pre-built BEAM files with embedded docs.
    ///
    /// when origin = edoc:
    ///   Reads edocs from comments in source files. Allows for dynamically
    ///   regenerating docs as the files are edited.
    fn load_doc_descriptions(&self, file_id: FileId, origin: DocOrigin) -> FileDoc;
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Doc {
    markdown_text: String,
}

impl fmt::Debug for Doc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("Doc");
        f.field("markdown_text", &self.markdown_text);
        f.finish()
    }
}

impl Doc {
    pub fn markdown_text(&self) -> &str {
        &self.markdown_text
    }

    pub fn new(markdown_text: String) -> Doc {
        Doc { markdown_text }
    }
}

pub trait ToDoc: Clone {
    fn to_doc(docs: &Documentation<'_>, ast: Self) -> Option<Doc>;
}

impl ToDoc for InFile<&ast::ModuleAttribute> {
    fn to_doc(docs: &Documentation<'_>, ast: Self) -> Option<Doc> {
        docs.module_doc(ast.file_id)
    }
}

impl ToDoc for InFile<&ast::BehaviourAttribute> {
    fn to_doc(docs: &Documentation<'_>, ast: Self) -> Option<Doc> {
        let module = docs.sema.to_def(ast)?;
        docs.module_doc(module.file.file_id)
    }
}

impl ToDoc for InFile<&ast::Fa> {
    fn to_doc(docs: &Documentation<'_>, ast: Self) -> Option<Doc> {
        let fa_def = docs.sema.to_def(ast)?;
        let name = match fa_def {
            hir::FaDef::Function(f) | hir::FaDef::FuzzyFunction(f) => Some(f.name),
            hir::FaDef::Type(_) => None,
            hir::FaDef::Callback(c) => Some(c.callback.name),
        }?;
        docs.function_doc(ast.file_id, name)
    }
}

impl ToDoc for InFile<&ast::Spec> {
    fn to_doc(docs: &Documentation<'_>, ast: Self) -> Option<Doc> {
        let fun_def = docs.sema.to_def(ast)?;
        docs.function_doc(ast.file_id, fun_def.name)
    }
}

impl ToDoc for InFile<&ast::Atom> {
    fn to_doc(docs: &Documentation<'_>, ast: Self) -> Option<Doc> {
        docs.sema
            .resolve_module_name(ast.file_id, &ast.value.raw_text())
            .and_then(|module| docs.module_doc(module.file.file_id))
    }
}

impl ToDoc for InFile<&ast::ExternalFun> {
    fn to_doc(docs: &Documentation<'_>, ast: Self) -> Option<Doc> {
        let fun_def = docs.sema.to_def(ast)?;
        docs.function_doc(fun_def.file.file_id, fun_def.name)
    }
}

impl ToDoc for InFile<&ast::Remote> {
    fn to_doc(docs: &Documentation<'_>, ast: Self) -> Option<Doc> {
        if let Some(call_def) = docs.sema.to_def(ast) {
            match call_def {
                CallDef::Function(fun_def) | CallDef::FuzzyFunction(fun_def) => {
                    let file_id = fun_def.file.file_id;
                    let name_arity = fun_def.name;
                    docs.function_doc(file_id, name_arity)
                }
                CallDef::Type(_) => None,
            }
        } else {
            None
        }
    }
}

impl ToDoc for InFile<&ast::Call> {
    fn to_doc(docs: &Documentation<'_>, ast: Self) -> Option<Doc> {
        if let Some(call_def) = docs.sema.to_def(ast) {
            match call_def {
                CallDef::Function(fun_def) | CallDef::FuzzyFunction(fun_def) => {
                    docs.function_doc(fun_def.file.file_id, fun_def.name)
                }
                CallDef::Type(_) => None,
            }
        } else {
            None
        }
    }
}

impl ToDoc for InFile<&ast::FunctionClause> {
    fn to_doc(docs: &Documentation<'_>, ast: Self) -> Option<Doc> {
        let function_id = docs
            .sema
            .find_enclosing_function(ast.file_id, ast.value.syntax())?;
        let function = docs
            .sema
            .function_def(&InFile::new(ast.file_id, function_id))?;
        docs.function_doc(ast.file_id, function.name)
    }
}

// edocs can exist on either a module attribute or a function definition,
// see https://www.erlang.org/doc/apps/edoc/chapter.html#introduction
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FileDoc {
    module_doc: Option<Doc>,
    function_docs: FxHashMap<NameArity, Doc>,
    pub diagnostics: Vec<DocDiagnostic>,
}

// TODO Add an input so we know when to invalidate?
#[salsa::query_group(DocDatabaseStorage)]
pub trait DocDatabase: DefDatabase + SourceDatabase + DocLoader + Upcast<dyn DefDatabase> {
    #[salsa::invoke(get_file_docs)]
    fn file_doc(&self, file_id: FileId) -> Arc<FileDoc>;

    #[salsa::invoke(get_file_specs)]
    fn file_specs(&self, file_id: FileId) -> Arc<FxHashMap<NameArity, Doc>>;
}

/// Primary API to get documentation information from HIR
pub struct Documentation<'db> {
    pub db: &'db dyn DocDatabase,
    pub sema: &'db Semantic<'db>,
}

impl<'db> Documentation<'db> {
    pub fn new<Db: DocDatabase>(db: &'db Db, sema: &'db Semantic) -> Self {
        Self { db, sema }
    }
}

impl<'db> Documentation<'db> {
    pub fn to_doc<T: ToDoc>(&self, ast: T) -> Option<Doc> {
        ToDoc::to_doc(self, ast)
    }

    fn file_doc(&self, file_id: FileId) -> Arc<FileDoc> {
        self.db.file_doc(file_id)
    }

    fn function_doc(&self, file_id: FileId, function: NameArity) -> Option<Doc> {
        let file_docs = self.file_doc(file_id);
        file_docs.function_docs.get(&function).map(|d| d.to_owned())
    }

    fn module_doc(&self, file_id: FileId) -> Option<Doc> {
        let file_docs = self.file_doc(file_id);
        file_docs.module_doc.clone()
    }
}

// Some(true) -> file is in OTP
// Some(false) -> file is not in OTP
// None -> Unknown (e.g. because OTP is not being tracked)
fn is_file_in_otp(db: &dyn DocDatabase, file_id: FileId) -> Option<bool> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nis_file_in_otp: {:?}", file_id));
    let root_id = db.file_source_root(file_id);
    if let Some(app_data) = db.app_data(root_id) {
        let project_id = app_data.project_id;
        Some(db.project_data(project_id).otp_project_id == Some(project_id))
    } else {
        log::error!(
            "Unknown application - could not load app_data to determine whether file is on OTP"
        );
        None
    }
}

fn get_file_docs(db: &dyn DocDatabase, file_id: FileId) -> Arc<FileDoc> {
    let origin = if Some(true) == is_file_in_otp(db, file_id) {
        DocOrigin::Eep48
    } else {
        DocOrigin::Edoc
    };

    let descriptions = db.load_doc_descriptions(file_id, origin);
    let specs = get_file_function_specs(db.upcast(), file_id);
    Arc::new(FileDoc {
        module_doc: descriptions.module_doc,
        function_docs: merge_descriptions_and_specs(descriptions.function_docs, specs),
        diagnostics: descriptions.diagnostics,
    })
}

fn get_file_specs(db: &dyn DocDatabase, file_id: FileId) -> Arc<FxHashMap<NameArity, Doc>> {
    let specs = get_file_function_specs(db.upcast(), file_id);
    Arc::new(specs)
}

fn merge_descriptions_and_specs(
    descriptions: FxHashMap<NameArity, Doc>,
    specs: FxHashMap<NameArity, Doc>,
) -> FxHashMap<NameArity, Doc> {
    let all_keys = descriptions.keys().chain(specs.keys());

    all_keys
        .map(|na| match (descriptions.get(na), specs.get(na)) {
            (Some(desc), Some(spec)) => (
                na.clone(),
                Doc::new(format!(
                    "{}\n\n-----\n\n{}",
                    spec.markdown_text(),
                    desc.markdown_text()
                )),
            ),
            (Some(desc), None) => (na.clone(), desc.clone()),
            (None, Some(spec)) => (na.clone(), spec.clone()),
            (None, None) => (
                na.clone(),
                Doc::new("_No documentation available_".to_string()),
            ),
        })
        .collect::<FxHashMap<NameArity, Doc>>()
}

fn get_file_function_specs(def_db: &dyn DefDatabase, file_id: FileId) -> FxHashMap<NameArity, Doc> {
    def_db
        .file_form_list(file_id)
        .specs()
        .map(|(_, spec)| {
            (
                spec.name.clone(),
                Doc::new(format!(
                    "```erlang\n{}\n```",
                    spec.form_id
                        .get(&def_db.parse(file_id).tree())
                        .syntax()
                        .text()
                )),
            )
        })
        .collect::<FxHashMap<NameArity, Doc>>()
}

impl DocLoader for crate::RootDatabase {
    fn load_doc_descriptions(&self, file_id: FileId, doc_origin: DocOrigin) -> FileDoc {
        _ = SourceDatabaseExt::file_text(self, file_id); // Take dependency on the contents of the file we're getting docs for
        // Context for T171541590
        let _ = stdx::panic_context::enter(format!("\nload_doc_descriptions: {:?}", file_id));
        let root_id = self.file_source_root(file_id);
        let root = self.source_root(root_id);
        let src_db: &dyn SourceDatabase = self.upcast();
        let app_data = if let Some(app_data) = src_db.app_data(root_id) {
            app_data
        } else {
            log::error!("No corresponding appdata found for file, so no docs can be loaded");
            return FileDoc {
                module_doc: None,
                function_docs: FxHashMap::default(),
                diagnostics: vec![],
            };
        };

        let project_id = app_data.project_id;
        let erlang_service = self.erlang_service_for(project_id);
        let path = root.path_for_file(&file_id).unwrap().as_path().unwrap();
        let raw_doc = erlang_service.request_doc(
            DocRequest {
                src_path: path.to_path_buf().into(),
                doc_origin,
            },
            || src_db.unwind_if_cancelled(),
        );
        match raw_doc {
            Ok(d) => FileDoc {
                module_doc: Some(Doc {
                    markdown_text: d.module_doc,
                }),
                function_docs: d
                    .function_docs
                    .into_iter()
                    .map(|((name, arity), markdown_text)| {
                        (
                            NameArity::new(Name::from_erlang_service(&name), arity),
                            Doc { markdown_text },
                        )
                    })
                    .collect(),
                diagnostics: d.diagnostics,
            },
            Err(_) => FileDoc {
                module_doc: None,
                function_docs: FxHashMap::default(),
                diagnostics: vec![],
            },
        }
    }
}

impl Doc {
    /// Returns the Doc for the token, assuming the token
    /// is a reference to a module/function definition e.g.:
    ///   - gets the docs for the f in m:f(a)
    ///   - gets the docs for the m in m:f(a)
    /// If both are available, we pick the more specific docs,
    /// i.e. the docs for the function
    pub fn from_reference(docdb: &Documentation, token: &InFile<SyntaxToken>) -> Option<Self> {
        let wrapper = token.value.parent()?;
        let parent = wrapper.parent()?;
        match_ast! {
            // All places which refer to a function or module name
            match parent {
                ast::ModuleAttribute(module) =>
                    docdb.to_doc(token.with_value(&module)),
                ast::BehaviourAttribute(behaviour) => {
                    let b = token.with_value(&behaviour);
                    docdb.to_doc(b)
                },
                ast::ImportAttribute(_) => None,
                ast::Fa(fa) =>
                    docdb.to_doc(token.with_value(&fa)),
                ast::TypeName(_) => None,
                ast::RecordDecl(_) => None,
                ast::Spec(spec) =>
                    docdb.to_doc(token.with_value(&spec)),
                ast::Callback(_) => None,
                ast::Module(_) => {
                    if let Some(atom) = ast::Atom::cast(wrapper) {
                        docdb.to_doc(token.with_value(&atom))
                    } else {
                        None
                    }
                },
                ast::AttrName(_) => None,
                ast::FunctionClause(clause) =>
                    docdb.to_doc(token.with_value(&clause)),
                ast::BitTypeList(_) => None,
                ast::RecordName(_) => None,
                ast::RecordFieldName(_) => None,
                ast::RecordField(_) => None,
                ast::InternalFun(_) => None,
                ast::ExternalFun(fun) =>
                    docdb.to_doc(token.with_value(&fun)),
                ast::TryClass(_) => None,
                // All places that embed an expr with special meaning
                ast::RemoteModule(_) => {
                    if let Some(atom) = ast::Atom::cast(wrapper) {
                        docdb.to_doc(token.with_value(&atom))
                    } else {
                        None
                    }
                },
                ast::Remote(remote) =>
                    docdb.to_doc(token.with_value(&remote)),
                ast::Call(call) =>
                    docdb.to_doc(token.with_value(&call)),
                _ => {
                    // Parent is nothing structured, it must be a raw atom or var literal
                    match_ast! {
                        match wrapper {
                            ast::Atom(atom) =>
                                docdb.to_doc(token.with_value(&atom)),
                            _ => {
                                None
                            }
                        }
                    }
                }
            }
        }
    }
}
