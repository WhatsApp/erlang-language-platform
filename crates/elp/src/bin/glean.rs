/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;
use std::mem;
use std::path::Path;

use anyhow::Result;
use elp::build::load;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp_eqwalizer::ast::Pos;
use elp_eqwalizer::EqwalizerDiagnostics;
use elp_ide::elp_ide_db::docs::DocDatabase;
use elp_ide::elp_ide_db::elp_base_db::module_name;
use elp_ide::elp_ide_db::elp_base_db::path_for_file;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::ModuleName;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_ide::elp_ide_db::EqwalizerDatabase;
use elp_ide::elp_ide_db::LineIndexDatabase;
use elp_ide::elp_ide_db::RootDatabase;
// @fb-only
use elp_ide::Analysis;
use elp_ide::TextRange;
use elp_project_model::buck::BuckQueryConfig;
use elp_project_model::AppType;
use elp_project_model::DiscoverConfig;
use elp_syntax::ast;
use elp_syntax::ast::DeprecatedFa;
use elp_syntax::ast::ExprMax;
use elp_syntax::ast::Fa;
use elp_syntax::ast::HasArity;
use elp_syntax::AstNode;
use fxhash::FxHashMap;
use hir::db::DefDatabase;
use hir::fold;
use hir::fold::AnyCallBackCtx;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::sema::to_def::resolve_call_target;
use hir::sema::to_def::resolve_type_target;
use hir::AsName;
use hir::Body;
use hir::CallTarget;
use hir::DefMap;
use hir::DefineId;
use hir::Expr;
use hir::ExprId;
use hir::ExprSource;
use hir::InFile;
use hir::Literal;
use hir::MacroName;
use hir::Name;
use hir::NameArity;
use hir::PPDirective;
use hir::Pat;
use hir::Semantic;
use hir::Strategy;
use hir::Term;
use hir::TypeExpr;
use hir::TypeExprId;
use itertools::Itertools;
use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;
use serde::Serialize;

use crate::args::Glean;

const REC_ARITY: u32 = 99;
const HEADER_ARITY: u32 = 100;
const FACTS_FILE: &str = "facts.json";

#[derive(Serialize, Debug, Eq, Hash, PartialEq, Clone)]
struct GleanFileId(u32);

#[derive(Clone, Debug, Default)]
struct IndexConfig {
    pub multi: bool,
    pub prefix: Option<String>,
}

impl Into<FileId> for GleanFileId {
    fn into(self) -> FileId {
        FileId::from_raw(self.0 - 1)
    }
}

impl From<FileId> for GleanFileId {
    fn from(value: FileId) -> Self {
        GleanFileId(value.index() + 1)
    }
}

#[derive(Serialize, Debug)]
pub(crate) struct FileFact {
    #[serde(rename = "id")]
    file_id: GleanFileId,
    #[serde(rename = "key")]
    file_path: String,
}

impl FileFact {
    fn new(file_id: FileId, file_path: String) -> Self {
        Self {
            file_id: file_id.into(),
            file_path,
        }
    }
}

#[derive(Serialize, Debug)]
pub(crate) struct FileLinesFact {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    lengths: Vec<u32>,
    #[serde(rename = "endsInNewline")]
    ends_with_new_line: bool,
    #[serde(rename = "hasUnicodeOrTabs")]
    unicode_or_tabs: bool,
}

impl FileLinesFact {
    fn new(file_id: FileId, lengths: Vec<u32>, ends_with_new_line: bool) -> Self {
        Self {
            file_id: file_id.into(),
            lengths,
            ends_with_new_line,
            unicode_or_tabs: true,
        }
    }
}

#[derive(Serialize, Debug)]
pub(crate) struct FunctionDeclarationFact {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    fqn: MFA,
    span: Location,
}

impl FunctionDeclarationFact {
    fn new(file_id: FileId, fqn: MFA, span: Location) -> Self {
        Self {
            file_id: file_id.into(),
            fqn,
            span,
        }
    }
}

#[derive(Serialize, Debug)]
pub(crate) struct XRefFact {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    xrefs: Vec<XRefFactVal>,
}

impl XRefFact {
    fn new(file_id: FileId, xrefs: Vec<XRefFactVal>) -> Self {
        Self {
            file_id: file_id.into(),
            xrefs,
        }
    }
}

#[derive(Serialize, Debug)]
struct XRefFactVal {
    source: Location,
    target: MFA,
}

impl XRefFactVal {
    fn new(source: Location, target: MFA) -> Self {
        Self { source, target }
    }
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
struct MFA {
    module: String,
    name: String,
    arity: u32,
    #[serde(skip_serializing)]
    file_id: GleanFileId,
}

impl MFA {
    fn new(module: &ModuleName, name: &Name, arity: u32, file_id: FileId) -> Self {
        Self {
            module: module.to_string(),
            name: name.to_string(),
            arity,
            file_id: file_id.into(),
        }
    }
}

#[derive(Serialize, Debug, Clone, Hash, PartialEq, Eq)]
struct Location {
    start: u32,
    length: u32,
}

impl From<Location> for TextRange {
    fn from(val: Location) -> Self {
        TextRange::at(val.start.into(), val.length.into())
    }
}

#[derive(Serialize, Debug)]
#[serde(tag = "predicate")]
pub(crate) enum Fact {
    #[serde(rename = "src.File")]
    File { facts: Vec<FileFact> },
    #[serde(rename = "src.FileLines")]
    FileLine { facts: Vec<Key<FileLinesFact>> },
    #[serde(rename = "erlang.FunctionDeclaration")]
    FunctionDeclaration {
        facts: Vec<Key<FunctionDeclarationFact>>,
    },
    #[serde(rename = "erlang.XRefsViaFqnByFile")]
    XRef { facts: Vec<Key<XRefFact>> },
    #[serde(rename = "erlang.DeclarationComment")]
    DeclarationComment { facts: Vec<Key<CommentFact>> },
}

#[derive(Serialize, Debug)]
pub(crate) struct XRefFile {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    xrefs: Vec<XRef>,
}

#[derive(Serialize, Debug)]
pub(crate) struct XRef {
    source: Location,
    target: XRefTarget,
}

#[derive(Serialize, Debug)]
pub(crate) enum XRefTarget {
    #[serde(rename = "func")]
    Function(Key<FunctionTarget>),
    #[serde(rename = "macro")]
    Macro(Key<MacroTarget>),
    #[serde(rename = "header")]
    Header(Key<HeaderTarget>),
    #[serde(rename = "record")]
    Record(Key<RecordTarget>),
    #[serde(rename = "ttype")]
    Type(Key<TypeTarget>),
    #[serde(rename = "varr")]
    Var(Key<VarTarget>),
}

impl XRefTarget {
    fn file_id(&self) -> &GleanFileId {
        match self {
            XRefTarget::Function(xref) => &xref.key.file_id,
            XRefTarget::Macro(xref) => &xref.key.file_id,
            XRefTarget::Header(xref) => &xref.key.file_id,
            XRefTarget::Record(xref) => &xref.key.file_id,
            XRefTarget::Type(xref) => &xref.key.file_id,
            XRefTarget::Var(xref) => &xref.key.file_id,
        }
    }
}

#[derive(Serialize, Debug)]
pub(crate) struct FunctionTarget {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    name: String,
    arity: u32,
}

#[derive(Serialize, Debug)]
pub(crate) struct MacroTarget {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    arity: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    expansion: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    ods_url: Option<String>,
}

#[derive(Serialize, Debug)]
pub(crate) struct HeaderTarget {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    name: String,
}

#[derive(Serialize, Debug)]
pub(crate) struct RecordTarget {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    name: String,
}

#[derive(Serialize, Debug)]
pub(crate) struct TypeTarget {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    name: String,
    arity: u32,
}

#[derive(Serialize, Debug)]
pub(crate) struct VarTarget {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    name: String,
}

#[derive(Serialize, Debug)]
pub(crate) struct FileDeclaration {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    declarations: Vec<Declaration>,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Key<T> {
    key: T,
}

impl<T> From<T> for Key<T> {
    fn from(item: T) -> Self {
        Key { key: item }
    }
}

#[derive(Serialize, Debug)]
pub(crate) struct CommentFact {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    declaration: Key<FunctionDeclarationFact>,
    span: Location,
    text: String,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) enum Declaration {
    #[serde(rename = "func")]
    FunctionDeclaration(Key<FuncDecl>),
    #[serde(rename = "macro")]
    MacroDeclaration(Key<MacroDecl>),
    #[serde(rename = "ttype")]
    TypeDeclaration(Key<TypeDecl>),
    #[serde(rename = "record")]
    RecordDeclaration(Key<RecordDecl>),
    #[serde(rename = "varr")]
    VarDeclaration(Key<VarDecl>),
    #[serde(rename = "header")]
    HeaderDeclaration(Key<HeaderDecl>),
    #[serde(rename = "doc")]
    DocDeclaration(Key<DocDecl>),
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct FuncDecl {
    name: String,
    arity: u32,
    span: Location,
    exported: bool,
    deprecated: bool,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct MacroDecl {
    name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    arity: Option<u32>,
    span: Location,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct TypeDecl {
    name: String,
    arity: u32,
    span: Location,
    exported: bool,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct RecordDecl {
    name: String,
    span: Location,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct VarDecl {
    name: String,
    doc: String,
    span: Location,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct HeaderDecl {
    name: String,
    span: Location,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct DocDecl {
    target: Box<Declaration>,
    span: Location,
    text: String,
}

#[derive(Debug, Default)]
struct IndexedFacts {
    file_facts: Vec<FileFact>,
    file_line_facts: Vec<FileLinesFact>,
    declaration_facts: Vec<FunctionDeclarationFact>,
    xref_facts: Vec<XRefFact>,
    //v2 facts
    file_declarations: Vec<FileDeclaration>,
    xref_v2: Vec<XRefFile>,
}

impl IndexedFacts {
    fn new(
        file_fact: FileFact,
        line_fact: FileLinesFact,
        decl: FileDeclaration,
        xref: XRefFile,
        facts_v1: Option<(Vec<FunctionDeclarationFact>, XRefFact)>,
    ) -> Self {
        let mut facts = Self::default();
        facts.file_facts.push(file_fact);
        facts.file_line_facts.push(line_fact);
        facts.file_declarations.push(decl);
        facts.xref_v2.push(xref);
        if let Some((decl, xref)) = facts_v1 {
            facts.declaration_facts.extend(decl);
            facts.xref_facts.push(xref);
        }
        facts
    }

    fn add(
        &mut self,
        file_fact: FileFact,
        line_fact: FileLinesFact,
        decl: FileDeclaration,
        xref: XRefFile,
        facts: Option<(Vec<FunctionDeclarationFact>, XRefFact)>,
    ) {
        self.file_facts.push(file_fact);
        self.file_line_facts.push(line_fact);
        self.file_declarations.push(decl);
        self.xref_v2.push(xref);
        if let Some((decl, xref)) = facts {
            self.declaration_facts.extend(decl);
            self.xref_facts.push(xref);
        }
    }

    fn to_v1_facts(mut self) -> Vec<Fact> {
        let file_lines_fact = mem::take(&mut self.file_line_facts);
        let file_lines_fact = file_lines_fact.into_iter().map_into().collect();
        let declaration_fact = mem::take(&mut self.declaration_facts);
        let declaration_fact = declaration_fact.into_iter().map_into().collect();
        let xref_fact = mem::take(&mut self.xref_facts);
        let xref_fact = xref_fact.into_iter().map_into().collect();
        vec![
            Fact::File {
                facts: mem::take(&mut self.file_facts),
            },
            Fact::FileLine {
                facts: file_lines_fact,
            },
            Fact::FunctionDeclaration {
                facts: declaration_fact,
            },
            Fact::XRef { facts: xref_fact },
        ]
    }

    fn declaration_to_v1(
        decl: Declaration,
        file_id: GleanFileId,
        module: String,
    ) -> Option<FunctionDeclarationFact> {
        let fact = match decl {
            Declaration::FunctionDeclaration(d) => {
                let fqn = MFA {
                    module,
                    name: d.key.name,
                    arity: d.key.arity,
                    file_id: file_id.clone(),
                };
                FunctionDeclarationFact {
                    file_id,
                    fqn,
                    span: d.key.span,
                }
            }
            Declaration::MacroDeclaration(d) => {
                let fqn = MFA {
                    module,
                    name: d.key.name,
                    arity: d.key.arity.unwrap_or(0),
                    file_id: file_id.clone(),
                };
                FunctionDeclarationFact {
                    file_id,
                    fqn,
                    span: d.key.span,
                }
            }
            Declaration::TypeDeclaration(d) => {
                let fqn = MFA {
                    module,
                    name: d.key.name,
                    arity: d.key.arity,
                    file_id: file_id.clone(),
                };
                FunctionDeclarationFact {
                    file_id,
                    fqn,
                    span: d.key.span,
                }
            }
            Declaration::RecordDeclaration(d) => {
                let fqn = MFA {
                    module,
                    name: d.key.name,
                    arity: REC_ARITY,
                    file_id: file_id.clone(),
                };
                FunctionDeclarationFact {
                    file_id,
                    fqn,
                    span: d.key.span,
                }
            }
            Declaration::HeaderDeclaration(d) => {
                let fqn = MFA {
                    module,
                    name: d.key.name,
                    arity: HEADER_ARITY,
                    file_id: file_id.clone(),
                };
                FunctionDeclarationFact {
                    file_id,
                    fqn,
                    span: d.key.span,
                }
            }
            Declaration::VarDeclaration(d) => {
                let fqn = MFA {
                    module,
                    name: d.key.name,
                    arity: d.key.span.start,
                    file_id: file_id.clone(),
                };
                FunctionDeclarationFact {
                    file_id,
                    fqn,
                    span: d.key.span,
                }
            }
            Declaration::DocDeclaration(_) => return None,
        };
        Some(fact)
    }

    fn to_v2_facts(mut self, modules: &FxHashMap<GleanFileId, String>) -> Vec<Fact> {
        let file_lines_fact = mem::take(&mut self.file_line_facts);
        let file_lines_fact = file_lines_fact.into_iter().map_into().collect();
        let declaration_fact = mem::take(&mut self.file_declarations);
        let mut declarations = vec![];
        let mut comments = vec![];
        for decl in declaration_fact {
            if let Some(module) = modules.get(&decl.file_id) {
                for d in decl.declarations {
                    let file_id = decl.file_id.clone();
                    if let Declaration::DocDeclaration(doc) = &d {
                        let declaration = doc.key.target.as_ref();
                        if let Some(target) = Self::declaration_to_v1(
                            declaration.clone(),
                            file_id.clone(),
                            module.clone(),
                        ) {
                            comments.push(
                                CommentFact {
                                    file_id,
                                    declaration: target.into(),
                                    span: doc.key.span.clone(),
                                    text: doc.key.text.clone(),
                                }
                                .into(),
                            );
                            continue;
                        }
                    }
                    if let Some(fact) = Self::declaration_to_v1(d, file_id, module.clone()) {
                        declarations.push(fact);
                    }
                }
            }
        }
        let declaration_fact = declarations.into_iter().map_into().collect();
        let xref_fact = mem::take(&mut self.xref_v2);
        let mut xrefs = vec![];
        for fact in xref_fact {
            let file_id = fact.file_id;
            let mut facts = vec![];
            for xref in fact.xrefs {
                let source = xref.source;
                let file_id = xref.target.file_id();
                if let Some(module) = modules.get(file_id) {
                    let target = match xref.target {
                        XRefTarget::Function(x) => MFA {
                            module: module.clone(),
                            name: x.key.name,
                            arity: x.key.arity,
                            file_id: x.key.file_id,
                        },
                        XRefTarget::Macro(x) => MFA {
                            module: module.clone(),
                            name: x.key.name,
                            arity: x.key.arity.unwrap_or(0),
                            file_id: x.key.file_id,
                        },
                        XRefTarget::Header(x) => MFA {
                            module: module.clone(),
                            name: x.key.name,
                            arity: HEADER_ARITY,
                            file_id: x.key.file_id,
                        },
                        XRefTarget::Record(x) => MFA {
                            module: module.clone(),
                            name: x.key.name,
                            arity: REC_ARITY,
                            file_id: x.key.file_id,
                        },
                        XRefTarget::Type(x) => MFA {
                            module: module.clone(),
                            name: x.key.name,
                            arity: x.key.arity,
                            file_id: x.key.file_id,
                        },
                        XRefTarget::Var(x) => MFA {
                            module: module.clone(),
                            name: x.key.name,
                            arity: source.start,
                            file_id: x.key.file_id,
                        },
                    };
                    let val = XRefFactVal { source, target };
                    facts.push(val);
                }
            }
            xrefs.push(XRefFact {
                file_id,
                xrefs: facts,
            });
        }
        let xref_fact = xrefs.into_iter().map_into().collect();
        vec![
            Fact::File {
                facts: mem::take(&mut self.file_facts),
            },
            Fact::FileLine {
                facts: file_lines_fact,
            },
            Fact::FunctionDeclaration {
                facts: declaration_fact,
            },
            Fact::XRef { facts: xref_fact },
            Fact::DeclarationComment { facts: comments },
        ]
    }
}

pub struct GleanIndexer {
    project_id: ProjectId,
    analysis: Analysis,
    module: Option<String>,
}

pub fn index(args: &Glean, cli: &mut dyn Cli, query_config: &BuckQueryConfig) -> Result<()> {
    let (indexer, _loaded) = GleanIndexer::new(args, cli, query_config)?;
    let config = IndexConfig {
        multi: args.multi,
        prefix: args.prefix.clone(),
    };
    let (facts, module_index) = indexer.index(config)?;
    write_results(facts, module_index, cli, args)
}

fn write_results(
    facts: FxHashMap<String, IndexedFacts>,
    module_index: FxHashMap<GleanFileId, String>,
    cli: &mut dyn Cli,
    args: &Glean,
) -> Result<()> {
    for (name, fact) in facts {
        let fact = if args.v2 {
            fact.to_v2_facts(&module_index)
        } else {
            fact.to_v1_facts()
        };
        let content = if args.pretty {
            serde_json::to_string_pretty(&fact)?
        } else {
            serde_json::to_string(&fact)?
        };
        let to = match (&args.to, args.multi) {
            (None, _) => None,
            (Some(to), true) => Some(to.join(name)),
            (Some(to), false) => Some(to).cloned(),
        };
        match to {
            Some(to) => std::fs::OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(to)?
                .write_all(content.as_bytes()),
            None => cli.write_all(content.as_bytes()),
        }?;
    }
    Ok(())
}

impl GleanIndexer {
    pub fn new(
        args: &Glean,
        cli: &mut dyn Cli,
        query_config: &BuckQueryConfig,
    ) -> Result<(Self, LoadResult)> {
        let config = DiscoverConfig::buck();
        let loaded = load::load_project_at(
            cli,
            &args.project,
            config,
            IncludeOtp::Yes,
            elp_eqwalizer::Mode::Server,
            query_config,
        )?;
        let analysis = loaded.analysis();
        let indexer = Self {
            project_id: loaded.project_id,
            analysis,
            module: args.module.clone(),
        };
        Ok((indexer, loaded))
    }

    fn index(
        &self,
        config: IndexConfig,
    ) -> Result<(
        FxHashMap<String, IndexedFacts>,
        FxHashMap<GleanFileId, String>,
    )> {
        let ctx = self.analysis.with_db(|db| {
            let project_id = self.project_id;
            let files = Self::project_files(db, project_id);
            // glean module index, which fake headers as modules with name header.hrl
            let module_index: FxHashMap<GleanFileId, String> = files
                .iter()
                .filter_map(|(file_id, path)| {
                    path_to_module_name(path).map(|name| ((*file_id).into(), name))
                })
                .collect();
            let facts = if let Some(module) = &self.module {
                let index = db.module_index(self.project_id);
                let file_id = index
                    .file_for_module(&ModuleName::new(module))
                    .expect("No module found");
                let source_root_id = db.file_source_root(file_id);
                let source_root = db.source_root(source_root_id);
                let path = source_root.path_for_file(&file_id).unwrap();
                match Self::index_file(
                    &db,
                    file_id,
                    &path,
                    project_id,
                    &module_index,
                    config.prefix.as_ref(),
                ) {
                    Some((file, line, decl, xref, facts)) => {
                        let mut result = FxHashMap::default();
                        result.insert(
                            FACTS_FILE.to_string(),
                            IndexedFacts::new(file, line, decl, xref, facts),
                        );
                        result
                    }
                    None => panic!("Can't find module {}", module),
                }
            } else {
                let iter = files
                    .into_par_iter()
                    .map_with(self.analysis.clone(), |analysis, (file_id, path)| {
                        analysis.with_db(|db| {
                            Self::index_file(
                                db,
                                file_id,
                                &path,
                                project_id,
                                &module_index,
                                config.prefix.as_ref(),
                            )
                        })
                    })
                    .flatten()
                    .flatten();
                if config.multi {
                    iter.map(|(file, line, decl, xref, facts)| {
                        IndexedFacts::new(file, line, decl, xref, facts)
                    })
                    .collect::<Vec<_>>()
                    .into_iter()
                    .enumerate()
                    .map(|(id, facts)| (format!("{}.json", id), facts))
                    .collect()
                } else {
                    let mut result = FxHashMap::default();
                    let facts = iter.collect::<Vec<_>>().into_iter().fold(
                        IndexedFacts::default(),
                        |mut acc, (file_fact, line_fact, decl, xref, facts)| {
                            acc.add(file_fact, line_fact, decl, xref, facts);
                            acc
                        },
                    );
                    result.insert(FACTS_FILE.to_string(), facts);
                    result
                }
            };
            (facts, module_index)
        })?;
        Ok(ctx)
    }

    fn project_files(db: &RootDatabase, project_id: ProjectId) -> Vec<(FileId, VfsPath)> {
        let project_data = db.project_data(project_id);
        let mut files = vec![];
        for &source_root_id in &project_data.source_roots {
            if let Some(app_data) = db.app_data(source_root_id) {
                if app_data.app_type == AppType::App {
                    let source_root = db.source_root(source_root_id);
                    for file_id in source_root.iter() {
                        if let Some(path) = source_root.path_for_file(&file_id) {
                            files.push((file_id, path.clone()));
                        }
                    }
                }
            }
        }
        files
    }

    fn index_file(
        db: &RootDatabase,
        file_id: FileId,
        path: &VfsPath,
        project_id: ProjectId,
        module_index: &FxHashMap<GleanFileId, String>,
        prefix: Option<&String>,
    ) -> Option<(
        FileFact,
        FileLinesFact,
        FileDeclaration,
        XRefFile,
        Option<(Vec<FunctionDeclarationFact>, XRefFact)>,
    )> {
        let file_fact = Self::file_fact(db, file_id, path, project_id, prefix)?;
        let line_fact = Self::line_fact(db, file_id);
        let mut xref_v2 = Self::xrefs_v2(db, file_id, module_index);
        let mut file_decl = Self::declarations_v2(db, file_id, path)?;
        Self::add_xref_based_declarations(db, project_id, file_id, &mut xref_v2, &mut file_decl);

        let elp_module_index = db.module_index(project_id);
        if let Some(module) = elp_module_index.module_for_file(file_id) {
            let decl = Self::declarations_v1(db, file_id, module);
            let xref = Self::xrefs(db, file_id, module_index);
            return Some((file_fact, line_fact, file_decl, xref_v2, Some((decl, xref))));
        }
        Some((file_fact, line_fact, file_decl, xref_v2, None))
    }

    fn add_xref_based_declarations(
        db: &RootDatabase,
        project_id: ProjectId,
        file_id: FileId,
        xrefs: &mut XRefFile,
        file_decl: &mut FileDeclaration,
    ) {
        let mut vars = FxHashMap::default();
        for xref in &mut xrefs.xrefs {
            match &mut xref.target {
                XRefTarget::Var(x) => {
                    vars.insert(&xref.source, &x.key.name);
                }
                XRefTarget::Macro(x) => {
                    let id: FileId = x.key.file_id.clone().into();
                    let def_map = db.def_map(id);
                    let name = Name::from_erlang_service(&x.key.name);
                    let def = def_map.get_macros().get(&MacroName::new(name, x.key.arity));
                    if let Some(def) = def {
                        let range = def.source(db).syntax().text_range();
                        let text = &db.file_text(id)[range];
                        let text = format!("```erlang\n{}\n```", text);
                        let doc = match (&x.key.expansion, &x.key.ods_url) {
                            (None, None) => text,
                            (None, Some(o)) => format!("[ODS]({})\n{}", o, text),
                            (Some(e), None) => format!("{}\n---\n\n{}", text, e),
                            (Some(e), Some(o)) => format!("[ODS]({})\n{}\n---\n\n{}", o, text, e),
                        };
                        let decl = Declaration::MacroDeclaration(
                            MacroDecl {
                                name: x.key.name.clone(),
                                arity: Some(xref.source.start),
                                span: xref.source.clone(),
                            }
                            .into(),
                        );
                        let doc_decl = Declaration::DocDeclaration(
                            DocDecl {
                                target: Box::new(decl.clone()),
                                span: xref.source.clone(),
                                text: doc,
                            }
                            .into(),
                        );
                        file_decl.declarations.push(decl);
                        file_decl.declarations.push(doc_decl);
                        x.key.file_id = file_id.into();
                        x.key.arity = Some(xref.source.start);
                    }
                }
                _ => (),
            }
        }
        let var_decls = Self::types(db, project_id, file_id, vars);
        for var in var_decls {
            // sometimes eqwalizer produces very long types
            // this check will prevent showing it on codehub

            if var.doc.len() > 1000 {
                continue;
            }
            let doc = var.doc.clone();
            let span = var.span.clone();
            let decl = Declaration::VarDeclaration(var.into());
            let doc_decl = Declaration::DocDeclaration(
                DocDecl {
                    target: Box::new(decl.clone()),
                    span,
                    text: doc,
                }
                .into(),
            );
            file_decl.declarations.push(decl);
            file_decl.declarations.push(doc_decl);
        }
    }

    fn file_fact(
        db: &RootDatabase,
        file_id: FileId,
        path: &VfsPath,
        project_id: ProjectId,
        prefix: Option<&String>,
    ) -> Option<FileFact> {
        let project_data = db.project_data(project_id);
        let root = project_data.root_dir.as_path();
        let file_path = path.as_path()?;
        let file_path = file_path.strip_prefix(root)?;
        let file_path = match prefix {
            Some(prefix) => Path::new(&prefix).join(file_path).to_str()?.into(),
            None => file_path.as_str().to_string(),
        };
        Some(FileFact::new(file_id, file_path))
    }

    fn line_fact(db: &RootDatabase, file_id: FileId) -> FileLinesFact {
        let line_index = db.file_line_index(file_id);
        let mut line = 1;
        let mut prev_offset = 0;
        let mut lengths = vec![];
        let mut ends_with_new_line = true;
        while let Some(offset) = line_index.line_at(line) {
            let curr_offset: u32 = offset.into();
            lengths.push(curr_offset - prev_offset);
            line += 1;
            prev_offset = curr_offset;
        }
        let content = db.file_text(file_id);
        if !content.ends_with('\n') {
            ends_with_new_line = false;
            let len = if content.len() as u32 >= prev_offset {
                content.len() as u32 - prev_offset
            } else {
                0
            };
            lengths.push(len);
        }
        FileLinesFact::new(file_id, lengths, ends_with_new_line)
    }

    fn declarations_v1(
        db: &RootDatabase,
        file_id: FileId,
        module: &ModuleName,
    ) -> Vec<FunctionDeclarationFact> {
        let def_map = db.local_def_map(file_id);
        let mut result = vec![];
        for (fun, def) in def_map.get_functions() {
            let range = def.range(db);
            if let Some(range) = range {
                let loc = range.into();
                let mfa = MFA::new(module, fun.name(), fun.arity(), def.file.file_id);
                result.push(FunctionDeclarationFact::new(file_id, mfa, loc));
            }
        }
        for (ty, def) in def_map.get_types() {
            let range = def.source(db).syntax().text_range();
            let loc = range.into();
            let mfa = MFA::new(module, ty.name(), ty.arity(), def.file.file_id);
            result.push(FunctionDeclarationFact::new(file_id, mfa, loc));
        }
        for (rec, def) in def_map.get_records() {
            let range = def.source(db).syntax().text_range();
            let loc = range.into();
            let mfa = MFA::new(module, rec, REC_ARITY, def.file.file_id);
            result.push(FunctionDeclarationFact::new(file_id, mfa, loc));
        }
        result
    }

    fn declarations_v2(
        db: &RootDatabase,
        file_id: FileId,
        path: &VfsPath,
    ) -> Option<FileDeclaration> {
        let mut declarations = vec![];
        let def_map = db.local_def_map(file_id);
        // file docs are too slow. Going with specs for now
        // let file_doc = db.file_doc(file_id);
        let specs = db.file_specs(file_id);
        for (fun, def) in def_map.get_functions() {
            let range = def.range(db);
            if let Some(range) = range {
                let span = range.into();
                let decl = Declaration::FunctionDeclaration(
                    FuncDecl {
                        name: fun.name().to_string(),
                        arity: fun.arity(),
                        span,
                        exported: def.exported,
                        deprecated: def.deprecated,
                    }
                    .into(),
                );
                if let (Some(spec_def), Some(spec)) = (&def.spec, specs.get(fun)) {
                    let doc_range = spec_def.source(db).syntax().text_range();
                    let doc = spec.markdown_text().to_string();
                    declarations.push(Declaration::DocDeclaration(
                        DocDecl {
                            target: Box::new(decl.clone()),
                            span: doc_range.into(),
                            text: doc,
                        }
                        .into(),
                    ));
                }
                declarations.push(decl);
            }
        }

        for (macros, def) in def_map.get_macros() {
            let range = def.source(db).syntax().text_range();
            let span = range.into();
            let decl = Declaration::MacroDeclaration(
                MacroDecl {
                    name: macros.name().to_string(),
                    arity: macros.arity(),
                    span,
                }
                .into(),
            );
            declarations.push(decl);
        }

        for (ty, def) in def_map.get_types() {
            let range = def.source(db).syntax().text_range();
            let text = &db.file_text(file_id)[range];
            let text = format!("```erlang\n{}\n```", text);
            let span: Location = range.into();

            let decl = Declaration::TypeDeclaration(
                TypeDecl {
                    name: ty.name().to_string(),
                    arity: ty.arity(),
                    span: span.clone(),
                    exported: def.exported,
                }
                .into(),
            );

            declarations.push(Declaration::DocDeclaration(
                DocDecl {
                    target: Box::new(decl.clone()),
                    span,
                    text,
                }
                .into(),
            ));
            declarations.push(decl);
        }

        for (rec, def) in def_map.get_records() {
            let range = def.source(db).syntax().text_range();
            let text = &db.file_text(file_id)[range];
            let text = format!("```erlang\n{}\n```", text);
            let span: Location = range.into();

            let decl = Declaration::RecordDeclaration(
                RecordDecl {
                    name: rec.to_string(),
                    span: span.clone(),
                }
                .into(),
            );

            declarations.push(Declaration::DocDeclaration(
                DocDecl {
                    target: Box::new(decl.clone()),
                    span,
                    text,
                }
                .into(),
            ));
            declarations.push(decl);
        }

        if let Some((name, Some("hrl"))) = path.name_and_extension() {
            declarations.push(Declaration::HeaderDeclaration(
                HeaderDecl {
                    name: format!("{}.hrl", name),
                    span: Location {
                        start: 0,
                        length: 1,
                    },
                }
                .into(),
            ));
        }

        Some(FileDeclaration {
            file_id: file_id.into(),
            declarations,
        })
    }

    fn xrefs(
        db: &RootDatabase,
        file_id: FileId,
        module_index: &FxHashMap<GleanFileId, String>,
    ) -> XRefFact {
        let sema = Semantic::new(db);
        let source_file = sema.parse(file_id);
        let mut xrefs = fold::fold_file(
            &sema,
            Strategy {
                macros: MacroStrategy::SurfaceOnly,
                parens: ParenStrategy::InvisibleParens,
            },
            file_id,
            vec![],
            &mut |mut acc, ctx| match &ctx.item {
                hir::AnyExpr::Expr(Expr::Call { target, args }) => {
                    if let Some((body, _, expr_source)) = ctx.body_with_expr_source(&sema) {
                        if let Some(range) =
                            Self::find_range(&sema, &ctx, &source_file, &expr_source)
                        {
                            let arity = args.len() as u32;
                            if let Some(fact) =
                                Self::resolve_call(&sema, target, arity, file_id, &body, range)
                            {
                                acc.push(fact);
                            }
                        }
                    }
                    acc
                }
                hir::AnyExpr::Expr(Expr::CaptureFun { target, arity }) => {
                    if let Some((body, range)) = ctx.find_range(&sema) {
                        let arity: Option<u32> = match body[*arity] {
                            Expr::Literal(Literal::Integer(int)) => int.try_into().ok(),
                            _ => None,
                        };
                        if let Some(arity) = arity {
                            if let Some(fact) =
                                Self::resolve_call(&sema, target, arity, file_id, &body, range)
                            {
                                acc.push(fact);
                            }
                        }
                    }
                    acc
                }
                hir::AnyExpr::TypeExpr(TypeExpr::Call { target, args }) => {
                    let arity = args.len() as u32;
                    if let Some(fact) = Self::resolve_type(&sema, target, arity, file_id, &ctx) {
                        acc.push(fact);
                    }
                    acc
                }
                hir::AnyExpr::Expr(Expr::Record { name, fields: _ }) => {
                    if let Some(fact) = Self::resolve_record(&sema, *name, file_id, &ctx) {
                        acc.push(fact);
                    }
                    acc
                }
                hir::AnyExpr::Expr(Expr::RecordIndex { name, field: _ }) => {
                    if let Some(fact) = Self::resolve_record(&sema, *name, file_id, &ctx) {
                        acc.push(fact);
                    }
                    acc
                }
                hir::AnyExpr::Expr(Expr::RecordField {
                    name,
                    expr: _,
                    field: _,
                }) => {
                    if let Some(fact) = Self::resolve_record(&sema, *name, file_id, &ctx) {
                        acc.push(fact);
                    }
                    acc
                }
                hir::AnyExpr::Expr(Expr::RecordUpdate {
                    name,
                    expr: _,
                    fields: _,
                }) => {
                    if let Some(fact) = Self::resolve_record(&sema, *name, file_id, &ctx) {
                        acc.push(fact);
                    }
                    acc
                }
                hir::AnyExpr::Pat(Pat::Record { name, fields: _ }) => {
                    if let Some(fact) = Self::resolve_record(&sema, *name, file_id, &ctx) {
                        acc.push(fact);
                    }
                    acc
                }
                hir::AnyExpr::Pat(Pat::RecordIndex { name, field: _ }) => {
                    if let Some(fact) = Self::resolve_record(&sema, *name, file_id, &ctx) {
                        acc.push(fact);
                    }
                    acc
                }
                hir::AnyExpr::TypeExpr(TypeExpr::Record { name, fields: _ }) => {
                    if let Some(fact) = Self::resolve_record(&sema, *name, file_id, &ctx) {
                        acc.push(fact);
                    }
                    acc
                }
                _ => acc,
            },
            &mut |acc, _on, _form_id| acc,
        );

        xrefs.retain(|x| module_index.contains_key(&x.target.file_id));

        XRefFact::new(file_id, xrefs)
    }

    fn xref_v2_callback(
        db: &RootDatabase,
        sema: &Semantic,
        source_file: &InFile<ast::SourceFile>,
        file_id: FileId,
        acc: &mut Vec<XRef>,
        ctx: &AnyCallBackCtx,
    ) -> Option<()> {
        let target = match &ctx.item {
            hir::AnyExpr::Pat(Pat::Var(var))
            | hir::AnyExpr::TypeExpr(TypeExpr::Var(var))
            | hir::AnyExpr::Expr(Expr::Var(var)) => {
                let name = var.as_string(db);
                let (_, range) = ctx.find_range(&sema)?;
                Some(XRef {
                    source: range.into(),
                    target: XRefTarget::Var(
                        VarTarget {
                            file_id: file_id.into(),
                            name,
                        }
                        .into(),
                    ),
                })
            }
            hir::AnyExpr::Expr(Expr::Call { target, args }) => {
                let (body, _, expr_source) = ctx.body_with_expr_source(&sema)?;
                let range = Self::find_range(&sema, &ctx, &source_file, &expr_source)?;
                let arity = args.len() as u32;
                Self::resolve_call_v2(&sema, target, arity, file_id, &body, range)
            }
            hir::AnyExpr::Expr(Expr::CaptureFun { target, arity }) => {
                let (body, range) = ctx.find_range(&sema)?;
                let arity: Option<u32> = match body[*arity] {
                    Expr::Literal(Literal::Integer(int)) => int.try_into().ok(),
                    _ => None,
                };
                Self::resolve_call_v2(&sema, target, arity?, file_id, &body, range)
            }
            hir::AnyExpr::Pat(Pat::Record { name, .. })
            | hir::AnyExpr::Pat(Pat::RecordIndex { name, .. })
            | hir::AnyExpr::TypeExpr(TypeExpr::Record { name, .. })
            | hir::AnyExpr::Expr(Expr::Record { name, .. })
            | hir::AnyExpr::Expr(Expr::RecordIndex { name, .. })
            | hir::AnyExpr::Expr(Expr::RecordUpdate { name, .. })
            | hir::AnyExpr::Expr(Expr::RecordField { name, .. }) => {
                Self::resolve_record_v2(&sema, *name, file_id, ctx)
            }
            hir::AnyExpr::Expr(Expr::MacroCall {
                macro_def, args, ..
            })
            | hir::AnyExpr::Pat(Pat::MacroCall {
                macro_def, args, ..
            })
            | hir::AnyExpr::TypeExpr(TypeExpr::MacroCall {
                macro_def, args, ..
            })
            | hir::AnyExpr::Term(Term::MacroCall {
                macro_def, args, ..
            }) => {
                let def = macro_def.as_ref()?;
                Self::resolve_macro_v2(&sema, def, args, &source_file, &ctx)
            }
            hir::AnyExpr::TypeExpr(TypeExpr::Call { target, args }) => {
                let arity = args.len() as u32;
                Self::resolve_type_v2(&sema, target, arity, file_id, &ctx)
            }
            _ => None,
        };
        acc.push(target?);
        None
    }

    fn types(
        db: &RootDatabase,
        project_id: ProjectId,
        file_id: FileId,
        vars: FxHashMap<&Location, &String>,
    ) -> Vec<VarDecl> {
        let mut result = vec![];
        if !db.is_eqwalizer_enabled(file_id, false) {
            return result;
        }
        let module_diagnostics = db.eqwalizer_diagnostics_by_project(project_id, vec![file_id]);
        if let EqwalizerDiagnostics::Diagnostics { type_info, .. } = module_diagnostics.as_ref() {
            let types = type_info
                .values()
                .flatten()
                .filter(|(_, ty)| !ty.is_dynamic())
                .collect_vec();
            for (pos, ty) in types {
                if let Pos::TextRange(range) = pos {
                    let range: TextRange = range.clone().into();
                    let range: Location = range.into();
                    if let Some(name) = vars.get(&range) {
                        let text = format!("```erlang\n{} :: {}\n```", name, ty);
                        let decl = VarDecl {
                            name: name.to_string(),
                            doc: text,
                            span: range.into(),
                        };
                        result.push(decl.into());
                    }
                }
            }
        }
        result
    }

    fn xrefs_v2(
        db: &RootDatabase,
        file_id: FileId,
        module_index: &FxHashMap<GleanFileId, String>,
    ) -> XRefFile {
        let sema = Semantic::new(db);
        let source_file = sema.parse(file_id);
        let form_list = sema.form_list(file_id);
        let def_map = sema.def_map(file_id);
        let mut xrefs = fold::fold_file(
            &sema,
            Strategy {
                macros: MacroStrategy::SurfaceOnly,
                parens: ParenStrategy::InvisibleParens,
            },
            file_id,
            vec![],
            &mut |mut acc, ctx| {
                Self::xref_v2_callback(db, &sema, &source_file, file_id, &mut acc, &ctx);
                acc
            },
            &mut |mut acc, on, form_id| match (on, form_id) {
                (hir::On::Entry, hir::FormIdx::PPDirective(idx)) => {
                    let directive = &form_list[idx];
                    if let PPDirective::Include(idx) = directive {
                        let include = &form_list[idx.clone()];
                        let ast = include.form_id().get_ast(db, file_id);
                        let range = ast.syntax().text_range().into();
                        if let Some(file) = db.resolve_include(InFile::new(file_id, idx.clone())) {
                            if let Some(path) = path_for_file(db, file) {
                                if let Some((name, Some("hrl"))) = path.name_and_extension() {
                                    let target = HeaderTarget {
                                        file_id: file.into(),
                                        name: format!("{}.hrl", name),
                                    };
                                    let xref = XRef {
                                        source: range,
                                        target: XRefTarget::Header(target.into()),
                                    };
                                    acc.push(xref);
                                }
                            }
                        }
                    }
                    acc
                }
                (hir::On::Entry, hir::FormIdx::Export(idx)) => {
                    let export = &form_list[idx];
                    let ast = export.form_id.get_ast(db, file_id);
                    for fun in ast.funs().into_iter() {
                        if let Some(na) = Self::fa_name_arity(&fun) {
                            if let Some(def) = sema.def_map(file_id).get_function(&na) {
                                let range = fun.syntax().text_range().into();
                                let target = FunctionTarget {
                                    file_id: def.file.file_id.into(),
                                    name: na.name().to_string(),
                                    arity: na.arity(),
                                };
                                let xref = XRef {
                                    source: range,
                                    target: XRefTarget::Function(target.into()),
                                };
                                acc.push(xref);
                            }
                        }
                    }
                    acc
                }
                (hir::On::Entry, hir::FormIdx::DeprecatedAttribute(idx)) => {
                    let deprecated = &form_list[idx];
                    let form_id = deprecated.form_id();
                    let ast = form_id.get_ast(db, file_id);
                    if let Some(attr) = ast.attr() {
                        match attr {
                            ast::DeprecatedDetails::DeprecatedFa(fun) => {
                                if let Some(xref) = Self::deprecated_xref(&def_map, &fun) {
                                    acc.push(xref);
                                }
                            }
                            ast::DeprecatedDetails::DeprecatedFas(funs) => {
                                for fun in funs.fa() {
                                    if let Some(xref) = Self::deprecated_xref(&def_map, &fun) {
                                        acc.push(xref);
                                    }
                                }
                            }
                            _ => (),
                        }
                    }

                    acc
                }
                _ => acc,
            },
        );
        xrefs.retain(|x| module_index.contains_key(&x.target.file_id()));
        XRefFile {
            file_id: file_id.into(),
            xrefs,
        }
    }

    fn deprecated_xref(def_map: &DefMap, fun: &DeprecatedFa) -> Option<XRef> {
        let name = fun.fun()?.as_name();
        let arity = fun.arity()?;
        let (na, def) = match arity {
            ast::DeprecatedFunArity::DeprecatedWildcard(_) => {
                let (na, def) = def_map
                    .get_functions()
                    .filter(|(na, _)| na.name() == &name)
                    .sorted_by_key(|(na, _)| na.arity())
                    .next()?;
                (na.clone(), def)
            }
            ast::DeprecatedFunArity::Integer(arity) => {
                let na = NameArity::new(name, arity.into());
                let def = def_map.get_function(&na)?;
                (na, def)
            }
        };
        let range = fun.fun()?.syntax().text_range().into();
        let target = FunctionTarget {
            file_id: def.file.file_id.into(),
            name: na.name().to_string(),
            arity: na.arity(),
        };
        let xref = XRef {
            source: range,
            target: XRefTarget::Function(target.into()),
        };
        Some(xref)
    }

    fn fa_name_arity(fa: &Fa) -> Option<NameArity> {
        let name = fa.fun()?.text()?;
        let name = Name::from_erlang_service(&name);
        let arity = fa.arity()?.value()?.arity_value()?;
        Some(NameArity::new(name, arity as u32))
    }

    fn resolve_call(
        sema: &Semantic<'_>,
        target: &CallTarget<ExprId>,
        arity: u32,
        file_id: FileId,
        body: &Body,
        range: TextRange,
    ) -> Option<XRefFactVal> {
        let def = resolve_call_target(sema, target, Some(arity), file_id, body)?;
        let module = &def.module?;
        let mfa = MFA::new(module, def.name.name(), arity, def.file.file_id);
        Some(XRefFactVal::new(range.into(), mfa))
    }

    fn resolve_call_v2(
        sema: &Semantic<'_>,
        target: &CallTarget<ExprId>,
        arity: u32,
        file_id: FileId,
        body: &Body,
        range: TextRange,
    ) -> Option<XRef> {
        let def = resolve_call_target(sema, target, Some(arity), file_id, body)?;
        let target = FunctionTarget {
            file_id: def.file.file_id.into(),
            name: def.name.name().to_string(),
            arity,
        };
        Some(XRef {
            source: range.into(),
            target: XRefTarget::Function(target.into()),
        })
    }

    fn resolve_macro_v2(
        sema: &Semantic<'_>,
        macro_def: &InFile<DefineId>,
        args: &[ExprId],
        source_file: &InFile<ast::SourceFile>,
        ctx: &AnyCallBackCtx,
    ) -> Option<XRef> {
        let (_, source_map, expr_source) = ctx.body_with_expr_source(&sema)?;
        let range = Self::find_range(&sema, &ctx, &source_file, &expr_source)?;
        let form_list = sema.form_list(macro_def.file_id);
        let define = &form_list[macro_def.value];
        let name = define.name.name();
        let expansion = Self::expand_macro(sema, &expr_source, source_file);
        let mut target = MacroTarget {
            file_id: macro_def.file_id.into(),
            name: name.to_string(),
            arity: define.name.arity(),
            expansion,
            ods_url: None,
        };
        // @fb-only
            // @fb-only
        Some(XRef {
            source: range.into(),
            target: XRefTarget::Macro(target.into()),
        })
    }

    fn expand_macro(
        sema: &Semantic,
        expr_source: &ExprSource,
        source_file: &InFile<ast::SourceFile>,
    ) -> Option<String> {
        let node = expr_source.to_node(source_file)?;
        if let ast::Expr::ExprMax(ExprMax::MacroCallExpr(macro_call)) = node {
            let (_, expansion) = sema.expand(InFile::new(source_file.file_id, &macro_call))?;
            let expansion = expansion.trim();
            let expansion = format!("```erlang\n{}\n```", expansion);
            return Some(expansion);
        }
        None
    }

    fn resolve_type(
        sema: &Semantic,
        target: &CallTarget<TypeExprId>,
        arity: u32,
        file_id: FileId,
        ctx: &AnyCallBackCtx,
    ) -> Option<XRefFactVal> {
        let (body, range) = ctx.find_range(sema)?;
        let def = resolve_type_target(sema, target, Some(arity), file_id, &body)?;
        let module = module_name(sema.db.upcast(), def.file.file_id)?;
        let mfa = MFA::new(
            &module,
            def.type_alias.name().name(),
            arity,
            def.file.file_id,
        );
        Some(XRefFactVal::new(range.into(), mfa))
    }

    fn resolve_type_v2(
        sema: &Semantic,
        target: &CallTarget<TypeExprId>,
        arity: u32,
        file_id: FileId,
        ctx: &AnyCallBackCtx,
    ) -> Option<XRef> {
        let (body, range) = ctx.find_range(sema)?;
        let def = resolve_type_target(sema, target, Some(arity), file_id, &body)?;
        Some(XRef {
            source: range.into(),
            target: XRefTarget::Type(
                TypeTarget {
                    file_id: def.file.file_id.into(),
                    name: def.type_alias.name().name().to_string(),
                    arity,
                }
                .into(),
            ),
        })
    }

    fn resolve_record(
        sema: &Semantic,
        name: hir::Atom,
        file_id: FileId,
        ctx: &AnyCallBackCtx,
    ) -> Option<XRefFactVal> {
        let record_name = sema.db.lookup_atom(name);
        let def_map = sema.db.def_map(file_id);
        let def = def_map.get_record(&record_name)?;
        let module = module_name(sema.db.upcast(), def.file.file_id)?;
        let (_, _, expr_source) = ctx.body_with_expr_source(&sema)?;
        let source_file = sema.parse(file_id);
        let range = Self::find_range(sema, ctx, &source_file, &expr_source)?;
        let mfa = MFA::new(&module, &def.record.name, REC_ARITY, def.file.file_id);
        Some(XRefFactVal::new(range.into(), mfa))
    }

    fn resolve_record_v2(
        sema: &Semantic,
        name: hir::Atom,
        file_id: FileId,
        ctx: &AnyCallBackCtx,
    ) -> Option<XRef> {
        let record_name = sema.db.lookup_atom(name);
        let def_map = sema.db.def_map(file_id);
        let def = def_map.get_record(&record_name)?;
        let (_, _, expr_source) = ctx.body_with_expr_source(&sema)?;
        let source_file = sema.parse(file_id);
        let range = Self::find_range(sema, ctx, &source_file, &expr_source)?;
        Some(XRef {
            source: range.into(),
            target: XRefTarget::Record(
                RecordTarget {
                    file_id: def.file.file_id.into(),
                    name: def.record.name.to_string(),
                }
                .into(),
            ),
        })
    }

    fn find_range(
        sema: &Semantic,
        ctx: &AnyCallBackCtx,
        source_file: &InFile<ast::SourceFile>,
        expr_source: &ExprSource,
    ) -> Option<TextRange> {
        let node = expr_source.to_node(&source_file)?;
        let range = match node {
            elp_syntax::ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(expr)) => {
                expr.name()?.syntax().text_range()
            }
            elp_syntax::ast::Expr::Call(expr) => expr.expr()?.syntax().text_range(),
            elp_syntax::ast::Expr::RecordExpr(expr) => expr.name()?.syntax().text_range(),
            elp_syntax::ast::Expr::RecordFieldExpr(expr) => expr.name()?.syntax().text_range(),
            elp_syntax::ast::Expr::RecordIndexExpr(expr) => expr.name()?.syntax().text_range(),
            elp_syntax::ast::Expr::RecordUpdateExpr(expr) => expr.name()?.syntax().text_range(),
            _ => ctx.find_range(sema)?.1,
        };
        Some(range)
    }
}

fn path_to_module_name(path: &VfsPath) -> Option<String> {
    match path.name_and_extension() {
        Some((name, Some("erl"))) => Some(name.to_string()),
        Some((name, Some("hrl"))) => Some(format!("{}.hrl", name)),
        _ => None,
    }
}

impl From<TextRange> for Location {
    fn from(range: TextRange) -> Self {
        let start: u32 = range.start().into();
        let length: u32 = range.len().into();
        Location { start, length }
    }
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;
    use std::fmt;
    use std::path::PathBuf;

    use elp::cli::Fake;
    use elp_ide::elp_ide_db::elp_base_db::fixture::extract_annotations;
    use elp_ide::elp_ide_db::elp_base_db::fixture::WithFixture;
    use elp_ide::elp_ide_db::elp_base_db::SourceDatabaseExt;
    use elp_ide::elp_ide_db::eqwalizer::otp_supported_by_eqwalizer;
    use elp_ide::AnalysisHost;
    use elp_project_model::test_fixture::DiagnosticsEnabled;
    use expect_test::expect_file;
    use fxhash::FxHashSet;

    use super::*;

    #[test]
    fn serialization_test_v1() {
        let mut cli = Fake::default();
        let file_id = FileId::from_raw(10071);
        let location = Location {
            start: 0,
            length: 10,
        };
        let module = "smax_product_catalog";
        let name = "product_visibility_update_request_iq";
        let arity = 0;

        let file_facts = vec![
            FileFact::new(
                file_id,
                "/local/whatsapp/server/erl/groupd_service/test/p13n/grpd_p13n_new_create_group_SUITE.erl".into(),
            )
        ];
        let file_line_facts = vec![FileLinesFact::new(file_id, vec![71, 42], true)];
        let decl = FileDeclaration {
            file_id: file_id.into(),
            declarations: vec![Declaration::FunctionDeclaration(
                FuncDecl {
                    name: name.to_string(),
                    arity,
                    span: location.clone(),
                    exported: false,
                    deprecated: false,
                }
                .into(),
            )],
        };
        let xref = XRefFile {
            file_id: file_id.into(),
            xrefs: vec![XRef {
                source: location,
                target: XRefTarget::Function(
                    FunctionTarget {
                        file_id: file_id.into(),
                        name: name.to_string(),
                        arity,
                    }
                    .into(),
                ),
            }],
        };

        let facts = IndexedFacts {
            file_facts,
            file_line_facts,
            declaration_facts: vec![],
            xref_facts: vec![],
            file_declarations: vec![decl],
            xref_v2: vec![xref],
        };
        let mut map = FxHashMap::default();
        map.insert(FACTS_FILE.to_string(), facts);
        let args = Glean {
            project: PathBuf::default(),
            module: None,
            to: None,
            v2: true,
            pretty: false,
            multi: false,
            prefix: None,
        };
        let mut module_index = FxHashMap::default();
        module_index.insert(file_id.into(), module.to_string());

        write_results(map, module_index, &mut cli, &args).expect("success");

        let (out, err) = cli.to_strings();
        let expected = expect_file!["../resources/test/glean/serialization_test.out"];
        expected.assert_eq(&out);
        assert_eq!(err, "")
    }

    #[test]
    fn file_fact_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module2.erl
        -module(glean_module2).
        "#;
        let result = facts_with_annotations(spec).0;
        assert_eq!(result.file_facts.len(), 1);
        let file_fact = &result.file_facts[0];
        assert_eq!(
            file_fact.file_path.as_str(),
            "glean/app_glean/src/glean_module2.erl"
        );
    }

    #[test]
    fn file_fact_prefix_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module2.erl
        -module(glean_module2).
        "#;
        let config = IndexConfig {
            multi: false,
            prefix: Some("my/prefix".to_string()),
        };
        let result = facts_with_annotations_with_config(spec, config).0;
        assert_eq!(result.file_facts.len(), 1);
        let file_fact = &result.file_facts[0];
        assert_eq!(
            file_fact.file_path.as_str(),
            "my/prefix/glean/app_glean/src/glean_module2.erl"
        );
    }

    #[test]
    fn line_fact_with_new_line_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module3.erl
        -module(glean_module3).
        main() ->
            bar.

        "#;
        let result = facts_with_annotations(spec).0;
        assert_eq!(result.file_line_facts.len(), 1);
        let line_fact = &result.file_line_facts[0];
        assert!(line_fact.ends_with_new_line);
        assert_eq!(line_fact.lengths, vec![24, 10, 9, 1]);
    }

    #[test]
    fn line_fact_without_new_line_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module4.erl
        -module(glean_module4).
        main() ->
            bar."#;
        let result = facts_with_annotations(spec).0;
        assert_eq!(result.file_line_facts.len(), 1);
        let line_fact = &result.file_line_facts[0];
        assert_eq!(line_fact.ends_with_new_line, false);
        assert_eq!(line_fact.lengths, vec![24, 10, 8]);
    }

    #[test]
    fn declaration_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module5.erl
            -module(glean_module5).
            -type tree() :: {'node', tree(), tree(), any(), any()}.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ glean_module5/tree/0
            -record(user, {name = "" :: string(), notes :: tree()}).
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ glean_module5/user/99
            main(A) -> A.
        %%  ^^^^^^^^^^^^^ glean_module5/main/1
        "#;
        decl_check(&spec);
    }

    #[test]
    fn declaration_v2_test() {
        if otp_supported_by_eqwalizer() {
            let spec = r#"
        //- /app_glean/src/glean_module5.erl app:app_glean
            -module(glean_module5).
            -export([foo/0, doc_foo/1, depr_foo/1]).
            -export_type([person/1]).
            -deprecated({depr_foo, 1, "use foo/0 instead"}).

            -define(PI, 3.14).
        %%  ^^^^^^^^^^^^^^^^^^ macro/PI/no_arity
            -define(MAX(X, Y), if X > Y -> X; true -> Y end).
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ macro/MAX/2

            -type person(Name) :: {name, list(Name)}.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ type/person/1/exported
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ doc/-type person(Name) :: {name, list(Name)}.
            -record(user, {name = "" :: string(), notes :: person(pos_integer())}).
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ rec/user
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ doc/-record(user, {name = "" :: string(), notes :: person(pos_integer())}).

            foo() -> 1.
        %%  ^^^^^^^^^^^ func/foo/0/not_deprecated/exported
            depr_foo(B) -> B.
        %%  ^^^^^^^^^^^^^^^^^ func/depr_foo/1/deprecated/exported
            -spec doc_foo(integer()) -> [integer()].
            doc_foo(Bar) -> [Bar].
        %%  ^^^^^^^^^^^^^^^^^^^^^^ func/doc_foo/1/not_deprecated/exported
        %%  ^^^^^^^^^^^^^^^^^^^^^^ doc/-spec doc_foo(integer()) -> [integer()].
        %%          ^^^ var/Bar :: number()
        %%          ^^^ doc/Bar :: number()
        %%                   ^^^ var/Bar :: number()
        %%                   ^^^ doc/Bar :: number()

            main(A) -> A.
        %%  ^^^^^^^^^^^^^ func/main/1/not_deprecated/not_exported
        "#;
            decl_v2_check(&spec);
        }
    }

    #[test]
    fn declaration_v2_types_test() {
        if otp_supported_by_eqwalizer() {
            let spec = r#"
        //- eqwalizer
        //- erlang_service
        //- /app_glean/src/glean_module5.erl app:app_glean
            -module(glean_module5).
            foo(B) -> 1.
        %%  ^^^^^^^^^^^^ func/foo/1/not_deprecated/not_exported
            -spec doc_foo(integer() | atom()) -> [integer()].
            doc_foo(Bar) -> A = foo(Bar), [Bar, A].
        %%          ^^^ var/Bar :: number() | atom()
        %%          ^^^ doc/Bar :: number() | atom()
        %%                          ^^^ var/Bar :: number() | atom()
        %%                          ^^^ doc/Bar :: number() | atom()
        %%                                 ^^^ var/Bar :: number() | atom()
        %%                                 ^^^ doc/Bar :: number() | atom()
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ func/doc_foo/1/not_deprecated/not_exported
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ doc/-spec doc_foo(integer() | atom()) -> [integer()].
        "#;
            decl_v2_check(&spec);
        }
    }

    #[test]
    fn declaration_in_header_v2_test() {
        let spec = r#"
        //- /glean/app_glean/include/glean_module5.hrl include_path:/include
        % header
        %%^ header/glean_module5.hrl
            -define(TAU, 6.28).
        %%  ^^^^^^^^^^^^^^^^^^^ macro/TAU/no_arity
            -spec doc_bar(integer()) -> [integer()].
            doc_bar(Bar) -> [Bar].
        %%  ^^^^^^^^^^^^^^^^^^^^^^ func/doc_bar/1/not_deprecated/not_exported
        %%  ^^^^^^^^^^^^^^^^^^^^^^ doc/-spec doc_bar(integer()) -> [integer()].
        //- /glean/app_glean/src/glean_module5.erl
            -module(glean_module5).
        "#;
        decl_v2_check(&spec);
    }

    #[test]
    fn xref_call_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module61.erl
        -module(glean_module61).
        foo(Bar) -> Bar + 1.
        %%          ^^^ glean_module61/Bar/37
        %%  ^^^ glean_module61/Bar/29

        //- /glean/app_glean/src/glean_module6.erl
        main() ->
            B = baz(1, 2),
        %%  ^ glean_module6/B/14
        %%      ^^^ glean_module6/baz/2
            F = glean_module61:foo(B),
        %%                         ^ glean_module6/B/113
        %%  ^ glean_module6/F/90
        %%      ^^^^^^^^^^^^^^^^^^ glean_module61/foo/1
            0.
        baz(1, 2) ->
            1 + 2.
        "#;

        xref_check(&spec);
    }

    #[test]
    fn xref_call_v2_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module61.erl
        -module(glean_module61).
        foo(Bar) -> Bar + 1.

        //- /glean/app_glean/src/glean_module6.erl
        main() ->
            B = baz(1, 2),
        %%      ^^^ glean_module6.erl/func/baz/2
            F = glean_module61:foo(B),
        %%      ^^^^^^^^^^^^^^^^^^ glean_module61.erl/func/foo/1
            F.
        baz(A, B) ->
            A + B."#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_export_v2_test() {
        let spec = r#"
        //- /src/glean_module61.erl
        -module(glean_module61).
        -export([foo/1]).
        %%       ^^^^^ glean_module61.erl/func/foo/1
        foo(Bar) -> Bar + 1.
        "#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_deprecated_v2_test() {
        let spec = r#"
        //- /src/glean_module61.erl
        -module(glean_module61).
        -deprecated([{foo, 1}]).
        %%            ^^^ glean_module61.erl/func/foo/1
        -deprecated([{bar, '_'}]).
        %%            ^^^ glean_module61.erl/func/bar/0
        foo(Bar) -> Bar + 1.
        bar() -> 1.
        bar(A) -> A.
        "#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_header_v2_test() {
        let spec = r#"
        //- /kernel/include/logger.hrl include_path:/include app:kernel
            % empty

        //- /include/macro.hrl include_path:/include
            % empty

        //- /src/glean_module61.erl
            -module(glean_module61).
            -include_lib("kernel/include/logger.hrl").
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ logger.hrl/header
            -include("macro.hrl").
        %%  ^^^^^^^^^^^^^^^^^^^^^^ macro.hrl/header
        "#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_captured_fun_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module71.erl
        foo(Bar) -> Bar + 1.
        %%  ^^^ glean_module71/Bar/4
        %%          ^^^ glean_module71/Bar/12

        //- /glean/app_glean/src/glean_module7.erl
        main() ->
            Foo = fun glean_module71:foo/1,
        %%        ^^^^^^^^^^^^^^^^^^^^^^^^ glean_module71/foo/1
        %%  ^^^ glean_module7/Foo/14
            Baz = fun baz/2.
        %%  ^^^ glean_module7/Baz/135
        %%        ^^^^^^^^^ glean_module7/baz/2
        baz(1, 2) ->
            1 + 2."#;

        xref_check(&spec);
    }

    #[test]
    fn xref_captured_fun_v2_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module71.erl
        foo(Bar) -> Bar + 1.

        //- /glean/app_glean/src/glean_module7.erl
        main() ->
            Foo = fun glean_module71:foo/1,
        %%        ^^^^^^^^^^^^^^^^^^^^^^^^ glean_module71.erl/func/foo/1
            Baz = fun baz/2.
        %%        ^^^^^^^^^ glean_module7.erl/func/baz/2
        baz(A, B) ->
            A + B."#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_types_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module81.erl
        -type small() :: #{non_neg_integer() | infinity}.

        //- /glean/app_glean/src/glean_module8.erl
        -type huuuge() :: #{non_neg_integer() | infinity}.
        -spec baz(
            A :: huuuge(),
        %%       ^^^^^^^^ glean_module8/huuuge/0
            B :: glean_module81:small()
        %%       ^^^^^^^^^^^^^^^^^^^^^^ glean_module81/small/0
        ) -> huuuge().
        %%   ^^^^^^^^ glean_module8/huuuge/0
        baz(A, B) ->
        %%  ^ glean_module8/A/265
        %%     ^ glean_module8/B/268
            1 + 2."#;

        xref_check(&spec);
    }

    #[test]
    fn xref_types_v2_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module81.erl
        -type small() :: #{non_neg_integer() | infinity}.

        //- /glean/app_glean/src/glean_module8.erl
        -type huuuge() :: #{non_neg_integer() | infinity}.
        -spec baz(
            A :: huuuge(),
        %%       ^^^^^^^^ glean_module8.erl/type/huuuge/0
            B :: glean_module81:small()
        %%       ^^^^^^^^^^^^^^^^^^^^^^ glean_module81.erl/type/small/0
        ) -> huuuge().
        %%   ^^^^^^^^ glean_module8.erl/type/huuuge/0
        baz(A, B) ->
            A + B."#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_record_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module9.erl
        -record(query, {
            size :: non_neg_integer()
        }).
        baz(A) ->
        %%  ^ glean_module9/A/55
            #query{ size = 1 }.
        %%  ^^^^^^ glean_module9/query/99
        "#;

        xref_check(&spec);
    }

    #[test]
    fn xref_record_v2_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module9.erl
        -record(query, {
            size :: non_neg_integer()
        }).
        baz(A) ->
            #query{ size = A }.
        %%  ^^^^^^ glean_module9.erl/rec/query
        "#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_record_index_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module10.erl
        -record(stats, {count, time}).
        baz(Time) ->
        %%  ^^^^ glean_module10/Time/35
            [{#stats.count, 1},
        %%    ^^^^^^ glean_module10/stats/99
            {#stats.time, 2}].
        %%   ^^^^^^ glean_module10/stats/99

        "#;

        xref_check(&spec);
    }

    #[test]
    fn xref_record_index_v2_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module10.erl
        -record(stats, {count, time}).
        baz(Time) ->
            [{#stats.count, 1},
        %%    ^^^^^^ glean_module10.erl/rec/stats
            {#stats.time, Time}].
        %%   ^^^^^^ glean_module10.erl/rec/stats

        "#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_record_field_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module11.erl
        -record(stats, {count, time}).
        baz(Stats) ->
        %%  ^^^^^ glean_module11/Stats/35
            Stats#stats.count.
        %%       ^^^^^^ glean_module11/stats/99
        %%  ^^^^^ glean_module11/Stats/83
        "#;

        xref_check(&spec);
    }

    #[test]
    fn xref_record_field_v2_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module11.erl
        -record(stats, {count, time}).
        baz(Stats) ->
            Stats#stats.count.
        %%       ^^^^^^ glean_module11.erl/rec/stats
        "#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_record_update_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module12.erl
        -record(stats, {count, time}).
        baz(Stats, 1) ->
        %%  ^^^^^ glean_module12/Stats/35
            Stats#stats{count = 1}.
        %%       ^^^^^^ glean_module12/stats/99
        %%  ^^^^^ glean_module12/Stats/86
        "#;

        xref_check(&spec);
    }

    #[test]
    fn xref_record_update_v2_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module12.erl
        -record(stats, {count, time}).
        baz(Stats, NewCnt) ->
            Stats#stats{count = NewCnt}.
        %%       ^^^^^^ glean_module12.erl/rec/stats
        "#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_pat_record_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module13.erl
        -record(stats, {count, time}).
        baz(Stats) ->
        %%  ^^^^^ glean_module13/Stats/35
            #stats{count = Count, time = Time} = Stats.
        %%                 ^^^^^ glean_module13/Count/98
        %%                               ^^^^ glean_module13/Time/112
        %%                                       ^^^^^ glean_module13/Stats/120
        %%  ^^^^^^ glean_module13/stats/99
        "#;

        xref_check(&spec);
    }

    #[test]
    fn xref_pat_record_v2_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module13.erl
        -record(stats, {count, time}).
        baz(Stats) ->
            #stats{count = Count, time = Time} = Stats.
        %%  ^^^^^^ glean_module13.erl/rec/stats
        "#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_pat_record_index() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module14.erl
        -record(rec, {field}).
        foo(#rec.field) -> ok.
        %%  ^^^^ glean_module14/rec/99
        "#;

        xref_check(&spec);
    }

    #[test]
    fn xref_pat_recordindex() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module14.erl
        -record(rec, {field}).
        foo(#rec.field) -> ok.
        %%  ^^^^ glean_module14.erl/rec/rec
        "#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_record_in_type_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module15.erl
        -record(stats, {count, time}).
        -spec baz() -> #stats{}.
        %%             ^^^^^^ glean_module15/stats/99
        baz() ->
            #stats{count = 1, time = 2}.
        %%  ^^^^^^ glean_module15/stats/99
        "#;
        xref_check(&spec);
    }

    #[test]
    fn xref_record_in_type_v2_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module15.erl
        -record(stats, {count, time}).
        -spec baz() -> #stats{}.
        %%             ^^^^^^ glean_module15.erl/rec/stats
        baz() ->
            #stats{count = 1, time = 2}.
        %%  ^^^^^^ glean_module15.erl/rec/stats
        "#;

        xref_v2_check(&spec);
    }

    #[test]
    fn xref_macro_v2_test() {
        let spec = r#"
        //- /include/macro.hrl include_path:/include
        -define(TAU, 6.28).

        //- /src/macro.erl
            -module(macro).
            -include("macro.hrl").
        %%  ^^^^^^^^^^^^^^^^^^^^^^ macro.hrl/header
            -define(MAX(X, Y), if X > Y -> X; true -> Y end).

            baz(1) -> ?TAU;
        %%             ^^^ macro.erl/macro/TAU/161/no_ods/6.28
            baz(N) -> ?MAX(N, 200).
        %%             ^^^ macro.erl/macro/MAX/236/no_ods/if (N > 200) -> N; 'true' -> 200 end

        "#;
        xref_v2_check(&spec);
    }

    #[test]
    fn xref_macro_ods_v2_test() {
        let spec = r#"
        //- /src/macro.erl
            -module(macro).
            -define(COUNT_INFRA(X), X).
            baz(atom) -> ?COUNT_INFRA(atom),
        %%                ^^^^^^^^^^^ macro.erl/macro/COUNT_INFRA/70/has_ods/'atom'

        "#;
        // @fb-only
    }

    #[test]
    fn xref_macro_in_pat_v2_test() {
        let spec = r#"
        //- /src/macro.erl
            -module(macro).
            -define(TAU, 6.28).

            baz(?TAU) -> 1.
        %%       ^^^ macro.erl/macro/TAU/54/no_ods/6.28

        "#;
        xref_v2_check(&spec);
    }

    #[test]
    fn xref_macro_in_type_v2_test() {
        let spec = r#"
        //- /src/macro.erl
            -module(macro).
            -define(TYPE, integer()).

            -spec baz(ok) -> ?TYPE.
        %%                    ^^^^ macro.erl/macro/TYPE/73/no_ods/'erlang':'integer'()
            baz(ok) -> 1.

        "#;
        xref_v2_check(&spec);
    }

    #[test]
    fn xref_macro_in_term_v2_test() {
        let spec = r#"
        //- /src/macro.erl
            -module(macro).
           -define(FOO(X), X).
           -wild(?FOO(atom)).
        %%        ^^^ macro.erl/macro/FOO/53/no_ods/'atom'

        "#;
        xref_v2_check(&spec);
    }

    fn facts_with_annotations(
        spec: &str,
    ) -> (
        IndexedFacts,
        HashMap<GleanFileId, Vec<(TextRange, String)>>,
        HashMap<GleanFileId, String>,
        DiagnosticsEnabled,
        FxHashMap<GleanFileId, String>,
    ) {
        let config = IndexConfig::default();
        facts_with_annotations_with_config(spec, config)
    }

    fn facts_with_annotations_with_config(
        spec: &str,
        config: IndexConfig,
    ) -> (
        IndexedFacts,
        HashMap<GleanFileId, Vec<(TextRange, String)>>,
        HashMap<GleanFileId, String>,
        DiagnosticsEnabled,
        FxHashMap<GleanFileId, String>,
    ) {
        let (db, files, diag) = RootDatabase::with_many_files(spec);
        let project_id = ProjectId(0);
        let host = AnalysisHost::new(db);
        let glean = GleanIndexer {
            project_id,
            analysis: host.analysis(),
            module: None,
        };
        let (facts, module_index) = glean.index(config).expect("success");
        let facts = facts.into_values().next().unwrap();
        let mut expected_by_file: HashMap<GleanFileId, _> = HashMap::new();
        let mut file_names = HashMap::new();
        let db = host.raw_database();
        for file_id in files {
            let source_root_id = db.file_source_root(file_id);
            let source_root = db.source_root(source_root_id);
            let path = source_root.path_for_file(&file_id).unwrap();
            let (name, ext) = path.name_and_extension().unwrap();
            let name = format!("{}.{}", name, ext.unwrap());
            file_names.insert(file_id.into(), name);
            let text = db.file_text(file_id);
            let annotations: Vec<_> = extract_annotations(&text);
            expected_by_file.insert(file_id.into(), annotations);
        }
        (facts, expected_by_file, file_names, diag, module_index)
    }

    fn xref_check(spec: &str) {
        let (facts, mut expected_by_file, _, _d, module_index) = facts_with_annotations(spec);
        let facts = facts.to_v2_facts(&module_index);
        let xref_facts = facts
            .iter()
            .find_map(|x| match x {
                Fact::XRef { facts } => Some(facts),
                _ => None,
            })
            .unwrap();
        for xref_fact in xref_facts {
            let file_id = &xref_fact.key.file_id;
            let mut annotations = expected_by_file
                .remove(file_id)
                .expect("Annotations shold be present");
            for xref in &xref_fact.key.xrefs {
                let range: TextRange = xref.source.clone().into();
                let label = xref.target.to_string();
                let tuple = (range, label);
                let idx = annotations.iter().position(|a| a == &tuple).expect(
                    format!("Expected to find {:?} in {:?}", &tuple, &annotations).as_str(),
                );
                annotations.remove(idx);
            }
            assert_eq!(annotations, vec![], "Expected no more annotations");
        }
        assert_eq!(
            expected_by_file,
            HashMap::new(),
            "Expected no more annotations"
        );
    }

    #[track_caller]
    fn xref_v2_check(spec: &str) {
        let (facts, mut expected_by_file, file_names, _d, _) = facts_with_annotations(spec);
        for xref_fact in facts.xref_v2 {
            let file_id = xref_fact.file_id;
            let mut annotations = expected_by_file
                .remove(&file_id)
                .expect("Annotations shold be present");
            for xref in xref_fact.xrefs {
                let range: TextRange = xref.source.clone().into();
                let file_name = file_names
                    .get(xref.target.file_id())
                    .expect("must be present");
                let label = xref.target.to_string();
                if label.is_empty() {
                    continue;
                }
                let label = format!("{}/{}", file_name, label);
                let tuple = (range, label);
                let idx = annotations.iter().position(|a| a == &tuple).expect(
                    format!("Expected to find {:?} in {:?}", &tuple, &annotations).as_str(),
                );
                annotations.remove(idx);
            }
            assert_eq!(annotations, vec![], "Expected no more annotations");
        }
        assert_eq!(
            expected_by_file,
            HashMap::new(),
            "Expected no more annotations"
        );
    }

    fn decl_check(spec: &str) {
        let (facts, mut expected_by_file, _, _d, module_index) = facts_with_annotations(spec);
        let facts = facts.to_v2_facts(&module_index);
        let func_decl = facts
            .iter()
            .find_map(|x| match x {
                Fact::FunctionDeclaration { facts } => Some(facts),
                _ => None,
            })
            .unwrap();
        let file_id = &func_decl[0].key.file_id;
        let mut annotations = expected_by_file
            .remove(file_id)
            .expect("Annotations shold be present");
        for decl in func_decl {
            let range: TextRange = decl.key.span.clone().into();
            let label = decl.key.fqn.to_string();
            let tuple = (range, label);
            let idx = annotations
                .iter()
                .position(|a| a == &tuple)
                .expect(format!("Expected to find {:?} in {:?}", &tuple, &annotations).as_str());
            annotations.remove(idx);
        }
        assert_eq!(annotations, vec![], "Expected no more annotations");
        assert_eq!(
            expected_by_file,
            HashMap::new(),
            "Expected no more annotations"
        );
    }

    fn decl_v2_check(spec: &str) {
        let (facts, mut expected_by_file, _, _d, _) = facts_with_annotations(spec);
        let hash_map = &expected_by_file.clone();
        let fixture_files = FxHashSet::from_iter(hash_map.keys());
        for file_decl in facts.file_declarations {
            // Skip files not in the fixture, from OTP
            if fixture_files.contains(&file_decl.file_id) {
                let mut annotations = expected_by_file
                    .remove(&file_decl.file_id)
                    .expect("Annotations shold be present");
                for decl in file_decl.declarations {
                    let range: TextRange = decl.span().clone().into();
                    let label = decl.to_string();
                    let tuple = (range, label);
                    let idx = annotations.iter().position(|a| a == &tuple).expect(
                        format!("Expected to find {:?} in {:?}", &tuple, &annotations).as_str(),
                    );
                    annotations.remove(idx);
                }
                assert_eq!(annotations, vec![], "Expected no more annotations");
            }
        }

        assert_eq!(
            expected_by_file,
            HashMap::new(),
            "Expected no more annotations"
        );
    }

    impl Declaration {
        fn span(&self) -> Location {
            match self {
                Declaration::FunctionDeclaration(decl) => decl.key.span.clone(),
                Declaration::MacroDeclaration(decl) => decl.key.span.clone(),
                Declaration::TypeDeclaration(decl) => decl.key.span.clone(),
                Declaration::RecordDeclaration(decl) => decl.key.span.clone(),
                Declaration::VarDeclaration(decl) => decl.key.span.clone(),
                Declaration::HeaderDeclaration(decl) => Location {
                    start: decl.key.span.start + 2,
                    length: decl.key.span.length,
                },
                Declaration::DocDeclaration(decl) => decl.key.target.span().clone(),
            }
        }
    }

    impl fmt::Display for Declaration {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Declaration::FunctionDeclaration(decl) => {
                    let deprecated = match decl.key.deprecated {
                        true => "deprecated",
                        false => "not_deprecated",
                    };
                    let exported = match decl.key.exported {
                        true => "exported",
                        false => "not_exported",
                    };
                    f.write_str(
                        format!(
                            "func/{}/{}/{}/{}",
                            decl.key.name, decl.key.arity, deprecated, exported
                        )
                        .as_str(),
                    )
                }
                Declaration::MacroDeclaration(decl) => {
                    let arity = match &decl.key.arity {
                        Some(arity) => arity.to_string(),
                        None => "no_arity".to_string(),
                    };
                    f.write_str(format!("macro/{}/{}", decl.key.name, arity).as_str())
                }
                Declaration::TypeDeclaration(decl) => {
                    let exported = match decl.key.exported {
                        true => "exported",
                        false => "not_exported",
                    };
                    f.write_str(
                        format!("type/{}/{}/{}", decl.key.name, decl.key.arity, exported).as_str(),
                    )
                }
                Declaration::RecordDeclaration(decl) => {
                    f.write_str(format!("rec/{}", decl.key.name).as_str())
                }
                Declaration::VarDeclaration(decl) => {
                    let ttype = decl
                        .key
                        .doc
                        .strip_prefix("```erlang\n")
                        .unwrap()
                        .strip_suffix("\n```")
                        .unwrap()
                        .to_string();
                    f.write_str(format!("var/{}", ttype).as_str())
                }
                Declaration::HeaderDeclaration(decl) => {
                    f.write_str(format!("header/{}", decl.key.name).as_str())
                }
                Declaration::DocDeclaration(decl) => f.write_str(
                    format!(
                        "doc/{}",
                        decl.key
                            .text
                            .replace("```erlang\n", "")
                            .replace("\n```", "")
                    )
                    .as_str(),
                ),
            }
        }
    }

    impl fmt::Display for XRefTarget {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                XRefTarget::Function(xref) => {
                    f.write_str(format!("func/{}/{}", xref.key.name, xref.key.arity).as_str())
                }
                XRefTarget::Macro(xref) => {
                    let arity = match &xref.key.arity {
                        Some(arity) => arity.to_string(),
                        None => "no_arity".to_string(),
                    };
                    let ods_link = match &xref.key.ods_url {
                        Some(_) => "has_ods",
                        None => "no_ods",
                    };
                    let exp = match &xref.key.expansion {
                        Some(exp) => exp
                            .strip_prefix("```erlang\n")
                            .unwrap()
                            .strip_suffix("\n```")
                            .unwrap()
                            .replace("\n", " ")
                            .split_whitespace()
                            .join(" "),
                        None => "no_exp".to_string(),
                    };
                    f.write_str(
                        format!("macro/{}/{}/{}/{}", xref.key.name, arity, ods_link, exp).as_str(),
                    )
                }
                XRefTarget::Header(_) => f.write_str("header"),
                XRefTarget::Record(xref) => f.write_str(format!("rec/{}", xref.key.name).as_str()),
                XRefTarget::Type(xref) => {
                    f.write_str(format!("type/{}/{}", xref.key.name, xref.key.arity).as_str())
                }
                XRefTarget::Var(_) => Ok(()),
            }
        }
    }

    impl fmt::Display for MFA {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str(format!("{}/{}/{}", self.module, self.name, self.arity).as_str())
        }
    }
}
