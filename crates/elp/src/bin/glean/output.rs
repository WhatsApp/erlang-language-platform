/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::mem;

use fxhash::FxHashMap;
use itertools::Itertools;

use super::types::CommentFact;
use super::types::Declaration;
use super::types::Fact;
use super::types::FileDeclaration;
use super::types::FileFact;
use super::types::FileLinesFact;
use super::types::FunctionDeclarationFact;
use super::types::GleanFileId;
use super::types::MFA;
use super::types::ModuleFact;
use super::types::XRefFact;
use super::types::XRefFactVal;
use super::types::XRefFile;
use super::types::XRefTarget;

const REC_ARITY: u32 = 99;
const HEADER_ARITY: u32 = 100;

#[derive(Debug, Default)]
pub(crate) struct IndexedFacts {
    pub(crate) file_facts: Vec<FileFact>,
    pub(crate) file_line_facts: Vec<FileLinesFact>,
    pub(crate) module_facts: Vec<ModuleFact>,
    pub(crate) file_declarations: Vec<FileDeclaration>,
    pub(crate) xrefs: Vec<XRefFile>,
}

impl IndexedFacts {
    pub(crate) fn new(
        file_fact: FileFact,
        line_fact: FileLinesFact,
        decl: FileDeclaration,
        xref: XRefFile,
        module_fact: Option<ModuleFact>,
    ) -> Self {
        let mut facts = Self::default();
        facts.file_facts.push(file_fact);
        facts.file_line_facts.push(line_fact);
        facts.file_declarations.push(decl);
        facts.xrefs.push(xref);
        if let Some(module_fact) = module_fact {
            facts.module_facts.push(module_fact);
        }
        facts
    }

    pub(crate) fn add(
        &mut self,
        file_fact: FileFact,
        line_fact: FileLinesFact,
        decl: FileDeclaration,
        xref: XRefFile,
        module_fact: Option<ModuleFact>,
    ) {
        self.file_facts.push(file_fact);
        self.file_line_facts.push(line_fact);
        self.file_declarations.push(decl);
        self.xrefs.push(xref);
        if let Some(module_fact) = module_fact {
            self.module_facts.push(module_fact);
        }
    }

    fn declaration_to_fact(
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

    pub(crate) fn into_glean_facts(
        mut self,
        modules: &FxHashMap<GleanFileId, String>,
    ) -> Vec<Fact> {
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
                        if let Some(target) = Self::declaration_to_fact(
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
                    if let Some(fact) = Self::declaration_to_fact(d, file_id, module.clone()) {
                        declarations.push(fact);
                    }
                }
            }
        }
        let declaration_fact = declarations.into_iter().map_into().collect();
        let xref_fact = mem::take(&mut self.xrefs);
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
        let module_facts = mem::take(&mut self.module_facts);
        let module_facts = module_facts.into_iter().map_into().collect();
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
            Fact::Module {
                facts: module_facts,
            },
        ]
    }
}

/// Metrics collected during a Glean indexing run.
#[derive(Debug, serde::Serialize)]
pub struct IndexerMetrics {
    pub duration_ms: u64,
    pub file_count: usize,
    pub module_count: usize,
    pub entity_count: usize,
    pub xref_count: usize,
    pub output_bytes: u64,
    pub files_errored: usize,
    pub success: bool,
    pub error_message: Option<String>,
}

impl IndexerMetrics {
    pub(crate) fn from_facts(
        facts: &FxHashMap<String, IndexedFacts>,
        files_errored: usize,
    ) -> Self {
        let mut file_count = 0;
        let mut module_count = 0;
        let mut entity_count = 0;
        let mut xref_count = 0;

        for indexed in facts.values() {
            file_count += indexed.file_facts.len();
            module_count += indexed.module_facts.len();
            entity_count += indexed
                .file_declarations
                .iter()
                .map(|fd| fd.declarations.len())
                .sum::<usize>();
            xref_count += indexed.xrefs.iter().map(|f| f.xrefs.len()).sum::<usize>();
        }

        Self {
            duration_ms: 0,
            file_count,
            module_count,
            entity_count,
            xref_count,
            output_bytes: 0,
            files_errored,
            success: true,
            error_message: None,
        }
    }

    pub(crate) fn failed(duration_ms: u64, error: &anyhow::Error) -> Self {
        Self {
            duration_ms,
            file_count: 0,
            module_count: 0,
            entity_count: 0,
            xref_count: 0,
            output_bytes: 0,
            files_errored: 0,
            success: false,
            error_message: Some(format!("{error:#}")),
        }
    }
}
