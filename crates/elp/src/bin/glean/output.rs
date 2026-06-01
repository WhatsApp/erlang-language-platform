/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::hash::Hash;
use std::hash::Hasher;
use std::mem;

use fxhash::FxHashMap;
use fxhash::FxHashSet;
use itertools::Itertools;

use super::types::GleanFileId;
use super::types::Key;
use super::types::Location;
use super::types::glean;
use super::types::parser;

#[derive(Debug, Default)]
pub(crate) struct FactCounts {
    pub(crate) file_count: usize,
    pub(crate) module_count: usize,
    pub(crate) declaration_count: usize,
    pub(crate) var_decl_count: usize,
    pub(crate) macro_usage_count: usize,
    pub(crate) xref_count: usize,
    pub(crate) doc_count: usize,
}

impl FactCounts {
    pub(crate) fn accumulate(&mut self, other: &FactCounts) {
        self.file_count += other.file_count;
        self.module_count += other.module_count;
        self.declaration_count += other.declaration_count;
        self.var_decl_count += other.var_decl_count;
        self.macro_usage_count += other.macro_usage_count;
        self.xref_count += other.xref_count;
        self.doc_count += other.doc_count;
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct IndexedFacts {
    pub(crate) file_facts: Vec<glean::FileFact>,
    pub(crate) file_line_facts: Vec<glean::FileLinesFact>,
    pub(crate) module_facts: Vec<parser::ModuleFact>,
    pub(crate) file_declarations: Vec<parser::FileDeclaration>,
    pub(crate) xrefs: Vec<parser::XRefFile>,
}

impl IndexedFacts {
    pub(crate) fn new(
        file_fact: glean::FileFact,
        line_fact: glean::FileLinesFact,
        decl: parser::FileDeclaration,
        xref: parser::XRefFile,
        module_fact: Option<parser::ModuleFact>,
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
        file_fact: glean::FileFact,
        line_fact: glean::FileLinesFact,
        decl: parser::FileDeclaration,
        xref: parser::XRefFile,
        module_fact: Option<parser::ModuleFact>,
    ) {
        self.file_facts.push(file_fact);
        self.file_line_facts.push(line_fact);
        self.file_declarations.push(decl);
        self.xrefs.push(xref);
        if let Some(module_fact) = module_fact {
            self.module_facts.push(module_fact);
        }
    }

    /// Convert internal parser facts into `erlang.2` Glean facts.
    pub(crate) fn into_glean_facts(
        mut self,
        modules: &FxHashMap<GleanFileId, String>,
        apps: &FxHashMap<GleanFileId, String>,
    ) -> (Vec<glean::Fact>, FactCounts) {
        let unknown = "unknown".to_string();
        let mut known_files: FxHashSet<GleanFileId> =
            self.file_facts.iter().map(|f| f.file_id.clone()).collect();
        let mut extra_file_facts: Vec<glean::FileFact> = vec![];

        let mut func_decls: Vec<Key<glean::FuncDecl>> = vec![];
        let mut macro_decls: Vec<Key<glean::MacroDecl>> = vec![];
        let mut record_decls: Vec<Key<glean::RecordDecl>> = vec![];
        let mut type_decls: Vec<Key<glean::TypeDecl>> = vec![];
        let mut header_decls: Vec<Key<glean::HeaderDecl>> = vec![];
        let mut callback_decls: Vec<Key<glean::CallbackDecl>> = vec![];
        let mut record_field_decls: Vec<Key<glean::RecordFieldDecl>> = vec![];
        let mut var_decls: Vec<Key<glean::VarDecl>> = vec![];
        let mut func_defs: Vec<Key<glean::FuncDef>> = vec![];
        let mut macro_defs: Vec<Key<glean::MacroDef>> = vec![];
        let mut record_defs: Vec<Key<glean::RecordDef>> = vec![];
        let mut type_defs: Vec<Key<glean::TypeDef>> = vec![];
        let mut callback_defs: Vec<Key<glean::CallbackDef>> = vec![];
        let mut decl_locations: Vec<Key<glean::DeclLocation>> = vec![];
        let mut var_locations: Vec<Key<glean::VarLocation>> = vec![];
        let mut comments: Vec<Key<glean::CommentFact>> = vec![];
        let mut file_decls_list: Vec<Key<glean::FileDeclarations>> = vec![];
        let mut macro_usages: Vec<Key<glean::MacroUsage>> = vec![];
        let mut macro_usage_locations: Vec<Key<glean::MacroUsageLocation>> = vec![];

        let on_load_by_file: FxHashMap<GleanFileId, Vec<String>> = self
            .module_facts
            .iter()
            .filter(|mf| !mf.on_load_fns.is_empty())
            .map(|mf| (mf.file_id.clone(), mf.on_load_fns.clone()))
            .collect();
        let nif_by_file: FxHashMap<GleanFileId, Vec<(String, u32)>> = self
            .module_facts
            .iter()
            .filter(|mf| !mf.nif_fns.is_empty())
            .map(|mf| (mf.file_id.clone(), mf.nif_fns.clone()))
            .collect();

        let file_declarations = mem::take(&mut self.file_declarations);
        for file_decl in file_declarations {
            let module = modules.get(&file_decl.file_id).unwrap_or(&unknown);
            let app = apps.get(&file_decl.file_id).unwrap_or(&unknown);
            let module_on_load_fns = on_load_by_file.get(&file_decl.file_id);
            let module_nif_fns = nif_by_file.get(&file_decl.file_id);
            let mut file_decls: Vec<glean::Declaration> = vec![];

            for d in file_decl.declarations {
                match d {
                    parser::Declaration::FunctionDeclaration(ref f) => {
                        let decl = glean::FuncDecl {
                            fqn: glean::Fqn {
                                module: module.clone(),
                                name: f.key.name.clone(),
                                arity: f.key.arity,
                            },
                            app: app.clone(),
                        };
                        let glean_decl = glean::Declaration::Func(decl.clone().into());
                        decl_locations.push(
                            glean::DeclLocation {
                                declaration: glean_decl.clone(),
                                file_id: file_decl.file_id.clone(),
                                span: f.key.span.clone(),
                            }
                            .into(),
                        );
                        let is_on_load =
                            module_on_load_fns.is_some_and(|fns| fns.contains(&f.key.name));
                        func_defs.push(
                            glean::FuncDef {
                                declaration: decl.clone().into(),
                                exported: f.key.exported,
                                deprecated: if f.key.deprecated {
                                    Some(
                                        f.key
                                            .deprecated_desc
                                            .clone()
                                            .unwrap_or_else(|| "true".to_string()),
                                    )
                                } else {
                                    None
                                },
                                is_on_load,
                                is_nif: module_nif_fns.is_some_and(|fns| {
                                    fns.iter().any(|(name, arity)| {
                                        name == &f.key.name && *arity == f.key.arity
                                    })
                                }),
                                spec_text: f.key.spec_text.clone(),
                            }
                            .into(),
                        );
                        file_decls.push(glean_decl);
                        func_decls.push(decl.into());
                    }
                    parser::Declaration::MacroDeclaration(ref m) => {
                        let decl = glean::MacroDecl {
                            name: m.key.name.clone(),
                            arity: m.key.arity,
                            module: module.clone(),
                            app: app.clone(),
                        };
                        let glean_decl = glean::Declaration::Macro(decl.clone().into());
                        decl_locations.push(
                            glean::DeclLocation {
                                declaration: glean_decl.clone(),
                                file_id: file_decl.file_id.clone(),
                                span: m.key.span.clone(),
                            }
                            .into(),
                        );
                        file_decls.push(glean_decl);
                        if m.key.definition_text.is_some() {
                            macro_defs.push(
                                glean::MacroDef {
                                    declaration: decl.clone().into(),
                                    definition_text: m.key.definition_text.clone(),
                                }
                                .into(),
                            );
                        }
                        macro_decls.push(decl.into());
                    }
                    parser::Declaration::TypeDeclaration(ref t) => {
                        let decl = glean::TypeDecl {
                            name: t.key.name.clone(),
                            arity: t.key.arity,
                            module: module.clone(),
                            app: app.clone(),
                        };
                        let glean_decl = glean::Declaration::Type(decl.clone().into());
                        decl_locations.push(
                            glean::DeclLocation {
                                declaration: glean_decl.clone(),
                                file_id: file_decl.file_id.clone(),
                                span: t.key.span.clone(),
                            }
                            .into(),
                        );
                        type_defs.push(
                            glean::TypeDef {
                                declaration: decl.clone().into(),
                                exported: t.key.exported,
                                opaque: t.key.opaque,
                                definition_text: t.key.definition_text.clone(),
                            }
                            .into(),
                        );
                        file_decls.push(glean_decl);
                        type_decls.push(decl.into());
                    }
                    parser::Declaration::RecordDeclaration(ref r) => {
                        let decl = glean::RecordDecl {
                            name: r.key.name.clone(),
                            module: module.clone(),
                            app: app.clone(),
                        };
                        let glean_decl = glean::Declaration::Record(decl.clone().into());
                        decl_locations.push(
                            glean::DeclLocation {
                                declaration: glean_decl.clone(),
                                file_id: file_decl.file_id.clone(),
                                span: r.key.span.clone(),
                            }
                            .into(),
                        );
                        file_decls.push(glean_decl);
                        record_decls.push(decl.into());
                    }
                    parser::Declaration::HeaderDeclaration(ref h) => {
                        let decl = glean::HeaderDecl {
                            name: h.key.name.clone(),
                            app: app.clone(),
                        };
                        let glean_decl = glean::Declaration::Header(decl.clone().into());
                        decl_locations.push(
                            glean::DeclLocation {
                                declaration: glean_decl.clone(),
                                file_id: file_decl.file_id.clone(),
                                span: h.key.span.clone(),
                            }
                            .into(),
                        );
                        file_decls.push(glean_decl);
                        header_decls.push(decl.into());
                    }
                    parser::Declaration::VarDeclaration(ref v) => {
                        let decl = glean::VarDecl {
                            name: v.key.name.clone(),
                            module: module.clone(),
                            app: app.clone(),
                            span_start: v.key.span.start,
                            type_text: v.key.type_text.clone(),
                        };
                        var_locations.push(
                            glean::VarLocation {
                                var_: decl.clone().into(),
                                file_id: file_decl.file_id.clone(),
                                span: v.key.span.clone(),
                            }
                            .into(),
                        );
                        var_decls.push(decl.into());
                    }
                    parser::Declaration::DocDeclaration(ref doc) => {
                        if let Some(glean_target) = self.decl_to_glean(&doc.key.target, module, app)
                        {
                            comments.push(
                                glean::CommentFact {
                                    declaration: glean_target,
                                    file_id: file_decl.file_id.clone(),
                                    span: doc.key.span.clone(),
                                    text: Some(doc.key.text.clone()),
                                }
                                .into(),
                            );
                        }
                    }
                }
            }

            file_decls_list.push(
                glean::FileDeclarations {
                    file_id: file_decl.file_id.clone(),
                    declarations: file_decls,
                }
                .into(),
            );
        }

        // Convert xrefs to typed xrefs + collect DeclarationTarget facts
        let xref_files = mem::take(&mut self.xrefs);
        let mut typed_xrefs: Vec<Key<glean::XRefsByFile>> = vec![];
        let mut var_xrefs: Vec<Key<glean::VarXRefsByFile>> = vec![];
        let mut decl_targets: Vec<Key<glean::DeclarationTarget>> = vec![];
        let mut xref_count = 0usize;
        for xref_file in xref_files {
            let mut file_typed: Vec<glean::XRef> = vec![];
            let mut var_map: FxHashMap<(String, u32), Vec<Location>> = FxHashMap::default();
            for xref in xref_file.xrefs {
                xref_count += 1;
                let caller = xref.caller;
                match &xref.target {
                    parser::XRefTarget::Function(f) => {
                        let target_module = modules.get(&f.key.file_id).unwrap_or(&unknown);
                        let target_app = apps.get(&f.key.file_id).unwrap_or(&unknown);
                        file_typed.push(glean::XRef {
                            target: glean::Declaration::Func(
                                glean::FuncDecl {
                                    fqn: glean::Fqn {
                                        module: target_module.clone(),
                                        name: f.key.name.clone(),
                                        arity: f.key.arity,
                                    },
                                    app: target_app.clone(),
                                }
                                .into(),
                            ),
                            source: xref.source,
                        });
                    }
                    parser::XRefTarget::Macro(m) => {
                        let target_module = modules.get(&m.key.file_id).unwrap_or(&unknown);
                        let target_app = apps.get(&m.key.file_id).unwrap_or(&unknown);
                        file_typed.push(glean::XRef {
                            target: glean::Declaration::Macro(
                                glean::MacroDecl {
                                    name: m.key.name.clone(),
                                    arity: m.key.arity,
                                    module: target_module.clone(),
                                    app: target_app.clone(),
                                }
                                .into(),
                            ),
                            source: xref.source.clone(),
                        });

                        let links: Vec<String> = m
                            .key
                            .tagged_urls
                            .iter()
                            .map(|u| {
                                if u.url.is_empty() {
                                    u.display_name.clone()
                                } else {
                                    format!("[{}]({})", u.display_name, u.url)
                                }
                            })
                            .collect();
                        let expansion = {
                            let links_text = links.join("\n\n");
                            let exp = m.key.expansion.as_deref().unwrap_or("");
                            match (links_text.is_empty(), exp.is_empty()) {
                                (false, false) => Some(format!("{}\n\n---\n\n{}", links_text, exp)),
                                (false, true) => Some(links_text),
                                (true, false) => Some(exp.to_string()),
                                (true, true) => None,
                            }
                        };
                        let mut hasher = fxhash::FxHasher::default();
                        expansion.hash(&mut hasher);
                        let content_hash = format!("{}", hasher.finish());
                        let usage = glean::MacroUsage {
                            name: m.key.name.clone(),
                            module: target_module.clone(),
                            arity: m.key.arity,
                            app: target_app.clone(),
                            expansion,
                            links,
                            content_hash,
                        };
                        macro_usage_locations.push(
                            glean::MacroUsageLocation {
                                usage: usage.clone().into(),
                                file_id: xref_file.file_id.clone(),
                                span: xref.source,
                            }
                            .into(),
                        );
                        macro_usages.push(usage.into());
                    }
                    parser::XRefTarget::Header(h) => {
                        let target_app = apps.get(&h.key.file_id).unwrap_or(&unknown);
                        file_typed.push(glean::XRef {
                            target: glean::Declaration::Header(
                                glean::HeaderDecl {
                                    name: h.key.name.clone(),
                                    app: target_app.clone(),
                                }
                                .into(),
                            ),
                            source: xref.source,
                        });
                    }
                    parser::XRefTarget::Record(r) => {
                        let target_module = modules.get(&r.key.file_id).unwrap_or(&unknown);
                        let target_app = apps.get(&r.key.file_id).unwrap_or(&unknown);
                        file_typed.push(glean::XRef {
                            target: glean::Declaration::Record(
                                glean::RecordDecl {
                                    name: r.key.name.clone(),
                                    module: target_module.clone(),
                                    app: target_app.clone(),
                                }
                                .into(),
                            ),
                            source: xref.source,
                        });
                    }
                    parser::XRefTarget::Type(t) => {
                        let target_module = modules.get(&t.key.file_id).unwrap_or(&unknown);
                        let target_app = apps.get(&t.key.file_id).unwrap_or(&unknown);
                        file_typed.push(glean::XRef {
                            target: glean::Declaration::Type(
                                glean::TypeDecl {
                                    name: t.key.name.clone(),
                                    arity: t.key.arity,
                                    module: target_module.clone(),
                                    app: target_app.clone(),
                                }
                                .into(),
                            ),
                            source: xref.source,
                        });
                    }
                    parser::XRefTarget::Var(v) => {
                        let span_start = v.key.decl_span_start.unwrap_or(xref.source.start);
                        let key = (v.key.name.clone(), span_start);
                        var_map.entry(key).or_default().push(xref.source);
                    }
                    parser::XRefTarget::RecordField(rf) => {
                        let target_module = modules.get(&rf.key.file_id).unwrap_or(&unknown);
                        let target_app = apps.get(&rf.key.file_id).unwrap_or(&unknown);
                        file_typed.push(glean::XRef {
                            target: glean::Declaration::RecordField(
                                glean::RecordFieldDecl {
                                    record_name: rf.key.record_name.clone(),
                                    field_name: rf.key.field_name.clone(),
                                    module: target_module.clone(),
                                    app: target_app.clone(),
                                }
                                .into(),
                            ),
                            source: xref.source,
                        });
                    }
                    parser::XRefTarget::Callback(cb) => {
                        let target_module = modules.get(&cb.key.file_id).unwrap_or(&unknown);
                        let target_app = apps.get(&cb.key.file_id).unwrap_or(&unknown);
                        file_typed.push(glean::XRef {
                            target: glean::Declaration::Callback(
                                glean::CallbackDecl {
                                    name: cb.key.name.clone(),
                                    arity: cb.key.arity,
                                    module: target_module.clone(),
                                    app: target_app.clone(),
                                }
                                .into(),
                            ),
                            source: xref.source,
                        });
                    }
                }
                if let Some((caller_file_id, caller_name, caller_arity)) = caller {
                    let caller_module = modules.get(&caller_file_id).unwrap_or(&unknown).clone();
                    let caller_app = apps.get(&caller_file_id).unwrap_or(&unknown).clone();
                    let source_decl = glean::Declaration::Func(
                        glean::FuncDecl {
                            fqn: glean::Fqn {
                                module: caller_module,
                                name: caller_name,
                                arity: caller_arity,
                            },
                            app: caller_app,
                        }
                        .into(),
                    );
                    if let Some(target_decl) =
                        Self::xref_target_to_decl(&xref.target, modules, apps, &unknown)
                    {
                        decl_targets.push(
                            glean::DeclarationTarget {
                                source: source_decl,
                                target: target_decl,
                            }
                            .into(),
                        );
                    }
                }
            }
            if !file_typed.is_empty() {
                typed_xrefs.push(
                    glean::XRefsByFile {
                        file_id: xref_file.file_id.clone(),
                        xrefs: file_typed,
                    }
                    .into(),
                );
            }
            if !var_map.is_empty() {
                let file_vars: Vec<glean::VarXRef> = var_map
                    .into_iter()
                    .map(|((name, span_start), sources)| glean::VarXRef {
                        target_name: name,
                        target_span_start: span_start,
                        sources,
                    })
                    .collect();
                var_xrefs.push(
                    glean::VarXRefsByFile {
                        file_id: xref_file.file_id,
                        xrefs: file_vars,
                    }
                    .into(),
                );
            }
        }

        // Convert module facts to erlang.2 Module + ModuleDefinition
        let module_facts = mem::take(&mut self.module_facts);
        let mut module_decls: Vec<Key<glean::ModuleDecl>> = vec![];
        let mut module_defs: Vec<Key<glean::ModuleDef>> = vec![];
        let mut file_includes: Vec<Key<glean::FileIncludes>> = vec![];
        for mf in module_facts {
            let app = apps.get(&mf.file_id).unwrap_or(&unknown);
            let decl = glean::ModuleDecl {
                file_id: mf.file_id.clone(),
                name: mf.name.clone(),
                app: app.clone(),
            };
            if let Some(span) = &mf.module_span {
                decl_locations.push(
                    glean::DeclLocation {
                        declaration: glean::Declaration::Module(decl.clone().into()),
                        file_id: mf.file_id.clone(),
                        span: span.clone(),
                    }
                    .into(),
                );
            }
            let exports: Vec<Key<glean::FuncDecl>> = mf
                .exports
                .unwrap_or_default()
                .into_iter()
                .filter_map(|e| {
                    // exports are "name/arity" strings
                    let parts: Vec<&str> = e.splitn(2, '/').collect();
                    if parts.len() == 2 {
                        Some(
                            glean::FuncDecl {
                                fqn: glean::Fqn {
                                    module: mf.name.clone(),
                                    name: parts[0].to_string(),
                                    arity: parts[1].parse().unwrap_or(0),
                                },
                                app: app.clone(),
                            }
                            .into(),
                        )
                    } else {
                        None
                    }
                })
                .collect();
            record_defs.extend(mf.record_def_texts.iter().map(|rd| {
                glean::RecordDef {
                    declaration: glean::RecordDecl {
                        name: rd.name.clone(),
                        module: mf.name.clone(),
                        app: app.clone(),
                    }
                    .into(),
                    definition_text: Some(rd.definition_text.clone()),
                }
                .into()
            }));
            for rf in &mf.record_fields {
                let rf_decl = glean::RecordFieldDecl {
                    record_name: rf.record_name.clone(),
                    field_name: rf.field_name.clone(),
                    module: mf.name.clone(),
                    app: app.clone(),
                };
                decl_locations.push(
                    glean::DeclLocation {
                        declaration: glean::Declaration::RecordField(rf_decl.clone().into()),
                        file_id: mf.file_id.clone(),
                        span: rf.span.clone(),
                    }
                    .into(),
                );
                record_field_decls.push(rf_decl.into());
            }
            for (inc_id, inc_path) in &mf.included_files {
                if !known_files.contains(inc_id) {
                    extra_file_facts.push(glean::FileFact::new_from_glean_id(
                        inc_id.clone(),
                        inc_path.clone(),
                    ));
                    known_files.insert(inc_id.clone());
                }
                file_includes.push(
                    glean::FileIncludes {
                        file_id: mf.file_id.clone(),
                        included: inc_id.clone(),
                    }
                    .into(),
                );
            }
            for cb in &mf.callbacks {
                let cb_decl = glean::CallbackDecl {
                    name: cb.name.clone(),
                    arity: cb.arity,
                    module: mf.name.clone(),
                    app: app.clone(),
                };
                decl_locations.push(
                    glean::DeclLocation {
                        declaration: glean::Declaration::Callback(cb_decl.clone().into()),
                        file_id: mf.file_id.clone(),
                        span: cb.span.clone(),
                    }
                    .into(),
                );
                callback_defs.push(
                    glean::CallbackDef {
                        declaration: cb_decl.clone().into(),
                        optional_: cb.optional,
                    }
                    .into(),
                );
                callback_decls.push(cb_decl.into());
            }
            for stub in &mf.behaviour_callback_stubs {
                let cb_decl = glean::CallbackDecl {
                    name: stub.callback_name.clone(),
                    arity: stub.callback_arity,
                    module: stub.behaviour_module.clone(),
                    app: stub.behaviour_app.clone(),
                };
                callback_decls.push(cb_decl.into());
            }
            if let Some(doc) = mf.module_doc {
                comments.push(
                    glean::CommentFact {
                        declaration: glean::Declaration::Module(decl.clone().into()),
                        file_id: mf.file_id.clone(),
                        span: doc.span,
                        text: Some(doc.text),
                    }
                    .into(),
                );
            }
            module_defs.push(
                glean::ModuleDef {
                    declaration: decl.clone().into(),
                    oncall: mf.oncall,
                    exports,
                    behaviours: mf.behaviours.unwrap_or_default(),
                    exdoc_link: mf.exdoc_link,
                    compile_options: mf.compile_options,
                }
                .into(),
            );
            module_decls.push(decl.into());
        }

        let counts = FactCounts {
            file_count: self.file_facts.len(),
            module_count: module_decls.len(),
            declaration_count: func_decls.len()
                + macro_decls.len()
                + record_decls.len()
                + type_decls.len()
                + header_decls.len()
                + callback_decls.len()
                + record_field_decls.len()
                + module_decls.len(),
            var_decl_count: var_decls.len(),
            macro_usage_count: macro_usages.len(),
            xref_count,
            doc_count: comments.len(),
        };

        let mut file_facts = mem::take(&mut self.file_facts);
        file_facts.extend(extra_file_facts);
        let file_line_facts = mem::take(&mut self.file_line_facts)
            .into_iter()
            .map_into()
            .collect();

        let mut result = vec![
            glean::Fact::File { facts: file_facts },
            glean::Fact::FileLine {
                facts: file_line_facts,
            },
        ];
        result.extend([
            glean::Fact::FunctionDeclaration { facts: func_decls },
            glean::Fact::MacroDeclaration { facts: macro_decls },
            glean::Fact::RecordDeclaration {
                facts: record_decls,
            },
            glean::Fact::TypeDeclaration { facts: type_decls },
            glean::Fact::HeaderDeclaration {
                facts: header_decls,
            },
            glean::Fact::CallbackDeclaration {
                facts: callback_decls,
            },
            glean::Fact::RecordFieldDeclaration {
                facts: record_field_decls,
            },
            glean::Fact::ModuleDeclaration {
                facts: module_decls,
            },
            glean::Fact::VarDeclaration { facts: var_decls },
            glean::Fact::FunctionDefinition { facts: func_defs },
            glean::Fact::MacroDefinition { facts: macro_defs },
            glean::Fact::RecordDefinition { facts: record_defs },
            glean::Fact::TypeDefinition { facts: type_defs },
            glean::Fact::CallbackDefinition {
                facts: callback_defs,
            },
            glean::Fact::ModuleDefinition { facts: module_defs },
            glean::Fact::DeclarationLocation {
                facts: decl_locations,
            },
            glean::Fact::VarLocation {
                facts: var_locations,
            },
            glean::Fact::XRefsByFile { facts: typed_xrefs },
            glean::Fact::VarXRefsByFile { facts: var_xrefs },
            glean::Fact::FileDeclarations {
                facts: file_decls_list,
            },
            glean::Fact::DeclarationComment { facts: comments },
            glean::Fact::FileIncludes {
                facts: file_includes,
            },
            glean::Fact::MacroUsage {
                facts: macro_usages,
            },
            glean::Fact::MacroUsageLocation {
                facts: macro_usage_locations,
            },
            glean::Fact::DeclarationTarget {
                facts: decl_targets,
            },
        ]);
        (result, counts)
    }

    /// Helper: convert internal parser::Declaration to glean::Declaration
    fn decl_to_glean(
        &self,
        decl: &parser::Declaration,
        module: &str,
        app: &str,
    ) -> Option<glean::Declaration> {
        match decl {
            parser::Declaration::FunctionDeclaration(f) => Some(glean::Declaration::Func(
                glean::FuncDecl {
                    fqn: glean::Fqn {
                        module: module.to_string(),
                        name: f.key.name.clone(),
                        arity: f.key.arity,
                    },
                    app: app.to_string(),
                }
                .into(),
            )),
            parser::Declaration::MacroDeclaration(m) => Some(glean::Declaration::Macro(
                glean::MacroDecl {
                    name: m.key.name.clone(),
                    arity: m.key.arity,
                    module: module.to_string(),
                    app: app.to_string(),
                }
                .into(),
            )),
            parser::Declaration::RecordDeclaration(r) => Some(glean::Declaration::Record(
                glean::RecordDecl {
                    name: r.key.name.clone(),
                    module: module.to_string(),
                    app: app.to_string(),
                }
                .into(),
            )),
            parser::Declaration::TypeDeclaration(t) => Some(glean::Declaration::Type(
                glean::TypeDecl {
                    name: t.key.name.clone(),
                    arity: t.key.arity,
                    module: module.to_string(),
                    app: app.to_string(),
                }
                .into(),
            )),
            parser::Declaration::HeaderDeclaration(h) => Some(glean::Declaration::Header(
                glean::HeaderDecl {
                    name: h.key.name.clone(),
                    app: app.to_string(),
                }
                .into(),
            )),
            _ => None,
        }
    }

    fn xref_target_to_decl(
        target: &parser::XRefTarget,
        modules: &FxHashMap<GleanFileId, String>,
        apps: &FxHashMap<GleanFileId, String>,
        unknown: &String,
    ) -> Option<glean::Declaration> {
        match target {
            parser::XRefTarget::Function(f) => {
                let module = modules.get(&f.key.file_id).unwrap_or(unknown);
                let app = apps.get(&f.key.file_id).unwrap_or(unknown);
                Some(glean::Declaration::Func(
                    glean::FuncDecl {
                        fqn: glean::Fqn {
                            module: module.clone(),
                            name: f.key.name.clone(),
                            arity: f.key.arity,
                        },
                        app: app.clone(),
                    }
                    .into(),
                ))
            }
            parser::XRefTarget::Macro(m) => {
                let module = modules.get(&m.key.file_id).unwrap_or(unknown);
                let app = apps.get(&m.key.file_id).unwrap_or(unknown);
                Some(glean::Declaration::Macro(
                    glean::MacroDecl {
                        name: m.key.name.clone(),
                        arity: m.key.arity,
                        module: module.clone(),
                        app: app.clone(),
                    }
                    .into(),
                ))
            }
            parser::XRefTarget::Record(r) => {
                let module = modules.get(&r.key.file_id).unwrap_or(unknown);
                let app = apps.get(&r.key.file_id).unwrap_or(unknown);
                Some(glean::Declaration::Record(
                    glean::RecordDecl {
                        name: r.key.name.clone(),
                        module: module.clone(),
                        app: app.clone(),
                    }
                    .into(),
                ))
            }
            parser::XRefTarget::Type(t) => {
                let module = modules.get(&t.key.file_id).unwrap_or(unknown);
                let app = apps.get(&t.key.file_id).unwrap_or(unknown);
                Some(glean::Declaration::Type(
                    glean::TypeDecl {
                        name: t.key.name.clone(),
                        arity: t.key.arity,
                        module: module.clone(),
                        app: app.clone(),
                    }
                    .into(),
                ))
            }
            parser::XRefTarget::Header(h) => {
                let app = apps.get(&h.key.file_id).unwrap_or(unknown);
                Some(glean::Declaration::Header(
                    glean::HeaderDecl {
                        name: h.key.name.clone(),
                        app: app.clone(),
                    }
                    .into(),
                ))
            }
            parser::XRefTarget::Var(_)
            | parser::XRefTarget::RecordField(_)
            | parser::XRefTarget::Callback(_) => None,
        }
    }
}

/// Metrics collected during a Glean indexing run.
#[derive(Debug, serde::Serialize)]
pub struct IndexerMetrics {
    pub duration_ms: u64,
    pub file_count: usize,
    pub module_count: usize,
    pub entity_count: usize,
    pub declaration_count: usize,
    pub var_decl_count: usize,
    pub macro_usage_count: usize,
    pub xref_count: usize,
    pub doc_count: usize,
    pub output_bytes: u64,
    pub files_errored: usize,
    pub success: bool,
    pub error_message: Option<String>,
}

impl IndexerMetrics {
    pub(crate) fn from_counts(counts: FactCounts, files_errored: usize) -> Self {
        let entity_count =
            counts.declaration_count + counts.var_decl_count + counts.macro_usage_count;
        Self {
            duration_ms: 0,
            file_count: counts.file_count,
            module_count: counts.module_count,
            entity_count,
            declaration_count: counts.declaration_count,
            var_decl_count: counts.var_decl_count,
            macro_usage_count: counts.macro_usage_count,
            xref_count: counts.xref_count,
            doc_count: counts.doc_count,
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
            declaration_count: 0,
            var_decl_count: 0,
            macro_usage_count: 0,
            xref_count: 0,
            doc_count: 0,
            output_bytes: 0,
            files_errored: 0,
            success: false,
            error_message: Some(format!("{error:#}")),
        }
    }
}
