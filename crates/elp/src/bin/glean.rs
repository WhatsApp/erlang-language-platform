/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use core::option::Option::None;
use std::io::Write;
use std::time::Instant;

use anyhow::Result;
use elp::build::load;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp_eqwalizer::EqwalizerDiagnostics;
use elp_eqwalizer::ast::Pos;
use elp_ide::Analysis;
use elp_ide::TextRange;
use elp_ide::elp_ide_db::EqwalizerDatabase;
use elp_ide::elp_ide_db::LineIndexDatabase;
use elp_ide::elp_ide_db::RootDatabase;
use elp_ide::elp_ide_db::docs::DocDatabase;
use elp_ide::elp_ide_db::docs::Documentation;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::ModuleName;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_ide::elp_ide_db::elp_base_db::path_for_file;
use elp_project_model::AppType;
use elp_project_model::DiscoverConfig;
use elp_project_model::buck::BuckQueryConfig;
use elp_syntax::AstNode;
use elp_syntax::ast;
use elp_syntax::ast::DeprecatedFa;
use elp_syntax::ast::ExprMax;
use elp_syntax::ast::Fa;
use elp_syntax::ast::HasArity;
use fxhash::FxHashMap;
use hir::AsName;
use hir::Body;
use hir::CallTarget;
use hir::DefMap;
use hir::DefineId;
use hir::Expr;
use hir::ExprId;
use hir::ExprSource;
use hir::File;
use hir::InFile;
use hir::Literal;
use hir::MacroName;
use hir::Module;
use hir::Name;
use hir::NameArity;
use hir::PPDirective;
use hir::Pat;
use hir::Semantic;
use hir::Strategy;
use hir::Term;
use hir::TypeExpr;
use hir::TypeExprId;
use hir::db::DefDatabase;
use hir::fold;
use hir::fold::AnyCallBackCtx;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::sema::to_def::resolve_call_target;
use hir::sema::to_def::resolve_type_target;
use itertools::Itertools;
use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;

use crate::args::Glean;

const FACTS_FILE: &str = "facts.json";

// @fb-only: mod meta_only;
pub(crate) mod output;
pub(crate) mod types;

use output::IndexedFacts;
use output::IndexerMetrics;
use types::Declaration;
use types::DocDecl;
use types::FileDeclaration;
use types::FileFact;
use types::FileLinesFact;
use types::FuncDecl;
use types::FunctionTarget;
use types::GleanFileId;
use types::HeaderDecl;
use types::HeaderTarget;
use types::Key;
use types::Location;
use types::MacroDecl;
use types::MacroTarget;
use types::ModuleFact;
use types::RecordDecl;
use types::RecordTarget;
use types::TaggedUrl;
use types::TypeDecl;
use types::TypeTarget;
use types::VarDecl;
use types::VarTarget;
use types::XRef;
use types::XRefFile;
use types::XRefTarget;

#[derive(Clone, Debug, Default)]
struct IndexConfig {
    pub multi: bool,
}

pub struct GleanIndexer {
    project_id: ProjectId,
    analysis: Analysis,
    module: Option<String>,
}

pub fn index(
    args: &Glean,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
    ifdef: bool,
) -> Result<()> {
    let start = Instant::now();
    let result = index_inner(args, cli, query_config, ifdef);
    let duration_ms = start.elapsed().as_millis() as u64;

    match result {
        Ok((mut metrics, errored_paths)) => {
            metrics.duration_ms = duration_ms;
            if !errored_paths.is_empty() {
                for path in errored_paths.iter().take(20) {
                    eprintln!("elp-glean: errored: {path}");
                }
                if errored_paths.len() > 20 {
                    eprintln!(
                        "elp-glean: ... and {} more ({} total)",
                        errored_paths.len() - 20,
                        errored_paths.len()
                    );
                }
            }
            if args.print_metrics {
                print_metrics(&metrics)?;
            } else {
                // @fb-only: meta_only::report_indexer_metrics(&metrics);
            }
            Ok(())
        }
        Err(err) => {
            let metrics = IndexerMetrics::failed(duration_ms, &err);
            if args.print_metrics {
                print_metrics(&metrics)?;
            } else {
                // @fb-only: meta_only::report_indexer_metrics(&metrics);
            }
            Err(err)
        }
    }
}

#[rustfmt::skip]
fn print_metrics(metrics: &IndexerMetrics) -> Result<()> {
    // @fb-only: println!("{}", meta_only::metrics_to_json(metrics));
    println!("{}", serde_json::to_string_pretty(metrics)?); // @oss-only
    Ok(())
}

/// Runs indexing and writes results. Duration filled in by caller.
fn index_inner(
    args: &Glean,
    cli: &mut dyn Cli,
    query_config: &BuckQueryConfig,
    ifdef: bool,
) -> Result<(IndexerMetrics, Vec<String>)> {
    let (indexer, _loaded) = GleanIndexer::new(args, cli, query_config, ifdef)?;
    let config = IndexConfig { multi: args.multi };
    let (facts, module_index, errored_paths) = indexer.index(config)?;
    let mut metrics = IndexerMetrics::from_facts(&facts, errored_paths.len());
    metrics.output_bytes = write_results(facts, module_index, cli, args)?;
    Ok((metrics, errored_paths))
}

/// Serializes and writes facts. Returns total bytes written.
fn write_results(
    facts: FxHashMap<String, IndexedFacts>,
    module_index: FxHashMap<GleanFileId, String>,
    cli: &mut dyn Cli,
    args: &Glean,
) -> Result<u64> {
    if args.v2 {
        eprintln!("elp-glean: --v2 is deprecated and now a no-op (v2 is always enabled)");
    }
    let mut total_bytes: u64 = 0;
    for (name, fact) in facts {
        let fact = fact.into_glean_facts(&module_index);
        let content = if args.pretty {
            serde_json::to_string_pretty(&fact)?
        } else {
            serde_json::to_string(&fact)?
        };
        total_bytes += content.len() as u64;
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
    Ok(total_bytes)
}

impl GleanIndexer {
    pub fn new(
        args: &Glean,
        cli: &mut dyn Cli,
        query_config: &BuckQueryConfig,
        ifdef: bool,
    ) -> Result<(Self, LoadResult)> {
        let config = DiscoverConfig::buck();
        let loaded = load::load_project_at(
            cli,
            &args.project,
            config,
            IncludeOtp::Yes,
            elp_eqwalizer::Mode::Server,
            query_config,
            ifdef,
        )?;
        let analysis = loaded.analysis();
        let indexer = Self {
            project_id: loaded.project_id,
            analysis,
            module: args.module.clone(),
        };
        Ok((indexer, loaded))
    }

    /// Returns (facts, module_index, errored_file_paths).
    fn index(
        &self,
        config: IndexConfig,
    ) -> Result<(
        FxHashMap<String, IndexedFacts>,
        FxHashMap<GleanFileId, String>,
        Vec<String>,
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
            let (facts, errored) = if let Some(module) = &self.module {
                let index = db.module_index(self.project_id);
                let file_id = index
                    .file_for_module(&ModuleName::new(module))
                    .expect("No module found");
                let source_root_id = db.file_source_root(file_id);
                let source_root = db.source_root(source_root_id);
                let path = source_root.path_for_file(&file_id).unwrap();
                match Self::index_file(db, file_id, path, project_id, &module_index) {
                    Some((file, line, decl, xref, module_fact)) => {
                        let mut result = FxHashMap::default();
                        result.insert(
                            FACTS_FILE.to_string(),
                            IndexedFacts::new(file, line, decl, xref, module_fact),
                        );
                        (result, vec![])
                    }
                    None => panic!("Can't find module {module}"),
                }
            } else {
                let errored = std::sync::Mutex::new(Vec::new());
                // Sort biggest files first to reduce long-tail in parallel processing
                let mut files = files;
                elp_ide::sort_by_size_descending(&mut files, |f| db.file_text(f.0).len());
                let results: Vec<_> = files
                    .into_par_iter()
                    .map_with(self.analysis.clone(), |analysis, (file_id, path)| {
                        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                            analysis.with_db(|db| {
                                Self::index_file(db, file_id, &path, project_id, &module_index)
                            })
                        }));
                        match result {
                            Ok(Ok(inner)) => inner,
                            Ok(Err(_)) | Err(_) => {
                                errored
                                    .lock()
                                    .expect("errored mutex poisoned")
                                    .push(path.to_string());
                                None
                            }
                        }
                    })
                    .filter_map(|x| x)
                    .collect();

                let iter = results.into_iter();
                let facts = if config.multi {
                    iter.map(|(file, line, decl, xref, module_fact)| {
                        IndexedFacts::new(file, line, decl, xref, module_fact)
                    })
                    .collect::<Vec<_>>()
                    .into_iter()
                    .enumerate()
                    .map(|(id, facts)| (format!("{id}.json"), facts))
                    .collect()
                } else {
                    let mut result = FxHashMap::default();
                    let facts = iter.fold(
                        IndexedFacts::default(),
                        |mut acc, (file_fact, line_fact, decl, xref, module_fact)| {
                            acc.add(file_fact, line_fact, decl, xref, module_fact);
                            acc
                        },
                    );
                    result.insert(FACTS_FILE.to_string(), facts);
                    result
                };
                (facts, errored.into_inner().expect("errored mutex poisoned"))
            };
            (facts, module_index, errored)
        })?;
        Ok(ctx)
    }

    fn project_files(db: &RootDatabase, project_id: ProjectId) -> Vec<(FileId, VfsPath)> {
        let project_data = db.project_data(project_id);
        let mut files = vec![];
        for &source_root_id in &project_data.source_roots {
            if let Some(app_data) = db.app_data(source_root_id)
                && app_data.app_type == AppType::App
            {
                let source_root = db.source_root(source_root_id);
                for file_id in source_root.iter() {
                    if let Some(path) = source_root.path_for_file(&file_id) {
                        files.push((file_id, path.clone()));
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
    ) -> Option<(
        FileFact,
        FileLinesFact,
        FileDeclaration,
        XRefFile,
        Option<ModuleFact>,
    )> {
        let file_fact = Self::file_fact(db, file_id, path, project_id)?;
        let line_fact = Self::line_fact(db, file_id);
        let mut xrefs = Self::xrefs(db, file_id, module_index);
        let mut file_decl = Self::declarations(db, file_id, path)?;
        Self::add_xref_based_declarations(db, project_id, file_id, &mut xrefs, &mut file_decl);

        let elp_module_index = db.module_index(project_id);
        let module_fact = elp_module_index
            .module_for_file(file_id)
            .map(|module| Self::module_fact(db, file_id, module));
        Some((file_fact, line_fact, file_decl, xrefs, module_fact))
    }

    fn module_fact(db: &RootDatabase, file_id: FileId, module_name: &ModuleName) -> ModuleFact {
        let module = Module {
            file: File { file_id },
        };
        let name = module_name.to_string();

        // Extract exported functions from def_map
        let def_map = db.def_map_local(file_id);

        let mut exports = vec![];
        for (fun, def) in def_map.get_functions() {
            if def.exported {
                exports.push(format!("{}/{}", fun.name(), fun.arity()));
            }
        }

        // Extract oncall, behaviour, and moduledoc using form_list API
        let sema = Semantic::new(db);
        let form_list = sema.form_list(file_id);

        let oncall = sema.attribute_value_as_string(file_id, hir::known::oncall);

        let behaviours: Vec<String> = form_list
            .behaviour_attributes()
            .map(|(_, behaviour)| behaviour.name.to_string())
            .collect();

        let module_doc = sema.module_attribute(file_id).and_then(|module_attribute| {
            let docs = Documentation::new(db, &sema);
            docs.to_doc(InFile::new(file_id, &module_attribute))
                .map(|doc| doc.markdown_text().to_string())
                .filter(|text| !text.is_empty())
        });

        // @fb-only: let exdoc_link = elp_ide::meta_only::exdoc_links::module_exdoc_link(&module, &sema);
        let exdoc_link: Option<String> = None; // @oss-only

        ModuleFact::new(
            file_id,
            name,
            oncall,
            (!exports.is_empty()).then_some(exports),
            (!behaviours.is_empty()).then_some(behaviours),
            module_doc,
            exdoc_link,
        )
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
                        let text = format!("```erlang\n{text}\n```");

                        // Generate links section from tagged_urls
                        // If URL is empty, output display_name only - this
                        // signifies preformatted markdown
                        let links_section: String = x
                            .key
                            .tagged_urls
                            .iter()
                            .map(|url| {
                                if url.url.is_empty() {
                                    format!("{}\n", url.display_name)
                                } else {
                                    format!("[{}]({})\n", url.display_name, url.url)
                                }
                            })
                            .collect();

                        let doc = format!(
                            "{}{}{}",
                            links_section,
                            text,
                            x.key
                                .expansion
                                .as_ref()
                                .map_or(String::new(), |e| format!("\n---\n\n{}", e))
                        );
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
                        // @fb-only: Using xref.source.start as arity is a
                        // @fb-only: design choice - see D55916496. This should
                        // @fb-only: be revisited during Glean schema redesign.
                        x.key.arity = Some(xref.source.start);
                    }
                }
                XRefTarget::Record(x) => {
                    // Add documentation for records that have tagged_urls
                    if !x.key.tagged_urls.is_empty() {
                        let id: FileId = x.key.file_id.clone().into();
                        let def_map = db.def_map(id);
                        let record_name = Name::from_erlang_service(&x.key.name);
                        if let Some(def) = def_map.get_record(&record_name) {
                            let range = def.source(db).syntax().text_range();
                            let text = &db.file_text(id)[range];
                            let text = format!("```erlang\n{text}\n```");

                            // Generate links section from tagged_urls
                            // If url is empty, display_name contains pre-formatted markdown
                            let links_section: String = x
                                .key
                                .tagged_urls
                                .iter()
                                .map(|url| {
                                    if url.url.is_empty() {
                                        format!("{}\n", url.display_name)
                                    } else {
                                        format!("[{}]({})\n", url.display_name, url.url)
                                    }
                                })
                                .collect();

                            let doc = format!("{}{}", links_section, text);
                            let decl = Declaration::RecordDeclaration(
                                RecordDecl {
                                    name: x.key.name.clone(),
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
                        }
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
    ) -> Option<FileFact> {
        let project_data = db.project_data(project_id);
        let root = project_data.root_dir.as_path();
        let file_path = path.as_path()?;
        let file_path = file_path.strip_prefix(root)?;
        let file_path = file_path.as_str().to_string();
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

    fn declarations(db: &RootDatabase, file_id: FileId, path: &VfsPath) -> Option<FileDeclaration> {
        let mut declarations = vec![];
        let def_map = db.def_map_local(file_id);
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
            let text = format!("```erlang\n{text}\n```");
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
            let text = format!("```erlang\n{text}\n```");
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
                    name: format!("{name}.hrl"),
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

    fn xref_callback(
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
                let (_, range) = ctx.find_range(sema)?;
                if range.file_id == file_id {
                    Some(XRef {
                        source: range.range.into(),
                        target: XRefTarget::Var(
                            VarTarget {
                                file_id: file_id.into(),
                                name,
                            }
                            .into(),
                        ),
                    })
                } else {
                    None
                }
            }
            hir::AnyExpr::Expr(Expr::Call { target, args }) => {
                let (body, _, expr_source) = ctx.body_with_expr_source(sema)?;
                let range = Self::find_range(sema, ctx, source_file, &expr_source)?;
                let arity = args.len() as u32;
                Self::resolve_call(sema, target, arity, file_id, &body, range)
            }
            hir::AnyExpr::Expr(Expr::CaptureFun { target, arity }) => {
                let (body, range) = ctx.find_range(sema)?;
                let arity: Option<u32> = match &body[*arity] {
                    Expr::Literal(Literal::Integer(int)) => int.value.try_into().ok(),
                    _ => None,
                };
                if range.file_id == file_id {
                    Self::resolve_call(sema, target, arity?, file_id, &body, range.range)
                } else {
                    None
                }
            }
            hir::AnyExpr::Pat(Pat::Record { name, .. })
            | hir::AnyExpr::Pat(Pat::RecordIndex { name, .. })
            | hir::AnyExpr::TypeExpr(TypeExpr::Record { name, .. })
            | hir::AnyExpr::Expr(Expr::Record { name, .. })
            | hir::AnyExpr::Expr(Expr::RecordIndex { name, .. })
            | hir::AnyExpr::Expr(Expr::RecordUpdate { name, .. })
            | hir::AnyExpr::Expr(Expr::RecordField { name, .. }) => {
                Self::resolve_record(sema, *name, file_id, ctx)
            }
            hir::AnyExpr::Expr(Expr::MacroCall {
                macro_def,
                expansion,
                ..
            }) => {
                let def = macro_def.as_ref()?;
                let mut resolved = Self::resolve_macro(sema, def, source_file, ctx)?;
                // @fb-only: meta_only::resolve_macro_expansion(sema, *expansion, ctx, &mut resolved);
                Some(resolved)
            }
            hir::AnyExpr::Pat(Pat::MacroCall { macro_def, .. })
            | hir::AnyExpr::TypeExpr(TypeExpr::MacroCall { macro_def, .. })
            | hir::AnyExpr::Term(Term::MacroCall { macro_def, .. }) => {
                let def = macro_def.as_ref()?;
                Self::resolve_macro(sema, def, source_file, ctx)
            }
            hir::AnyExpr::TypeExpr(TypeExpr::Call { target, args }) => {
                let (body, _, expr_source) = ctx.body_with_expr_source(sema)?;
                let range = Self::find_range(sema, ctx, source_file, &expr_source)?;
                let arity = args.len() as u32;
                Self::resolve_type(sema, target, arity, file_id, &body, range)
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
        if !db.is_eqwalizer_enabled(file_id) {
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
                        let text = format!("```erlang\n{name} :: {ty}\n```");
                        let decl = VarDecl {
                            name: name.to_string(),
                            doc: text,
                            span: range,
                        };
                        result.push(decl);
                    }
                }
            }
        }
        result
    }

    fn xrefs(
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
                macros: MacroStrategy::DoNotExpand,
                parens: ParenStrategy::InvisibleParens,
            },
            file_id,
            vec![],
            &mut |mut acc, ctx| {
                Self::xref_callback(db, &sema, &source_file, file_id, &mut acc, &ctx);
                acc
            },
            &mut |mut acc, on, form_id| match (on, form_id) {
                (hir::On::Entry, hir::FormIdx::PPDirective(idx)) => {
                    let directive = &form_list[idx];
                    if let PPDirective::Include(idx) = directive {
                        let include = &form_list[*idx];
                        let ast = include.form_id().get_ast(db, file_id);
                        let range = ast.syntax().text_range().into();
                        if let Some(file) = db.resolve_include(
                            db.app_data_id_by_file(file_id),
                            InFile::new(file_id, *idx),
                        ) && let Some(path) = path_for_file(db, file)
                            && let Some((name, Some("hrl"))) = path.name_and_extension()
                        {
                            let target = HeaderTarget {
                                file_id: file.into(),
                                name: format!("{name}.hrl"),
                            };
                            let xref = XRef {
                                source: range,
                                target: XRefTarget::Header(target.into()),
                            };
                            acc.push(xref);
                        }
                    }
                    acc
                }
                (hir::On::Entry, hir::FormIdx::Export(idx)) => {
                    let export = &form_list[idx];
                    let ast = export.form_id.get_ast(db, file_id);
                    for fun in ast.funs() {
                        if let Some(na) = Self::fa_name_arity(&fun)
                            && let Some(def) = sema.def_map(file_id).get_function(&na)
                        {
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
        xrefs.retain(|x| module_index.contains_key(x.target.file_id()));
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

    fn resolve_macro(
        sema: &Semantic<'_>,
        macro_def: &InFile<DefineId>,
        source_file: &InFile<ast::SourceFile>,
        ctx: &AnyCallBackCtx,
    ) -> Option<XRef> {
        let (_, _, expr_source) = ctx.body_with_expr_source(sema)?;
        let range = Self::find_range(sema, ctx, source_file, &expr_source)?;
        let form_list = sema.form_list(macro_def.file_id);
        let define = &form_list[macro_def.value];
        let name = define.name.name();
        let expansion = Self::expand_macro(sema, &expr_source, source_file);
        let target = MacroTarget {
            file_id: macro_def.file_id.into(),
            name: name.to_string(),
            arity: define.name.arity(),
            expansion,
            tagged_urls: Vec::new(),
        };
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
            let expansion = format!("```erlang\n{expansion}\n```");
            return Some(expansion);
        }
        None
    }

    fn resolve_type(
        sema: &Semantic,
        target: &CallTarget<TypeExprId>,
        arity: u32,
        file_id: FileId,
        body: &Body,
        range: TextRange,
    ) -> Option<XRef> {
        let def = resolve_type_target(sema, target, Some(arity), file_id, body)?;
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
    ) -> Option<XRef> {
        let record_name = sema.db.lookup_atom(name);
        let def_map = sema.db.def_map(file_id);
        let def = def_map.get_record(&record_name)?;
        let (_, _, expr_source) = ctx.body_with_expr_source(sema)?;
        let source_file = sema.parse(file_id);
        let range = Self::find_range(sema, ctx, &source_file, &expr_source)?;

        // @fb-only: let tagged_urls = meta_only::build_record_tagged_urls(sema, name);
        let tagged_urls: Vec<TaggedUrl> = Vec::new(); // @oss-only

        Some(XRef {
            source: range.into(),
            target: XRefTarget::Record(
                RecordTarget {
                    file_id: def.file.file_id.into(),
                    name: def.record.name.to_string(),
                    tagged_urls,
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
        let node = expr_source.to_node(source_file)?;
        match node {
            elp_syntax::ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(expr)) => {
                Some(expr.name()?.syntax().text_range())
            }
            elp_syntax::ast::Expr::Call(expr) => match expr.expr()? {
                ast::Expr::ExprMax(expr_max) => Some(expr_max.syntax().text_range()),
                expr => Some(expr.syntax().text_range()),
            },
            elp_syntax::ast::Expr::RecordExpr(expr) => Some(expr.name()?.syntax().text_range()),
            elp_syntax::ast::Expr::RecordFieldExpr(expr) => {
                Some(expr.name()?.syntax().text_range())
            }
            elp_syntax::ast::Expr::RecordIndexExpr(expr) => {
                Some(expr.name()?.syntax().text_range())
            }
            elp_syntax::ast::Expr::RecordUpdateExpr(expr) => {
                Some(expr.name()?.syntax().text_range())
            }
            _ => {
                let range = ctx.find_range(sema)?.1;
                if range.file_id == source_file.file_id {
                    Some(range.range)
                } else {
                    None
                }
            }
        }
    }
}

fn path_to_module_name(path: &VfsPath) -> Option<String> {
    match path.name_and_extension() {
        Some((name, Some("erl"))) => Some(name.to_string()),
        Some((name, Some("hrl"))) => Some(format!("{name}.hrl")),
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
mod tests;
