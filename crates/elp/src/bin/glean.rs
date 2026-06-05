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
use fxhash::FxHashSet;
use hir::AsName;
use hir::Body;
use hir::BodyOrigin;
use hir::CallTarget;
use hir::DefMap;
use hir::DefineId;
use hir::Expr;
use hir::ExprId;
use hir::ExprSource;
use hir::File;
use hir::FormIdx;
use hir::InFile;
use hir::Literal;
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
use hir::Var;
use hir::db::DefDatabase;
use hir::fold;
use hir::fold::AnyCallBackCtx;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::resolver::Resolver;
use hir::sema::to_def::resolve_call_target;
use hir::sema::to_def::resolve_type_target;
use itertools::Itertools;
use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;
use serde::Deserialize;

use crate::args::Glean;

const FACTS_FILE: &str = "facts.json";

// @fb-only: mod meta_only;
pub(crate) mod output;
pub(crate) mod types;

use output::FactCounts;
use output::IndexedFacts;
use output::IndexerMetrics;
use types::GleanFileId;
use types::Key;
use types::Location;
use types::glean;
use types::parser;

/// Per-file facts produced by [`GleanIndexer::index_file`], assembled into
/// [`IndexedFacts`] by the caller. A named struct (rather than a wide tuple)
/// so call sites bind fields by name instead of by position.
struct IndexedFileFacts {
    file: glean::FileFact,
    line: glean::FileLinesFact,
    declaration: parser::FileDeclaration,
    xref: parser::XRefFile,
    module_fact: Option<parser::ModuleFact>,
    thrift_markers: Vec<(u32, glean::ThriftDeclaration)>,
}

#[derive(Clone, Debug, Default)]
struct IndexConfig {
    pub multi: bool,
    pub source_root: Option<String>,
}

struct IndexResult {
    facts: FxHashMap<String, IndexedFacts>,
    module_index: FxHashMap<GleanFileId, String>,
    app_index: FxHashMap<GleanFileId, String>,
    app_infos: Vec<glean::AppInfo>,
    errored_paths: Vec<String>,
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
    let config = IndexConfig {
        multi: args.multi,
        source_root: args.source_root.clone(),
    };
    let result = indexer.index(config)?;
    let errored_paths = result.errored_paths.clone();
    let (output_bytes, counts) = write_results(result, cli, args)?;
    let mut metrics = IndexerMetrics::from_counts(counts, errored_paths.len());
    metrics.output_bytes = output_bytes;
    Ok((metrics, errored_paths))
}

/// Serializes and writes facts. Returns (total bytes written, accumulated fact counts).
fn write_results(
    result: IndexResult,
    cli: &mut dyn Cli,
    args: &Glean,
) -> Result<(u64, FactCounts)> {
    if args.schema2 {
        eprintln!("elp-glean: --schema2 is deprecated and now a no-op");
    }
    let IndexResult {
        facts,
        module_index,
        app_index,
        app_infos,
        ..
    } = result;
    let mut total_bytes: u64 = 0;
    let mut total_counts = FactCounts::default();
    let mut app_infos_emitted = false;
    for (name, fact) in facts {
        let (mut fact, counts) = fact.into_glean_facts(&module_index, &app_index);
        if !app_infos_emitted && !app_infos.is_empty() {
            fact.push(glean::Fact::AppInfo {
                facts: app_infos
                    .iter()
                    .map(|ai| types::Key { key: ai.clone() })
                    .collect(),
            });
            app_infos_emitted = true;
        }
        total_counts.accumulate(&counts);
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
    Ok((total_bytes, total_counts))
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

    fn index(&self, config: IndexConfig) -> Result<IndexResult> {
        let ctx = self.analysis.with_db(|db| {
            let project_id = self.project_id;
            let files = Self::project_files(db, project_id);
            // glean module index, which fake headers as modules with name header.hrl
            let mut module_index: FxHashMap<GleanFileId, String> = files
                .iter()
                .filter_map(|(file_id, path)| {
                    path_to_module_name(path).map(|name| ((*file_id).into(), name))
                })
                .collect();
            // Collect OTP source roots.
            let project_data = db.project_data(project_id);
            let otp_source_roots: Vec<_> = project_data
                .otp_project_id
                .map(|otp_id| db.project_data(otp_id).source_roots.clone())
                .unwrap_or_default();
            // app index: file_id → OTP application name
            let (app_index, app_infos) = {
                let project_data = db.project_data(project_id);
                let mut index = FxHashMap::default();
                let mut seen_apps = FxHashSet::default();
                let mut infos = Vec::new();
                for &source_root_id in project_data
                    .source_roots
                    .iter()
                    .chain(otp_source_roots.iter())
                {
                    if let Some(app_data) = db.app_data(source_root_id) {
                        let app_name = app_data.name.as_str().to_string();
                        if seen_apps.insert(app_name.clone()) {
                            let type_ = match app_data.app_type {
                                AppType::App => glean::AppType::FirstParty,
                                AppType::Otp => glean::AppType::Otp,
                                AppType::Dep => glean::AppType::ThirdParty,
                            };
                            infos.push(glean::AppInfo {
                                name: app_name.clone(),
                                type_,
                            });
                        }
                        let source_root = db.source_root(source_root_id);
                        for file_id in source_root.iter() {
                            let file_app = db
                                .file_app_data(file_id)
                                .map(|ad| ad.name.as_str().to_string())
                                .unwrap_or_else(|| app_name.clone());
                            index.insert(file_id.into(), file_app);
                        }
                    }
                }
                (index, infos)
            };
            // Extend module_index with OTP files for xrefs to OTP functions
            for &source_root_id in &otp_source_roots {
                let source_root = db.source_root(source_root_id);
                for file_id in source_root.iter() {
                    if let Some(path) = source_root.path_for_file(&file_id)
                        && let Some(name) = path_to_module_name(path)
                    {
                        module_index.insert(file_id.into(), name);
                    }
                }
            }
            let (facts, errored) = if let Some(module) = &self.module {
                let index = db.module_index(self.project_id);
                let file_id = index
                    .file_for_module(&ModuleName::new(module))
                    .expect("No module found");
                let source_root_id = db.file_source_root(file_id);
                let source_root = db.source_root(source_root_id);
                let path = source_root.path_for_file(&file_id).unwrap();
                match Self::index_file(
                    db,
                    file_id,
                    path,
                    project_id,
                    &module_index,
                    config.source_root.as_deref(),
                ) {
                    Some(IndexedFileFacts {
                        file,
                        line,
                        declaration,
                        xref,
                        module_fact,
                        thrift_markers,
                    }) => {
                        let mut facts =
                            IndexedFacts::new(file, line, declaration, xref, module_fact);
                        facts.add_thrift_annotations(file_id.into(), thrift_markers);
                        let mut result = FxHashMap::default();
                        result.insert(FACTS_FILE.to_string(), facts);
                        (result, vec![])
                    }
                    None => panic!("Can't find module {module}"),
                }
            } else {
                let errored = std::sync::Mutex::new(Vec::new());
                // Sort biggest files first to reduce long-tail in parallel processing
                let mut files = files;
                elp_ide::sort_by_size_descending(&mut files, |f| db.file_text(f.0).len());
                let source_root = config.source_root.clone();
                let results: Vec<_> = files
                    .into_par_iter()
                    .map_with(self.analysis.clone(), |analysis, (file_id, path)| {
                        let source_root = source_root.as_deref();
                        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                            analysis.with_db(|db| {
                                Self::index_file(
                                    db,
                                    file_id,
                                    &path,
                                    project_id,
                                    &module_index,
                                    source_root,
                                )
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
                    iter.map(|indexed| {
                        let IndexedFileFacts {
                            file,
                            line,
                            declaration,
                            xref,
                            module_fact,
                            thrift_markers,
                        } = indexed;
                        let file_id = file.file_id.clone();
                        let mut facts =
                            IndexedFacts::new(file, line, declaration, xref, module_fact);
                        facts.add_thrift_annotations(file_id, thrift_markers);
                        facts
                    })
                    .collect::<Vec<_>>()
                    .into_iter()
                    .enumerate()
                    .map(|(id, facts)| (format!("{id}.json"), facts))
                    .collect()
                } else {
                    let mut result = FxHashMap::default();
                    let facts = iter.fold(IndexedFacts::default(), |mut acc, indexed| {
                        let IndexedFileFacts {
                            file,
                            line,
                            declaration,
                            xref,
                            module_fact,
                            thrift_markers,
                        } = indexed;
                        let file_id = file.file_id.clone();
                        acc.add(file, line, declaration, xref, module_fact);
                        acc.add_thrift_annotations(file_id, thrift_markers);
                        acc
                    });
                    result.insert(FACTS_FILE.to_string(), facts);
                    result
                };
                (facts, errored.into_inner().expect("errored mutex poisoned"))
            };
            IndexResult {
                facts,
                module_index,
                app_index,
                app_infos,
                errored_paths: errored,
            }
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
        source_root: Option<&str>,
    ) -> Option<IndexedFileFacts> {
        let file_fact = Self::file_fact(db, file_id, path, project_id, source_root)?;
        let line_fact = Self::line_fact(db, file_id);
        let mut xrefs = Self::xrefs(db, file_id, module_index);
        let mut file_decl = Self::declarations(db, file_id, path)?;
        Self::add_xref_based_declarations(db, project_id, file_id, &mut xrefs, &mut file_decl);

        let elp_module_index = db.module_index(project_id);
        let source = db.file_text(file_id);
        let is_thrift_generated = has_codegen_source(&source);

        let module_fact = elp_module_index
            .module_for_file(file_id)
            .cloned()
            .or_else(|| match path.name_and_extension() {
                Some((_, Some("escript"))) => {
                    path_to_module_name(path).map(|name| ModuleName::new(&name))
                }
                // Thrift `.hrl` headers have no `-module`; index them so field
                // markers can resolve to their record fields.
                Some((_, Some("hrl"))) if is_thrift_generated => {
                    path_to_module_name(path).map(|name| ModuleName::new(&name))
                }
                _ => None,
            })
            .map(|module| Self::module_fact(db, file_id, &module, project_id));

        let thrift_markers = if is_thrift_generated {
            parse_thrift_markers(&source)
        } else {
            vec![]
        };
        Some(IndexedFileFacts {
            file: file_fact,
            line: line_fact,
            declaration: file_decl,
            xref: xrefs,
            module_fact,
            thrift_markers,
        })
    }

    fn module_fact(
        db: &RootDatabase,
        file_id: FileId,
        module_name: &ModuleName,
        project_id: ProjectId,
    ) -> parser::ModuleFact {
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

        let behaviour_callback_stubs: Vec<types::parser::BehaviourCallbackStub> = behaviours
            .iter()
            .flat_map(|behaviour_name| {
                let name = Name::from_erlang_service(behaviour_name);
                sema.resolve_behaviour(file_id, &name)
                    .into_iter()
                    .filter_map(move |(behaviour_mod, callbacks)| {
                        let app_data =
                            db.app_data(db.file_source_root(behaviour_mod.file.file_id))?;
                        if app_data.app_type != AppType::Otp {
                            return None;
                        }
                        let bn = behaviour_name.clone();
                        let ba = app_data.name.as_str().to_string();
                        Some(callbacks.into_keys().map(move |na| {
                            types::parser::BehaviourCallbackStub {
                                callback_name: na.name().to_string(),
                                callback_arity: na.arity(),
                                behaviour_module: bn.clone(),
                                behaviour_app: ba.clone(),
                            }
                        }))
                    })
                    .flatten()
            })
            .collect();

        let module_attribute = sema.module_attribute(file_id);
        let module_doc = module_attribute.as_ref().and_then(|ma| {
            let docs = Documentation::new(db, &sema);
            let text = docs
                .to_doc(InFile::new(file_id, ma))
                .map(|doc| doc.markdown_text().to_string())
                .filter(|text| !text.is_empty())?;

            let span = form_list
                .moduledoc_attributes()
                .next()
                .map(|(_, attr)| {
                    let ast = attr.form_id.get_ast(db, file_id);
                    ast.syntax().text_range().into()
                })
                .or_else(|| {
                    sema.module_edoc_header(file_id, ma)
                        .and_then(|edoc| edoc.doc.as_ref().map(|d| d.range.into()))
                })
                .unwrap_or_else(|| ma.syntax().text_range().into());

            Some(parser::ModuleDocComment { text, span })
        });

        // @fb-only: let exdoc_link = elp_ide::meta_only::exdoc_links::module_exdoc_link(&module, &sema);
        let exdoc_link: Option<String> = None; // @oss-only

        let def_map = db.def_map(file_id);
        let def_map_local = db.def_map_local(file_id);

        let record_def_texts: Vec<types::parser::RecordDefText> = def_map_local
            .get_records()
            .iter()
            .map(|(name, rec_def)| {
                let text = rec_def.source(db).syntax().text().to_string();
                types::parser::RecordDefText {
                    name: name.to_string(),
                    definition_text: text,
                }
            })
            .collect();

        let record_fields: Vec<types::parser::RecordFieldInfo> = def_map_local
            .get_records()
            .iter()
            .flat_map(|(name, rec_def)| {
                rec_def.fields(db).map(move |(field_name, field_def)| {
                    let span = field_def.source(db).syntax().text_range().into();
                    types::parser::RecordFieldInfo {
                        record_name: name.to_string(),
                        field_name: field_name.to_string(),
                        span,
                    }
                })
            })
            .collect();

        let project_data = db.project_data(project_id);
        let root = project_data.root_dir.as_path();
        let included_files: Vec<(GleanFileId, String)> = def_map
            .get_included_files()
            .filter_map(|inc_file_id| {
                let sr_id = db.file_source_root(inc_file_id);
                let sr = db.source_root(sr_id);
                let path = sr.path_for_file(&inc_file_id)?;
                let rel_path = path.as_path()?.strip_prefix(root)?;
                Some((
                    GleanFileId::from(inc_file_id),
                    rel_path.as_str().to_string(),
                ))
            })
            .collect();

        let callbacks: Vec<types::parser::CallbackInfo> = def_map
            .get_callbacks()
            .iter()
            .map(|(na, cb_def)| {
                let span = cb_def.source(db).syntax().text_range().into();
                types::parser::CallbackInfo {
                    name: na.name().to_string(),
                    arity: na.arity(),
                    optional: cb_def.optional,
                    span,
                }
            })
            .collect();

        let source = db.parse(file_id);
        let compile_options: Vec<String> = form_list
            .compile_attributes()
            .flat_map(|(_, opt)| {
                let ast_opt = opt.form_id.get(&source.tree());
                ast_opt.options().into_iter().flat_map(|options| {
                    options
                        .syntax()
                        .descendants()
                        .filter_map(|n| ast::Atom::cast(n).map(|a| a.as_name().to_string()))
                })
            })
            .collect();

        let on_load_fns: Vec<String> = form_list
            .attributes()
            .filter(|(_, attr)| attr.name == hir::known::on_load)
            .flat_map(|(_, attr)| {
                let attr_ast = attr.form_id.get(&source.tree());
                attr_ast.value().into_iter().flat_map(|value| {
                    value
                        .syntax()
                        .descendants()
                        .filter_map(|n| ast::Atom::cast(n).map(|a| a.as_name().to_string()))
                })
            })
            .collect();

        let nif_fns: Vec<(String, u32)> = form_list
            .attributes()
            .filter(|(_, attr)| attr.name == hir::known::nifs)
            .flat_map(|(_, attr)| {
                let attr_ast = attr.form_id.get(&source.tree());
                let text = attr_ast
                    .value()
                    .map(|v| v.syntax().text().to_string())
                    .unwrap_or_default();
                text.split([',', '[', ']'])
                    .filter_map(|s| {
                        let s = s.trim();
                        let (name, arity) = s.split_once('/')?;
                        Some((
                            name.trim().trim_matches('\'').to_string(),
                            arity.trim().parse::<u32>().ok()?,
                        ))
                    })
                    .collect::<Vec<_>>()
            })
            .collect();

        let module_span = module_attribute
            .as_ref()
            .map(|ma| ma.syntax().text_range().into());

        parser::ModuleFact {
            file_id: file_id.into(),
            name,
            oncall,
            exports: (!exports.is_empty()).then_some(exports),
            behaviours: (!behaviours.is_empty()).then_some(behaviours),
            module_doc,
            exdoc_link,
            module_span,
            callbacks,
            compile_options,
            on_load_fns,
            nif_fns,
            included_files,
            record_fields,
            record_def_texts,
            behaviour_callback_stubs,
        }
    }

    fn add_xref_based_declarations(
        db: &RootDatabase,
        project_id: ProjectId,
        file_id: FileId,
        xrefs: &mut parser::XRefFile,
        file_decl: &mut parser::FileDeclaration,
    ) {
        let mut vars = FxHashMap::default();
        for xref in &mut xrefs.xrefs {
            if let parser::XRefTarget::Var(x) = &mut xref.target {
                vars.insert(&xref.source, &x.key.name);
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
            let decl = parser::Declaration::VarDeclaration(var.into());
            let doc_decl = parser::Declaration::DocDeclaration(
                parser::DocDecl {
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
        source_root: Option<&str>,
    ) -> Option<glean::FileFact> {
        let project_data = db.project_data(project_id);
        let root = project_data.root_dir.as_path();
        let file_path = path.as_path()?;
        let file_path = file_path.strip_prefix(root)?;
        Some(glean::FileFact::new(
            file_id,
            apply_source_root(file_path.as_str(), source_root),
        ))
    }

    fn line_fact(db: &RootDatabase, file_id: FileId) -> glean::FileLinesFact {
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
        glean::FileLinesFact::new(file_id, lengths, ends_with_new_line)
    }

    fn declarations(
        db: &RootDatabase,
        file_id: FileId,
        path: &VfsPath,
    ) -> Option<parser::FileDeclaration> {
        let mut declarations = vec![];
        let def_map = db.def_map_local(file_id);
        // file docs are too slow. Going with specs for now
        // let file_doc = db.file_doc(file_id);
        let specs = db.file_specs(file_id);
        for (fun, def) in def_map.get_functions() {
            let range = def.range(db);
            if let Some(range) = range {
                let span = range.into();
                let spec_text = def
                    .spec
                    .as_ref()
                    .map(|s| s.source(db).syntax().text().to_string());
                let deprecated_desc = def.deprecated_desc.as_ref().map(|d| {
                    use hir::form_list::DeprecatedDesc;
                    match d {
                        DeprecatedDesc::Str(s) | DeprecatedDesc::Atom(s) => s.to_string(),
                    }
                });
                let decl = parser::Declaration::FunctionDeclaration(
                    parser::FuncDecl {
                        name: fun.name().to_string(),
                        arity: fun.arity(),
                        span,
                        exported: def.exported,
                        deprecated: def.deprecated,
                        deprecated_desc,
                        spec_text,
                    }
                    .into(),
                );
                if let (Some(spec_def), Some(spec)) = (&def.spec, specs.get(fun)) {
                    let doc_range = spec_def.source(db).syntax().text_range();
                    let doc = spec.markdown_text().to_string();
                    declarations.push(parser::Declaration::DocDeclaration(
                        parser::DocDecl {
                            target: Box::new(decl.clone()),
                            span: doc_range.into(),
                            text: doc,
                        }
                        .into(),
                    ));
                }
                if let Some(doc_id) = def.doc_id {
                    let form_list = db.file_form_list(file_id);
                    let attr = form_list[doc_id].form_id.get_ast(db, file_id);
                    let doc_range = attr.syntax().text_range();
                    let text = attr.syntax().text().to_string();
                    declarations.push(parser::Declaration::DocDeclaration(
                        parser::DocDecl {
                            target: Box::new(decl.clone()),
                            span: doc_range.into(),
                            text,
                        }
                        .into(),
                    ));
                } else if let Some(edoc) = def.edoc_comments(db)
                    && let Some(doc_tag) = &edoc.doc
                    && let Some(markdown) = doc_tag.to_markdown()
                {
                    let span = doc_tag.range.into();
                    declarations.push(parser::Declaration::DocDeclaration(
                        parser::DocDecl {
                            target: Box::new(decl.clone()),
                            span,
                            text: markdown,
                        }
                        .into(),
                    ));
                }
                declarations.push(decl);
            }
        }

        for (macros, def) in def_map.get_macros() {
            let source = def.source(db);
            let range = source.syntax().text_range();
            let span: Location = range.into();
            let definition_text = Some(source.syntax().text().to_string());
            let decl = parser::Declaration::MacroDeclaration(
                parser::MacroDecl {
                    name: macros.name().to_string(),
                    arity: macros.arity(),
                    span,
                    definition_text,
                }
                .into(),
            );
            declarations.push(decl);
        }

        for (ty, def) in def_map.get_types() {
            let source = def.source(db);
            let opaque = matches!(source, hir::TypeAliasSource::Opaque(_));
            let range = source.syntax().text_range();
            let definition_text = db.file_text(file_id)[range].to_string();
            let text = format!("```erlang\n{definition_text}\n```");
            let span: Location = range.into();

            let decl = parser::Declaration::TypeDeclaration(
                parser::TypeDecl {
                    name: ty.name().to_string(),
                    arity: ty.arity(),
                    span: span.clone(),
                    exported: def.exported,
                    opaque,
                    definition_text: Some(definition_text),
                }
                .into(),
            );

            declarations.push(parser::Declaration::DocDeclaration(
                parser::DocDecl {
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

            let decl = parser::Declaration::RecordDeclaration(
                parser::RecordDecl {
                    name: rec.to_string(),
                    span: span.clone(),
                }
                .into(),
            );

            declarations.push(parser::Declaration::DocDeclaration(
                parser::DocDecl {
                    target: Box::new(decl.clone()),
                    span,
                    text,
                }
                .into(),
            ));
            declarations.push(decl);
        }

        if let Some((name, Some("hrl"))) = path.name_and_extension() {
            let file_length = db.file_text(file_id).len() as u32;
            declarations.push(parser::Declaration::HeaderDeclaration(
                parser::HeaderDecl {
                    name: format!("{name}.hrl"),
                    span: Location {
                        start: 0,
                        length: file_length,
                    },
                }
                .into(),
            ));
        }

        Some(parser::FileDeclaration {
            file_id: file_id.into(),
            declarations,
        })
    }

    fn xref_callback(
        sema: &Semantic,
        source_file: &InFile<ast::SourceFile>,
        file_id: FileId,
        acc: &mut Vec<parser::XRef>,
        ctx: &AnyCallBackCtx,
    ) -> Option<()> {
        let target = match &ctx.item {
            hir::AnyExpr::Pat(Pat::Var(var))
            | hir::AnyExpr::TypeExpr(TypeExpr::Var(var))
            | hir::AnyExpr::Expr(Expr::Var(var)) => {
                let name = var.as_string();
                let (_, range) = ctx.find_range(sema)?;
                if range.file_id == file_id {
                    let decl_span_start = Self::resolve_var_decl_span(sema, var, ctx);
                    Some(parser::XRef {
                        source: range.range.into(),
                        target: parser::XRefTarget::Var(
                            parser::VarTarget {
                                file_id: file_id.into(),
                                name,
                                decl_span_start,
                            }
                            .into(),
                        ),
                        caller: None,
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
            | hir::AnyExpr::Expr(Expr::RecordUpdate { name, .. })
            | hir::AnyExpr::Expr(Expr::RecordIndex { name, .. })
            | hir::AnyExpr::Expr(Expr::RecordField { name, .. }) => {
                if let Some(record_xref) = Self::resolve_record(sema, name.atom, file_id, ctx) {
                    Self::resolve_record_fields(sema, name.atom, file_id, source_file, ctx, acc);
                    Some(record_xref)
                } else {
                    None
                }
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
        let mut xref = target?;
        xref.caller = Self::resolve_caller(sema, file_id, ctx);
        acc.push(xref);
        None
    }

    fn resolve_caller(
        sema: &Semantic,
        file_id: FileId,
        ctx: &AnyCallBackCtx,
    ) -> Option<(GleanFileId, String, u32)> {
        if let BodyOrigin::FormIdx {
            file_id: origin_file_id,
            form_id: FormIdx::FunctionClause(clause_id),
        } = &ctx.body_origin
            && *origin_file_id == file_id
        {
            let def_map = sema.def_map(file_id);
            let fn_def_id = def_map.function_def_id(&InFile::new(file_id, *clause_id))?;
            let fn_def = def_map.get_by_function_id(&InFile::new(file_id, *fn_def_id))?;
            Some((
                file_id.into(),
                fn_def.name.name().to_string(),
                fn_def.name.arity(),
            ))
        } else {
            None
        }
    }

    fn resolve_var_decl_span(sema: &Semantic, var: &Var, ctx: &AnyCallBackCtx) -> Option<u32> {
        match &ctx.body_origin {
            BodyOrigin::FormIdx {
                file_id,
                form_id: FormIdx::FunctionClause(clause_id),
            } => {
                let clause = InFile::new(*file_id, *clause_id);
                let scopes = sema.db.function_clause_scopes(clause);
                let resolver = Resolver::new(scopes);
                let pat_ids = resolver.resolve_any_expr_id(var, ctx.item_id)?;
                let first_pat = pat_ids.first()?;
                let (_, source_map) = sema.db.function_clause_body_with_source(clause);
                let expr_source = source_map.pat(*first_pat)?;
                Some(u32::from(expr_source.range().range.start()))
            }
            _ => None,
        }
    }

    fn types(
        db: &RootDatabase,
        project_id: ProjectId,
        file_id: FileId,
        vars: FxHashMap<&Location, &String>,
    ) -> Vec<parser::VarDecl> {
        let mut result = vec![];
        if !db.file_kind(file_id).is_module() || !db.is_eqwalizer_enabled(file_id) {
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
                        let type_str = format!("{name} :: {ty}");
                        let text = format!("```erlang\n{type_str}\n```");
                        let decl = parser::VarDecl {
                            name: name.to_string(),
                            doc: text,
                            type_text: Some(type_str),
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
    ) -> parser::XRefFile {
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
                Self::xref_callback(&sema, &source_file, file_id, &mut acc, &ctx);
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
                            let target = parser::HeaderTarget {
                                file_id: file.into(),
                                name: format!("{name}.hrl"),
                            };
                            let xref = parser::XRef {
                                source: range,
                                target: parser::XRefTarget::Header(target.into()),
                                caller: None,
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
                            let target = parser::FunctionTarget {
                                file_id: def.file.file_id.into(),
                                name: na.name().to_string(),
                                arity: na.arity(),
                            };
                            let xref = parser::XRef {
                                source: range,
                                target: parser::XRefTarget::Function(target.into()),
                                caller: None,
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
                (hir::On::Entry, hir::FormIdx::FunctionClause(function_id)) => {
                    let function_def_id =
                        InFile::new(file_id, hir::FunctionDefId::new(function_id));
                    if let Some(fun_def) = def_map.get_by_function_id(&function_def_id) {
                        let fun_na = &fun_def.name;
                        for behaviour_name in def_map.get_behaviours() {
                            if let Some((behaviour_module, callbacks)) =
                                sema.resolve_behaviour(file_id, behaviour_name)
                                && callbacks.contains_key(fun_na)
                            {
                                let fun_ast = form_list[function_id].form_id.get_ast(db, file_id);
                                let fun_range: Location = fun_ast.syntax().text_range().into();
                                acc.push(parser::XRef {
                                    source: fun_range,
                                    target: parser::XRefTarget::Callback(
                                        types::parser::CallbackTarget {
                                            file_id: behaviour_module.file.file_id.into(),
                                            name: fun_na.name().to_string(),
                                            arity: fun_na.arity(),
                                        }
                                        .into(),
                                    ),
                                    caller: None,
                                });
                            }
                        }
                    }
                    acc
                }
                _ => acc,
            },
        );
        xrefs.retain(|x| module_index.contains_key(x.target.file_id()));
        parser::XRefFile {
            file_id: file_id.into(),
            xrefs,
        }
    }

    fn deprecated_xref(def_map: &DefMap, fun: &DeprecatedFa) -> Option<parser::XRef> {
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
        let target = parser::FunctionTarget {
            file_id: def.file.file_id.into(),
            name: na.name().to_string(),
            arity: na.arity(),
        };
        let xref = parser::XRef {
            source: range,
            target: parser::XRefTarget::Function(target.into()),
            caller: None,
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
    ) -> Option<parser::XRef> {
        let def = resolve_call_target(sema, target, Some(arity), file_id, body)?;
        let target = parser::FunctionTarget {
            file_id: def.file.file_id.into(),
            name: def.name.name().to_string(),
            arity,
        };
        Some(parser::XRef {
            source: range.into(),
            target: parser::XRefTarget::Function(target.into()),
            caller: None,
        })
    }

    fn resolve_macro(
        sema: &Semantic<'_>,
        macro_def: &InFile<DefineId>,
        source_file: &InFile<ast::SourceFile>,
        ctx: &AnyCallBackCtx,
    ) -> Option<parser::XRef> {
        let (_, _, expr_source) = ctx.body_with_expr_source(sema)?;
        let range = Self::find_range(sema, ctx, source_file, &expr_source)?;
        let form_list = sema.form_list(macro_def.file_id);
        let define = &form_list[macro_def.value];
        let name = define.name.name();
        let expansion = Self::expand_macro(sema, &expr_source, source_file);
        let arity = define.name.arity();
        let file_id: GleanFileId = macro_def.file_id.into();
        let target = parser::MacroTarget {
            name: name.to_string(),
            file_id,
            arity,
            expansion,
            tagged_urls: Vec::new(),
        };
        Some(parser::XRef {
            source: range.into(),
            target: parser::XRefTarget::Macro(target.into()),
            caller: None,
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
    ) -> Option<parser::XRef> {
        let def = resolve_type_target(sema, target, Some(arity), file_id, body)?;
        Some(parser::XRef {
            source: range.into(),
            target: parser::XRefTarget::Type(
                parser::TypeTarget {
                    file_id: def.file.file_id.into(),
                    name: def.type_alias.name().name().to_string(),
                    arity,
                }
                .into(),
            ),
            caller: None,
        })
    }

    fn lookup_record(
        sema: &Semantic,
        name: hir::Atom,
        file_id: FileId,
        ctx: &AnyCallBackCtx,
    ) -> Option<(hir::RecordDef, ExprSource)> {
        let record_name = name.as_name();
        let def_map = sema.db.def_map(file_id);
        let def = def_map.get_record(&record_name)?.clone();
        let (_, _, expr_source) = ctx.body_with_expr_source(sema)?;
        Some((def, expr_source))
    }

    fn resolve_record(
        sema: &Semantic,
        name: hir::Atom,
        file_id: FileId,
        ctx: &AnyCallBackCtx,
    ) -> Option<parser::XRef> {
        let (def, expr_source) = Self::lookup_record(sema, name, file_id, ctx)?;
        let source_file = sema.parse(file_id);
        let range = Self::find_range(sema, ctx, &source_file, &expr_source)?;

        // @fb-only: let tagged_urls = meta_only::build_record_tagged_urls(name);
        let tagged_urls: Vec<parser::TaggedUrl> = Vec::new(); // @oss-only

        Some(parser::XRef {
            source: range.into(),
            target: parser::XRefTarget::Record(
                parser::RecordTarget {
                    name: def.record.name.to_string(),
                    file_id: def.file.file_id.into(),
                    tagged_urls,
                }
                .into(),
            ),
            caller: None,
        })
    }

    fn resolve_record_context(
        sema: &Semantic,
        name: hir::Atom,
        file_id: FileId,
        source_file: &InFile<ast::SourceFile>,
        ctx: &AnyCallBackCtx,
    ) -> Option<(hir::RecordDef, ast::Expr)> {
        let (def, expr_source) = Self::lookup_record(sema, name, file_id, ctx)?;
        let node = expr_source.to_node(source_file)?;
        Some((def, node))
    }

    fn resolve_record_fields(
        sema: &Semantic,
        name: hir::Atom,
        file_id: FileId,
        source_file: &InFile<ast::SourceFile>,
        ctx: &AnyCallBackCtx,
        acc: &mut Vec<parser::XRef>,
    ) {
        let (def, node) = match Self::resolve_record_context(sema, name, file_id, source_file, ctx)
        {
            Some(t) => t,
            None => return,
        };
        let record_name_str = name.as_name().to_string();
        let target_file_id: GleanFileId = def.file.file_id.into();

        let fields: Vec<(String, TextRange)> = match &node {
            ast::Expr::RecordExpr(e) => e
                .fields()
                .filter_map(|f| {
                    let n = f.name()?;
                    Some((n.syntax().text().to_string(), n.syntax().text_range()))
                })
                .collect(),
            ast::Expr::RecordUpdateExpr(e) => e
                .fields()
                .filter_map(|f| {
                    let n = f.name()?;
                    Some((n.syntax().text().to_string(), n.syntax().text_range()))
                })
                .collect(),
            ast::Expr::RecordFieldExpr(e) => e
                .field()
                .map(|f| {
                    let text = f.syntax().text().to_string();
                    let name = text.strip_prefix('.').unwrap_or(&text).to_string();
                    vec![(name, f.syntax().text_range())]
                })
                .unwrap_or_default(),
            ast::Expr::RecordIndexExpr(e) => e
                .field()
                .map(|f| {
                    let text = f.syntax().text().to_string();
                    let name = text.strip_prefix('.').unwrap_or(&text).to_string();
                    vec![(name, f.syntax().text_range())]
                })
                .unwrap_or_default(),
            _ => return,
        };

        for (field_name, range) in fields {
            acc.push(parser::XRef {
                source: range.into(),
                target: parser::XRefTarget::RecordField(
                    types::parser::RecordFieldTarget {
                        file_id: target_file_id.clone(),
                        record_name: record_name_str.clone(),
                        field_name,
                    }
                    .into(),
                ),
                caller: None,
            });
        }
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
            elp_syntax::ast::Expr::Remote(remote) => match remote.fun()? {
                // Return range of mod:fun, excluding args
                ast::Expr::Call(call) => {
                    let start = remote.syntax().text_range().start();
                    let end = call.expr()?.syntax().text_range().end();
                    Some(TextRange::new(start, end))
                }
                // Bare remote: M:F without call
                fun => {
                    let start = remote.syntax().text_range().start();
                    let end = fun.syntax().text_range().end();
                    Some(TextRange::new(start, end))
                }
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

// ── Erlang ↔ Thrift bridge: parse the generated `%% Glean {...}` markers ──
//
// The thrift code generator emits one JSON marker per generated declaration:
//   %% Glean {"file": "foo/foo.thrift", "kind": "struct", "name": "Point"}
// The indexer attaches each marker to the declaration that immediately
// follows it and emits an `erlang.ErlangToThrift` fact.

const GLEAN_MARKER_PREFIX: &str = "%% Glean ";

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct GleanMarker {
    file: String,
    kind: String,
    name: String,
    #[serde(default)]
    service: Option<String>,
    #[serde(default)]
    container: Option<String>,
    #[serde(default)]
    container_kind: Option<String>,
}

/// Parse a single `%% Glean {...}` comment line into a marker, if it is one.
fn parse_glean_marker(line: &str) -> Option<GleanMarker> {
    let json = line.trim().strip_prefix(GLEAN_MARKER_PREFIX)?;
    serde_json::from_str(json).ok()
}

/// Map a parsed marker to the thrift declaration it names.
fn marker_to_thrift_decl(m: &GleanMarker) -> Option<glean::ThriftDeclaration> {
    let file = m.file.as_str();
    let decl = match m.kind.as_str() {
        "struct" => glean::ThriftDeclaration::named(file, &m.name, glean::NAMED_KIND_STRUCT),
        "union" => glean::ThriftDeclaration::named(file, &m.name, glean::NAMED_KIND_UNION),
        "enum" => glean::ThriftDeclaration::named(file, &m.name, glean::NAMED_KIND_ENUM),
        "typedef" => glean::ThriftDeclaration::named(file, &m.name, glean::NAMED_KIND_TYPEDEF),
        "exception" => glean::ThriftDeclaration::exception(file, &m.name),
        "service" => glean::ThriftDeclaration::service(file, &m.name),
        "constant" => glean::ThriftDeclaration::constant(file, &m.name),
        "function" => glean::ThriftDeclaration::function(file, m.service.as_deref()?, &m.name),
        "field" => {
            let kind = match m.container_kind.as_deref()? {
                "struct" => glean::FIELD_KIND_STRUCT,
                "exception" => glean::FIELD_KIND_EXCEPTION,
                _ => return None,
            };
            glean::ThriftDeclaration::field(file, m.container.as_deref()?, kind, &m.name)
        }
        _ => return None,
    };
    Some(decl)
}

/// Scan a generated file for `%% Glean {...}` markers. Each result pairs a thrift
/// declaration with the byte offset just past its marker line — the declaration
/// that follows that offset is the one the marker annotates.
fn parse_thrift_markers(source: &str) -> Vec<(u32, glean::ThriftDeclaration)> {
    let mut markers = vec![];
    let mut offset: u32 = 0;
    // split_inclusive keeps the line terminator, so offsets stay byte-exact
    // against ELP's spans regardless of `\n` vs `\r\n`.
    for line in source.split_inclusive('\n') {
        let next = offset + line.len() as u32;
        if let Some(marker) = parse_glean_marker(line)
            && let Some(decl) = marker_to_thrift_decl(&marker)
        {
            markers.push((next, decl));
        }
        offset = next;
    }
    markers
}

/// Returns whether `source` is a thrift-generated module. `.erl` files carry a
/// `-codegen_source(...)` attribute; `.hrl` a `% @codegen_source` comment.
fn has_codegen_source(source: &str) -> bool {
    const ATTR_PREFIX: &str = "-codegen_source(\"";
    const COMMENT_PREFIX: &str = "% @codegen_source ";
    source.lines().any(|line| {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix(ATTR_PREFIX) {
            let path = rest
                .strip_suffix("\").")
                .or_else(|| rest.strip_suffix("\")"));
            return path.is_some_and(|p| p.ends_with(".thrift"));
        }
        line.strip_prefix(COMMENT_PREFIX)
            .is_some_and(|path| path.ends_with(".thrift"))
    })
}

fn path_to_module_name(path: &VfsPath) -> Option<String> {
    match path.name_and_extension() {
        Some((name, Some("erl"))) => Some(name.to_string()),
        Some((name, Some("escript"))) => Some(format!("{name}.escript")),
        Some((name, Some("hrl"))) => Some(format!("{name}.hrl")),
        _ => None,
    }
}

fn apply_source_root(file_path: &str, source_root: Option<&str>) -> String {
    match source_root {
        Some(prefix) if !prefix.is_empty() => {
            format!("{}/{}", prefix.trim_end_matches('/'), file_path)
        }
        _ => file_path.to_string(),
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
