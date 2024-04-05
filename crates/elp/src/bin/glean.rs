/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::io::Write;
use std::mem;
use std::path::PathBuf;

use anyhow::Result;
use elp::build::load;
use elp::cli::Cli;
use elp_ide::elp_ide_db::elp_base_db::module_name;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::ModuleName;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_ide::elp_ide_db::LineIndexDatabase;
use elp_ide::elp_ide_db::RootDatabase;
use elp_ide::Analysis;
use elp_ide::TextRange;
use elp_project_model::AppType;
use elp_project_model::DiscoverConfig;
use elp_syntax::ast;
use elp_syntax::AstNode;
use hir::db::DefDatabase;
use hir::fold;
use hir::fold::AnyCallBackCtx;
use hir::sema::to_def::resolve_call_target;
use hir::sema::to_def::resolve_type_target;
use hir::Body;
use hir::CallTarget;
use hir::Expr;
use hir::ExprId;
use hir::ExprSource;
use hir::InFile;
use hir::Literal;
use hir::Name;
use hir::Pat;
use hir::Semantic;
use hir::Strategy;
use hir::TypeExpr;
use hir::TypeExprId;
use serde::Serialize;

use crate::args::Glean;

#[derive(Serialize, Debug, Eq, Hash, PartialEq)]
struct GleanFileId(u32);

impl GleanFileId {
    fn new(file_id: FileId) -> Self {
        Self(file_id.0 + 1)
    }
}

impl From<FileId> for GleanFileId {
    fn from(value: FileId) -> Self {
        GleanFileId::new(value)
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
    key: FileLinesFactKey,
}

impl FileLinesFact {
    fn new(file_id: FileId, lengths: Vec<u32>, ends_with_new_line: bool) -> Self {
        FileLinesFact {
            key: FileLinesFactKey {
                file_id: file_id.into(),
                lengths,
                ends_with_new_line,
                unicode_or_tabs: true,
            },
        }
    }
}

#[derive(Serialize, Debug)]
struct FileLinesFactKey {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    lengths: Vec<u32>,
    #[serde(rename = "endsInNewline")]
    ends_with_new_line: bool,
    #[serde(rename = "hasUnicodeOrTabs")]
    unicode_or_tabs: bool,
}

#[derive(Serialize, Debug)]
pub(crate) struct FunctionDeclarationFact {
    key: FunctionDeclarationKey,
}

impl FunctionDeclarationFact {
    fn new(file_id: FileId, fqn: MFA, span: Location) -> Self {
        Self {
            key: FunctionDeclarationKey {
                file_id: file_id.into(),
                fqn,
                span,
            },
        }
    }
}

#[derive(Serialize, Debug)]
struct FunctionDeclarationKey {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    fqn: MFA,
    span: Location,
}

#[derive(Serialize, Debug)]
pub(crate) struct XRefFact {
    key: XRefFactKey,
}

impl XRefFact {
    fn new(file_id: FileId, xrefs: Vec<XRefFactVal>) -> Self {
        Self {
            key: XRefFactKey {
                file_id: file_id.into(),
                xrefs,
            },
        }
    }
}

#[derive(Serialize, Debug)]
struct XRefFactKey {
    #[serde(rename = "file")]
    file_id: GleanFileId,
    xrefs: Vec<XRefFactVal>,
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
}

impl MFA {
    fn new(module: &ModuleName, name: &Name, arity: u32) -> Self {
        Self {
            module: module.to_string(),
            name: name.to_string(),
            arity,
        }
    }
}

impl fmt::Display for MFA {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(format!("{}/{}/{}", self.module, self.name, self.arity).as_str())
    }
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
struct Location {
    start: u32,
    length: u32,
}

impl Location {
    fn new(start: u32, length: u32) -> Self {
        Self { start, length }
    }
}

impl Into<TextRange> for Location {
    fn into(self) -> TextRange {
        TextRange::at(self.start.into(), self.length.into())
    }
}

#[derive(Serialize, Debug)]
#[serde(tag = "predicate")]
pub(crate) enum Fact {
    #[serde(rename = "src.File")]
    File { facts: Vec<FileFact> },
    #[serde(rename = "src.FileLines")]
    FileLine { facts: Vec<FileLinesFact> },
    #[serde(rename = "erlang.FunctionDeclaration")]
    FunctionDeclaration { facts: Vec<FunctionDeclarationFact> },
    #[serde(rename = "erlang.XRefsViaFqnByFile")]
    XRef { facts: Vec<XRefFact> },
}

struct IndexedFacts {
    file_facts: Vec<FileFact>,
    file_line_facts: Vec<FileLinesFact>,
    declaration_facts: Vec<FunctionDeclarationFact>,
    xref_facts: Vec<XRefFact>,
}

impl IndexedFacts {
    fn new() -> Self {
        Self {
            file_facts: vec![],
            file_line_facts: vec![],
            declaration_facts: vec![],
            xref_facts: vec![],
        }
    }
}

pub struct GleanIndexer {
    project_id: ProjectId,
    analysis: Analysis,
    module: Option<String>,
}

pub fn index(args: &Glean, cli: &mut dyn Cli) -> Result<()> {
    let indexer = GleanIndexer::new(args, cli)?;
    let facts = indexer.index()?;
    write_results(facts, cli, &args.to)
}

fn write_results(
    mut indexed_facts: IndexedFacts,
    cli: &mut dyn Cli,
    to: &Option<PathBuf>,
) -> Result<()> {
    let facts = vec![
        Fact::File {
            facts: mem::take(&mut indexed_facts.file_facts),
        },
        Fact::FileLine {
            facts: mem::take(&mut indexed_facts.file_line_facts),
        },
        Fact::FunctionDeclaration {
            facts: mem::take(&mut indexed_facts.declaration_facts),
        },
        Fact::XRef {
            facts: mem::take(&mut indexed_facts.xref_facts),
        },
    ];
    let content = serde_json::to_string(&facts)?;
    match to {
        Some(to) => std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(to)?
            .write_all(content.as_bytes()),
        None => cli.write_all(content.as_bytes()),
    }?;
    Ok(())
}

impl GleanIndexer {
    pub fn new(args: &Glean, cli: &mut dyn Cli) -> Result<Self> {
        let config = DiscoverConfig::buck();
        let loaded = load::load_project_at(
            cli,
            &args.project,
            config,
            IncludeOtp::Yes,
            elp_eqwalizer::Mode::Cli,
        )?;
        let analysis = loaded.analysis();
        let indexer = Self {
            project_id: loaded.project_id,
            analysis,
            module: args.module.clone(),
        };
        Ok(indexer)
    }

    fn index(&self) -> Result<IndexedFacts> {
        let ctx = self.analysis.with_db(|db| {
            let mut ctx = IndexedFacts::new();
            if let Some(module) = &self.module {
                let index = db.module_index(self.project_id);
                let file_id = index
                    .file_for_module(&ModuleName::new(module))
                    .expect("No module found");
                let source_root_id = db.file_source_root(file_id);
                let source_root = db.source_root(source_root_id);
                let path = source_root.path_for_file(&file_id).unwrap();
                self.index_file(&db, file_id, &path, &mut ctx).unwrap();
            } else {
                let project_data = db.project_data(self.project_id);
                for &source_root_id in &project_data.source_roots {
                    if let Some(app_data) = db.app_data(source_root_id) {
                        if app_data.app_type == AppType::App {
                            let source_root = db.source_root(source_root_id);
                            for file_id in source_root.iter() {
                                if let Some(path) = source_root.path_for_file(&file_id) {
                                    if let Err(err) = self.index_file(&db, file_id, path, &mut ctx)
                                    {
                                        log::warn!("Error indexing file {:?}: {}", path, err);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            ctx
        })?;
        Ok(ctx)
    }

    fn index_file(
        &self,
        db: &RootDatabase,
        file_id: FileId,
        path: &VfsPath,
        facts: &mut IndexedFacts,
    ) -> Result<()> {
        let file_fact = match self.file_fact(db, file_id, path) {
            Some(file_fact) => file_fact,
            None => return Ok(()),
        };
        let line_fact = self.line_fact(db, file_id);
        facts.file_facts.push(file_fact);
        facts.file_line_facts.push(line_fact);

        let module_index = db.module_index(self.project_id);
        if let Some(module) = module_index.module_for_file(file_id) {
            let decl = Self::declarations(db, file_id, module);
            facts.declaration_facts.extend(decl);
            let xref = Self::xrefs(db, file_id);
            facts.xref_facts.push(xref);
        }
        Ok(())
    }

    fn file_fact(&self, db: &RootDatabase, file_id: FileId, path: &VfsPath) -> Option<FileFact> {
        let project_data = db.project_data(self.project_id);
        let root = project_data.root_dir.as_path();
        let file_path = path.as_path()?;
        let file_path = file_path.strip_prefix(root)?;
        let file_path = file_path.as_ref().to_str()?.into();
        Some(FileFact::new(file_id, file_path))
    }

    fn line_fact(&self, db: &RootDatabase, file_id: FileId) -> FileLinesFact {
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

    fn declarations(
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
                let mfa = MFA::new(module, fun.name(), fun.arity());
                result.push(FunctionDeclarationFact::new(file_id, mfa, loc));
            }
        }
        for (ty, def) in def_map.get_types() {
            let range = def.source(db).syntax().text_range();
            let loc = range.into();
            let mfa = MFA::new(module, ty.name(), ty.arity());
            result.push(FunctionDeclarationFact::new(file_id, mfa, loc));
        }
        for (rec, def) in def_map.get_records() {
            let range = def.source(db).syntax().text_range();
            let loc = range.into();
            let mfa = MFA::new(module, rec, 99);
            result.push(FunctionDeclarationFact::new(file_id, mfa, loc));
        }
        result
    }

    fn xrefs(db: &RootDatabase, file_id: FileId) -> XRefFact {
        let sema = Semantic::new(db);
        let source_file = sema.parse(file_id);
        let xrefs = fold::fold_file(
            &sema,
            Strategy::SurfaceOnly,
            file_id,
            vec![],
            &mut |mut acc, ctx| match &ctx.item {
                hir::AnyExpr::Expr(Expr::Call { target, args }) => {
                    if let Some((body, expr_source)) = ctx.body_with_expr_source(&sema) {
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

        XRefFact::new(file_id, xrefs)
    }

    fn resolve_call(
        sema: &Semantic<'_>,
        target: &CallTarget<ExprId>,
        arity: u32,
        file_id: FileId,
        body: &Body,
        range: TextRange,
    ) -> Option<XRefFactVal> {
        let def = resolve_call_target(sema, target, arity, file_id, body)?;
        let module = &def.module?;
        let mfa = MFA::new(module, def.name.name(), arity);
        Some(XRefFactVal::new(range.into(), mfa))
    }

    fn resolve_type(
        sema: &Semantic,
        target: &CallTarget<TypeExprId>,
        arity: u32,
        file_id: FileId,
        ctx: &AnyCallBackCtx,
    ) -> Option<XRefFactVal> {
        let (body, range) = ctx.find_range(sema)?;
        let def = resolve_type_target(sema, target, arity, file_id, &body)?;
        let module = module_name(sema.db.upcast(), def.file.file_id)?;
        let mfa = MFA::new(&module, def.type_alias.name().name(), arity);
        Some(XRefFactVal::new(range.into(), mfa))
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
        let (_, range) = ctx.find_range(sema)?;
        let mfa = MFA::new(&module, &def.record.name, 99);
        Some(XRefFactVal::new(range.into(), mfa))
    }

    fn find_range(
        sema: &Semantic,
        ctx: &AnyCallBackCtx,
        source_file: &InFile<ast::SourceFile>,
        expr_source: &ExprSource,
    ) -> Option<TextRange> {
        let node = expr_source.to_node(&source_file)?;
        let range = match node {
            elp_syntax::ast::Expr::Call(expr) => expr.expr()?.syntax().text_range(),
            _ => ctx.find_range(sema)?.1,
        };
        Some(range)
    }
}

impl From<TextRange> for Location {
    fn from(range: TextRange) -> Self {
        let start: u32 = range.start().into();
        let length: u32 = range.len().into();
        Location::new(start, length)
    }
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;
    use std::collections::HashSet;

    use elp::cli::Fake;
    use elp_ide::elp_ide_db::elp_base_db::fixture::extract_annotations;
    use elp_ide::elp_ide_db::elp_base_db::fixture::WithFixture;
    use elp_ide::elp_ide_db::elp_base_db::SourceDatabaseExt;
    use elp_ide::AnalysisHost;
    use elp_project_model::test_fixture::FixtureWithProjectMeta;
    use expect_test::expect_file;

    use super::*;

    #[test]
    fn serialization_test() {
        let mut cli = Fake::default();
        let file_id = FileId(10071);
        let location = Location::new(0, 10);
        let mfa = mfa(
            "smax_product_catalog",
            "product_visibility_update_request_iq",
            0,
        );

        let file_facts = vec![
            FileFact::new(
                file_id,
                "/local/whatsapp/server/erl/groupd_service/test/p13n/grpd_p13n_new_create_group_SUITE.erl".into(),
            )
        ];
        let file_line_facts = vec![FileLinesFact::new(file_id, vec![71, 42], true)];
        let declaration_facts = vec![FunctionDeclarationFact::new(
            file_id,
            mfa.clone(),
            location.clone(),
        )];

        let xref_facts = vec![XRefFact::new(
            file_id,
            vec![XRefFactVal::new(location, mfa)],
        )];
        let facts = IndexedFacts {
            file_facts,
            file_line_facts,
            declaration_facts,
            xref_facts,
        };

        write_results(facts, &mut cli, &None).expect("success");

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
        let result = run_spec(spec, "glean_module2");
        assert_eq!(result.file_facts.len(), 1);
        let file_fact = &result.file_facts[0];
        assert_eq!(file_fact.file_path.as_str(), "src/glean_module2.erl");
    }

    #[test]
    fn line_fact_with_new_line_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module3.erl
        -module(glean_module3).
        main() ->
            bar.

        "#;
        let result = run_spec(spec, "glean_module3");
        assert_eq!(result.file_line_facts.len(), 1);
        let line_fact = &result.file_line_facts[0].key;
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
        let result = run_spec(spec, "glean_module4");
        assert_eq!(result.file_line_facts.len(), 1);
        let line_fact = &result.file_line_facts[0].key;
        assert_eq!(line_fact.ends_with_new_line, false);
        assert_eq!(line_fact.lengths, vec![24, 10, 8]);
    }

    #[test]
    fn declaration_test() {
        let module = "glean_module5";
        let spec = r#"
        //- /glean/app_glean/src/glean_module5.erl
        -module(glean_module5).
        -type tree() :: {'node', tree(), tree(), any(), any()}.
        -record(user, {name = "" :: string(),
                       notes :: tree(),
                       age :: non_neg_integer(),
                       friends=[] :: [user()],
                       bio :: string() | binary()}).
        main(A) ->
            A."#;
        let result = run_spec(spec, module);
        let decl_fact = &result.declaration_facts;
        assert_eq!(decl_fact.len(), 3);
        let main = mfa(module, "main", 1);
        let typ = mfa(module, "tree", 0);
        let rec = mfa(module, "user", 99);
        assert_eq!(decl_fact[0].key.fqn, main);
        assert_eq!(decl_fact[0].key.span, Location::new(275, 17));
        assert_eq!(decl_fact[1].key.fqn, typ);
        assert_eq!(decl_fact[1].key.span, Location::new(24, 55));
        assert_eq!(decl_fact[2].key.fqn, rec);
        assert_eq!(decl_fact[2].key.span, Location::new(80, 194));
    }

    #[test]
    fn xref_call_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module61.erl
        -module(glean_module61).
        foo(Bar) -> Bar + 1.

        //- /glean/app_glean/src/glean_module6.erl
        main() ->
            B = baz(1, 2),
        %%      ^^^ glean_module6/baz/2
            F = glean_module61:foo(B),
        %%      ^^^^^^^^^^^^^^^^^^ glean_module61/foo/1
            F.
        baz(A, B) ->
            A + B."#;

        xref_check(&spec);
    }

    #[test]
    fn xref_captured_fun_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module71.erl
        foo(Bar) -> Bar + 1.

        //- /glean/app_glean/src/glean_module7.erl
        main() ->
            Foo = fun glean_module71:foo/1,
        %%        ^^^^^^^^^^^^^^^^^^^^^^^^ glean_module71/foo/1
            Baz = fun baz/2.
        %%        ^^^^^^^^^ glean_module7/baz/2
        baz(A, B) ->
            A + B."#;

        xref_check(&spec);
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
            A + B."#;

        xref_check(&spec);
    }

    #[test]
    fn xref_record_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module9.erl
        -record(query, {
            size :: non_neg_integer()
        }).
        baz(A) ->
            #query{ size = A }.
        %%  ^^^^^^^^^^^^^^^^^^ glean_module9/query/99
        "#;

        xref_check(&spec);
    }

    #[test]
    fn xref_record_index_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module10.erl
        -record(stats, {count, time}).
        baz(Time) ->
            [{#stats.count, 1},
        %%    ^^^^^^^^^^^^ glean_module10/stats/99
            {#stats.time, Time}].
        %%   ^^^^^^^^^^^ glean_module10/stats/99

        "#;

        xref_check(&spec);
    }

    #[test]
    fn xref_record_field_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module11.erl
        -record(stats, {count, time}).
        baz(Stats) ->
            Stats#stats.count.
        %%  ^^^^^^^^^^^^^^^^^ glean_module11/stats/99
        "#;

        xref_check(&spec);
    }

    #[test]
    fn xref_record_update_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module12.erl
        -record(stats, {count, time}).
        baz(Stats, NewCnt) ->
            Stats#stats{count = NewCnt}.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ glean_module12/stats/99
        "#;

        xref_check(&spec);
    }

    #[test]
    fn xref_pat_record_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module13.erl
        -record(stats, {count, time}).
        baz(Stats) ->
            #stats{count = Count, time = Time} = Stats.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ glean_module13/stats/99
        "#;

        xref_check(&spec);
    }

    #[test]
    fn xref_pat_record_index() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module14.erl
        -record(rec, {field}).
        foo(#rec.field) -> ok.
        %%  ^^^^^^^^^^ glean_module14/rec/99
        "#;
        xref_check(&spec);
    }

    #[test]
    fn xref_record_in_type_test() {
        let spec = r#"
        //- /glean/app_glean/src/glean_module15.erl
        -record(stats, {count, time}).
        -spec baz() -> #stats{}.
        %%             ^^^^^^^^ glean_module15/stats/99
        baz() ->
            #stats{count = 1, time = 2}.
        %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ glean_module15/stats/99
        "#;
        xref_check(&spec);
    }

    fn run_spec(spec: &str, module: &str) -> IndexedFacts {
        let mut cli = Fake::default();
        let dir = FixtureWithProjectMeta::gen_project(spec);

        let args = Glean {
            project: dir.path().join("glean").join("app_glean"),
            module: Some(module.into()),
            to: None,
        };
        let indexer = GleanIndexer::new(&args, &mut cli).expect("success");
        indexer.index().expect("should be ok")
    }

    fn mfa(module: &str, name: &str, arity: u32) -> MFA {
        MFA {
            module: module.into(),
            name: name.into(),
            arity,
        }
    }

    fn xref_check(spec: &str) {
        let (db, files, _) = RootDatabase::with_many_files(spec);
        let host = AnalysisHost::new(db);
        let glean = GleanIndexer {
            project_id: ProjectId(0),
            analysis: host.analysis(),
            module: None,
        };
        let facts = glean.index().expect("success");
        let mut expected_by_file: HashMap<GleanFileId, _> = HashMap::new();
        for file_id in files {
            let text = host.raw_database().file_text(file_id);
            let annotations_set: HashSet<_> = extract_annotations(&text).into_iter().collect();
            expected_by_file.insert(file_id.into(), annotations_set);
        }
        for xref_fact in facts.xref_facts {
            let file_id = xref_fact.key.file_id;
            let annotations = expected_by_file
                .remove(&file_id)
                .expect("Annotations shold be present");
            for xref in xref_fact.key.xrefs {
                let range: TextRange = xref.source.clone().into();
                let label = xref.target.to_string();
                let tuple = (range, label);
                if !annotations.contains(&tuple) {
                    panic!("Expected to find {:?} in {:?}", tuple, &annotations);
                }
            }
        }
        assert!(expected_by_file.is_empty(), "Expected no more annotations");
    }
}
