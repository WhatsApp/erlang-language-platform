/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::convert::TryInto;
use std::iter;
use std::iter::FromIterator;
use std::ops::ControlFlow;
use std::sync::Arc;

use elp_base_db::FileId;
use elp_base_db::FileKind;
use elp_base_db::FileRange;
use elp_base_db::ProjectId;
use elp_syntax::algo;
use elp_syntax::ast;
use elp_syntax::match_ast;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::db::DefDatabase;
use hir::File;
use hir::InFile;
use hir::Semantic;
use memchr::memmem::Finder;
use once_cell::unsync::Lazy;

use crate::ReferenceType;
use crate::SymbolClass;
use crate::SymbolDefinition;

#[derive(Debug, Default, Clone)]
pub struct UsageSearchResult {
    references: FxHashMap<FileId, Vec<NameLike>>,
}

impl UsageSearchResult {
    pub fn is_empty(&self) -> bool {
        self.references.is_empty()
    }

    pub fn len(&self) -> usize {
        self.references.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &[NameLike])> + '_ {
        self.references
            .iter()
            .map(|(&file_id, refs)| (file_id, &**refs))
    }

    pub fn file_ranges(&self) -> impl Iterator<Item = FileRange> + '_ {
        self.references.iter().flat_map(|(&file_id, refs)| {
            refs.iter().map(move |name| FileRange {
                file_id,
                range: name.syntax().text_range(),
            })
        })
    }
}

impl IntoIterator for UsageSearchResult {
    type Item = (FileId, Vec<NameLike>);
    type IntoIter = <FxHashMap<FileId, Vec<NameLike>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.references.into_iter()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ReferenceCategory {
    Write,
    Read,
}

/// Generally, `search_scope` returns files that might contain references for the element.
/// For module-local things (e.g. local function) it's a module + all included headers,
/// for exported things it's the entire project (because xref violations are allowed).
/// For things defined in a header, it's all the modules including the header, and all
/// the other headers they include.
/// In some cases, the location of the references is known to within a `TextRange`,
/// e.g. for things like local variables.
#[derive(Clone, Debug)]
pub struct SearchScope {
    entries: FxHashMap<FileId, Option<TextRange>>,
}

impl SearchScope {
    pub fn single_file(file_id: FileId, range: Option<TextRange>) -> SearchScope {
        SearchScope {
            entries: FxHashMap::from_iter([(file_id, range)]),
        }
    }

    fn files(files: impl Iterator<Item = FileId>) -> SearchScope {
        SearchScope {
            entries: files.map(|file_id| (file_id, None)).collect(),
        }
    }

    pub fn file_range(range: FileRange) -> SearchScope {
        SearchScope {
            entries: std::iter::once((range.file_id, Some(range.range))).collect(),
        }
    }

    fn project(db: &dyn DefDatabase, project_id: ProjectId) -> SearchScope {
        let mut entries = FxHashMap::default();

        for &source_root_id in &db.project_data(project_id).source_roots {
            entries.extend(
                db.source_root(source_root_id)
                    .iter()
                    .map(|file_id| (file_id, None)),
            )
        }
        SearchScope { entries }
    }
}

impl IntoIterator for SearchScope {
    type Item = (FileId, Option<TextRange>);
    type IntoIter = std::collections::hash_map::IntoIter<FileId, Option<TextRange>>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_iter()
    }
}

impl SymbolDefinition {
    fn search_scope(&self, sema: &Semantic) -> SearchScope {
        if let SymbolDefinition::Var(var) = self {
            let range = var
                .source(sema.db.upcast())
                .syntax()
                .ancestors()
                .find_map(|node| {
                    match_ast! {
                        match node {
                            ast::FunctionClause(clause) => Some(clause.syntax().text_range()),
                            // In case we're outside function, limit search to a form
                            ast::Form(form) => Some(form.syntax().text_range()),
                            _ => None
                        }
                    }
                });
            SearchScope::single_file(var.file.file_id, range)
        } else if self.is_local() {
            let file = self.file();
            match file.kind(sema.db.upcast()) {
                FileKind::SrcModule | FileKind::TestModule => SearchScope::files(
                    iter::once(file.file_id).chain(file.def_map(sema.db).get_included_files()),
                ),
                FileKind::Header => {
                    let mut includers = FxHashSet::default();
                    recursive_include_files(*file, sema, &mut includers);
                    SearchScope::files(includers.into_iter())
                }
                FileKind::Escript => SearchScope::single_file(self.file().file_id, None),
                FileKind::Other => SearchScope::single_file(self.file().file_id, None),
                FileKind::OutsideProjectModel => {
                    SearchScope::single_file(self.file().file_id, None)
                }
            }
        } else {
            // Consider the entire project
            // Ideally, we'd use information about application dependencies to limit the search,
            // but our dependencies are not precise enough - especially for types
            let file_id = self.file().file_id;
            // Context for T171541590
            let _ = stdx::panic_context::enter(format!("\nsearch_scope: {:?}", file_id));
            let project_id = match sema.db.app_data(sema.db.file_source_root(file_id)) {
                Some(app_data) => app_data.project_id,
                None => return SearchScope::single_file(file_id, None),
            };
            SearchScope::project(sema.db, project_id)
        }
    }

    pub fn usages<'a>(self, sema: &'a Semantic<'_>) -> FindUsages<'a> {
        FindUsages {
            def: self,
            scope: None,
            sema,
            direct_only: false,
        }
    }
}

fn recursive_include_files(file: File, sema: &Semantic, includers: &mut FxHashSet<FileId>) {
    if !includers.contains(&file.file_id) {
        let def_map = file.def_map(sema.db);
        includers.insert(file.file_id);
        includers.extend(def_map.get_included_files());
        let header = SymbolDefinition::Header(file);
        header.usages(sema).all().iter().for_each(|(file_id, _)| {
            let file = File { file_id };
            if let FileKind::Header = file.kind(sema.db.upcast()) {
                recursive_include_files(file, sema, includers);
            }
            includers.insert(file_id);
        });
    }
}

#[derive(Clone)]
pub struct FindUsages<'a> {
    def: SymbolDefinition,
    scope: Option<&'a SearchScope>,
    sema: &'a Semantic<'a>,
    direct_only: bool,
}

impl<'a> FindUsages<'a> {
    /// Enable searching for direct references only.
    pub fn direct_only(mut self) -> FindUsages<'a> {
        self.direct_only = true;
        self
    }

    /// Limits search results to the specified `scope`
    pub fn set_scope(&mut self, scope: &'a SearchScope) -> &mut FindUsages<'a> {
        self.scope = Some(scope);
        self
    }

    pub fn at_least_one(&self) -> bool {
        let mut found = false;
        self.search(&mut |_, _| {
            found = true;
            ControlFlow::Break(())
        });
        found
    }

    pub fn all(&self) -> UsageSearchResult {
        let mut res = UsageSearchResult::default();
        self.search(&mut |file_id, reference| {
            res.references.entry(file_id).or_default().push(reference);
            ControlFlow::Continue(())
        });
        res
    }

    fn search(&self, sink: &mut dyn FnMut(FileId, NameLike) -> ControlFlow<(), ()>) {
        let _p = profile::span("FindUsages:search");
        let sema = self.sema;

        let search_scope = match self.scope {
            None => Cow::Owned(self.def.search_scope(sema)),
            Some(scope) => Cow::Borrowed(scope),
        };

        let name = self.def.search_name(sema.db);
        let finder = Finder::new(name.as_str());

        fn match_indices<'a>(
            text: &'a str,
            finder: &'a Finder<'a>,
            search_range: TextRange,
        ) -> impl Iterator<Item = TextSize> + 'a {
            finder.find_iter(text.as_bytes()).filter_map(move |idx| {
                let offset: TextSize = idx.try_into().unwrap();
                if !search_range.contains_inclusive(offset) {
                    return None;
                }
                Some(offset)
            })
        }

        fn scope_files<'a>(
            sema: &'a Semantic<'_>,
            scope: &'a SearchScope,
        ) -> impl Iterator<Item = (Arc<str>, FileId, TextRange)> + 'a {
            scope.entries.iter().map(move |(&file_id, &search_range)| {
                let text = sema.db.file_text(file_id);
                let search_range =
                    search_range.unwrap_or_else(|| TextRange::up_to(TextSize::of(&*text)));

                (text, file_id, search_range)
            })
        }

        for (text, file_id, search_range) in scope_files(sema, &search_scope) {
            let tree = Lazy::new(move || sema.parse(file_id).value.syntax().clone());
            // Search for occurrences of the items name
            for offset in match_indices(&text, &finder, search_range) {
                if let Some(name) = algo::find_node_at_offset::<NameLike>(&tree, offset) {
                    if let Some(token) = name.syntax().first_token() {
                        match SymbolClass::classify(sema, InFile::new(file_id, token)) {
                            Some(SymbolClass::Definition(_)) => {}
                            Some(SymbolClass::Reference {
                                refs: _,
                                typ: ReferenceType::Fuzzy,
                            }) => {}
                            Some(SymbolClass::Reference { refs, typ }) => {
                                if refs.iter().any(|def| {
                                    def == self.def
                                        && !(self.direct_only && typ != ReferenceType::Direct)
                                }) {
                                    match sink(file_id, name) {
                                        ControlFlow::Continue(()) => {}
                                        ControlFlow::Break(()) => return,
                                    }
                                }
                            }
                            None => {}
                        }
                    }
                }
            }
        }
    }
}

/// Represents possible ast reference points -
/// a string for header, or ast::Name for everything else
#[derive(Debug, Clone)]
pub enum NameLike {
    Name(ast::Name),
    String(ast::String),
}

impl AstNode for NameLike {
    fn can_cast(kind: elp_syntax::SyntaxKind) -> bool {
        ast::Name::can_cast(kind) || ast::String::can_cast(kind)
    }

    fn cast(syntax: elp_syntax::SyntaxNode) -> Option<Self> {
        if ast::Name::can_cast(syntax.kind()) {
            Some(Self::Name(ast::Name::cast(syntax)?))
        } else {
            Some(Self::String(ast::String::cast(syntax)?))
        }
    }

    fn syntax(&self) -> &elp_syntax::SyntaxNode {
        match self {
            NameLike::Name(name) => name.syntax(),
            NameLike::String(str) => str.syntax(),
        }
    }
}

impl From<ast::Var> for NameLike {
    fn from(var: ast::Var) -> Self {
        NameLike::Name(ast::Name::Var(var))
    }
}
