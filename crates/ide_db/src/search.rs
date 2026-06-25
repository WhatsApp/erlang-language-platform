/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;
use std::cell::LazyCell;
use std::convert::TryInto;
use std::iter;
use std::iter::FromIterator;
use std::ops::ControlFlow;
use std::sync::Arc;

use elp_base_db::FileId;
use elp_base_db::FileKind;
use elp_base_db::FileRange;
use elp_base_db::ProjectId;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use elp_syntax::algo;
use elp_syntax::ast;
use elp_syntax::match_ast;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::Body;
use hir::DefineDef;
use hir::DefineId;
use hir::Expr;
use hir::File;
use hir::InFile;
use hir::MacroName;
use hir::Name;
use hir::Pat;
use hir::Semantic;
use hir::Term;
use hir::TypeExpr;
use hir::db::DefDatabase;
use hir::form_list::PPCondition;
use hir::form_list::PPConditionResult;
use hir::form_list::PPDirective;
use memchr::memmem::Finder;

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

    pub fn project(db: &dyn DefDatabase, project_id: ProjectId) -> SearchScope {
        let mut entries = FxHashMap::default();

        for &source_root_id in &db.project_data(project_id).project_data(db).source_roots {
            entries.extend(
                db.source_root(source_root_id)
                    .source_root(db)
                    .iter()
                    .map(|file_id| (file_id, None)),
            )
        }
        SearchScope { entries }
    }

    pub fn exclude_file(mut self, file_id: FileId) -> SearchScope {
        self.entries.remove(&file_id);
        self
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
            let _ = stdx::panic_context::enter(format!("\nsearch_scope: {file_id:?}"));
            let project_id = match sema.db.file_app_data(file_id) {
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
        // Fast path: when ifdef condition evaluation is enabled, finding any
        // usage of a `-define` macro can be answered from preprocessor data
        // collected during form-list lowering, avoiding a project-wide text
        // search whose cost grows pathologically with short macro names.
        // Only applies to the natural search scope (file + included files);
        // when a custom scope or direct_only is requested, fall back to the
        // text-based search.
        if !self.direct_only
            && self.scope.is_none()
            && self.sema.db.ifdef_enabled()
            && let SymbolDefinition::Define(def) = &self.def
            && let Some(found) = define_has_usage_via_preprocessor(self.sema, def)
        {
            return found;
        }
        self.at_least_n(1)
    }

    pub fn at_least_n(&self, n: usize) -> bool {
        let mut count = 0;
        self.search(&mut |_, _| {
            count += 1;
            if count >= n {
                ControlFlow::Break(())
            } else {
                ControlFlow::Continue(())
            }
        });
        count >= n
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
        let _p = tracing::info_span!("FindUsages:search").entered();
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
                let offset: TextSize = idx.try_into().expect("file offset should fit in TextSize");
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
                let text = sema.db.file_text(file_id).text(sema.db);
                let search_range =
                    search_range.unwrap_or_else(|| TextRange::up_to(TextSize::of(&*text)));

                (text, file_id, search_range)
            })
        }

        for (text, file_id, search_range) in scope_files(sema, &search_scope) {
            let tree = LazyCell::new(move || sema.parse(file_id).value.syntax().clone());
            // Search for occurrences of the items name
            for offset in match_indices(&text, &finder, search_range) {
                if let Some(name) = algo::find_node_at_offset::<NameLike>(&tree, offset)
                    && let Some(token) = name.syntax().first_token()
                {
                    let Some(SymbolClass::Reference { refs, typ }) =
                        SymbolClass::classify(sema, InFile::new(file_id, token))
                    else {
                        continue;
                    };
                    if typ == ReferenceType::Fuzzy {
                        continue;
                    }
                    let matches_def = refs.iter().any(|def| {
                        def == self.def && !(self.direct_only && typ != ReferenceType::Direct)
                    });
                    if !matches_def {
                        continue;
                    }
                    match sink(file_id, name) {
                        ControlFlow::Continue(()) => {}
                        ControlFlow::Break(()) => return,
                    }
                }
            }
        }
    }
}

/// Returns true when there is at least one usage of `def` reachable from its
/// home file or any file it includes, using preprocessor data instead of a
/// text-based search.
///
/// The search scope is the macro's home file plus its included headers, so
/// this only applies when the home file is a module — for header-defined
/// macros the natural scope is the set of includers, which the caller must
/// pass explicitly via `set_scope` (which disables this fast path).
///
/// Strategy:
///   1. If the macro's bare name appears in any `-ifdef`, `-ifndef`, or
///      `-undef` directive in the scope, count that as a usage.
///   2. Otherwise, if the bare name appears in any `?MACRO` / `??MACRO`
///      reference cached on the form list, that is a usage — unless the
///      macro is arity-overloaded in the active branch, in which case the
///      name-level match is ambiguous and we fall through to body-level
///      `DefineId` resolution to be precise.
fn define_has_usage_via_preprocessor(sema: &Semantic, def: &DefineDef) -> Option<bool> {
    let file_id = def.file.file_id;
    match def.file.kind(sema.db.upcast()) {
        FileKind::SrcModule | FileKind::TestModule => {}
        _ => return None,
    }
    let macro_name = &def.define.name;
    let bare_name = macro_name.name();

    let scope_files: Vec<FileId> = std::iter::once(file_id)
        .chain(sema.def_map(file_id).get_included_files())
        .collect();

    if scope_files
        .iter()
        .any(|&fid| pp_directive_references(sema, fid, bare_name))
    {
        return Some(true);
    }

    if !scope_files
        .iter()
        .any(|&fid| sema.form_list(fid).macro_usages().contains(bare_name))
    {
        return Some(false);
    }

    // Arity overloading must be checked across the whole scope, not just the
    // home file: e.g. FOO/0 in the module and FOO/1 in an included header
    // both contribute to the bare-name match in `macro_usages`, so the
    // name-level match is ambiguous.
    let total_with_bare_name: usize = scope_files
        .iter()
        .map(|&fid| count_active_defines_with_name(sema, fid, bare_name))
        .sum();
    if total_with_bare_name <= 1 {
        // No arity overloading: the name-level match is sufficient.
        // Body-level DefineId checks can miss usages in form-list
        // positions (e.g. -record(?NAME, {})) and unresolved macro
        // calls (macro_def is None).
        return Some(true);
    }

    let Some(&target) = active_define_ids(sema, file_id).get(macro_name) else {
        return Some(false);
    };
    let target = InFile::new(file_id, target);
    Some(
        scope_files
            .iter()
            .any(|&fid| has_macro_usage_in_bodies(sema, fid, target)),
    )
}

fn pp_directive_references(sema: &Semantic, file_id: FileId, name: &Name) -> bool {
    let form_list = sema.form_list(file_id);
    form_list.pp_conditions().any(|(_, cond)| {
        matches!(
            cond,
            PPCondition::Ifdef { name: n, .. } | PPCondition::Ifndef { name: n, .. } if n == name
        )
    }) || form_list
        .pp_stack()
        .iter()
        .any(|(_, directive)| matches!(directive, PPDirective::Undef { name: n, .. } if n == name))
}

fn count_active_defines_with_name(sema: &Semantic, file_id: FileId, bare_name: &Name) -> usize {
    let form_list = sema.form_list(file_id);
    form_list
        .define_attributes()
        .filter(|(_, define)| {
            form_list.is_form_active(sema.db, file_id, &define.pp_ctx, None)
                != PPConditionResult::Inactive
        })
        .filter(|(_, define)| define.name.name() == bare_name)
        .count()
}

fn active_define_ids(sema: &Semantic, file_id: FileId) -> FxHashMap<MacroName, DefineId> {
    // Only include defines from active branches. The raw form_list contains
    // every -define regardless of ifdef state, so a last-wins collect can
    // pick an inactive DefineId (e.g. from -else when -ifdef is active).
    // Body lowering resolves macro calls via the preprocessor's active-only
    // state, so an inactive target here would never match and produce false
    // positives.
    let form_list = sema.form_list(file_id);
    form_list
        .define_attributes()
        .filter(|(_, define)| {
            form_list.is_form_active(sema.db, file_id, &define.pp_ctx, None)
                != PPConditionResult::Inactive
        })
        .map(|(id, define)| (define.name.clone(), id))
        .collect()
}

fn has_macro_usage_in_bodies(sema: &Semantic, file_id: FileId, target: InFile<DefineId>) -> bool {
    let form_list = sema.form_list(file_id);
    form_list.forms().iter().any(|&form_idx| {
        sema.get_body_and_map(file_id, form_idx)
            .is_some_and(|(body, _)| body_references_macro(&body, target))
    })
}

fn body_references_macro(body: &Body, target: InFile<DefineId>) -> bool {
    body.exprs.iter().any(
        |(_, expr)| matches!(expr, Expr::MacroCall { macro_def: Some(def), .. } if *def == target),
    ) || body.pats.iter().any(
        |(_, pat)| matches!(pat, Pat::MacroCall { macro_def: Some(def), .. } if *def == target),
    ) || body.type_exprs.iter().any(
        |(_, te)| matches!(te, TypeExpr::MacroCall { macro_def: Some(def), .. } if *def == target),
    ) || body.terms.iter().any(
        |(_, term)| matches!(term, Term::MacroCall { macro_def: Some(def), .. } if *def == target),
    )
}

/// Represents possible ast reference points -
/// a string for header, or ast::Name for everything else
#[derive(Debug, Clone, PartialEq, Eq)]
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
