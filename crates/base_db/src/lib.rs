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
use std::hash::BuildHasherDefault;
use std::sync::Arc;
use std::sync::LazyLock;

use dashmap::DashMap;
use dashmap::Entry;
use elp_project_model::buck::BuckProjectIndex;
use elp_project_model::buck::IncludeMappingScope;
use elp_project_model::buck::ReachableTargets;
use elp_project_model::buck::TargetFullName;
use elp_syntax::AstNode;
use elp_syntax::Parse;
use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use elp_syntax::ast::SourceFile;
use fxhash::FxHashMap;
use fxhash::FxHasher;
use regex::Regex;
use smallvec::SmallVec;

mod dynamic_calls;
mod file_map;
mod file_set_config;
mod include;
mod input;
mod line_endings;
mod lower;
mod module_index;
mod ownership;
#[cfg(test)]
mod test_db;

// ---------------------------------------------------------------------
// Public API

pub mod fixture;
pub mod in_flight;
// @fb-only: mod meta_only;
pub mod test_utils;
pub use eetf;
pub use elp_project_model::AppName;
pub use elp_project_model::AppType;
pub use elp_project_model::buck::DepKind;
pub use elp_project_model::otp::Otp;
pub use elp_project_model::test_fixture::CURSOR_MARKER;
pub use elp_project_model::test_fixture::RangeOrOffset;
pub use elp_project_model::test_fixture::extract_offset;
pub use elp_project_model::test_fixture::remove_annotations;
pub use elp_project_model::test_fixture::render_annotations;
pub use file_map::FileMap;
pub use file_set_config::FileSetConfig;
pub use file_set_config::FileSetConfigBuilder;
pub use include::IncludeCtx;
pub use include::generated_file_include_lib;
pub use input::App;
pub use input::FileSource;
pub use input::IncludeOtp;
pub use input::Project;
pub use input::ProjectApps;
pub use input::SourceRoot;
pub use input::SourceRootId;
pub use input::Workspace;
pub use line_endings::LineEndings;
pub use lower::lower;
pub use lower::set_source_roots;
pub use module_index::ModuleIndex;
pub use module_index::ModuleName;
pub use module_index::Modules;
pub use paths::AbsPath;
pub use paths::AbsPathBuf;
pub use paths::RelPath;
pub use paths::RelPathBuf;
pub use salsa;
use salsa::Setter;
pub use vfs::AnchoredPath;
pub use vfs::AnchoredPathBuf;
pub use vfs::ChangeKind;
pub use vfs::FileId;
pub use vfs::VfsPath;
pub use vfs::file_set::FileSet;
pub use vfs::loader;

// ---------------------------------------------------------------------

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct FilePosition {
    pub file_id: FileId,
    pub offset: TextSize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct FileRange {
    pub file_id: FileId,
    pub range: TextRange,
}
impl FileRange {
    pub fn cover(&self, other: FileRange) -> Option<FileRange> {
        if self.file_id == other.file_id {
            Some(FileRange {
                file_id: self.file_id,
                range: self.range.cover(other.range),
            })
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum FileKind {
    SrcModule,
    TestModule,
    Header,
    Escript,
    Other,
    OutsideProjectModel,
}

/// Status indicating whether a file is generated, partially-generated, or regular.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum GeneratedStatus {
    /// File contains the `generated` marker.
    /// This has precedence over `partially-generated`.
    Generated,
    /// File contains the `partially-generated` marker.
    /// The file contains a number of manual and generated sections.
    PartiallyGenerated,
    /// Regular file without any generation markers.
    Regular,
}

impl GeneratedStatus {
    /// Returns true if the file is fully generated (no manual sections).
    pub fn is_generated(self) -> bool {
        matches!(self, GeneratedStatus::Generated)
    }

    /// Returns true if the file is partially generated (has manual sections).
    pub fn is_partially_generated(self) -> bool {
        matches!(self, GeneratedStatus::PartiallyGenerated)
    }

    /// Returns true if the file is a regular (non-generated) file.
    pub fn is_regular(self) -> bool {
        matches!(self, GeneratedStatus::Regular)
    }
}

impl FileKind {
    pub fn is_module(self) -> bool {
        match self {
            Self::SrcModule | Self::TestModule => true,
            _ => false,
        }
    }

    pub fn is_elp_supported(self) -> bool {
        match self {
            Self::SrcModule | Self::TestModule | Self::Header | Self::Escript => true,
            _ => false,
        }
    }
}

pub use dynamic_calls::AritySource;
pub use dynamic_calls::DynamicCallPatternInput;
pub use dynamic_calls::ModuleArgShape;
pub use dynamic_calls::PatternKey;
pub use dynamic_calls::build_index;
pub use dynamic_calls::parse_dynamic_call_pattern;
pub use dynamic_calls::pattern_key;

#[derive(Default, Clone)]
pub struct Files {
    files: Arc<DashMap<FileId, File, BuildHasherDefault<FxHasher>>>,
    source_roots: Arc<DashMap<SourceRootId, SourceRootInput, BuildHasherDefault<FxHasher>>>,
    file_source_roots: Arc<DashMap<FileId, FileSourceRootInput, BuildHasherDefault<FxHasher>>>,
    source_root_apps: Arc<DashMap<SourceRootId, SourceRootAppInput, BuildHasherDefault<FxHasher>>>,
}

impl std::fmt::Debug for Files {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Files").finish()
    }
}

impl Files {
    pub fn file(&self, file_id: FileId) -> File {
        *self
            .files
            .get(&file_id)
            .expect("Unable to fetch file; this is a bug")
    }

    /// # Panics
    ///
    /// Panics if `file_id` already has a `File`.
    pub fn insert_file(&self, file_id: FileId, file: File) {
        let prev = self.files.insert(file_id, file);
        assert!(prev.is_none(), "{file_id:?} should have a single File");
    }

    pub fn source_root(&self, source_root_id: SourceRootId) -> SourceRootInput {
        let source_root = self
            .source_roots
            .get(&source_root_id)
            .expect("Unable to fetch source root id; this is a bug");

        *source_root
    }

    pub fn set_source_root(
        &self,
        db: &mut dyn SourceDatabase,
        source_root_id: SourceRootId,
        source_root: Arc<SourceRoot>,
    ) {
        let source_roots = Arc::clone(&self.source_roots);
        match source_roots.entry(source_root_id) {
            Entry::Occupied(mut occupied) => {
                occupied.get_mut().set_source_root(db).to(source_root);
            }
            Entry::Vacant(vacant) => {
                let source_root = SourceRootInput::builder(source_root).new(db);
                vacant.insert(source_root);
            }
        };
    }

    pub fn file_source_root(&self, id: FileId) -> FileSourceRootInput {
        let file_source_root = self
            .file_source_roots
            .get(&id)
            .expect("Unable to fetch FileSourceRootInput; this is a bug");
        *file_source_root
    }

    pub fn set_file_source_root(
        &self,
        db: &mut dyn SourceDatabase,
        id: FileId,
        source_root_id: SourceRootId,
    ) {
        let file_source_roots = Arc::clone(&self.file_source_roots);
        match file_source_roots.entry(id) {
            Entry::Occupied(mut occupied) => {
                occupied.get_mut().set_source_root_id(db).to(source_root_id);
            }
            Entry::Vacant(vacant) => {
                let file_source_root = FileSourceRootInput::builder(source_root_id).new(db);
                vacant.insert(file_source_root);
            }
        };
    }

    pub fn source_root_app(&self, source_root_id: SourceRootId) -> SourceRootAppInput {
        let source_root_app = self
            .source_root_apps
            .get(&source_root_id)
            .expect("Unable to fetch the app of a source root; this is a bug");
        *source_root_app
    }

    /// Sets the app of a source root, if it changed.
    pub fn set_source_root_app(
        &self,
        db: &mut dyn SourceDatabase,
        source_root_id: SourceRootId,
        app: Option<App>,
    ) {
        let existing = self
            .source_root_apps
            .get(&source_root_id)
            .map(|input| *input);
        match existing {
            Some(input) => {
                if input.app(db) != app {
                    input.set_app(db).to(app);
                }
            }
            None => {
                let input = SourceRootAppInput::builder(app).new(db);
                self.source_root_apps.insert(source_root_id, input);
            }
        }
    }
}

/// A file, whether or not it exists. Each path has one `File` for the life of
/// the process: deleting the file sets `exists` to false and clears `text`, and
/// recreating it sets `exists` back on the same `File`.
#[salsa::input(debug)]
pub struct File {
    #[returns(ref)]
    pub path: VfsPath,
    #[returns(ref)]
    pub text: Arc<str>,
    pub exists: bool,
    /// Those of the file on disk or in the editor, for edits sent back to it.
    pub line_endings: LineEndings,
}

impl File {
    /// Creates the `File` of a newly interned `file_id`.
    pub fn create(
        db: &dyn SourceDatabase,
        file_id: FileId,
        path: VfsPath,
        contents: Option<(Arc<str>, LineEndings)>,
    ) -> File {
        let exists = contents.is_some();
        let (text, line_endings) = contents.unwrap_or_else(|| (Arc::from(""), LineEndings::Unix));
        let file = File::new(db, path, text, exists, line_endings);
        db.insert_file(file_id, file);
        file
    }

    /// Sets the file's contents, `None` if it doesn't exist, writing only the
    /// fields that change: salsa's setters bump the revision without comparing
    /// values.
    pub fn set_contents(
        self,
        db: &mut dyn SourceDatabase,
        contents: Option<(Arc<str>, LineEndings)>,
    ) {
        let exists = contents.is_some();
        let (text, line_endings) = match contents {
            Some(contents) => contents,
            None => (Arc::from(""), self.line_endings(db)),
        };
        if **self.text(db) != *text {
            self.set_text(db).to(text);
        }
        if self.line_endings(db) != line_endings {
            self.set_line_endings(db).to(line_endings);
        }
        if self.exists(db) != exists {
            self.set_exists(db).to(exists);
        }
    }
}

#[salsa::input]
pub struct FileSourceRootInput {
    pub source_root_id: SourceRootId,
}

#[salsa::input]
pub struct SourceRootInput {
    pub source_root: Arc<SourceRoot>,
}

/// The app of a source root, `None` for the source root of files outside
/// every app.
#[salsa::input]
pub struct SourceRootAppInput {
    pub app: Option<App>,
}

/// Salsa-interned wrapper around `vfs::FileId`. Used to satisfy salsa 0.25's
/// "first non-`db` argument must implement `SalsaStructInDb`" constraint for
/// queries that take a `FileId`. Construct via `InternedFileId::new(db, file_id)`;
/// unwrap via `interned.file_id(db)`.
///
/// See `QUERY_GROUP_MIGRATION_PLAN.md` Phase 1 / PR 1.1.
#[salsa::interned(no_lifetime)]
pub struct InternedFileId {
    pub file_id: FileId,
}

/// Database which stores all significant input facts: source code and project
/// model. Everything else in ELP is derived from these queries.
#[ra_ap_query_group_macro::query_group]
pub trait RootQueryDb: SourceDatabase + salsa::Database {
    /// The apps of `project` that list the file, the primary owner first. A
    /// query about one file depends on this rather than on the project's whole
    /// index, so it runs again only when that file's owners change.
    #[salsa::invoke(ownership::file_apps)]
    fn file_apps(&self, project: Project, file_id: FileId) -> SmallVec<[App; 1]>;

    /// The primary owner of the file: in the first project whose apps list
    /// it, else the app of its source root.
    #[salsa::transparent]
    #[salsa::invoke(ownership::file_app)]
    fn file_app(&self, file_id: FileId) -> Option<App>;

    fn include_file_id(&self, project: Project, path: VfsPath) -> Option<FileId>;

    fn mapped_include_file(
        &self,
        project: Project,
        scope: IncludeMappingScope,
        path: SmolStr,
    ) -> Option<FileId>;

    /// Every target reachable from the buck target `source`, or `None` for a
    /// project with no build-system dependency graph. Every reference a
    /// dependency diagnostic checks, and every remote `-include_lib`, asks this
    /// of the referencing file's target, so it is computed once per target
    /// rather than per reference.
    fn reachable_targets(
        &self,
        project: Project,
        source: TargetFullName,
        dep_kind: DepKind,
    ) -> Option<Arc<ReachableTargets>>;

    /// Extra dynamic call patterns from `.elp_lint.toml` configuration.
    /// These patterns extend the built-in OTP patterns for find-references
    /// on dynamic calls (e.g., `apply/3`, `rpc:call/4`).
    ///
    /// Stored pre-indexed by `(module, function, arity)` so look-ups are
    /// O(1) (mirroring the built-in pattern index).
    #[salsa::input]
    fn extra_dynamic_call_patterns(&self) -> Arc<FxHashMap<PatternKey, DynamicCallPatternInput>>;

    /// Returns a map from module name to FileId of the containing file.
    fn module_index(&self, project: Project) -> Arc<ModuleIndex>;

    fn include_file_index(&self, project: Project) -> Arc<IncludeFileIndex>;

    /// Parse the file_id to AST
    #[salsa::transparent]
    #[salsa::invoke(parse_dispatch)]
    fn parse(&self, file_id: FileId) -> Parse<SourceFile>;

    /// Salsa-tracked sibling of `parse` that takes the `InternedFileId` wrapper
    /// directly. The non-interned `parse` is a transparent dispatch that wraps
    /// the `FileId` and forwards here; this is the cached path.
    #[salsa::invoke(parse_inner)]
    fn parse_interned(&self, fid: InternedFileId) -> Parse<SourceFile>;

    /// Returns the generation status of the file.
    /// - `Generated`: File has `generated` marker (no manual sections)
    /// - `PartiallyGenerated`: File has `partially-generated` marker
    /// - `Regular`: No generation markers
    #[salsa::transparent]
    #[salsa::invoke(generated_status_dispatch)]
    fn generated_status(&self, file_id: FileId) -> GeneratedStatus;

    #[salsa::invoke(generated_status_inner)]
    fn generated_status_interned(&self, fid: InternedFileId) -> GeneratedStatus;

    /// Returns the ranges of manual sections in a partially-generated file.
    /// Manual sections are delimited by `% BEGIN MANUAL SECTION` and
    /// `% END MANUAL SECTION` comment markers.
    #[salsa::transparent]
    #[salsa::invoke(manual_section_ranges_dispatch)]
    fn manual_section_ranges(&self, file_id: FileId) -> Arc<Vec<TextRange>>;

    #[salsa::invoke(manual_section_ranges_inner)]
    fn manual_section_ranges_interned(&self, fid: InternedFileId) -> Arc<Vec<TextRange>>;

    /// The top level items are expressions, not forms.  Typically
    /// when parsing e.g. a `rebar.config` file.
    #[salsa::transparent]
    #[salsa::invoke(is_erlang_config_file_dispatch)]
    fn is_erlang_config_file(&self, file_id: FileId) -> bool;

    #[salsa::invoke(is_erlang_config_file_inner)]
    fn is_erlang_config_file_interned(&self, fid: InternedFileId) -> bool;

    #[salsa::transparent]
    #[salsa::invoke(is_otp_dispatch)]
    fn is_otp(&self, file_id: FileId) -> Option<bool>;

    #[salsa::invoke(is_otp_inner)]
    fn is_otp_interned(&self, fid: InternedFileId) -> Option<bool>;

    #[salsa::transparent]
    #[salsa::invoke(is_test_suite_or_test_helper_dispatch)]
    fn is_test_suite_or_test_helper(&self, file_id: FileId) -> Option<bool>;

    #[salsa::invoke(is_test_suite_or_test_helper_inner)]
    fn is_test_suite_or_test_helper_interned(&self, fid: InternedFileId) -> Option<bool>;

    #[salsa::transparent]
    #[salsa::invoke(file_app_type_dispatch)]
    fn file_app_type(&self, file_id: FileId) -> Option<AppType>;

    #[salsa::invoke(file_app_type_inner)]
    fn file_app_type_interned(&self, fid: InternedFileId) -> Option<AppType>;

    #[salsa::transparent]
    #[salsa::invoke(file_app_name_dispatch)]
    fn file_app_name(&self, file_id: FileId) -> Option<AppName>;

    #[salsa::invoke(file_app_name_inner)]
    fn file_app_name_interned(&self, fid: InternedFileId) -> Option<AppName>;

    #[salsa::transparent]
    #[salsa::invoke(file_project_dispatch)]
    fn file_project(&self, file_id: FileId) -> Option<Project>;

    #[salsa::invoke(file_project_inner)]
    fn file_project_interned(&self, fid: InternedFileId) -> Option<Project>;

    #[salsa::transparent]
    #[salsa::invoke(file_kind_dispatch)]
    fn file_kind(&self, file_id: FileId) -> FileKind;

    #[salsa::invoke(file_kind_inner)]
    fn file_kind_interned(&self, fid: InternedFileId) -> FileKind;

    /// When we get a range from the client, limit it to what is in the source file
    #[salsa::transparent]
    #[salsa::invoke(clamp_range_dispatch)]
    fn clamp_range(&self, file_id: FileId, range: TextRange) -> TextRange;

    #[salsa::invoke(clamp_range_inner)]
    fn clamp_range_interned(&self, fid: InternedFileId, range: TextRange) -> TextRange;

    #[salsa::transparent]
    #[salsa::invoke(clamp_offset_dispatch)]
    fn clamp_offset(&self, file_id: FileId, offset: TextSize) -> TextSize;

    #[salsa::invoke(clamp_offset_inner)]
    fn clamp_offset_interned(&self, fid: InternedFileId, offset: TextSize) -> TextSize;

    #[salsa::transparent]
    #[salsa::invoke(IncludeCtx::resolve_local_dispatch)]
    fn resolve_local(&self, file_id: FileId, path: SmolStr) -> Option<FileId>;

    #[salsa::invoke(IncludeCtx::resolve_local_inner)]
    fn resolve_local_interned(&self, fid: InternedFileId, path: SmolStr) -> Option<FileId>;

    #[salsa::invoke_interned(IncludeCtx::resolve_remote_query)]
    fn resolve_remote(
        &self,
        orig_app: Option<App>,
        current_file_id: FileId,
        path: SmolStr,
    ) -> Option<FileId>;
}

#[salsa::db]
pub trait SourceDatabase: salsa::Database {
    fn file(&self, file_id: FileId) -> File;

    /// Records the `File` of a newly interned `file_id`; see [`File::create`].
    fn insert_file(&self, file_id: FileId, file: File);

    /// Contents of the source root.
    fn source_root(&self, id: SourceRootId) -> SourceRootInput;

    fn set_source_root(&mut self, source_root_id: SourceRootId, source_root: Arc<SourceRoot>);

    /// Source root of the file.
    fn file_source_root(&self, id: FileId) -> FileSourceRootInput;

    fn set_file_source_root(&mut self, id: FileId, source_root_id: SourceRootId);

    /// The app of the source root.
    fn source_root_app(&self, id: SourceRootId) -> SourceRootAppInput;

    fn set_source_root_app(&mut self, id: SourceRootId, app: Option<App>);
}

fn module_index(db: &dyn RootQueryDb, project: Project) -> Arc<ModuleIndex> {
    let mut builder = ModuleIndex::builder();

    for &source_root_id in project.source_roots(db).iter() {
        let source_root = db.source_root(source_root_id).source_root(db);
        for file_id in source_root.iter() {
            if db.file_kind(file_id).is_module()
                && let Some(app) = db.file_app(file_id)
                && let Some(path) = source_root.path_for_file(&file_id)
                && app.file_source(db, path).is_some()
                && let Some((name, Some("erl"))) = path.name_and_extension()
            {
                builder.insert(file_id, ModuleName::new(name));
            }
        }
    }

    if let Some(otp) = project.otp(db)
        && otp != project
    {
        builder.set_otp(db.module_index(otp))
    }

    builder.build()
}

/// Whether `defining_app` is reachable from `referencing_app`, following the edges `dep_kind` allows - see
/// [`DepKind`], which is what decides whether `distributed_dependencies` count.
///
/// An application is always reachable from itself. OTP applications are always
/// reachable, and reachability is transitive - both are
/// `AppDepGraph::is_reachable_in` semantics, for either `dep_kind`. A project
/// with no build-system dependency graph, or an application with no buck
/// target, has nothing to check against and is treated as reachable.
pub fn is_app_reachable(
    db: &dyn RootQueryDb,
    referencing_app: App,
    defining_app: &AppName,
    dep_kind: DepKind,
) -> bool {
    if referencing_app.name(db) == defining_app {
        return true;
    }

    let Some(referencing_target) = referencing_app.buck_target_name(db).as_ref() else {
        return true;
    };

    let project = referencing_app.project(db);
    let Some(buck_index) = project.buck_index(db) else {
        return true;
    };
    db.reachable_targets(project, referencing_target.clone(), dep_kind)
        .is_none_or(|reachable| {
            buck_index
                .app_deps
                .is_reachable_in(&reachable, defining_app)
        })
}

fn reachable_targets(
    db: &dyn RootQueryDb,
    project: Project,
    source: TargetFullName,
    dep_kind: DepKind,
) -> Option<Arc<ReachableTargets>> {
    let buck_index = project.buck_index(db).as_ref()?;
    Some(Arc::new(
        buck_index.app_deps.reachable_targets(&source, dep_kind),
    ))
}

/// Whether `pred` holds for any application of `project` that `file_id` is
/// compiled into, else for the file's own application, or `None` when no
/// application for the file is known, in which case there is nothing to check
/// reachability against.
pub fn any_owning_app(
    db: &dyn RootQueryDb,
    project: Project,
    file_id: FileId,
    mut pred: impl FnMut(&AppName) -> bool,
) -> Option<bool> {
    let owners = db.file_apps(project, file_id);
    if owners.is_empty() {
        return db.file_app(file_id).map(|app| pred(app.name(db)));
    }
    Some(owners.iter().any(|app| pred(app.name(db))))
}

/// Whether any application `callee_file` is compiled into is reachable from
/// `referencing_app` under `dep_kind`.
///
/// Prefer this to `is_app_reachable` when asking whether a call resolves: a
/// file compiled into several targets has no single defining application, and
/// linking any one of them satisfies the call.
pub fn is_any_owner_reachable(
    db: &dyn RootQueryDb,
    referencing_app: App,
    callee_file: FileId,
    dep_kind: DepKind,
) -> bool {
    any_owning_app(db, referencing_app.project(db), callee_file, |app| {
        is_app_reachable(db, referencing_app, app, dep_kind)
    })
    .unwrap_or(true)
}

/// A map from file path to `FileId` for each `.hrl` file we have
/// loaded.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct IncludeFileIndex {
    // TODO: should we map from the file name in the directory to a
    // set of FileId instead?
    // Then include file resolution is about finding "blah.hrl" in the
    // list, getting the paths from the FileId and making sure we have
    // the earliest match.  Or *a* match? Perhaps using a FileSet
    // This means we get candidate paths quickly, and just need to validate them.
    // Perhaps use a pre-cached version of the SourceRoot partition calcs.
    pub path_to_file_id: FxHashMap<VfsPath, FileId>,
    /// The include mapping and application dependency graph derived
    /// from the buck target graph.
    pub buck_index: Arc<BuckProjectIndex>,
}

impl IncludeFileIndex {
    pub fn add(&mut self, path: &VfsPath, file_id: FileId) {
        self.path_to_file_id.insert(path.clone(), file_id);
    }
}

fn include_file_index(db: &dyn RootQueryDb, project: Project) -> Arc<IncludeFileIndex> {
    let mut include_file_index = IncludeFileIndex::default();
    build_include_file_index(db, project, &mut include_file_index);
    if let Some(otp) = project.otp(db)
        && otp != project
    {
        build_include_file_index(db, otp, &mut include_file_index);
    }
    if let Some(buck_index) = project.buck_index(db) {
        include_file_index.buck_index = buck_index.clone();
    }
    Arc::new(include_file_index)
}

fn build_include_file_index(
    db: &dyn RootQueryDb,
    project: Project,
    include_file_index: &mut IncludeFileIndex,
) {
    for &source_root_id in project.source_roots(db).iter() {
        let source_root = db.source_root(source_root_id).source_root(db);
        for file_id in source_root.iter() {
            let path = db.file(file_id).path(db);
            if let Some((_name, Some("hrl"))) = path.name_and_extension() {
                include_file_index.add(path, file_id);
            }
        }
    }
}

fn include_file_id(db: &dyn RootQueryDb, project: Project, path: VfsPath) -> Option<FileId> {
    let include_file_index = db.include_file_index(project);
    include_file_index.path_to_file_id.get(&path).copied()
}

fn mapped_include_file(
    db: &dyn RootQueryDb,
    project: Project,
    scope: IncludeMappingScope,
    path: SmolStr,
) -> Option<FileId> {
    let include_file_index = db.include_file_index(project);
    let file_path = include_file_index.buck_index.includes.get(scope, &path)?;
    include_file_index
        .path_to_file_id
        .get(&VfsPath::from(file_path.clone()))
        .copied()
}

fn parse_dispatch(db: &dyn RootQueryDb, file_id: FileId) -> Parse<SourceFile> {
    db.parse_interned(InternedFileId::new(db, file_id))
}

fn parse_inner(db: &dyn RootQueryDb, fid: InternedFileId) -> Parse<SourceFile> {
    let file_id = fid.file_id(db);
    let text = db.file(file_id).text(db);
    SourceFile::parse_text(text)
}

fn generated_status_dispatch(db: &dyn RootQueryDb, file_id: FileId) -> GeneratedStatus {
    db.generated_status_interned(InternedFileId::new(db, file_id))
}

fn generated_status_inner(db: &dyn RootQueryDb, fid: InternedFileId) -> GeneratedStatus {
    let file_id = fid.file_id(db);

    // We operate at byte level via a regex (as opposed to use .contains)
    // to avoid issues with UTF8 character boundaries.
    // See https://github.com/WhatsApp/erlang-language-platform/issues/24
    // The format macro is used to avoid marking the whole file as generated.
    static RE_GENERATED: LazyLock<regex::bytes::Regex> = LazyLock::new(|| {
        regex::bytes::Regex::new(&format!("{}generated", "@")).expect("regex should be valid")
    });
    static RE_PARTIALLY_GENERATED: LazyLock<regex::bytes::Regex> = LazyLock::new(|| {
        regex::bytes::Regex::new(&format!("{}partially-generated", "@"))
            .expect("regex should be valid")
    });

    let contents = db.file(file_id).text(db);
    let header = &contents.as_bytes()[0..(2001.min(contents.len()))];

    // Check generated first - it takes precedence over partially-generated
    if RE_GENERATED.is_match(header) {
        GeneratedStatus::Generated
    } else if RE_PARTIALLY_GENERATED.is_match(header) {
        GeneratedStatus::PartiallyGenerated
    } else {
        GeneratedStatus::Regular
    }
}

fn manual_section_ranges_dispatch(db: &dyn RootQueryDb, file_id: FileId) -> Arc<Vec<TextRange>> {
    db.manual_section_ranges_interned(InternedFileId::new(db, file_id))
}

fn manual_section_ranges_inner(db: &dyn RootQueryDb, fid: InternedFileId) -> Arc<Vec<TextRange>> {
    let file_id = fid.file_id(db);

    static RE_BEGIN_MANUAL: LazyLock<regex::bytes::Regex> = LazyLock::new(|| {
        regex::bytes::Regex::new(r"% BEGIN MANUAL SECTION").expect("regex should be valid")
    });
    static RE_END_MANUAL: LazyLock<regex::bytes::Regex> = LazyLock::new(|| {
        regex::bytes::Regex::new(r"% END MANUAL SECTION").expect("regex should be valid")
    });

    let contents = db.file(file_id).text(db);
    let bytes = contents.as_bytes();
    let mut ranges = Vec::new();

    // Find all BEGIN markers
    let begin_positions: Vec<usize> = RE_BEGIN_MANUAL.find_iter(bytes).map(|m| m.end()).collect();

    // Find all END markers
    let end_positions: Vec<usize> = RE_END_MANUAL.find_iter(bytes).map(|m| m.start()).collect();

    // Match BEGIN/END pairs in order
    for (begin, end) in begin_positions.iter().zip(end_positions.iter()) {
        if begin < end {
            let start = TextSize::new(*begin as u32);
            let end = TextSize::new(*end as u32);
            ranges.push(TextRange::new(start, end));
        }
    }

    Arc::new(ranges)
}

/// Returns true if the given range is within a manual section of a
/// partially-generated file.
///
/// # Arguments
///
/// * `db` - The source database providing file contents and metadata
/// * `file_id` - The identifier of the file to check
/// * `range` - The text range to check for manual section containment
///
/// # Returns
///
/// `true` if the file is partially-generated and the given range falls within
/// a manual section (between `% BEGIN MANUAL SECTION` and `% END MANUAL SECTION`
/// markers), `false` otherwise.
pub fn is_in_manual_section(db: &dyn RootQueryDb, file_id: FileId, range: TextRange) -> bool {
    if !db.generated_status(file_id).is_partially_generated() {
        return false;
    }

    let manual_ranges = db.manual_section_ranges(file_id);
    manual_ranges
        .iter()
        .any(|manual_range| manual_range.contains_range(range))
}

fn is_erlang_config_file_dispatch(db: &dyn RootQueryDb, file_id: FileId) -> bool {
    db.is_erlang_config_file_interned(InternedFileId::new(db, file_id))
}

fn is_erlang_config_file_inner(db: &dyn RootQueryDb, fid: InternedFileId) -> bool {
    let file_id = fid.file_id(db);
    let parse = db.parse(file_id);
    parse.tree().is_erlang_config_file()
}

fn is_otp_dispatch(db: &dyn RootQueryDb, file_id: FileId) -> Option<bool> {
    db.is_otp_interned(InternedFileId::new(db, file_id))
}

fn is_otp_inner(db: &dyn RootQueryDb, fid: InternedFileId) -> Option<bool> {
    let file_id = fid.file_id(db);
    let project = db.file_app(file_id)?.project(db);
    Some(project.otp(db) == Some(project))
}

fn is_test_suite_or_test_helper_dispatch(db: &dyn RootQueryDb, file_id: FileId) -> Option<bool> {
    db.is_test_suite_or_test_helper_interned(InternedFileId::new(db, file_id))
}

fn is_test_suite_or_test_helper_inner(db: &dyn RootQueryDb, fid: InternedFileId) -> Option<bool> {
    let file_id = fid.file_id(db);
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nis_test_suite_or_test_helper: {file_id:?}"));
    let app = db.file_app(file_id)?;
    let path = db.file(file_id).path(db);
    if app.is_extra_src_file(db, path) {
        Some(true)
    } else {
        Some(false)
    }
}

fn file_app_type_dispatch(db: &dyn RootQueryDb, file_id: FileId) -> Option<AppType> {
    db.file_app_type_interned(InternedFileId::new(db, file_id))
}

fn file_app_type_inner(db: &dyn RootQueryDb, fid: InternedFileId) -> Option<AppType> {
    let file_id = fid.file_id(db);
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_app_type: {file_id:?}"));
    Some(db.file_app(file_id)?.app_type(db))
}

fn file_app_name_dispatch(db: &dyn RootQueryDb, file_id: FileId) -> Option<AppName> {
    db.file_app_name_interned(InternedFileId::new(db, file_id))
}

fn file_app_name_inner(db: &dyn RootQueryDb, fid: InternedFileId) -> Option<AppName> {
    let file_id = fid.file_id(db);
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_app_name: {file_id:?}"));
    Some(db.file_app(file_id)?.name(db).clone())
}

fn file_project_dispatch(db: &dyn RootQueryDb, file_id: FileId) -> Option<Project> {
    db.file_project_interned(InternedFileId::new(db, file_id))
}

fn file_project_inner(db: &dyn RootQueryDb, fid: InternedFileId) -> Option<Project> {
    let file_id = fid.file_id(db);
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_project: {file_id:?}"));
    Some(db.file_app(file_id)?.project(db))
}

pub fn module_name(db: &dyn RootQueryDb, file_id: FileId) -> Option<ModuleName> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nmodule_name: {file_id:?}"));
    let module_index = db.module_index(db.file_app(file_id)?.project(db));
    module_index.module_for_file(file_id).cloned()
}

/// Whether the file is one of its app's sources or extra sources, or neither.
pub fn file_source(db: &dyn RootQueryDb, file_id: FileId) -> Option<FileSource> {
    db.file_app(file_id)?
        .file_source(db, db.file(file_id).path(db))
}

static IGNORED_SOURCES: LazyLock<Vec<Regex>> = LazyLock::new(|| {
    let regexes: Vec<Vec<Regex>> = vec![
        vec![Regex::new(r"^.*_SUITE_data/.+$").expect("regex should be valid")],
        //ignore sources goes here
        // @fb-only: meta_only::ignored_sources_regexes(),
    ];
    regexes.into_iter().flatten().collect::<Vec<Regex>>()
});

fn file_kind_dispatch(db: &dyn RootQueryDb, file_id: FileId) -> FileKind {
    db.file_kind_interned(InternedFileId::new(db, file_id))
}

fn file_kind_inner(db: &dyn RootQueryDb, fid: InternedFileId) -> FileKind {
    let file_id = fid.file_id(db);
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_kind: {file_id:?}"));
    let path = db.file(file_id).path(db);
    let ignored_path = path
        .as_path()
        .map(|path| {
            let path = path
                .as_os_str()
                .to_str()
                .expect("path should be valid UTF-8");
            IGNORED_SOURCES.iter().any(|r| r.is_match(path))
        })
        .unwrap_or(false);
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_kind: {file_id:?}"));
    if ignored_path {
        // not part of the known project model, and on list of ignored
        // sources, do not process
        FileKind::OutsideProjectModel
    } else {
        let name_and_ext = path.name_and_extension();
        match name_and_ext {
            Some((name, Some("erl"))) => {
                if name.ends_with("_SUITE") {
                    FileKind::TestModule
                } else {
                    FileKind::SrcModule
                }
            }
            Some((_, Some("hrl"))) => FileKind::Header,
            Some((_, Some("escript"))) => FileKind::Escript,
            _ => FileKind::Other,
        }
    }
}

/// When we get a range from the client, limit it to what is in the source file
fn clamp_range_dispatch(db: &dyn RootQueryDb, file_id: FileId, range: TextRange) -> TextRange {
    db.clamp_range_interned(InternedFileId::new(db, file_id), range)
}

fn clamp_range_inner(db: &dyn RootQueryDb, fid: InternedFileId, range: TextRange) -> TextRange {
    let file_id = fid.file_id(db);
    let source_file = db.parse(file_id).tree();
    let file_range = source_file.syntax().text_range();
    let start = file_range.start();
    let end = file_range.end();
    TextRange::new(
        range.start().clamp(start, end),
        range.end().clamp(start, end),
    )
}

fn clamp_offset_dispatch(db: &dyn RootQueryDb, file_id: FileId, offset: TextSize) -> TextSize {
    db.clamp_offset_interned(InternedFileId::new(db, file_id), offset)
}

fn clamp_offset_inner(db: &dyn RootQueryDb, fid: InternedFileId, offset: TextSize) -> TextSize {
    let file_id = fid.file_id(db);
    let source_file = db.parse(file_id).tree();
    let file_range = source_file.syntax().text_range();
    let start = file_range.start();
    let end = file_range.end();
    offset.clamp(start, end)
}

/// If the `input` string represents an atom, and needs quoting, quote
/// it.
pub fn to_quoted_string(input: &str) -> Cow<'_, str> {
    fn is_valid_atom(input: &str) -> bool {
        let mut chars = input.chars();
        chars.next().is_some_and(|c| c.is_lowercase())
            && chars.all(|c| char::is_alphanumeric(c) || c == '_' || c == '@')
    }
    if is_valid_atom(input) {
        Cow::Borrowed(input)
    } else {
        Cow::Owned(format!("'{}'", input))
    }
}

// ---------------------------------------------------------------------

// We make the limit fairly generous, we hit problems on the other
// side with javascript strings > 512 Mb.
pub const MAX_LOGGED_STRING_LEN: usize = 100_000;
pub fn truncate_string(s: &str, n: usize) -> String {
    let chars: Vec<char> = s.chars().collect();
    if chars.len() > n {
        format!("{}...", chars.iter().take(n).collect::<String>())
    } else {
        s.to_string()
    }
}
pub fn limit_logged_string(s: &str) -> String {
    truncate_string(s, MAX_LOGGED_STRING_LEN)
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;
    use std::sync::Arc;

    use vfs::FileId;
    use vfs::VfsPath;

    use crate::File;
    use crate::LineEndings;
    use crate::SourceDatabase;
    use crate::assert_eq_expected;
    use crate::test_db::TestDB;

    thread_local! {
        static EXECUTIONS: Cell<usize> = const { Cell::new(0) };
    }

    #[salsa::tracked]
    fn observe(db: &dyn SourceDatabase, file: File) -> (Arc<str>, bool, LineEndings) {
        EXECUTIONS.with(|n| n.set(n.get() + 1));
        (
            file.text(db).clone(),
            file.exists(db),
            file.line_endings(db),
        )
    }

    /// Runs `observe` on `file`, and returns how many times it executed.
    fn executions(db: &TestDB, file: File) -> usize {
        let before = EXECUTIONS.with(Cell::get);
        observe(db, file);
        EXECUTIONS.with(Cell::get) - before
    }

    #[test]
    fn setting_identical_contents_does_not_rerun_dependent_queries() {
        let mut db = TestDB::default();
        let path = VfsPath::new_real_path("/a.erl".to_string());
        let contents = Some((Arc::from("-module(a)."), LineEndings::Unix));
        let file = File::create(&db, FileId::from_raw(0), path, contents);
        assert_eq_expected!(1, executions(&db, file));

        file.set_contents(&mut db, Some((Arc::from("-module(a)."), LineEndings::Unix)));
        assert_eq_expected!(0, executions(&db, file), "after identical contents");

        file.set_contents(&mut db, Some((Arc::from("-module(b)."), LineEndings::Unix)));
        assert_eq_expected!(1, executions(&db, file), "after a text change");

        file.set_contents(&mut db, Some((Arc::from("-module(b)."), LineEndings::Dos)));
        assert_eq_expected!(1, executions(&db, file), "after a line-endings change");

        file.set_contents(&mut db, None);
        assert_eq_expected!(1, executions(&db, file), "after deleting the file");
        file.set_contents(&mut db, None);
        assert_eq_expected!(0, executions(&db, file), "after deleting it again");
    }
}
