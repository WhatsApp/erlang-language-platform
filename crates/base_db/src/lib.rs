/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use elp_project_model::AppName;
use elp_syntax::ast::SourceFile;
use elp_syntax::AstNode;
use elp_syntax::Parse;
use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use lazy_static::lazy_static;

mod change;
mod include;
mod input;
mod module_index;

// ---------------------------------------------------------------------
// Public API

pub mod fixture;
// @fb-only: mod meta_only;
pub mod test_utils;
pub use change::Change;
pub use elp_project_model::AppType;
pub use include::IncludeCtx;
pub use input::AppData;
pub use input::AppRoots;
pub use input::AppStructure;
pub use input::FileSource;
pub use input::IncludeOtp;
pub use input::ProjectApps;
pub use input::ProjectData;
pub use input::ProjectId;
pub use input::SourceRoot;
pub use input::SourceRootId;
pub use module_index::ModuleIndex;
pub use module_index::ModuleName;
pub use module_index::Modules;
pub use paths::AbsPath;
pub use paths::AbsPathBuf;
pub use paths::RelPath;
pub use paths::RelPathBuf;
use regex::Regex;
pub use salsa;
pub use vfs::file_set::FileSet;
pub use vfs::file_set::FileSetConfig;
pub use vfs::file_set::FileSetConfigBuilder;
pub use vfs::loader;
pub use vfs::AnchoredPath;
pub use vfs::AnchoredPathBuf;
pub use vfs::ChangeKind;
pub use vfs::ChangedFile;
pub use vfs::FileId;
pub use vfs::Vfs;
pub use vfs::VfsPath;

// ---------------------------------------------------------------------

#[macro_export]
macro_rules! impl_intern_key {
    ($name:ident) => {
        impl $crate::salsa::InternKey for $name {
            fn from_intern_id(v: $crate::salsa::InternId) -> Self {
                $name(v)
            }
            fn as_intern_id(&self) -> $crate::salsa::InternId {
                self.0
            }
        }
    };
}

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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum FileKind {
    SrcModule,
    TestModule,
    Header,
    Escript,
    Other,
    OutsideProjectModel,
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

pub trait FileLoader {
    /// Text of the file.
    fn file_text(&self, file_id: FileId) -> Arc<str>;
}

/// Database which stores all significant input facts: source code and project
/// model. Everything else in ELP is derived from these queries.
#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: FileLoader + salsa::Database {
    /// Path to a file, relative to the root of its source root.
    /// Source root of the file.
    #[salsa::input]
    fn file_source_root(&self, file_id: FileId) -> SourceRootId;

    #[salsa::input]
    fn catch_all_source_root(&self) -> SourceRootId;

    /// Contents of the source root.
    #[salsa::input]
    fn source_root(&self, id: SourceRootId) -> Arc<SourceRoot>;

    #[salsa::input]
    fn app_data(&self, id: SourceRootId) -> Option<Arc<AppData>>;

    #[salsa::input]
    fn project_data(&self, id: ProjectId) -> Arc<ProjectData>;

    /// A revision number that is bumped when the file state changes.
    /// Crucially, we update it in server mode when the unsaved file
    /// contents are changed in VFS, but also when we receive a
    /// DidSave indication. So long as it is read to trigger a salsa
    /// dependency any process such as the erlang_service which
    /// accesses the on-disk file state, salsa should call it
    /// appropriately, rather than using the cached unsaved version.
    #[salsa::input]
    fn file_revision(&self, file_id: FileId) -> u64;
    // housekeeping notes:
    // - we initialise it when a file is initialised,
    //   so all the places set_file_source_root is called
    // - we bump it in all the places set_file_text is called
    // - we also bump in on DidSaveTextDocument

    /// Returns a map from module name to FileId of the containing file.
    fn module_index(&self, project_id: ProjectId) -> Arc<ModuleIndex>;

    /// Parse the file_id to AST
    fn parse(&self, file_id: FileId) -> Parse<SourceFile>;

    fn is_generated(&self, file_id: FileId) -> bool;

    fn is_otp(&self, file_id: FileId) -> Option<bool>;

    fn is_test_suite_or_test_helper(&self, file_id: FileId) -> Option<bool>;

    fn file_app_type(&self, file_id: FileId) -> Option<AppType>;

    fn file_app_name(&self, file_id: FileId) -> Option<AppName>;

    fn file_project_id(&self, file_id: FileId) -> Option<ProjectId>;

    fn file_kind(&self, file_id: FileId) -> FileKind;

    /// When we get a range from the client, limit it to what is in the source file
    fn clamp_range(&self, file_id: FileId, range: TextRange) -> TextRange;

    fn clamp_offset(&self, file_id: FileId, offset: TextSize) -> TextSize;

    #[salsa::invoke(IncludeCtx::resolve_local_query)]
    fn resolve_local(&self, source_root: SourceRootId, path: SmolStr) -> Option<FileId>;

    #[salsa::invoke(IncludeCtx::resolve_remote_query)]
    fn resolve_remote(&self, source_root: SourceRootId, path: SmolStr) -> Option<FileId>;
}

fn module_index(db: &dyn SourceDatabase, project_id: ProjectId) -> Arc<ModuleIndex> {
    let mut builder = ModuleIndex::builder();

    let project_data = db.project_data(project_id);
    for &source_root_id in &project_data.source_roots {
        if let Some(app_data) = db.app_data(source_root_id) {
            let source_root = db.source_root(source_root_id);
            for (file_id, file_source, path) in source_root.iter_app_files(&app_data) {
                if let Some((name, Some("erl"))) = path.name_and_extension() {
                    builder.insert(file_id, file_source, ModuleName::new(name));
                }
            }
        }
    }

    project_data
        .otp_project_id
        .iter()
        .for_each(|otp_project_id| {
            if *otp_project_id == project_id {
                builder.is_otp()
            } else {
                builder.set_otp(db.module_index(*otp_project_id))
            }
        });

    builder.build()
}

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> Parse<SourceFile> {
    let text = db.file_text(file_id);
    SourceFile::parse_text(&text)
}

pub fn path_for_file(db: &dyn SourceDatabase, file_id: FileId) -> Option<VfsPath> {
    let source_root_id = db.file_source_root(file_id);
    let source_root = db.source_root(source_root_id);
    source_root.path_for_file(&file_id).cloned()
}

fn is_generated(db: &dyn SourceDatabase, file_id: FileId) -> bool {
    lazy_static! {
        // We operate a byte level via a regex (as opposed to use .contains)
        // to avoid issues with UTF8 character boundaries.
        // See https://github.com/WhatsApp/erlang-language-platform/issues/24
        // The format macro is used to avoid marking the whole file as generated
        static ref RE: regex::bytes::Regex = regex::bytes::Regex::new(&format!("{}generated", "@")).unwrap();
    }
    let contents = db.file_text(file_id);
    RE.is_match(&contents.as_bytes()[0..(2001.min(contents.len()))])
}

fn is_otp(db: &dyn SourceDatabase, file_id: FileId) -> Option<bool> {
    let root_id = db.file_source_root(file_id);
    let app_data = db.app_data(root_id)?;
    let project_id = app_data.project_id;
    Some(db.project_data(project_id).otp_project_id == Some(project_id))
}

fn is_test_suite_or_test_helper(db: &dyn SourceDatabase, file_id: FileId) -> Option<bool> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nis_test_suite_or_test_helper: {:?}", file_id));
    let root_id = db.file_source_root(file_id);
    let root = db.source_root(root_id);
    let app_data = db.app_data(root_id)?;
    let path = root.path_for_file(&file_id)?;
    if app_data.is_extra_src_file(path) {
        Some(true)
    } else {
        Some(false)
    }
}

fn file_app_type(db: &dyn SourceDatabase, file_id: FileId) -> Option<AppType> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_app_type: {:?}", file_id));
    let app_data = db.app_data(db.file_source_root(file_id))?;
    Some(app_data.app_type)
}

fn file_app_name(db: &dyn SourceDatabase, file_id: FileId) -> Option<AppName> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_app_name: {:?}", file_id));
    let app_data = db.app_data(db.file_source_root(file_id))?;
    Some(app_data.name.clone())
}

fn file_project_id(db: &dyn SourceDatabase, file_id: FileId) -> Option<ProjectId> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_project_id: {:?}", file_id));
    let app_data = db.app_data(db.file_source_root(file_id))?;
    Some(app_data.project_id)
}

pub fn module_name(db: &dyn SourceDatabase, file_id: FileId) -> Option<ModuleName> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nmodule_name: {:?}", file_id));
    let source_root_id = db.file_source_root(file_id);
    let app_data = db.app_data(source_root_id)?;
    let module_index = db.module_index(app_data.project_id);
    module_index.module_for_file(file_id).cloned()
}

lazy_static! {
static ref IGNORED_SOURCES: Vec<Regex> = {
    let regexes: Vec<Vec<Regex>> = vec![
        //ignore sources goes here
        // @fb-only: meta_only::ignored_sources_regexes()
    ];
    regexes.into_iter().flatten().collect::<Vec<Regex>>()
   };
}

fn file_kind(db: &dyn SourceDatabase, file_id: FileId) -> FileKind {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_kind: {:?}", file_id));
    let source_root_id = db.file_source_root(file_id);
    let source_root = db.source_root(source_root_id);
    let ignored_path = source_root
        .path_for_file(&file_id)
        .and_then(|path| path.as_path())
        .map(|path| {
            let path = path.as_os_str().to_str().unwrap();
            IGNORED_SOURCES.iter().any(|r| r.is_match(path))
        })
        .unwrap_or(false);
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_kind: {:?}", file_id));
    if ignored_path {
        // not part of the known project model, and on list of ignored
        // sources, do not process
        FileKind::OutsideProjectModel
    } else {
        let name_and_ext = source_root
            .path_for_file(&file_id)
            .and_then(|path| path.name_and_extension());
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
fn clamp_range(db: &dyn SourceDatabase, file_id: FileId, range: TextRange) -> TextRange {
    let source_file = db.parse(file_id).tree();
    let file_range = source_file.syntax().text_range();
    let start = file_range.start();
    let end = file_range.end();
    TextRange::new(
        range.start().clamp(start, end),
        range.end().clamp(start, end),
    )
}

fn clamp_offset(db: &dyn SourceDatabase, file_id: FileId, offset: TextSize) -> TextSize {
    let source_file = db.parse(file_id).tree();
    let file_range = source_file.syntax().text_range();
    let start = file_range.start();
    let end = file_range.end();
    offset.clamp(start, end)
}

/// We don't want to give HIR knowledge of source roots, hence we extract these
/// methods into a separate DB.
#[salsa::query_group(SourceDatabaseExtStorage)]
pub trait SourceDatabaseExt: SourceDatabase {
    #[salsa::input]
    fn file_text(&self, file_id: FileId) -> Arc<str>;
}

/// Silly workaround for cyclic deps between the traits
pub struct FileLoaderDelegate<T>(pub T);

impl<T: SourceDatabaseExt> FileLoader for FileLoaderDelegate<&'_ T> {
    fn file_text(&self, file_id: FileId) -> Arc<str> {
        SourceDatabaseExt::file_text(self.0, file_id)
    }
}

pub fn bump_file_revision(file_id: FileId, db: &mut dyn SourceDatabaseExt) {
    db.set_file_revision(file_id, db.file_revision(file_id) + 1);
}

/// Read the file revision to register a salsa dependency on the
/// on-disk version
pub fn read_file_revision(file_id: FileId, db: &dyn SourceDatabaseExt) {
    let _ = db.file_revision(file_id);
}

/// If the `input` string represents an atom, and needs quoting, quote
/// it.
pub fn to_quoted_string(input: &str) -> String {
    fn is_valid_atom(input: &str) -> bool {
        let mut chars = input.chars();
        chars.next().map_or(false, |c| c.is_lowercase())
            && chars.all(|c| char::is_alphanumeric(c) || c == '_' || c == '@')
    }
    if is_valid_atom(input) {
        input.to_string()
    } else {
        format!("'{}'", &input)
    }
}
