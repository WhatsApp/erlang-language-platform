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
use std::sync::Arc;

use elp_project_model::AppName;
use elp_project_model::buck::IncludeMapping;
use elp_project_model::buck::IncludeMappingScope;
use elp_syntax::AstNode;
use elp_syntax::Parse;
use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use elp_syntax::ast::SourceFile;
use fxhash::FxHashMap;
use lazy_static::lazy_static;
use regex::Regex;

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
pub use eetf;
pub use elp_project_model::AppType;
pub use elp_project_model::otp::OTP_VERSION;
pub use elp_project_model::test_fixture::CURSOR_MARKER;
pub use elp_project_model::test_fixture::RangeOrOffset;
pub use elp_project_model::test_fixture::extract_offset;
pub use elp_project_model::test_fixture::remove_annotations;
pub use include::IncludeCtx;
pub use include::generated_file_include_lib;
pub use input::AppData;
pub use input::AppDataId;
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
pub use salsa;
pub use vfs::AnchoredPath;
pub use vfs::AnchoredPathBuf;
pub use vfs::ChangeKind;
pub use vfs::ChangedFile;
pub use vfs::FileId;
pub use vfs::Vfs;
pub use vfs::VfsPath;
pub use vfs::file_set::FileSet;
pub use vfs::file_set::FileSetConfig;
pub use vfs::file_set::FileSetConfigBuilder;
pub use vfs::loader;

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
    /// This has precedence over `@partially-generated`.
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

    /// The data for a given application. We can access this either
    /// from the `FileId` or by `SourceRootId`, so introduce an
    /// intermediate `AppDataId` to map from the two sources.
    fn app_data(&self, id: SourceRootId) -> Option<Arc<AppData>>;
    #[salsa::input]
    fn app_data_by_id(&self, id: AppDataId) -> Option<Arc<AppData>>;
    #[salsa::input]
    fn app_data_id(&self, id: SourceRootId) -> AppDataId;

    fn app_data_id_by_file(&self, id: FileId) -> Option<AppDataId>;

    fn include_file_id(&self, project_id: ProjectId, path: VfsPath) -> Option<FileId>;

    fn mapped_include_file(
        &self,
        project_id: ProjectId,
        scope: IncludeMappingScope,
        path: SmolStr,
    ) -> Option<FileId>;

    #[salsa::input]
    fn project_data(&self, id: ProjectId) -> Arc<ProjectData>;

    /// Whether ifdef/ifndef condition evaluation is enabled (experimental).
    /// When true, preprocessor conditions are evaluated and inactive forms are skipped.
    /// When false (default), all forms are treated as active (legacy behavior).
    #[salsa::input]
    fn ifdef_enabled(&self) -> bool;

    fn file_app_data(&self, file_id: FileId) -> Option<Arc<AppData>>;

    /// Returns a map from module name to FileId of the containing file.
    fn module_index(&self, project_id: ProjectId) -> Arc<ModuleIndex>;

    fn include_file_index(&self, project_id: ProjectId) -> Arc<IncludeFileIndex>;

    /// Returns a map from FileId to the AppDataId of the app the file
    /// belongs to.
    #[salsa::input]
    fn app_index(&self) -> Arc<AppDataIndex>;

    /// Parse the file_id to AST
    fn parse(&self, file_id: FileId) -> Parse<SourceFile>;

    /// Returns the generation status of the file.
    /// - `Generated`: File has `generated` marker (no manual sections)
    /// - `PartiallyGenerated`: File has `partially-generated` marker
    /// - `Regular`: No generation markers
    fn generated_status(&self, file_id: FileId) -> GeneratedStatus;

    /// Returns the ranges of manual sections in a partially-generated file.
    /// Manual sections are delimited by `% BEGIN MANUAL SECTION` and
    /// `% END MANUAL SECTION` comment markers.
    fn manual_section_ranges(&self, file_id: FileId) -> Arc<Vec<TextRange>>;

    /// The top level items are expressions, not forms.  Typically
    /// when parsing e.g. a `rebar.config` file.
    fn is_erlang_config_file(&self, file_id: FileId) -> bool;

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
    fn resolve_local(&self, file_id: FileId, path: SmolStr) -> Option<FileId>;

    #[salsa::invoke(IncludeCtx::resolve_remote_query)]
    fn resolve_remote(
        &self,
        orig_app_data_id: Option<AppDataId>,
        current_file_id: FileId,
        path: SmolStr,
    ) -> Option<FileId>;
}

fn app_data(db: &dyn SourceDatabase, id: SourceRootId) -> Option<Arc<AppData>> {
    db.app_data_by_id(db.app_data_id(id))
}

fn file_app_data(db: &dyn SourceDatabase, file_id: FileId) -> Option<Arc<AppData>> {
    let lookup = if let Some(id) = db.app_data_id_by_file(file_id) {
        db.app_data_by_id(id)
    } else {
        None
    };
    lookup
        // We need this fallback to cope with new files added in IDE
        // mode, and OTP files
        .or_else(|| {
            let source_root_id = db.file_source_root(file_id);
            db.app_data(source_root_id)
        })
}

fn module_index(db: &dyn SourceDatabase, project_id: ProjectId) -> Arc<ModuleIndex> {
    let mut builder = ModuleIndex::builder();

    let project_data = db.project_data(project_id);
    for &source_root_id in &project_data.source_roots {
        let source_root = db.source_root(source_root_id);
        for file_id in source_root.iter() {
            if db.file_kind(file_id).is_module()
                && let Some(app_data) = db.file_app_data(file_id)
                && let Some((file_id, file_source, path)) =
                    source_root.file_info(file_id, &app_data)
                && let Some((name, Some("erl"))) = path.name_and_extension()
            {
                builder.insert(file_id, file_source, ModuleName::new(name));
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
    /// Mapping from the raw text seen in an `-include` or
    /// `-include_lib` directive to the associated file
    pub include_mapping: Arc<IncludeMapping>,
}

impl IncludeFileIndex {
    pub fn add(&mut self, path: &VfsPath, file_id: FileId) {
        self.path_to_file_id.insert(path.clone(), file_id);
    }
}

fn include_file_index(db: &dyn SourceDatabase, project_id: ProjectId) -> Arc<IncludeFileIndex> {
    let mut include_file_index = IncludeFileIndex::default();
    let project_data = db.project_data(project_id);
    build_include_file_index(db, &project_data, &mut include_file_index);
    project_data
        .otp_project_id
        .iter()
        .for_each(|otp_project_id| {
            if *otp_project_id != project_id {
                build_include_file_index(
                    db,
                    &db.project_data(*otp_project_id),
                    &mut include_file_index,
                );
            }
        });
    if let Some(include_mapping) = &project_data.include_mapping {
        include_file_index.include_mapping = include_mapping.clone();
    }
    Arc::new(include_file_index)
}

fn build_include_file_index(
    db: &dyn SourceDatabase,
    project_data: &Arc<ProjectData>,
    include_file_index: &mut IncludeFileIndex,
) {
    for &source_root_id in &project_data.source_roots {
        let source_root = db.source_root(source_root_id);
        for file_id in source_root.iter() {
            if let Some(path) = source_root.path_for_file(&file_id) {
                if let Some((_name, Some("hrl"))) = path.name_and_extension() {
                    include_file_index.add(path, file_id);
                }
            } else {
                log::warn!("No file path for {file_id:?}");
            }
        }
    }
}

fn include_file_id(
    db: &dyn SourceDatabase,
    project_id: ProjectId,
    path: VfsPath,
) -> Option<FileId> {
    let include_file_index = db.include_file_index(project_id);
    include_file_index.path_to_file_id.get(&path).copied()
}

fn mapped_include_file(
    db: &dyn SourceDatabase,
    project_id: ProjectId,
    scope: IncludeMappingScope,
    path: SmolStr,
) -> Option<FileId> {
    let include_file_index = db.include_file_index(project_id);
    let file_path = include_file_index.include_mapping.get(scope, &path)?;
    include_file_index
        .path_to_file_id
        .get(&VfsPath::from(file_path.clone()))
        .copied()
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct AppDataIndex {
    pub map: FxHashMap<FileId, AppDataId>,
}

fn app_data_id_by_file(db: &dyn SourceDatabase, file_id: FileId) -> Option<AppDataId> {
    let app_data_index = db.app_index();
    app_data_index.map.get(&file_id).copied()
}

pub fn set_app_data_id_by_file(db: &mut dyn SourceDatabase, id: FileId, app_data_id: AppDataId) {
    let mut app_data_index: Arc<AppDataIndex> = db.app_index();
    Arc::make_mut(&mut app_data_index)
        .map
        .insert(id, app_data_id);
    db.set_app_index(app_data_index);
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

fn generated_status(db: &dyn SourceDatabase, file_id: FileId) -> GeneratedStatus {
    lazy_static! {
        // We operate at byte level via a regex (as opposed to use .contains)
        // to avoid issues with UTF8 character boundaries.
        // See https://github.com/WhatsApp/erlang-language-platform/issues/24
        // The format macro is used to avoid marking the whole file as generated.
        static ref RE_GENERATED: regex::bytes::Regex = regex::bytes::Regex::new(&format!("{}generated", "@")).expect("regex should be valid");
        static ref RE_PARTIALLY_GENERATED: regex::bytes::Regex = regex::bytes::Regex::new(&format!("{}partially-generated", "@")).expect("regex should be valid");
    }
    let contents = db.file_text(file_id);
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

fn manual_section_ranges(db: &dyn SourceDatabase, file_id: FileId) -> Arc<Vec<TextRange>> {
    lazy_static! {
        static ref RE_BEGIN_MANUAL: regex::bytes::Regex =
            regex::bytes::Regex::new(r"% BEGIN MANUAL SECTION").expect("regex should be valid");
        static ref RE_END_MANUAL: regex::bytes::Regex =
            regex::bytes::Regex::new(r"% END MANUAL SECTION").expect("regex should be valid");
    }

    let contents = db.file_text(file_id);
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
pub fn is_in_manual_section(db: &dyn SourceDatabase, file_id: FileId, range: TextRange) -> bool {
    if !db.generated_status(file_id).is_partially_generated() {
        return false;
    }

    let manual_ranges = db.manual_section_ranges(file_id);
    manual_ranges
        .iter()
        .any(|manual_range| manual_range.contains_range(range))
}

fn is_erlang_config_file(db: &dyn SourceDatabase, file_id: FileId) -> bool {
    let parse = db.parse(file_id);
    parse.tree().is_erlang_config_file()
}

fn is_otp(db: &dyn SourceDatabase, file_id: FileId) -> Option<bool> {
    let app_data = db.file_app_data(file_id)?;
    let project_id = app_data.project_id;
    Some(db.project_data(project_id).otp_project_id == Some(project_id))
}

fn is_test_suite_or_test_helper(db: &dyn SourceDatabase, file_id: FileId) -> Option<bool> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nis_test_suite_or_test_helper: {file_id:?}"));
    let app_data = db.file_app_data(file_id)?;
    let root_id = db.file_source_root(file_id);
    let root = db.source_root(root_id);
    let path = root.path_for_file(&file_id)?;
    if app_data.is_extra_src_file(path) {
        Some(true)
    } else {
        Some(false)
    }
}

fn file_app_type(db: &dyn SourceDatabase, file_id: FileId) -> Option<AppType> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_app_type: {file_id:?}"));
    let app_data = db.file_app_data(file_id)?;
    Some(app_data.app_type)
}

fn file_app_name(db: &dyn SourceDatabase, file_id: FileId) -> Option<AppName> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_app_name: {file_id:?}"));
    let app_data = db.file_app_data(file_id)?;
    Some(app_data.name.clone())
}

fn file_project_id(db: &dyn SourceDatabase, file_id: FileId) -> Option<ProjectId> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_project_id: {file_id:?}"));
    let app_data = db.file_app_data(file_id)?;
    Some(app_data.project_id)
}

pub fn module_name(db: &dyn SourceDatabase, file_id: FileId) -> Option<ModuleName> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nmodule_name: {file_id:?}"));
    let app_data = db.file_app_data(file_id)?;
    let module_index = db.module_index(app_data.project_id);
    module_index.module_for_file(file_id).cloned()
}

lazy_static! {
static ref IGNORED_SOURCES: Vec<Regex> = {
    let regexes: Vec<Vec<Regex>> = vec![
        vec![Regex::new(r"^.*_SUITE_data/.+$").expect("regex should be valid")],
        //ignore sources goes here
        // @fb-only: meta_only::ignored_sources_regexes()
    ];
    regexes.into_iter().flatten().collect::<Vec<Regex>>()
   };
}

fn file_kind(db: &dyn SourceDatabase, file_id: FileId) -> FileKind {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nfile_kind: {file_id:?}"));
    let source_root_id = db.file_source_root(file_id);
    let source_root = db.source_root(source_root_id);
    let ignored_path = source_root
        .path_for_file(&file_id)
        .and_then(|path| path.as_path())
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
        Cow::Owned(format!("'{}'", &input))
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
