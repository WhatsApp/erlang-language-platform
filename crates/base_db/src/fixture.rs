/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A set of high-level utility fixture methods to use in tests.

// Based on rust-analyzer base_db::fixture

use std::collections::hash_map::Entry;
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use elp_project_model::json::JsonConfig;
use elp_project_model::otp::Otp;
use elp_project_model::rebar::RebarProject;
use elp_project_model::temp_dir::TempDir;
use elp_project_model::test_fixture::DiagnosticsEnabled;
use elp_project_model::test_fixture::FixtureWithProjectMeta;
use elp_project_model::AppName;
use elp_project_model::AppType;
use elp_project_model::Project;
use elp_project_model::ProjectAppData;
use elp_project_model::ProjectBuildData;
use elp_project_model::ProjectManifest;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use fxhash::FxHashMap;
use lazy_static::lazy_static;
use paths::AbsPathBuf;
use regex::Regex;
use vfs::file_set::FileSet;
use vfs::FileId;
use vfs::VfsPath;

use crate::change::Change;
use crate::input::IncludeOtp;
use crate::FilePosition;
use crate::FileRange;
use crate::ProjectApps;
use crate::ProjectId;
use crate::SourceDatabaseExt;
use crate::SourceRoot;

pub trait WithFixture: Default + SourceDatabaseExt + 'static {
    fn with_single_file(fixture: &str) -> (Self, FileId) {
        let (db, fixture) = Self::with_fixture(fixture);
        assert_eq!(fixture.files.len(), 1);
        (db, fixture.files[0])
    }

    fn with_many_files(fixture: &str) -> (Self, Vec<FileId>, DiagnosticsEnabled) {
        let (db, fixture) = Self::with_fixture(fixture);
        assert!(fixture.file_position.is_none());
        (db, fixture.files, fixture.diagnostics_enabled)
    }

    fn with_position(fixture: &str) -> (Self, FilePosition, DiagnosticsEnabled) {
        let (db, fixture) = Self::with_fixture(fixture);
        (db, fixture.position(), fixture.diagnostics_enabled)
    }

    fn with_range(fixture: &str) -> (Self, FileRange) {
        let (db, fixture) = Self::with_fixture(fixture);
        (db, fixture.range())
    }

    fn with_range_or_offset(fixture: &str) -> (Self, FileId, RangeOrOffset) {
        let (db, fixture) = Self::with_fixture(fixture);
        let (file_id, range_or_offset) = fixture
            .file_position
            .expect("Could not find file position in fixture. Did you forget to add an `~`?");
        (db, file_id, range_or_offset)
    }

    fn with_fixture(fixture_str: &str) -> (Self, ChangeFixture) {
        let (fixture, change) = ChangeFixture::parse(fixture_str);
        let mut db = Self::default();
        change.apply(&mut db);
        (db, fixture)
    }
}

impl<DB: SourceDatabaseExt + Default + 'static> WithFixture for DB {}

#[derive(Clone, Debug)]
pub struct ChangeFixture {
    pub file_position: Option<(FileId, RangeOrOffset)>,
    pub files: Vec<FileId>,
    pub diagnostics_enabled: DiagnosticsEnabled,
}

struct Builder {
    project_dir: Option<TempDir>,
}

impl Builder {
    fn new(diagnostics_enabled: DiagnosticsEnabled) -> Builder {
        let project_dir = if diagnostics_enabled.needs_erlang_service() {
            let tmp_dir = TempDir::new().keep();
            let tmp_dir_path: &Path = tmp_dir.path();

            // When trying to debug a fixture, you can set it to a
            // well-known directory location as shown in the next line
            // let tmp_dir_path = Path::new("/tmp/elp_fixture");

            let _ = fs::create_dir_all(tmp_dir_path);

            // See comment in `TempDir::new` as to why we need this.
            #[cfg(target_os = "macos")]
            let tmp_dir_path = tmp_dir_path.canonicalize().unwrap();

            Some(TempDir {
                path: tmp_dir_path.to_path_buf(),
                // When trying to debug a fixture, set this to true.
                keep: false,
            })
        } else {
            None
        };

        Builder { project_dir }
    }

    fn absolute_path(&self, path: String) -> String {
        if let Some(project_dir) = &self.project_dir {
            let project_dir_str = project_dir.path().as_os_str().to_str().unwrap();
            format!("{}/{}", project_dir_str, path)
        } else {
            path
        }
    }

    fn project_dir(&self) -> Option<&Path> {
        self.project_dir.as_ref().and_then(|d| Some(d.path()))
    }
}

impl ChangeFixture {
    fn parse(test_fixture: &str) -> (ChangeFixture, Change) {
        let fixture_with_meta = FixtureWithProjectMeta::parse(test_fixture);
        let FixtureWithProjectMeta {
            fixture,
            mut diagnostics_enabled,
        } = fixture_with_meta.clone();

        let builder = Builder::new(diagnostics_enabled.clone());
        let mut change = Change::new();

        let mut files = Vec::new();
        let source_root_prefix = "/".to_string();
        let mut file_id = FileId(0);

        let mut file_position = None;
        let mut app_map = AppMap::default();
        let mut otp: Option<Otp> = None;
        let mut app_files = SourceRootMap::default();

        for entry in fixture.clone() {
            let (text, file_pos) = Self::get_text_and_pos(&entry.text, file_id);
            if entry.scratch_buffer.is_some() {
                panic!("scratch buffers no longer needed/supported");
            }
            if file_pos.is_some() {
                assert!(file_position.is_none());
                file_position = file_pos;
            }

            assert!(entry.path.starts_with(&source_root_prefix));

            let app_name = entry.app_data.name.clone();

            if let Some(otp_extra) = entry.otp {
                if otp.is_none() {
                    otp = Some(otp_extra);
                }
            }
            app_map.combine(entry.app_data);

            change.change_file(file_id, Some(Arc::from(text)));

            let path = if diagnostics_enabled.needs_erlang_service()
                && entry.path.char_indices().nth(0) == Some((0, '/'))
            {
                let mut path = entry.path.clone();
                path.remove(0);
                let path: String = builder.absolute_path(path);
                VfsPath::new_real_path(path)
            } else {
                VfsPath::new_real_path(entry.path)
            };
            app_files.insert(app_name, file_id, path);
            files.push(file_id);

            file_id.0 += 1;
        }
        if let Some(_project_dir) = builder.project_dir() {
            // We need to add the erlang module to the file contents too.
            let erts_app: ProjectAppData = OTP_ERLANG_APP.clone();
            app_map.combine(erts_app.clone());
            change.change_file(file_id, Some(Arc::from(OTP_ERLANG_MODULE.1.clone())));
            app_files.insert(
                erts_app.name,
                file_id,
                VfsPath::new_real_path(OTP_ERLANG_MODULE.0.to_string_lossy().to_string()),
            );
            // We bump the file_id in case we decide to add another
            // file later, but do not push the current one to files,
            // as it is not part of the as-written test fixture.
            file_id.0 += 1;
        }

        let otp = otp.unwrap_or_else(|| Otp {
            // We only care about the otp lib_dir for the tests
            lib_dir: AbsPathBuf::assert("/".into()),
        });

        let root = AbsPathBuf::assert("/".into());
        let apps = app_map.all_apps().cloned().collect();
        let apps_with_includes = RebarProject::add_app_includes(apps, &vec![], &otp.lib_dir);
        let rebar_project = RebarProject::new(root, Default::default());
        let mut project = Project::otp(otp, app_map.otp_apps().cloned().collect());
        project.add_apps(apps_with_includes);
        project.project_build_data = ProjectBuildData::Rebar(rebar_project);

        if let Some(project_dir) = builder.project_dir() {
            // Dump a copy of the fixture into a temp dir
            let files: Vec<(String, String)> = fixture
                .iter()
                .map(|entry| {
                    let (text, _file_pos) = Self::get_text_and_pos(&entry.text, FileId(0));
                    (entry.path.clone(), text)
                })
                .collect();
            let project_dir_str = project_dir.as_os_str().to_str().unwrap();

            let tmp_dir_path = project_dir;
            for (path, text) in files {
                let path = tmp_dir_path.join(&path[1..]);
                let parent = path.parent().unwrap();
                fs::create_dir_all(parent).unwrap();
                let mut tmp_file = File::create(path).unwrap();
                write!(tmp_file, "{}", &text).unwrap();
            }

            let json_config_file = format!("{}/build_info.json", project_dir_str);

            let mut writer = File::create(&json_config_file).unwrap();

            let json_str = serde_json::to_string_pretty::<JsonConfig>(
                &project.as_json(AbsPathBuf::assert(project_dir.to_path_buf())),
            )
            .unwrap();
            writer.write_all(json_str.as_bytes()).unwrap();

            let first_fixture = &fixture_with_meta.fixture[0];

            if first_fixture.path.char_indices().nth(0) == Some((0, '/')) {
                let mut path = first_fixture.path.clone();
                path.remove(0);
                project_dir.join(path)
            } else {
                project_dir.join(first_fixture.path.clone())
            };
            let (elp_config, manifest) =
                ProjectManifest::discover(&AbsPathBuf::assert(json_config_file.into())).unwrap();
            let loaded_project = Project::load(&manifest, elp_config.eqwalizer).unwrap();
            project = loaded_project;
        }

        let projects = [project];

        let project_apps = if diagnostics_enabled.needs_erlang_service() {
            // The static manifest already includes OTP
            let mut project_apps = ProjectApps::new(&projects, IncludeOtp::No);

            let last_project = ProjectId(projects.len() as u32 - 1);
            project_apps.all_apps.push((last_project, &OTP_ERLANG_APP));
            project_apps
        } else {
            ProjectApps::new(&projects, IncludeOtp::Yes)
        };
        change.set_app_structure(project_apps.app_structure());

        let mut roots = Vec::new();
        // We must iterate here in project_apps order, it defines the
        // mapping of apps to SourceRootIds
        for (_project_id, app) in project_apps.all_apps {
            if let Some(file_set) = app_files.app_map.get(&app.name) {
                let root = SourceRoot::new(file_set.clone());
                roots.push(root);
            } else {
                // We have eqwalizer support, add an empty SourceRoot
                // to keep things lined up
                let root = SourceRoot::new(FileSet::default());
                roots.push(root);
            }
        }
        change.set_roots(roots);

        // Store the projects so the buildinfo does not get dropped
        // prematurely and the tempdir deleted.
        diagnostics_enabled.tmp_dir = builder.project_dir.map(|d| (projects.to_vec(), d));
        (
            ChangeFixture {
                file_position,
                files,
                diagnostics_enabled,
            },
            change,
        )
    }

    pub fn annotations(&self, db: &dyn SourceDatabaseExt) -> Vec<(FileRange, String)> {
        self.files
            .iter()
            .flat_map(|&file_id| {
                let text = SourceDatabaseExt::file_text(db, file_id);
                extract_annotations(&text)
                    .into_iter()
                    .map(move |(range, data)| (FileRange { file_id, range }, data))
            })
            .collect()
    }

    pub fn position(&self) -> FilePosition {
        let (file_id, range_or_offset) = self
            .file_position
            .expect("Could not find file position in fixture. Did you forget to add a `~`?");
        FilePosition {
            file_id,
            offset: range_or_offset.expect_offset(),
        }
    }

    pub fn file_id(&self) -> FileId {
        if let Some((file_id, _range_or_offset)) = self.file_position {
            file_id
        } else {
            FileId(0)
        }
    }

    pub fn range(&self) -> FileRange {
        let (file_id, range_or_offset) = self
            .file_position
            .expect("Could not find file position in fixture. Did you forget to add a `~`?");
        FileRange {
            file_id,
            range: range_or_offset.into(),
        }
    }

    fn get_text_and_pos(
        entry_text: &str,
        file_id: FileId,
    ) -> (String, Option<(FileId, RangeOrOffset)>) {
        if entry_text.contains(CURSOR_MARKER) {
            if entry_text.contains(ESCAPED_CURSOR_MARKER) {
                (
                    entry_text.replace(ESCAPED_CURSOR_MARKER, CURSOR_MARKER),
                    None,
                )
            } else {
                let (range_or_offset, text) = extract_range_or_offset(entry_text);
                let file_position = Some((file_id, range_or_offset));
                (text, file_position)
            }
        } else {
            (entry_text.to_string(), None)
        }
    }
}

lazy_static! {
    pub static ref OTP_ROOT: PathBuf =
        Otp::find_otp().expect("tests should always be able to find OTP");
    pub static ref OTP_ERTS_DIR: AbsPathBuf = get_erts_dir();
    pub static ref OTP_ERLANG_MODULE: (PathBuf, String) = get_erlang_module();
    pub static ref OTP_ERLANG_APP: ProjectAppData = ProjectAppData::fixture_app_data(
        AppName("erts".to_string()),
        OTP_ERTS_DIR.clone(),
        Vec::default(),
        vec![OTP_ERTS_DIR.join("src")],
        Vec::default(),
    );
}

fn get_erts_dir() -> AbsPathBuf {
    let (_otp, apps) = Otp::discover(OTP_ROOT.to_path_buf());
    for app in apps {
        if app.name == AppName("erts".to_string()) {
            return app.dir;
        }
    }
    panic!()
}

fn get_erlang_module() -> (PathBuf, String) {
    let erlang_path = OTP_ERTS_DIR.join("src/erlang.erl");
    let contents = std::fs::read_to_string(&erlang_path).unwrap();
    (erlang_path.into(), contents)
}

#[derive(Debug, Clone, Default)]
pub struct AppMap {
    app_map: FxHashMap<AppName, ProjectAppData>,
}

impl AppMap {
    fn combine(&mut self, other: ProjectAppData) {
        match self.app_map.entry(other.name.clone()) {
            Entry::Occupied(mut occupied) => {
                occupied.get_mut().combine(other);
            }
            Entry::Vacant(vacant) => {
                vacant.insert(other);
            }
        }
    }

    fn otp_apps(&self) -> impl Iterator<Item = &ProjectAppData> + '_ {
        self.app_map
            .values()
            .filter(|pd| pd.app_type == AppType::Otp)
    }

    fn all_apps(&self) -> impl Iterator<Item = &ProjectAppData> + '_ {
        self.app_map
            .values()
            .filter(|pd| pd.app_type != AppType::Otp)
    }
}

#[derive(Debug, Clone, Default)]
pub struct SourceRootMap {
    app_map: FxHashMap<AppName, FileSet>,
}

impl SourceRootMap {
    fn insert(&mut self, app_name: AppName, file_id: FileId, path: VfsPath) {
        self.app_map
            .entry(app_name)
            .or_default()
            .insert(file_id, path);
    }
}

// ---------------------------------------------------------------------

/// Infallible version of `try_extract_offset()`.
pub fn extract_offset(text: &str) -> (TextSize, String) {
    match try_extract_marker(CURSOR_MARKER, text) {
        None => panic!("text should contain cursor marker"),
        Some(result) => result,
    }
}

/// Infallible version of `try_extract_offset()`.
pub fn extract_marker_offset(marker: &str, text: &str) -> (TextSize, String) {
    match try_extract_marker(marker, text) {
        None => panic!("text should contain marker '{}'", marker),
        Some(result) => result,
    }
}

pub const CURSOR_MARKER: &str = "~";
pub const ESCAPED_CURSOR_MARKER: &str = "\\~";

/// Returns the offset of the first occurrence of `~` marker and the copy of `text`
/// without the marker.
fn try_extract_offset(text: &str) -> Option<(TextSize, String)> {
    try_extract_marker(CURSOR_MARKER, text)
}

fn try_extract_marker(marker: &str, text: &str) -> Option<(TextSize, String)> {
    let cursor_pos = text.find(marker)?;
    let mut new_text = String::with_capacity(text.len() - marker.len());
    new_text.push_str(&text[..cursor_pos]);
    new_text.push_str(&text[cursor_pos + marker.len()..]);
    let cursor_pos = TextSize::from(cursor_pos as u32);
    Some((cursor_pos, new_text))
}

/// Infallible version of `try_extract_range()`.
pub fn extract_range(text: &str) -> (TextRange, String) {
    match try_extract_range(text) {
        None => panic!("text should contain cursor marker"),
        Some(result) => result,
    }
}

/// Returns `TextRange` between the first two markers `^...^` and the copy
/// of `text` without both of these markers.
fn try_extract_range(text: &str) -> Option<(TextRange, String)> {
    let (start, text) = try_extract_offset(text)?;
    let (end, text) = try_extract_offset(&text)?;
    Some((TextRange::new(start, end), text))
}

/// Extracts ranges, marked with `<tag> </tag>` pairs from the `text`
pub fn extract_tags(mut text: &str, tag: &str) -> (Vec<(TextRange, Option<String>)>, String) {
    let open = format!("<{tag}");
    let close = format!("</{tag}>");
    let mut ranges = Vec::new();
    let mut res = String::new();
    let mut stack = Vec::new();
    loop {
        match text.find('<') {
            None => {
                res.push_str(text);
                break;
            }
            Some(i) => {
                res.push_str(&text[..i]);
                text = &text[i..];
                if text.starts_with(&open) {
                    let close_open = text.find('>').unwrap();
                    let attr = text[open.len()..close_open].trim();
                    let attr = if attr.is_empty() {
                        None
                    } else {
                        Some(attr.to_string())
                    };
                    text = &text[close_open + '>'.len_utf8()..];
                    let from = TextSize::of(&res);
                    stack.push((from, attr));
                } else if text.starts_with(&close) {
                    text = &text[close.len()..];
                    let (from, attr) = stack.pop().unwrap_or_else(|| panic!("unmatched </{tag}>"));
                    let to = TextSize::of(&res);
                    ranges.push((TextRange::new(from, to), attr));
                } else {
                    res.push('<');
                    text = &text['<'.len_utf8()..];
                }
            }
        }
    }
    assert!(stack.is_empty(), "unmatched <{}>", tag);
    ranges.sort_by_key(|r| (r.0.start(), r.0.end()));
    (ranges, res)
}

#[test]
fn test_extract_tags_1() {
    let (tags, text) = extract_tags(r#"<tag region>foo() -> ok.</tag>"#, "tag");
    let actual = tags
        .into_iter()
        .map(|(range, attr)| (&text[range], attr))
        .collect::<Vec<_>>();
    assert_eq!(actual, vec![("foo() -> ok.", Some("region".into()))]);
}

#[test]
fn test_extract_tags_2() {
    let (tags, text) = extract_tags(
        r#"bar() -> ok.\n<tag region>foo() -> ok.</tag>\nbaz() -> ok."#,
        "tag",
    );
    let actual = tags
        .into_iter()
        .map(|(range, attr)| (&text[range], attr))
        .collect::<Vec<_>>();
    assert_eq!(actual, vec![("foo() -> ok.", Some("region".into()))]);
}

#[derive(Clone, Copy, Debug)]
pub enum RangeOrOffset {
    Range(TextRange),
    Offset(TextSize),
}

impl RangeOrOffset {
    pub fn expect_offset(self) -> TextSize {
        match self {
            RangeOrOffset::Offset(it) => it,
            RangeOrOffset::Range(_) => panic!("expected an offset but got a range instead"),
        }
    }
    pub fn expect_range(self) -> TextRange {
        match self {
            RangeOrOffset::Range(it) => it,
            RangeOrOffset::Offset(_) => panic!("expected a range but got an offset"),
        }
    }
}

impl From<RangeOrOffset> for TextRange {
    fn from(selection: RangeOrOffset) -> Self {
        match selection {
            RangeOrOffset::Range(it) => it,
            RangeOrOffset::Offset(it) => TextRange::empty(it),
        }
    }
}

/// Extracts `TextRange` or `TextSize` depending on the amount of `^` markers
/// found in `text`.
///
/// # Panics
/// Panics if no `^` marker is present in the `text`.
pub fn extract_range_or_offset(text: &str) -> (RangeOrOffset, String) {
    if let Some((range, text)) = try_extract_range(text) {
        return (RangeOrOffset::Range(range), text);
    }
    let (offset, text) = extract_offset(text);
    (RangeOrOffset::Offset(offset), text)
}

// ---------------------------------------------------------------------

/// Extracts `%%^^^ some text` annotations.
///
/// A run of `^^^` can be arbitrary long and points to the corresponding range
/// in the line above.
///
/// The `%% ^file text` syntax can be used to attach `text` to the entirety of
/// the file.
///
/// The `%%<^^^ text` syntax can be used to attach `text` the span
/// starting at `%%`, rather than the first `^`.
///
/// Multiline string values are supported:
///
/// %% ^^^ first line
/// %%   | second line
///
/// Annotations point to the last line that actually was long enough for the
/// range, not counting annotations themselves. So overlapping annotations are
/// possible:
/// ```no_run
/// %% stuff        other stuff
/// %% ^^ 'st'
/// %% ^^^^^ 'stuff'
/// %%              ^^^^^^^^^^^ 'other stuff'
/// ```
pub fn extract_annotations(text: &str) -> Vec<(TextRange, String)> {
    let mut res = Vec::new();
    // map from line length to beginning of last line that had that length
    let mut line_start_map = BTreeMap::new();
    let mut line_start: TextSize = 0.into();
    let mut prev_line_annotations: Vec<(TextSize, usize)> = Vec::new();
    for (idx, line) in text.split_inclusive('\n').enumerate() {
        if idx == 0 && line.starts_with(TOP_OF_FILE_MARKER) {
            // First line, look for header marker
            if let Some(anno) = line.strip_prefix(TOP_OF_FILE_MARKER) {
                res.push((*TOP_OF_FILE_RANGE, anno.trim_end().to_string()));
            }
        } else if line.contains(TOP_OF_FILE_MARKER) {
            panic!(
                "Annotation line {} is invalid here. \
                     The top of file marker '{}' can only appear first in the file on the left margin.\n\
                     The offending line: {:?}",
                idx, TOP_OF_FILE_MARKER, line
            );
        }
        let mut this_line_annotations = Vec::new();
        let line_length = if let Some((prefix, suffix)) = line.split_once("%%") {
            let ss_len = TextSize::of("%%");
            let annotation_offset = TextSize::of(prefix) + ss_len;
            for annotation in extract_line_annotations(suffix.trim_end_matches('\n')) {
                match annotation {
                    LineAnnotation::Annotation {
                        mut range,
                        zero_offset,
                        content,
                        file,
                    } => {
                        if zero_offset {
                            range = TextRange::new(0.into(), range.end() + annotation_offset);
                        } else {
                            range += annotation_offset;
                        };
                        this_line_annotations.push((range.end(), res.len()));
                        let range = if file {
                            TextRange::up_to(TextSize::of(text))
                        } else {
                            let zero: TextSize = 0.into();
                            let line_start = line_start_map
                                .range(range.end()..)
                                .next()
                                .unwrap_or((&zero, &zero));

                            range + line_start.1
                        };
                        res.push((range, content))
                    }
                    LineAnnotation::Continuation {
                        mut offset,
                        content,
                    } => {
                        // Deal with "annotations" in files from otp
                        if res.len() > 0 {
                            offset += annotation_offset;
                            this_line_annotations.push((offset, res.len() - 1));
                            let &(_, idx) = prev_line_annotations
                                .iter()
                                .find(|&&(off, _idx)| off == offset)
                                .unwrap();
                            res[idx].1.push('\n');
                            res[idx].1.push_str(&content);
                        }
                    }
                }
            }
            annotation_offset
        } else {
            TextSize::of(line)
        };

        line_start_map = line_start_map.split_off(&line_length);
        line_start_map.insert(line_length, line_start);

        line_start += TextSize::of(line);

        if !this_line_annotations.is_empty() {
            prev_line_annotations = this_line_annotations;
        }
    }

    res
}

lazy_static! {
    static ref TOP_OF_FILE_RANGE: TextRange = TextRange::new(0.into(), 0.into());
}

const TOP_OF_FILE_MARKER: &str = "%% <<< ";

/// Return a copy of the input text, with all `%% ^^^ 💡 some text` annotations removed
pub fn remove_annotations(marker: Option<&str>, text: &str) -> String {
    let mut lines = Vec::new();
    for line in text.split('\n') {
        if !contains_annotation(line) {
            if let Some(marker) = marker {
                if let Some((_pos, clean_line)) = try_extract_marker(marker, line) {
                    lines.push(clean_line)
                } else {
                    lines.push(line.to_string())
                }
            } else {
                lines.push(line.to_string())
            }
        }
    }
    lines.join("\n")
}

/// Check if the given line contains a `%% ^^^ 💡 some text` annotation
pub fn contains_annotation(line: &str) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^\s*%%[\s<]+(\^)* +💡.*$").unwrap();
    }
    RE.is_match(line)
}

#[derive(Debug)]
enum LineAnnotation {
    Annotation {
        range: TextRange,
        /// True if the marker starts with `<`, indicating it starts
        /// at the left margin, i.e. 0
        zero_offset: bool,
        content: String,
        file: bool,
    },
    Continuation {
        offset: TextSize,
        content: String,
    },
}

fn extract_line_annotations(mut line: &str) -> Vec<LineAnnotation> {
    let mut res = Vec::new();
    let mut offset: TextSize = 0.into();
    let marker: fn(char) -> bool = if line.contains('^') {
        |c| c == '^' || c == '<'
    } else {
        |c| c == '|'
    };
    while let Some(idx) = line.find(marker) {
        offset += TextSize::try_from(idx).unwrap();
        line = &line[idx..];

        let len_prefix = line.chars().take_while(|&it| it == '<').count();
        let mut len = line[len_prefix..]
            .chars()
            .take_while(|&it| it == '^')
            .count();
        len = len + len_prefix;
        let mut continuation = false;
        if len == 0 {
            assert!(line.starts_with('|'));
            continuation = true;
            len = 1;
        }
        let range = TextRange::at(offset, len.try_into().unwrap());
        let next = line[len..].find(marker).map_or(line.len(), |it| it + len);
        let mut content = &line[len..][..next - len];

        let mut file = false;
        if !continuation && content.starts_with("file") {
            file = true;
            content = &content["file".len()..]
        }

        let content = content.trim().to_string();

        let annotation = if continuation {
            LineAnnotation::Continuation {
                offset: range.end(),
                content,
            }
        } else {
            LineAnnotation::Annotation {
                range,
                zero_offset: len_prefix > 0,
                content,
                file,
            }
        };
        res.push(annotation);

        line = &line[next..];
        offset += TextSize::try_from(next).unwrap();
    }

    res
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::ChangeFixture;
    use crate::fixture::extract_annotations;
    use crate::fixture::remove_annotations;

    #[test]
    fn test_extract_annotations_1() {
        let text = stdx::trim_indent(
            r#"
fn main() {
    let (x,     y) = (9, 2);
       %%^ def  ^ def
    zoo + 1
} %%^^^ type:
  %%  | i32

%% ^file
    "#,
        );
        let res = extract_annotations(&text)
            .into_iter()
            .map(|(range, ann)| (&text[range], ann))
            .collect::<Vec<_>>();

        assert_eq!(
            res[..3],
            [
                ("x", "def".into()),
                ("y", "def".into()),
                ("zoo", "type:\ni32".into())
            ]
        );
        assert_eq!(res[3].0.len(), 115);
    }

    #[test]
    fn test_extract_annotations_2() {
        let text = stdx::trim_indent(
            r#"
fn main() {
    (x,   y);
   %%^ a
      %%  ^ b
  %%^^^^^^^^ c
}"#,
        );
        let res = extract_annotations(&text)
            .into_iter()
            .map(|(range, ann)| (&text[range], ann))
            .collect::<Vec<_>>();

        assert_eq!(
            res,
            [
                ("x", "a".into()),
                ("y", "b".into()),
                ("(x,   y)", "c".into())
            ]
        );
    }

    #[test]
    fn test_extract_annotations_3() {
        let text = stdx::trim_indent(
            r#"
-module(foo).
bar() -> ?FOO.
       %% ^^^ error: unresolved macro `FOO`

"#,
        );
        let res = extract_annotations(&text)
            .into_iter()
            .map(|(range, ann)| (format!("{:?}", range), &text[range], ann))
            .collect::<Vec<_>>();

        assert_eq!(
            res,
            [
                (
                    "24..27".into(),
                    "FOO",
                    "error: unresolved macro `FOO`".into()
                ),
                // TODO: something weird here, this range does not tie in
                // to what the diagnostic reports.  But it shows up correcly in VsCode.
                // No time to look more deeply now.
                // ("25..28".into(), "FOO", "error: unresolved macro `FOO`".into()),
            ]
        );
    }

    #[test]
    fn extract_annotation_top_of_file_no_location() {
        let text = stdx::trim_indent(
            r#"
          %% <<< top of file, location zero as it is not associated with anything particular
            -module(main).
            main() -> ok."#,
        );
        let res = extract_annotations(&text);

        expect![[r#"
            [
                (
                    0..0,
                    "top of file, location zero as it is not associated with anything particular",
                ),
            ]
        "#]]
        .assert_debug_eq(&res);
    }

    #[test]
    #[should_panic]
    fn extract_annotations_top_of_file_syntax_only_if_at_top_of_file() {
        let text = stdx::trim_indent(
            r#"
            -module(main).
          %% <<< NOT top of file, no annotation
            main() -> ok."#,
        );
        let res = extract_annotations(&text);

        expect![[r#"
            []
        "#]]
        .assert_debug_eq(&res);
    }

    #[test]
    fn unresolved_macro_diag_include_dir() {
        // Test the scenario where the include file is referenced
        // relative to a project include directory
        let (_fixture, change) = ChangeFixture::parse(
            r#"
//- /opt/lib/comp-1.3/include/comp.hrl otp_app:/opt/lib/comp-1.3
-define(COMP,3).
//- /include/foo.hrl include_path:/include app:foo-app
-define(FOO,3).
//- /src/foo.erl
-module(foo).
-include("foo.hrl").
bar() -> ?FOO.
"#,
        );

        expect![[r#"
            Some(
                AppStructure {
                    app_map: {
                        SourceRootId(
                            0,
                        ): Some(
                            AppData {
                                project_id: ProjectId(
                                    0,
                                ),
                                name: AppName(
                                    "test-fixture",
                                ),
                                dir: AbsPathBuf(
                                    "/",
                                ),
                                include_path: [
                                    AbsPathBuf(
                                        "/src",
                                    ),
                                    AbsPathBuf(
                                        "/opt/lib",
                                    ),
                                ],
                                src_path: [
                                    AbsPathBuf(
                                        "/src",
                                    ),
                                ],
                                extra_src_dirs: [],
                                macros: [],
                                parse_transforms: [],
                                app_type: App,
                                ebin_path: None,
                            },
                        ),
                        SourceRootId(
                            2,
                        ): Some(
                            AppData {
                                project_id: ProjectId(
                                    1,
                                ),
                                name: AppName(
                                    "comp",
                                ),
                                dir: AbsPathBuf(
                                    "/opt/lib/comp-1.3",
                                ),
                                include_path: [
                                    AbsPathBuf(
                                        "/opt/lib/comp-1.3/include",
                                    ),
                                    AbsPathBuf(
                                        "/opt/lib/comp-1.3/src",
                                    ),
                                    AbsPathBuf(
                                        "/opt/lib",
                                    ),
                                ],
                                src_path: [
                                    AbsPathBuf(
                                        "/opt/lib/comp-1.3/src",
                                    ),
                                ],
                                extra_src_dirs: [],
                                macros: [],
                                parse_transforms: [],
                                app_type: Otp,
                                ebin_path: Some(
                                    AbsPathBuf(
                                        "/opt/lib/comp-1.3/ebin",
                                    ),
                                ),
                            },
                        ),
                        SourceRootId(
                            1,
                        ): Some(
                            AppData {
                                project_id: ProjectId(
                                    0,
                                ),
                                name: AppName(
                                    "foo-app",
                                ),
                                dir: AbsPathBuf(
                                    "/",
                                ),
                                include_path: [
                                    AbsPathBuf(
                                        "/include",
                                    ),
                                    AbsPathBuf(
                                        "/opt/lib",
                                    ),
                                ],
                                src_path: [],
                                extra_src_dirs: [],
                                macros: [],
                                parse_transforms: [],
                                app_type: App,
                                ebin_path: None,
                            },
                        ),
                        SourceRootId(
                            3,
                        ): None,
                    },
                    project_map: {
                        ProjectId(
                            0,
                        ): ProjectData {
                            source_roots: [
                                SourceRootId(
                                    0,
                                ),
                                SourceRootId(
                                    1,
                                ),
                            ],
                            root_dir: AbsPathBuf(
                                "/",
                            ),
                            deps_ebins: [],
                            build_info_path: None,
                            otp_project_id: Some(
                                ProjectId(
                                    1,
                                ),
                            ),
                            app_roots: AppRoots {
                                otp: Some(
                                    AppRoots {
                                        otp: None,
                                        app_map: {
                                            AppName(
                                                "comp",
                                            ): SourceRootId(
                                                2,
                                            ),
                                        },
                                    },
                                ),
                                app_map: {
                                    AppName(
                                        "test-fixture",
                                    ): SourceRootId(
                                        0,
                                    ),
                                    AppName(
                                        "foo-app",
                                    ): SourceRootId(
                                        1,
                                    ),
                                },
                            },
                            eqwalizer_config: EqwalizerConfig {
                                enable_all: true,
                                max_tasks: 4,
                            },
                        },
                        ProjectId(
                            1,
                        ): ProjectData {
                            source_roots: [
                                SourceRootId(
                                    2,
                                ),
                            ],
                            root_dir: AbsPathBuf(
                                "/opt/lib",
                            ),
                            deps_ebins: [],
                            build_info_path: None,
                            otp_project_id: Some(
                                ProjectId(
                                    1,
                                ),
                            ),
                            app_roots: AppRoots {
                                otp: None,
                                app_map: {
                                    AppName(
                                        "comp",
                                    ): SourceRootId(
                                        2,
                                    ),
                                },
                            },
                            eqwalizer_config: EqwalizerConfig {
                                enable_all: true,
                                max_tasks: 4,
                            },
                        },
                    },
                    catch_all_source_root: SourceRootId(
                        3,
                    ),
                },
            )"#]]
        .assert_eq(format!("{:#?}", change.app_structure).as_str());
    }

    #[test]
    fn unresolved_macro_diag_include_dir2() {
        // Test the scenario where the include file is referenced
        // relative to a project include directory
        let (_fixture, change) = ChangeFixture::parse(
            r#"
//- /extra/include/bar.hrl include_path:/extra/include
-define(BAR,4).
//- /include/foo.hrl include_path:/include
-define(FOO,3).
//- /src/foo.erl
-module(foo).
-include("foo.hrl").
-include("bar.hrl").
bar() -> ?FOO.
foo() -> ?BAR.
"#,
        );

        expect![[r#"
            Some(
                AppStructure {
                    app_map: {
                        SourceRootId(
                            0,
                        ): Some(
                            AppData {
                                project_id: ProjectId(
                                    0,
                                ),
                                name: AppName(
                                    "test-fixture",
                                ),
                                dir: AbsPathBuf(
                                    "/extra",
                                ),
                                include_path: [
                                    AbsPathBuf(
                                        "/",
                                    ),
                                    AbsPathBuf(
                                        "/extra/include",
                                    ),
                                    AbsPathBuf(
                                        "/include",
                                    ),
                                    AbsPathBuf(
                                        "/src",
                                    ),
                                    AbsPathBuf(
                                        "/",
                                    ),
                                ],
                                src_path: [
                                    AbsPathBuf(
                                        "/src",
                                    ),
                                ],
                                extra_src_dirs: [],
                                macros: [],
                                parse_transforms: [],
                                app_type: App,
                                ebin_path: None,
                            },
                        ),
                        SourceRootId(
                            1,
                        ): None,
                    },
                    project_map: {
                        ProjectId(
                            0,
                        ): ProjectData {
                            source_roots: [
                                SourceRootId(
                                    0,
                                ),
                            ],
                            root_dir: AbsPathBuf(
                                "/",
                            ),
                            deps_ebins: [],
                            build_info_path: None,
                            otp_project_id: Some(
                                ProjectId(
                                    1,
                                ),
                            ),
                            app_roots: AppRoots {
                                otp: None,
                                app_map: {
                                    AppName(
                                        "test-fixture",
                                    ): SourceRootId(
                                        0,
                                    ),
                                },
                            },
                            eqwalizer_config: EqwalizerConfig {
                                enable_all: true,
                                max_tasks: 4,
                            },
                        },
                        ProjectId(
                            1,
                        ): ProjectData {
                            source_roots: [],
                            root_dir: AbsPathBuf(
                                "/",
                            ),
                            deps_ebins: [],
                            build_info_path: None,
                            otp_project_id: Some(
                                ProjectId(
                                    1,
                                ),
                            ),
                            app_roots: AppRoots {
                                otp: None,
                                app_map: {},
                            },
                            eqwalizer_config: EqwalizerConfig {
                                enable_all: true,
                                max_tasks: 4,
                            },
                        },
                    },
                    catch_all_source_root: SourceRootId(
                        1,
                    ),
                },
            )"#]]
        .assert_eq(format!("{:#?}", change.app_structure).as_str());
    }

    #[test]
    fn test_remove_annotations() {
        let text = stdx::trim_indent(
            r#"
-module(my_module).
-export([meaning_of_life/0]).
meaning_of_life() ->
    Thoughts = thinking(),
 %% ^^^^^^^^ 💡 L1268: variable 'Thoughts' is unused
    42.
"#,
        );
        expect![[r#"
            -module(my_module).
            -export([meaning_of_life/0]).
            meaning_of_life() ->
                Thoughts = thinking(),
                42.
        "#]]
        .assert_eq(remove_annotations(None, &text).as_str());
    }

    #[test]
    fn extract_annotations_continuation_1() {
        let text = stdx::trim_indent(
            r#"
             fn main() {
                 zoo + 1
             } %%^^^ type:
               %%  | i32
                 "#,
        );
        let res = extract_annotations(&text)
            .into_iter()
            .map(|(range, ann)| (&text[range], range, ann))
            .collect::<Vec<_>>();

        expect![[r#"
            [
                (
                    "zoo",
                    16..19,
                    "type:\ni32",
                ),
            ]
        "#]]
        .assert_debug_eq(&res);
    }

    #[test]
    fn extract_annotations_continuation_2() {
        let text = stdx::trim_indent(
            r#"
            -module(main).

            foo(Node) ->
                erlang:spawn(Node, fun() -> ok end).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning:
            %%                                    |
            %%                                    | Production code blah
            %%                                    | more
            "#,
        );
        let res = extract_annotations(&text)
            .into_iter()
            .map(|(range, ann)| (&text[range], range, ann))
            .collect::<Vec<_>>();
        expect![[r#"
            [
                (
                    "erlang:spawn(Node, fun() -> ok end)",
                    33..68,
                    "warning:\n\nProduction code blah\nmore",
                ),
            ]
        "#]]
        .assert_debug_eq(&res);
    }

    #[test]
    fn extract_annotations_continuation_3() {
        let text = stdx::trim_indent(
            r#"
            -module(main).

            main() ->
                 zoo + 1.
               %%^^^ type:
               %%  | i32

            foo(Node) ->
                erlang:spawn(Node, fun() -> ok end).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning:
            %%                                    | Production code blah
            %%                                    | more
            "#,
        );
        let res = extract_annotations(&text)
            .into_iter()
            .map(|(range, ann)| (&text[range], range, ann))
            .collect::<Vec<_>>();
        expect![[r#"
            [
                (
                    "zoo",
                    31..34,
                    "type:\ni32",
                ),
                (
                    "erlang:spawn(Node, fun() -> ok end)",
                    86..121,
                    "warning:\nProduction code blah\nmore",
                ),
            ]
        "#]]
        .assert_debug_eq(&res);
    }

    #[test]
    fn extract_annotations_zero_offset() {
        let text = stdx::trim_indent(
            r#"
            -module(main).

            main() ->
            %%<^^^
                 zoo + 1.
            "#,
        );
        let res = extract_annotations(&text)
            .into_iter()
            .map(|(range, ann)| (&text[range], range, ann))
            .collect::<Vec<_>>();
        expect![[r#"
            [
                (
                    "main()",
                    16..22,
                    "",
                ),
            ]
        "#]]
        .assert_debug_eq(&res);
    }
}
