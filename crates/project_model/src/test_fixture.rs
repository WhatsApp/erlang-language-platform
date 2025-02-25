/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Based on rust-analyzer test_utils::fixture

//! Defines `Fixture` -- a convenient way to describe the initial state of
//! ELP database from a single string.
//!
//! Fixtures are strings containing Erlang source code with optional metadata.
//! A fixture without metadata is parsed into a single source file.
//! Use this to test functionality local to one file.
//!
//! Simple Example:
//! ```not_rust
//! r#"
//! main() ->
//!     ok.
//! "#
//! ```
//!
//! Metadata can be added to a fixture after a `//-` comment.
//! The basic form is specifying filenames,
//! which is also how to define multiple files in a single test fixture
//!
//! Example using two files in the same crate:
//! ```not_rust
//! "
//! //- /main.erl
//! -module(main).
//! main() ->
//!     foo:bar().
//!
//! //- /foo.erl
//! -module(foo).
//! bar() -> ok.
//! "
//! ```
//!
//! Certain diagnostics (e.g. Common Test) need to operate on the filesystem directly.
//! The default behaviour (`scratch_buffer:false`) uses the in-memory representation of the file system.
//! To dump a fixture to the filesystem, you can use the `scratch_buffer:true` option.
//! Since tests can run in parallel, ensure the name of the file is unique to prevent race conditions.
//!
//! ```not_rust
//! "
//! //- /src/my_SUITE.erl scratch_buffer:true
//! -module(my_SUITE).
//! "
//! ```
//!
//! //! Specify OTP, and an OTP app
//! ```not_rust
//! "
//! //- /test/opt/lib/comp-1.3/include/comp.hrl otp_app:/opt/lib/comp-1.3
//! -define(COMP,3).
//! "
//! ```
//!
//! Example setting up multi-app project, and OTP
//! ```not_rust
//! "
//! //- /opt/lib/comp-1.3/include/comp.hrl otp_app:/opt/lib/comp-1.3
//! -define(COMP,3).
//! //- /extra/include/bar.hrl include_path:/extra/include app:app_a
//! -define(BAR,4).
//! //- /include/foo.hrl include_path:/include app:app_a
//! -define(FOO,3).
//! //- /src/foo.erl app:app_b
//! -module(foo).
//! -include("foo.hrl").
//! -include("bar.hrl").
//! bar() -> ?FOO.
//! foo() -> ?BAR.
//! "
//! ```

use std::collections::BTreeMap;
use std::fs;
use std::fs::File;
use std::io::Write;

use lazy_static::lazy_static;
use paths::AbsPath;
use paths::AbsPathBuf;
use paths::Utf8Path;
use paths::Utf8PathBuf;
use regex::Regex;
pub use stdx::trim_indent;
use text_size::TextRange;
use text_size::TextSize;

use crate::otp::Otp;
use crate::temp_dir::TempDir;
use crate::AppName;
use crate::Project;
use crate::ProjectAppData;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Fixture {
    pub path: String,
    pub text: String,
    pub app_data: ProjectAppData,
    pub otp: Option<Otp>,
    pub tag: Option<String>,
    pub tags: Vec<(TextRange, Option<String>)>,
}

#[derive(Clone, Debug, Default)]
pub struct DiagnosticsEnabled {
    pub use_native: bool,
    pub use_erlang_service: bool,
    pub use_eqwalizer: bool,
    pub use_ct: bool,
    pub use_edoc: bool,
    /// Keep a copy of the project we loaded the fixture from, as it
    /// has a reference to the temporary directory holding build_info
    /// for Eqwalizer. Ditto for the TempDir we dump the test fixture
    /// to.  This is dropped when it goes out of scope, so we need to
    /// keep it around. This structure is used to manage the services
    /// that need it, so it is a good place for it to go.
    #[allow(unused)]
    pub tmp_dir: Option<(Vec<Project>, TempDir)>,
}

impl DiagnosticsEnabled {
    pub fn needs_fixture_on_disk(&self) -> bool {
        let DiagnosticsEnabled {
            use_native: _,
            use_erlang_service: _,
            use_eqwalizer: _,
            use_ct,
            use_edoc,
            tmp_dir: _,
        } = self;
        *use_ct || *use_edoc
    }

    #[track_caller]
    pub fn assert_ct_enabled(&self) {
        if !self.use_ct {
            panic!("Expecting `//- common_test` at top of fixture");
        }
    }

    #[track_caller]
    pub fn assert_erlang_service_enabled(&self) {
        if !self.use_erlang_service {
            panic!("Expecting `//- erlang_service` at top of fixture");
        }
    }

    #[track_caller]
    pub fn assert_edoc_enabled(&self) {
        if !self.use_edoc {
            panic!("Expecting `//- edoc` at top of fixture");
        }
    }

    /// If no other diagnostics are enabled, enable native.
    /// If any are explicitly enabled, then native must also be
    /// explicitly enabled.
    fn set_default_native(&mut self) {
        let DiagnosticsEnabled {
            use_native: _,
            use_erlang_service,
            use_eqwalizer,
            use_ct,
            use_edoc,
            tmp_dir: _,
        } = &self;
        if !(*use_erlang_service || *use_ct || *use_eqwalizer || *use_edoc) {
            self.use_native = true;
        }
    }
}

#[derive(Clone, Debug)]
pub struct FixtureWithProjectMeta {
    pub fixture: Vec<Fixture>,
    pub diagnostics_enabled: DiagnosticsEnabled,
}

impl FixtureWithProjectMeta {
    /// Parses text which looks like this:
    ///
    ///  ```not_rust
    ///  //- some meta
    ///  line 1
    ///  line 2
    ///  //- other meta
    ///  ```
    #[track_caller]
    pub fn parse(fixture: &str) -> FixtureWithProjectMeta {
        let fixture = trim_indent(fixture);
        let mut fixture = fixture.as_str();
        let mut res: Vec<Fixture> = Vec::new();
        let mut diagnostics_enabled = DiagnosticsEnabled::default();

        // ---------------------------------------
        // Each of the following is optional, but they must always
        // appear in the same (alphabetical) order
        if let Some(meta) = fixture.strip_prefix("//- common_test") {
            let (_meta, remain) = meta.split_once('\n').unwrap();
            diagnostics_enabled.use_ct = true;
            fixture = remain;
        }

        if let Some(meta) = fixture.strip_prefix("//- edoc") {
            let (_meta, remain) = meta.split_once('\n').unwrap();
            diagnostics_enabled.use_edoc = true;
            fixture = remain;
        }

        if let Some(meta) = fixture.strip_prefix("//- eqwalizer") {
            let (_meta, remain) = meta.split_once('\n').unwrap();
            diagnostics_enabled.use_eqwalizer = true;
            fixture = remain;
        }

        if let Some(meta) = fixture.strip_prefix("//- erlang_service") {
            let (_meta, remain) = meta.split_once('\n').unwrap();
            diagnostics_enabled.use_erlang_service = true;
            fixture = remain;
        }

        if let Some(meta) = fixture.strip_prefix("//- native") {
            let (_meta, remain) = meta.split_once('\n').unwrap();
            diagnostics_enabled.use_native = true;
            fixture = remain;
        }

        diagnostics_enabled.set_default_native();

        // End of optional top-level meta info
        // ---------------------------------------

        let default = if fixture.contains("//-") {
            None
        } else {
            Some("//- /main.erl")
        };

        for (ix, line) in default
            .into_iter()
            .chain(fixture.split_inclusive('\n'))
            .enumerate()
        {
            if line.contains("//-") {
                assert!(
                    line.starts_with("//-"),
                    "Metadata line {} has invalid indentation. \
                     All metadata lines need to have the same indentation.\n\
                     The offending line: {:?}",
                    ix,
                    line
                );
            }

            if line.starts_with("//-") {
                let meta = FixtureWithProjectMeta::parse_meta_line(line);
                res.push(meta)
            } else {
                if line.starts_with("// ")
                    && line.contains(':')
                    && !line.contains("::")
                    && line.chars().all(|it| !it.is_uppercase())
                {
                    panic!("looks like invalid metadata line: {:?}", line)
                }

                if let Some(entry) = res.last_mut() {
                    entry.text.push_str(line);
                }
            }
        }

        for fixture in &mut res {
            if let Some(tag) = &fixture.tag {
                let (tags, text) = extract_tags(&fixture.text, tag);
                fixture.tags = tags;
                fixture.text = text;
            }
        }

        FixtureWithProjectMeta {
            fixture: res,
            diagnostics_enabled,
        }
    }

    /// Create an on-disk image of a test fixture in a temporary directory
    pub fn gen_project(spec: &str) -> TempDir {
        let fixtures = FixtureWithProjectMeta::parse(spec);
        FixtureWithProjectMeta::gen_project_from_fixture(&fixtures)
    }

    /// Create an on-disk image of a test fixture in a temporary directory
    pub fn gen_project_from_fixture(fixtures: &FixtureWithProjectMeta) -> TempDir {
        let tmp_dir = TempDir::new();
        for fixture in &fixtures.fixture {
            let path = tmp_dir.path().join(&fixture.path[1..]);
            let parent = path.parent().unwrap();
            fs::create_dir_all(parent).unwrap();
            let mut tmp_file = File::create(path).unwrap();
            write!(tmp_file, "{}", &fixture.text).unwrap();
        }
        tmp_dir
    }

    //- /module.erl app:foo
    //- /opt/lib/comp-1.3/include/comp.hrl otp_app:/opt/lib/comp-1.3
    //- /my_app/test/file_SUITE.erl extra:test
    fn parse_meta_line(meta: &str) -> Fixture {
        assert!(meta.starts_with("//-"));
        let meta = meta["//-".len()..].trim();
        let components = meta.split_ascii_whitespace().collect::<Vec<_>>();

        let path = components[0].to_string();
        assert!(
            path.starts_with('/'),
            "fixture path does not start with `/`: {:?}",
            path
        );

        let mut app_name = None;
        let mut include_dirs = Vec::new();
        let mut extra_dirs = Vec::new();
        let mut otp = None;
        let mut tag = None;

        for component in components[1..].iter() {
            let (key, value) = component
                .split_once(':')
                .unwrap_or_else(|| panic!("invalid meta line: {:?}", meta));
            match key {
                "app" => app_name = Some(AppName(value.to_string())),
                "include_path" => include_dirs
                    .push(AbsPath::assert(&Utf8PathBuf::from(value.to_string())).normalize()),
                "otp_app" => {
                    // We have an app directory, the OTP lib dir is its parent
                    let path = AbsPathBuf::assert(Utf8PathBuf::from(value.to_string()));
                    let lib_dir = path.parent().unwrap().normalize();
                    let versioned_name = path.file_name().unwrap();
                    let app = ProjectAppData::otp_app_data(&versioned_name, &path);

                    otp = Some((Otp { lib_dir }, app));
                }
                "extra" => {
                    // We have an extra directory, such as for a test suite
                    // It needs to be relative to the app dir.
                    let dir = value.to_string();
                    extra_dirs.push(dir);
                }
                "tag" => {
                    tag = Some(value.to_string());
                }
                _ => panic!("bad component: {:?}", component),
            }
        }

        let (otp, app_data) = if let Some((otp, app)) = otp {
            (Some(otp), app)
        } else {
            // Try inferring dir - parent once to get to ./src, parent twice to get to app root
            let dir = AbsPath::assert(Utf8Path::new(&path)).parent().unwrap();
            let dir = dir.parent().unwrap_or(dir).normalize();
            let app_name = app_name.unwrap_or(AppName("test-fixture".to_string()));
            let abs_path = AbsPathBuf::assert(Utf8PathBuf::from(path.clone()));
            let mut src_dirs = vec![];
            if let Some(ext) = abs_path.extension() {
                if ext == "erl" {
                    if let Some(parent) = abs_path.parent() {
                        let path = parent.to_path_buf();
                        src_dirs.push(path)
                    }
                }
            }
            (
                None,
                ProjectAppData::fixture_app_data(app_name, dir, include_dirs, src_dirs, extra_dirs),
            )
        };

        Fixture {
            path,
            text: String::new(),
            app_data,
            otp,
            tag,
            tags: Vec::new(),
        }
    }
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
/// ```not_rust
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
        static ref RE: Regex = Regex::new(r"^\s*%%( +\^+|[<\^]+| <<<| +\|) +💡?.*$").unwrap();
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

#[cfg(test)]
mod tests {

    use expect_test::expect;
    use paths::AbsPath;
    use paths::Utf8PathBuf;

    use super::FixtureWithProjectMeta;
    use crate::test_fixture::contains_annotation;
    use crate::test_fixture::extract_annotations;
    use crate::test_fixture::remove_annotations;

    #[test]
    #[should_panic]
    fn parse_fixture_checks_further_indented_metadata() {
        FixtureWithProjectMeta::parse(
            r"
        //- /lib.rs
          mod bar;

          fn foo() {}
          //- /bar.rs
          pub fn baz() {}
          ",
        );
    }

    #[test]
    fn parse_fixture_multiple_files() {
        let fixture = FixtureWithProjectMeta::parse(
            r#"
//- /foo.erl
-module(foo).
foo() -> ok.
//- /bar.erl
-module(bar).
bar() -> ok.
"#,
        );
        assert_eq!(fixture.diagnostics_enabled.use_ct, false);
        assert_eq!(fixture.diagnostics_enabled.use_erlang_service, false);
        let parsed = fixture.fixture;
        assert_eq!(2, parsed.len());

        let meta0 = &parsed[0];
        assert_eq!("-module(foo).\nfoo() -> ok.\n", meta0.text);

        let meta1 = &parsed[1];
        assert_eq!("-module(bar).\nbar() -> ok.\n", meta1.text);

        assert_eq!("/foo.erl", meta0.path);

        assert_eq!("/bar.erl", meta1.path);
    }

    #[test]
    fn parse_fixture_erlang_service() {
        let fixture = FixtureWithProjectMeta::parse(
            r#"
//- erlang_service
//- /foo.erl
-module(foo).
foo() -> ok.
//- /bar.erl
-module(bar).
bar() -> ok.
"#,
        );
        assert_eq!(fixture.diagnostics_enabled.use_ct, false);
        assert_eq!(fixture.diagnostics_enabled.use_erlang_service, true);
        let parsed = fixture.fixture;
        assert_eq!(2, parsed.len());

        let meta0 = &parsed[0];
        assert_eq!("-module(foo).\nfoo() -> ok.\n", meta0.text);

        let meta1 = &parsed[1];
        assert_eq!("-module(bar).\nbar() -> ok.\n", meta1.text);

        assert_eq!("/foo.erl", meta0.path);

        assert_eq!("/bar.erl", meta1.path);
    }

    #[test]
    fn parse_fixture_common_test() {
        let fixture = FixtureWithProjectMeta::parse(
            r#"
//- common_test
//- /foo.erl
-module(foo).
foo() -> ok.
"#,
        );
        assert_eq!(fixture.diagnostics_enabled.use_ct, true);
        assert_eq!(fixture.diagnostics_enabled.use_erlang_service, false);
    }

    #[test]
    fn parse_fixture_gets_app_data() {
        let fixture = FixtureWithProjectMeta::parse(
            r#"
//- /include/foo.hrl include_path:/include
-define(FOO,3).
//- /src/foo.erl
-module(foo).
foo() -> ok.
//- /src/bar.erl
-module(bar).
bar() -> ok.
"#,
        );
        let parsed = fixture.fixture;
        assert_eq!(3, parsed.len());

        let app_data = &parsed[0].app_data;
        assert_eq!(
            vec![AbsPath::assert(&Utf8PathBuf::from("/include")).normalize()],
            app_data.include_dirs
        );
        let meta0 = &parsed[0];
        assert_eq!("-define(FOO,3).\n", meta0.text);

        let meta1 = &parsed[1];
        assert_eq!("-module(foo).\nfoo() -> ok.\n", meta1.text);

        let meta2 = &parsed[2];
        assert_eq!("-module(bar).\nbar() -> ok.\n", meta2.text);

        assert_eq!("/include/foo.hrl", meta0.path);

        assert_eq!("/src/foo.erl", meta1.path);

        assert_eq!("/src/bar.erl", meta2.path);

        expect![[r#"
            ProjectAppData {
                name: AppName(
                    "test-fixture",
                ),
                dir: AbsPathBuf(
                    "/",
                ),
                ebin: None,
                extra_src_dirs: [],
                include_dirs: [
                    AbsPathBuf(
                        "/include",
                    ),
                ],
                abs_src_dirs: [],
                macros: [],
                parse_transforms: [],
                app_type: App,
                include_path: [],
                applicable_files: None,
                is_test_target: None,
            }"#]]
        .assert_eq(format!("{:#?}", meta0.app_data).as_str());
    }

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

    #[test]
    fn test_contains_annotation() {
        assert!(contains_annotation("  %% ^^ blah"));
        assert!(contains_annotation(
            "  %%                | Rather use 'orelse'."
        ));
        assert!(contains_annotation("%%  ^^ 💡 warning: blah"));
        assert!(!contains_annotation("%%  an ordinary comment"));
        assert!(contains_annotation(
            "%% <<< 💡 error: Top of file diagnostic"
        ));
    }
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
