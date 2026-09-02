/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
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
use std::sync::LazyLock;

use paths::AbsPath;
use paths::AbsPathBuf;
use paths::Utf8Path;
use paths::Utf8PathBuf;
use regex::Regex;
pub use stdx::trim_indent;
use text_size::TextRange;
use text_size::TextSize;

use crate::AppName;
use crate::ProjectAppData;
use crate::otp::Otp;
use crate::temp_dir::TempDir;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Fixture {
    pub path: String,
    pub text: String,
    pub app_data: ProjectAppData,
    pub otp: Option<Otp>,
    pub tag: Option<String>,
    pub tags: Vec<(TextRange, Option<String>)>,
    pub annotations: Vec<(TextRange, String)>,
    pub marker_pos: Option<RangeOrOffset>,
    /// Macro names defined for this file (e.g., from `macros:[A,B,C]` metadata).
    /// These are passed to the preprocessor as external defines.
    pub macros: Vec<String>,
    /// App names that this file's app depends on (for buck-style dependency checking).
    pub deps: Vec<String>,
    /// When set, places this file into the named app's SourceRoot/FileSet
    /// instead of its own app's. This models the buck scenario where
    /// lib + test targets share the same directory (== source root).
    /// The file's `app:` annotation still controls its AppDataId via
    /// `applicable_files`.
    pub src_app: Option<String>,
}

#[derive(Clone, Debug, Default)]
pub struct DiagnosticsEnabled {
    pub use_native: bool,
    pub use_erlang_service: bool,
    pub use_eqwalizer: bool,
}

impl DiagnosticsEnabled {
    #[track_caller]
    pub fn assert_erlang_service_enabled(&self) {
        if !self.use_erlang_service {
            panic!("Expecting `//- erlang_service` at top of fixture");
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
        } = &self;
        if !(*use_erlang_service || *use_eqwalizer) {
            self.use_native = true;
        }
    }
}

#[derive(Clone, Debug)]
pub struct FixtureWithProjectMeta {
    pub fixture: Vec<Fixture>,
    pub diagnostics_enabled: DiagnosticsEnabled,
    pub expect_parse_errors: bool,
    /// OTP app names to load from the real OTP installation (e.g., "stdlib").
    pub otp_apps: Vec<String>,
    /// Extra dynamic call pattern strings (e.g., `"my_rpc:call(_, Module, Function, Args)"`).
    pub dynamic_call_patterns: Vec<String>,
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
        let mut expect_parse_errors = false;

        // ---------------------------------------
        // Each of the following is optional, but they must always
        // appear in the same (alphabetical) order
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

        if let Some(meta) = fixture.strip_prefix("//- expect_parse_errors") {
            let (_meta, remain) = meta.split_once('\n').unwrap();
            expect_parse_errors = true;
            fixture = remain;
        }

        if let Some(meta) = fixture.strip_prefix("//- native") {
            let (_meta, remain) = meta.split_once('\n').unwrap();
            diagnostics_enabled.use_native = true;
            fixture = remain;
        }

        let mut otp_apps = Vec::new();
        let mut dynamic_call_patterns = Vec::new();
        if let Some(meta) = fixture.strip_prefix("//- otp_apps:") {
            let (apps_str, remain) = meta.split_once('\n').unwrap();
            for app in apps_str.split(',') {
                let app = app.trim();
                if !app.is_empty() {
                    otp_apps.push(app.to_string());
                }
            }
            fixture = remain;
        }
        while let Some(meta) = fixture.strip_prefix("//- dynamic_call:") {
            let (pattern, remain) = meta.split_once('\n').unwrap();
            let pat = pattern.trim();
            if !pat.is_empty() {
                dynamic_call_patterns.push(pat.to_string());
            }
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
                    "Metadata line {ix} has invalid indentation. \
                     All metadata lines need to have the same indentation.\n\
                     The offending line: {line:?}"
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
                    panic!("looks like invalid metadata line: {line:?}")
                }

                if let Some(entry) = res.last_mut() {
                    entry.text.push_str(line);
                } else if line.chars().any(|c| !c.is_whitespace()) {
                    panic!(
                        "Fixture has content before the first file marker (`//- /path/to/file.erl`). \
                         Did you forget to add a file marker at the beginning?\n\
                         The offending line: {line:?}"
                    );
                }
            }
        }

        for fixture in &mut res {
            if let Some(tag) = &fixture.tag {
                let (tags, text) = extract_tags(&fixture.text, tag);
                fixture.tags = tags;
                fixture.text = text;
            }
            let (mut annotations, text_without_annotations) = extract_annotations(&fixture.text);
            let (text, marker_pos) = get_text_and_pos(&text_without_annotations);
            annotations.sort_by_key(|(range, _content)| range.start());
            fixture.annotations = annotations;
            fixture.text = text;
            fixture.marker_pos = marker_pos;
        }

        FixtureWithProjectMeta {
            fixture: res,
            diagnostics_enabled,
            expect_parse_errors,
            otp_apps,
            dynamic_call_patterns,
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
            write!(tmp_file, "{}", fixture.text).unwrap();
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
            "fixture path does not start with `/`: {path:?}"
        );

        let mut app_name = None;
        let mut include_dirs = Vec::new();
        let mut extra_dirs = Vec::new();
        let mut otp = None;
        let mut tag = None;
        let mut macros = Vec::new();
        let mut buck_target = None;
        let mut deps = Vec::new();
        let mut src_app = None;

        for component in components[1..].iter() {
            let (key, value) = component
                .split_once(':')
                .unwrap_or_else(|| panic!("invalid meta line: {meta:?}"));
            match key {
                "app" => app_name = Some(AppName(value.to_string())),
                "include_path" => include_dirs
                    .push(AbsPath::assert(&Utf8PathBuf::from(value.to_string())).normalize()),
                "otp_app" => {
                    // We have an app directory, the OTP lib dir is its parent
                    let path = AbsPathBuf::assert(Utf8PathBuf::from(value.to_string()));
                    let lib_dir = path.parent().unwrap().normalize();
                    let versioned_name = path.file_name().unwrap();
                    let app = ProjectAppData::otp_app_data(versioned_name, &path);

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
                "macros" => {
                    // Parse format like "[A,B,C]" or "[]"
                    let value = value.trim_start_matches('[').trim_end_matches(']');
                    for macro_name in value.split(',') {
                        let name = macro_name.trim();
                        if !name.is_empty() {
                            macros.push(name.to_string());
                        }
                    }
                }
                "buck_target" => {
                    buck_target = Some(value.to_string());
                }
                "deps" => {
                    for dep in value.split(',') {
                        let dep = dep.trim();
                        if !dep.is_empty() {
                            deps.push(dep.to_string());
                        }
                    }
                }
                "src_app" => {
                    src_app = Some(value.to_string());
                }
                _ => panic!("bad component: {component:?}"),
            }
        }

        let (otp, mut app_data) = if let Some((otp, app)) = otp {
            (Some(otp), app)
        } else {
            // Try inferring dir - parent once to get to ./src, parent twice to get to app root
            let dir = AbsPath::assert(Utf8Path::new(&path)).parent().unwrap();
            let dir = dir.parent().unwrap_or(dir).normalize();
            let app_name = app_name.unwrap_or(AppName("test-fixture".to_string()));
            let abs_path = AbsPathBuf::assert(Utf8PathBuf::from(path.clone()));
            let mut src_dirs = vec![];
            if let Some(ext) = abs_path.extension()
                && ext == "erl"
                && let Some(parent) = abs_path.parent()
            {
                let path = parent.to_path_buf();
                src_dirs.push(path)
            }
            (
                None,
                ProjectAppData::fixture_app_data(app_name, dir, include_dirs, src_dirs, extra_dirs),
            )
        };

        if let Some(target) = buck_target {
            app_data.buck_target_name = Some(target);
        }

        Fixture {
            path,
            text: String::new(),
            app_data,
            otp,
            tag,
            tags: Vec::new(),
            annotations: Vec::new(),
            marker_pos: None,
            macros,
            deps,
            src_app,
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
    assert!(stack.is_empty(), "unmatched <{tag}>");
    ranges.sort_by_key(|r| (r.0.start(), r.0.end()));
    (ranges, res)
}

// ---------------------------------------------------------------------

/// Extracts `%% ^^^ some text` annotations.
///
/// A run of `^^^` can be arbitrary long and points to the corresponding range
/// in the line above.
///
/// The `%% <<< text` syntax (at the beginning of the file) creates an annotation
/// at position 0..0, useful for file-level diagnostics.
///
/// The `%% ^^^file text` syntax (with the keyword `file` after the carets) can be
/// used to attach `text` to the entirety of the file contents.
///
/// The `%%<^^^ text` syntax can be used to attach `text` to the span
/// starting at the `%%` position (left margin), rather than at the first `^`.
///
/// Multiline string values are supported using continuation lines:
///
/// ```not_rust
/// %% ^^^ first line
/// %%   | second line
/// ```
///
/// This is useful for representing diagnostics with related information:
///
/// ```not_rust
/// foo() -> syntax error oops.
/// %%              ^^^^^ error: P1711: syntax error before: error
/// %%                  | Related info: 0:25-30 function foo/0 undefined
/// ```
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
pub fn extract_annotations(text: &str) -> (Vec<(TextRange, String)>, String) {
    let text_without_annotations = remove_annotations(None, text);
    let mut res = Vec::new();
    // map from line length to beginning of last line that had that length
    let mut line_start_map = BTreeMap::new();
    let mut line_start: TextSize = 0.into();
    let mut prev_line_annotations: Vec<(TextSize, usize)> = Vec::new();
    for (idx, line) in text.split_inclusive('\n').enumerate() {
        if idx == 0 {
            // First line, look for header marker
            if line.starts_with(TOP_OF_FILE_MARKER) {
                if let Some(anno) = line.strip_prefix(TOP_OF_FILE_MARKER) {
                    res.push((*TOP_OF_FILE_RANGE, anno.trim_end().to_string()));
                }
            } else if line.starts_with(&(CURSOR_MARKER.to_string() + TOP_OF_FILE_MARKER))
                && let Some(anno) =
                    line.strip_prefix(&(CURSOR_MARKER.to_string() + TOP_OF_FILE_MARKER))
            {
                res.push((*TOP_OF_FILE_RANGE, anno.trim_end().to_string()));
            }
        } else if line.contains(TOP_OF_FILE_MARKER) {
            panic!(
                "Annotation line {idx} is invalid here. \
                     The top of file marker '{TOP_OF_FILE_MARKER}' can only appear first in the file on the left margin.\n\
                     The offending line: {line:?}"
            );
        }
        assert!(
            !is_misprefixed_annotation(line),
            "Annotation line {idx} must start with exactly `%%`.\n\
             This one is read as an annotation, because the prefix is split on the \
             first `%%`, but it is not recognised as one to strip, so it stays in \
             the file under test and every annotation below it is reported \
             shifted by its length.\n\
             The offending line: {line:?}"
        );
        let mut this_line_annotations = Vec::new();
        let line_length = if let Some((prefix, suffix)) = line.split_once("%%") {
            let ss_len = TextSize::of("%%");
            let annotation_offset = TextSize::of(prefix) + ss_len;
            let annotations = extract_line_annotations(suffix.trim_end_matches('\n'));
            if annotations.is_empty() {
                TextSize::of(line)
            } else {
                for annotation in annotations {
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
                                TextRange::up_to(TextSize::of(&text_without_annotations))
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
                            if !res.is_empty() {
                                offset += annotation_offset;
                                this_line_annotations.push((offset, res.len() - 1));
                                // Try to find a previous annotation at the same offset
                                let idx = if let Some(&(_, idx)) = prev_line_annotations
                                    .iter()
                                    .find(|&&(off, _idx)| off == offset)
                                {
                                    idx
                                } else {
                                    // If no exact offset match, append to the most recent annotation
                                    res.len() - 1
                                };
                                res[idx].1.push('\n');
                                res[idx].1.push_str(&content);
                            }
                        }
                    }
                }
                TextSize::from(0)
            }
        } else if line.contains(CURSOR_MARKER) {
            if line.contains(ESCAPED_CURSOR_MARKER) {
                TextSize::of(line) - TextSize::of(ESCAPED_CURSOR_MARKER)
            } else {
                TextSize::of(line) - TextSize::of(CURSOR_MARKER)
            }
        } else {
            TextSize::of(line)
        };

        line_start_map = line_start_map.split_off(&line_length);
        line_start_map.insert(line_length, line_start);

        line_start += line_length;

        if !this_line_annotations.is_empty() {
            prev_line_annotations = this_line_annotations;
        }
    }

    (res, text_without_annotations)
}

// ---------------------------------------------------------------------

/// Renders `annotations` back into `text` as `%% ^^^ ...` comment lines,
/// inverting [`extract_annotations`].
///
/// `text` is a fixture body whose annotation lines have already been stripped,
/// still carrying the cursor marker. The ranges in `annotations` are the ones
/// [`extract_annotations`] produces for that body.
///
/// Returns `None` when an annotation cannot be expressed in the caret syntax,
/// for example a range spanning several lines, or one starting in the two
/// columns the leading `%%` occupies. Callers should leave the fixture alone in
/// that case rather than write a lossy approximation. Note that rendering is
/// only ever *proposed*: callers are expected to feed the result back through
/// [`extract_annotations`] and confirm it round-trips before using it.
pub fn render_annotations(text: &str, annotations: &[(TextRange, String)]) -> Option<String> {
    let lines = fixture_lines(text);
    let whole_file_range = TextRange::up_to(TextSize::of(&remove_annotations(None, text)));

    let mut top_of_file = None;
    let mut by_line: Vec<Vec<String>> = vec![Vec::new(); lines.len()];

    for (range, content) in annotations {
        if range.is_empty() && range.start() == TextSize::new(0) {
            if top_of_file.is_some() {
                return None;
            }
            top_of_file = Some(content);
            continue;
        }

        let (idx, rendered) = if *range == whole_file_range && !lines.is_empty() {
            (0, render_whole_file_annotation(content))
        } else {
            let idx = lines.iter().position(|line| {
                line.start <= range.start() && range.start() < line.start + line.len
            })?;
            (
                idx,
                render_caret_annotation(*range, lines[idx].start, content)?,
            )
        };
        by_line[idx].push(rendered);
    }

    let mut res = String::new();
    if let Some(content) = top_of_file {
        // Unlike an annotation line, `%% <<< text` counts towards the line
        // offsets `extract_annotations` accumulates, so inserting one would
        // shift every other annotation in the file.
        if annotations.len() > 1 {
            return None;
        }
        let mut content_lines = content.split('\n');
        if let Some(first) = content_lines.next() {
            res.push_str(TOP_OF_FILE_MARKER);
            res.push_str(first);
            res.push('\n');
        }
        for line in content_lines {
            res.push_str("%%    | ");
            res.push_str(line);
            res.push('\n');
        }
    }
    for (idx, line) in lines.iter().enumerate() {
        res.push_str(line.text);
        if by_line[idx].is_empty() {
            continue;
        }
        if !line.text.ends_with('\n') {
            res.push('\n');
        }
        for annotation in &by_line[idx] {
            res.push_str(annotation);
            res.push('\n');
        }
    }
    // Annotations are whole lines, so the result must still end the way `text`
    // did. Getting this wrong runs the closing `"#` onto an annotation line.
    if !text.ends_with('\n') && res.ends_with('\n') {
        res.pop();
    }
    Some(res)
}

/// A `%%  ^^^^ text` annotation, plus a `%%     | text` line per extra line of
/// `content`.
fn render_caret_annotation(
    range: TextRange,
    line_start: TextSize,
    content: &str,
) -> Option<String> {
    let prefix = "%%";
    let column = u32::from(range.start().checked_sub(line_start)?) as usize;
    let mut res = String::new();

    // Column just past the caret run, which is the offset the extractor
    // records for this annotation.
    let caret_end = if column >= prefix.len() {
        let carets = usize::max(u32::from(range.len()) as usize, 1);
        res.push_str(prefix);
        res.push_str(&" ".repeat(column - prefix.len()));
        res.push_str(&"^".repeat(carets));
        column + carets
    } else {
        // No room for `%%` ahead of the carets, so anchor at the left margin
        // with the `%%<^^^` form. The extractor reads that as starting at the
        // line, and spanning the `<`, the carets and the `%%` itself, so the
        // run is three shorter than the range.
        if column != 0 {
            return None;
        }
        let carets = (u32::from(range.len()) as usize).checked_sub("%%<".len())?;
        // `%%<` alone has no caret for the extractor to find, so it would read
        // back as an ordinary comment and stay in the file under test.
        if carets == 0 {
            return None;
        }
        res.push_str("%%<");
        res.push_str(&"^".repeat(carets));
        carets + "%%<".len()
    };

    let mut content_lines = content.split('\n');
    if let Some(first) = content_lines.next() {
        res.push(' ');
        res.push_str(first);
    }
    // A continuation binds to the annotation whose end offset is one past the
    // `|`, so line the marker up just inside the end of the caret run.
    let continuation_column = usize::max(caret_end.saturating_sub(1), prefix.len());
    for line in content_lines {
        res.push('\n');
        res.push_str(prefix);
        res.push_str(&" ".repeat(continuation_column - prefix.len()));
        res.push_str("| ");
        res.push_str(line);
    }
    Some(res)
}

/// The `%%  ^^^file text` form, which attaches `text` to the whole file.
fn render_whole_file_annotation(content: &str) -> String {
    format!("%%  ^^^file {}", content.replace('\n', " "))
}

struct FixtureLine<'a> {
    text: &'a str,
    /// Start offset of the line, accumulated exactly as `extract_annotations`
    /// does: annotation lines count as zero-length and the cursor marker does
    /// not count towards a line's length.
    start: TextSize,
    len: TextSize,
}

fn fixture_lines(text: &str) -> Vec<FixtureLine<'_>> {
    let mut res = Vec::new();
    let mut start: TextSize = 0.into();
    for line in text.split_inclusive('\n') {
        let len = if let Some((_prefix, suffix)) = line.split_once("%%") {
            if extract_line_annotations(suffix.trim_end_matches('\n')).is_empty() {
                TextSize::of(line)
            } else {
                TextSize::from(0)
            }
        } else if line.contains(CURSOR_MARKER) {
            if line.contains(ESCAPED_CURSOR_MARKER) {
                TextSize::of(line) - TextSize::of(ESCAPED_CURSOR_MARKER)
            } else {
                TextSize::of(line) - TextSize::of(CURSOR_MARKER)
            }
        } else {
            TextSize::of(line)
        };
        res.push(FixtureLine {
            text: line,
            start,
            len,
        });
        start += len;
    }
    res
}

// ---------------------------------------------------------------------

/// Whether the harness should rewrite fixtures in place rather than fail.
///
/// Matches `expect-test`: the variable counts as set the moment it exists,
/// even when empty.
pub fn update_fixtures_requested() -> bool {
    std::env::var_os("UPDATE_EXPECT").is_some()
}

/// Rewrites the fixture literal containing `original` at `caller`'s call site
/// so that it reads `new_fixture`, preserving the literal's indentation and its
/// leading and trailing whitespace.
///
/// The literal is located by matching its contents against `original`, not by
/// taking the first literal at or after `caller`, so it does not matter which
/// argument position the fixture occupies, nor whether earlier arguments happen
/// to contain string literals of their own.
///
/// Returns `false` when the literal cannot be found, leaving the file alone.
pub fn patch_fixture_literal(
    caller: &std::panic::Location<'_>,
    original: &str,
    new_fixture: &str,
) -> std::io::Result<bool> {
    let path = workspace_path(caller.file());
    // Each test runs in its own process, so the fixtures of one source file are
    // rewritten concurrently. Reading under an exclusive lock means every
    // rewrite starts from the previous one's result; otherwise two writers can
    // both start from the same text and the second silently drops the first.
    let lock = File::open(&path)?;
    lock.lock()?;
    let source = fs::read_to_string(&path)?;
    let Some((body_start, body_end)) =
        find_fixture_literal(&source, caller.line() as usize, original)
    else {
        return Ok(false);
    };

    let body = &source[body_start..body_end];
    let indent = common_indent(body);
    // Fixtures conventionally open with a newline before the first line of
    // content, and close with the indentation of the terminating `"#`.
    let lead_end = body.find('\n').map_or(0, |idx| idx + 1);
    let trail_start = body.rfind('\n').map_or(body.len(), |idx| idx + 1);

    let mut lines: Vec<&str> = new_fixture.split('\n').collect();
    // `trim_indent` keeps the closing run only when it is longer than the
    // indentation it strips, so the rendered text may or may not still carry
    // it. Fall back to the run the fixture had.
    let last = lines.pop().unwrap_or_default();

    let mut patched = String::new();
    patched.push_str(&source[..body_start]);
    patched.push_str(&body[..lead_end]);
    for line in lines {
        if !line.is_empty() {
            patched.push_str(&indent);
        }
        patched.push_str(line);
        patched.push('\n');
    }
    if last.is_empty() {
        patched.push_str(&body[trail_start..]);
    } else {
        patched.push_str(&indent);
        patched.push_str(last);
    }
    patched.push_str(&source[body_end..]);

    fs::write(&path, patched)?;
    Ok(true)
}

/// Byte range of the body of the first raw string literal at or after `line`
/// whose contents match `original` once indentation is stripped.
fn find_fixture_literal(source: &str, line: usize, original: &str) -> Option<(usize, usize)> {
    let want = trim_indent(original);
    let bytes = source.as_bytes();
    let mut idx = line_offset(source, line);

    while idx < bytes.len() {
        if bytes[idx] == b'r' && (idx == 0 || !is_ident_byte(bytes[idx - 1])) {
            let mut cursor = idx + 1;
            while cursor < bytes.len() && bytes[cursor] == b'#' {
                cursor += 1;
            }
            if cursor < bytes.len() && bytes[cursor] == b'"' {
                let hashes = cursor - (idx + 1);
                let body_start = cursor + 1;
                let close = format!("\"{}", "#".repeat(hashes));
                if let Some(rel_end) = source[body_start..].find(&close) {
                    let body_end = body_start + rel_end;
                    if trim_indent(&source[body_start..body_end]) == want {
                        return Some((body_start, body_end));
                    }
                    idx = body_end + close.len();
                    continue;
                }
            }
        }
        idx += 1;
    }
    None
}

fn is_ident_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}

/// Byte offset of the start of 1-based `line`.
fn line_offset(source: &str, line: usize) -> usize {
    source
        .split_inclusive('\n')
        .take(line.saturating_sub(1))
        .map(str::len)
        .sum()
}

/// The indentation `trim_indent` would strip from `text`.
fn common_indent(text: &str) -> String {
    let width = text
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| line.len() - line.trim_start().len())
        .min()
        .unwrap_or(0);
    " ".repeat(width)
}

fn workspace_path(file: &str) -> Utf8PathBuf {
    let path = Utf8Path::new(file);
    if path.is_absolute() {
        return path.to_path_buf();
    }
    // Mirrors `expect-test`. Buck sets `CARGO_WORKSPACE_DIR` to the empty
    // string, leaving the path relative to the test's working directory.
    match std::env::var("CARGO_WORKSPACE_DIR") {
        Ok(root) => Utf8Path::new(&root).join(path),
        Err(_) => match std::env::var("CARGO_MANIFEST_DIR") {
            Ok(manifest) => Utf8Path::new(&manifest).join(path),
            Err(_) => path.to_path_buf(),
        },
    }
}

static TOP_OF_FILE_RANGE: LazyLock<TextRange> =
    LazyLock::new(|| TextRange::new(0.into(), 0.into()));

const TOP_OF_FILE_MARKER: &str = "%% <<< ";

/// Return a copy of the input text, with all `%% ^^^ 💡 some text` annotations removed
pub fn remove_annotations(marker: Option<&str>, text: &str) -> String {
    let mut lines = Vec::new();
    let mut prepend_cursor = false;
    for (idx, line) in text.split('\n').enumerate() {
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
        } else if (idx == 0) && line.starts_with(CURSOR_MARKER) {
            prepend_cursor = true;
        }
    }
    if prepend_cursor {
        CURSOR_MARKER.to_string() + &lines.join("\n")
    } else {
        lines.join("\n")
    }
}

/// Check if the given line would be an annotation but for carrying more than
/// two leading `%`.
///
/// [`extract_annotations`] splits on the first `%%` and so reads such a line as
/// an annotation, while [`contains_annotation`] does not match it and leaves it
/// in the file. Rejecting it keeps the two in agreement.
fn is_misprefixed_annotation(line: &str) -> bool {
    static RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^\s*~?%%%+( +\^+|[<\^]+| <<<| +\|)").unwrap());

    RE.is_match(line)
}

/// Check if the given line contains a `%% ^^^ 💡 some text` annotation
pub fn contains_annotation(line: &str) -> bool {
    static RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^\s*~?%%( +\^+|[<\^]+| <<<| +\|)\s?💡?.*$").unwrap());

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
        len += len_prefix;
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
        None => panic!("text should contain marker '{marker}'"),
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

pub fn get_text_and_pos(entry_text: &str) -> (String, Option<RangeOrOffset>) {
    if entry_text.contains(CURSOR_MARKER) {
        if entry_text.contains(ESCAPED_CURSOR_MARKER) {
            (
                entry_text.replace(ESCAPED_CURSOR_MARKER, CURSOR_MARKER),
                None,
            )
        } else {
            let (range_or_offset, text) = extract_range_or_offset(entry_text);
            (text, Some(range_or_offset))
        }
    } else {
        (entry_text.to_string(), None)
    }
}

#[cfg(test)]
mod tests {

    use expect_test::expect;
    use paths::AbsPath;
    use paths::Utf8PathBuf;

    use super::FixtureWithProjectMeta;
    use crate::test_fixture::contains_annotation;
    use crate::test_fixture::extract_annotations;
    use crate::test_fixture::extract_tags;
    use crate::test_fixture::remove_annotations;
    use crate::test_fixture::render_annotations;

    /// Render the annotations `text` declares back into the stripped fixture
    /// body, and check the result reports the very same annotations over the
    /// very same text. Preserving the text matters as much as the annotations:
    /// a render that drops a byte changes the file the test runs against.
    fn check_render_round_trip(text: &str) -> String {
        let text = stdx::trim_indent(text);
        let (annotations, stripped) = extract_annotations(&text);
        let rendered = render_annotations(&stripped, &annotations)
            .expect("fixture should be expressible in the caret syntax");
        let (round_tripped, round_tripped_text) = extract_annotations(&rendered);
        assert_eq!(
            annotations, round_tripped,
            "annotations rendered into the fixture should read back unchanged"
        );
        assert_eq!(
            stripped, round_tripped_text,
            "rendering annotations should not change the text they describe"
        );
        rendered
    }

    #[test]
    #[should_panic(expected = "Fixture has content before the first file marker")]
    fn parse_fixture_panics_on_content_before_first_file_marker() {
        FixtureWithProjectMeta::parse(
            r#"
        -module(main).
        foo() -> ok.
        //- /src/erl_eval.erl
        -module(erl_eval).
        "#,
        );
    }

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
        assert!(!fixture.diagnostics_enabled.use_erlang_service);
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
        assert!(fixture.diagnostics_enabled.use_erlang_service);
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
                buck_target_name: None,
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
                gen_src_files: None,
                applicable_files: None,
                is_test_target: None,
                is_buck_generated: None,
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
        let (annotations, text_without_annotations) = extract_annotations(&text);
        let res = annotations
            .into_iter()
            .map(|(range, ann)| (&text_without_annotations[range], ann))
            .collect::<Vec<_>>();

        assert_eq!(
            res[..3],
            [
                ("x", "def".into()),
                ("y", "def".into()),
                ("zoo", "type:\ni32".into())
            ]
        );
        assert_eq!(res[3].0.len(), 72);
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
        let (annotations, text_without_annotations) = extract_annotations(&text);
        let res = annotations
            .into_iter()
            .map(|(range, ann)| (&text_without_annotations[range], ann))
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
        let (annotations, _text_without_annotations) = extract_annotations(&text);
        let res = annotations
            .into_iter()
            .map(|(range, ann)| (format!("{range:?}"), &text[range], ann))
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
        let (res, _text_without_annotations) = extract_annotations(&text);

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
        let (res, _text_without_annotations) = extract_annotations(&text);

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
        let (annotations, _text_without_annotations) = extract_annotations(&text);
        let res = annotations
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
        let (annotations, _text_without_annotations) = extract_annotations(&text);
        let res = annotations
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
        let (annotations, text_without_annotations) = extract_annotations(&text);
        let res = annotations
            .into_iter()
            .map(|(range, ann)| (&text_without_annotations[range], range, ann))
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
                    58..93,
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
        let (annotations, _text_without_annotations) = extract_annotations(&text);
        let res = annotations
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
    fn render_annotations_zero_offset_on_first_line() {
        let rendered = check_render_round_trip(
            r#"
            main() ->
            %%<^^^ warning: W0000: something
                 zoo + 1.
            "#,
        );
        expect![[r#"
            main() ->
            %%<^^^ warning: W0000: something
                 zoo + 1.
        "#]]
        .assert_eq(&rendered);
    }

    #[test]
    fn render_annotations_keeps_trailing_newline_under_last_annotation() {
        let rendered = check_render_round_trip(
            r#"
            -module(main).
            main() -> ok.
            %%         ^^ warning: W0000: something
            "#,
        );
        expect![[r#"
            -module(main).
            main() -> ok.
            %%         ^^ warning: W0000: something
        "#]]
        .assert_eq(&rendered);
    }

    #[test]
    fn render_annotations_zero_offset_after_first_line() {
        let rendered = check_render_round_trip(
            r#"
            -module(main).

            main() ->
            %%<^^^ warning: W0000: something
            %%   | Related info: 0:1-2 elsewhere
                 zoo + 1.
            "#,
        );
        expect![[r#"
            -module(main).

            main() ->
            %%<^^^ warning: W0000: something
            %%   | Related info: 0:1-2 elsewhere
                 zoo + 1.
        "#]]
        .assert_eq(&rendered);
    }

    #[test]
    #[should_panic(expected = "Annotation line 2 must start with exactly `%%`")]
    fn extract_annotations_rejects_extra_percent() {
        extract_annotations(&stdx::trim_indent(
            r#"
            -module(main).
            foo(Unused) -> ok.
            %%% ^^^^^^ warning: W0010: unused
            "#,
        ));
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

    #[test]
    fn parse_fixture_macros() {
        let fixture = FixtureWithProjectMeta::parse(
            r#"
//- /src/foo.erl macros:[FOO,BAR,BAZ]
-module(foo).
foo() -> ok.
"#,
        );
        let parsed = fixture.fixture;
        assert_eq!(1, parsed.len());

        let meta0 = &parsed[0];
        assert_eq!(
            vec!["FOO".to_string(), "BAR".to_string(), "BAZ".to_string()],
            meta0.macros
        );
    }

    #[test]
    fn parse_fixture_macros_empty() {
        let fixture = FixtureWithProjectMeta::parse(
            r#"
//- /src/foo.erl macros:[]
-module(foo).
foo() -> ok.
"#,
        );
        let parsed = fixture.fixture;
        assert_eq!(1, parsed.len());

        let meta0 = &parsed[0];
        assert!(meta0.macros.is_empty());
    }

    #[test]
    fn parse_fixture_otp_apps_single() {
        let fixture = FixtureWithProjectMeta::parse(
            r#"
//- otp_apps:stdlib
//- /src/foo.erl
-module(foo).
"#,
        );
        assert_eq!(fixture.otp_apps, vec!["stdlib".to_string()]);
    }

    #[test]
    fn parse_fixture_otp_apps_multiple() {
        let fixture = FixtureWithProjectMeta::parse(
            r#"
//- otp_apps:stdlib,kernel,crypto
//- /src/foo.erl
-module(foo).
"#,
        );
        assert_eq!(
            fixture.otp_apps,
            vec![
                "stdlib".to_string(),
                "kernel".to_string(),
                "crypto".to_string()
            ]
        );
    }

    #[test]
    fn parse_fixture_otp_apps_with_spaces() {
        let fixture = FixtureWithProjectMeta::parse(
            r#"
//- otp_apps:stdlib, kernel
//- /src/foo.erl
-module(foo).
"#,
        );
        assert_eq!(
            fixture.otp_apps,
            vec!["stdlib".to_string(), "kernel".to_string()]
        );
    }

    #[test]
    fn parse_fixture_otp_apps_empty_when_absent() {
        let fixture = FixtureWithProjectMeta::parse(
            r#"
//- /src/foo.erl
-module(foo).
"#,
        );
        assert!(fixture.otp_apps.is_empty());
    }

    #[test]
    fn parse_fixture_otp_apps_with_other_directives() {
        let fixture = FixtureWithProjectMeta::parse(
            r#"
//- eqwalizer
//- native
//- otp_apps:stdlib
//- /src/foo.erl
-module(foo).
"#,
        );
        assert_eq!(fixture.otp_apps, vec!["stdlib".to_string()]);
        assert!(fixture.diagnostics_enabled.use_eqwalizer);
        assert!(fixture.diagnostics_enabled.use_native);
    }
}
