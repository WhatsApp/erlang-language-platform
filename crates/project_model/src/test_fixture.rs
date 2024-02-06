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

use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

use paths::AbsPath;
use paths::AbsPathBuf;
pub use stdx::trim_indent;
use tempfile::tempdir;
use tempfile::TempDir;

use crate::otp::Otp;
use crate::AppName;
use crate::ProjectAppData;

#[derive(Debug, Eq, PartialEq)]
pub struct Fixture {
    pub path: String,
    pub text: String,
    pub app_data: ProjectAppData,
    pub otp: Option<Otp>,
    pub scratch_buffer: Option<PathBuf>,
}

#[derive(Clone, Debug, Default)]
pub struct DiagnosticsEnabled {
    pub use_native: bool,
    pub use_erlang_service: bool,
    pub use_ct: bool,
}

impl DiagnosticsEnabled {
    pub fn needs_erlang_service(&self) -> bool {
        let DiagnosticsEnabled {
            use_native: _,
            use_erlang_service,
            use_ct,
        } = self;
        *use_erlang_service || *use_ct
    }

    #[track_caller]
    pub fn assert_ct_enabled(&self) {
        if !self.use_ct {
            panic!("Expecting `//- common_test` at top of fixture");
        }
    }

    /// If no other diagnostics are enabled, enable native.
    /// If any are explicitly enabled, then native must also be
    /// explicitly enabled.
    fn set_default_native(&mut self) {
        let DiagnosticsEnabled {
            use_native: _,
            use_erlang_service,
            use_ct,
        } = &self;
        if !(*use_erlang_service || *use_ct) {
            self.use_native = true;
        }
    }
}

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

        FixtureWithProjectMeta {
            fixture: res,
            diagnostics_enabled,
        }
    }

    /// Create an on-disk image of a test fixture in a temporary directory
    pub fn gen_project(spec: &str) -> TempDir {
        let fixtures = FixtureWithProjectMeta::parse(spec);
        let tmp_dir = TempDir::new().unwrap();
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

        let mut path = components[0].to_string();
        assert!(
            path.starts_with('/'),
            "fixture path does not start with `/`: {:?}",
            path
        );

        let mut app_name = None;
        let mut include_dirs = Vec::new();
        let mut extra_dirs = Vec::new();
        let mut otp = None;
        let mut scratch_buffer = None;

        for component in components[1..].iter() {
            let (key, value) = component
                .split_once(':')
                .unwrap_or_else(|| panic!("invalid meta line: {:?}", meta));
            match key {
                "app" => app_name = Some(AppName(value.to_string())),
                "include_path" => include_dirs
                    .push(AbsPath::assert(&PathBuf::from(value.to_string())).normalize()),
                "otp_app" => {
                    // We have an app directory, the OTP lib dir is its parent
                    let path = AbsPathBuf::assert(PathBuf::from(value.to_string()));
                    let lib_dir = path.parent().unwrap().normalize();
                    let versioned_name = path.file_name().unwrap().to_str().unwrap().to_string();
                    let app = ProjectAppData::otp_app_data(&versioned_name, path);

                    otp = Some((Otp { lib_dir }, app));
                }
                "extra" => {
                    // We have an extra directory, such as for a test suite
                    // It needs to be relative to the app dir.
                    let dir = value.to_string();
                    extra_dirs.push(dir);
                }
                "scratch_buffer" => {
                    // Certain features depending on the Erlang Service
                    // require a physical copy of the file on the FS
                    let tmp_dir = tempdir().unwrap();
                    let tmp_path = tmp_dir.path().join(path.strip_prefix('/').unwrap());
                    path = tmp_path.to_str().unwrap().to_string();
                    scratch_buffer = Some(tmp_path);
                }
                _ => panic!("bad component: {:?}", component),
            }
        }

        let (otp, app_data) = if let Some((otp, app)) = otp {
            (Some(otp), app)
        } else {
            // Try inferring dir - parent once to get to ./src, parent twice to get to app root
            let dir = AbsPath::assert(Path::new(&path)).parent().unwrap();
            let dir = dir.parent().unwrap_or(dir).normalize();
            let app_name = app_name.unwrap_or(AppName("test-fixture".to_string()));
            let abs_path = AbsPathBuf::assert(PathBuf::from(path.clone()));
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
            scratch_buffer,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use expect_test::expect;
    use paths::AbsPath;

    use super::FixtureWithProjectMeta;

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
            vec![AbsPath::assert(&PathBuf::from("/include")).normalize()],
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
                ebin: Some(
                    AbsPathBuf(
                        "/ebin",
                    ),
                ),
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
            }"#]]
        .assert_eq(format!("{:#?}", meta0.app_data).as_str());
    }
}
