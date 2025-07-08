/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! A set of high-level utility fixture methods to use in tests.

// Based on rust-analyzer base_db::fixture

use std::collections::hash_map::Entry;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::sync::Arc;

use elp_project_model::AppName;
use elp_project_model::AppType;
use elp_project_model::Project;
use elp_project_model::ProjectAppData;
use elp_project_model::ProjectBuildData;
use elp_project_model::ProjectManifest;
use elp_project_model::buck::BuckQueryConfig;
use elp_project_model::json::JsonConfig;
use elp_project_model::otp::OTP_ERLANG_APP;
use elp_project_model::otp::OTP_ERLANG_MODULE;
use elp_project_model::otp::Otp;
use elp_project_model::rebar::RebarProject;
use elp_project_model::temp_dir::TempDir;
use elp_project_model::test_fixture::DiagnosticsEnabled;
use elp_project_model::test_fixture::FixtureWithProjectMeta;
use elp_project_model::test_fixture::RangeOrOffset;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use paths::AbsPathBuf;
use paths::Utf8Path;
use vfs::FileId;
use vfs::VfsPath;
use vfs::file_set::FileSet;

use crate::FilePosition;
use crate::FileRange;
use crate::ProjectApps;
use crate::ProjectId;
use crate::SourceDatabaseExt;
use crate::SourceRoot;
use crate::change::Change;
use crate::input::IncludeOtp;

pub trait WithFixture: Default + SourceDatabaseExt + 'static {
    #[track_caller]
    fn with_single_file(fixture: &str) -> (Self, FileId) {
        let (db, fixture) = Self::with_fixture(fixture);
        assert_eq!(fixture.files.len(), 1);
        (db, fixture.files[0])
    }

    #[track_caller]
    fn with_many_files(fixture: &str) -> (Self, Vec<FileId>, DiagnosticsEnabled) {
        let (db, fixture) = Self::with_fixture(fixture);
        assert!(fixture.file_position.is_none());
        (db, fixture.files, fixture.diagnostics_enabled)
    }

    #[track_caller]
    fn with_range(fixture: &str) -> (Self, FileRange) {
        let (db, fixture) = Self::with_fixture(fixture);
        (db, fixture.range())
    }

    #[track_caller]
    fn with_range_or_offset(fixture: &str) -> (Self, FileId, RangeOrOffset) {
        let (db, fixture) = Self::with_fixture(fixture);
        let (file_id, range_or_offset) = fixture
            .file_position
            .expect("Could not find file position in fixture. Did you forget to add an `~`?");
        (db, file_id, range_or_offset)
    }

    #[track_caller]
    fn with_fixture(fixture_str: &str) -> (Self, ChangeFixture) {
        let (fixture, change) = ChangeFixture::parse(fixture_str);
        let mut db = Self::default();
        change.apply(&mut db, &|path| fixture.resolve_file_id(path));
        (db, fixture)
    }
}

impl<DB: SourceDatabaseExt + Default + 'static> WithFixture for DB {}

#[derive(Clone, Debug)]
pub struct ChangeFixture {
    pub file_position: Option<(FileId, RangeOrOffset)>,
    pub files: Vec<FileId>,
    pub files_by_path: FxHashMap<VfsPath, FileId>,
    pub diagnostics_enabled: DiagnosticsEnabled,
    pub tags: FxHashMap<FileId, Vec<(TextRange, Option<String>)>>,
    pub annotations: FxHashMap<FileId, Vec<(TextRange, String)>>,
}

struct Builder {
    project_dir: Option<TempDir>,
    diagnostics_enabled: DiagnosticsEnabled,
}

impl Builder {
    fn new(diagnostics_enabled: DiagnosticsEnabled) -> Builder {
        let project_dir = if diagnostics_enabled.needs_fixture_on_disk() {
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

        Builder {
            project_dir,
            diagnostics_enabled,
        }
    }

    fn absolute_path(&self, path: String) -> String {
        if let Some(project_dir) = &self.project_dir {
            let project_dir_str = project_dir.path().as_os_str().to_str().unwrap();
            format!("{}/{}", project_dir_str, path)
        } else {
            path
        }
    }

    fn project_dir(&self) -> Option<&Utf8Path> {
        self.project_dir
            .as_ref()
            .and_then(|d| Utf8Path::from_path(d.path()))
    }

    fn needs_otp(&self) -> bool {
        self.project_dir().is_some() || self.diagnostics_enabled.use_eqwalizer
    }
}

impl ChangeFixture {
    #[track_caller]
    pub fn parse(test_fixture: &str) -> (ChangeFixture, Change) {
        let (fixture, change, _) = ChangeFixture::parse_detail(test_fixture);
        (fixture, change)
    }

    #[track_caller]
    pub fn parse_detail(test_fixture: &str) -> (ChangeFixture, Change, Project) {
        let fixture_with_meta = FixtureWithProjectMeta::parse(test_fixture);
        let FixtureWithProjectMeta {
            fixture,
            mut diagnostics_enabled,
        } = fixture_with_meta.clone();

        let builder = Builder::new(diagnostics_enabled.clone());
        let mut change = Change::new();

        let mut files = Vec::new();
        let source_root_prefix = "/".to_string();
        let mut file_id = FileId::from_raw(0);

        let mut file_position = None;
        let mut app_map = AppMap::default();
        let mut otp: Option<Otp> = None;
        let mut app_files = SourceRootMap::default();
        let mut files_by_path: FxHashMap<VfsPath, FileId> = FxHashMap::default();
        let mut tags: FxHashMap<FileId, Vec<(TextRange, Option<String>)>> = FxHashMap::default();
        let mut annotations: FxHashMap<FileId, Vec<(TextRange, String)>> = FxHashMap::default();

        for entry in fixture.clone() {
            if let Some(range) = entry.marker_pos {
                assert!(file_position.is_none());
                file_position = Some((file_id, range));
            }

            assert!(entry.path.starts_with(&source_root_prefix));

            let app_name = entry.app_data.name.clone();

            if let Some(otp_extra) = entry.otp {
                if otp.is_none() {
                    otp = Some(otp_extra);
                }
            }
            app_map.combine(entry.app_data);

            change.change_file(file_id, Some(Arc::from(entry.text)));

            let path = if diagnostics_enabled.needs_fixture_on_disk()
                && entry.path.char_indices().nth(0) == Some((0, '/'))
            {
                let mut path = entry.path.clone();
                path.remove(0);
                let path: String = builder.absolute_path(path);
                VfsPath::new_real_path(path)
            } else {
                VfsPath::new_real_path(entry.path)
            };
            app_files.insert(app_name, file_id, path.clone());
            files_by_path.insert(path, file_id);
            files.push(file_id);
            tags.insert(file_id, entry.tags);
            annotations.insert(file_id, entry.annotations);

            inc_file_id(&mut file_id);
        }
        if builder.needs_otp() {
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
            inc_file_id(&mut file_id);
        }

        let otp = otp.unwrap_or_else(|| Otp {
            // We only care about the otp lib_dir for the tests
            lib_dir: AbsPathBuf::assert("/".into()),
        });

        let root = AbsPathBuf::assert("/".into());
        let apps = app_map.all_apps().cloned().collect();
        let apps_with_includes = RebarProject::add_app_includes(apps, &[], &otp.lib_dir);
        let rebar_project = RebarProject::new(root, Default::default());
        let mut project = Project::otp(otp, app_map.otp_apps().cloned().collect());
        project.add_apps(apps_with_includes);
        project.project_build_data = ProjectBuildData::Rebar(rebar_project);

        if let Some(project_dir) = builder.project_dir() {
            // Dump a copy of the fixture into a temp dir
            let files: Vec<(String, String)> = fixture
                .iter()
                .map(|entry| (entry.path.clone(), entry.text.clone()))
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
            let loaded_project = Project::load(
                &manifest,
                elp_config.eqwalizer,
                &BuckQueryConfig::BuildGeneratedCode,
                &|_| {},
            )
            .unwrap();
            project = loaded_project;
        }

        let projects = [project.clone()];

        let project_apps = if diagnostics_enabled.needs_fixture_on_disk() {
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
                files_by_path,
                diagnostics_enabled,
                tags,
                annotations,
            },
            change,
            project,
        )
    }

    pub fn annotations(&self) -> Vec<(FileRange, String)> {
        self.annotations
            .iter()
            .flat_map(|(&file_id, annotations)| {
                annotations.iter().map(move |(range, content)| {
                    (
                        FileRange {
                            file_id,
                            range: *range,
                        },
                        content.clone(),
                    )
                })
            })
            .collect()
    }

    pub fn annotations_by_file_id(&self, file_id: &FileId) -> Vec<(TextRange, String)> {
        self.annotations.get(file_id).cloned().unwrap_or_default()
    }

    #[track_caller]
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
            FileId::from_raw(0)
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

    pub fn resolve_file_id(&self, path: &AbsPathBuf) -> Option<FileId> {
        self.files_by_path
            .get(&VfsPath::from(path.clone()))
            .cloned()
    }
}

fn inc_file_id(file_id: &mut FileId) {
    *file_id = FileId::from_raw(file_id.index() + 1);
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

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::ChangeFixture;

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
                        ): (
                            Some(
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
                                    include_dirs: [],
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
                                    is_test_target: None,
                                },
                            ),
                            None,
                        ),
                        SourceRootId(
                            2,
                        ): (
                            Some(
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
                                    include_dirs: [
                                        AbsPathBuf(
                                            "/opt/lib/comp-1.3/include",
                                        ),
                                    ],
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
                                    is_test_target: None,
                                },
                            ),
                            None,
                        ),
                        SourceRootId(
                            1,
                        ): (
                            Some(
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
                                    include_dirs: [
                                        AbsPathBuf(
                                            "/include",
                                        ),
                                    ],
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
                                    is_test_target: None,
                                },
                            ),
                            None,
                        ),
                        SourceRootId(
                            3,
                        ): (
                            None,
                            None,
                        ),
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
                            include_mapping: Some(
                                IncludeMapping {
                                    includes: {},
                                },
                            ),
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
                            include_mapping: Some(
                                IncludeMapping {
                                    includes: {},
                                },
                            ),
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
                        ): (
                            Some(
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
                                    include_dirs: [
                                        AbsPathBuf(
                                            "/extra/include",
                                        ),
                                        AbsPathBuf(
                                            "/include",
                                        ),
                                    ],
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
                                    is_test_target: None,
                                },
                            ),
                            None,
                        ),
                        SourceRootId(
                            1,
                        ): (
                            None,
                            None,
                        ),
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
                            include_mapping: Some(
                                IncludeMapping {
                                    includes: {},
                                },
                            ),
                        },
                        ProjectId(
                            1,
                        ): ProjectData {
                            source_roots: [],
                            root_dir: AbsPathBuf(
                                "/",
                            ),
                            deps_ebins: [],
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
                            include_mapping: Some(
                                IncludeMapping {
                                    includes: {},
                                },
                            ),
                        },
                    },
                    catch_all_source_root: SourceRootId(
                        1,
                    ),
                },
            )"#]]
        .assert_eq(format!("{:#?}", change.app_structure).as_str());
    }
}
