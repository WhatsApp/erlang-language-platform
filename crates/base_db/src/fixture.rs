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
use std::sync::Arc;
use std::sync::Arc as StdArc;

use eetf;
use elp_project_model::AppName;
use elp_project_model::AppType;
use elp_project_model::Project;
use elp_project_model::ProjectAppData;
use elp_project_model::ProjectBuildData;
use elp_project_model::buck::BuckProjectIndex;
use elp_project_model::otp::Otp;
use elp_project_model::otp::find_otp_app;
use elp_project_model::otp::read_otp_app_sources;
use elp_project_model::rebar::RebarProject;
use elp_project_model::test_fixture::DiagnosticsEnabled;
use elp_project_model::test_fixture::FixtureWithProjectMeta;
use elp_project_model::test_fixture::RangeOrOffset;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use paths::AbsPathBuf;
use vfs::FileId;
use vfs::VfsPath;
use vfs::file_set::FileSet;

use crate::FileMap;
use crate::FilePosition;
use crate::FileRange;
use crate::LineEndings;
use crate::ProjectApps;
use crate::RootQueryDb;
use crate::SourceDatabase;
use crate::SourceRoot;
use crate::input::IncludeOtp;
use crate::lower;
use crate::set_source_roots;

pub trait WithFixture: Default + SourceDatabase + 'static {
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
    fn with_fixture(fixture_str: &str) -> (Self, Fixture) {
        let mut db = Self::default();
        let (fixture, _file_map, _project) = Fixture::load(&mut db, fixture_str);
        let patterns: Vec<crate::DynamicCallPatternInput> = fixture
            .dynamic_call_patterns
            .iter()
            .map(|s| {
                crate::parse_dynamic_call_pattern(s)
                    .unwrap_or_else(|e| panic!("invalid dynamic call pattern '{}': {}", s, e))
            })
            .collect();
        db.set_extra_dynamic_call_patterns(std::sync::Arc::new(crate::build_index(patterns)));
        fixture.validate(&db);
        (db, fixture)
    }
}

impl<DB: SourceDatabase + Default + 'static> WithFixture for DB {}

/// Gives the file at `path` a `FileId` from `file_map`, and sets its text.
///
/// # Panics
///
/// Panics if the fixture already has a file at `path`.
fn add_file(
    db: &mut dyn SourceDatabase,
    file_map: &FileMap,
    path: VfsPath,
    text: Arc<str>,
) -> FileId {
    assert!(
        file_map.file_id(&path).is_none(),
        "the fixture lists {path} more than once"
    );
    let file_id = file_map.intern(db, path);
    let line_endings = LineEndings::detect(&text);
    db.file(file_id)
        .set_contents(db, Some((text, line_endings)));
    file_id
}

#[derive(Clone, Debug)]
pub struct Fixture {
    pub file_position: Option<(FileId, RangeOrOffset)>,
    pub files: Vec<FileId>,
    pub files_by_path: FxHashMap<VfsPath, FileId>,
    pub diagnostics_enabled: DiagnosticsEnabled,
    pub dynamic_call_patterns: Vec<String>,
    pub tags: FxHashMap<FileId, Vec<(TextRange, Option<String>)>>,
    pub annotations: FxHashMap<FileId, Vec<(TextRange, String)>>,
    pub expect_parse_errors: bool,
}

struct Builder {
    diagnostics_enabled: DiagnosticsEnabled,
}

impl Builder {
    fn new(diagnostics_enabled: DiagnosticsEnabled) -> Builder {
        Builder {
            diagnostics_enabled,
        }
    }

    fn needs_otp(&self) -> bool {
        self.diagnostics_enabled.use_eqwalizer
    }
}

impl Fixture {
    /// Loads the files of `test_fixture` into `db` and a new file map, sets
    /// their source roots, and lowers the fixture's project as the language
    /// server and the CLI lower theirs. Returns the fixture, the file map and
    /// the project.
    #[track_caller]
    pub fn load(db: &mut dyn RootQueryDb, test_fixture: &str) -> (Fixture, FileMap, Project) {
        let fixture_with_meta = FixtureWithProjectMeta::parse(test_fixture);
        let FixtureWithProjectMeta {
            fixture,
            diagnostics_enabled,
            expect_parse_errors,
            otp_apps: mut requested_otp_apps,
            dynamic_call_patterns,
        } = fixture_with_meta.clone();

        let builder = Builder::new(diagnostics_enabled.clone());
        let file_map = FileMap::default();

        let mut files = Vec::new();
        let source_root_prefix = "/".to_string();

        let mut file_position = None;
        let mut app_map = AppMap::default();
        let mut otp: Option<Otp> = None;
        let mut app_files = SourceRootMap::default();
        let mut files_by_path: FxHashMap<VfsPath, FileId> = FxHashMap::default();
        let mut tags: FxHashMap<FileId, Vec<(TextRange, Option<String>)>> = FxHashMap::default();
        let mut annotations: FxHashMap<FileId, Vec<(TextRange, String)>> = FxHashMap::default();

        // Collect per-app deps and file paths for building buck-style metadata
        let mut app_deps: FxHashMap<AppName, Vec<String>> = FxHashMap::default();
        let mut app_distributed_deps: FxHashMap<AppName, Vec<String>> = FxHashMap::default();
        let mut app_file_paths: FxHashMap<AppName, Vec<AbsPathBuf>> = FxHashMap::default();
        let mut has_buck_metadata = false;

        for mut entry in fixture.clone() {
            assert!(entry.path.starts_with(&source_root_prefix));

            let app_name = entry.app_data.name.clone();

            if let Some(otp_extra) = entry.otp
                && otp.is_none()
            {
                otp = Some(otp_extra);
            }

            // Convert macro names from fixture to eetf::Term atoms for app_data.macros
            // These will be picked up by file_external_defines_query
            for macro_name in &entry.macros {
                entry
                    .app_data
                    .macros
                    .push(eetf::Atom::from(macro_name.as_str()).into());
            }

            // Track buck metadata
            if entry.app_data.buck_target_name.is_some() {
                has_buck_metadata = true;
            }
            if !entry.deps.is_empty() {
                app_deps
                    .entry(app_name.clone())
                    .or_default()
                    .extend(entry.deps.iter().cloned());
            }
            if !entry.distributed_deps.is_empty() {
                app_distributed_deps
                    .entry(app_name.clone())
                    .or_default()
                    .extend(entry.distributed_deps.iter().cloned());
            }
            let abs_entry_path = AbsPathBuf::assert(entry.path.clone().into());
            app_file_paths
                .entry(app_name.clone())
                .or_default()
                .push(abs_entry_path.clone());
            for also in &entry.also_apps {
                app_file_paths
                    .entry(AppName(also.clone()))
                    .or_default()
                    .push(abs_entry_path.clone());
            }

            app_map.combine(entry.app_data);

            let path = VfsPath::new_real_path(entry.path);
            let file_id = add_file(db, &file_map, path.clone(), Arc::from(entry.text));
            if let Some(range) = entry.marker_pos {
                assert!(file_position.is_none());
                file_position = Some((file_id, range));
            }
            // When src_app is specified, place the file into that app's
            // SourceRoot/FileSet instead. This models buck's shared source
            // root scenario (e.g., lib + test targets in the same directory).
            let source_root_app = if let Some(ref override_app) = entry.src_app {
                AppName(override_app.clone())
            } else {
                app_name.clone()
            };
            app_files.insert(source_root_app, file_id, path.clone());
            files_by_path.insert(path, file_id);
            files.push(file_id);
            tags.insert(file_id, entry.tags);
            annotations.insert(file_id, entry.annotations);
        }
        if builder.needs_otp() {
            // When eqwalizer is enabled, we need erts, stdlib, and kernel
            // so that eqwalizer can resolve type declarations (e.g.,
            // calendar:date() referenced in erlang.erl specs).
            // These form the minimal Erlang release.
            for implicit_app in &["erts", "stdlib", "kernel"] {
                let name = implicit_app.to_string();
                if !requested_otp_apps.contains(&name) {
                    requested_otp_apps.push(name);
                }
            }
        }

        // Load OTP apps (explicitly requested via `//- otp_apps:` directive
        // plus implicit ones from `//- eqwalizer`)
        for otp_app_name in &requested_otp_apps {
            let app_data = find_otp_app(otp_app_name).unwrap_or_else(|| {
                panic!(
                    "OTP app '{}' requested via //- otp_apps: directive not found in OTP installation",
                    otp_app_name
                )
            });
            for (path, contents) in read_otp_app_sources(&app_data) {
                let path = VfsPath::new_real_path(path.to_string_lossy().to_string());
                let file_id = add_file(db, &file_map, path.clone(), Arc::from(contents));
                app_files.insert(app_data.name.clone(), file_id, path);
            }
            app_map.combine(app_data);
        }

        let otp = otp.unwrap_or_else(|| Otp {
            // We only care about the otp lib_dir for the tests
            lib_dir: AbsPathBuf::assert("/".into()),
        });

        let root = AbsPathBuf::assert("/".into());
        let mut apps: Vec<ProjectAppData> = app_map.all_apps().cloned().collect();
        let mut project_buck_index = None;

        // If any app has buck metadata, build a BuckProjectIndex and set
        // applicable_files so the buck code path in resolve_remote_query
        // is exercised.
        if has_buck_metadata {
            let mut buck_index = BuckProjectIndex::default();

            for app in &mut apps {
                if let Some(ref target_name) = app.buck_target_name {
                    // Register app ↔ target mapping
                    buck_index
                        .app_deps
                        .register_app_target(app.name.clone(), target_name.clone());

                    // Set applicable_files from the collected file paths
                    if let Some(paths) = app_file_paths.get(&app.name) {
                        let file_set: FxHashSet<AbsPathBuf> = paths.iter().cloned().collect();
                        app.applicable_files = Some(file_set);
                    }
                }

                // Add remote include entries for .hrl files in include_dirs
                for hrl_path in app_file_paths
                    .get(&app.name)
                    .into_iter()
                    .flatten()
                    .filter(|p| p.extension() == Some("hrl"))
                {
                    // Build a remote include key like "R:app_name/include/header.hrl"
                    if let Some(file_name) = hrl_path.file_name() {
                        let remote_path = elp_syntax::SmolStr::new(format!(
                            "R:{}/include/{}",
                            app.name, file_name
                        ));
                        buck_index.includes.insert(remote_path, hrl_path.clone());
                    }
                }
            }

            // Wire up deps
            for (app_name, dep_names) in &app_deps {
                if let Some(app) = apps.iter().find(|a| &a.name == app_name)
                    && let Some(ref source_target) = app.buck_target_name
                {
                    for dep_name in dep_names {
                        let dep_app_name = AppName(dep_name.clone());
                        if let Some(dep_app) = apps.iter().find(|a| a.name == dep_app_name)
                            && let Some(ref dep_target) = dep_app.buck_target_name
                        {
                            buck_index
                                .app_deps
                                .add_dep(source_target.clone(), dep_target.clone());
                        }
                    }
                }
            }

            // Wire up distributed_dependencies, which name applications
            // directly rather than buck targets.
            for (app_name, dep_names) in &app_distributed_deps {
                if let Some(app) = apps.iter().find(|a| &a.name == app_name)
                    && let Some(ref source_target) = app.buck_target_name
                {
                    for dep_name in dep_names {
                        buck_index
                            .app_deps
                            .add_distributed_dep(source_target.clone(), AppName(dep_name.clone()));
                    }
                }
            }

            // Update the app_map with modified apps (applicable_files set)
            for app in &apps {
                app_map.update(app.clone());
            }

            project_buck_index = Some(StdArc::new(buck_index));
        }

        let apps_with_includes = RebarProject::add_app_includes(apps, &[], &otp.lib_dir);
        let rebar_project = RebarProject::new(root, Default::default());
        let mut project = Project::otp(otp, app_map.otp_apps().cloned().collect());
        project.add_apps(apps_with_includes);
        project.project_build_data = ProjectBuildData::Rebar(rebar_project);
        project.buck_index = project_buck_index;
        project.eqwalizer_config.enable_all = diagnostics_enabled.use_eqwalizer;

        let projects = [project.clone()];

        let project_apps = ProjectApps::new(&projects, IncludeOtp::Yes);

        let mut roots = Vec::new();
        // We must iterate here in project_apps order, it defines the
        // mapping of apps to SourceRootIds
        for (_project_id, app) in &project_apps.all_apps {
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
        set_source_roots(db, roots);
        lower(db, &file_map, &project_apps);
        (
            Fixture {
                file_position,
                files,
                files_by_path,
                diagnostics_enabled,
                dynamic_call_patterns,
                tags,
                annotations,
                expect_parse_errors,
            },
            file_map,
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

    /// Validate all files in the fixture for syntax errors.
    /// Panics with context if any syntax errors are found.
    /// Skips validation if `expect_parse_errors` is set to true.
    #[track_caller]
    pub fn validate<DB: RootQueryDb>(&self, db: &DB) {
        if self.expect_parse_errors {
            return;
        }

        let mut errors_found = Vec::new();

        for file_id in &self.files {
            let parse = db.parse(*file_id);
            let errors = parse.errors();

            if !errors.is_empty() {
                let path = self
                    .files_by_path
                    .iter()
                    .find_map(|(vfs_path, id)| {
                        if id == file_id {
                            Some(
                                vfs_path
                                    .as_path()
                                    .map(|p| p.to_string())
                                    .unwrap_or_else(|| format!("{:?}", vfs_path)),
                            )
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| format!("FileId({:?})", file_id));

                let file = db.file(*file_id);
                let tree = parse.tree();
                errors_found.push((path, file.text(db).to_string(), errors.to_vec(), tree));
            }
        }

        if !errors_found.is_empty() {
            let mut message =
                String::from("Fixture validation failed: syntax errors found in test fixture\n\n");

            for (path, text, errors, tree) in errors_found {
                message.push_str(&format!("File: {}\n", path));
                message.push_str(&format!("Errors: {:?}\n", errors));
                message.push_str(&format!("Content:\n{}\n", text));
                message.push_str(&format!("Parse Tree:\n{:#?}\n", tree));
                message.push_str("---\n");
            }
            message.push_str(
                "If this is expected, add `//- expect_parse_errors` to the start of the fixture\n",
            );

            panic!("{}", message);
        }
    }
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

    fn update(&mut self, app: ProjectAppData) {
        self.app_map.insert(app.name.clone(), app);
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
    use std::sync::Arc;

    use expect_test::expect;

    use super::Fixture;
    use super::WithFixture;
    use crate::AppData;
    use crate::ProjectData;
    use crate::RootQueryDb;
    use crate::Workspace;
    use crate::test_db::TestDB;

    #[test]
    fn unresolved_macro_diag_include_dir() {
        // Test the scenario where the include file is referenced
        // relative to a project include directory
        let lowered = lowered_model(
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
            [
                (
                    ProjectData {
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
                        otp_project_id: Some(
                            ProjectId(
                                1,
                            ),
                        ),
                        eqwalizer_config: EqwalizerConfig {
                            enable_all: false,
                            max_tasks: 4,
                            ignore_modules: [],
                            ignore_modules_compiled_patterns: [],
                        },
                        buck_index: None,
                    },
                    [
                        AppData {
                            project_id: ProjectId(
                                0,
                            ),
                            name: AppName(
                                "test-fixture",
                            ),
                            buck_target_name: None,
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
                        AppData {
                            project_id: ProjectId(
                                0,
                            ),
                            name: AppName(
                                "foo-app",
                            ),
                            buck_target_name: None,
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
                    ],
                ),
                (
                    ProjectData {
                        source_roots: [
                            SourceRootId(
                                2,
                            ),
                        ],
                        root_dir: AbsPathBuf(
                            "/opt/lib",
                        ),
                        otp_project_id: Some(
                            ProjectId(
                                1,
                            ),
                        ),
                        eqwalizer_config: EqwalizerConfig {
                            enable_all: true,
                            max_tasks: 4,
                            ignore_modules: [],
                            ignore_modules_compiled_patterns: [],
                        },
                        buck_index: None,
                    },
                    [
                        AppData {
                            project_id: ProjectId(
                                1,
                            ),
                            name: AppName(
                                "comp",
                            ),
                            buck_target_name: None,
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
                    ],
                ),
            ]"#]]
        .assert_eq(&lowered);
    }

    #[test]
    fn unresolved_macro_diag_include_dir2() {
        // Test the scenario where the include file is referenced
        // relative to a project include directory
        let lowered = lowered_model(
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
            [
                (
                    ProjectData {
                        source_roots: [
                            SourceRootId(
                                0,
                            ),
                        ],
                        root_dir: AbsPathBuf(
                            "/",
                        ),
                        otp_project_id: Some(
                            ProjectId(
                                1,
                            ),
                        ),
                        eqwalizer_config: EqwalizerConfig {
                            enable_all: false,
                            max_tasks: 4,
                            ignore_modules: [],
                            ignore_modules_compiled_patterns: [],
                        },
                        buck_index: None,
                    },
                    [
                        AppData {
                            project_id: ProjectId(
                                0,
                            ),
                            name: AppName(
                                "test-fixture",
                            ),
                            buck_target_name: None,
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
                    ],
                ),
                (
                    ProjectData {
                        source_roots: [],
                        root_dir: AbsPathBuf(
                            "/",
                        ),
                        otp_project_id: Some(
                            ProjectId(
                                1,
                            ),
                        ),
                        eqwalizer_config: EqwalizerConfig {
                            enable_all: true,
                            max_tasks: 4,
                            ignore_modules: [],
                            ignore_modules_compiled_patterns: [],
                        },
                        buck_index: None,
                    },
                    [],
                ),
            ]"#]]
        .assert_eq(&lowered);
    }

    /// The data of each project, and of the app of each of its source roots.
    fn lowered_model(fixture: &str) -> String {
        let (db, _) = TestDB::with_fixture(fixture);
        let projects: Vec<(Arc<ProjectData>, Vec<AppData>)> = Workspace::get(&db)
            .projects(&db)
            .iter()
            .map(|project| {
                let project_data = project.project_data(&db);
                let apps = project_data
                    .source_roots
                    .iter()
                    .filter_map(|source_root_id| db.app_data(*source_root_id))
                    .map(|app_data| (*app_data).clone())
                    .collect();
                (project_data, apps)
            })
            .collect();
        format!("{projects:#?}")
    }

    #[test]
    #[should_panic(expected = "OTP app 'stdlb' requested via //- otp_apps: directive not found")]
    fn otp_apps_misspelled_app_panics() {
        Fixture::load(
            &mut TestDB::default(),
            r#"
//- otp_apps:stdlb
//- /src/foo.erl
-module(foo).
"#,
        );
    }

    #[test]
    #[should_panic(expected = "the fixture lists /src/a.erl more than once")]
    fn a_repeated_path_panics() {
        TestDB::with_fixture(
            r#"
//- /src/a.erl
-module(a).
//- /src/a.erl
-module(a_again).
"#,
        );
    }
}
