/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lowering: setting the project model in the database from the loaded
//! projects. The language server, the CLI loaders and test fixtures all lower
//! through [`lower`].

use std::sync::Arc;

use elp_project_model::AppType;
use elp_project_model::ProjectAppData;
use elp_project_model::ProjectBuildData;
use elp_project_model::buck::TargetFullName;
use fxhash::FxHashMap;
use salsa::Setter;
use salsa::plumbing::AsId;
use vfs::AbsPathBuf;
use vfs::VfsPath;

use crate::App;
use crate::File;
use crate::FileMap;
use crate::Project;
use crate::ProjectApps;
use crate::RootQueryDb;
use crate::SourceDatabase;
use crate::SourceRoot;
use crate::SourceRootId;
use crate::Workspace;

/// Sets the project model from `project_apps`, setting only the fields that
/// changed since the last lowering: salsa's setters bump the revision without
/// comparing values. A loaded project gets the input the workspace has for its
/// root, a loaded one before a removed one; projects that share a root take
/// those inputs in order. An app gets the input its project has for its key the
/// same way. Salsa never frees an input: a project that disappears moves to
/// `Workspace::removed_projects`, and an app to `Project::removed_apps`, with
/// their large fields cleared, and gets its input back if it returns. Every
/// file an app lists gets a `FileId` from `file_map`, whether or not it has
/// loaded. Returns the app of each generated source file.
pub fn lower(
    db: &mut dyn RootQueryDb,
    file_map: &FileMap,
    project_apps: &ProjectApps,
) -> FxHashMap<AbsPathBuf, App> {
    let (mut known, previous_source_roots): (Vec<Project>, usize) = match Workspace::try_get(db) {
        Some(workspace) => {
            let projects = workspace.projects(db);
            let removed = workspace.removed_projects(db).iter();
            (
                projects.iter().chain(removed).copied().collect(),
                projects
                    .iter()
                    .map(|project| project.source_roots(db).len())
                    .sum::<usize>()
                    + 1,
            )
        }
        None => (Vec::new(), 0),
    };

    let mut loaded_apps: Vec<Vec<(usize, &ProjectAppData)>> =
        vec![Vec::new(); project_apps.projects.len()];
    for (idx, (project_idx, loaded)) in project_apps.all_apps.iter().enumerate() {
        loaded_apps[*project_idx].push((idx, *loaded));
    }
    let mut apps: Vec<Option<App>> = vec![None; project_apps.all_apps.len()];
    let mut projects = Vec::with_capacity(project_apps.projects.len());
    let mut own_apps: Vec<Vec<App>> = Vec::with_capacity(project_apps.projects.len());
    // A generated input several apps list goes to the first of them, in the
    // order of the projects and then of their apps.
    let mut gen_src_apps = FxHashMap::default();
    for (loaded, loaded_apps) in project_apps.projects.iter().zip(loaded_apps) {
        let root = VfsPath::from(loaded.root().into_owned());
        let found = known.iter().position(|project| *project.root(db) == root);
        let known_project = found.map(|idx| known.remove(idx));
        let source_roots = loaded_apps
            .iter()
            .map(|(idx, _)| SourceRootId(*idx as u32))
            .collect();
        let project = lower_project(db, loaded, root, source_roots, known_project);
        let mut own = Vec::with_capacity(loaded_apps.len());
        for (idx, app) in lower_apps(db, file_map, project, &loaded_apps) {
            apps[idx] = Some(app);
            own.push(app);
            let (_, loaded) = project_apps.all_apps[idx];
            for path in loaded.gen_src_files.iter().flatten() {
                gen_src_apps.entry(path.clone()).or_insert(app);
            }
        }
        projects.push(project);
        own_apps.push(own);
    }

    // A project links to the OTP project of its OTP installation, and lists
    // that project's apps after its own.
    let otp_projects: FxHashMap<&AbsPathBuf, usize> = project_apps
        .projects
        .iter()
        .enumerate()
        .filter(|(_, loaded)| loaded.project_build_data == ProjectBuildData::Otp)
        .map(|(idx, loaded)| (&loaded.otp.lib_dir, idx))
        .collect();
    for (idx, loaded) in project_apps.projects.iter().enumerate() {
        let project = projects[idx];
        let otp_idx = otp_projects.get(&loaded.otp.lib_dir).copied();
        let otp = otp_idx.map(|otp_idx| projects[otp_idx]);
        if project.otp(db) != otp {
            project.set_otp(db).to(otp);
        }
        let otp_apps = match otp_idx {
            Some(otp_idx) if otp_idx != idx => own_apps[otp_idx].as_slice(),
            _ => &[],
        };
        let apps: Arc<[App]> = own_apps[idx].iter().chain(otp_apps).copied().collect();
        if *project.apps(db) != apps {
            project.set_apps(db).to(apps);
        }
    }

    // Whatever is left is no longer loaded, and neither are its own apps.
    for &project in &known {
        let apps: Vec<App> = project
            .apps(db)
            .iter()
            .copied()
            .filter(|app| app.project(db) == project)
            .collect();
        for &app in &apps {
            clear_app(db, app);
        }
        let removed_apps: Arc<[App]> = apps
            .iter()
            .chain(project.removed_apps(db).iter())
            .copied()
            .collect();
        if !project.apps(db).is_empty() {
            project.set_apps(db).to(Arc::from([]));
        }
        if *project.removed_apps(db) != removed_apps {
            project.set_removed_apps(db).to(removed_apps);
        }
        if project.buck_index(db).is_some() {
            project.set_buck_index(db).to(None);
        }
        if !project.source_roots(db).is_empty() {
            project.set_source_roots(db).to(Arc::from([]));
        }
    }

    let apps: Vec<App> = apps
        .into_iter()
        .map(|app| app.expect("every loaded app should belong to a loaded project"))
        .collect();

    // The last source root holds the files outside every app.
    let source_root_apps = apps.iter().map(|&app| Some(app)).chain([None]);
    for (idx, app) in source_root_apps.enumerate() {
        db.set_source_root_app(SourceRootId(idx as u32), app);
    }
    for idx in apps.len() + 1..previous_source_roots {
        db.set_source_root_app(SourceRootId(idx as u32), None);
    }

    let projects: Arc<[Project]> = projects.into();
    let removed: Arc<[Project]> = known.into();
    match Workspace::try_get(db) {
        Some(workspace) => {
            if *workspace.projects(db) != projects {
                workspace.set_projects(db).to(projects);
            }
            if *workspace.removed_projects(db) != removed {
                workspace.set_removed_projects(db).to(removed);
            }
        }
        None => {
            Workspace::new(db, projects, removed, 0);
        }
    }

    gen_src_apps
}

/// Identifies an app among the apps of its project.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum AppKey {
    /// Overlapping Buck projects can load the same target.
    Target(TargetFullName),
    /// The project of an OTP app is its OTP lib directory.
    Otp(String),
    /// JSON apps can share a directory.
    Dir { name: String, dir: VfsPath },
}

fn app_key(app: &ProjectAppData) -> AppKey {
    if app.app_type == AppType::Otp {
        AppKey::Otp(app.name.0.clone())
    } else if let Some(target) = &app.buck_target_name {
        AppKey::Target(target.clone())
    } else {
        AppKey::Dir {
            name: app.name.0.clone(),
            dir: VfsPath::from(app.dir.clone()),
        }
    }
}

/// The key of `app`, as [`app_key`] gives it for the app it was lowered from.
fn app_key_of(db: &dyn RootQueryDb, app: App) -> AppKey {
    if app.app_type(db) == AppType::Otp {
        AppKey::Otp(app.name(db).0.clone())
    } else if let Some(target) = app.buck_target_name(db) {
        AppKey::Target(target.clone())
    } else {
        AppKey::Dir {
            name: app.name(db).0.clone(),
            dir: app.dir(db).clone(),
        }
    }
}

/// The input for `loaded`: `known`, with the fields that changed set, else a
/// new one. Its apps and its OTP project are set once every project has an
/// input.
fn lower_project(
    db: &mut dyn RootQueryDb,
    loaded: &elp_project_model::Project,
    root: VfsPath,
    source_roots: Arc<[SourceRootId]>,
    known: Option<Project>,
) -> Project {
    let buck_index = loaded.buck_index.clone();
    let code_path: Arc<[AbsPathBuf]> = loaded.deps_ebins().into();
    let eqwalizer_config = loaded.eqwalizer_config.clone();
    let Some(project) = known else {
        return Project::new(
            db,
            root,
            Arc::from([]),
            Arc::from([]),
            None,
            buck_index,
            code_path,
            eqwalizer_config,
            source_roots,
        );
    };
    if *project.buck_index(db) != buck_index {
        project.set_buck_index(db).to(buck_index);
    }
    if *project.code_path(db) != code_path {
        project.set_code_path(db).to(code_path);
    }
    if *project.eqwalizer_config(db) != eqwalizer_config {
        project.set_eqwalizer_config(db).to(eqwalizer_config);
    }
    if *project.source_roots(db) != source_roots {
        project.set_source_roots(db).to(source_roots);
    }
    project
}

/// Lowers the own apps of `project` from `loaded`, given with their source
/// roots' indices. A loaded app gets the input the project has for its key, a
/// loaded one before a removed one; apps that share a key take those inputs in
/// order. Returns each app with its source root's index, sorted by key, so
/// that identical reloads compare equal; the sort is stable, so apps that
/// share a key stay in the loader's order.
fn lower_apps(
    db: &mut dyn RootQueryDb,
    file_map: &FileMap,
    project: Project,
    loaded: &[(usize, &ProjectAppData)],
) -> Vec<(usize, App)> {
    // In reverse, so that popping from the back takes the first.
    let mut known: FxHashMap<AppKey, Vec<App>> = FxHashMap::default();
    let known_apps: Vec<App> = project
        .apps(db)
        .iter()
        .chain(project.removed_apps(db).iter())
        .copied()
        .filter(|app| app.project(db) == project)
        .collect();
    for &app in known_apps.iter().rev() {
        known.entry(app_key_of(db, app)).or_default().push(app);
    }

    let mut own: Vec<(AppKey, usize, App)> = loaded
        .iter()
        .map(|&(idx, loaded)| {
            let key = app_key(loaded);
            let known_app = known.get_mut(&key).and_then(Vec::pop);
            (
                key,
                idx,
                lower_app(db, file_map, project, loaded, known_app),
            )
        })
        .collect();

    let mut removed: Vec<App> = known.into_values().flatten().collect();
    removed.sort_by_key(|app| app.as_id());
    for &app in &removed {
        clear_app(db, app);
    }

    own.sort_by(|(a, _, _), (b, _, _)| a.cmp(b));
    let removed: Arc<[App]> = removed.into();
    if *project.removed_apps(db) != removed {
        project.set_removed_apps(db).to(removed);
    }
    own.into_iter().map(|(_, idx, app)| (idx, app)).collect()
}

/// The input for `loaded`: `known`, with the fields that changed set, else a
/// new one.
fn lower_app(
    db: &mut dyn RootQueryDb,
    file_map: &FileMap,
    project: Project,
    loaded: &ProjectAppData,
    known: Option<App>,
) -> App {
    let name = loaded.name.clone();
    let dir = VfsPath::from(loaded.dir.clone());
    let files = app_files(db, file_map, loaded);
    let macros: Arc<[eetf::Term]> = loaded.macros.as_slice().into();
    let parse_transforms: Arc<[eetf::Term]> = loaded.parse_transforms.as_slice().into();
    // Other dependencies' ebins go on the code path.
    let ebin = if loaded.app_type == AppType::Otp {
        loaded.ebin.clone()
    } else {
        None
    };
    let docs_url = loaded.docs_url.clone();
    let buck_target_name = loaded.buck_target_name.clone();
    let include_dirs: Arc<[AbsPathBuf]> = loaded.include_dirs.as_slice().into();
    let include_path: Arc<[AbsPathBuf]> = loaded.include_path.as_slice().into();
    let src_dirs: Arc<[AbsPathBuf]> = loaded.abs_src_dirs.as_slice().into();
    let extra_src_dirs: Arc<[String]> = loaded.extra_src_dirs.as_slice().into();
    let Some(app) = known else {
        return App::new(
            db,
            project,
            name,
            loaded.app_type,
            dir,
            files,
            macros,
            parse_transforms,
            ebin,
            docs_url,
            buck_target_name,
            include_dirs,
            include_path,
            src_dirs,
            extra_src_dirs,
            loaded.is_test_target,
        );
    };
    if *app.name(db) != name {
        app.set_name(db).to(name);
    }
    if app.app_type(db) != loaded.app_type {
        app.set_app_type(db).to(loaded.app_type);
    }
    if *app.dir(db) != dir {
        app.set_dir(db).to(dir);
    }
    if *app.files(db) != files {
        app.set_files(db).to(files);
    }
    if *app.macros(db) != macros {
        app.set_macros(db).to(macros);
    }
    if *app.parse_transforms(db) != parse_transforms {
        app.set_parse_transforms(db).to(parse_transforms);
    }
    if *app.ebin(db) != ebin {
        app.set_ebin(db).to(ebin);
    }
    if *app.docs_url(db) != docs_url {
        app.set_docs_url(db).to(docs_url);
    }
    if *app.buck_target_name(db) != buck_target_name {
        app.set_buck_target_name(db).to(buck_target_name);
    }
    if *app.include_dirs(db) != include_dirs {
        app.set_include_dirs(db).to(include_dirs);
    }
    if *app.include_path(db) != include_path {
        app.set_include_path(db).to(include_path);
    }
    if *app.src_dirs(db) != src_dirs {
        app.set_src_dirs(db).to(src_dirs);
    }
    if *app.extra_src_dirs(db) != extra_src_dirs {
        app.set_extra_src_dirs(db).to(extra_src_dirs);
    }
    if app.is_test_target(db) != loaded.is_test_target {
        app.set_is_test_target(db).to(loaded.is_test_target);
    }
    app
}

/// The files `loaded` lists, sorted by path, each with its `FileId` from
/// `file_map`.
fn app_files(db: &dyn RootQueryDb, file_map: &FileMap, loaded: &ProjectAppData) -> Arc<[File]> {
    let mut paths: Vec<VfsPath> = loaded
        .applicable_files
        .iter()
        .flatten()
        .map(|path| VfsPath::from(path.clone()))
        .collect();
    paths.sort();
    paths
        .into_iter()
        .map(|path| db.file(file_map.intern(db, path)))
        .collect()
}

/// Clears the large fields of an app that is no longer loaded.
fn clear_app(db: &mut dyn RootQueryDb, app: App) {
    if !app.files(db).is_empty() {
        app.set_files(db).to(Arc::from([]));
    }
    if !app.macros(db).is_empty() {
        app.set_macros(db).to(Arc::from([]));
    }
    if !app.parse_transforms(db).is_empty() {
        app.set_parse_transforms(db).to(Arc::from([]));
    }
    if !app.include_dirs(db).is_empty() {
        app.set_include_dirs(db).to(Arc::from([]));
    }
    if !app.include_path(db).is_empty() {
        app.set_include_path(db).to(Arc::from([]));
    }
    if !app.src_dirs(db).is_empty() {
        app.set_src_dirs(db).to(Arc::from([]));
    }
    if !app.extra_src_dirs(db).is_empty() {
        app.set_extra_src_dirs(db).to(Arc::from([]));
    }
}

/// Sets `roots` as the source roots, numbered in order, and the source root of
/// each of their files.
pub fn set_source_roots(db: &mut dyn SourceDatabase, roots: Vec<SourceRoot>) {
    for (idx, root) in roots.into_iter().enumerate() {
        let root_id = SourceRootId(idx as u32);
        for file_id in root.iter() {
            db.set_file_source_root(file_id, root_id);
        }
        db.set_source_root(root_id, Arc::new(root));
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::cell::RefCell;

    use elp_project_model::AppName;
    use elp_project_model::AppType;
    use elp_project_model::ProjectAppData;
    use elp_project_model::ProjectBuildData;
    use elp_project_model::StaticProject;
    use elp_project_model::otp::Otp;
    use fxhash::FxHashSet;
    use vfs::AbsPathBuf;
    use vfs::FileId;
    use vfs::VfsPath;

    use super::lower;
    use crate::App;
    use crate::FileMap;
    use crate::IncludeOtp;
    use crate::Project;
    use crate::ProjectApps;
    use crate::RootQueryDb;
    use crate::SourceDatabase;
    use crate::Workspace;
    use crate::assert_eq_expected;
    use crate::fixture::WithFixture;
    use crate::test_db::TestDB;

    thread_local! {
        static RUNS: RefCell<Vec<&'static str>> = const { RefCell::new(Vec::new()) };
    }

    fn ran(query: &'static str) {
        RUNS.with(|runs| runs.borrow_mut().push(query));
    }

    /// The queries that ran during `f`.
    fn runs(f: impl FnOnce()) -> Vec<&'static str> {
        RUNS.with(|runs| runs.borrow_mut().clear());
        f();
        RUNS.with(|runs| runs.take())
    }

    /// Reads every input lowering sets.
    #[salsa::tracked]
    fn read_model(db: &dyn RootQueryDb, workspace: Workspace) {
        ran("read_model");
        for project in workspace.projects(db).iter() {
            project.root(db);
            project.otp(db);
            project.buck_index(db);
            project.code_path(db);
            project.eqwalizer_config(db);
            for source_root_id in project.source_roots(db).iter() {
                db.source_root_app(*source_root_id).app(db);
            }
            for app in project.apps(db).iter() {
                app.project(db);
                app.name(db);
                app.app_type(db);
                app.dir(db);
                app.files(db);
                app.macros(db);
                app.parse_transforms(db);
                app.ebin(db);
                app.docs_url(db);
                app.buck_target_name(db);
                app.include_dirs(db);
                app.include_path(db);
                app.src_dirs(db);
                app.extra_src_dirs(db);
                app.is_test_target(db);
            }
        }
    }

    #[derive(Default)]
    pub(crate) struct Model {
        pub(crate) db: TestDB,
        file_map: FileMap,
    }

    impl Model {
        pub(crate) fn lower(&mut self, projects: &[elp_project_model::Project]) {
            let project_apps = ProjectApps::new(projects, IncludeOtp::No);
            lower(&mut self.db, &self.file_map, &project_apps);
        }

        /// Lowers `projects` with an OTP project for each OTP installation.
        fn lower_with_otp(&mut self, projects: &[elp_project_model::Project]) {
            let project_apps = ProjectApps::new(projects, IncludeOtp::Yes);
            lower(&mut self.db, &self.file_map, &project_apps);
        }

        fn projects(&self) -> Vec<Project> {
            Workspace::get(&self.db).projects(&self.db).to_vec()
        }

        pub(crate) fn project(&self, idx: usize) -> Project {
            Workspace::get(&self.db).projects(&self.db)[idx]
        }

        fn apps(&self, idx: usize) -> Vec<App> {
            self.project(idx).apps(&self.db).to_vec()
        }

        pub(crate) fn app_named(&self, idx: usize, name: &str) -> App {
            self.apps(idx)
                .into_iter()
                .find(|app| app.name(&self.db).0 == name)
                .unwrap_or_else(|| panic!("project {idx} should have an app {name}"))
        }

        pub(crate) fn file_id(&self, path: &str) -> FileId {
            self.file_map
                .file_id(&VfsPath::new_real_path(path.to_string()))
                .unwrap_or_else(|| panic!("{path} should be interned"))
        }

        fn listing_apps(&self, path: &str) -> Vec<App> {
            let db = &self.db;
            let file = db.file(self.file_id(path));
            self.projects()
                .iter()
                .flat_map(|project| project.apps(db).iter())
                .filter(|app| app.files(db).contains(&file))
                .copied()
                .collect()
        }
    }

    pub(crate) fn path(path: &str) -> AbsPathBuf {
        AbsPathBuf::assert(path.into())
    }

    pub(crate) fn app(name: &str, dir: &str) -> ProjectAppData {
        ProjectAppData::fixture_app_data(
            AppName(name.to_string()),
            path(dir),
            vec![],
            vec![path(&format!("{dir}/src"))],
            vec![],
        )
    }

    pub(crate) fn listing(mut app: ProjectAppData, files: &[&str]) -> ProjectAppData {
        app.applicable_files = Some(FxHashSet::from_iter(files.iter().map(|file| path(file))));
        app
    }

    fn with_macro(mut app: ProjectAppData, name: &str) -> ProjectAppData {
        app.macros.push(eetf::Atom::from(name).into());
        app
    }

    pub(crate) fn target(mut app: ProjectAppData, label: &str) -> ProjectAppData {
        app.buck_target_name = Some(label.to_string());
        app
    }

    /// A JSON project at `root`.
    pub(crate) fn json_project(
        root: &str,
        apps: Vec<ProjectAppData>,
    ) -> elp_project_model::Project {
        let mut project = elp_project_model::Project::empty(Otp {
            lib_dir: path("/otp"),
        });
        project.project_build_data = ProjectBuildData::Static(StaticProject {
            config_path: path(&format!("{root}/build_info.json")),
        });
        project.add_apps(apps);
        project
    }

    /// A JSON project at `root` on the OTP installation at `lib_dir`, which
    /// gives it the OTP apps `otp_apps`.
    fn json_project_on_otp(
        root: &str,
        lib_dir: &str,
        apps: Vec<ProjectAppData>,
        otp_apps: &[&str],
    ) -> elp_project_model::Project {
        let mut project = json_project(root, apps);
        project.otp = Otp {
            lib_dir: path(lib_dir),
        };
        project.otp_apps = otp_apps
            .iter()
            .map(|name| {
                let dir = path(&format!("{lib_dir}/{name}-1.0"));
                ProjectAppData::otp_app_data(&format!("{name}-1.0"), &dir)
            })
            .collect();
        project
    }

    #[test]
    fn an_identical_lowering_sets_nothing() {
        let projects = [json_project(
            "/p",
            vec![
                listing(with_macro(app("a", "/p/a"), "A"), &["/p/a/src/a.erl"]),
                app("b", "/p/b"),
            ],
        )];
        let mut model = Model::default();
        model.lower(&projects);
        let workspace = Workspace::get(&model.db);
        assert_eq_expected!(
            vec!["read_model"],
            runs(|| read_model(&model.db, workspace))
        );

        model.lower(&projects);
        assert_eq_expected!(
            Vec::<&str>::new(),
            runs(|| read_model(&model.db, workspace)),
            "after an identical lowering"
        );
    }

    #[test]
    fn a_removed_app_is_cleared_and_returns_with_its_input() {
        let with_b = [json_project(
            "/p",
            vec![
                app("a", "/p/a"),
                listing(with_macro(app("b", "/p/b"), "B"), &["/p/b/src/b.erl"]),
            ],
        )];
        let mut model = Model::default();
        model.lower(&with_b);
        let [a, b] = [model.app_named(0, "a"), model.app_named(0, "b")];

        model.lower(&[json_project("/p", vec![app("a", "/p/a")])]);
        assert_eq_expected!(vec![a], model.apps(0));
        let db = &model.db;
        assert!(
            b.files(db).is_empty(),
            "the removed app's files are cleared"
        );
        assert!(
            b.macros(db).is_empty(),
            "the removed app's macros are cleared"
        );
        assert!(
            b.src_dirs(db).is_empty(),
            "the removed app's source dirs are cleared"
        );
        assert_eq_expected!(Vec::<App>::new(), model.listing_apps("/p/b/src/b.erl"));

        model.lower(&with_b);
        assert_eq_expected!(vec![a, b], model.apps(0), "the app returns with its input");
        assert_eq_expected!(1, b.macros(&model.db).len());
        assert_eq_expected!(vec![b], model.listing_apps("/p/b/src/b.erl"));
    }

    #[test]
    fn apps_sharing_a_directory_stay_distinct() {
        let mut model = Model::default();
        model.lower(&[json_project(
            "/p",
            vec![app("mock", "/p/mock"), app("mock_handler_only", "/p/mock")],
        )]);
        let mock = model.app_named(0, "mock");
        let handler_only = model.app_named(0, "mock_handler_only");
        assert_ne!(mock, handler_only, "each app should have its own input");

        model.lower(&[json_project(
            "/p",
            vec![app("mock_handler_only", "/p/mock"), app("mock", "/p/mock")],
        )]);
        assert_eq!(
            mock,
            model.app_named(0, "mock"),
            "mock should keep its input after reordering"
        );
        assert_eq!(
            handler_only,
            model.app_named(0, "mock_handler_only"),
            "mock_handler_only should keep its input after reordering"
        );
    }

    #[test]
    fn apps_sharing_a_key_keep_their_inputs_in_order() {
        let projects = [json_project(
            "/p",
            vec![
                with_macro(app("a", "/p/a"), "FIRST"),
                with_macro(app("a", "/p/a"), "SECOND"),
            ],
        )];
        let mut model = Model::default();
        model.lower(&projects);
        let workspace = Workspace::get(&model.db);
        runs(|| read_model(&model.db, workspace));

        model.lower(&projects);
        assert_eq_expected!(
            Vec::<&str>::new(),
            runs(|| read_model(&model.db, workspace)),
            "each app should keep its input"
        );
    }

    #[test]
    fn a_target_in_overlapping_projects_is_an_app_of_each() {
        let projects = ["/p1", "/p2"]
            .map(|root| json_project(root, vec![target(app("a", "/cell/a"), "cell//a:a")]));
        let mut model = Model::default();
        model.lower(&projects);
        let [in_p1, in_p2] = [model.apps(0), model.apps(1)];
        assert_ne!(in_p1, in_p2, "each project should have its own app");
        assert_eq!(
            model.project(0),
            in_p1[0].project(&model.db),
            "the first project's app should be in the first project"
        );
        assert_eq!(
            model.project(1),
            in_p2[0].project(&model.db),
            "the second project's app should be in the second project"
        );

        model.lower(&projects);
        assert_eq!(in_p1, model.apps(0), "the apps should keep their inputs");
        assert_eq!(in_p2, model.apps(1), "the apps should keep their inputs");
    }

    #[test]
    fn a_generated_input_goes_to_the_first_project_listing_it() {
        let generating = |root: &str| {
            let mut app = target(app("a", "/cell/a"), "cell//a:a");
            app.gen_src_files = Some(FxHashSet::from_iter([path("/cell/a/a.thrift")]));
            json_project(root, vec![app])
        };
        let mut model = Model::default();
        let projects = [generating("/p2"), generating("/p1")];
        let gen_src_apps = lower(
            &mut model.db,
            &model.file_map,
            &ProjectApps::new(&projects, IncludeOtp::No),
        );

        let owner = gen_src_apps[&path("/cell/a/a.thrift")];
        assert_eq!(
            model.project(0),
            owner.project(&model.db),
            "the generated input should go to the first project"
        );
    }

    #[test]
    fn apps_are_sorted_by_key_and_files_by_path() {
        let mut model = Model::default();
        model.lower(&[json_project(
            "/p",
            vec![
                listing(app("b", "/p/b"), &["/p/b/src/z.erl", "/p/b/src/a.erl"]),
                app("a", "/p/a"),
            ],
        )]);
        let db = &model.db;
        let names: Vec<String> = model
            .apps(0)
            .iter()
            .map(|app| app.name(db).0.clone())
            .collect();
        assert_eq_expected!(vec!["a".to_string(), "b".to_string()], names);
        let b = model.app_named(0, "b");
        let files: Vec<String> = b
            .files(db)
            .iter()
            .map(|file| file.path(db).to_string())
            .collect();
        let expected_files = vec!["/p/b/src/a.erl".to_string(), "/p/b/src/z.erl".to_string()];
        assert_eq_expected!(expected_files, files);
    }

    #[test]
    fn a_project_keeps_its_input_when_another_is_added_before_it() {
        let p1 = json_project("/p1", vec![app("a", "/p1/a")]);
        let p0 = json_project("/p0", vec![app("b", "/p0/b")]);
        let mut model = Model::default();
        model.lower(std::slice::from_ref(&p1));
        let expected_p1 = model.projects()[0];

        model.lower(&[p0, p1]);
        assert_eq_expected!(expected_p1, model.projects()[1]);
    }

    #[test]
    fn a_loaded_project_keeps_its_input_over_a_removed_one_with_its_root() {
        let project = json_project("/p", vec![app("a", "/p/a")]);
        let mut model = Model::default();
        model.lower(&[project.clone(), project.clone()]);
        let expected = model.projects()[0];

        model.lower(std::slice::from_ref(&project));
        model.lower(std::slice::from_ref(&project));
        assert_eq_expected!(vec![expected], model.projects());
    }

    #[test]
    fn a_removed_project_is_cleared_and_returns_with_its_input() {
        let both = [
            json_project("/p1", vec![app("a", "/p1/a")]),
            json_project("/p2", vec![app("b", "/p2/b")]),
        ];
        let mut model = Model::default();
        model.lower(&both);
        let [p1, p2] = [model.projects()[0], model.projects()[1]];
        let b = model.app_named(1, "b");

        model.lower(&both[..1]);
        assert_eq_expected!(vec![p1], model.projects());
        assert!(
            p2.source_roots(&model.db).is_empty(),
            "the removed project's source roots are cleared"
        );
        assert!(
            p2.apps(&model.db).is_empty(),
            "the removed project's apps are removed"
        );
        assert!(
            b.src_dirs(&model.db).is_empty(),
            "the removed project's apps are cleared"
        );

        model.lower(&both);
        assert_eq_expected!(
            vec![p1, p2],
            model.projects(),
            "the project returns with its input"
        );
        assert_eq_expected!(1, p2.source_roots(&model.db).len());
        assert_eq_expected!(vec![b], model.apps(1), "its app returns with its input");
    }

    #[test]
    fn app_named_finds_the_last_app_with_the_name() {
        let mut model = Model::default();
        model.lower(&[json_project(
            "/p",
            vec![app("a", "/p/a1"), app("b", "/p/b"), app("a", "/p/a2")],
        )]);
        let project = model.projects()[0];
        let db = &model.db;
        let dir = |name: &str| {
            Some(
                project
                    .app_named(db, AppName(name.to_string()))?
                    .dir(db)
                    .to_string(),
            )
        };
        assert_eq_expected!(Some("/p/a2".to_string()), dir("a"), "the last app named a");
        assert_eq_expected!(Some("/p/b".to_string()), dir("b"));
        assert_eq_expected!(None, dir("c"), "no app is named c");
    }

    #[test]
    fn app_named_falls_back_to_the_otp_app() {
        let (db, _) = TestDB::with_fixture(
            r#"
//- /src/a.erl
-module(a).
//- /opt/lib/comp-1.3/src/comp.erl otp_app:/opt/lib/comp-1.3
-module(comp).
"#,
        );
        let project = Workspace::get(&db).projects(&db)[0];
        let app = project
            .app_named(&db, AppName("comp".to_string()))
            .expect("comp should be found in the OTP project");
        assert_eq_expected!(AppType::Otp, app.app_type(&db));
    }

    #[test]
    fn app_named_prefers_the_projects_own_app_to_an_otp_app() {
        let mut model = Model::default();
        model.lower_with_otp(&[json_project_on_otp(
            "/p",
            "/otp",
            vec![app("ssl", "/p/ssl")],
            &["ssl"],
        )]);
        let db = &model.db;
        let app_type = |idx: usize| {
            let app = model
                .project(idx)
                .app_named(db, AppName("ssl".to_string()))?;
            Some(app.app_type(db))
        };
        assert_eq_expected!(Some(AppType::Project), app_type(0), "in the project");
        assert_eq_expected!(Some(AppType::Otp), app_type(1), "in the OTP project");
    }

    #[test]
    fn projects_on_one_otp_installation_share_its_otp_project() {
        let mut model = Model::default();
        model.lower_with_otp(
            &["/p1", "/p2"]
                .map(|root| json_project_on_otp(root, "/otp", vec![app("a", root)], &["kernel"])),
        );
        let db = &model.db;
        let expected_otp = Some(model.project(2));
        let kernel = model.app_named(2, "kernel");
        for idx in 0..3 {
            assert_eq_expected!(expected_otp, model.project(idx).otp(db), "in project {idx}");
        }
        for idx in 0..2 {
            let names: Vec<&str> = model
                .apps(idx)
                .iter()
                .map(|app| app.name(db).as_str())
                .collect();
            assert_eq_expected!(vec!["a", "kernel"], names, "in project {idx}");
            assert_eq!(
                kernel,
                model.app_named(idx, "kernel"),
                "project {idx} should list the OTP project's app"
            );
        }
    }

    #[test]
    fn projects_on_different_otp_installations_get_their_own() {
        let mut model = Model::default();
        model.lower_with_otp(&[
            json_project_on_otp("/p1", "/otp1", vec![app("a", "/p1")], &["kernel"]),
            json_project_on_otp("/p2", "/otp2", vec![app("b", "/p2")], &["kernel"]),
        ]);
        let db = &model.db;
        let expected_otps = [Some(model.project(2)), Some(model.project(3))];
        assert_eq_expected!(
            expected_otps,
            [model.project(0).otp(db), model.project(1).otp(db)]
        );
        let kernel_dir = |idx: usize| model.app_named(idx, "kernel").dir(db).to_string();
        assert_eq_expected!("/otp1/kernel-1.0", kernel_dir(0));
        assert_eq_expected!("/otp2/kernel-1.0", kernel_dir(1));
    }

    #[test]
    fn an_identical_lowering_with_otp_projects_sets_nothing() {
        let projects = ["/p1", "/p2"].map(|root| {
            json_project_on_otp(root, "/otp", vec![app("a", root)], &["kernel", "stdlib"])
        });
        let mut model = Model::default();
        model.lower_with_otp(&projects);
        let workspace = Workspace::get(&model.db);
        runs(|| read_model(&model.db, workspace));

        model.lower_with_otp(&projects);
        assert_eq_expected!(
            Vec::<&str>::new(),
            runs(|| read_model(&model.db, workspace)),
            "after an identical lowering"
        );
    }

    #[test]
    fn only_otp_apps_keep_their_ebin() {
        let mut dep = app("dep", "/p/dep");
        dep.app_type = AppType::ThirdParty;
        dep.ebin = Some(path("/p/dep/ebin"));
        let mut model = Model::default();
        model.lower_with_otp(&[json_project_on_otp("/p", "/otp", vec![dep], &["kernel"])]);

        let db = &model.db;
        let ebin = |name: &str| model.app_named(0, name).ebin(db).clone();
        assert_eq_expected!(None::<AbsPathBuf>, ebin("dep"), "a third-party app's ebin");
        assert_eq_expected!(Some(path("/otp/kernel-1.0/ebin")), ebin("kernel"));
        assert_eq_expected!(
            vec![path("/p/dep/ebin")],
            model.project(0).code_path(db).to_vec(),
            "a third-party app's ebin goes on the code path"
        );
    }

    #[test]
    fn a_removed_project_leaves_the_otp_apps_it_listed() {
        let projects = ["/p1", "/p2"]
            .map(|root| json_project_on_otp(root, "/otp", vec![app("a", root)], &["kernel"]));
        let mut model = Model::default();
        model.lower_with_otp(&projects);
        model.lower_with_otp(&projects[1..]);

        let kernel = model.app_named(0, "kernel");
        assert!(
            !kernel.src_dirs(&model.db).is_empty(),
            "the OTP app should keep its fields"
        );
    }
}
