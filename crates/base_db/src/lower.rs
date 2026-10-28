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

use fxhash::FxHashMap;
use salsa::Setter;
use vfs::AbsPathBuf;
use vfs::VfsPath;

use crate::AppDataId;
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
/// those inputs in order. Salsa never frees an input: a project that disappears
/// moves to `Workspace::removed_projects` with its large fields cleared, and
/// gets its input back if it returns. Every file an app lists gets a `FileId`
/// from `file_map`, whether or not it has loaded. Returns the app of each
/// generated source file.
pub fn lower(
    db: &mut dyn RootQueryDb,
    file_map: &FileMap,
    project_apps: &ProjectApps,
) -> FxHashMap<AbsPathBuf, AppDataId> {
    let mut known: Vec<Project> = Workspace::try_get(db)
        .map(|workspace| {
            let removed = workspace.removed_projects(db).iter();
            workspace
                .projects(db)
                .iter()
                .chain(removed)
                .copied()
                .collect()
        })
        .unwrap_or_default();

    let mut source_roots: Vec<Vec<SourceRootId>> = vec![Vec::new(); project_apps.projects.len()];
    for (idx, (project_id, _)) in project_apps.all_apps.iter().enumerate() {
        source_roots[project_id.0 as usize].push(SourceRootId(idx as u32));
    }
    let mut projects = Vec::with_capacity(project_apps.projects.len());
    for (loaded, source_roots) in project_apps.projects.iter().zip(source_roots) {
        let root = VfsPath::from(loaded.root().into_owned());
        let found = known.iter().position(|project| *project.root(db) == root);
        let known_project = found.map(|idx| known.remove(idx));
        projects.push(lower_project(
            db,
            loaded,
            root,
            source_roots.into(),
            known_project,
        ));
    }
    let otp = project_apps
        .otp_project_id
        .map(|otp_project_id| projects[otp_project_id.0 as usize]);
    for &project in &projects {
        if project.otp(db) != otp {
            project.set_otp(db).to(otp);
        }
    }

    // Whatever is left is no longer loaded.
    for &project in &known {
        if project.buck_index(db).is_some() {
            project.set_buck_index(db).to(None);
        }
        if !project.source_roots(db).is_empty() {
            project.set_source_roots(db).to(Arc::from([]));
        }
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

    project_apps.app_structure().apply(db, file_map)
}

/// The input for `loaded`: `known`, with the fields that changed set, else a
/// new one. Its OTP project is set once every project has an input.
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
mod tests {
    use std::cell::RefCell;

    use elp_project_model::AppName;
    use elp_project_model::AppType;
    use elp_project_model::EqwalizerConfig;
    use elp_project_model::ProjectAppData;
    use elp_project_model::ProjectBuildData;
    use elp_project_model::StaticProject;
    use elp_project_model::otp::Otp;
    use vfs::AbsPathBuf;

    use super::lower;
    use crate::FileMap;
    use crate::IncludeOtp;
    use crate::Project;
    use crate::ProjectApps;
    use crate::RootQueryDb;
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

    /// Reads every project input lowering sets.
    #[salsa::tracked]
    fn read_projects(db: &dyn RootQueryDb, workspace: Workspace) {
        ran("read_projects");
        for project in workspace.projects(db).iter() {
            project.root(db);
            project.otp(db);
            project.buck_index(db);
            project.code_path(db);
            project.eqwalizer_config(db);
            project.source_roots(db);
        }
    }

    #[salsa::tracked]
    fn project_root(db: &dyn RootQueryDb, project: Project) -> String {
        ran("project_root");
        project.root(db).to_string()
    }

    #[salsa::tracked]
    fn project_eqwalizer_config(db: &dyn RootQueryDb, project: Project) -> bool {
        ran("project_eqwalizer_config");
        project.eqwalizer_config(db).enable_all
    }

    #[derive(Default)]
    struct Model {
        db: TestDB,
        file_map: FileMap,
    }

    impl Model {
        fn lower(&mut self, projects: &[elp_project_model::Project]) {
            let project_apps = ProjectApps::new(projects, IncludeOtp::No);
            lower(&mut self.db, &self.file_map, &project_apps);
        }

        fn projects(&self) -> Vec<Project> {
            Workspace::get(&self.db).projects(&self.db).to_vec()
        }
    }

    fn path(path: &str) -> AbsPathBuf {
        AbsPathBuf::assert(path.into())
    }

    fn app(name: &str, dir: &str) -> ProjectAppData {
        ProjectAppData::fixture_app_data(
            AppName(name.to_string()),
            path(dir),
            vec![],
            vec![path(&format!("{dir}/src"))],
            vec![],
        )
    }

    fn json_project(root: &str, apps: Vec<ProjectAppData>) -> elp_project_model::Project {
        let mut project = elp_project_model::Project::empty(Otp {
            lib_dir: path("/otp"),
        });
        project.project_build_data = ProjectBuildData::Static(StaticProject {
            config_path: path(&format!("{root}/build_info.json")),
        });
        project.add_apps(apps);
        project
    }

    #[test]
    fn an_identical_lowering_sets_no_project_field() {
        let projects = [
            json_project("/p1", vec![app("a", "/p1/a")]),
            json_project("/p2", vec![app("b", "/p2/b")]),
        ];
        let mut model = Model::default();
        model.lower(&projects);
        let workspace = Workspace::get(&model.db);
        assert_eq_expected!(
            vec!["read_projects"],
            runs(|| read_projects(&model.db, workspace))
        );

        model.lower(&projects);
        assert_eq_expected!(
            Vec::<&str>::new(),
            runs(|| read_projects(&model.db, workspace)),
            "after an identical lowering"
        );
    }

    #[test]
    fn a_changed_project_field_sets_only_that_field() {
        let project = |root: &str, enable_all: bool| {
            let mut project = json_project(root, vec![app("a", &format!("{root}/a"))]);
            project.eqwalizer_config = EqwalizerConfig {
                enable_all,
                ..EqwalizerConfig::default()
            };
            project
        };
        let mut model = Model::default();
        model.lower(&[project("/p1", false), project("/p2", false)]);
        let [p1, p2] = [model.projects()[0], model.projects()[1]];
        let db = &model.db;
        runs(|| {
            project_root(db, p1);
            project_eqwalizer_config(db, p1);
            project_eqwalizer_config(db, p2);
        });

        model.lower(&[project("/p1", true), project("/p2", false)]);
        let db = &model.db;
        let p1_config = runs(|| {
            project_eqwalizer_config(db, p1);
        });
        assert_eq_expected!(
            vec!["project_eqwalizer_config"],
            p1_config,
            "p1's eqWAlizer config changed"
        );
        let p2_config = runs(|| {
            project_eqwalizer_config(db, p2);
        });
        assert_eq_expected!(Vec::<&str>::new(), p2_config, "p2's didn't");
        let p1_root = runs(|| {
            project_root(db, p1);
        });
        assert_eq_expected!(Vec::<&str>::new(), p1_root, "p1's root didn't");
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
            let source_root_id = project.app_named(db, AppName(name.to_string()))?;
            Some(db.app_data(source_root_id)?.dir.to_string())
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
        let source_root_id = project
            .app_named(&db, AppName("comp".to_string()))
            .expect("comp should be found in the OTP project");
        let app_data = db
            .app_data(source_root_id)
            .expect("the source root should have an app");
        assert_eq_expected!(AppType::Otp, app_data.app_type);
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

        model.lower(&both[..1]);
        assert_eq_expected!(vec![p1], model.projects());
        assert!(
            p2.source_roots(&model.db).is_empty(),
            "the removed project's source roots are cleared"
        );

        model.lower(&both);
        assert_eq_expected!(
            vec![p1, p2],
            model.projects(),
            "the project returns with its input"
        );
        assert_eq_expected!(1, p2.source_roots(&model.db).len());
    }
}
