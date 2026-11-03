/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Which apps own which files.

use std::sync::Arc;

use fxhash::FxHashMap;
use smallvec::SmallVec;
use vfs::FileId;
use vfs::VfsPath;

use crate::App;
use crate::AppType;
use crate::File;
use crate::FileSource;
use crate::Project;
use crate::RootQueryDb;
use crate::Workspace;

/// The apps of `project` that list each file, the primary owner first.
#[salsa::tracked]
fn project_ownership(
    db: &dyn RootQueryDb,
    project: Project,
) -> Arc<FxHashMap<File, SmallVec<[App; 1]>>> {
    let mut owners: FxHashMap<File, SmallVec<[App; 1]>> = FxHashMap::default();
    for &app in project.apps(db).iter() {
        for &file in app.files(db).iter() {
            owners.entry(file).or_default().push(app);
        }
    }
    for (file, apps) in &mut owners {
        if apps.len() > 1 {
            sort_owners(db, file.path(db), apps);
        }
    }
    Arc::new(owners)
}

/// Sorts the apps that list the file at `path`, the primary owner first: the
/// app whose directory contains it most closely, then one that doesn't list it
/// as an extra source, then a project app over a dependency, then by name. The
/// sort is stable, so apps that tie keep their order in `Project::apps`.
fn sort_owners(db: &dyn RootQueryDb, path: &VfsPath, apps: &mut [App]) {
    apps.sort_by_cached_key(|&app| {
        let depth = path
            .strip_prefix(app.dir(db))
            .map_or(usize::MAX, |rel| rel.as_utf8_path().components().count());
        let extra = app.file_source(db, path) == Some(FileSource::Extra);
        let dependency = app.app_type(db) != AppType::App;
        (depth, extra, dependency, app.name(db).0.clone())
    });
}

pub(crate) fn file_apps(
    db: &dyn RootQueryDb,
    project: Project,
    file_id: FileId,
) -> SmallVec<[App; 1]> {
    project_ownership(db, project)
        .get(&db.file(file_id))
        .cloned()
        .unwrap_or_default()
}

pub(crate) fn file_app(db: &dyn RootQueryDb, file_id: FileId) -> Option<App> {
    Workspace::try_get(db)
        .and_then(|workspace| {
            workspace
                .projects(db)
                .iter()
                .find_map(|&project| db.file_apps(project, file_id).first().copied())
        })
        .or_else(|| {
            let source_root_id = db.file_source_root(file_id).source_root_id(db);
            db.source_root_app(source_root_id).app(db)
        })
}

#[cfg(test)]
mod tests {
    use elp_project_model::ProjectAppData;

    use crate::AppType;
    use crate::RootQueryDb;
    use crate::assert_eq_expected;
    use crate::lower::tests::Model;
    use crate::lower::tests::app;
    use crate::lower::tests::json_project;
    use crate::lower::tests::listing;
    use crate::lower::tests::target;

    fn dependency(mut app: ProjectAppData) -> ProjectAppData {
        app.app_type = AppType::Dep;
        app
    }

    /// A Buck test target, which lists its suite as an extra source.
    fn test_target(app: ProjectAppData, label: &str) -> ProjectAppData {
        let mut app = target(app, label);
        app.abs_src_dirs.clear();
        app.is_test_target = Some(true);
        app
    }

    /// The owners of the file at `path` in the project at `idx`.
    fn owners(model: &Model, idx: usize, path: &str) -> Vec<String> {
        let db = &model.db;
        db.file_apps(model.project(idx), model.file_id(path))
            .iter()
            .map(|app| app.name(db).0.clone())
            .collect()
    }

    fn primary_owner(model: &Model, path: &str) -> Option<String> {
        let db = &model.db;
        let app = db.file_app(model.file_id(path))?;
        Some(app.name(db).0.clone())
    }

    #[test]
    fn the_app_with_the_closest_directory_is_the_primary_owner() {
        let file = "/p/lib/src/x.erl";
        let mut model = Model::default();
        model.lower(&[json_project(
            "/p",
            vec![
                listing(app("elsewhere", "/q"), &[file]),
                listing(app("outer", "/p"), &[file]),
                listing(app("inner", "/p/lib"), &[file]),
            ],
        )]);

        assert_eq_expected!(vec!["inner", "outer", "elsewhere"], owners(&model, 0, file));
        assert_eq_expected!(Some("inner".to_string()), primary_owner(&model, file));
    }

    #[test]
    fn the_first_project_listing_a_file_has_its_primary_owner() {
        let file = "/cell/a/src/x.erl";
        let mut model = Model::default();
        // The second project's app is closer to the file.
        model.lower(&[
            json_project("/p1", vec![listing(app("outer", "/cell"), &[file])]),
            json_project("/p2", vec![listing(app("inner", "/cell/a"), &[file])]),
        ]);

        assert_eq_expected!(Some("outer".to_string()), primary_owner(&model, file));
        assert_eq_expected!(vec!["outer"], owners(&model, 0, file));
        assert_eq_expected!(vec!["inner"], owners(&model, 1, file));
    }

    #[test]
    fn a_tie_goes_to_a_source_then_by_name() {
        let file = "/p/a/src/a_SUITE.erl";
        let mut model = Model::default();
        // All three apps have the same directory. The test target lists the
        // file as an extra source; the names of the others sort the other way
        // from their targets.
        model.lower(&[json_project(
            "/p",
            vec![
                listing(
                    test_target(app("a_test", "/p/a"), "cell//a:a_test"),
                    &[file],
                ),
                listing(target(app("c", "/p/a"), "cell//a:a"), &[file]),
                listing(target(app("b", "/p/a"), "cell//a:z"), &[file]),
            ],
        )]);

        assert_eq_expected!(vec!["b", "c", "a_test"], owners(&model, 0, file));
    }

    #[test]
    fn a_tie_goes_to_a_project_app_before_a_dependency() {
        let file = "/cell/a/src/x.erl";
        let mut model = Model::default();
        // The dependencies' names sort first.
        model.lower(&[json_project(
            "/p",
            vec![
                listing(dependency(app("a_dep", "/cell/a")), &[file]),
                listing(app("c_app", "/cell/a"), &[file]),
                listing(dependency(app("b_dep", "/cell/a")), &[file]),
            ],
        )]);

        assert_eq_expected!(vec!["c_app", "a_dep", "b_dep"], owners(&model, 0, file));
    }

    #[test]
    fn a_tie_between_namesakes_goes_by_key() {
        let file = "/cell/a/src/x.erl";
        let namesake = |label| listing(target(app("a", "/cell/a"), label), &[file]);
        let mut model = Model::default();
        // The apps come out of the loader in the reverse of their keys' order.
        model.lower(&[json_project(
            "/p",
            vec![namesake("cell//a:z"), namesake("cell//a:b")],
        )]);

        let db = &model.db;
        let targets: Vec<String> = db
            .file_apps(model.project(0), model.file_id(file))
            .iter()
            .filter_map(|app| app.buck_target_name(db).clone())
            .collect();
        assert_eq_expected!(vec!["cell//a:b", "cell//a:z"], targets);
    }
}
