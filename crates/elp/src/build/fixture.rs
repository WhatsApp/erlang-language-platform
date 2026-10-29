/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide::AnalysisHost;
use elp_ide::elp_ide_db::RootDatabase;
use elp_ide::elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide::elp_ide_db::elp_base_db::fixture::Fixture;

use super::types::LoadResult;

/// Creates analysis from a multi-file fixture
#[track_caller]
pub fn load_result(fixture_str: &str) -> LoadResult {
    let mut db = RootDatabase::default();
    let (_fixture, file_map, project) = Fixture::load(&mut db, fixture_str);

    LoadResult::new(
        AnalysisHost::new(db),
        file_map,
        project,
        FileSetConfig::default(),
    )
}

#[cfg(test)]
mod tests {
    use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
    use elp_ide::elp_ide_db::elp_base_db::AppData;
    use elp_ide::elp_ide_db::elp_base_db::RootQueryDb;
    use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
    use elp_ide::elp_ide_db::elp_base_db::VfsPath;
    use elp_ide::elp_ide_db::elp_base_db::Workspace;
    use elp_project_model::AppName;
    use elp_project_model::Project;
    use elp_project_model::ProjectAppData;
    use elp_project_model::buck::BuckQueryConfig;
    use elp_project_model::otp::Otp;
    use elp_project_model::rebar::RebarProject;
    use fxhash::FxHashSet;

    use super::load_result;
    use crate::build::load::LoadConfig;
    use crate::build::load::load_project;
    use crate::build::types::LoadResult;
    use crate::cli::Fake;

    /// The apps of the project and of the OTP project, and each file's app
    /// and the app of its source root.
    type Lowered = (
        [Vec<AppData>; 2],
        Vec<(AbsPathBuf, Option<AppName>, Option<AppName>)>,
    );

    fn lowered(loaded: &LoadResult, paths: &[AbsPathBuf]) -> Lowered {
        let db = loaded.analysis_host.raw_database();
        let projects = Workspace::get(db).projects(db);
        let apps = [projects[0], projects[1]].map(|project| {
            let mut apps: Vec<AppData> = project
                .source_roots(db)
                .iter()
                .filter_map(|root| db.app_data(*root))
                .map(|app| {
                    let mut app = (*app).clone();
                    // The rebar loader collects the apps' parent directories
                    // in a hash set, so their order depends on the apps'.
                    app.include_path.sort();
                    app
                })
                .collect();
            apps.sort_by(|a, b| a.name.0.cmp(&b.name.0));
            apps
        });
        let files = paths
            .iter()
            .map(|path| {
                let file_id = loaded
                    .file_id(&VfsPath::from(path.clone()))
                    .unwrap_or_else(|| panic!("{path} should be loaded"));
                let app = db.file_app_data(file_id).map(|app| app.name.clone());
                let root = db.file_source_root(file_id).source_root_id(db);
                let root_app = db.app_data(root).map(|app| app.name.clone());
                (path.clone(), app, root_app)
            })
            .collect();
        (apps, files)
    }

    /// A fixture is lowered as the CLI lowers the same apps loaded from disk.
    #[test]
    fn a_fixture_lowers_as_the_cli_loader_does() {
        let dir = tempfile::tempdir().unwrap();
        let root = AbsPathBuf::assert_utf8(dunce::canonicalize(dir.path()).unwrap());
        let files = [
            ("src/top.erl", String::new(), "-module(top)."),
            ("app_a/src/a.erl", "app:app_a".to_string(), "-module(a)."),
            (
                "app_a/include/a.hrl",
                format!("app:app_a include_path:{root}/app_a/include"),
                "-define(A, a).",
            ),
            (
                "app_b/src/b.erl",
                "app:app_b buck_target:cell//app_b:app_b".to_string(),
                "-module(b).",
            ),
            (
                "otp/lib/comp-1.0/src/comp.erl",
                format!("otp_app:{root}/otp/lib/comp-1.0"),
                "-module(comp).",
            ),
        ];
        let paths = files.clone().map(|(path, _, _)| root.join(path));
        let fixture: String = files
            .iter()
            .map(|(path, meta, text)| format!("//- {root}/{path} {meta}\n{text}\n"))
            .collect();
        for (path, _, text) in &files {
            let path = root.join(path);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        }

        let otp = Otp {
            lib_dir: root.join("otp/lib"),
        };
        let app = |name: &str, dir: AbsPathBuf, include_dirs: Vec<AbsPathBuf>| {
            let src_dirs = vec![dir.join("src")];
            ProjectAppData::fixture_app_data(
                AppName(name.to_string()),
                dir,
                include_dirs,
                src_dirs,
                vec![],
            )
        };
        let mut app_b = app("app_b", root.join("app_b"), vec![]);
        app_b.buck_target_name = Some("cell//app_b:app_b".to_string());
        app_b.applicable_files = Some(FxHashSet::from_iter([paths[3].clone()]));
        let apps = vec![
            app("test-fixture", root.clone(), vec![]),
            app(
                "app_a",
                root.join("app_a"),
                vec![root.join("app_a/include")],
            ),
            app_b,
        ];
        let mut project = Project::empty(otp.clone());
        project.add_apps(vec![ProjectAppData::otp_app_data(
            "comp-1.0",
            &root.join("otp/lib/comp-1.0"),
        )]);
        project.add_apps(RebarProject::add_app_includes(apps, &[], &otp.lib_dir));
        let loaded = load_project(
            &Fake::default(),
            project,
            LoadConfig::new(
                elp_eqwalizer::Mode::Cli,
                BuckQueryConfig::BuildGeneratedCode,
            ),
        )
        .expect("the project should load");

        assert_eq!(
            lowered(&loaded, &paths),
            lowered(&load_result(&fixture), &paths),
            "the fixture should be lowered as the CLI lowers the same apps"
        );
    }
}
