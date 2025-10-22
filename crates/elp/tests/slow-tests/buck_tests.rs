/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;

    use elp::build::load;
    use elp::cli::Fake;
    use elp_ide::elp_ide_db::elp_base_db::AbsPath;
    use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
    use elp_project_model::AppType;
    use elp_project_model::DiscoverConfig;
    use elp_project_model::Project;
    use elp_project_model::ProjectAppData;
    use elp_project_model::ProjectManifest;
    use elp_project_model::buck::BuckQueryConfig;
    use elp_project_model::to_abs_path_buf;
    use itertools::Itertools;

    const BUCK_QUERY_CONFIG: BuckQueryConfig = BuckQueryConfig::BuildGeneratedCode;

    #[test]
    #[ignore]
    fn test_success_case() {
        let path_str = "../../test_projects/buck_tests";
        let path: PathBuf = path_str.into();
        let cli = Fake::default();

        let conf = DiscoverConfig::buck();
        let result = load::load_project_at(
            &cli,
            &path,
            conf,
            IncludeOtp::No,
            elp_eqwalizer::Mode::Cli,
            &BUCK_QUERY_CONFIG,
        )
        .unwrap_or_else(|_| panic!("Can't load project from path {}", &path.to_string_lossy()));
        let analysis = &result.analysis();
        let modules = vec![
            ("test_elp", true),
            ("test_elp_SUITE", false),
            ("test_elp_direct_dep", true),
            ("test_elp_transitive_dep", true),
        ];
        let project_id = result.project_id;
        for (module, eqwalizer_enabled) in modules {
            let file_id = analysis.module_file_id(project_id, module).unwrap();
            let file_id = file_id.unwrap_or_else(|| panic!("Can't find file id for {module}"));
            analysis
                .file_text(file_id)
                .unwrap_or_else(|_| panic!("No file text for {module}"));
            let prj_id = analysis.project_id(file_id).unwrap();
            let prj_id = prj_id.unwrap_or_else(|| panic!("Can't find project id for {module}"));
            assert_eq!(prj_id, project_id);
            let ast = analysis.module_ast(file_id).unwrap();
            assert_eq!(ast.errors, vec![]);
            let eq_enabled = analysis
                .is_eqwalizer_enabled(file_id, false)
                .unwrap_or_else(|_| panic!("Failed to check if eqwalizer enabled for {module}"));
            assert_eq!(eq_enabled, eqwalizer_enabled);
            let project_data = analysis.project_data(file_id).unwrap();
            project_data.unwrap_or_else(|| panic!("Can't find project data for {module}"));
        }
    }

    #[test]
    #[ignore]
    fn test_load_buck_targets() {
        let path_str = "../../test_projects/buck_tests";
        let path: PathBuf = path_str.into();

        let (elp_config, buck_config) =
            ProjectManifest::discover(&to_abs_path_buf(&path).unwrap()).unwrap();

        let project =
            Project::load(&buck_config, &elp_config, &BUCK_QUERY_CONFIG, &|_| {}).unwrap();

        let project_data: Vec<ProjectAppData> = project
            .non_otp_apps()
            .filter(|&app| app.app_type == AppType::App)
            .filter(|&app| {
                !app.dir
                    .as_os_str()
                    .to_string_lossy()
                    .contains("third-party")
            })
            .cloned()
            .sorted_by_key(|data| data.name.0.clone())
            .collect();

        let test_data: Vec<ProjectAppTestData> = get_test_data()
            .into_iter()
            .sorted_by_key(|data| data.name)
            .collect();

        //this assert checks absence of test_elp_suite_data app
        assert_eq!(&project_data.len(), &test_data.len());

        for (proj, test) in project_data.iter().zip(test_data.iter()) {
            assert_project_app_data(proj, test);
        }
    }

    //this data represents ~/local/whatsapp/server/tools/elp/_build/buck_tests. Structure:
    //following 4 targets has buck inside
    //test_elp: [BUCK, src, test[tests, test_elp_SUITE_data], include]
    //  has direct dep to [test_elp_direct_dep, test_elp_no_private_headers, test_elp_no_public_headers,
    //  test_elp_flat_outside_target, test_elp_flat_inside_target] and transitive to test_elp_transitive_dep
    //test_elp_direct_dep: [BUCK, src, test, include]
    //  depends on test_elp_transitive_dep and on test_elp making cycle
    //test_elp_transitive_dep: [BUCK, src, test, include]
    //test_elp_flat_inside_target: [BUCK, *.erl, *.hrl]
    //  this app has flat structure (without src, include) and BUCK file inside app root
    //BUCK
    //  following 3 apps has target definition
    //test_elp_no_private_headers: [src, include]
    //  app without hrl file in src dir
    //test_elp_no_public_headers: [src]
    //  app without include dir
    //test_elp_flat_outside_target: [*.erl, *.hrl]
    //  flat structure app with
    fn get_test_data() -> Vec<ProjectAppTestData<'static>> {
        let test_elp = ProjectAppTestData {
            name: "test_elp",
            dir: "server/tools/elp/_build/buck_tests/test_elp",
            abs_src_dirs: vec!["server/tools/elp/_build/buck_tests/test_elp/src"],
            extra_src_dirs: vec!["test"],
            include_dirs: vec!["server/tools/elp/_build/buck_tests/test_elp/include"],
            include_path: vec![
                //self include
                "server/tools/elp/_build/buck_tests/test_elp/include",
                //root dir of test_elp
                "server/tools/elp/_build/buck_tests",
            ],
            include_path_must_be_absent: vec![
                "server/tools/elp/_build/buck_tests/test_elp_no_private_headers/src",
                "server/tools/elp/_build/buck_tests/test_elp_no_public_headers/include",
                "server/tools/elp/_build/buck_tests/test_elp_no_public_headers/src",
            ],
        };
        let test_elp_direct_dep = ProjectAppTestData {
            name: "test_elp_direct_dep",
            dir: "server/tools/elp/_build/buck_tests/test_elp_direct_dep",
            abs_src_dirs: vec!["server/tools/elp/_build/buck_tests/test_elp_direct_dep/src"],
            extra_src_dirs: vec![],
            include_dirs: vec!["server/tools/elp/_build/buck_tests/test_elp_direct_dep/include"],
            include_path: vec![
                //self deps
                "server/tools/elp/_build/buck_tests/test_elp_direct_dep/src",
                "server/tools/elp/_build/buck_tests/test_elp_direct_dep/include",
                //root dir
                "server/tools/elp/_build/buck_tests",
            ],
            include_path_must_be_absent: vec![
                //src should be empty because we don't have tests in this module
                "server/tools/elp/_build/buck_tests/test_elp_transitive_dep/src",
            ],
        };
        let test_elp_transitive_dep = ProjectAppTestData {
            name: "test_elp_transitive_dep",
            dir: "server/tools/elp/_build/buck_tests/test_elp_transitive_dep",
            abs_src_dirs: vec!["server/tools/elp/_build/buck_tests/test_elp_transitive_dep/src"],
            extra_src_dirs: vec![],
            include_dirs: vec![
                "server/tools/elp/_build/buck_tests/test_elp_transitive_dep/include",
            ],
            include_path: vec![
                //self
                "server/tools/elp/_build/buck_tests/test_elp_transitive_dep/src",
                "server/tools/elp/_build/buck_tests/test_elp_transitive_dep/include",
                //root
                "server/tools/elp/_build/buck_tests",
            ],
            include_path_must_be_absent: vec![],
        };
        let test_elp_no_public_headers = ProjectAppTestData {
            name: "test_elp_no_public_headers",
            dir: "server/tools/elp/_build/buck_tests/test_elp_no_public_headers",
            abs_src_dirs: vec!["server/tools/elp/_build/buck_tests/test_elp_no_public_headers/src"],
            extra_src_dirs: vec![],
            include_dirs: vec![],
            include_path: vec!["server/tools/elp/_build/buck_tests"],
            include_path_must_be_absent: vec![
                "server/tools/elp/_build/buck_tests/test_elp_no_public_headers/include",
            ],
        };
        let test_elp_flat_inside_target = ProjectAppTestData {
            name: "test_elp_flat_inside_target",
            dir: "server/tools/elp/_build/buck_tests/test_elp_flat_inside_target",
            abs_src_dirs: vec![""],
            extra_src_dirs: vec![],
            include_dirs: vec!["server/tools/elp/_build/buck_tests/test_elp_flat_inside_target"],
            include_path: vec![
                //self
                "server/tools/elp/_build/buck_tests/test_elp_flat_inside_target",
                //root
                "server/tools/elp/_build/buck_tests",
            ],
            include_path_must_be_absent: vec![],
        };
        let test_elp_flat_outside_target = ProjectAppTestData {
            name: "test_elp_flat_outside_target",
            dir: "server/tools/elp/_build/buck_tests/test_elp_flat_outside_target",
            abs_src_dirs: vec![""],
            extra_src_dirs: vec![],
            include_dirs: vec!["server/tools/elp/_build/buck_tests/test_elp_flat_outside_target"],
            include_path: vec![
                //self
                "server/tools/elp/_build/buck_tests/test_elp_flat_outside_target",
                //root
                "server/tools/elp/_build/buck_tests",
            ],
            include_path_must_be_absent: vec![],
        };
        let test_elp_no_private_headers = ProjectAppTestData {
            name: "test_elp_no_private_headers",
            dir: "server/tools/elp/_build/buck_tests/test_elp_no_private_headers",
            abs_src_dirs: vec![
                "server/tools/elp/_build/buck_tests/test_elp_no_private_headers/src",
            ],
            extra_src_dirs: vec![],
            include_dirs: vec![
                "server/tools/elp/_build/buck_tests/test_elp_no_private_headers/include",
            ],
            include_path: vec![
                //self
                "server/tools/elp/_build/buck_tests/test_elp_no_private_headers/include",
                //root
                "server/tools/elp/_build/buck_tests",
            ],
            include_path_must_be_absent: vec![],
        };
        vec![
            test_elp,
            test_elp_direct_dep,
            test_elp_transitive_dep,
            test_elp_no_public_headers,
            test_elp_flat_inside_target,
            test_elp_flat_outside_target,
            test_elp_no_private_headers,
        ]
    }

    fn assert_project_app_data(app: &ProjectAppData, test: &ProjectAppTestData) {
        assert_eq!(&app.name.0, test.name);
        assert!(
            ends_with(&app.dir, test.dir),
            "dir: {:?}, expected: {:?}",
            &app.dir,
            &test.dir
        );
        for src in &test.abs_src_dirs {
            assert!(app.abs_src_dirs.iter().any(|path| ends_with(path, src)));
        }
        assert_eq!(app.extra_src_dirs, test.extra_src_dirs);
        for include in &test.include_dirs {
            assert!(app.include_dirs.iter().any(|path| ends_with(path, include)));
        }
        for include_path in &test.include_path {
            assert!(
                app.include_path
                    .iter()
                    .any(|path| ends_with(path, include_path))
            );
        }

        for include_path in &test.include_path_must_be_absent {
            assert!(
                !app.include_path
                    .iter()
                    .any(|path| ends_with(path, include_path))
            );
        }
    }

    fn ends_with(path: &AbsPath, s: &str) -> bool {
        let path = fs::canonicalize(path).unwrap();
        let path = path.as_os_str().to_string_lossy();
        path.ends_with(s)
    }

    struct ProjectAppTestData<'a> {
        name: &'a str,
        dir: &'a str,
        abs_src_dirs: Vec<&'a str>,
        extra_src_dirs: Vec<&'a str>,
        include_dirs: Vec<&'a str>,
        include_path: Vec<&'a str>,
        include_path_must_be_absent: Vec<&'a str>,
    }
}
