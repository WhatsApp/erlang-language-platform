/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter;

use elp_ide::elp_ide_db::elp_base_db::loader;
use elp_ide::elp_ide_db::elp_base_db::AppType;
use elp_ide::elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide::elp_ide_db::elp_base_db::ProjectApps;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use lsp_types::WatchKind;

#[derive(Debug)]
pub struct ProjectFolders {
    pub load: Vec<loader::Entry>,
    pub watch: Vec<lsp_types::FileSystemWatcher>,
    pub file_set_config: FileSetConfig,
}

impl ProjectFolders {
    pub fn new(project_apps: &ProjectApps) -> ProjectFolders {
        let file_set_config = project_apps
            .all_apps
            .iter()
            .fold(
                FileSetConfig::builder(),
                |mut builder, (_project_id, app)| {
                    let mut file_sets: Vec<VfsPath> = app
                        .abs_src_dirs
                        .iter()
                        .map(|src| VfsPath::from(src.clone()))
                        .collect();
                    let dir = VfsPath::from(app.dir.clone());
                    file_sets.push(dir);
                    builder.add_file_set(file_sets);
                    builder
                },
            )
            .build();

        let load = project_apps
            .all_apps
            .iter()
            .flat_map(|(_, app)| {
                let dirs = loader::Directories {
                    extensions: vec!["erl".to_string(), "hrl".to_string(), "escript".to_string()],
                    include: app.all_source_dirs(),
                    exclude: vec![],
                };
                let dir_entry = loader::Entry::Directories(dirs);
                match app.app_type {
                    AppType::App => vec![
                        dir_entry,
                        loader::Entry::Files(vec![app.dir.join(".eqwalizer")]),
                    ],
                    _ => vec![dir_entry],
                }
            })
            .collect();

        let mut watch: Vec<_> = project_apps
            .all_apps
            .iter()
            .flat_map(|(project_id, app)| iter::repeat(project_id).zip(app.all_source_dirs()))
            .filter_map(|(project_id, root)| {
                if Some(*project_id) != project_apps.otp_project_id {
                    Some(lsp_types::FileSystemWatcher {
                        glob_pattern: format!("{}/**/*.{{e,h}}rl", root.display()),
                        kind: None,
                    })
                } else {
                    None
                }
            })
            .collect();

        for project in &project_apps.projects {
            let root = project.root();
            watch.extend(vec![
                lsp_types::FileSystemWatcher {
                    glob_pattern: format!("{}/**/BUCK", root.display()),
                    kind: Some(WatchKind::Create),
                },
                lsp_types::FileSystemWatcher {
                    glob_pattern: format!("{}/**/TARGETS", root.display()),
                    kind: Some(WatchKind::Create),
                },
                lsp_types::FileSystemWatcher {
                    glob_pattern: format!("{}/**/TARGETS.v2", root.display()),
                    kind: Some(WatchKind::Create),
                },
            ]);
        }

        ProjectFolders {
            load,
            watch,
            file_set_config,
        }
    }
}
