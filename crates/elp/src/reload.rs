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
use fxhash::FxHashSet;

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
                    let mut file_sets: FxHashSet<VfsPath> = app
                        .abs_src_dirs
                        .iter()
                        .map(|src| VfsPath::from(src.clone()))
                        .collect();
                    let dir = VfsPath::from(app.dir.clone());
                    file_sets.insert(dir);
                    builder.add_file_set(file_sets.into_iter().collect());
                    builder
                },
            )
            .build();

        let load = loader_config(project_apps);

        let mut watch: Vec<_> = project_apps
            .all_apps
            .iter()
            .flat_map(|(project_id, app)| {
                iter::repeat(project_id).zip(app.all_source_and_include_dirs())
            })
            .filter_map(|(project_id, root)| {
                if Some(*project_id) != project_apps.otp_project_id {
                    Some(lsp_types::FileSystemWatcher {
                        glob_pattern: format!("{}/**/*.{{e,h}}rl", root),
                        kind: None,
                    })
                } else {
                    None
                }
            })
            .collect();

        for project in &project_apps.projects {
            let root = project.root();
            // LSP spec says "If omitted it defaults to
            // WatchKind.Create | WatchKind.Change | WatchKind.Delete
            // which is 7".
            let kind = None;
            watch.extend(vec![
                lsp_types::FileSystemWatcher {
                    glob_pattern: format!("{}/**/BUCK", root),
                    kind,
                },
                lsp_types::FileSystemWatcher {
                    glob_pattern: format!("{}/**/TARGETS", root),
                    kind,
                },
                lsp_types::FileSystemWatcher {
                    glob_pattern: format!("{}/**/TARGETS.v2", root),
                    kind,
                },
                lsp_types::FileSystemWatcher {
                    glob_pattern: format!("{}/.elp.toml", root),
                    kind,
                },
                lsp_types::FileSystemWatcher {
                    glob_pattern: format!("{}/.elp_lint.toml", root),
                    kind,
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

fn loader_config(project_apps: &ProjectApps<'_>) -> Vec<loader::Entry> {
    let mut app_dirs = FxHashSet::default();
    let mut include_dirs = FxHashSet::default();
    let mut files = FxHashSet::default();
    project_apps.all_apps.iter().for_each(|(_, app)| {
        app_dirs.extend(app.all_source_dirs());
        if app.app_type == AppType::App {
            files.insert(app.dir.join(".eqwalizer"));
        }
        app.applicable_files.as_ref().map(|applicable_files| {
            files.extend(applicable_files.clone());
        });
        include_dirs.extend(app.include_dirs.clone());
    });

    let load = vec![
        loader::Entry::Directories(loader::Directories {
            extensions: vec!["erl".to_string(), "hrl".to_string(), "escript".to_string()],
            include: app_dirs.into_iter().collect(),
            exclude: vec![],
        }),
        loader::Entry::Directories(loader::Directories {
            extensions: vec!["hrl".to_string()],
            include: include_dirs.into_iter().collect(),
            exclude: vec![],
        }),
        loader::Entry::Files(files.into_iter().collect()),
    ];
    load
}
