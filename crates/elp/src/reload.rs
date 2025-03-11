/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp;
use std::iter;

use elp_ide::elp_ide_db::elp_base_db::loader;
use elp_ide::elp_ide_db::elp_base_db::AppType;
use elp_ide::elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide::elp_ide_db::elp_base_db::ProjectApps;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use fxhash::FxHashSet;
use vfs::AbsPathBuf;

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

        let mut watch_paths: FxHashSet<_> = project_apps
            .all_apps
            .iter()
            .flat_map(|(project_id, app)| {
                iter::repeat(project_id).zip(app.all_source_and_include_dirs())
            })
            .filter_map(|(project_id, root)| {
                if Some(*project_id) != project_apps.otp_project_id {
                    Some(format!("{}/**/*.{{e,h}}rl", root))
                } else {
                    None
                }
            })
            .collect();

        for project in &project_apps.projects {
            let root = project.root();
            watch_paths.extend(vec![
                format!("{}/**/BUCK", root),
                format!("{}/**/TARGETS", root),
                format!("{}/**/TARGETS.v2", root),
                format!("{}/**/.elp.toml", root),
                format!("{}/**/.elp_lint.toml", root),
                format!("{}/**/rebar.{{config,config.script,lock}}", root),
            ]);
        }

        // LSP spec says "If omitted it defaults to
        // WatchKind.Create | WatchKind.Change | WatchKind.Delete
        // which is 7".
        let kind = None;
        let watch: Vec<_> = watch_paths
            .into_iter()
            .map(|glob_pattern| lsp_types::FileSystemWatcher { glob_pattern, kind })
            .collect();

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

    let app_dirs_vec: Vec<AbsPathBuf> = app_dirs.into_iter().collect();
    let include_dirs_vec: Vec<AbsPathBuf> = include_dirs.into_iter().collect();
    // Create chunks so our loader spinner shows progress. And to
    // interleave loading and storing
    // There is no particular significance to the numbers chosen, the
    // intent is to have a reasonable progress bar, with 100 as the total.
    // We split the chunks arbitrarily between apps and include dirs, leaving
    // room for the files too.
    // For some reason our progress reporter lists this as 100, even
    // though the total seems to be 80 + 16 + 1 = 97

    let apps_chunk_size = cmp::max(app_dirs_vec.len() / 80, 1);
    let include_dirs_chunk_size = cmp::max(include_dirs_vec.len() / 16, 1);

    let mut load: Vec<_> = vec![];
    load.extend(
        app_dirs_vec
            .chunks(apps_chunk_size)
            .map(|chunk| -> Vec<AbsPathBuf> { chunk.into() })
            .map(|include| {
                loader::Entry::Directories(loader::Directories {
                    extensions: vec!["erl".to_string(), "hrl".to_string(), "escript".to_string()],
                    include,
                    exclude: vec![],
                })
            }),
    );
    load.extend(
        include_dirs_vec
            .chunks(include_dirs_chunk_size)
            .map(|chunk| -> Vec<AbsPathBuf> { chunk.into() })
            .map(|include| {
                loader::Entry::Directories(loader::Directories {
                    extensions: vec!["hrl".to_string()],
                    include,
                    exclude: vec![],
                })
            }),
    );
    // Put the files last to catch anything missed in the prior loads.
    // TODO: consider removing files that will already be selected
    // from the directory load entries. This needs a many-many
    // correlation though.
    load.push(loader::Entry::Files(files.into_iter().collect()));
    load
}
