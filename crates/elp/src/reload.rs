/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cmp;
use std::sync::Arc;

use elp_ide::elp_ide_db::RootDatabase;
use elp_ide::elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide::elp_ide_db::elp_base_db::ProjectApps;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide::elp_ide_db::elp_base_db::SourceRoot;
use elp_ide::elp_ide_db::elp_base_db::SourceRootId;
use elp_ide::elp_ide_db::elp_base_db::Vfs;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_ide::elp_ide_db::elp_base_db::loader;
use elp_project_model::ProjectAppData;
use elp_project_model::ProjectBuildData;
use elp_project_model::buck::BuckProject;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use vfs::AbsPathBuf;
use vfs::ChangedFile;
use vfs::FileId;

use crate::document::Document;
use crate::line_endings::LineEndings;

#[derive(Debug)]
pub struct ProjectFolders {
    pub load: Vec<loader::Entry>,
    pub watch: Vec<lsp_types::FileSystemWatcher>,
    pub file_set_config: FileSetConfig,
}

pub fn watch_paths_for_app(
    app: &ProjectAppData,
    project_id: ProjectId,
    otp_project_id: Option<ProjectId>,
) -> Vec<String> {
    let mut paths = Vec::new();
    for root in app.all_dirs_to_watch() {
        if Some(project_id) != otp_project_id {
            paths.push(format!("{root}/**/*.{{e,h}}rl"));
        }
    }
    for file in app.all_files_to_watch() {
        paths.push(file.to_string());
    }
    paths
}

pub fn project_root_watch_paths(root: &AbsPathBuf) -> Vec<String> {
    vec![
        format!("{}/**/BUCK", root),
        format!("{}/**/TARGETS", root),
        format!("{}/**/TARGETS.v2", root),
        format!("{}/**/.elp.toml", root),
        format!("{}/**/.elp_lint.toml", root),
        format!("{}/**/rebar.{{config,config.script,lock}}", root),
    ]
}

fn buck_project_watch_paths(buck_project: &BuckProject) -> Vec<String> {
    let dirs = buck_project.buck_conf.included_target_dirs();
    if dirs.is_empty() {
        let root = buck_project.buck_conf.source_root();
        return project_root_watch_paths(&root);
    }
    let mut paths: Vec<String> = dirs
        .iter()
        .flat_map(|dir| {
            [
                format!("{}/**/BUCK", dir),
                format!("{}/**/TARGETS", dir),
                format!("{}/**/TARGETS.v2", dir),
            ]
        })
        .collect();
    if let Some(config_dir) = buck_project.buck_conf.config_dir() {
        paths.push(format!("{}/**/.elp.toml", config_dir));
        paths.push(format!("{}/**/.elp_lint.toml", config_dir));
    }
    paths
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

        let otp_project_id = project_apps.otp_project_id;

        let mut watch_paths: FxHashSet<_> = project_apps
            .all_apps
            .iter()
            .flat_map(|(project_id, app)| watch_paths_for_app(app, *project_id, otp_project_id))
            .collect();

        for project in &project_apps.projects {
            match &project.project_build_data {
                ProjectBuildData::Buck(buck_project) => {
                    watch_paths.extend(buck_project_watch_paths(buck_project));
                }
                _ => {
                    let root = project.root();
                    watch_paths.extend(project_root_watch_paths(&root));
                }
            }
        }

        // LSP spec says "If omitted it defaults to
        // WatchKind.Create | WatchKind.Change | WatchKind.Delete
        // which is 7".
        let kind = None;
        let watch: Vec<_> = watch_paths
            .into_iter()
            .map(|glob_pattern| lsp_types::FileSystemWatcher {
                glob_pattern: lsp_types::GlobPattern::String(glob_pattern),
                kind,
            })
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
        if let Some(applicable_files) = app.applicable_files.as_ref() {
            files.extend(applicable_files.clone());
        }
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

pub fn apply_vfs_text_changes<'a>(
    db: &mut RootDatabase,
    vfs: &Vfs,
    changed_files: impl IntoIterator<Item = &'a ChangedFile>,
    line_ending_map: &mut FxHashMap<FileId, LineEndings>,
) {
    for file in changed_files {
        if file.change != vfs::Change::Delete && vfs.exists(file.file_id) {
            if let vfs::Change::Create(v, _) | vfs::Change::Modify(v, _) = &file.change {
                let document = Document::from_bytes(v);
                let (text, line_ending) = document.vfs_to_salsa();
                db.set_file_text(file.file_id, Arc::from(text));
                line_ending_map.insert(file.file_id, line_ending);
            }
        } else {
            db.set_file_text(file.file_id, Arc::from(""));
        }
    }
}

pub fn apply_source_roots(db: &mut RootDatabase, vfs: &Vfs, file_set_config: &FileSetConfig) {
    let sets = file_set_config.partition(vfs);
    for (idx, set) in sets.into_iter().enumerate() {
        let root_id = SourceRootId(idx as u32);
        for file_id in set.iter() {
            db.set_file_source_root(file_id, root_id);
        }
        let root = SourceRoot::new(set);
        db.set_source_root(root_id, Arc::new(root));
    }
}
