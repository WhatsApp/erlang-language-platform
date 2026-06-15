/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Loads a rebar project into a static instance of ELP,
//! without support for incorporating changes
use std::path::Path;
use std::path::PathBuf;

use anyhow::Result;
use crossbeam_channel::Receiver;
use crossbeam_channel::unbounded;
use elp_ide::AnalysisHost;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::ProjectApps;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::RootQueryDb;
use elp_ide::elp_ide_db::elp_base_db::Vfs;
use elp_ide::elp_ide_db::elp_base_db::loader;
use elp_ide::elp_ide_db::elp_base_db::loader::Handle;
use elp_project_model::DiscoverConfig;
use elp_project_model::ElpConfig;
use elp_project_model::IncludeParentDirs;
use elp_project_model::Project;
use elp_project_model::ProjectManifest;
use elp_project_model::buck::BuckQueryConfig;
use fxhash::FxHashMap;
use vfs::VfsPath;
use vfs::loader::LoadingProgress;

use crate::build::types::LoadResult;
use crate::cli::Cli;
use crate::line_endings::LineEndings;
use crate::reload::ProjectFolders;
use crate::reload::apply_source_roots;
use crate::reload::apply_vfs_text_changes;

/// Discover the project manifest by walking upward from the given path.
/// This is the shared discovery logic used by both `load_project_at` and
/// the daemon's project root discovery.
pub fn discover_manifest(
    root: &Path,
    conf: &DiscoverConfig,
) -> Result<(ElpConfig, ProjectManifest)> {
    let root = dunce::canonicalize(root)?;
    let root = AbsPathBuf::assert_utf8(root);
    let (elp_config, manifest) = match conf.rebar {
        true => (
            ElpConfig::default(),
            ProjectManifest::discover_rebar(
                &root,
                Some(conf.rebar_profile.clone()),
                IncludeParentDirs::Yes,
            )?,
        ),
        false => {
            let (elp_config, manifest) = ProjectManifest::discover(&root)?;
            (elp_config, Some(manifest))
        }
    };
    let manifest = manifest.ok_or_else(|| anyhow::anyhow!("no projects"))?;
    Ok((elp_config, manifest))
}

/// The directory that contains the project manifest file — i.e. the project
/// root. Lint config (`.elp_lint.toml`) discovery is anchored here so it is
/// resolved relative to the same root as the project (`.elp.toml`) config,
/// regardless of the directory `--project` points at. Otherwise the two configs
/// can diverge and the reported diagnostics depend on the current directory.
/// See T271918831.
pub fn project_root_dir(manifest: &ProjectManifest) -> PathBuf {
    let p = manifest.root();
    let root: &Path = p.parent().unwrap_or(p).as_ref();
    root.to_path_buf()
}

pub fn load_project_at(
    cli: &dyn Cli,
    root: &Path,
    conf: DiscoverConfig,
    include_otp: IncludeOtp,
    eqwalizer_mode: elp_eqwalizer::Mode,
    query_config: &BuckQueryConfig,
    ifdef: bool,
) -> Result<LoadResult> {
    let (elp_config, manifest) = discover_manifest(root, &conf)?;
    load_project_from_manifest(
        cli,
        &manifest,
        &elp_config,
        include_otp,
        eqwalizer_mode,
        query_config,
        ifdef,
    )
}

/// Load a project from an already-discovered manifest.
/// Use this when the manifest has been discovered separately (e.g., by the daemon
/// to avoid redundant discovery).
pub fn load_project_from_manifest(
    cli: &dyn Cli,
    manifest: &ProjectManifest,
    elp_config: &ElpConfig,
    include_otp: IncludeOtp,
    eqwalizer_mode: elp_eqwalizer::Mode,
    query_config: &BuckQueryConfig,
    ifdef: bool,
) -> Result<LoadResult> {
    crate::ensure_rayon_pool();
    log::info!("Loading project: {manifest:?}");
    let pb = cli.spinner("Loading build info");
    let project = Project::load(manifest, elp_config, query_config, &|message| {
        pb.set_message(message.to_string())
    })?;
    pb.finish();

    load_project(cli, project, include_otp, eqwalizer_mode, ifdef)
}

fn load_project(
    cli: &dyn Cli,
    project: Project,
    include_otp: IncludeOtp,
    eqwalizer_mode: elp_eqwalizer::Mode,
    ifdef: bool,
) -> Result<LoadResult> {
    let project_id = ProjectId(0);
    let (sender, receiver) = unbounded();
    let mut vfs = Vfs::default();
    let mut line_ending_map = FxHashMap::default();
    let mut loader = {
        let loader = vfs_notify::NotifyHandle::spawn(sender);
        Box::new(loader)
    };

    let projects = [project.clone()];
    let project_apps = ProjectApps::new(&projects, include_otp);
    let folders = ProjectFolders::new(&project_apps);

    let vfs_loader_config = loader::Config {
        load: folders.load,
        watch: vec![],
        version: 0,
    };
    loader.set_config(vfs_loader_config);

    let analysis_host = load_database(
        cli,
        &project_apps,
        &folders.file_set_config,
        &mut vfs,
        &mut line_ending_map,
        &receiver,
        eqwalizer_mode,
        ifdef,
    )?;
    Ok(LoadResult::new(
        analysis_host,
        vfs,
        line_ending_map,
        project_id,
        project,
        folders.file_set_config,
    ))
}

#[allow(clippy::too_many_arguments)]
fn load_database(
    cli: &dyn Cli,
    project_apps: &ProjectApps,
    file_set_config: &FileSetConfig,
    vfs: &mut Vfs,
    line_ending_map: &mut FxHashMap<FileId, LineEndings>,
    receiver: &Receiver<loader::Message>,
    eqwalizer_mode: elp_eqwalizer::Mode,
    ifdef: bool,
) -> Result<AnalysisHost> {
    let mut analysis_host = AnalysisHost::default();

    let db = analysis_host.raw_database_mut();

    db.set_eqwalizer_mode(eqwalizer_mode);
    db.set_ifdef_enabled(ifdef);

    let pb = cli.simple_progress(0, "Loading applications");

    for task in receiver {
        match task {
            loader::Message::Progress {
                n_done, n_total, ..
            } => {
                pb.set_length(n_total as u64);
                match n_done {
                    LoadingProgress::Started => {}
                    LoadingProgress::Progress(n_done) => {
                        pb.set_position(n_done as u64);
                    }
                    LoadingProgress::Finished => {
                        break;
                    }
                }
            }
            loader::Message::Loaded { files } | loader::Message::Changed { files } => {
                for (path, contents) in files {
                    vfs.set_file_contents(path.into(), contents);
                }
            }
        }
    }

    pb.finish();

    let pb = cli.spinner("Seeding database");

    apply_source_roots(db, vfs, file_set_config);

    project_apps.app_structure().apply(db, &|path| {
        vfs.file_id(&VfsPath::from(path.clone())).map(|(id, _)| id)
    });

    let changed_files = vfs.take_changes();
    apply_vfs_text_changes(db, vfs, changed_files.values(), line_ending_map);

    pb.finish();

    Ok(analysis_host)
}
