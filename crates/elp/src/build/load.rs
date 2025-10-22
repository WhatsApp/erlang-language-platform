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
use std::fs;
use std::path::Path;
use std::sync::Arc;

use anyhow::Result;
use anyhow::bail;
use crossbeam_channel::Receiver;
use crossbeam_channel::unbounded;
use elp_ide::AnalysisHost;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::ProjectApps;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide::elp_ide_db::elp_base_db::SourceRoot;
use elp_ide::elp_ide_db::elp_base_db::SourceRootId;
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
use crate::document::Document;
use crate::line_endings::LineEndings;
use crate::reload::ProjectFolders;

pub fn load_project_at(
    cli: &dyn Cli,
    root: &Path,
    conf: DiscoverConfig,
    include_otp: IncludeOtp,
    eqwalizer_mode: elp_eqwalizer::Mode,
    query_config: &BuckQueryConfig,
) -> Result<LoadResult> {
    let root = fs::canonicalize(root)?;
    let root = AbsPathBuf::assert_utf8(root);
    let (elp_config, manifest): (ElpConfig, Option<ProjectManifest>) = match conf.rebar {
        true => (
            ElpConfig::default(),
            ProjectManifest::discover_rebar(
                &root,
                Some(conf.rebar_profile),
                IncludeParentDirs::Yes,
            )?,
        ),
        false => {
            let (elp_config, manifest) = ProjectManifest::discover(&root)?;
            (elp_config, Some(manifest))
        }
    };
    let manifest = if let Some(manifest) = manifest {
        manifest
    } else {
        bail!("no projects")
    };

    log::info!("Discovered project: {manifest:?}");
    let pb = cli.spinner("Loading build info");
    let project = Project::load(&manifest, &elp_config, query_config, &|_progress| {})?;
    pb.finish();

    load_project(cli, project, include_otp, eqwalizer_mode)
}

fn load_project(
    cli: &dyn Cli,
    project: Project,
    include_otp: IncludeOtp,
    eqwalizer_mode: elp_eqwalizer::Mode,
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

fn load_database(
    cli: &dyn Cli,
    project_apps: &ProjectApps,
    file_set_config: &FileSetConfig,
    vfs: &mut Vfs,
    line_ending_map: &mut FxHashMap<FileId, LineEndings>,
    receiver: &Receiver<loader::Message>,
    eqwalizer_mode: elp_eqwalizer::Mode,
) -> Result<AnalysisHost> {
    let mut analysis_host = AnalysisHost::default();

    let db = analysis_host.raw_database_mut();

    db.set_eqwalizer_mode(eqwalizer_mode);

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

    let sets = file_set_config.partition(vfs);
    for (idx, set) in sets.into_iter().enumerate() {
        let root_id = SourceRootId(idx as u32);
        for file_id in set.iter() {
            db.set_file_source_root(file_id, root_id);
        }
        let root = SourceRoot::new(set);
        db.set_source_root(root_id, Arc::new(root));
    }

    project_apps.app_structure().apply(db, &|path| {
        vfs.file_id(&VfsPath::from(path.clone())).map(|(id, _)| id)
    });

    let changes = vfs.take_changes();
    for (_file_id, file) in changes {
        if file.exists()
            && let vfs::Change::Create(v, _) | vfs::Change::Modify(v, _) = file.change
        {
            let document = Document::from_bytes(&v);
            let (text, line_ending) = document.vfs_to_salsa();
            db.set_file_text(file.file_id, Arc::from(text));
            line_ending_map.insert(file.file_id, line_ending);
        }
    }

    pb.finish();

    Ok(analysis_host)
}
