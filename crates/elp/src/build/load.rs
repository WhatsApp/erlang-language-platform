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

use anyhow::Context;
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

pub fn canonicalize_project_root(root: &Path) -> Result<AbsPathBuf> {
    let root = match dunce::canonicalize(root) {
        Ok(root) => root,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            bail!("project does not exist at {}", root.display());
        }
        Err(err) => {
            return Err(err)
                .with_context(|| format!("unable to read project at {}", root.display()));
        }
    };
    Ok(AbsPathBuf::assert_utf8(root))
}

/// Discover the project manifest by walking upward from the given path.
pub fn discover_manifest(
    root: &Path,
    conf: &DiscoverConfig,
) -> Result<(ElpConfig, ProjectManifest)> {
    let root = canonicalize_project_root(root)?;
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

/// The order in which loaded files are interned into the VFS, which is the
/// order their `FileId`s are allocated in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FileOrder {
    /// Intern each file as the loader delivers it. The loader reads its
    /// directory chunks in parallel, so which `FileId` a given path gets
    /// varies from run to run.
    AsLoaded,
    /// Buffer the loaded files and intern them in path order, so a given path
    /// always gets the same `FileId` and the run is reproducible. Costs one
    /// sort of the file list, and interning waits for the loader to finish
    /// rather than overlapping it -- only worth asking for when the output is
    /// compared across runs.
    ByPath,
}

/// How a project is loaded, beyond which project it is: what to pull in
/// alongside it, and how to read and index it.
///
/// Build one with [`LoadConfig::new`] and override the rest through struct
/// update syntax, so a caller only spells out what it does differently:
///
/// ```ignore
/// LoadConfig {
///     file_order: FileOrder::ByPath,
///     ..LoadConfig::new(elp_eqwalizer::Mode::Server, *query_config)
/// }
/// ```
#[derive(Clone, Debug)]
pub struct LoadConfig {
    /// Whether OTP is loaded alongside the project's own applications.
    pub include_otp: IncludeOtp,
    pub eqwalizer_mode: elp_eqwalizer::Mode,
    pub query_config: BuckQueryConfig,
    pub file_order: FileOrder,
}

impl LoadConfig {
    /// Defaults to loading OTP, and to [`FileOrder::AsLoaded`] — reproducible
    /// `FileId`s cost something, so they are asked for rather than assumed.
    pub fn new(eqwalizer_mode: elp_eqwalizer::Mode, query_config: BuckQueryConfig) -> Self {
        Self {
            include_otp: IncludeOtp::Yes,
            eqwalizer_mode,
            query_config,
            file_order: FileOrder::AsLoaded,
        }
    }
}

pub fn load_project_at(
    cli: &dyn Cli,
    root: &Path,
    conf: DiscoverConfig,
    load_config: LoadConfig,
) -> Result<LoadResult> {
    let (elp_config, manifest) = discover_manifest(root, &conf)?;
    load_project_from_manifest(cli, &manifest, &elp_config, load_config)
}

/// Load a project from an already-discovered manifest.
/// Use this when the manifest has been discovered separately (e.g., by the daemon
/// to avoid redundant discovery).
pub fn load_project_from_manifest(
    cli: &dyn Cli,
    manifest: &ProjectManifest,
    elp_config: &ElpConfig,
    load_config: LoadConfig,
) -> Result<LoadResult> {
    crate::ensure_rayon_pool();
    log::info!("Loading project: {manifest:?}");
    let pb = cli.spinner("Loading build info");
    let project = Project::load(
        manifest,
        elp_config,
        &load_config.query_config,
        &|message| pb.set_message(message.to_string()),
    )?;
    pb.finish();

    load_project(cli, project, load_config)
}

fn load_project(cli: &dyn Cli, project: Project, load_config: LoadConfig) -> Result<LoadResult> {
    let project_id = ProjectId(0);
    let (sender, receiver) = unbounded();
    let mut vfs = Vfs::default();
    let mut line_ending_map = FxHashMap::default();
    let mut loader = {
        let loader = vfs_notify::NotifyHandle::spawn(sender);
        Box::new(loader)
    };

    let projects = [project.clone()];
    let project_apps = ProjectApps::new(&projects, load_config.include_otp);
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
        &load_config,
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
    load_config: &LoadConfig,
) -> Result<AnalysisHost> {
    let mut analysis_host = AnalysisHost::default();

    let db = analysis_host.raw_database_mut();

    db.set_eqwalizer_mode(load_config.eqwalizer_mode.clone());

    let pb = cli.simple_progress(0, "Loading applications");

    // Stays empty, and unallocated, under `FileOrder::AsLoaded`.
    let mut deferred: Vec<(AbsPathBuf, Option<Vec<u8>>)> = Vec::new();
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
                match load_config.file_order {
                    FileOrder::AsLoaded => {
                        for (path, contents) in files {
                            vfs.set_file_contents(path.into(), contents);
                        }
                    }
                    FileOrder::ByPath => deferred.extend(files),
                }
            }
        }
    }

    // `FileId` is the path's insertion index into the VFS, and the loader reads
    // its directory chunks with rayon, so interning in arrival order gives a
    // given path a different `FileId` on every run. Anything keyed on `FileId`
    // then varies too — the `src.File` ids in an `elp glean` dump, iteration of
    // an `FxHashSet<FileId>`, the tiebreak of a stable sort over
    // `SourceRoot::iter()`. Interning in path order makes those reproducible,
    // at the cost of a sort and of waiting for the loader to finish before
    // interning anything, which is why the caller has to ask for it.
    //
    // A stable sort, so that a path loaded and then changed keeps its arrival
    // order and the later contents still win.
    deferred.sort_by(|(a, _), (b, _)| a.cmp(b));
    for (path, contents) in deferred {
        vfs.set_file_contents(path.into(), contents);
    }

    pb.finish();

    let pb = cli.spinner("Seeding database");

    apply_source_roots(db, vfs, file_set_config);

    project_apps.app_structure().apply(db, &|path| {
        vfs.file_id(&VfsPath::from(path.clone())).map(|(id, _)| id)
    });

    let changed_files = vfs.take_changes();
    line_ending_map.extend(apply_vfs_text_changes(db, vfs, changed_files.values()));

    pb.finish();

    Ok(analysis_host)
}
