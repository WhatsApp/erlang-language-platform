/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::path::Path;
use std::sync::Arc;

use elp_project_model::AppName;
use elp_project_model::AppType;
use elp_project_model::EqwalizerConfig;
use elp_project_model::ProjectAppData;
use elp_project_model::buck::BuckProjectIndex;
use elp_project_model::buck::TargetFullName;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use paths::RelPath;
use paths::Utf8Path;
use salsa::plumbing::AsId;
use vfs::AbsPath;
use vfs::AbsPathBuf;
use vfs::FileId;
use vfs::VfsPath;
use vfs::file_set::FileSet;

use crate::AppDataIndex;
use crate::FileMap;
use crate::RootQueryDb;

/// Files are grouped into source roots. A source root is a directory on the
/// file systems which is watched for changes. Typically it corresponds to an OTP
/// application. Source roots *might* be nested: in this case, a file belongs to
/// the nearest enclosing source root. Paths to files are always relative to a
/// source root, and ELP does not know the root path of the source root at
/// all. So, a file from one source root can't refer to a file in another source
/// root by path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct SourceRootId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceRoot {
    file_set: FileSet,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FileSource {
    Src,
    Extra,
}

impl SourceRoot {
    pub fn new(file_set: FileSet) -> SourceRoot {
        SourceRoot { file_set }
    }

    pub fn path_for_file(&self, file: &FileId) -> Option<&VfsPath> {
        self.file_set.path_for_file(file)
    }

    pub fn file_for_path(&self, path: &VfsPath) -> Option<FileId> {
        self.file_set.file_for_path(path).copied()
    }

    pub fn relative_path(&self, file: FileId, segment: &str) -> Option<FileId> {
        let base_path = self.path_for_file(&file)?;
        self.file_for_path(&base_path.parent()?.join(segment)?)
    }

    pub fn iter(&self) -> impl Iterator<Item = FileId> + '_ {
        self.file_set.iter()
    }

    pub fn iter_app_files<'a>(
        &'a self,
        app_data: &'a AppData,
    ) -> impl Iterator<Item = (FileId, FileSource, &'a VfsPath)> + 'a {
        self.iter()
            .flat_map(move |file_id| self.file_info(file_id, app_data))
    }

    pub fn file_info(
        &self,
        file_id: FileId,
        app_data: &AppData,
    ) -> Option<(FileId, FileSource, &VfsPath)> {
        let path = self.path_for_file(&file_id)?;
        if app_data.is_src_file(path) {
            Some((file_id, FileSource::Src, path))
        } else if app_data.is_extra_src_file(path) {
            Some((file_id, FileSource::Extra, path))
        } else if app_data.is_test_target == Some(true) {
            // buck test target source file. It only has one.
            Some((file_id, FileSource::Extra, path))
        } else {
            None
        }
    }
}

/// The loaded projects.
#[salsa::input(singleton, debug)]
pub struct Workspace {
    /// In the order the loader gave them, the OTP project last.
    #[returns(ref)]
    pub projects: Arc<[Project]>,
    /// The inputs of the projects that are no longer loaded, with their large
    /// fields cleared, for lowering to give back to a project that returns. No
    /// query reads them.
    #[returns(ref)]
    pub removed_projects: Arc<[Project]>,
    /// Bumped whenever the erlang_service pool drops its VMs. The queries that
    /// ask a VM read it, so that they run again on a new one.
    pub vm_epoch: u64,
}

#[salsa::input]
pub struct Project {
    #[returns(ref)]
    pub root: VfsPath,
    /// The OTP project, whose own link is to itself; `None` when OTP isn't
    /// loaded.
    pub otp: Option<Project>,
    #[returns(ref)]
    pub buck_index: Option<Arc<BuckProjectIndex>>,
    /// The dependencies' ebin directories, in code-path order. Selects the
    /// erlang_service VM.
    #[returns(ref)]
    pub code_path: Arc<[AbsPathBuf]>,
    #[returns(ref)]
    pub eqwalizer_config: EqwalizerConfig,
    /// The source roots of the apps, in the order the loader gave the apps.
    #[returns(ref)]
    pub source_roots: Arc<[SourceRootId]>,
}

#[salsa::tracked]
impl Project {
    /// The project's root, which lowering gives as a real path.
    pub fn root_dir(self, db: &dyn RootQueryDb) -> &AbsPath {
        as_real_path(self.root(db))
    }

    /// The source root of the app `name` refers to in the project: the last
    /// app of the project with that name, in the order the loader gave them,
    /// else the OTP app with that name.
    #[salsa::tracked]
    pub fn app_named(self, db: &dyn RootQueryDb, name: AppName) -> Option<SourceRootId> {
        let find = |project: Project| {
            project
                .source_roots(db)
                .iter()
                .rev()
                .copied()
                .find(|&source_root_id| {
                    db.app_data(source_root_id)
                        .is_some_and(|app_data| app_data.name == name)
                })
        };
        find(self).or_else(|| self.otp(db).filter(|&otp| otp != self).and_then(find))
    }
}

// Only the id: the OTP project links to itself, and salsa's derived `Debug`
// would follow that cycle.
impl fmt::Debug for Project {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Project").field(&self.as_id()).finish()
    }
}

/// `AppData` is stored in salsa, indexed by `SourceRootId`.
/// We create a `SourceRoot` for every app in every `Project`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AppData {
    pub project: Project,
    pub name: AppName,
    /// Target name if this application originates from a buck target
    pub buck_target_name: Option<TargetFullName>,
    pub dir: AbsPathBuf,
    /// Include directories belonging to this app only. Used for
    /// include_lib resolution
    pub include_dirs: Vec<AbsPathBuf>,
    /// The set of local include directories as well as those of all
    /// dependencies
    pub include_path: Vec<AbsPathBuf>,
    pub src_path: Vec<AbsPathBuf>,
    pub extra_src_dirs: Vec<String>,
    pub macros: Vec<eetf::Term>,
    pub parse_transforms: Vec<eetf::Term>,
    pub app_type: AppType,
    pub ebin_path: Option<AbsPathBuf>,
    /// When the app is generated from buck, each test module shows
    /// up as a different own app.
    pub is_test_target: Option<bool>,
}

impl AppData {
    fn is_src_file(&self, path: &VfsPath) -> bool {
        if let Some(path) = path.as_path() {
            // src_dirs are recursive, check path begins with one
            return self
                .src_path
                .iter()
                .any(|src_dir| path.starts_with(src_dir));
        }
        false
    }

    pub(crate) fn is_extra_src_file(&self, path: &VfsPath) -> bool {
        if let Some(path) = self.local_file_path(path) {
            // extra_src_dirs are not recursive, check parent dir is one
            let path: &Utf8Path = path.as_ref();
            if let Some(parent) = path.parent() {
                return self
                    .extra_src_dirs
                    .iter()
                    .any(|src_dir| parent == Path::new(src_dir));
            }
        }
        false
    }

    fn local_file_path<'a>(&self, path: &'a VfsPath) -> Option<&'a RelPath> {
        path.as_path()?.strip_prefix(&self.dir)
    }
}

/// Note that `AppStructure` is build-system agnostic
#[derive(Debug, Clone, Default /* Serialize, Deserialize */)]
pub struct AppStructure {
    pub(crate) app_map: FxHashMap<SourceRootId, AppMapData>,
    pub(crate) catch_all_source_root: SourceRootId,
}

#[derive(Debug, Clone, Default)]
pub struct AppMapData {
    app_data: Option<AppData>, // TODO: should this be Arc?
    applicable_files: Option<FxHashSet<AbsPathBuf>>,
    gen_src_files: Option<FxHashSet<AbsPathBuf>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct AppDataId(pub u32);

impl AppStructure {
    pub fn add_app_data(
        &mut self,
        source_root_id: SourceRootId,
        app_data: Option<AppData>,
        applicable_files: Option<FxHashSet<AbsPathBuf>>,
        gen_src_files: Option<FxHashSet<AbsPathBuf>>,
    ) {
        let prev = self.app_map.insert(
            source_root_id,
            AppMapData {
                app_data,
                applicable_files,
                gen_src_files,
            },
        );
        assert!(prev.is_none());
    }

    /// Set the salsa inputs according to this AppStructure, and return the app
    /// of each generated source file. Each path an app lists gets a `FileId`
    /// from `file_map`, whether or not the file has loaded.
    pub fn apply(
        self,
        db: &mut dyn RootQueryDb,
        file_map: &FileMap,
    ) -> FxHashMap<AbsPathBuf, AppDataId> {
        let mut app_index = AppDataIndex::default();
        let mut app_data_id = AppDataId(0);
        let mut gen_src_inputs = FxHashMap::default();
        for (source_root_id, app_map_data) in self.app_map {
            let arc_data = app_map_data.app_data.map(Arc::new);
            db.set_app_data_by_id(app_data_id, arc_data);
            db.set_app_data_id(source_root_id, app_data_id);
            for path in app_map_data.applicable_files.into_iter().flatten() {
                let file_id = file_map.intern(db, VfsPath::from(path));
                app_index.insert_owner(file_id, app_data_id);
            }
            if let Some(files) = app_map_data.gen_src_files {
                for file in files {
                    gen_src_inputs.insert(file.clone(), app_data_id);
                }
            }
            app_data_id = AppDataId(app_data_id.0 + 1);
        }
        db.set_app_index(Arc::new(app_index));
        db.set_catch_all_source_root(self.catch_all_source_root);

        gen_src_inputs
    }
}

/// Lowering gives projects only real paths.
fn as_real_path(path: &VfsPath) -> &AbsPath {
    path.as_path()
        .expect("lowering should give only real paths")
}

// ---------------------------------------------------------------------
/// Currently we don't eqWAlize OTP
/// Historical reasons:
/// - We used to not support the full language. Now we may actually be able to eqWAlize OTP if we could get the project model
/// - OTP code is written very differently from WhatsApp code, so we didn't want to bias our trade-offs at the beginning.
///
/// It could change in future
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncludeOtp {
    Yes,
    No,
}

#[derive(Debug)]
pub struct ProjectApps<'a> {
    /// All the applications in a set of projects, with the position of their
    /// project in `projects`.  The order here will correspond with the vfs
    /// sourceRootId's
    pub all_apps: Vec<(usize, &'a ProjectAppData)>,
    /// The position of the OTP project in `projects`. Sometimes we don't have
    /// an OTP project because we are explicitly opting out of using it, e.g.
    /// for eqWAlizer compatibility
    pub otp_project_idx: Option<usize>,
    pub projects: Vec<elp_project_model::Project>,
}

impl<'a> ProjectApps<'a> {
    pub fn new(
        projects: &'a [elp_project_model::Project],
        include_otp: IncludeOtp,
    ) -> ProjectApps<'a> {
        let mut all_apps: Vec<(usize, &ProjectAppData)> = projects
            .iter()
            .enumerate()
            .flat_map(|(project_idx, project)| project.all_apps().map(move |p| (project_idx, p)))
            .collect();

        // We assume that all of the `Project`s use the same OTP.
        // And so we extract the OTP apps from the first one, and put
        // them into a `Project` specifically for OTP, so we do not
        // duplicate it for every project. .
        let first_project = &projects[0];
        let mut projects: Vec<_> = projects.into();
        let otp_project_idx = if include_otp == IncludeOtp::Yes {
            let otp_project_idx = projects.len();
            let mut all_otp_apps: Vec<(usize, &ProjectAppData)> = first_project
                .otp_apps()
                .map(|app| (otp_project_idx, app))
                .collect();
            all_apps.append(&mut all_otp_apps);
            let otp_project = elp_project_model::Project::otp(
                first_project.otp.clone(),
                first_project.otp_apps().cloned().collect(),
            );
            projects.push(otp_project);
            Some(otp_project_idx)
        } else {
            None
        };

        ProjectApps {
            all_apps,
            otp_project_idx,
            projects,
        }
    }

    /// The apps of `projects`, the inputs lowering made of `self.projects`.
    pub fn app_structure(&self, projects: &[Project]) -> AppStructure {
        let mut app_structure = AppStructure::default();
        let mut app_idx = 0;

        // Reconstruct the per-project list
        let mut apps_by_project: FxHashMap<usize, Vec<&ProjectAppData>> = FxHashMap::default();

        for (project_idx, appdata) in self.all_apps.iter() {
            apps_by_project
                .entry(*project_idx)
                .or_default()
                .push(appdata);
        }

        for (project_idx, &project) in projects.iter().enumerate() {
            let empty = vec![];
            let apps = apps_by_project.get(&project_idx).unwrap_or(&empty);

            for app in apps {
                let root_id = SourceRootId(app_idx);
                app_idx += 1;
                let input_data = AppData {
                    project,
                    name: app.name.clone(),
                    buck_target_name: app.buck_target_name.clone(),
                    dir: app.dir.clone(),
                    include_dirs: app.include_dirs.clone(),
                    include_path: app.include_path.clone(),
                    extra_src_dirs: app.extra_src_dirs.clone(),
                    macros: app.macros.clone(),
                    parse_transforms: app.parse_transforms.clone(),
                    app_type: app.app_type,
                    src_path: app.abs_src_dirs.clone(),
                    ebin_path: app.ebin.clone(),
                    is_test_target: app.is_test_target,
                };
                app_structure.add_app_data(
                    root_id,
                    Some(input_data),
                    app.applicable_files.clone(),
                    app.gen_src_files.clone(),
                );
            }
        }

        // Final SourceRoot for out-of-project files
        log::info!("Final source root: {:?}", SourceRootId(app_idx));
        app_structure.add_app_data(SourceRootId(app_idx), None, None, None);
        app_structure.catch_all_source_root = SourceRootId(app_idx);
        app_structure
    }
}
