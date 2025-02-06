/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::hash::Hash;
use std::path::Path;
use std::sync::Arc;

use elp_project_model::AppName;
use elp_project_model::AppType;
use elp_project_model::ApplicableFiles;
use elp_project_model::EqwalizerConfig;
use elp_project_model::Project;
use elp_project_model::ProjectAppData;
use fxhash::FxHashMap;
use paths::RelPath;
use paths::Utf8Path;
use vfs::file_set::FileSet;
use vfs::AbsPathBuf;
use vfs::FileId;
use vfs::VfsPath;

use crate::AppDataIndex;
use crate::SourceDatabaseExt;

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

    pub fn has_eqwalizer_marker<'a>(&'a self, app_data: &'a AppData) -> bool {
        self.iter().any(|file_id| {
            self.path_for_file(&file_id)
                .iter()
                .any(|p| app_data.is_eqwalizer_marker(p))
        })
    }
}

/// Source roots (apps) are grouped into projects that share some
/// of the configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProjectId(pub u32);

/// `ProjectData` is stored in salsa, indexed by `ProjectId`
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProjectData {
    pub source_roots: Vec<SourceRootId>,
    pub root_dir: AbsPathBuf,
    pub deps_ebins: Vec<AbsPathBuf>,
    pub otp_project_id: Option<ProjectId>,
    pub app_roots: AppRoots,
    pub eqwalizer_config: EqwalizerConfig,
}

/// `AppData` is stored in salsa, indexed by `SourceRootId`.
/// We create a `SourceRoot` for every app in every `Project`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AppData {
    pub project_id: ProjectId,
    pub name: AppName,
    pub dir: AbsPathBuf,
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

    fn is_eqwalizer_marker(&self, path: &VfsPath) -> bool {
        if let Some(path) = self.local_file_path(path) {
            let path: &Utf8Path = path.as_ref();
            return path == Utf8Path::new(".eqwalizer");
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
    pub(crate) app_map: FxHashMap<SourceRootId, (Option<AppData>, Option<ApplicableFiles>)>,
    pub(crate) project_map: FxHashMap<ProjectId, ProjectData>,
    pub(crate) catch_all_source_root: SourceRootId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct AppDataId(pub u32);

impl AppStructure {
    pub fn add_app_data(
        &mut self,
        source_root_id: SourceRootId,
        app_data: Option<AppData>,
        applicable_files: Option<ApplicableFiles>,
    ) {
        let prev = self
            .app_map
            .insert(source_root_id, (app_data, applicable_files));
        assert!(prev.is_none());
    }
    pub fn add_project_data(&mut self, project_id: ProjectId, project_data: ProjectData) {
        let prev = self.project_map.insert(project_id, project_data);
        assert!(prev.is_none());
    }

    /// Set the salsa inputs according to this AppStructure
    pub fn apply(
        self,
        db: &mut dyn SourceDatabaseExt,
        resolve_file_id: &impl Fn(&AbsPathBuf) -> Option<FileId>,
    ) -> FxHashMap<AbsPathBuf, AppDataId> {
        let mut app_index = AppDataIndex::default();
        let mut app_data_id = AppDataId(0);
        let mut unresolved_paths = FxHashMap::default();
        for (source_root_id, (data, applicable_files)) in self.app_map {
            let arc_data = data.map(Arc::new);
            db.set_app_data_by_id(app_data_id, arc_data);
            db.set_app_data_id(source_root_id, app_data_id);
            applicable_files.map(|files| {
                files.iter().for_each(|path| {
                    if let Some(file_id) = resolve_file_id(path) {
                        app_index.map.insert(file_id, app_data_id);
                    } else {
                        unresolved_paths.insert(path.clone(), app_data_id);
                    }
                })
            });
            app_data_id = AppDataId(app_data_id.0 + 1);
        }
        for (project_id, project_data) in self.project_map {
            db.set_project_data(project_id, Arc::new(project_data));
        }
        db.set_app_index(Arc::new(app_index));
        db.set_catch_all_source_root(self.catch_all_source_root);

        unresolved_paths
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct AppRoots {
    otp: Option<Arc<AppRoots>>,
    app_map: FxHashMap<AppName, SourceRootId>,
}

impl AppRoots {
    pub fn insert(&mut self, app: AppName, source_root_id: SourceRootId) {
        self.app_map.insert(app, source_root_id);
    }

    pub fn set_otp(&mut self, otp: Option<Arc<AppRoots>>) {
        self.otp = otp;
    }

    pub fn get<Q: ?Sized>(&self, app: &Q) -> Option<SourceRootId>
    where
        AppName: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.app_map
            .get(app)
            .cloned()
            .or_else(|| self.otp.as_ref().and_then(|otp| otp.get(app)))
    }
}

// ---------------------------------------------------------------------
/// Currently we don't eqWAlize OTP
/// Historical reasons:
/// - We used to not support the full language. Now we may actually be able to eqWAlize OTP if we could get the project model
/// - OTP code is written very differently from WhatsApp code, so we didn't want to bias our trade-offs at the beginning.
/// It could change in future
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncludeOtp {
    Yes,
    No,
}

#[derive(Debug)]
pub struct ProjectApps<'a> {
    /// All the applications in a set of projects.  The order here
    /// will correspond with the vfs sourceRootId's
    pub all_apps: Vec<(ProjectId, &'a ProjectAppData)>,
    /// Sometimes we don't have an OTP project because we are explicitly
    /// opting out of using it, e.g. for eqWAlizer compatibility
    pub otp_project_id: Option<ProjectId>,
    // We store the original projects to we can make the AppStructure later
    pub projects: Vec<Project>,
}

impl<'a> ProjectApps<'a> {
    pub fn new(projects: &'a [Project], include_otp: IncludeOtp) -> ProjectApps<'a> {
        let mut all_apps: Vec<(ProjectId, &ProjectAppData)> = projects
            .iter()
            .enumerate()
            .flat_map(|(project_idx, project)| {
                project
                    .all_apps()
                    .into_iter()
                    .map(move |p| (ProjectId(project_idx as u32), p))
            })
            .collect();

        // We assume that all of the `Project`s use the same OTP.
        // And so we extract the OTP apps from the first one, and put
        // them into a `Project` specifically for OTP, so we do not
        // duplicate it for every project. .
        let first_project = &projects[0];
        let mut projects: Vec<_> = projects.into();
        let otp_project_id = if include_otp == IncludeOtp::Yes {
            let otp_project_id = ProjectId(projects.len() as u32);
            let mut all_otp_apps: Vec<(ProjectId, &ProjectAppData)> = first_project
                .otp_apps()
                .map(|app| (otp_project_id, app))
                .collect();
            all_apps.append(&mut all_otp_apps);
            // The only part of this we (currently) use in
            // ProjectApps::app_structure() is Project.otp
            let otp_project = Project::otp(
                first_project.otp.clone(),
                first_project.otp_apps().cloned().collect(),
            );
            projects.push(otp_project);
            Some(otp_project_id)
        } else {
            None
        };

        ProjectApps {
            all_apps,
            otp_project_id,
            projects,
        }
    }

    pub fn app_structure(&self) -> AppStructure {
        let mut app_structure = AppStructure::default();
        let mut app_idx = 0;

        // Reconstruct the per-project list
        let mut apps_by_project: FxHashMap<ProjectId, Vec<&ProjectAppData>> = FxHashMap::default();

        for (project_id, appdata) in self.all_apps.iter() {
            apps_by_project
                .entry(*project_id)
                .or_default()
                .push(appdata);
        }

        let mut project_root_map = app_source_roots(&self.all_apps);
        let otp_root = self
            .otp_project_id
            .and_then(|otp_project_id| project_root_map.get(&otp_project_id))
            .cloned()
            .map(Arc::new);

        for (project_idx, project) in self.projects.iter().enumerate() {
            let project_id = ProjectId(project_idx as u32);
            let empty = vec![];
            let apps = apps_by_project.get(&project_id).unwrap_or(&empty);

            let mut project_source_roots = vec![];
            for app in apps {
                let root_id = SourceRootId(app_idx);
                app_idx += 1;
                project_source_roots.push(root_id);
                let input_data = AppData {
                    project_id,
                    name: app.name.clone(),
                    dir: app.dir.clone(),
                    include_path: app.include_path.clone(),
                    extra_src_dirs: app.extra_src_dirs.clone(),
                    macros: app.macros.clone(),
                    parse_transforms: app.parse_transforms.clone(),
                    app_type: app.app_type,
                    src_path: app.abs_src_dirs.clone(),
                    ebin_path: app.ebin.clone(),
                    is_test_target: app.is_test_target.clone(),
                };
                app_structure.add_app_data(root_id, Some(input_data), app.applicable_files.clone());
            }

            let mut app_roots = project_root_map.remove(&project_id).unwrap_or_default();

            if self.otp_project_id != Some(project_id) {
                app_roots.set_otp(otp_root.clone());
            }

            let project_data = ProjectData {
                source_roots: project_source_roots,
                root_dir: project.root().into_owned(),
                deps_ebins: project.deps_ebins(),
                otp_project_id: self.otp_project_id,
                app_roots,
                eqwalizer_config: project.eqwalizer_config.clone(),
            };
            app_structure.add_project_data(project_id, project_data);
        }

        // Final SourceRoot for out-of-project files
        log::info!("Final source root: {:?}", SourceRootId(app_idx));
        app_structure.add_app_data(SourceRootId(app_idx), None, None);
        app_structure.catch_all_source_root = SourceRootId(app_idx);
        app_structure
    }
}

fn app_source_roots(all_apps: &[(ProjectId, &ProjectAppData)]) -> FxHashMap<ProjectId, AppRoots> {
    let mut app_source_roots: FxHashMap<ProjectId, AppRoots> = FxHashMap::default();

    for (idx, (project_id, app)) in all_apps.iter().enumerate() {
        let source_root_id = SourceRootId(idx as u32);
        app_source_roots
            .entry(*project_id)
            .or_default()
            .insert(app.name.clone(), source_root_id);
    }
    app_source_roots
}
