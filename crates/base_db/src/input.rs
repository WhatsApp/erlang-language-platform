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
use paths::Utf8Path;
use salsa::plumbing::AsId;
use url::Url;
use vfs::AbsPath;
use vfs::AbsPathBuf;
use vfs::FileId;
use vfs::VfsPath;
use vfs::file_set::FileSet;

use crate::File;
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

    pub fn file_info(
        &self,
        db: &dyn RootQueryDb,
        file_id: FileId,
        app: App,
    ) -> Option<(FileId, FileSource, &VfsPath)> {
        let path = self.path_for_file(&file_id)?;
        let source = app.file_source(db, path)?;
        Some((file_id, source, path))
    }
}

/// The loaded projects.
#[salsa::input(singleton, debug)]
pub struct Workspace {
    /// In the order the loader gave them, then the OTP projects.
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
    /// The project's own apps, sorted by the key lowering gives each app so
    /// that identical reloads compare equal, then the apps of its OTP project.
    #[returns(ref)]
    pub apps: Arc<[App]>,
    /// The inputs of the project's apps that are no longer loaded, with their
    /// large fields cleared, for lowering to give back to an app that returns.
    /// No query reads them.
    #[returns(ref)]
    pub removed_apps: Arc<[App]>,
    /// The OTP project of the project's OTP installation, whose own link is to
    /// itself; `None` when OTP isn't loaded.
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

#[salsa::input]
pub struct App {
    pub project: Project,
    #[returns(ref)]
    pub name: AppName,
    pub app_type: AppType,
    #[returns(ref)]
    pub dir: VfsPath,
    /// The files the loader lists for the app, sorted by path. Only the Buck
    /// loaders list them.
    #[returns(ref)]
    pub files: Arc<[File]>,
    #[returns(ref)]
    pub macros: Arc<[eetf::Term]>,
    #[returns(ref)]
    pub parse_transforms: Arc<[eetf::Term]>,
    /// The directory of the app's compiled modules, set only for apps whose
    /// compiled modules ELP reads: OTP apps. eqWAlizer reads their types from
    /// the BEAM files, and hover docs come from their EEP-48 chunks.
    #[returns(ref)]
    pub ebin: Option<AbsPathBuf>,
    /// The URL the documentation pages of the app's modules, `<module>.html`,
    /// are under.
    #[returns(ref)]
    pub docs_url: Option<Arc<Url>>,
    #[returns(ref)]
    pub buck_target_name: Option<TargetFullName>,
    #[returns(ref)]
    pub include_dirs: Arc<[AbsPathBuf]>,
    #[returns(ref)]
    pub include_path: Arc<[AbsPathBuf]>,
    #[returns(ref)]
    pub src_dirs: Arc<[AbsPathBuf]>,
    #[returns(ref)]
    pub extra_src_dirs: Arc<[String]>,
    pub is_test_target: Option<bool>,
}

impl App {
    /// The app's directory, which lowering gives as a real path.
    pub fn dir_path(self, db: &dyn RootQueryDb) -> &AbsPath {
        as_real_path(self.dir(db))
    }

    /// Whether `path` is one of the app's sources or extra sources, or neither.
    pub(crate) fn file_source(self, db: &dyn RootQueryDb, path: &VfsPath) -> Option<FileSource> {
        if self.is_src_file(db, path) {
            Some(FileSource::Src)
        } else if self.is_extra_src_file(db, path) {
            Some(FileSource::Extra)
        } else if self.is_test_target(db) == Some(true) {
            // buck test target source file. It only has one.
            Some(FileSource::Extra)
        } else {
            None
        }
    }

    fn is_src_file(self, db: &dyn RootQueryDb, path: &VfsPath) -> bool {
        if let Some(path) = path.as_path() {
            // src_dirs are recursive, check path begins with one
            return self
                .src_dirs(db)
                .iter()
                .any(|src_dir| path.starts_with(src_dir));
        }
        false
    }

    pub(crate) fn is_extra_src_file(self, db: &dyn RootQueryDb, path: &VfsPath) -> bool {
        if let Some(path) = path
            .as_path()
            .and_then(|path| path.strip_prefix(self.dir_path(db)))
        {
            // extra_src_dirs are not recursive, check parent dir is one
            let path: &Utf8Path = path.as_ref();
            if let Some(parent) = path.parent() {
                return self
                    .extra_src_dirs(db)
                    .iter()
                    .any(|src_dir| parent == Path::new(src_dir));
            }
        }
        false
    }
}

#[salsa::tracked]
impl Project {
    /// The project's root, which lowering gives as a real path.
    pub fn root_dir(self, db: &dyn RootQueryDb) -> &AbsPath {
        as_real_path(self.root(db))
    }

    /// The app `name` refers to in the project: the last of the project's own
    /// apps with that name, else of its OTP apps.
    #[salsa::tracked]
    pub fn app_named(self, db: &dyn RootQueryDb, name: AppName) -> Option<App> {
        self.apps(db)
            .iter()
            .rev()
            .filter(|app| *app.name(db) == name)
            .min_by_key(|app| app.project(db) != self)
            .copied()
    }
}

// The id, and the name while a database is attached, as it is inside every
// query. Not salsa's derived `Debug`: an app's project lists the app, and the
// derived one would follow that cycle.
impl fmt::Debug for App {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut app = f.debug_tuple("App");
        app.field(&self.as_id());
        if let Some(name) = salsa::with_attached_database(|db| self.name(db).0.clone()) {
            app.field(&name);
        }
        app.finish()
    }
}

// Only the id, for the same reason as `App`.
impl fmt::Debug for Project {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Project").field(&self.as_id()).finish()
    }
}

/// Lowering gives apps and projects only real paths.
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
    /// The loaded projects, then an OTP project for each OTP installation they
    /// use. Sometimes we don't have OTP projects because we are explicitly
    /// opting out of using them, e.g. for eqWAlizer compatibility
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
            .flat_map(|(project_idx, project)| {
                project
                    .project_apps
                    .iter()
                    .map(move |app| (project_idx, app))
            })
            .collect();

        // The projects using an OTP installation share one OTP project, with
        // the OTP apps of the first of them, so that no project duplicates it.
        let mut otp_projects: Vec<elp_project_model::Project> = Vec::new();
        if include_otp == IncludeOtp::Yes {
            for project in projects {
                if otp_projects.iter().any(|otp| otp.otp == project.otp) {
                    continue;
                }
                let otp_project_idx = projects.len() + otp_projects.len();
                all_apps.extend(project.otp_apps.iter().map(|app| (otp_project_idx, app)));
                otp_projects.push(elp_project_model::Project::otp(
                    project.otp.clone(),
                    project.otp_apps.clone(),
                ));
            }
        }
        let projects = projects.iter().cloned().chain(otp_projects).collect();

        ProjectApps { all_apps, projects }
    }
}
