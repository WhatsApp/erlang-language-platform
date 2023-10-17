/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::iter;
use std::mem;
use std::ops::Deref;
use std::ops::DerefMut;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;

use anyhow::bail;
use anyhow::Context;
use anyhow::Result;
use buck::EqwalizerConfig;
use elp_log::timeit;
use lazy_static::lazy_static;
use parking_lot::MutexGuard;
use paths::AbsPath;
use paths::AbsPathBuf;
use tempfile::NamedTempFile;
use tempfile::TempPath;

use crate::buck::BuckProject;
use crate::json::JsonConfig;
use crate::otp::Otp;
use crate::rebar::Profile;
use crate::rebar::RebarConfig;
use crate::rebar::RebarProject;

pub mod buck;
pub mod json;
pub mod otp;
pub mod rebar;

pub struct CommandProxy<'a>(MutexGuard<'a, ()>, Command);

impl<'a> Deref for CommandProxy<'a> {
    type Target = Command;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<'a> DerefMut for CommandProxy<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

#[derive(Debug, Clone)]
pub struct DiscoverConfig {
    pub rebar: bool,
    pub rebar_profile: Profile,
}

impl DiscoverConfig {
    pub fn new(rebar: bool, profile: &str) -> Self {
        if rebar {
            Self::rebar(Some(profile.to_owned()))
        } else {
            Self::buck()
        }
    }

    pub fn rebar(profile: Option<String>) -> Self {
        let rebar_profile = profile.map(Profile).unwrap_or_default();
        Self {
            rebar: true,
            rebar_profile,
        }
    }

    pub fn buck() -> DiscoverConfig {
        Self {
            rebar: false,
            rebar_profile: Default::default(),
        }
    }

    pub fn to_rebar(mut self) -> Self {
        self.rebar = true;
        self
    }

    pub fn manifest_files(&self) -> &Vec<&'static str> {
        lazy_static! {
            static ref BUCK_MANIFEST_FILES: Vec<&'static str> = vec![".elp.toml"];
            static ref REBAR_MANIFEST_FILES: Vec<&'static str> =
                vec!["rebar.config", "rebar.config.script"];
        }
        if self.rebar {
            &REBAR_MANIFEST_FILES
        } else {
            &BUCK_MANIFEST_FILES
        }
    }
}

impl Default for DiscoverConfig {
    fn default() -> Self {
        DiscoverConfig::rebar(None)
    }
}

impl Display for DiscoverConfig {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.rebar {
            write!(f, "rebar --profile {}", self.rebar_profile.0)
        } else {
            write!(f, "buck")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProjectManifest {
    Rebar(RebarConfig),
    Toml(buck::ElpConfig),
    Json(JsonConfig),
}

impl ProjectManifest {
    pub fn root(&self) -> &AbsPath {
        match self {
            ProjectManifest::Rebar(conf) => conf.config_path(),
            ProjectManifest::Toml(conf) => conf.config_path(),
            ProjectManifest::Json(conf) => conf.config_path(),
        }
    }

    fn find_in_dir<'a>(
        path: &'a Path,
        manifests: &'a [&str],
    ) -> impl Iterator<Item = AbsPathBuf> + 'a {
        let mut curr: Option<&Path> = Some(path);
        let ancestors = iter::from_fn(move || {
            let next = curr.and_then(|path| path.parent());
            mem::replace(&mut curr, next)
        });
        ancestors.flat_map(|path| {
            manifests
                .iter()
                .map(|file| path.join(file))
                .filter(|file| file.exists())
                .map(AbsPathBuf::assert)
        })
    }

    pub fn discover_rebar(
        path: &AbsPath,
        profile: Option<Profile>,
    ) -> Result<Option<ProjectManifest>> {
        let _timer = timeit!("discover rebar");
        let path =
            Self::find_in_dir(path.as_ref(), &vec!["rebar.config", "rebar.config.script"]).last();
        if let Some(path) = path {
            let rebar = RebarConfig::from_config_path(path, profile.unwrap_or_default())?;
            Ok(Some(ProjectManifest::Rebar(rebar)))
        } else {
            Ok(None)
        }
    }

    fn discover_toml(path: &AbsPath) -> Result<Option<ProjectManifest>> {
        let _timer = timeit!("discover toml");
        let toml_path = Self::find_in_dir(path.as_ref(), &vec![".elp.toml"]).next();
        if let Some(path) = toml_path {
            let toml = buck::ElpConfig::try_parse(&path)?;
            Ok(Some(ProjectManifest::Toml(toml)))
        } else {
            Ok(None)
        }
    }

    fn discover_static(path: &AbsPath) -> Result<Option<ProjectManifest>> {
        let _timer = timeit!("discover static");
        let json_path = Self::find_in_dir(path.as_ref(), &vec!["build_info.json"]).next();
        if let Some(path) = json_path {
            let json = json::JsonConfig::try_parse(&path)?;
            Ok(Some(ProjectManifest::Json(json)))
        } else {
            Ok(None)
        }
    }

    pub fn discover(path: &AbsPath) -> Result<Option<ProjectManifest>> {
        let _timer = timeit!("discover all projects");
        if let Some(t) = Self::discover_toml(path)? {
            return Ok(Some(t));
        };
        if let Some(r) = Self::discover_rebar(path, None)? {
            return Ok(Some(r));
        };
        if let Some(s) = Self::discover_static(path)? {
            return Ok(Some(s));
        }
        Ok(None)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProjectBuildData {
    Otp,
    Rebar(RebarProject),
    Buck(BuckProject),
    Static(StaticProject),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StaticProject {
    pub apps: Vec<ProjectAppData>,
    pub deps: Vec<ProjectAppData>,
    pub config_path: AbsPathBuf,
}

#[derive(Clone)]
pub struct Project {
    build_info_file: Option<BuildInfoFile>,
    pub otp: Otp,
    pub project_build_data: ProjectBuildData,
}

#[derive(Clone)]
pub struct BuildInfoFile(Arc<TempPath>);

impl BuildInfoFile {
    pub fn build_info_file(&self) -> AbsPathBuf {
        let BuildInfoFile(loaded) = &self;
        AbsPathBuf::assert(loaded.to_path_buf())
    }
}

impl PartialEq for Project {
    fn eq(&self, other: &Self) -> bool {
        // Explicitly ignore build_info field - if we inferred the same data after loading
        // from rebar, it's enough for equality comparison
        self.otp == other.otp && self.project_build_data == other.project_build_data
    }
}

impl Project {
    pub fn new(build_info: BuildInfoFile, otp: Otp, rebar: RebarProject) -> Self {
        Self {
            build_info_file: Some(build_info),
            otp,
            project_build_data: ProjectBuildData::Rebar(rebar),
        }
    }

    pub fn otp(otp: Otp) -> Self {
        Self {
            build_info_file: None,
            otp,
            project_build_data: ProjectBuildData::Otp,
        }
    }

    pub fn empty(otp: Otp) -> Self {
        Self {
            build_info_file: None,
            otp,
            project_build_data: ProjectBuildData::Rebar(Default::default()),
        }
    }

    pub fn add_app(&mut self, app: ProjectAppData) {
        match self.project_build_data {
            ProjectBuildData::Otp => unimplemented!(),
            ProjectBuildData::Rebar(ref mut rebar) => rebar.apps.push(app),
            ProjectBuildData::Buck(_) => unimplemented!(),
            ProjectBuildData::Static(_) => unimplemented!(),
        }
    }

    pub fn all_apps(&self) -> Vec<&ProjectAppData> {
        match &self.project_build_data {
            ProjectBuildData::Otp => self.otp.apps.iter().collect(),
            ProjectBuildData::Rebar(rebar) => rebar.apps.iter().chain(rebar.deps.iter()).collect(),
            ProjectBuildData::Buck(buck) => buck.project_app_data.iter().collect(),
            ProjectBuildData::Static(stat) => stat.apps.iter().chain(stat.deps.iter()).collect(),
        }
    }

    pub fn root(&self) -> Cow<AbsPathBuf> {
        match &self.project_build_data {
            ProjectBuildData::Otp => Cow::Borrowed(&self.otp.lib_dir),
            ProjectBuildData::Rebar(rebar) => Cow::Borrowed(&rebar.root),
            ProjectBuildData::Buck(buck) => buck.config.buck.source_root(),
            ProjectBuildData::Static(stat) => Cow::Borrowed(&stat.config_path),
        }
    }

    pub fn name(&self) -> String {
        let root = self.root();
        root.file_name()
            .map(|name| name.to_string_lossy().to_string())
            .unwrap_or_else(move || root.as_os_str().to_string_lossy().to_string())
    }

    pub fn build_info_file(&self) -> Option<AbsPathBuf> {
        self.build_info_file
            .as_ref()
            .map(|info| info.build_info_file())
    }

    pub fn deps_ebins(&self) -> Vec<AbsPathBuf> {
        match &self.project_build_data {
            ProjectBuildData::Otp => vec![],
            ProjectBuildData::Rebar(rebar) => {
                rebar.deps.iter().flat_map(|app| app.ebin.clone()).collect()
            }
            ProjectBuildData::Buck(buck) => buck
                .target_info
                .targets
                .values()
                .flat_map(|target| &target.ebin)
                .cloned()
                .collect(),
            ProjectBuildData::Static(stat) => stat
                .deps
                .iter()
                .flat_map(|app| &app.ebin)
                .cloned()
                .collect(),
        }
    }

    pub fn eqwalizer_config(&self) -> EqwalizerConfig {
        match &self.project_build_data {
            ProjectBuildData::Buck(buck) => buck.config.eqwalizer.clone(),
            ProjectBuildData::Otp => EqwalizerConfig::default(),
            ProjectBuildData::Rebar(_) => EqwalizerConfig::default(),
            ProjectBuildData::Static(_) => EqwalizerConfig::default(),
        }
    }
}

impl fmt::Debug for Project {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Project")
            .field("otp", &self.otp)
            .field("project_build_info", &self.project_build_data)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AppName(pub String);

impl AppName {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Display for AppName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Borrow<str> for AppName {
    fn borrow(&self) -> &str {
        &self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AppType {
    App,
    Dep,
    Otp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectAppData {
    pub name: AppName,
    pub dir: AbsPathBuf,
    pub ebin: Option<AbsPathBuf>,
    pub extra_src_dirs: Vec<String>,
    //usually single include dir
    pub include_dirs: Vec<AbsPathBuf>,
    pub abs_src_dirs: Vec<AbsPathBuf>,
    pub macros: Vec<eetf::Term>,
    pub parse_transforms: Vec<eetf::Term>,
    pub app_type: AppType,
    //list of directories required by module to compile
    //usually includes all dependencies include paths and otp
    pub include_path: Vec<AbsPathBuf>,
}

impl ProjectAppData {
    pub fn fixture_app_data(
        name: AppName,
        dir: AbsPathBuf,
        include_dirs: Vec<AbsPathBuf>,
        src_dirs: Vec<AbsPathBuf>,
        extra_src_dirs: Vec<String>,
    ) -> ProjectAppData {
        ProjectAppData {
            name,
            ebin: Some(dir.join("ebin")),
            extra_src_dirs,
            include_dirs,
            dir,
            macros: vec![],
            parse_transforms: vec![],
            app_type: AppType::App,
            include_path: vec![],
            abs_src_dirs: src_dirs,
        }
    }

    pub fn otp_app_data(versioned_name: &str, dir: AbsPathBuf) -> Self {
        let name = versioned_name
            .split_once('-')
            .map_or(versioned_name, |(base, _version)| base);
        let parent = dir.parent().unwrap_or(dir.as_path()).to_path_buf();
        let src = dir.join(PathBuf::from("src"));
        let include = dir.join("include");
        let abs_src_dir = dir.join("src");
        Self {
            name: AppName(name.to_string()),
            ebin: Some(dir.join("ebin")),
            extra_src_dirs: vec![],
            // This makes sure files in ./include are loaded into VFS
            include_dirs: vec![include.clone()],
            dir,
            macros: vec![],
            parse_transforms: vec![],
            app_type: AppType::Otp,
            include_path: vec![include, src, parent],
            abs_src_dirs: vec![abs_src_dir],
        }
    }

    pub fn include_dirs(&self) -> Vec<AbsPathBuf> {
        self.include_dirs
            .iter()
            .chain(self.abs_src_dirs.iter())
            .cloned()
            .collect()
    }

    /// Source directories for the application including the extra
    /// sources and includes
    pub fn all_source_dirs(&self) -> Vec<AbsPathBuf> {
        self.extra_src_dirs
            .iter()
            .map(|src_dir| self.dir.join(src_dir))
            .chain(self.abs_src_dirs.iter().cloned())
            .chain(self.include_dirs.iter().cloned())
            .collect()
    }

    /// Combine the info from the other ProjectAppData into this one
    pub fn combine(&mut self, other: ProjectAppData) {
        self.abs_src_dirs.extend(other.abs_src_dirs);
        self.abs_src_dirs.dedup();
        self.extra_src_dirs.extend(other.extra_src_dirs);
        self.extra_src_dirs.dedup();
        self.include_dirs.extend(other.include_dirs);
        self.include_dirs.dedup();
        self.macros.extend(other.macros);
        self.macros.dedup();
        self.parse_transforms.extend(other.parse_transforms);
        self.parse_transforms.dedup();
    }
}

impl Project {
    pub fn compile_deps(&self) -> Result<()> {
        let _timer = timeit!("compile deps");
        match &self.project_build_data {
            ProjectBuildData::Otp => Ok(()),
            ProjectBuildData::Rebar(rebar) => {
                let mut cmd = rebar.rebar_config.rebar3_command();
                cmd.arg("compile");
                cmd.arg("--deps_only");

                let _ = utf8_stdout(&mut cmd)?;
                Ok(())
            }
            ProjectBuildData::Buck(_) => Ok(()),
            ProjectBuildData::Static(_) => Ok(()),
        }
    }

    pub fn load(manifest: ProjectManifest) -> Result<Project> {
        let (project_build_info, build_info, otp_root) = match manifest {
            ProjectManifest::Rebar(ref rebar_setting) => {
                let _timer = timeit!(
                    "load project from rebar config {}",
                    rebar_setting.config_file.display()
                );
                let rebar_version = {
                    let mut cmd = Command::new("rebar3");
                    cmd.arg("version");
                    utf8_stdout(&mut cmd)?
                };

                let loaded = Project::load_rebar_build_info(rebar_setting).with_context(|| {
                    format!(
                        "Failed to read rebar build info for config file {}, {}",
                        rebar_setting.config_file.display(),
                        rebar_version
                    )
                })?;
                let (rebar_project, otp_root) =
                    RebarProject::from_rebar_build_info(&loaded, rebar_setting.clone())
                        .with_context(|| {
                            format!(
                                "Failed to decode rebar build info for config file {:?}",
                                manifest
                            )
                        })?;
                (
                    ProjectBuildData::Rebar(rebar_project),
                    BuildInfoFile(Arc::new(loaded)),
                    otp_root,
                )
            }
            ProjectManifest::Toml(ref config) => {
                let (project, build_info, otp_root) = BuckProject::load_from_config(config)?;
                (ProjectBuildData::Buck(project), build_info, otp_root)
            }
            ProjectManifest::Json(ref config) => {
                let otp_root = Otp::find_otp()?;
                let config_path = config.config_path().to_path_buf();
                let (apps, deps, terms) = json::gen_app_data(config, AbsPath::assert(&otp_root));
                let build_info_term = buck::make_build_info(terms, vec![], &otp_root, &config_path);
                let build_info = buck::save_build_info(build_info_term)?;
                let project = StaticProject {
                    apps,
                    deps,
                    config_path,
                };
                (ProjectBuildData::Static(project), build_info, otp_root)
            }
        };

        Ok(Project {
            build_info_file: Some(build_info),
            otp: Otp::discover(otp_root),
            project_build_data: project_build_info,
        })
    }

    fn load_rebar_build_info(build: &RebarConfig) -> Result<TempPath> {
        let out_file = NamedTempFile::new()?.into_temp_path();
        let mut cmd = build.rebar3_command();
        cmd.arg("build_info");
        cmd.arg("--to");
        cmd.arg(&out_file);

        let _ = utf8_stdout(&mut cmd)?;

        Ok(out_file)
    }
}

pub fn utf8_stdout(cmd: &mut Command) -> Result<String> {
    let output = cmd.output().with_context(|| format!("{:?} failed", cmd))?;
    let stdout = String::from_utf8(output.stdout)?;
    if !output.status.success() {
        match String::from_utf8(output.stderr) {
            Ok(stderr) if !stderr.is_empty() => {
                bail!(
                    "{:?} failed, {}\nstdout:\n{}\nstderr:\n{}",
                    cmd,
                    output.status,
                    stdout,
                    stderr
                )
            }
            _ => bail!("{:?} failed, {}\nstdout:{}", cmd, output.status, stdout),
        }
    }
    Ok(stdout.trim().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_discover() {
        let root = AbsPathBuf::assert(Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures"));
        let manifest = ProjectManifest::discover_rebar(&root.join("nested"), None);
        match manifest {
            Ok(Some(ProjectManifest::Rebar(RebarConfig {
                config_file: actual,
                profile: _,
                features: _,
            }))) => {
                let expected = root.join("rebar.config.script");
                assert_eq!(actual, expected);
            }
            _ => {
                unimplemented!()
            }
        }
    }
}
