/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use anyhow::Result;
use anyhow::anyhow;
use anyhow::bail;
use fxhash::FxHashSet;
use lazy_static::lazy_static;
use parking_lot::Mutex;
use paths::AbsPath;
use paths::AbsPathBuf;
use paths::Utf8PathBuf;
use semver::Version;
use semver::VersionReq;

use crate::AppName;
use crate::AppType;
use crate::CommandProxy;
use crate::ProjectAppData;
use crate::ProjectModelError;

pub const REQUIRED_REBAR3_VERSION: &str = ">=3.24.0";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RebarProject {
    pub root: AbsPathBuf,
    pub(crate) rebar_config: RebarConfig,
}

/// corresponds to rebar profile
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Profile(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RebarConfig {
    pub config_file: AbsPathBuf, // rebar config or build_info
    pub profile: Profile,
}

impl Default for Profile {
    fn default() -> Self {
        Self("test".to_string())
    }
}

impl RebarConfig {
    pub fn from_config_path(config_file: AbsPathBuf, profile: Profile) -> Result<RebarConfig> {
        let config = RebarConfig {
            config_file,
            profile,
        };

        check_version(&config)?;
        Ok(config)
    }

    pub fn rebar3_command(&self) -> CommandProxy<'_> {
        lazy_static! {
            static ref REBAR_GLOBAL_LOCK: Mutex<()> = Mutex::new(());
        }
        let guard = REBAR_GLOBAL_LOCK.lock();

        let mut cmd = if cfg!(target_os = "windows") {
            let mut cmd = Command::new("cmd");
            cmd.args(["/C", "rebar3"]);
            cmd
        } else {
           Command::new("rebar3")
        };
        
        cmd.arg("as");
        cmd.arg(&self.profile.0);
        if let Some(parent) = self.config_file.parent() {
            cmd.current_dir(parent);
        }
        CommandProxy::new(guard, cmd)
    }

    pub fn config_path(&self) -> &AbsPath {
        &self.config_file
    }
}

fn rebar3_version(config: &RebarConfig) -> Result<String> {
    let mut cmd = config.rebar3_command();
    cmd.arg("version");
    let output = cmd.output()?;
    match (
        output.status.success(),
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
    ) {
        (true, Some(0), version) => {
            // Sometimes the rebar3 output contains information other than the version,
            // which is guaranteed to be the last line.
            let version = version
                .lines()
                .last()
                .ok_or(ProjectModelError::RebarVersionError {
                    error: format!("Cannot extract version line from {}", version.clone()),
                })?;
            Ok(version.into())
        }
        (_, _, out) => {
            bail!(ProjectModelError::RebarVersionError {
                error: out.to_string()
            })
        }
    }
}

fn check_version(config: &RebarConfig) -> Result<bool> {
    let version = rebar3_version(config)?;
    let required = VersionReq::parse(REQUIRED_REBAR3_VERSION)?;
    let version = Version::parse(version.split(' ').nth(1).ok_or(
        ProjectModelError::RebarVersionError {
            error: format!("Cannot extract version from {}", version.clone()),
        },
    )?)
    .map_err(|err| ProjectModelError::RebarVersionError {
        error: format!("{} Version: {}", err, version.clone()),
    })?;
    match required.matches(&version) {
        true => Ok(true),
        false => {
            bail!(ProjectModelError::RebarVersionTooOld {
                expected: required.to_string(),
                actual: version.to_string()
            })
        }
    }
}

impl RebarProject {
    pub fn new(root: AbsPathBuf, rebar_config: RebarConfig) -> Self {
        Self { root, rebar_config }
    }

    pub fn from_rebar_build_info(
        path: impl AsRef<Path>,
        rebar_config: RebarConfig,
    ) -> Result<(RebarProject, Utf8PathBuf, Vec<ProjectAppData>)> {
        Self::_from_rebar_build_info(path.as_ref(), rebar_config)
    }

    fn _from_rebar_build_info(
        path: &Path,
        rebar_config: RebarConfig,
    ) -> Result<(RebarProject, Utf8PathBuf, Vec<ProjectAppData>)> {
        let data = fs::read(path)?;
        let mut build_info = eetf::Term::decode(&*data)?;
        let otp_root = into_abs_path(map_pop(&mut build_info, "otp_lib_dir")?)?;

        let apps: Vec<_> = into_vec(map_pop(&mut build_info, "apps")?)?
            .into_iter()
            .map(|term| into_app_data(term, AppType::App))
            .collect::<Result<_>>()?;
        let deps: Vec<_> = into_vec(map_pop(&mut build_info, "deps")?)?
            .into_iter()
            .map(|term| into_app_data(term, AppType::Dep))
            .collect::<Result<_>>()?;
        let root = into_abs_path(map_pop(&mut build_info, "source_root")?)?;

        let mut apps_with_includes = RebarProject::add_app_includes(apps, &deps, &otp_root);
        let deps_with_includes = RebarProject::add_app_includes(deps.clone(), &deps, &otp_root);

        apps_with_includes.extend(deps_with_includes);
        return Ok((
            RebarProject::new(root, rebar_config),
            otp_root.into(),
            apps_with_includes,
        ));

        fn into_app_data(mut term: eetf::Term, is_dep: AppType) -> Result<ProjectAppData> {
            let dir = into_abs_path(map_pop(&mut term, "dir")?)?;
            let abs_src_dirs: Vec<AbsPathBuf> = into_vec(map_pop(&mut term, "src_dirs")?)?
                .into_iter()
                .map(|term| Ok(dir.join(into_string(term)?)))
                .collect::<Result<_>>()?;
            let include_dirs: Vec<AbsPathBuf> = into_vec(map_pop(&mut term, "include_dirs")?)?
                .into_iter()
                .map(into_abs_path)
                .collect::<Result<_>>()?;
            Ok(ProjectAppData {
                name: AppName(into_string(map_pop(&mut term, "name")?)?),
                buck_target_name: None,
                dir,
                ebin: map_pop(&mut term, "ebin")
                    .ok()
                    .and_then(|e| into_abs_path(e).ok()),
                extra_src_dirs: into_vec(map_pop(&mut term, "extra_src_dirs")?)?
                    .into_iter()
                    .map(into_string)
                    .collect::<Result<_>>()?,
                include_dirs,
                macros: into_vec(map_pop(&mut term, "macros")?)?
                    .into_iter()
                    .map(|term: eetf::Term| into_tuple(term))
                    .collect::<Result<_>>()?,
                parse_transforms: into_vec(map_pop(&mut term, "parse_transforms")?)?,
                app_type: is_dep,
                include_path: vec![],
                abs_src_dirs,
                applicable_files: None,
                is_test_target: None,
            })
        }
    }

    pub fn add_app_includes(
        mut apps: Vec<ProjectAppData>,
        deps: &[ProjectAppData],
        otp_root: &AbsPathBuf,
    ) -> Vec<ProjectAppData> {
        let global_includes = RebarProject::global_includes(&apps, deps);
        for app in &mut apps {
            let mut include_paths = global_includes.clone();
            include_paths.extend(app.include_dirs());
            include_paths.push(otp_root.to_path_buf());
            app.include_path = include_paths;
        }
        apps
    }

    /// Replicates behaviour of -include_lib through
    /// the -include fallback in a regularly structured
    /// rebar3 project without compiling modules
    fn global_includes(apps: &[ProjectAppData], deps: &[ProjectAppData]) -> Vec<AbsPathBuf> {
        apps.iter()
            .chain(deps)
            .filter_map(|app| app.dir.parent())
            .map(AbsPath::to_path_buf)
            .collect::<FxHashSet<_>>()
            .into_iter()
            .collect()
    }
}

//Temporary for Project::empty()
impl Default for RebarProject {
    fn default() -> Self {
        RebarProject {
            root: AbsPathBuf::assert("/".into()).normalize(),
            rebar_config: Default::default(),
        }
    }
}

impl Default for RebarConfig {
    fn default() -> Self {
        RebarConfig {
            config_file: AbsPathBuf::assert("/".into()).normalize(),
            profile: Default::default(),
        }
    }
}

fn map_pop(term: &mut eetf::Term, key: &str) -> Result<eetf::Term> {
    let expected = eetf::Atom::from(key).into();
    match term {
        eetf::Term::Map(eetf::Map { map }) => map
            .remove(&expected)
            .ok_or_else(|| anyhow!("missing key {:?} in {:?}", key, map)),
        _ => bail!("expected a map, got: {:?}", term),
    }
}

fn into_bin(term: eetf::Term) -> Result<Vec<u8>> {
    match term {
        eetf::Term::Binary(eetf::Binary { bytes }) => Ok(bytes),
        _ => bail!("expected a binary, got: {:?}", term),
    }
}

fn into_string(term: eetf::Term) -> Result<String> {
    Ok(String::from_utf8(into_bin(term)?)?)
}

fn into_tuple(mut term: eetf::Term) -> Result<eetf::Term> {
    let key = map_pop(&mut term, "key")?;
    let value = map_pop(&mut term, "value")?;
    Ok(eetf::Term::Tuple(eetf::Tuple::from(vec![key, value])))
}

fn into_abs_path(term: eetf::Term) -> Result<AbsPathBuf> {
    let path_buf = PathBuf::from(into_string(term)?);
    let path = if fs::metadata(&path_buf).is_ok() {
        match fs::canonicalize::<PathBuf>(path_buf) {
            Ok(path) => path,
            Err(err) => bail!("expected absolute path, got: {:?}", err),
        }
    } else {
        path_buf
    };
    Ok(AbsPathBuf::assert(
        Utf8PathBuf::from_path_buf(path).expect("Could not convert to Utf8PathBuf"),
    ))
}

fn into_vec(term: eetf::Term) -> Result<Vec<eetf::Term>> {
    match term {
        eetf::Term::List(eetf::List { elements }) => Ok(elements),
        _ => bail!("expected a list, got: {:?}", term),
    }
}
