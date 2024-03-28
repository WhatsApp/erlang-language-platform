/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::TryInto;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use anyhow::anyhow;
use anyhow::bail;
use anyhow::Result;
use fxhash::FxHashSet;
use lazy_static::lazy_static;
use log::debug;
use log::warn;
use parking_lot::Mutex;
use paths::AbsPath;
use paths::AbsPathBuf;

use crate::AppName;
use crate::AppType;
use crate::CommandProxy;
use crate::ProjectAppData;
use crate::ProjectModelError;

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

        check_build_info(&config)?;
        Ok(config)
    }

    pub fn rebar3_command(&self) -> CommandProxy<'_> {
        lazy_static! {
            static ref REBAR_GLOBAL_LOCK: Mutex<()> = Mutex::new(());
        }
        let guard = REBAR_GLOBAL_LOCK.lock();
        let mut cmd = Command::new("rebar3");
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

fn check_build_info(config: &RebarConfig) -> Result<()> {
    let mut cmd = config.rebar3_command();
    cmd.arg("help");
    cmd.arg("build_info");
    match cmd.output() {
        Ok(cmd) => {
            match (
                cmd.status.success(),
                cmd.status.code(),
                String::from_utf8_lossy(&cmd.stdout),
            ) {
                (true, _, _) => Ok(()),
                (_, Some(1), out) if out.contains("Unknown task build_info") => {
                    bail!(ProjectModelError::NoBuildInfo)
                }
                (_, _, out) => bail!("Failed to run rebar3 help build_info: {:?}", out),
            }
        }
        Err(error) => {
            warn!("rebar3 build_info is not available");
            debug!("rebar3 help build_info: {}", error);
            bail!(ProjectModelError::MissingRebar(error))
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
    ) -> Result<(RebarProject, PathBuf, Vec<ProjectAppData>)> {
        Self::_from_rebar_build_info(path.as_ref(), rebar_config)
    }

    fn _from_rebar_build_info(
        path: &Path,
        rebar_config: RebarConfig,
    ) -> Result<(RebarProject, PathBuf, Vec<ProjectAppData>)> {
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
                dir,
                ebin: map_pop(&mut term, "ebin")
                    .ok()
                    .and_then(|e| into_abs_path(e).ok()),
                extra_src_dirs: into_vec(map_pop(&mut term, "extra_src_dirs")?)?
                    .into_iter()
                    .map(into_string)
                    .collect::<Result<_>>()?,
                include_dirs,
                macros: into_vec(map_pop(&mut term, "macros")?)?,
                parse_transforms: into_vec(map_pop(&mut term, "parse_transforms")?)?,
                app_type: is_dep,
                include_path: vec![],
                abs_src_dirs,
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

fn into_abs_path(term: eetf::Term) -> Result<AbsPathBuf> {
    match PathBuf::from(into_string(term)?).try_into() {
        Ok(abs_path) => Ok(abs_path),
        Err(path_buf) => bail!("expected absolute path, got: {:?}", path_buf),
    }
}

fn into_vec(term: eetf::Term) -> Result<Vec<eetf::Term>> {
    match term {
        eetf::Term::List(eetf::List { elements }) => Ok(elements),
        _ => bail!("expected a list, got: {:?}", term),
    }
}
