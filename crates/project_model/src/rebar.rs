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
    pub apps: Vec<ProjectAppData>,
    pub deps: Vec<ProjectAppData>,
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
        CommandProxy(guard, cmd)
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
                (_, _, out) => bail!("Failed to run rebar3 help buil-info: {:?}", out),
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
    pub fn new(
        mut apps: Vec<ProjectAppData>,
        deps: Vec<ProjectAppData>,
        root: AbsPathBuf,
        rebar_config: RebarConfig,
        otp_root: &AbsPathBuf,
    ) -> Self {
        let global_includes = RebarProject::global_includes(&apps, &deps);
        for app in &mut apps {
            let mut include_paths = global_includes.clone();
            include_paths.extend(app.include_dirs());
            include_paths.push(otp_root.to_path_buf());
            app.include_path = include_paths;
        }

        Self {
            apps,
            deps,
            root,
            rebar_config,
        }
    }

    pub fn from_rebar_build_info(
        path: impl AsRef<Path>,
        rebar_config: RebarConfig,
    ) -> Result<(RebarProject, PathBuf)> {
        Self::_from_rebar_build_info(path.as_ref(), rebar_config)
    }

    fn _from_rebar_build_info(
        path: &Path,
        rebar_config: RebarConfig,
    ) -> Result<(RebarProject, PathBuf)> {
        let data = fs::read(path)?;
        let build_info = eetf::Term::decode(&*data)?;
        let otp_root = to_abs_path(map_get(&build_info, "otp_lib_dir")?)?;

        let apps = to_vec(map_get(&build_info, "apps")?)?
            .iter()
            .map(|term| to_app_data(term, AppType::App))
            .collect::<Result<_>>()?;
        let deps = to_vec(map_get(&build_info, "deps")?)?
            .iter()
            .map(|term| to_app_data(term, AppType::Dep))
            .collect::<Result<_>>()?;
        let root = to_abs_path(map_get(&build_info, "source_root")?)?;

        return Ok((
            RebarProject::new(apps, deps, root, rebar_config, &otp_root),
            otp_root.into(),
        ));

        fn to_app_data(term: &eetf::Term, is_dep: AppType) -> Result<ProjectAppData> {
            let dir = to_abs_path(map_get(term, "dir")?)?;
            let src_dirs: Vec<String> = to_vec(map_get(term, "src_dirs")?)?
                .iter()
                .map(|term| Ok(to_string(term)?.to_owned()))
                .collect::<Result<_>>()?;
            let abs_src_dirs: Vec<AbsPathBuf> = src_dirs.iter().map(|src| dir.join(src)).collect();
            let include_dirs: Vec<AbsPathBuf> = to_vec(map_get(term, "include_dirs")?)?
                .iter()
                .map(to_abs_path)
                .collect::<Result<_>>()?;
            Ok(ProjectAppData {
                name: AppName(to_string(map_get(term, "name")?)?.to_string()),
                dir,
                ebin: map_get(term, "ebin").ok().and_then(|e| to_abs_path(e).ok()),
                extra_src_dirs: to_vec(map_get(term, "extra_src_dirs")?)?
                    .iter()
                    .map(|term| Ok(to_string(term)?.to_owned()))
                    .collect::<Result<_>>()?,
                include_dirs: include_dirs.clone(),
                macros: to_vec(map_get(term, "macros")?)?.to_owned(),
                parse_transforms: to_vec(map_get(term, "parse_transforms")?)?.to_owned(),
                app_type: is_dep,
                include_path: include_dirs,
                abs_src_dirs,
            })
        }
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

//Temporary for Buck
impl Default for RebarProject {
    fn default() -> Self {
        RebarProject {
            apps: vec![],
            deps: vec![],
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

fn map_get<'a>(term: &'a eetf::Term, key: &str) -> Result<&'a eetf::Term> {
    let expected = eetf::Atom::from(key).into();
    match term {
        eetf::Term::Map(eetf::Map { entries }) => entries
            .iter()
            .find_map(|(key, value)| if key == &expected { Some(value) } else { None })
            .ok_or_else(|| anyhow!("missing key {:?}", key)),
        _ => bail!("expected a map, got: {:?}", term),
    }
}

fn to_bin(term: &eetf::Term) -> Result<&[u8]> {
    match term {
        eetf::Term::Binary(eetf::Binary { bytes }) => Ok(bytes),
        _ => bail!("expected a binary, got: {:?}", term),
    }
}

fn to_string(term: &eetf::Term) -> Result<&str> {
    Ok(std::str::from_utf8(to_bin(term)?)?)
}

fn to_abs_path(term: &eetf::Term) -> Result<AbsPathBuf> {
    match PathBuf::from(to_string(term)?).try_into() {
        Ok(abs_path) => Ok(abs_path),
        Err(path_buf) => bail!("expected absolute path, got: {:?}", path_buf),
    }
}

fn to_vec(term: &eetf::Term) -> Result<&[eetf::Term]> {
    match term {
        eetf::Term::List(eetf::List { elements }) => Ok(elements),
        _ => bail!("expected a list, got: {:?}", term),
    }
}
