/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

extern crate serde;

use std::fs;
use std::path::Path;

use anyhow::anyhow;
use anyhow::Result;
use eetf::Atom;
use eetf::Term;
use indexmap::indexset;
use indexmap::IndexSet;
use paths::AbsPath;
use paths::AbsPathBuf;
use serde::Deserialize;

use crate::buck;
use crate::eqwalizer_support;
use crate::AppName;
use crate::AppType;
use crate::ProjectAppData;
use crate::ProjectModelError;

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct JsonConfig {
    #[serde(default)]
    pub apps: Vec<JsonProjectAppData>,
    #[serde(default)]
    pub deps: Vec<JsonProjectAppData>,
    #[serde(skip_deserializing)]
    config_path: Option<AbsPathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct JsonProjectAppData {
    pub name: String,
    pub dir: String,
    #[serde(default = "default_src_dirs")]
    pub src_dirs: Vec<String>,
    pub ebin: Option<String>,
    #[serde(default)]
    pub extra_src_dirs: Vec<String>,
    #[serde(default)]
    pub include_dirs: Vec<String>,
    #[serde(default)]
    pub macros: Vec<String>,
}

impl JsonProjectAppData {
    pub fn to_project_app_data(&self, root_path: &AbsPath, is_dep: bool) -> Result<ProjectAppData> {
        let dir = canonicalize(root_path.join(&self.dir))?;
        let ebin = match &self.ebin {
            Some(ebin) => Some(canonicalize(dir.join(ebin))?),
            None => None,
        };
        let include_dirs = self.include_dirs.iter().map(|inc| dir.join(inc)).collect();
        let abs_src_dirs = self.src_dirs.iter().map(|src| dir.join(src)).collect();
        let macros = self
            .macros
            .iter()
            .map(|macros| Term::from(Atom::from(macros.clone())))
            .collect();
        let app_type = match is_dep {
            true => AppType::Dep,
            false => AppType::App,
        };
        Ok(ProjectAppData {
            name: AppName(self.name.clone()),
            dir,
            ebin,
            extra_src_dirs: self.extra_src_dirs.clone(),
            include_dirs,
            abs_src_dirs,
            macros,
            parse_transforms: vec![],
            app_type,
            include_path: vec![],
        })
    }
}

impl JsonConfig {
    pub fn new(
        apps: Vec<JsonProjectAppData>,
        deps: Vec<JsonProjectAppData>,
        config_path: AbsPathBuf,
    ) -> Self {
        Self {
            apps,
            deps,
            config_path: Some(config_path),
        }
    }

    pub fn try_parse(path: &AbsPath) -> Result<JsonConfig> {
        let config_content = fs::read_to_string(path)?;
        let mut config: JsonConfig = serde_json::from_str(&config_content)
            .map_err(|err| ProjectModelError::InvalidBuildInfoJson(anyhow!(err)))?;
        config.config_path = Some(path.to_path_buf());
        Ok(config)
    }

    pub fn config_path(&self) -> &AbsPath {
        self.config_path.as_ref().unwrap()
    }
}

pub(crate) fn gen_app_data(
    config: &JsonConfig,
    otp_root: &AbsPath,
) -> (
    Vec<ProjectAppData>,
    Vec<ProjectAppData>,
    Vec<Term>,
    Vec<Term>,
) {
    let mut terms = vec![];
    let mut global_includes = indexset![otp_root.to_path_buf()];
    let path = config.config_path().parent().unwrap();

    fn make_app_data(
        path: &AbsPath,
        data: &[JsonProjectAppData],
        is_dep: bool,
        terms: &mut Vec<Term>,
        global_includes: &mut IndexSet<AbsPathBuf>,
    ) -> Vec<ProjectAppData> {
        let mut result = vec![];
        for app in data {
            let app: ProjectAppData = match app.to_project_app_data(path, is_dep) {
                Ok(data) => data,
                Err(err) => {
                    log::error!("Error parsing app: {:?}. Err: {}", app, err);
                    continue;
                }
            };
            let ebin = app.ebin.clone().unwrap_or(app.dir.join("ebin"));
            let term = buck::build_info_app(&app, ebin);
            terms.push(term);
            let parent = app.dir.parent().map(|path| path.to_path_buf());
            if let Some(path) = parent {
                global_includes.insert(path);
            }
            result.push(app);
        }
        result
    }

    let mut apps = make_app_data(path, &config.apps, false, &mut terms, &mut global_includes);
    let mut deps = make_app_data(path, &config.deps, true, &mut terms, &mut global_includes);

    for app in &mut apps {
        let mut include_path = global_includes.clone();
        include_path.extend(app.include_dirs());
        app.include_path = include_path.into_iter().collect();
    }
    let (eqwalizer_support_app, eqwalizer_support_term) =
        eqwalizer_support::eqwalizer_suppport_data(otp_root);
    deps.push(eqwalizer_support_app);

    (apps, deps, terms, vec![eqwalizer_support_term])
}

fn default_src_dirs() -> Vec<String> {
    vec!["src".to_string()]
}

fn canonicalize(path: impl AsRef<Path>) -> Result<AbsPathBuf> {
    let abs = fs::canonicalize(path)?;
    Ok(AbsPathBuf::assert(abs))
}
