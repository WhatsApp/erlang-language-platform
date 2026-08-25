/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

extern crate serde;

use std::fs;

use anyhow::Context;
use anyhow::Result;
use eetf::Atom;
use eetf::Term;
use eetf::Tuple;
use fxhash::FxHashMap;
use indexmap::IndexSet;
use indexmap::indexset;
use paths::AbsPath;
use paths::AbsPathBuf;
use paths::Utf8Path;
use paths::Utf8PathBuf;
use serde::Deserialize;
use serde::Serialize;

use crate::AppName;
use crate::AppType;
use crate::ProjectAppData;

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct JsonConfig {
    #[serde(default)]
    pub apps: Vec<JsonProjectAppData>,
    #[serde(default)]
    pub deps: Vec<JsonProjectAppData>,
    #[serde(skip_deserializing, skip_serializing)]
    pub config_path: Option<AbsPathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct JsonProjectAppData {
    pub name: String,
    pub dir: String,
    #[serde(default = "default_src_dirs")]
    pub src_dirs: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ebin: Option<String>,
    #[serde(default)]
    pub extra_src_dirs: Vec<String>,
    #[serde(default)]
    pub include_dirs: Vec<String>,
    #[serde(default)]
    pub macros: FxHashMap<String, String>,
}

fn default_src_dirs() -> Vec<String> {
    vec!["src".to_string()]
}

impl JsonProjectAppData {
    pub fn to_project_app_data(&self, root_path: &AbsPath, is_dep: bool) -> Result<ProjectAppData> {
        let dir = canonicalize(root_path.join(&self.dir))
            .with_context(|| format!("Checking dir: {}", self.dir))?;
        let ebin = match &self.ebin {
            Some(ebin) => Some(
                canonicalize(dir.join(ebin))
                    .with_context(|| format!("Checking ebin dir: {ebin}"))?,
            ),
            None => None,
        };
        let include_dirs = self
            .include_dirs
            .iter()
            .map(|inc| dir.absolutize(inc))
            .collect();
        let abs_src_dirs = self.src_dirs.iter().map(|src| dir.join(src)).collect();
        let macros = self
            .macros
            .iter()
            .map(|(key, value)| {
                let elements: Vec<Term> = vec![
                    Term::from(Atom::from(key.clone())),
                    Term::from(Atom::from(value.clone())),
                ];
                Term::from(Tuple::from(elements))
            })
            .collect();
        let app_type = match is_dep {
            true => AppType::Dep,
            false => AppType::App,
        };
        Ok(ProjectAppData {
            name: AppName(self.name.clone()),
            buck_target_name: None,
            dir,
            ebin,
            extra_src_dirs: self.extra_src_dirs.clone(),
            include_dirs,
            abs_src_dirs,
            macros,
            parse_transforms: vec![],
            app_type,
            include_path: vec![],
            gen_src_files: None,
            applicable_files: None,
            is_test_target: None,
            is_buck_generated: None,
        })
    }

    pub fn from_project_app_data(
        root: &AbsPathBuf,
        project_app_data: &ProjectAppData,
    ) -> JsonProjectAppData {
        JsonProjectAppData {
            name: project_app_data.name.0.clone(),
            dir: abs_path_buf_to_relative_string(&project_app_data.dir, root),
            src_dirs: project_app_data
                .abs_src_dirs
                .iter()
                .map(|p| abs_path_buf_to_relative_string(p, &project_app_data.dir))
                .collect(),
            ebin: project_app_data
                .ebin
                .clone()
                .map(|p| abs_path_buf_to_relative_string(&p, &project_app_data.dir)),
            extra_src_dirs: project_app_data.extra_src_dirs.clone(),
            include_dirs: project_app_data
                .include_dirs
                .iter()
                .map(|p| abs_path_buf_to_relative_string(p, &project_app_data.dir))
                .collect(),
            macros: project_app_data.macros.iter().map(convert_macro).collect(),
        }
    }
}

fn convert_macro(mac: &eetf::Term) -> (String, String) {
    match mac {
        Term::Atom(atom) => (atom.name.clone(), "true".to_string()),
        Term::Tuple(tuple) => match &tuple.elements[..] {
            [Term::Atom(key), Term::Atom(value)] => (key.name.clone(), value.name.to_string()),
            _ => panic!("Invalid macro in ${tuple}"),
        },
        term => panic!("Term not supported for macro definition: {term}"),
    }
}

fn abs_path_buf_to_relative_string(abs_path: &AbsPathBuf, base: &AbsPathBuf) -> String {
    if let Some(relative) = abs_path.strip_prefix(base) {
        relative.as_str().to_string()
    } else {
        // `abs_path` is outside `base`: climb from `base` up to their common
        // ancestor, then descend into `abs_path`. Use path components rather
        // than splitting the string on '/', so the result is also correct for
        // non-Unix separators.
        let abs_components: Vec<_> = abs_path.components().collect();
        let base_components: Vec<_> = base.components().collect();
        if abs_components.first() != base_components.first() {
            // The two paths do not share a root (e.g. different Windows
            // drives), so no relative path exists; keep the absolute one.
            return abs_path.as_str().to_string();
        }

        let common = abs_components
            .iter()
            .zip(base_components.iter())
            .take_while(|(a, b)| a == b)
            .count();

        let ups = base_components.len() - common;
        let mut parts: Vec<&str> = vec![".."; ups];
        parts.extend(abs_components[common..].iter().map(|c| c.as_str()));
        parts.join("/")
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
        let mut config: JsonConfig = serde_json::from_str(&config_content)?;
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
) -> (Vec<ProjectAppData>, Vec<ProjectAppData>) {
    let mut global_includes = indexset![otp_root.to_path_buf()];
    let path = config.config_path().parent().unwrap();

    fn make_app_data(
        path: &AbsPath,
        data: &[JsonProjectAppData],
        is_dep: bool,
        global_includes: &mut IndexSet<AbsPathBuf>,
    ) -> Vec<ProjectAppData> {
        let mut result = vec![];
        for app in data {
            let app: ProjectAppData = match app.to_project_app_data(path, is_dep) {
                Ok(data) => data,
                Err(err) => {
                    log::error!("Error parsing app: {app:?}. Err: {err}");
                    continue;
                }
            };
            let parent = app.dir.parent().map(|path| path.to_path_buf());
            if let Some(path) = parent {
                global_includes.insert(path);
            }
            result.push(app);
        }
        result
    }

    let mut apps = make_app_data(path, &config.apps, false, &mut global_includes);
    let mut deps = make_app_data(path, &config.deps, true, &mut global_includes);

    for app in apps.iter_mut().chain(deps.iter_mut()) {
        let mut include_path = global_includes.clone();
        include_path.extend(app.include_dirs());
        app.include_path = include_path.into_iter().collect();
    }
    (apps, deps)
}

pub fn canonicalize(path: impl AsRef<Utf8Path>) -> Result<AbsPathBuf> {
    let abs = dunce::canonicalize(path.as_ref())?;
    Ok(AbsPathBuf::assert(
        Utf8PathBuf::from_path_buf(abs).expect("Could not convert to UTF8"),
    ))
}

#[cfg(test)]
mod tests {
    use elp_base_db::assert_eq_expected;

    use super::*;
    use crate::temp_dir::TempDir;

    fn abs(p: &str) -> AbsPathBuf {
        AbsPathBuf::assert(Utf8PathBuf::from(p))
    }

    /// `abs_path_buf_to_relative_string` must emit a real `../`-relative path
    /// when `abs_path` is not under `base`, not a root-relative string.
    #[test]
    fn relative_string_outside_base() {
        // Under-base: unchanged behavior (strip_prefix happy path).
        assert_eq_expected!(
            "d",
            abs_path_buf_to_relative_string(&abs("/a/b/c/d"), &abs("/a/b/c"))
        );
        // Two levels up: include dir shared at the project root.
        assert_eq_expected!(
            "../../inc",
            abs_path_buf_to_relative_string(&abs("/a/b/inc"), &abs("/a/b/apps/appA"))
        );
        // Three levels up: the base has 5 components (a, b, apps, appA, src) and
        // shares only 2 with the absolute path (a, b), so ups = 5 - 2 = 3.
        assert_eq_expected!(
            "../../../other",
            abs_path_buf_to_relative_string(&abs("/a/b/other"), &abs("/a/b/apps/appA/src"))
        );
        // Disjoint roots: up every remaining base component, then descend.
        assert_eq_expected!(
            "../../x/inc",
            abs_path_buf_to_relative_string(&abs("/x/inc"), &abs("/a/b"))
        );
        // Equal paths: the relative path is empty.
        assert_eq_expected!(
            "",
            abs_path_buf_to_relative_string(&abs("/a/b"), &abs("/a/b"))
        );
        // `abs_path` is an ancestor of `base`: climb up to it.
        assert_eq_expected!(
            "..",
            abs_path_buf_to_relative_string(&abs("/a"), &abs("/a/b"))
        );
    }

    /// The regression this PR fixes: an include dir that lives outside the app
    /// directory must survive the JSON round-trip exactly.
    #[test]
    fn include_dirs_round_trip_outside_app_dir() {
        let temp = TempDir::new();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).expect("temp dir is utf8");
        let root = canonicalize(root).unwrap();
        fs::create_dir_all(root.join("apps/appA")).unwrap();

        let app_dir = root.join("apps/appA");
        let expected_inc = root.join("inc");
        let project_data = ProjectAppData::fixture_app_data(
            AppName("appA".to_string()),
            app_dir.clone(),
            vec![expected_inc.clone()],
            vec![app_dir.join("src")],
            vec![],
        );

        let json = JsonProjectAppData::from_project_app_data(&root, &project_data);
        assert_eq_expected!("apps/appA", json.dir);
        assert_eq_expected!(vec!["../../inc".to_string()], json.include_dirs);

        let round_tripped = json.to_project_app_data(&root, false).unwrap();
        assert_eq_expected!(vec![expected_inc], round_tripped.include_dirs);
    }
}
