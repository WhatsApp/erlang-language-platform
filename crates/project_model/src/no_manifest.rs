/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;

use paths::AbsPath;
use paths::AbsPathBuf;

use crate::AppName;
use crate::AppType;
use crate::ProjectAppData;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NoManifestConfig {
    pub root_path: AbsPathBuf,
    pub config_path: AbsPathBuf,
    pub name: AppName,
    pub abs_src_dirs: Vec<AbsPathBuf>,
    pub include_dirs: Vec<AbsPathBuf>,
    pub extra_src_dirs: Vec<String>,
}

impl NoManifestConfig {
    pub fn new(root_path: AbsPathBuf, name: AppName, abs_src_dirs: Vec<AbsPathBuf>) -> Self {
        let config_path = root_path.join(".static");
        let include_path = root_path.join("include");
        let test_path = root_path.join("test");
        let mut include_dirs = vec![];
        let mut extra_src_dirs = vec![];
        if fs::metadata(&include_path).is_ok() {
            include_dirs.push(include_path);
        }
        if fs::metadata(test_path).is_ok() {
            extra_src_dirs.push("test".to_string());
        }

        Self {
            root_path,
            config_path,
            name,
            abs_src_dirs,
            include_dirs,
            extra_src_dirs,
        }
    }

    pub fn config_path(&self) -> &AbsPath {
        &self.config_path
    }

    pub fn to_project_app_data(&self, otp_root: &AbsPath) -> Vec<ProjectAppData> {
        let mut data = ProjectAppData {
            name: self.name.clone(),
            dir: self.root_path.clone(),
            include_dirs: self.include_dirs.clone(),
            abs_src_dirs: self.abs_src_dirs.clone(),
            ebin: None,
            extra_src_dirs: self.extra_src_dirs.clone(),
            app_type: AppType::App,
            macros: vec![],
            parse_transforms: vec![],
            include_path: vec![otp_root.to_path_buf()],
            applicable_files: None,
        };
        data.include_path.extend(data.include_dirs());
        if let Some(path) = self.root_path.parent() {
            data.include_path.push(path.to_path_buf());
        }
        vec![data]
    }
}
