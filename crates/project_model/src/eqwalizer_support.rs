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

use anyhow::Result;
use dirs;
use include_dir::Dir;
use lazy_static::lazy_static;
use paths::AbsPath;
use paths::AbsPathBuf;
use paths::Utf8PathBuf;

use crate::AppName;
use crate::AppType;
use crate::ProjectAppData;

lazy_static! {
    pub static ref EQWALIZER_SUPPORT: Utf8PathBuf = dirs::cache_dir()
        .map(|d| Utf8PathBuf::from_path_buf(d).ok())
        .unwrap()
        .expect("Could not get cache dir")
        .join("elp")
        .join("eqwalizer_support");
}

pub(crate) fn eqwalizer_suppport_data(otp_root: &AbsPath) -> ProjectAppData {
    let eqwalizer_support = AbsPathBuf::assert(EQWALIZER_SUPPORT.to_path_buf());

    ProjectAppData {
        name: AppName("eqwalizer_support".to_string()),
        buck_target_name: None,
        dir: eqwalizer_support.clone(),
        include_dirs: vec![],
        abs_src_dirs: vec![eqwalizer_support.join("src")],
        ebin: None,
        extra_src_dirs: vec![],
        app_type: AppType::App,
        macros: vec![],
        parse_transforms: vec![],
        include_path: vec![otp_root.to_path_buf()],
        gen_src_files: None,
        applicable_files: None,
        is_test_target: None,
        is_buck_generated: None,
    }
}

pub fn setup_eqwalizer_support(project_dir: &Dir) -> Result<()> {
    if fs::metadata(&*EQWALIZER_SUPPORT).is_err() {
        fs::create_dir_all(&*EQWALIZER_SUPPORT)?;
        project_dir.extract(&*EQWALIZER_SUPPORT)?;
    }
    Ok(())
}
