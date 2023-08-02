/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use anyhow::bail;
use anyhow::Result;
use elp_log::timeit;
use paths::AbsPathBuf;

use crate::ProjectAppData;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Otp {
    pub lib_dir: AbsPathBuf,
    pub apps: Vec<ProjectAppData>,
}

impl Otp {
    pub fn find_otp() -> Result<PathBuf> {
        let _timer = timeit!("find otp");
        let output = Command::new("erl")
            .arg("-noshell")
            .arg("-eval")
            .arg("io:format('~s', [code:root_dir()])")
            .arg("-s")
            .arg("erlang")
            .arg("halt")
            .output()?;

        if !output.status.success() {
            bail!(
                "Failed to get OTP dir, error code: {:?}, stderr: {:?}",
                output.status.code(),
                String::from_utf8(output.stderr)
            );
        }
        let path = String::from_utf8(output.stdout)?;
        let result: PathBuf = format!("{}/lib", path).into();
        Ok(result)
    }

    pub fn discover(path: PathBuf) -> Otp {
        let apps = Self::discover_otp_apps(&path);
        Otp {
            lib_dir: AbsPathBuf::assert(path),
            apps,
        }
    }

    fn discover_otp_apps(path: &Path) -> Vec<ProjectAppData> {
        log::info!("Loading OTP apps from {:?}", path);
        if let Ok(entries) = fs::read_dir(path) {
            entries
                .into_iter()
                .filter_map(|entry| {
                    let entry = entry.ok()?;
                    let name = entry.file_name();
                    let dir = AbsPathBuf::assert(entry.path());
                    Some(ProjectAppData::otp_app_data(name.to_str()?, dir))
                })
                .collect()
        } else {
            vec![]
        }
    }

    /// Used to combine more than one OTP definition in a test suite
    pub fn combine(&mut self, other: Otp) {
        self.apps.extend(other.apps);
    }
}
