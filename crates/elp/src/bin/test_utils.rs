/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::path::PathBuf;

// @fb-only: #[rustfmt::skip]
// @fb-only: #[cfg(buck_build)]
// @fb-only: pub fn get_resources_dir() -> PathBuf { crate::meta_only::get_resources_dir() }

#[cfg(not(buck_build))]
pub fn get_resources_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src")
        .join("resources")
        .join("test")
}

pub fn project_path(project: &str) -> String {
    #[cfg(buck_build)]
    {
        let project_path = buck_resources::get("whatsapp/elp/crates/elp/test_projects")
            .unwrap()
            .join("test")
            .join("test_projects")
            .join(project);
        let canonical = dunce::canonicalize(&project_path).unwrap_or_else(|_| {
            panic!(
                "Failed to canonicalize project path: {}",
                project_path.display()
            )
        });
        canonical.to_string_lossy().to_string()
    }

    #[cfg(not(buck_build))]
    {
        let project_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../test/test_projects")
            .join(project);
        dunce::canonicalize(&project_path)
            .unwrap_or_else(|_| {
                panic!(
                    "Failed to canonicalize project path: {}",
                    project_path.display()
                )
            })
            .to_string_lossy()
            .to_string()
    }
}

macro_rules! resource_file {
    ($filename:expr) => {
        expect_file!(crate::test_utils::get_resources_dir().join($filename))
    };
}

pub(crate) use resource_file;
