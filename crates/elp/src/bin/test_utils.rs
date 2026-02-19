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

#[cfg(buck_build)]
pub fn get_resources_dir() -> PathBuf {
    let resource_path = buck_resources::get("whatsapp/elp/crates/elp/test_resources")
        .unwrap()
        .join("src/resources/test");
    std::fs::canonicalize(&resource_path).unwrap_or_else(|_| {
        panic!(
            "Failed to canonicalize resources path: {}",
            resource_path.display()
        )
    })
}

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
        let canonical = std::fs::canonicalize(&project_path).unwrap_or_else(|_| {
            panic!(
                "Failed to canonicalize project path: {}",
                project_path.display()
            )
        });
        canonical.to_string_lossy().to_string()
    }

    #[cfg(not(buck_build))]
    {
        let project_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("..")
            .join("test")
            .join("test_projects")
            .join(project);
        dbg!(&project_path);
        let canonical = std::fs::canonicalize(&project_path).unwrap_or_else(|_| {
            panic!(
                "Failed to canonicalize project path: {}",
                project_path.display()
            )
        });
        canonical.to_string_lossy().to_string()
    }
}

macro_rules! resource_file {
    ($filename:expr) => {
        expect_file!(crate::test_utils::get_resources_dir().join($filename))
    };
}

pub(crate) use resource_file;
