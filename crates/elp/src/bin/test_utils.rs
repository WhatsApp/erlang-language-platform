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

use expect_test::ExpectFile;
use expect_test::expect_file;

// Snapshot dir from `ELP_RESOURCE_DIR`, set by both the Buck and cargo pipelines.
// The `expect[update]` value is repo-relative, so absolutize it against CWD.
pub fn resource_dir() -> PathBuf {
    let dir = PathBuf::from(std::env::var("ELP_RESOURCE_DIR").expect("ELP_RESOURCE_DIR not set"));
    if dir.is_absolute() {
        dir
    } else {
        std::env::current_dir().unwrap().join(dir)
    }
}

pub fn resource_file(name: &str) -> ExpectFile {
    expect_file!(resource_dir().join(name))
}

pub fn project_path(project: &str) -> PathBuf {
    #[cfg(buck_build)]
    let test_projects = buck_resources::get("whatsapp/elp/crates/elp/test_projects")
        .unwrap()
        .join("test/test_projects");

    #[cfg(not(buck_build))]
    let test_projects = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../test/test_projects");

    let project_path = test_projects.join(project);
    dunce::canonicalize(&project_path).unwrap_or_else(|_| {
        panic!(
            "Failed to canonicalize project path: {}",
            project_path.display()
        )
    })
}
