/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use paths::AbsPathBuf;

use crate::AppName;
use crate::AppType;
use crate::ProjectAppData;
use crate::otp::Otp;

/// The directory of the `eqwalizer_support` app bundled with ELP, next to the
/// OTP apps. Nothing exists there on disk: ELP serves the sources from memory.
pub fn bundled_dir(otp: &Otp) -> AbsPathBuf {
    otp.lib_dir.join("eqwalizer_support")
}

/// The `eqwalizer_support` app bundled with ELP. It is loaded with the OTP apps,
/// once for all projects, so a project's own apps and modules take precedence
/// over it, as they do over OTP's, but it is not treated as part of OTP.
pub(crate) fn bundled_app(otp: &Otp) -> ProjectAppData {
    let dir = bundled_dir(otp);
    ProjectAppData {
        name: AppName("eqwalizer_support".to_string()),
        buck_target_name: None,
        dir: dir.clone(),
        include_dirs: vec![],
        abs_src_dirs: vec![dir.join("src")],
        ebin: None,
        extra_src_dirs: vec![],
        app_type: AppType::Bundled,
        macros: vec![],
        parse_transforms: vec![],
        include_path: vec![otp.lib_dir.clone()],
        gen_src_files: None,
        applicable_files: None,
        is_test_target: None,
        is_buck_generated: None,
    }
}
