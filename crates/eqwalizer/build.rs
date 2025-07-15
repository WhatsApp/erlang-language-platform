/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let source_directory = Path::new("../../../eqwalizer/eqwalizer");
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let eqwalizer_out_dir = Path::new("../../../../../buck-out/eqwalizer/scala-3.6.4");
    let dest_path = Path::new(&out_dir).join("eqwalizer");
    let extension;

    if let Some(path) = env::var_os("ELP_EQWALIZER_PATH") {
        let from = Path::new(&path);
        extension = from
            .extension()
            .unwrap_or_default()
            .to_str()
            .unwrap()
            .to_string();
        fs::copy(from, dest_path).expect("Copying precompiled eqwalizer failed");
    } else {
        extension = "".to_string();
        // Use the sbt wrapper on linux or otherwise require sbt to be installed
        let sbt = fs::canonicalize(source_directory.join("./meta/sbt.sh")).unwrap();
        let output = Command::new(sbt)
            .arg("assembly")
            .current_dir(source_directory)
            .env("EQWALIZER_USE_BUCK_OUT", "true")
            .output()
            .expect("failed to execute sbt assembly");

        if !output.status.success() {
            let stdout =
                String::from_utf8(output.stdout).expect("valid utf8 output from sbt assembly");
            let stderr =
                String::from_utf8(output.stderr).expect("valid utf8 output from sbt assembly");
            panic!("sbt assembly failed with stdout:\n{stdout}\n\nstderr:\n{stderr}");
        }

        let jar = fs::canonicalize(eqwalizer_out_dir.join("eqwalizer.jar")).unwrap();

        let native_image =
            fs::canonicalize(source_directory.join("./meta/native-image.sh")).unwrap();
        let image_path = fs::canonicalize(eqwalizer_out_dir)
            .unwrap()
            .join("eqwalizer");
        let output = Command::new(native_image)
            .current_dir(source_directory)
            .arg("-H:IncludeResources=application.conf")
            .arg("--no-server")
            .arg("--no-fallback")
            .arg("-jar")
            .arg(jar)
            .arg(&image_path)
            .output()
            .expect("failed to execute native-image");

        if !output.status.success() {
            let stdout =
                String::from_utf8(output.stdout).expect("valid utf8 output from native-image");
            let stderr =
                String::from_utf8(output.stderr).expect("valid utf8 output from native-image");
            panic!("native-image failed with stdout:\n{stdout}\n\nstderr:\n{stderr}");
        }

        fs::copy(image_path, dest_path).expect("Copying fresh eqwalizer failed");

        rerun_if_changed(source_directory.join("build.sbt"));
        rerun_if_changed(source_directory.join("src"));
    }

    println!("cargo:rustc-env=ELP_EQWALIZER_EXT={extension}");
    println!("cargo:rerun-if-env-changed=ELP_EQWALIZER_PATH");
}

fn rerun_if_changed(path: impl AsRef<Path>) {
    println!("cargo:rerun-if-changed={}", path.as_ref().display());
}
