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
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::LazyLock;
use std::sync::RwLock;

use anyhow::Result;
use anyhow::bail;
use elp_log::timeit;
use paths::AbsPathBuf;
use paths::Utf8Path;
use paths::Utf8PathBuf;

use crate::OtpConfig;
use crate::ProjectAppData;
use crate::project_cache_enabled;

static OTP_DISCOVER_CACHE: LazyLock<
    parking_lot::Mutex<fxhash::FxHashMap<String, Vec<ProjectAppData>>>,
> = LazyLock::new(|| parking_lot::Mutex::new(fxhash::FxHashMap::default()));

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Otp {
    pub lib_dir: AbsPathBuf,
}

#[cfg(buck_build)]
fn get_default_erl_path() -> String {
    buck_resources::get("whatsapp/elp/crates/project_model/erl")
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| "erl".to_string())
}

#[cfg(not(buck_build))]
fn get_default_erl_path() -> String {
    "erl".to_string()
}

pub static ERL: LazyLock<RwLock<String>> = LazyLock::new(|| RwLock::new(get_default_erl_path()));

/// All OTP apps discovered from the local OTP installation, cached for
/// the lifetime of the process so that `Otp::discover()` is only called once.
pub static OTP_APPS: LazyLock<Vec<ProjectAppData>> = LazyLock::new(|| {
    let otp_root = Otp::find_otp().expect("tests should always be able to find OTP");
    let (_otp, apps) = Otp::discover(otp_root.to_path_buf(), &OtpConfig::default());
    apps
});

pub static ERTS_APP: LazyLock<ProjectAppData> =
    LazyLock::new(|| find_otp_app("erts").expect("erts app must exist in OTP installation"));
pub static STDLIB_APP: LazyLock<ProjectAppData> =
    LazyLock::new(|| find_otp_app("stdlib").expect("stdlib app must exist in OTP installation"));
pub static KERNEL_APP: LazyLock<ProjectAppData> =
    LazyLock::new(|| find_otp_app("kernel").expect("kernel app must exist in OTP installation"));

/// Data points queried from the `erl` runtime.
#[derive(Debug, Clone)]
struct ErlSystemInfo {
    /// The OTP `lib` directory (canonicalized), i.e. what `find_otp` returns.
    lib_dir: Utf8PathBuf,
    /// `erlang:system_info(otp_release)`, e.g. "26".
    otp_release: String,
    /// `erlang:system_info(system_version)`, the human-readable banner.
    system_version: String,
}

/// Separator used between fields in the consolidated `erl` output. None of the
/// values we query (a path, an OTP release number, the version banner) contain
/// this token.
const ERL_FIELD_SEP: &str = "|ELP-SEP|";

/// Cap on the port table for the Erlang VMs we start, unless the caller knows
/// it needs fewer.
pub const DEFAULT_PORT_LIMIT: u32 = 65536;

/// Prepare a command that starts an Erlang VM, replacing the ambient
/// `ERL_*FLAGS` with a `+Q` of our own choosing.
///
/// The port table is preallocated at startup and defaults to the process file
/// descriptor limit, so on a host with a generous `ulimit -n` even a VM that
/// opens no ports pays for it. `+Q` has to travel via `ERL_FLAGS` rather than
/// argv because most of the VMs we start are escripts, where argv belongs to
/// the script rather than to the emulator.
///
/// Taking over `ERL_FLAGS` also fixes the reverse problem: `ERL_FLAGS` and
/// `ERL_ZFLAGS` are applied *after* the command line, and after an escript's
/// `%%!` emu args, so an ambient `+P`/`+Q` silently overrides whatever we ask
/// for — enough to take a trivial VM from tens of MB resident to over 2GB.
/// `ERL_AFLAGS` is applied first and so loses to our own flags, but is cleared
/// too so the VMs we start are not perturbed by the environment.
pub fn configure_erl_env(cmd: &mut Command, port_limit: u32) -> &mut Command {
    cmd.env_remove("ERL_AFLAGS")
        .env_remove("ERL_ZFLAGS")
        .env("ERL_FLAGS", format!("+Q {port_limit}"))
}

/// The probe opens no ports and spawns no processes, so both tables can sit at
/// the minimum the emulator accepts.
const PROBE_TABLE_LIMIT: u32 = 1024;

/// Run `erl` once to collect the OTP root dir, release, and system version.
fn query_erl_system_info() -> Result<ErlSystemInfo> {
    let _timer = timeit!("query erl system info");
    let eval = format!(
        "io:format(\"~s{sep}~s{sep}~s\", [code:root_dir(), erlang:system_info(otp_release), erlang:system_info(system_version)])",
        sep = ERL_FIELD_SEP
    );
    let erl = ERL.read().unwrap();
    let mut cmd = Command::new(&*erl);
    let output = configure_erl_env(&mut cmd, PROBE_TABLE_LIMIT)
        // Speed up startup on loaded machines: a single scheduler, no async
        // threads, and minimal code loading.
        .arg("+S1")
        .arg("+A0")
        // The probe spawns no processes, so pin the process table to the
        // minimum rather than paying for the default of 1M entries.
        .arg("+P")
        .arg(PROBE_TABLE_LIMIT.to_string())
        .arg("-mode")
        .arg("minimal")
        .arg("-noshell")
        .arg("-eval")
        .arg(&eval)
        .arg("-s")
        .arg("erlang")
        .arg("halt")
        .output()?;

    if !output.status.success() {
        bail!(
            "Failed to query erl system info, error code: {:?}, stderr: {:?}",
            output.status.code(),
            String::from_utf8(output.stderr)
        );
    }

    let stdout = String::from_utf8(output.stdout)?;
    let mut parts = stdout.splitn(3, ERL_FIELD_SEP);
    let (Some(root_dir), Some(otp_release), Some(system_version)) =
        (parts.next(), parts.next(), parts.next())
    else {
        bail!("Unexpected erl output, could not parse system info: {stdout:?}");
    };

    let lib_dir: Utf8PathBuf = format!("{root_dir}/lib").into();
    let lib_dir = dunce::canonicalize(lib_dir)?;
    let lib_dir = Utf8PathBuf::from_path_buf(lib_dir).expect("Could not create Utf8PathBuf");

    Ok(ErlSystemInfo {
        lib_dir,
        otp_release: otp_release.to_string(),
        system_version: system_version.to_string(),
    })
}

/// Find an OTP app's ProjectAppData by app name (e.g., "stdlib").
/// Returns None if the app is not found in the OTP installation.
/// Uses the cached `OTP_APPS` list to avoid repeated filesystem discovery.
pub fn find_otp_app(app_name: &str) -> Option<ProjectAppData> {
    OTP_APPS
        .iter()
        .find(|app| app.name.as_str() == app_name)
        .cloned()
}

/// Read all .erl source files from an OTP app's src directories.
/// Returns Vec<(PathBuf, String)> of (path, contents) pairs.
pub fn read_otp_app_sources(app_data: &ProjectAppData) -> Vec<(PathBuf, String)> {
    let mut sources = Vec::new();
    for src_dir in &app_data.abs_src_dirs {
        if let Ok(entries) = fs::read_dir(Path::new(src_dir.as_os_str())) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().is_some_and(|ext| ext == "erl")
                    && let Ok(contents) = fs::read_to_string(&path)
                {
                    sources.push((path, contents));
                }
            }
        }
    }
    sources
}

impl Otp {
    /// The OTP `lib` directory, or an error if `erl` could not be queried.
    pub fn find_otp() -> Result<&'static Utf8Path> {
        Self::system_info().map(|info| info.lib_dir.as_path())
    }

    /// The `erlang:system_info(system_version)` banner, or an error if `erl`
    /// could not be queried.
    pub fn system_version() -> Result<&'static str> {
        Self::system_info().map(|info| info.system_version.as_str())
    }

    /// The OTP data points, queried from `erl` exactly once and cached for the
    /// lifetime of the process. Returns the original query failure on every call
    /// so accessors can degrade gracefully rather than panicking.
    fn system_info() -> Result<&'static ErlSystemInfo> {
        static ERL_SYSTEM_INFO: LazyLock<Result<ErlSystemInfo>> =
            LazyLock::new(query_erl_system_info);
        ERL_SYSTEM_INFO
            .as_ref()
            .map_err(|err| anyhow::anyhow!("{err:#}"))
    }

    /// The OTP release of the local installation (e.g. "28"), or `None` if `erl`
    /// could not be queried.
    pub fn version() -> Option<&'static str> {
        Self::system_info()
            .ok()
            .map(|info| info.otp_release.as_str())
    }

    pub fn sets_v2_not_default() -> bool {
        Self::version().map(|v| v < "28").unwrap_or(true)
    }

    pub fn epp_has_function_arity_bug() -> bool {
        Self::version().map(|v| v < "29").unwrap_or(true)
    }

    pub fn discover(path: Utf8PathBuf, otp_config: &OtpConfig) -> (Otp, Vec<ProjectAppData>) {
        let cache_key = format!("{}:{:?}", path, otp_config.exclude_apps);

        // Check cache (test-only optimisation).
        let cached = if project_cache_enabled() {
            OTP_DISCOVER_CACHE.lock().get(&cache_key).cloned()
        } else {
            None
        };

        let apps = cached.unwrap_or_else(|| {
            let apps = Self::discover_otp_apps(&path, otp_config);
            if project_cache_enabled() {
                OTP_DISCOVER_CACHE.lock().insert(cache_key, apps.clone());
            }
            apps
        });
        (
            Otp {
                lib_dir: AbsPathBuf::assert(path),
            },
            apps,
        )
    }

    fn discover_otp_apps(path: &Utf8Path, otp_config: &OtpConfig) -> Vec<ProjectAppData> {
        let exclude_apps = &otp_config.exclude_apps;
        log::info!("Loading OTP apps from {path:?}");
        if let Ok(entries) = fs::read_dir(path) {
            entries
                .into_iter()
                .filter_map(|entry| {
                    let entry = entry.ok()?;
                    let name = entry.file_name();
                    let name_str = name.to_str()?;

                    // Extract app name from versioned directory name (e.g., "megaco-4.5" -> "megaco")
                    let app_name = name_str
                        .split_once('-')
                        .map_or(name_str, |(base, _version)| base);

                    // Skip excluded applications
                    if exclude_apps.contains(&app_name.to_string()) {
                        log::info!("Excluding OTP app: {}", app_name);
                        return None;
                    }

                    let path =
                        dunce::canonicalize(entry.path()).expect("Could not canonicalize path");
                    let dir = AbsPathBuf::assert(
                        Utf8PathBuf::from_path_buf(path).expect("Could not convert to Utf8PathBuf"),
                    );
                    Some(ProjectAppData::otp_app_data(name_str, &dir))
                })
                .collect()
        } else {
            vec![]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_discover_with_excluded_apps() {
        // Create a mock OTP directory structure
        let temp_dir = tempfile::tempdir().unwrap();
        let otp_lib_dir = Utf8Path::from_path(temp_dir.path()).unwrap().join("lib");
        std::fs::create_dir_all(&otp_lib_dir).unwrap();

        // Create mock OTP app directories
        let apps = vec![
            "kernel-9.2",
            "stdlib-5.2",
            "megaco-4.5",
            "eunit-2.8.2",
            "common_test-1.25.1",
        ];

        for app in &apps {
            let app_dir = otp_lib_dir.join(app);
            std::fs::create_dir_all(app_dir.join("src")).unwrap();
            std::fs::create_dir_all(app_dir.join("include")).unwrap();
            std::fs::create_dir_all(app_dir.join("ebin")).unwrap();
        }

        // Test without exclusions
        let (_otp, all_apps) = Otp::discover(otp_lib_dir.clone(), &OtpConfig::default());
        let app_names: Vec<&str> = all_apps.iter().map(|app| app.name.as_str()).collect();
        assert!(app_names.contains(&"kernel"));
        assert!(app_names.contains(&"stdlib"));
        assert!(app_names.contains(&"megaco"));
        assert!(app_names.contains(&"eunit"));
        assert!(app_names.contains(&"common_test"));

        // Test with exclusions
        let otp_config = OtpConfig {
            exclude_apps: vec!["megaco".to_string(), "eunit".to_string()],
        };
        let (_otp, filtered_apps) = Otp::discover(otp_lib_dir, &otp_config);
        let filtered_names: Vec<&str> = filtered_apps.iter().map(|app| app.name.as_str()).collect();
        assert!(filtered_names.contains(&"kernel"));
        assert!(filtered_names.contains(&"stdlib"));
        assert!(filtered_names.contains(&"common_test"));
        assert!(!filtered_names.contains(&"megaco"));
        assert!(!filtered_names.contains(&"eunit"));

        // Verify the filtered list is smaller
        assert!(filtered_apps.len() < all_apps.len());
        assert_eq!(filtered_apps.len(), all_apps.len() - 2);
    }

    #[test]
    fn find_otp_app_stdlib() {
        // stdlib should always be available in any OTP installation
        let app = find_otp_app("stdlib");
        assert!(app.is_some(), "stdlib should be found in OTP installation");
        let app = app.unwrap();
        assert_eq!(app.name.as_str(), "stdlib");
        assert!(!app.abs_src_dirs.is_empty(), "stdlib should have src dirs");
        assert!(app.ebin.is_some(), "stdlib should have an ebin directory");
    }

    #[test]
    fn find_otp_app_nonexistent() {
        let app = find_otp_app("this_app_does_not_exist_xyz");
        assert!(app.is_none());
    }

    #[test]
    fn read_otp_app_sources_stdlib() {
        let app = find_otp_app("stdlib").expect("stdlib must be present");
        let sources = read_otp_app_sources(&app);
        assert!(!sources.is_empty(), "stdlib should have source files");

        // All returned files should be .erl files
        for (path, _contents) in &sources {
            assert_eq!(
                path.extension().and_then(|e| e.to_str()),
                Some("erl"),
                "Expected .erl file, got: {:?}",
                path
            );
        }

        // stdlib should contain well-known modules like lists.erl
        let has_lists = sources.iter().any(|(path, _)| {
            path.file_name()
                .and_then(|n| n.to_str())
                .is_some_and(|n| n == "lists.erl")
        });
        assert!(has_lists, "stdlib sources should include lists.erl");
    }
}
