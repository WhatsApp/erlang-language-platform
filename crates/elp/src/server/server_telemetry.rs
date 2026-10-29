/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Telemetry helpers for the LSP server.

use std::cmp::Reverse;

use elp_ide::elp_ide_db::elp_base_db::ProjectApps;
use elp_log::telemetry;
use elp_project_model::Project;
use elp_project_model::ProjectBuildData;
use elp_project_model::buck::BuckQueryConfig;
use itertools::Itertools;

use crate::reload::project_root_watch_paths;
use crate::reload::watch_paths_for_app;

pub(crate) fn send_project_size(
    project_apps: &ProjectApps,
    projects: &[Project],
    watch_count: usize,
    buck_query_config: BuckQueryConfig,
) {
    let details: Vec<serde_json::Value> = project_apps
        .projects
        .iter()
        .enumerate()
        .filter_map(|(idx, project)| {
            if Some(idx) == project_apps.otp_project_idx {
                return None;
            }

            let mut app_watch_counts: Vec<(&str, usize)> = project_apps
                .all_apps
                .iter()
                .filter(|(project_idx, _)| *project_idx == idx)
                .map(|(_, app)| {
                    (
                        app.name.as_str(),
                        watch_paths_for_app(app, idx, project_apps.otp_project_idx).len(),
                    )
                })
                .collect();

            let app_count = app_watch_counts.len();

            let (build_watch_dirs, build_type) = match &project.project_build_data {
                ProjectBuildData::Buck(buck_project) => {
                    let dirs = buck_project.buck_conf.included_target_dirs();
                    if dirs.is_empty() {
                        (vec![project.root().to_string()], "buck_fallback")
                    } else {
                        let dir_strs: Vec<String> = dirs.iter().map(|d| d.to_string()).collect();
                        (dir_strs, "buck_scoped")
                    }
                }
                _ => (vec![project.root().to_string()], "other"),
            };

            let watch_count: usize = app_watch_counts.iter().map(|(_, c)| *c).sum::<usize>()
                + project_root_watch_paths(&project.root()).len();

            app_watch_counts.sort_by_key(|b| Reverse(b.1));
            let top_apps: Vec<_> = app_watch_counts
                .into_iter()
                .take(5)
                .map(|(name, count)| serde_json::json!({"name": name, "watch_count": count}))
                .collect();

            Some(serde_json::json!({
                "root": project.root().to_string(),
                "app_count": app_count,
                "watch_count": watch_count,
                "build_type": build_type,
                "build_watch_dirs": build_watch_dirs,
                "top_apps": top_apps,
            }))
        })
        .sorted_by(|a, b| {
            b["watch_count"]
                .as_u64()
                .unwrap_or(0)
                .cmp(&a["watch_count"].as_u64().unwrap_or(0))
        })
        .collect();

    let data = serde_json::json!({
        "app_count": project_apps.all_apps.len(),
        "project_count": projects.len(),
        "watch_count": watch_count,
        "query_config": buck_query_config.to_string(),
        "details": details,
    });

    telemetry::send("project_size".to_string(), data);
}
