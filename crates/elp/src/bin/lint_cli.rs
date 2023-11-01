/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::str;
use std::sync::Arc;

use anyhow::bail;
use anyhow::Result;
use elp::build::load;
use elp::build::types::LoadResult;
use elp::cli::Cli;
use elp::convert;
use elp::document::Document;
use elp::otp_file_to_ignore;
use elp::read_lint_config_file;
use elp::LintConfig;
use elp_eqwalizer::Mode;
use elp_ide::diagnostics;
use elp_ide::diagnostics::group_label_ignore;
use elp_ide::diagnostics::DiagnosticCode;
use elp_ide::diagnostics::DiagnosticsConfig;
use elp_ide::diagnostics::LabeledDiagnostics;
use elp_ide::diff::diff_from_textedit;
use elp_ide::diff::DiffRange;
use elp_ide::elp_ide_assists::Assist;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::Change;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FilePosition;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::ModuleName;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::Vfs;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_ide::elp_ide_db::LineCol;
use elp_ide::Analysis;
use elp_ide::AnalysisHost;
use elp_project_model::AppName;
use elp_project_model::AppType;
use elp_project_model::DiscoverConfig;
use fxhash::FxHashSet;
use hir::FormIdx;
use hir::InFile;
use indicatif::ParallelProgressIterator;
use rayon::prelude::ParallelBridge;
use rayon::prelude::ParallelIterator;
use text_edit::TextSize;

use crate::args::Lint;
use crate::reporting;

pub fn lint_all(args: &Lint, cli: &mut dyn Cli) -> Result<()> {
    log::info!("Loading project at: {:?}", args.project);
    let config = DiscoverConfig::new(args.rebar, &args.profile);
    let mut loaded = load::load_project_at(cli, &args.project, config, IncludeOtp::Yes, Mode::Cli)?;

    if let Some(to) = &args.to {
        fs::create_dir_all(to)?
    };

    do_codemod(cli, &mut loaded, args)
}

fn do_parse_all(
    cli: &dyn Cli,
    analysis: &Analysis,
    project_id: &ProjectId,
    config: &DiagnosticsConfig,
    include_generated: bool,
    include_ct_diagnostics: bool,
    include_edoc_diagnostics: bool,
    ignore_apps: &[String],
) -> Result<Vec<(String, FileId, LabeledDiagnostics<diagnostics::Diagnostic>)>> {
    let module_index = analysis.module_index(*project_id).unwrap();
    let module_iter = module_index.iter_own();

    let ignored_apps: FxHashSet<Option<Option<AppName>>> = ignore_apps
        .iter()
        .map(|name| Some(Some(AppName(name.to_string()))))
        .collect();
    let pb = cli.progress(module_iter.len() as u64, "Parsing modules (parallel)");

    Ok(module_iter
        .par_bridge()
        .progress_with(pb)
        .map_with(
            analysis.clone(),
            |db, (module_name, _file_source, file_id)| {
                if !otp_file_to_ignore(db, file_id)
                    && db.file_app_type(file_id).ok() != Some(Some(AppType::Dep))
                    && !ignored_apps.contains(&db.file_app_name(file_id).ok())
                {
                    do_parse_one(
                        db,
                        config,
                        file_id,
                        module_name.as_str(),
                        include_generated,
                        include_ct_diagnostics,
                        include_edoc_diagnostics,
                    )
                    .unwrap()
                } else {
                    None
                }
            },
        )
        .flatten()
        .collect())
}

fn do_parse_one(
    db: &Analysis,
    config: &DiagnosticsConfig,
    file_id: FileId,
    name: &str,
    include_generated: bool,
    include_ct_diagnostics: bool,
    include_edoc_diagnostics: bool,
) -> Result<Option<(String, FileId, LabeledDiagnostics<diagnostics::Diagnostic>)>> {
    let mut diagnostics = db.diagnostics(config, file_id, include_generated)?;

    if include_ct_diagnostics {
        let ct_diagnostics = db
            .ct_diagnostics(file_id)?
            .into_iter()
            .map(|d| (d.range, d));
        diagnostics.extend(ct_diagnostics);
    }
    if include_edoc_diagnostics {
        let edoc_diagnostics = db
            .edoc_diagnostics(file_id)?
            .into_iter()
            .filter(|(f, _)| *f == file_id)
            .flat_map(|(_, ds)| ds.into_iter().map(|d| (d.range, d)));
        diagnostics.extend(edoc_diagnostics);
    }

    if !diagnostics.is_empty() {
        let res = (name.to_string(), file_id, diagnostics);
        Ok(Some(res))
    } else {
        Ok(None)
    }
}

// ---------------------------------------------------------------------

pub fn do_codemod(cli: &mut dyn Cli, loaded: &mut LoadResult, args: &Lint) -> Result<()> {
    // First check if we are doing a codemod. We need to have a whole
    // bunch of args set
    match args {
        Lint {
            recursive,
            in_place,
            diagnostic_ignore,
            diagnostic_filter,
            line_from,
            line_to,
            ignore_apps,
            read_config,
            config_file,
            project,
            ..
        } => {
            let cfg_from_file = if *read_config || config_file.is_some() {
                read_lint_config_file(project, config_file)?
            } else {
                LintConfig::default()
            };
            let mut allowed_diagnostics: FxHashSet<DiagnosticCode> = cfg_from_file
                .enabled_lints
                .into_iter()
                .collect::<FxHashSet<_>>();
            let mut disabled_diagnostics: FxHashSet<DiagnosticCode> =
                cfg_from_file.disabled_lints.into_iter().collect();

            if let Some(diagnostic_ignore) = diagnostic_ignore {
                let diagnostic_ignore = DiagnosticCode::from(diagnostic_ignore.as_str());
                // Make sure we do not mask the one we explicitly asked for
                allowed_diagnostics.remove(&diagnostic_ignore);
                disabled_diagnostics.insert(diagnostic_ignore);
            }

            if let Some(diagnostic_filter) = diagnostic_filter {
                let diagnostic_filter = DiagnosticCode::from(diagnostic_filter.as_str());
                // Make sure we do not mask the one we explicitly asked for
                disabled_diagnostics.remove(&diagnostic_filter);
                allowed_diagnostics.insert(diagnostic_filter);
            }

            // Make sure the enabled ones win out over disabled if a lint appears in both
            disabled_diagnostics.retain(|d| !allowed_diagnostics.contains(d));

            if allowed_diagnostics.is_empty() {
                bail!("No diagnostics enabled. Use --diagnostic-filter to specify one.");
            }

            let mut cfg = DiagnosticsConfig::default();
            cfg.disable_experimental = args.experimental_diags;
            cfg.disabled = disabled_diagnostics;
            let cfg = cfg.from_config(&Arc::new(cfg_from_file.ad_hoc_lints));
            // Declare outside the block so it has the right lifetime for filter_diagnostics
            let res;
            let mut diags = {
                // We put this in its own block so they analysis is
                // freed before we apply lints. To apply lints
                // recursively, we need to update the underlying
                // ananalysis_host, which will deadlock if there is
                // still an active analysis().
                let analysis = loaded.analysis();

                let (file_id, name) = match &args.module {
                    Some(module) => {
                        if args.is_format_normal() {
                            writeln!(cli, "module specified: {}", module)?;
                        }
                        let file_id = analysis.module_file_id(loaded.project_id, module)?;
                        (file_id, analysis.module_name(file_id.unwrap())?)
                    }
                    None => match &args.file {
                        Some(file_name) => {
                            if args.is_format_normal() {
                                writeln!(cli, "file specified: {}", file_name)?;
                            }
                            let path_buf = fs::canonicalize(file_name).unwrap();
                            let path = AbsPath::assert(&path_buf);
                            let path = path.as_os_str().to_str().unwrap();
                            (
                                loaded
                                    .vfs
                                    .file_id(&VfsPath::new_real_path(path.to_string())),
                                path_buf
                                    .as_path()
                                    .file_name()
                                    .map(|n| ModuleName::new(n.to_str().unwrap())),
                            )
                        }
                        None => (None, None),
                    },
                };

                res = match (file_id, name) {
                    (None, _) => do_parse_all(
                        cli,
                        &analysis,
                        &loaded.project_id,
                        &cfg,
                        args.include_generated,
                        args.include_ct_diagnostics,
                        args.include_edoc_diagnostics,
                        ignore_apps,
                    )?,
                    (Some(file_id), Some(name)) => do_parse_one(
                        &analysis,
                        &cfg,
                        file_id,
                        &name,
                        args.include_generated,
                        args.include_ct_diagnostics,
                        args.include_edoc_diagnostics,
                    )?
                    .map_or(vec![], |x| vec![x]),
                    (Some(file_id), _) => {
                        panic!("Could not get name from file_id for {:?}", file_id)
                    }
                };

                filter_diagnostics(
                    &analysis,
                    &args.module,
                    Some(&allowed_diagnostics),
                    *line_from,
                    *line_to,
                    &res,
                    &FxHashSet::default(),
                )?
            };
            if diags.is_empty() {
                if args.is_format_normal() {
                    writeln!(cli, "No diagnostics reported")?;
                }
            } else {
                diags.sort_by(|(a, _, _), (b, _, _)| a.cmp(b));
                let mut err_in_diag = false;
                if args.is_format_json() {
                    for (_name, file_id, diags) in &diags {
                        if args.print_diags {
                            for diag in diags {
                                if let diagnostics::Severity::Error = diag.severity {
                                    err_in_diag = true;
                                };
                                let vfs_path = loaded.vfs.file_path(*file_id);
                                let analysis = loaded.analysis();
                                let root_path = &analysis
                                    .project_data(*file_id)
                                    .unwrap_or_else(|_err| panic!("could not find project data"))
                                    .unwrap_or_else(|| panic!("could not find project data"))
                                    .root_dir;
                                let relative_path =
                                    reporting::get_relative_path(root_path, &vfs_path);
                                print_diagnostic_json(
                                    diag,
                                    &analysis,
                                    *file_id,
                                    relative_path,
                                    cli,
                                )?;
                            }
                        }
                    }
                } else {
                    writeln!(cli, "Diagnostics reported in {} modules:", diags.len())?;

                    for (name, file_id, diags) in &diags {
                        writeln!(cli, "  {}: {}", name, diags.len())?;
                        if args.print_diags {
                            for diag in diags {
                                if let diagnostics::Severity::Error = diag.severity {
                                    err_in_diag = true;
                                };
                                print_diagnostic(diag, &loaded.analysis(), *file_id, cli)?;
                            }
                        }
                    }
                }
                if args.apply_fix {
                    let mut changed_files = FxHashSet::default();
                    let mut lints = Lints::new(
                        &mut loaded.analysis_host,
                        &cfg,
                        &mut loaded.vfs,
                        &args.to,
                        args.include_generated,
                        args.include_ct_diagnostics,
                        args.include_edoc_diagnostics,
                        *in_place,
                        *recursive,
                        &mut changed_files,
                        diags,
                    );
                    match lints.apply_relevant_fixes(args.is_format_normal(), cli) {
                        Ok(_) => {}
                        Err(err) => {
                            writeln!(cli, "Apply fix failed: {:?}", err).ok();
                        }
                    };
                }
                if err_in_diag {
                    bail!("Errors found")
                }
            }
            Ok(())
        }
    }
}

fn print_diagnostic(
    diag: &diagnostics::Diagnostic,
    analysis: &Analysis,
    file_id: FileId,
    cli: &mut dyn Cli,
) -> Result<(), anyhow::Error> {
    let line_index = analysis.line_index(file_id)?;
    writeln!(cli, "      {}", diag.print(&line_index))?;
    Ok(())
}

fn print_diagnostic_json(
    diagnostic: &diagnostics::Diagnostic,
    analysis: &Analysis,
    file_id: FileId,
    path: &Path,
    cli: &mut dyn Cli,
) -> Result<(), anyhow::Error> {
    let line_index = analysis.line_index(file_id)?;
    let converted_diagnostic = convert::ide_to_arc_diagnostic(&line_index, path, diagnostic);
    writeln!(
        cli,
        "{}",
        serde_json::to_string(&converted_diagnostic).unwrap_or_else(|err| panic!(
            "print_diagnostics_json failed for '{:?}': {}",
            converted_diagnostic, err
        ))
    )?;
    Ok(())
}

fn filter_diagnostics<'a>(
    db: &Analysis,
    module: &'a Option<String>,
    allowed_diagnostics: Option<&FxHashSet<DiagnosticCode>>,
    line_from: Option<u32>,
    line_to: Option<u32>,
    diags: &'a [(String, FileId, LabeledDiagnostics<diagnostics::Diagnostic>)],
    changed_forms: &FxHashSet<InFile<FormIdx>>,
) -> Result<Vec<(String, FileId, Vec<diagnostics::Diagnostic>)>> {
    Ok(diags
        .to_owned()
        .into_iter()
        .filter_map(|(m, file_id, ds)| {
            let line_index = db.line_index(file_id).ok()?;
            if module.is_none() || &Some(m.to_string()) == module {
                let ds2 = ds
                    .iter()
                    .filter(|d| {
                        let range = convert::range(&line_index, d.range);
                        let line = range.start.line;
                        let form_id = get_form_id_at_offset(db, file_id, d.range.start())
                            .map(|form_id| InFile::new(file_id, form_id));
                        diagnostic_is_allowed(d, allowed_diagnostics)
                            && check(&line_from, |l| &line >= l)
                            && check(&line_to, |l| &line <= l)
                            && check_changes(changed_forms, form_id)
                    })
                    .cloned()
                    .collect::<Vec<diagnostics::Diagnostic>>();
                if !ds2.is_empty() {
                    Some((m, file_id, ds2))
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect::<Vec<_>>())
}

fn diagnostic_is_allowed(
    d: &diagnostics::Diagnostic,
    diagnostic_code: Option<&FxHashSet<DiagnosticCode>>,
) -> bool {
    if let Some(allowed_codes) = diagnostic_code {
        allowed_codes.contains(&d.code)
    } else {
        d.has_category(diagnostics::Category::SimplificationRule)
    }
}

// No changes mean no constraint, so the condition passes. If there
// are changes, the given line must be in at least one of the changed
// ranges.
fn check_changes(
    changed_forms: &FxHashSet<InFile<FormIdx>>,
    form_id: Option<InFile<FormIdx>>,
) -> bool {
    changed_forms.is_empty()
        || form_id
            .map(|form_id| changed_forms.contains(&form_id))
            .unwrap_or(false)
}

fn check<T>(maybe_constraint: &Option<T>, f: impl FnOnce(&T) -> bool) -> bool {
    if let Some(constraint) = maybe_constraint {
        f(constraint)
    } else {
        true
    }
}

struct Lints<'a> {
    analysis_host: &'a mut AnalysisHost,
    cfg: &'a DiagnosticsConfig<'a>,
    vfs: &'a mut Vfs,
    to: &'a Option<PathBuf>,
    include_generated: bool,
    include_ct_diagnostics: bool,
    include_edoc_diagnostics: bool,
    in_place: bool,
    recursive: bool,
    changed_files: &'a mut FxHashSet<(FileId, String)>,
    diags: Vec<(String, FileId, Vec<diagnostics::Diagnostic>)>,
    changed_forms: FxHashSet<InFile<FormIdx>>,
}

#[derive(Debug)]
struct FixResult {
    file_id: FileId,
    name: String,
    source: String,
    changes: Vec<DiffRange>, // Valid in FixResult.source field
    diff: Option<String>,
}

const LINT_APPLICATION_RECURSION_LIMIT: i32 = 10;

impl<'a> Lints<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        analysis_host: &'a mut AnalysisHost,
        cfg: &'a DiagnosticsConfig,
        vfs: &'a mut Vfs,
        to: &'a Option<PathBuf>,
        include_generated: bool,
        include_ct_diagnostics: bool,
        include_edoc_diagnostics: bool,
        in_place: bool,
        recursive: bool,
        changed_files: &'a mut FxHashSet<(FileId, String)>,
        diags: Vec<(String, FileId, Vec<diagnostics::Diagnostic>)>,
    ) -> Lints<'a> {
        let mut changed_forms = FxHashSet::default();
        for (_name, file_id, diags) in &diags {
            for diag in diags {
                if let Some(form_id) =
                    get_form_id_at_offset(&analysis_host.analysis(), *file_id, diag.range.start())
                {
                    changed_forms.insert(InFile::new(*file_id, form_id));
                }
            }
        }
        Lints {
            analysis_host,
            cfg,
            vfs,
            to,
            include_generated,
            include_ct_diagnostics,
            include_edoc_diagnostics,
            in_place,
            recursive,
            changed_files,
            diags,
            changed_forms,
        }
    }

    fn apply_relevant_fixes(&mut self, format_normal: bool, cli: &mut dyn Cli) -> Result<()> {
        let mut recursion_limit = LINT_APPLICATION_RECURSION_LIMIT;
        loop {
            let changes = self.apply_diagnostics_fixes(format_normal, cli)?;
            if recursion_limit <= 0 || changes.is_empty() {
                if recursion_limit < 0 {
                    bail!(
                        "Hit recursion limit ({}) while applying fixes",
                        LINT_APPLICATION_RECURSION_LIMIT
                    );
                }
                break;
            }
            recursion_limit -= 1;
            let new_diags = changes
                .into_iter()
                .map(
                    |FixResult {
                         file_id,
                         name,
                         source,
                         changes,
                         diff: _,
                     }|
                     -> Result<
                        Option<(String, FileId, LabeledDiagnostics<diagnostics::Diagnostic>)>,
                    > {
                        self.changed_files.insert((file_id, name.clone()));
                        let path = self.vfs.file_path(file_id);
                        self.vfs
                            .set_file_contents(path, Some(source.clone().into_bytes()));

                        self.analysis_host.apply_change(Change {
                            roots: None,
                            files_changed: vec![(file_id, Some(Arc::from(source)))],
                            app_structure: None,
                        });

                        let changes = changes
                            .iter()
                            .filter_map(|d| {
                                form_from_diff(&self.analysis_host.analysis(), file_id, d)
                            })
                            .collect::<Vec<_>>();

                        for form_id in &changes {
                            self.changed_forms
                                .insert(InFile::new(file_id, form_id.clone()));
                        }

                        do_parse_one(
                            &self.analysis_host.analysis(),
                            self.cfg,
                            file_id,
                            &name,
                            self.include_generated,
                            self.include_ct_diagnostics,
                            self.include_edoc_diagnostics,
                        )
                    },
                )
                .collect::<Result<Vec<Option<_>>>>()?
                .into_iter()
                .flatten()
                .collect::<Vec<_>>();
            self.diags = filter_diagnostics(
                &self.analysis_host.analysis(),
                &None,
                None,
                None,
                None,
                &new_diags,
                &self.changed_forms,
            )?;
            if !self.diags.is_empty() {
                writeln!(cli, "---------------------------------------------\n")?;
                writeln!(cli, "New filtered diagnostics")?;
                for (name, file_id, diags) in &self.diags {
                    writeln!(cli, "  {}: {}", name, diags.len())?;
                    for diag in diags.iter() {
                        print_diagnostic(diag, &self.analysis_host.analysis(), *file_id, cli)?;
                    }
                }
            }
            if !self.recursive {
                break;
            }
        }
        self.changed_files.iter().for_each(|(file_id, name)| {
            let bytes = self.vfs.file_contents(*file_id);
            let document = Document::from_bytes(bytes.to_vec());
            self.write_fix_result(*file_id, name, &document.content);
        });
        Ok(())
    }

    fn apply_diagnostics_fixes(
        &self,
        format_normal: bool,
        cli: &mut dyn Cli,
    ) -> Result<Vec<FixResult>> {
        // Only apply a single fix, then re-parse. This avoids potentially
        // conflicting changes.
        let changes = self
            .diags
            .iter()
            .flat_map(|(m, file_id, ds)| {
                ds.iter().next().map_or(Ok(vec![]), |d| {
                    self.apply_fixes(m, d, *file_id, format_normal, cli)
                })
            })
            .flatten()
            .collect::<Vec<FixResult>>();
        Ok(changes)
    }

    /// Apply any assists included in the diagnostic
    fn apply_fixes(
        &self,
        name: &String,
        diagnostic: &diagnostics::Diagnostic,
        file_id: FileId,
        format_normal: bool,
        cli: &mut dyn Cli,
    ) -> Result<Vec<FixResult>> {
        if let Some(fixes) = &diagnostic.fixes {
            let fixes: Vec<_> = fixes
                .iter()
                .filter(|f| f.group != Some(group_label_ignore()))
                .collect();
            if !fixes.is_empty() {
                if format_normal {
                    writeln!(cli, "---------------------------------------------\n")?;
                    writeln!(cli, "Applying fix in module '{name}' for")?;
                    print_diagnostic(diagnostic, &self.analysis_host.analysis(), file_id, cli)?;
                }
                let changed = fixes
                    .iter()
                    .filter_map(|fix| self.apply_one_fix(fix, name))
                    .collect::<Vec<FixResult>>();
                if format_normal {
                    changed.iter().for_each(|r| {
                        if let Some(unified) = &r.diff {
                            _ = writeln!(cli, "{unified}");
                        }
                    });
                }
                Ok(changed)
            } else {
                bail!("Only 'ignore' fixes in {:?}", diagnostic);
            }
        } else {
            bail!("No fixes in {:?}", diagnostic);
        }
    }

    /// Apply a single assist
    fn apply_one_fix(&self, fix: &Assist, name: &str) -> Option<FixResult> {
        let source_change = fix.source_change.as_ref()?;
        let file_id = *source_change.source_file_edits.keys().next().unwrap();
        let mut actual = self
            .analysis_host
            .analysis()
            .file_text(file_id)
            .ok()?
            .to_string();
        let original = actual.clone();

        for edit in source_change.source_file_edits.values() {
            // The invariant for a `TextEdit` requires that they
            // disjoint and sorted by `delete`
            edit.apply(&mut actual);
        }
        let (diff, unified) = diff_from_textedit(&original, &actual);

        Some(FixResult {
            file_id,
            name: name.to_owned(),
            source: actual,
            changes: diff,
            diff: unified,
        })
    }

    fn write_fix_result(&self, file_id: FileId, name: &String, actual: &String) -> Option<()> {
        if self.in_place {
            let file_path = self.vfs.file_path(file_id);
            let to_path = file_path.as_path()?;
            let mut output = File::create(to_path).ok()?;
            write!(output, "{actual}").ok()?;
        } else if let Some(to) = self.to {
            let to_path = to.join(format!("{}.erl", name));
            let mut output = File::create(to_path).ok()?;
            write!(output, "{actual}").ok()?;
        } else {
            return None;
        };
        Some(())
    }
}

/// Take the diff location, find the FormIdx of the enclosing form of its start position
fn form_from_diff(analysis: &Analysis, file_id: FileId, diff: &DiffRange) -> Option<FormIdx> {
    let line_index = analysis.line_index(file_id).ok()?;
    let pos = line_index.offset(LineCol {
        line: diff.after_start,
        col_utf16: 0,
    });
    let form_id = get_form_id_at_offset(analysis, file_id, pos)?;
    Some(form_id)
}

fn get_form_id_at_offset(
    analysis: &Analysis,
    file_id: FileId,
    offset: TextSize,
) -> Option<FormIdx> {
    let form_list = analysis.form_list(file_id).ok()?;
    let form = analysis
        .enclosing_form(FilePosition { file_id, offset })
        .ok()??;
    let form_id = form_list.find_form(&form)?;
    Some(form_id)
}

#[cfg(test)]
mod tests {
    use elp_ide::diagnostics::DiagnosticCode;
    use elp_ide::diagnostics::Lint;
    use elp_ide::diagnostics::LintsFromConfig;
    use elp_ide::diagnostics::ReplaceCall;
    use elp_ide::diagnostics::ReplaceCallAction;
    use elp_ide::diagnostics::Replacement;
    use elp_ide::FunctionMatch;
    use expect_test::expect;

    use super::LintConfig;

    #[test]
    fn serde_serialize_lint_config() {
        let result = toml::to_string::<LintConfig>(&LintConfig {
            ad_hoc_lints: LintsFromConfig {
                lints: vec![Lint::ReplaceCall(ReplaceCall {
                    matcher: FunctionMatch::mf("mod_a", "func"),
                    action: ReplaceCallAction::Replace(Replacement::UseOk),
                })],
            },
            enabled_lints: vec![DiagnosticCode::HeadMismatch],
            disabled_lints: vec![],
        })
        .unwrap();

        expect![[r#"
            enabled_lints = ["P1700"]
            disabled_lints = []
            [[ad_hoc_lints.lints]]
            type = "ReplaceCall"

            [ad_hoc_lints.lints.matcher]
            type = "MF"
            module = "mod_a"
            name = "func"

            [ad_hoc_lints.lints.action]
            action = "Replace"
            type = "UseOk"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_deserialize_lint_config() {
        let lint_config: LintConfig = toml::from_str(
            r#"enabled_lints =['W0014', 'trivial_match']
               disabled_lints = []
             "#,
        )
        .unwrap();

        expect![[r#"
            LintConfig {
                enabled_lints: [
                    CrossNodeEval,
                    TrivialMatch,
                ],
                disabled_lints: [],
                ad_hoc_lints: LintsFromConfig {
                    lints: [],
                },
            }
        "#]]
        .assert_debug_eq(&lint_config);
    }
}
