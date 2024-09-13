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
use elp::otp_file_to_ignore;
use elp::read_lint_config_file;
use elp_eqwalizer::Mode;
use elp_ide::diagnostics;
use elp_ide::diagnostics::DiagnosticCode;
use elp_ide::diagnostics::DiagnosticsConfig;
use elp_ide::diagnostics::LintConfig;
use elp_ide::diagnostics::RemoveElpReported;
use elp_ide::diagnostics_collection::DiagnosticCollection;
use elp_ide::diff::diff_from_textedit;
use elp_ide::diff::DiffRange;
use elp_ide::elp_ide_assists::Assist;
use elp_ide::elp_ide_assists::GroupLabel;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::Change;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FilePosition;
use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
use elp_ide::elp_ide_db::elp_base_db::ModuleName;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::Vfs;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_ide::elp_ide_db::source_change::SourceChange;
use elp_ide::elp_ide_db::LineCol;
use elp_ide::Analysis;
use elp_ide::AnalysisHost;
use elp_project_model::buck::BuckQueryConfig;
use elp_project_model::AppName;
use elp_project_model::AppType;
use elp_project_model::DiscoverConfig;
use fxhash::FxHashSet;
use hir::FormIdx;
use hir::InFile;
use indicatif::ParallelProgressIterator;
use itertools::Itertools;
use paths::Utf8PathBuf;
use rayon::prelude::ParallelBridge;
use rayon::prelude::ParallelIterator;
use text_edit::TextSize;

use crate::args::Lint;
use crate::reporting;

pub fn lint_all(args: &Lint, cli: &mut dyn Cli, query_config: &BuckQueryConfig) -> Result<()> {
    log::info!("Loading project at: {:?}", args.project);
    let config = DiscoverConfig::new(args.rebar, &args.profile);
    let mut loaded = load::load_project_at(
        cli,
        &args.project,
        config,
        IncludeOtp::Yes,
        Mode::Server,
        query_config,
    )?;

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
    args: &Lint,
    ignore_apps: &[String],
) -> Result<Vec<(String, FileId, DiagnosticCollection)>> {
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
                    do_parse_one(db, config, file_id, module_name.as_str(), args).unwrap()
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
    args: &Lint,
) -> Result<Option<(String, FileId, DiagnosticCollection)>> {
    if !args.include_tests && db.is_test_suite_or_test_helper(file_id)?.unwrap_or(false) {
        return Ok(None);
    }

    let mut diagnostics = DiagnosticCollection::default();
    let native = db.native_diagnostics(config, file_id)?;
    diagnostics.set_native(file_id, native);
    if args.include_erlc_diagnostics || config.request_erlang_service_diagnostics {
        let erlang_service =
            db.erlang_service_diagnostics(file_id, config, RemoveElpReported::Yes)?;
        for (file_id, diags) in erlang_service {
            diagnostics.set_erlang_service(file_id, diags);
        }
    }
    if args.include_ct_diagnostics {
        diagnostics.set_ct(file_id, db.ct_diagnostics(file_id, config)?);
    }
    if args.include_edoc_diagnostics {
        let edoc_diagnostics = db
            .edoc_diagnostics(file_id)?
            .into_iter()
            .filter(|(f, _)| *f == file_id)
            .flat_map(|(_, ds)| ds.into_iter().map(|d| d))
            .collect_vec();
        diagnostics.set_edoc(file_id, edoc_diagnostics);
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
            diagnostic_ignore,
            diagnostic_filter,
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
            let cfg = DiagnosticsConfig::default()
                .configure_diagnostics(&cfg_from_file, diagnostic_filter, diagnostic_ignore)?
                .set_include_generated(args.include_generated)
                .set_experimental(args.experimental_diags)
                .set_include_suppressed(args.include_suppressed);

            let allowed_diagnostics = cfg.enabled.clone();

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
                    Some(module) => match analysis.module_file_id(loaded.project_id, module)? {
                        Some(file_id) => {
                            if args.is_format_normal() {
                                writeln!(cli, "module specified: {}", module)?;
                            }
                            (Some(file_id), analysis.module_name(file_id)?)
                        }
                        None => panic!("Module not found: {module}"),
                    },
                    None => match &args.file {
                        Some(file_name) => {
                            if args.is_format_normal() {
                                writeln!(cli, "file specified: {}", file_name)?;
                            }
                            let path_buf =
                                Utf8PathBuf::from_path_buf(fs::canonicalize(file_name).unwrap())
                                    .expect("UTF8 conversion failed");
                            let path = AbsPath::assert(&path_buf);
                            let path = path.as_os_str().to_str().unwrap();
                            (
                                loaded
                                    .vfs
                                    .file_id(&VfsPath::new_real_path(path.to_string())),
                                path_buf.as_path().file_name().map(|n| ModuleName::new(n)),
                            )
                        }
                        None => (None, None),
                    },
                };

                res = match (file_id, name) {
                    (None, _) => {
                        do_parse_all(cli, &analysis, &loaded.project_id, &cfg, args, ignore_apps)?
                    }
                    (Some(file_id), Some(name)) => {
                        do_parse_one(&analysis, &cfg, file_id, &name, args)?
                            .map_or(vec![], |x| vec![x])
                    }
                    (Some(file_id), _) => {
                        panic!("Could not get name from file_id for {:?}", file_id)
                    }
                };

                filter_diagnostics(
                    &analysis,
                    &args.module,
                    Some(&allowed_diagnostics),
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
                                // We use JSON output for CI, and want to see warnings too.
                                // So do not filter on errors only
                                err_in_diag = true;
                                let vfs_path = loaded.vfs.file_path(*file_id);
                                let analysis = loaded.analysis();
                                let root_path = &analysis
                                    .project_data(*file_id)
                                    .unwrap_or_else(|_err| panic!("could not find project data"))
                                    .unwrap_or_else(|| panic!("could not find project data"))
                                    .root_dir;
                                let relative_path =
                                    reporting::get_relative_path(root_path, &vfs_path);
                                let prefix = args.prefix.as_ref();
                                print_diagnostic_json(
                                    diag,
                                    &analysis,
                                    *file_id,
                                    with_prefix(relative_path, prefix).as_path(),
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
                        &args,
                        &mut changed_files,
                        diags,
                    );
                    // We handle the fix application result here, so
                    // the overall status of whether error-severity
                    // diagnostics is still returned as usual, in the
                    // next statement.
                    match lints.apply_relevant_fixes(args.is_format_normal(), cli) {
                        Ok(_) => {}
                        Err(err) => {
                            writeln!(cli, "Apply fix failed: {:#}", err).ok();
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
    diags: &'a [(String, FileId, DiagnosticCollection)],
    changed_forms: &FxHashSet<InFile<FormIdx>>,
) -> Result<Vec<(String, FileId, Vec<diagnostics::Diagnostic>)>> {
    // The initial set of diagnostics is filtered with a `Some` value
    // for allowed_diagnostics. Thereafter, during the simplification
    // stage, it is called with `None`.  This means
    // `Category::SimplificationRule` is always enabled for that
    // usage.
    Ok(diags
        .to_owned()
        .into_iter()
        .filter_map(|(m, file_id, ds)| {
            if module.is_none() || &Some(m.to_string()) == module {
                let ds2 = ds
                    .diagnostics_for(file_id)
                    .iter()
                    .filter(|d| {
                        let form_id = get_form_id_at_offset(db, file_id, d.range.start())
                            .map(|form_id| InFile::new(file_id, form_id));
                        diagnostic_is_allowed(d, allowed_diagnostics)
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

struct Lints<'a> {
    analysis_host: &'a mut AnalysisHost,
    cfg: &'a DiagnosticsConfig<'a>,
    vfs: &'a mut Vfs,
    args: &'a Lint,
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

const LINT_APPLICATION_RECURSION_LIMIT: i32 = 50;

impl<'a> Lints<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        analysis_host: &'a mut AnalysisHost,
        cfg: &'a DiagnosticsConfig,
        vfs: &'a mut Vfs,
        args: &'a Lint,
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
            args,
            changed_files,
            diags,
            changed_forms,
        }
    }

    fn apply_relevant_fixes(&mut self, format_normal: bool, cli: &mut dyn Cli) -> Result<()> {
        let mut recursion_limit = LINT_APPLICATION_RECURSION_LIMIT;
        loop {
            let changes = self.apply_diagnostics_fixes(format_normal, cli)?;
            if changes.is_empty() {
                // Done
                break;
            }
            if recursion_limit <= 0 {
                bail!(
                    "Hit recursion limit ({}) while applying fixes",
                    LINT_APPLICATION_RECURSION_LIMIT
                );
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
                     -> Result<Option<(String, FileId, DiagnosticCollection)>> {
                        self.changed_files.insert((file_id, name.clone()));
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
                            self.changed_forms.insert(InFile::new(file_id, *form_id));
                        }

                        do_parse_one(
                            &self.analysis_host.analysis(),
                            self.cfg,
                            file_id,
                            &name,
                            self.args,
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
            if !self.args.recursive {
                break;
            }
        }
        self.changed_files.iter().for_each(|(file_id, name)| {
            self.write_fix_result(*file_id, name);
        });
        Ok(())
    }

    fn apply_diagnostics_fixes(
        &self,
        format_normal: bool,
        cli: &mut dyn Cli,
    ) -> Result<Vec<FixResult>> {
        let mut changes: Vec<FixResult> = Vec::default();
        if self.args.one_shot {
            self.diags.iter().for_each(|(m, file_id, ds)| {
                if let Ok(fs) = self.apply_all_fixes(m, ds, *file_id, format_normal, cli) {
                    changes.extend(fs.into_iter());
                }
            });
        } else {
            // Only apply a single fix, then re-parse. This avoids potentially
            // conflicting changes.
            changes = self
                .diags
                .iter()
                .flat_map(|(m, file_id, ds)| {
                    ds.iter().next().map_or(Ok(vec![]), |d| {
                        self.apply_fixes(m, d, *file_id, format_normal, cli)
                    })
                })
                .flatten()
                .collect::<Vec<FixResult>>();
        };
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
        // Get code action ones too
        let fixes = diagnostic.get_diagnostic_fixes(self.analysis_host.raw_database(), file_id);
        if !fixes.is_empty() {
            let fixes: Vec<_> = fixes
                .iter()
                .filter(|f| {
                    if self.args.ignore_fix_only {
                        f.group == Some(GroupLabel::ignore())
                    } else {
                        f.group != Some(GroupLabel::ignore())
                    }
                })
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

    fn apply_all_fixes(
        &self,
        name: &String,
        diagnostics: &[diagnostics::Diagnostic],
        file_id: FileId,
        format_normal: bool,
        cli: &mut dyn Cli,
    ) -> Result<Vec<FixResult>> {
        // Get code action ones too
        let fixes = diagnostics
            .into_iter()
            .filter_map(|d| {
                let fs = d
                    .get_diagnostic_fixes(self.analysis_host.raw_database(), file_id)
                    .iter()
                    .filter(|f| {
                        if self.args.ignore_fix_only {
                            f.group == Some(GroupLabel::ignore())
                        } else {
                            f.group != Some(GroupLabel::ignore())
                        }
                    })
                    .map(|a| a.clone())
                    .collect_vec();
                if fs.is_empty() {
                    None
                } else {
                    Some((d.clone(), fs))
                }
            })
            .collect_vec();
        if !fixes.is_empty() {
            let (diagnostics, assists): (Vec<diagnostics::Diagnostic>, Vec<Vec<Assist>>) =
                fixes.iter().cloned().unzip();
            if format_normal {
                writeln!(cli, "---------------------------------------------\n")?;
                writeln!(cli, "Applying fix(es) in module '{name}' for")?;
                for diagnostic in diagnostics {
                    print_diagnostic(&diagnostic, &self.analysis_host.analysis(), file_id, cli)?;
                }
            }
            let source_change =
                Self::assists_to_source_change(&assists.into_iter().flatten().collect_vec());
            let changed = self
                .apply_one_source_change(&source_change, name)
                .into_iter()
                .collect_vec();
            if format_normal {
                changed.iter().for_each(|r| {
                    if let Some(unified) = &r.diff {
                        _ = writeln!(cli, "{unified}");
                    }
                });
            }
            Ok(changed)
        } else {
            bail!("No fixes in {:?}", diagnostics);
        }
    }

    fn assists_to_source_change(assists: &[Assist]) -> SourceChange {
        assists
            .iter()
            .filter_map(|a| a.source_change.as_ref())
            .map(|c| (*c).clone())
            .reduce(|acc, elem| acc.merge(elem))
            .unwrap_or(SourceChange::default())
    }

    /// Apply a single assist
    fn apply_one_fix(&self, fix: &Assist, name: &str) -> Option<FixResult> {
        let source_change = fix.source_change.as_ref()?;
        self.apply_one_source_change(source_change, name)
    }

    fn apply_one_source_change(
        &self,
        source_change: &SourceChange,
        name: &str,
    ) -> Option<FixResult> {
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

    fn write_fix_result(&self, file_id: FileId, name: &String) -> Option<()> {
        let file_text = self.analysis_host.analysis().file_text(file_id).ok()?;
        if self.args.in_place {
            let file_path = self.vfs.file_path(file_id);
            let to_path = file_path.as_path()?;
            let mut output = File::create(to_path).ok()?;
            write!(output, "{file_text}").ok()?;
        } else if let Some(to) = &self.args.to {
            let to_path = to.join(format!("{}.erl", name));
            let mut output = File::create(to_path).ok()?;
            write!(output, "{file_text}").ok()?;
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

fn with_prefix(path: &Path, prefix: Option<&String>) -> PathBuf {
    match prefix {
        Some(prefix) => Path::new(prefix).join(path),
        None => path.into(),
    }
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
