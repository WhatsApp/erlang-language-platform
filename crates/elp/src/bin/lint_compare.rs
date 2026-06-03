/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! `elp lint-compare`: pure-data diff over two `elp lint --format=json`
//! JSONL artifacts.
//!
//! Given two sets of JSONL diagnostics — typically one captured at a
//! prior project state (the **base**) and one from the current run (the
//! **diff**) — compute the per-(`name`, `severity`) count delta, format
//! a Markdown report, and exit `1` if any group's count changed.
//!
//! The command intentionally does **not** load a project, run lint, or
//! depend on BUCK / OTP / a source tree. The expected workflow is:
//!
//! 1. `elp lint --format=json > before.jsonl` (at base state)
//! 2. `elp lint --format=json > after.jsonl` (at diff state)
//! 3. `elp lint-compare --base before.jsonl --diff after.jsonl`
//!
//! By default the Markdown report is written to stdout; pass
//! `--report-path <PATH>` to redirect it to a file instead.
//!
//! Step 3 is the same primitive whether the caller is CI comparing two
//! diff revisions, a developer checking what they changed locally, or a
//! tool comparing two ELP releases. Splitting the operation in two
//! ("run lint" and "compare two JSONLs") means each command does one
//! thing and the producer side is reusable / cacheable / replaceable.
//!
//! The schema is the existing `arc_types::Diagnostic`, so any JSONL
//! that `elp lint --format=json` could have produced is a valid input
//! to `lint-compare` — no separate "baseline format" to maintain.
//!
//! The grouping key is `(name, severity)` — typically e.g.
//! `("W0006 (unsafe_integer_conversion)", warning)`. Counting by group
//! rather than by raw line lets the signal flag "the diagnostic
//! landscape shifted" without firing on noise (e.g. line numbers
//! drifting inside the same module when no diagnostic count actually
//! changed). A lint that flips severity (e.g. `warning` -> `error`)
//! shows up as one removed row plus one added row, surfacing the
//! semantic change rather than hiding it inside a single count.
//!
//! ## Exit codes
//!
//! - `0` ............... per-(`name`, `severity`) counts match the base
//! - `1` ............... at least one group's count changed
//! - `101` ............. any other failure

use std::fs;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;

use anyhow::Context;
use anyhow::Result;
use elp::arc_types;
use elp::cli::Cli;

use crate::args::LintCompare;

// ---------------------------------------------------------------------
// Error type that the CLI returns when a delta is detected.
// `main::handle_res` matches on it to produce a dedicated exit code (1)
// distinct from the generic 101 used for any other failure.
// ---------------------------------------------------------------------

/// Returned from `run_lint_compare_command` when the per-(`name`,
/// `severity`) delta is non-zero. `main::handle_res` detects this via
/// `anyhow::Error::downcast_ref` and translates it into exit code `1`,
/// so callers can distinguish "regression detected" from "elp itself
/// failed" (101) or "no regression" (0).
#[derive(Debug, thiserror::Error)]
#[error(
    "elp lint-compare: detected regression in {changed_rows} diagnostic group(s) (see report for details)"
)]
pub(crate) struct LintCompareRegression {
    /// Number of `(name, severity)` rows whose count changed between
    /// base and diff. Purely informational — the per-row detail
    /// lives in the report file the caller wrote out (if any). This
    /// field just carries enough state for a useful `Display` impl.
    pub(crate) changed_rows: usize,
}

// ---------------------------------------------------------------------
// Command handler
// ---------------------------------------------------------------------

/// Entry point for `elp lint-compare`.
///
/// Loads the two JSONL files, computes the per-(`name`, `severity`)
/// delta, writes the Markdown report (to stdout by default, or to
/// the path provided via `--report-path`), and returns
/// [`LintCompareRegression`] when any group's count changed (which
/// `main::handle_res` translates to exit code `1`).
pub(crate) fn run_lint_compare_command(args: &LintCompare, cli: &mut dyn Cli) -> Result<()> {
    let base = load_jsonl_diagnostics(&args.base)?;
    let diff = load_jsonl_diagnostics(&args.diff)?;
    let result = CompareResult::compute(&base, &diff);
    let report = result.markdown_report();

    match &args.report_path {
        Some(report_path) => {
            fs::write(report_path, &report).with_context(|| {
                format!(
                    "failed to write lint-compare report to {}",
                    report_path.display()
                )
            })?;
        }
        None => {
            write!(cli, "{report}").context("failed to write lint-compare report to stdout")?;
        }
    }

    if result.has_changes() {
        return Err(LintCompareRegression {
            changed_rows: result.rows.len(),
        }
        .into());
    }
    Ok(())
}

// ---------------------------------------------------------------------
// Loading
// ---------------------------------------------------------------------

/// Load a JSONL artifact produced by `elp lint --format=json`. The
/// on-disk format is one JSON object per line (newline-delimited JSON).
///
/// Empty lines are skipped. Each non-empty line must deserialise into
/// an [`arc_types::Diagnostic`]; the first malformed line surfaces a
/// `Result::Err` with the line number and offending text, plus a hint
/// about how to produce a valid input.
pub(crate) fn load_jsonl_diagnostics(path: &Path) -> Result<Vec<arc_types::Diagnostic>> {
    let file = fs::File::open(path)
        .with_context(|| format!("failed to open lint-compare input {}", path.display()))?;
    let reader = BufReader::new(file);
    reader
        .lines()
        .enumerate()
        .filter_map(|(lineno, line)| -> Option<Result<arc_types::Diagnostic>> {
            let line = match line.with_context(|| {
                format!("failed to read line {} of {}", lineno + 1, path.display())
            }) {
                Ok(l) => l,
                Err(e) => return Some(Err(e)),
            };
            if line.trim().is_empty() {
                return None;
            }
            Some(serde_json::from_str::<arc_types::Diagnostic>(&line).with_context(|| {
                format!(
                    "failed to parse line {} of {} as JSON Diagnostic (did you produce this file with `elp lint --format=json`?): {}",
                    lineno + 1,
                    path.display(),
                    line
                )
            }))
        })
        .collect()
}

// ---------------------------------------------------------------------
// Diff
// ---------------------------------------------------------------------

/// A single per-(`name`, `severity`) row in a [`CompareResult`].
///
/// `base_count` and `diff_count` are inclusive — a row with both
/// counts equal is **not** included in [`CompareResult::rows`] (the
/// diff only retains rows where the counts differ).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CompareRow {
    /// `(name, severity)` group key — e.g.
    /// `("W0006 (unsafe_integer_conversion)", "warning")`.
    pub(crate) name: String,
    pub(crate) severity: String,
    pub(crate) base_count: usize,
    pub(crate) diff_count: usize,
}

impl CompareRow {
    pub(crate) fn delta(&self) -> isize {
        self.diff_count as isize - self.base_count as isize
    }
}

/// The result of comparing two JSONL diagnostic artifacts, grouped by
/// `(name, severity)` with rows of equal counts elided.
///
/// `rows` is sorted by group key so the output is stable across runs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CompareResult {
    pub(crate) rows: Vec<CompareRow>,
}

impl CompareResult {
    /// Compute the per-(`name`, `severity`) delta between `base` and
    /// `diff`. Returns a [`CompareResult`] with only the rows whose
    /// counts differ.
    pub(crate) fn compute(base: &[arc_types::Diagnostic], diff: &[arc_types::Diagnostic]) -> Self {
        use std::collections::BTreeMap;

        // Per-(name, severity) count buckets. BTreeMap because we want a
        // deterministic row order in the output (sorted by name+severity).
        let mut base_buckets: BTreeMap<(String, String), usize> = BTreeMap::new();
        let mut diff_buckets: BTreeMap<(String, String), usize> = BTreeMap::new();
        for d in base {
            *base_buckets
                .entry((d.name().to_string(), d.severity().to_string()))
                .or_insert(0) += 1;
        }
        for d in diff {
            *diff_buckets
                .entry((d.name().to_string(), d.severity().to_string()))
                .or_insert(0) += 1;
        }

        // Union of keys, sorted by the BTreeMap iterator over the
        // chained set.
        let keys: std::collections::BTreeSet<&(String, String)> =
            base_buckets.keys().chain(diff_buckets.keys()).collect();

        let rows: Vec<CompareRow> = keys
            .into_iter()
            .filter_map(|k| {
                let base_count = base_buckets.get(k).copied().unwrap_or(0);
                let diff_count = diff_buckets.get(k).copied().unwrap_or(0);
                if base_count == diff_count {
                    None
                } else {
                    Some(CompareRow {
                        name: k.0.clone(),
                        severity: k.1.clone(),
                        base_count,
                        diff_count,
                    })
                }
            })
            .collect();

        CompareResult { rows }
    }

    /// True iff any per-(`name`, `severity`) row has a non-zero delta.
    /// This is the source of truth for the CLI's exit code under
    /// `lint-compare` — line drift inside the same group does **not**
    /// fire the regression signal.
    pub(crate) fn has_changes(&self) -> bool {
        !self.rows.is_empty()
    }

    /// Full Markdown report containing the per-(diagnostic, severity)
    /// summary table. This is the single artifact emitted by `--report`;
    /// consumers post it as-is or extract / truncate as needed.
    pub(crate) fn markdown_report(&self) -> String {
        let mut out = String::new();
        out.push_str("# ELP Lint Compare Report\n\n");
        out.push_str("Comparing two `elp lint --format=json` JSONL artifacts.\n\n");
        out.push_str("## Per-(diagnostic, severity) summary\n\n");
        out.push_str("| diagnostic [severity] | base | diff | \u{0394} |\n");
        out.push_str("|------------------------|------|------|---|\n");
        if self.rows.is_empty() {
            out.push_str("| _no change_ | _ | _ | 0 |\n");
        } else {
            for row in &self.rows {
                let delta = row.delta();
                let sign = if delta > 0 { "+" } else { "" };
                out.push_str(&format!(
                    "| {} [{}] | {} | {} | {}{} |\n",
                    row.name, row.severity, row.base_count, row.diff_count, sign, delta,
                ));
            }
        }
        out
    }
}

// ---------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use std::io::Write;
    use std::path::Path;

    use elp::arc_types::Diagnostic;
    use elp::arc_types::Severity;
    use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;
    use expect_test::Expect;
    use expect_test::expect;

    use super::*;

    fn diag(path: &str, line: u32, name: &str, severity: Severity) -> Diagnostic {
        Diagnostic::new(
            Path::new(path),
            line,
            Some(1),
            severity,
            name.to_string(),
            "msg".to_string(),
            None,
            None,
        )
    }

    fn check_report(result: &CompareResult, expect: Expect) {
        expect.assert_eq(&result.markdown_report());
    }

    // ---- diff primitive: empty / no-change ----

    #[test]
    fn empty_inputs_have_no_changes() {
        let result = CompareResult::compute(&[], &[]);
        assert!(!result.has_changes());
        check_report(
            &result,
            expect![[r#"
                # ELP Lint Compare Report

                Comparing two `elp lint --format=json` JSONL artifacts.

                ## Per-(diagnostic, severity) summary

                | diagnostic [severity] | base | diff | Δ |
                |------------------------|------|------|---|
                | _no change_ | _ | _ | 0 |
            "#]],
        );
    }

    #[test]
    fn identical_inputs_have_no_changes() {
        let base = vec![diag("a.erl", 1, "W0001 (foo)", Severity::Warning)];
        let diff = base.clone();
        let result = CompareResult::compute(&base, &diff);
        assert!(!result.has_changes());
    }

    // ---- diff primitive: one added ----

    #[test]
    fn one_added_diagnostic_fires_and_renders() {
        let base = vec![diag("a.erl", 1, "W0001 (foo)", Severity::Warning)];
        let diff = vec![
            diag("a.erl", 1, "W0001 (foo)", Severity::Warning),
            diag("b.erl", 2, "W0002 (bar)", Severity::Warning),
        ];
        let result = CompareResult::compute(&base, &diff);
        assert!(result.has_changes());
        check_report(
            &result,
            expect![[r#"
                # ELP Lint Compare Report

                Comparing two `elp lint --format=json` JSONL artifacts.

                ## Per-(diagnostic, severity) summary

                | diagnostic [severity] | base | diff | Δ |
                |------------------------|------|------|---|
                | W0002 (bar) [warning] | 0 | 1 | +1 |
            "#]],
        );
    }

    // ---- diff primitive: severity flip surfaces as -1 + +1 ----

    #[test]
    fn severity_flip_shows_as_two_rows() {
        let base: Vec<_> = (1..=5)
            .map(|i| diag(&format!("m{i}.erl"), i, "W0007 (baz)", Severity::Warning))
            .collect();
        let diff: Vec<_> = (1..=5)
            .map(|i| diag(&format!("m{i}.erl"), i, "W0007 (baz)", Severity::Error))
            .collect();
        let result = CompareResult::compute(&base, &diff);
        assert!(result.has_changes());
        check_report(
            &result,
            expect![[r#"
                # ELP Lint Compare Report

                Comparing two `elp lint --format=json` JSONL artifacts.

                ## Per-(diagnostic, severity) summary

                | diagnostic [severity] | base | diff | Δ |
                |------------------------|------|------|---|
                | W0007 (baz) [error] | 0 | 5 | +5 |
                | W0007 (baz) [warning] | 5 | 0 | -5 |
            "#]],
        );
    }

    // ---- diff primitive: same group, different field (line) ----

    #[test]
    fn same_group_different_line_does_not_fire() {
        // Same (name, severity), same count, but different line numbers
        // -> per-group delta is zero -> has_changes() must be false.
        let base = vec![diag("a.erl", 1, "W0001 (foo)", Severity::Warning)];
        let diff = vec![diag("a.erl", 2, "W0001 (foo)", Severity::Warning)];
        let result = CompareResult::compute(&base, &diff);
        assert!(!result.has_changes());
    }

    // ---- diff primitive: removed ----

    #[test]
    fn removed_diagnostic_fires() {
        let base = vec![
            diag("a.erl", 1, "W0001 (foo)", Severity::Warning),
            diag("b.erl", 2, "W0002 (bar)", Severity::Warning),
        ];
        let diff = vec![diag("a.erl", 1, "W0001 (foo)", Severity::Warning)];
        let result = CompareResult::compute(&base, &diff);
        assert!(result.has_changes());
    }

    // ---- loader ----

    #[test]
    fn load_jsonl_diagnostics_round_trips() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let d1 = diag("a.erl", 1, "W0001 (foo)", Severity::Warning);
        let d2 = diag("b.erl", 2, "W0002 (bar)", Severity::Error);
        {
            let mut f = tmp.reopen().unwrap();
            writeln!(f, "{}", serde_json::to_string(&d1).unwrap()).unwrap();
            // Blank line in the middle should be tolerated.
            writeln!(f).unwrap();
            writeln!(f, "{}", serde_json::to_string(&d2).unwrap()).unwrap();
        }
        let loaded = load_jsonl_diagnostics(tmp.path()).unwrap();
        assert_eq_expected!(2, loaded.len());
        assert_eq_expected!(d1, loaded[0]);
        assert_eq_expected!(d2, loaded[1]);
    }

    #[test]
    fn load_jsonl_diagnostics_reports_malformed_line() {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        {
            let mut f = tmp.reopen().unwrap();
            writeln!(f, "not json").unwrap();
        }
        let err = load_jsonl_diagnostics(tmp.path()).unwrap_err();
        let msg = format!("{err:#}");
        assert!(msg.contains("line 1"), "error message was: {msg}");
        assert!(
            msg.contains("--format=json"),
            "error should hint at the producer flag, was: {msg}"
        );
    }

    #[test]
    fn regression_display_includes_count() {
        let err = LintCompareRegression { changed_rows: 3 };
        let expected = "elp lint-compare: detected regression in 3 diagnostic group(s) (see report for details)";
        assert_eq_expected!(expected, format!("{err}"));
    }

    // ---- end-to-end run_lint_compare_command ----

    /// Synthetic JSONL inputs in -> handler returns Ok/Err and writes
    /// (or doesn't write) the report file. This is the full external
    /// contract of the subcommand.
    fn write_jsonl(diags: &[Diagnostic]) -> tempfile::NamedTempFile {
        let tmp = tempfile::NamedTempFile::new().unwrap();
        let mut f = tmp.reopen().unwrap();
        for d in diags {
            writeln!(f, "{}", serde_json::to_string(d).unwrap()).unwrap();
        }
        tmp
    }

    #[test]
    fn run_lint_compare_no_regression_when_identical() {
        let d = diag("a.erl", 1, "W0001 (foo)", Severity::Warning);
        let base = write_jsonl(std::slice::from_ref(&d));
        let diff = write_jsonl(std::slice::from_ref(&d));
        let report = tempfile::NamedTempFile::new().unwrap();

        let args = LintCompare {
            base: base.path().to_path_buf(),
            diff: diff.path().to_path_buf(),
            report_path: Some(report.path().to_path_buf()),
        };
        let mut cli = elp::cli::Fake::default();
        let result = run_lint_compare_command(&args, &mut cli);
        assert!(result.is_ok(), "expected Ok, got {result:?}");

        // Report is still written even when there are no changes,
        // because consumers want a stable file path to read regardless
        // of outcome.
        let body = std::fs::read_to_string(report.path()).unwrap();
        assert!(body.contains("_no change_"), "report was: {body}");

        // With --report-path set, stdout receives nothing.
        assert_eq_expected!("", cli.to_strings().0);
    }

    #[test]
    fn run_lint_compare_regression_when_diff_has_extra() {
        let common = diag("a.erl", 1, "W0001 (foo)", Severity::Warning);
        let extra = diag("b.erl", 2, "W0002 (bar)", Severity::Warning);
        let base = write_jsonl(std::slice::from_ref(&common));
        let diff = write_jsonl(&[common, extra]);
        let report = tempfile::NamedTempFile::new().unwrap();

        let args = LintCompare {
            base: base.path().to_path_buf(),
            diff: diff.path().to_path_buf(),
            report_path: Some(report.path().to_path_buf()),
        };
        let mut cli = elp::cli::Fake::default();
        let err = run_lint_compare_command(&args, &mut cli).expect_err("expected regression error");
        let regression = err
            .downcast_ref::<LintCompareRegression>()
            .expect("error should be LintCompareRegression");
        assert_eq_expected!(1, regression.changed_rows);

        let body = std::fs::read_to_string(report.path()).unwrap();
        assert!(
            body.contains("| W0002 (bar) [warning] | 0 | 1 | +1 |"),
            "report should contain the +1 row, was: {body}"
        );
    }

    #[test]
    fn run_lint_compare_writes_report_to_stdout_when_no_report_path() {
        // Without --report-path, the report goes to stdout. Even when
        // there's no delta, the report is still emitted (so callers
        // always have a human-readable artifact).
        let base = write_jsonl(&[]);
        let diff = write_jsonl(&[]);
        let args = LintCompare {
            base: base.path().to_path_buf(),
            diff: diff.path().to_path_buf(),
            report_path: None,
        };
        let mut cli = elp::cli::Fake::default();
        assert!(run_lint_compare_command(&args, &mut cli).is_ok());
        let stdout = cli.to_strings().0;
        assert!(
            stdout.contains("# ELP Lint Compare Report"),
            "stdout should contain the report, was: {stdout}"
        );
        assert!(
            stdout.contains("_no change_"),
            "stdout should contain the no-change row, was: {stdout}"
        );
    }
}
