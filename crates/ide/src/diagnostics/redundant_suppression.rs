/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Detection of redundant `% elp:ignore` / `% elp:fixme` annotations (W0081).
//!
//! ELP supports `% elp:ignore <CODE>` and `% elp:fixme <CODE>` annotations
//! that suppress a single ELP diagnostic on the line immediately following
//! the annotation. This module flags annotations (or individual codes within
//! a multi-code annotation) that do not actually suppress anything in the
//! current configuration.
//!
//! ## Granularity
//!
//! Detection is **per code**: an annotation `% elp:ignore W0007 W0008` where
//! W0007 fires but W0008 doesn't produces a single W0081 diagnostic anchored
//! on `W0008` only — the user gets actionable feedback about the specific
//! stale code rather than being told the whole annotation is redundant.
//! Codeless annotations (`% elp:ignore` with no code) are flagged at the
//! pattern marker since there is no code text to anchor on.
//!
//! ## Filter interaction
//!
//! Consumption is tracked **pre-retain**, so an annotation that suppresses a
//! diagnostic that's later filtered out by `--diagnostic-ignore`,
//! `config.disabled`, the experimental gate, or `should_process_app` is
//! still recorded as "doing useful work" and not flagged. Conversely, codes
//! whose linter isn't currently running (e.g. an experimental linter with
//! `experimental=false`) are skipped to avoid false positives.

use elp_ide_db::DiagnosticCode;
use elp_ide_db::LineIndex;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::metadata::Annotation;
use elp_ide_db::metadata::Metadata;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use fxhash::FxHashSet;

use crate::diagnostics::DIAGNOSTIC_WHOLE_FILE_RANGE;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticsConfig;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::fix;

/// Generic message used for codeless `% elp:ignore` annotations, where there
/// is no specific code to mention in the diagnostic.
const DESCRIPTION_CODELESS: &str =
    "Redundant suppression: this annotation does not suppress any diagnostic.";

/// Linter handle for W0081 (RedundantSuppression).
///
/// Implements only the base [`Linter`] trait so W0081 has the same
/// metadata shape as every other linter (id, description, severity,
/// suppressibility) and can be enablement-checked via the standard
/// `should_run` gate. The actual diagnostic computation is the free
/// function [`run`] (called as a post-pass from the diagnostics driver),
/// not a trait method — W0081 is the only post-pass linter today, so a
/// dedicated trait would be premature abstraction.
pub(crate) struct RedundantSuppressionLinter;

impl Linter for RedundantSuppressionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::RedundantSuppression
    }

    fn description(&self) -> &'static str {
        "redundant ELP suppression annotation"
    }
}

pub(crate) static LINTER: RedundantSuppressionLinter = RedundantSuppressionLinter;

/// Run the redundant-suppression post-pass for one file. Walks
/// `pre_retain_diagnostics` to populate the consumption set, then emits
/// W0081 diagnostics for any annotation/code pair that didn't suppress
/// anything currently reportable.
///
/// Called by the diagnostics driver. Enablement gating is the driver's
/// responsibility (via `should_run(&LINTER, ...)`); by the time this
/// runs we already know W0081 should be emitted.
pub(crate) fn run(
    metadata: &Metadata,
    config: &DiagnosticsConfig,
    file_id: FileId,
    line_index: &LineIndex,
    file_text: &str,
    code_reportable: &dyn Fn(&DiagnosticCode) -> bool,
    pre_retain_diagnostics: &[Diagnostic],
) -> Vec<Diagnostic> {
    let mut consumed: Consumed = FxHashSet::default();
    for d in pre_retain_diagnostics {
        record_consumed_annotations(metadata, d, &mut consumed);
    }
    compute_diagnostics(
        metadata,
        &mut consumed,
        config,
        file_id,
        line_index,
        file_text,
        code_reportable,
    )
}

/// A "consumption" record: annotation index + the specific code that
/// suppressed a diagnostic. Tracking per-(annotation, code) lets us flag
/// individual stale codes inside a multi-code annotation while leaving the
/// active codes alone.
type Consumed = FxHashSet<(usize, DiagnosticCode)>;

/// Record consumption of `Source::Elp` annotations by the given diagnostic.
///
/// All annotations that contain `diag.code` AND whose `suppression_range`
/// covers `diag.range.start()` (or whose diagnostic spans the whole file)
/// have the pair `(annotation_idx, diag.code)` inserted into `consumed`.
///
/// Multiple annotations can be consumed by a single diagnostic — for
/// example two stacked `% elp:ignore W0007` comments would both be recorded
/// as consuming a single W0007 diagnostic that fires inside the overlap.
fn record_consumed_annotations(metadata: &Metadata, diag: &Diagnostic, consumed: &mut Consumed) {
    for (idx, annotation) in metadata.elp_annotations_indexed() {
        if annotation.codes.contains(&diag.code)
            && (annotation.suppression_range.contains(diag.range.start())
                || diag.range == DIAGNOSTIC_WHOLE_FILE_RANGE)
        {
            consumed.insert((idx, diag.code.clone()));
        }
    }
}

/// Locate the file range of a single code token (e.g. `W0007`) within an
/// annotation comment. Falls back to the comment's pattern range if the
/// code text cannot be found verbatim — this can happen for codes whose
/// `as_code()` differs from how the user wrote them, but in practice both
/// sides are the canonical "Wnnnn" string.
fn find_code_range(annotation: &Annotation, code_str: &str) -> TextRange {
    if let Some(offset) = annotation.comment.find(code_str) {
        let start = annotation.token_range.start() + TextSize::from(offset as u32);
        let end = start + TextSize::from(code_str.len() as u32);
        TextRange::new(start, end)
    } else {
        annotation.comment_range
    }
}

/// Returns the form (`as_code()` or `as_label()`) of `code` that appears
/// verbatim in `annotation.comment`. The annotation parser accepts either
/// the numeric code (`W0007`) or the linter label (`trivial_match`); we
/// echo back whichever the user actually wrote so that highlights and
/// per-code quickfixes operate on the exact text in the source. Defaults
/// to `as_code()` if neither is found, which should be unreachable in
/// practice (the parser only inserts a code when one of the two forms
/// matched) but keeps the function total.
fn written_form(annotation: &Annotation, code: &DiagnosticCode) -> String {
    let canonical = code.as_code();
    if annotation.comment.contains(&canonical) {
        return canonical;
    }
    let label = code.as_label();
    if annotation.comment.contains(&label) {
        return label;
    }
    canonical
}

/// Quickfix that deletes the annotation's entire physical line, including
/// any leading whitespace and the trailing newline. Uses the line index to
/// find the line boundaries cleanly.
fn make_remove_line_fix(
    file_id: FileId,
    line_index: &LineIndex,
    file_text: &str,
    annotation: &Annotation,
    target_range: TextRange,
) -> Option<Diagnostic> {
    let line_num = line_index.line_col(annotation.comment_range.start()).line;
    let line_start = line_index.line_at(line_num as usize)?;
    let line_end = line_index
        .line_at((line_num + 1) as usize)
        .unwrap_or_else(|| TextSize::from(file_text.len() as u32));
    let edit = TextEdit::delete(TextRange::new(line_start, line_end));
    let assist = fix(
        "delete_redundant_suppression",
        "Delete redundant suppression",
        SourceChange::from_text_edit(file_id, edit),
        target_range,
    );
    // Returned wrapped in a Diagnostic so callers can attach via `with_fixes`.
    // Caller will read `.fixes` only.
    Some(Diagnostic::new(LINTER.id(), "", target_range).with_fixes(Some(vec![assist])))
}

/// Quickfix that deletes a single redundant code token (and any whitespace
/// preceding it within the comment). Used when the annotation lists other
/// codes that *are* doing useful work, so deleting the whole line would be
/// too aggressive.
fn make_remove_code_fix(
    file_id: FileId,
    annotation: &Annotation,
    code_str: &str,
    highlight_range: TextRange,
) -> Option<Diagnostic> {
    let comment = &annotation.comment;
    let code_pos = comment.find(code_str)?;
    // Sweep back over any leading whitespace so we don't leave a double
    // space behind. `rfind` returns the last NON-whitespace char before
    // `code_pos`; the next position is where the run of whitespace begins.
    let whitespace_start = match comment[..code_pos].rfind(|c: char| !c.is_whitespace()) {
        Some(p) => p + comment[p..].chars().next()?.len_utf8(),
        None => code_pos,
    };
    let token_start = annotation.token_range.start();
    let delete_start = token_start + TextSize::from(whitespace_start as u32);
    let delete_end =
        token_start + TextSize::from(code_pos as u32) + TextSize::from(code_str.len() as u32);
    let edit = TextEdit::delete(TextRange::new(delete_start, delete_end));
    let assist = fix(
        "remove_redundant_code",
        &format!("Remove redundant code `{code_str}`"),
        SourceChange::from_text_edit(file_id, edit),
        highlight_range,
    );
    Some(Diagnostic::new(LINTER.id(), "", highlight_range).with_fixes(Some(vec![assist])))
}

/// Build a W0081 diagnostic for a specific stale code inside an annotation.
fn build_diagnostic_for_code(
    annotation: &Annotation,
    code: &DiagnosticCode,
    all_codes_redundant: bool,
    file_id: FileId,
    line_index: &LineIndex,
    file_text: &str,
) -> Diagnostic {
    let code_str = written_form(annotation, code);
    let labeled_code = code.as_labeled_code();
    let highlight_range = find_code_range(annotation, &code_str);
    let message =
        format!("Redundant suppression: no `{labeled_code}` diagnostic in suppressed range");
    let fix_holder = if all_codes_redundant {
        make_remove_line_fix(file_id, line_index, file_text, annotation, highlight_range)
    } else {
        make_remove_code_fix(file_id, annotation, &code_str, highlight_range)
    };
    let fixes = fix_holder.and_then(|d| d.fixes);
    Diagnostic::new(LINTER.id(), message, highlight_range)
        .with_severity(Severity::Warning)
        .with_fixes(fixes)
}

/// Build a W0081 diagnostic for a codeless `% elp:ignore` (which can never
/// suppress anything in the current ELP suppression model and is therefore
/// always redundant).
fn build_diagnostic_codeless(
    annotation: &Annotation,
    file_id: FileId,
    line_index: &LineIndex,
    file_text: &str,
) -> Diagnostic {
    let target = annotation.comment_range;
    let fix_holder = make_remove_line_fix(file_id, line_index, file_text, annotation, target);
    let fixes = fix_holder.and_then(|d| d.fixes);
    Diagnostic::new(LINTER.id(), DESCRIPTION_CODELESS, target)
        .with_severity(Severity::Warning)
        .with_fixes(fixes)
}

/// For a code-bearing annotation, return the subset of its codes that are
/// considered redundant: each must be currently reportable AND not
/// consumed. Returns `None` if the annotation is out of scope entirely
/// (non-native, etc.).
fn redundant_codes_of(
    annotation: &Annotation,
    annotation_idx: usize,
    consumed: &Consumed,
    code_reportable: &dyn Fn(&DiagnosticCode) -> bool,
) -> Option<Vec<DiagnosticCode>> {
    if !annotation.is_native_only() {
        return None;
    }
    if annotation.codes.is_empty() {
        return None; // codeless: handled separately
    }
    let mut redundant: Vec<DiagnosticCode> = annotation
        .codes
        .iter()
        .filter(|code| {
            code_reportable(code) && !consumed.contains(&(annotation_idx, (*code).clone()))
        })
        .cloned()
        .collect();
    // Keep output deterministic (codes is a HashSet).
    redundant.sort_by_key(|c| c.as_code());
    Some(redundant)
}

/// Returns `true` if every native, reportable code in the annotation is
/// redundant — which means deleting the whole comment line is the correct
/// fix. If even one code is "active" (consumed) or non-reportable, we
/// prefer the per-code "remove just this code" fix.
fn all_codes_redundant(annotation: &Annotation, redundant: &[DiagnosticCode]) -> bool {
    redundant.len() == annotation.codes.len()
}

/// Compute the W0081 diagnostics for a file.
///
/// Iteratively expands the `consumed` set: each round, we compute candidate
/// W0081 diagnostics for currently-stale (annotation, code) pairs. A
/// candidate emitted at code Ci's range inside annotation A's comment can
/// itself be consumed by another annotation B that lists W0081 in its codes
/// and whose suppression range covers A's line. When that happens, B is
/// recorded as consumed for code W0081 and is therefore not itself flagged.
/// Iterating to a fixed point handles `% elp:ignore W0081` placed above an
/// intentionally-redundant annotation.
///
/// After the fixed point is reached, candidates that would themselves be
/// suppressed by an annotation (i.e. `should_be_suppressed`) are filtered
/// out — this is the secondary mechanism by which `% elp:ignore W0081`
/// silences W0081 emitted on a codeless annotation (codeless annotations
/// have no codes to participate in the consumption-based iteration).
fn compute_diagnostics(
    metadata: &Metadata,
    consumed: &mut Consumed,
    config: &DiagnosticsConfig,
    file_id: FileId,
    line_index: &LineIndex,
    file_text: &str,
    code_reportable: &dyn Fn(&DiagnosticCode) -> bool,
) -> Vec<Diagnostic> {
    // Termination: `consumed` is insert-only (entries are never removed), and
    // every candidate emitted in this loop carries `DiagnosticCode::
    // RedundantSuppression`, so each annotation contributes at most one
    // `(idx, W0081)` entry. Each iteration therefore either grows `consumed`
    // by ≥1 or breaks via the `consumed.len() == prev_len` check, giving at
    // most `annotations + 1` iterations. The `debug_assert!` is a tripwire
    // for future refactors that break either invariant (monotonicity, or
    // only-W0081-inserted-here).
    let max_iterations = metadata.elp_annotations_indexed().count() + 1;
    let mut candidates: Vec<Diagnostic> = Vec::new();
    for iteration in 0.. {
        debug_assert!(
            iteration < max_iterations,
            "redundant_suppression fixed-point loop exceeded {max_iterations} iterations; \
             `consumed` is no longer monotone or the bound is wrong",
        );
        candidates = Vec::new();
        for (idx, annotation) in metadata.elp_annotations_indexed() {
            if annotation.codes.is_empty() {
                // Codeless: emit one diagnostic per annotation. Codeless
                // annotations have no codes to enter `consumed`, so they are
                // re-emitted every iteration with the same range — harmless
                // because the loop only checks `consumed.len()` to decide
                // when to stop.
                if annotation.is_native_only() {
                    candidates.push(build_diagnostic_codeless(
                        annotation, file_id, line_index, file_text,
                    ));
                }
                continue;
            }
            let Some(redundant) = redundant_codes_of(annotation, idx, consumed, code_reportable)
            else {
                continue;
            };
            let all_redundant = all_codes_redundant(annotation, &redundant);
            for code in &redundant {
                candidates.push(build_diagnostic_for_code(
                    annotation,
                    code,
                    all_redundant,
                    file_id,
                    line_index,
                    file_text,
                ));
            }
        }

        let prev_len = consumed.len();
        for d in &candidates {
            record_consumed_annotations(metadata, d, consumed);
        }
        if consumed.len() == prev_len {
            break;
        }
    }

    // Final pass: drop candidates that an `% elp:ignore W0081` annotation
    // would suppress. This mirrors the existing retain-time
    // `should_be_suppressed` filtering for normal diagnostics, and lets
    // users keep an intentionally-redundant annotation by placing an
    // `% elp:ignore W0081` above it. Particularly important for codeless
    // annotations, which don't participate in the iterative consumption
    // loop above.
    candidates.retain(|d| !d.should_be_suppressed(metadata, config));
    candidates
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix;

    #[test]
    fn redundant_when_no_diagnostic_below() {
        check_diagnostics(
            r#"
-module(main).

% elp:ignore W0007
%%           ^^^^^ 💡 warning: W0081: Redundant suppression: no `W0007 (trivial_match)` diagnostic in suppressed range
test() ->
  ok.
"#,
        );
    }

    #[test]
    fn not_redundant_when_diagnostic_fires() {
        check_diagnostics(
            r#"
-module(main).

baz() ->
  % elp:ignore W0007
  Foo = 1,
  ok.
"#,
        );
    }

    #[test]
    fn redundant_when_label_used() {
        // The user can write the linter label (`trivial_match`) instead of
        // the numeric code (`W0007`); the annotation parser indexes both
        // forms via `DIAGNOSTIC_CODE_LOOKUPS`. The diagnostic message uses
        // the canonical labeled-code form, but the highlight tracks the
        // form the user actually wrote.
        check_diagnostics(
            r#"
-module(main).

% elp:ignore trivial_match
%%           ^^^^^^^^^^^^^ 💡 warning: W0081: Redundant suppression: no `W0007 (trivial_match)` diagnostic in suppressed range
test() ->
  ok.
"#,
        );
    }

    #[test]
    fn not_redundant_when_label_used_and_diagnostic_fires() {
        // Negative case: the label form is just an alias for the code, so it
        // participates in consumption identically — the W0007 fired on
        // `Foo = 1` is suppressed and W0081 must NOT be emitted.
        check_diagnostics(
            r#"
-module(main).

baz() ->
  % elp:ignore trivial_match
  Foo = 1,
  ok.
"#,
        );
    }

    #[test]
    fn redundant_when_two_annotations_mixed_forms() {
        // Single annotation listing two codes via different forms (numeric
        // `W0007` and label `statement_has_no_effect` for W0006). Neither
        // fires on `Bar = 2,` so both are stale, and each W0081 highlight
        // tracks the exact form the user wrote in the comment.
        check_diagnostics(
            r#"
-module(main).

baz() ->
  % elp:ignore W0007 statement_has_no_effect
%%             ^^^^^ 💡 warning: W0081: Redundant suppression: no `W0007 (trivial_match)` diagnostic in suppressed range
%%                   ^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0081: Redundant suppression: no `W0006 (statement_has_no_effect)` diagnostic in suppressed range
  Bar = 2,
  Bar.
"#,
        );
    }

    #[test]
    fn redundant_when_codeless() {
        // Codeless annotations have no specific code text to anchor on, so
        // the diagnostic falls back to the pattern marker range and uses
        // the generic message.
        check_diagnostics(
            r#"
-module(main).

% elp:ignore
%%<^^^^^^^^^ 💡 warning: W0081: Redundant suppression: this annotation does not suppress any diagnostic.
test() ->
  ok.
"#,
        );
    }

    #[test]
    fn redundant_when_only_other_code_fires() {
        // The annotation lists W0007 only; W0006 fires on `ok,` but the
        // annotation doesn't list it, so W0007 is still redundant.
        check_diagnostics(
            r#"
-module(main).

baz() ->
  % elp:ignore W0007
%%             ^^^^^ 💡 warning: W0081: Redundant suppression: no `W0007 (trivial_match)` diagnostic in suppressed range
  ok,
%%^^ 💡 warning: W0006: this statement has no effect
  Bar = 2,
  Bar.
"#,
        );
    }

    #[test]
    fn fixme_variant_redundant() {
        check_diagnostics(
            r#"
-module(main).

% elp:fixme W0007
%%          ^^^^^ 💡 warning: W0081: Redundant suppression: no `W0007 (trivial_match)` diagnostic in suppressed range
test() ->
  ok.
"#,
        );
    }

    #[test]
    fn non_native_codes_not_flagged() {
        // L1234 is an erlang_service code; tracking is not yet threaded
        // through that backend, so the annotation is conservatively
        // treated as out of scope.
        check_diagnostics(
            r#"
-module(main).

% elp:ignore L1234
test() ->
  ok.
"#,
        );
    }

    #[test]
    fn multi_code_annotation_one_fires() {
        // Listing both W0007 and W0006: W0006 fires on `ok,` and is
        // suppressed; W0007 does not fire. Per-code semantics: W0007 is
        // flagged individually as the stale code.
        check_diagnostics(
            r#"
-module(main).

baz() ->
  % elp:ignore W0007 W0006
%%             ^^^^^ 💡 warning: W0081: Redundant suppression: no `W0007 (trivial_match)` diagnostic in suppressed range
  ok,
  Bar = 2,
  Bar.
"#,
        );
    }

    #[test]
    fn multi_code_annotation_neither_fires() {
        // Per-code: ONE diagnostic per stale code, both anchored on their
        // respective code text.
        check_diagnostics(
            r#"
-module(main).

baz() ->
  % elp:ignore W0007 W0006
%%             ^^^^^ 💡 warning: W0081: Redundant suppression: no `W0007 (trivial_match)` diagnostic in suppressed range
%%                   ^^^^^ 💡 warning: W0081: Redundant suppression: no `W0006 (statement_has_no_effect)` diagnostic in suppressed range
  Bar = 2,
  Bar.
"#,
        );
    }

    #[test]
    fn redundant_suppression_can_itself_be_suppressed() {
        // The outer `% elp:ignore W0081` consumes the W0081 candidate
        // emitted on the inner `% elp:ignore W0007`, so neither annotation
        // is flagged.
        check_diagnostics(
            r#"
-module(main).

% elp:ignore W0081
% elp:ignore W0007
test() ->
  ok.
"#,
        );
    }

    #[test]
    fn not_redundant_when_code_disabled_via_config() {
        // Config disables W0007. The W0007 diagnostic IS computed (the
        // linter still runs) and therefore consumes the (annotation, W0007)
        // pair pre-retain; only after retain is W0007 dropped. So the
        // annotation must NOT be flagged as redundant.
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::UnspecificInclude)
            .disable(DiagnosticCode::BinaryStringToSigil)
            .disable(DiagnosticCode::HirUnresolvedMacro)
            .disable(DiagnosticCode::BoundVarInLhs)
            .disable(DiagnosticCode::HirUnresolvedInclude)
            .disable(DiagnosticCode::TrivialMatch);
        check_diagnostics_with_config(
            config,
            r#"
-module(main).

baz() ->
  Foo = 1,
  % elp:ignore W0007
  Bar = 2,
  ok.
"#,
        );
    }

    #[test]
    fn redundant_when_only_other_disabled_codes_listed() {
        // Annotation lists only W0007, which is disabled. The annotation
        // cannot meaningfully suppress anything reportable in the current
        // configuration, so by the eligibility rule we conservatively
        // skip it (do NOT flag).
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::UnspecificInclude)
            .disable(DiagnosticCode::BinaryStringToSigil)
            .disable(DiagnosticCode::HirUnresolvedMacro)
            .disable(DiagnosticCode::BoundVarInLhs)
            .disable(DiagnosticCode::HirUnresolvedInclude)
            .disable(DiagnosticCode::TrivialMatch);
        check_diagnostics_with_config(
            config,
            r#"
-module(main).

% elp:ignore W0007
test() ->
  ok.
"#,
        );
    }

    #[test]
    fn fix_removes_entire_line_when_all_codes_redundant() {
        // All codes are redundant -> "Delete redundant suppression" fix
        // wipes the whole line, including indentation and trailing newline.
        check_fix(
            r#"
//- /src/main.erl
-module(main).

baz() ->
  % elp:ignore ~W0007
  ok.
"#,
            expect![[r#"
                -module(main).

                baz() ->
                  ok.
            "#]],
        );
    }

    #[test]
    fn fix_removes_only_redundant_code() {
        // W0007 is consumed (Foo = 1 fires it), W0006 is stale -> per-code
        // fix removes only "W0006" plus its leading whitespace, leaving
        // the rest of the comment intact.
        check_fix(
            r#"
//- /src/main.erl
-module(main).

baz() ->
  % elp:ignore W0007 ~W0006
  Foo = 1,
  ok.
"#,
            expect![[r#"
                -module(main).

                baz() ->
                  % elp:ignore W0007
                  Foo = 1,
                  ok.
            "#]],
        );
    }

    #[test]
    fn fix_removes_line_when_codeless() {
        check_fix(
            r#"
//- /src/main.erl
-module(main).

% ~elp:ignore
test() ->
  ok.
"#,
            expect![[r#"
                -module(main).

                test() ->
                  ok.
            "#]],
        );
    }

    #[test]
    fn diagnostic_filter_w0081_alone_still_audits() {
        // Regression test for the meta-diagnostic behaviour. A user running
        //
        //     elp lint --diagnostic-filter W0081
        //
        // (without `--recursive`) must still see W0081 fire. Naively the
        // filter would shut down the W0007 linter via `should_run` and the
        // eligibility predicate, leaving consumption tracking blind and
        // producing no useful output. `DiagnosticCode::requires_all_linters_to_run`
        // opts W0081 out of that filter check, which is what this test
        // exercises. The CLI's separate output filter then drops the
        // non-W0081 diagnostics from the final report; here in the unit
        // test we still see the W0007 firing because we're observing the
        // raw native_diagnostics output before that CLI-level filter.
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::UnspecificInclude)
            .disable(DiagnosticCode::BinaryStringToSigil)
            .disable(DiagnosticCode::HirUnresolvedMacro)
            .disable(DiagnosticCode::BoundVarInLhs)
            .disable(DiagnosticCode::HirUnresolvedInclude);
        let config = DiagnosticsConfig {
            diagnostic_filter: Some(DiagnosticCode::RedundantSuppression),
            ..config
        };
        check_diagnostics_with_config(
            config,
            r#"
-module(main).

% elp:ignore W0007
%%           ^^^^^ 💡 warning: W0081: Redundant suppression: no `W0007 (trivial_match)` diagnostic in suppressed range
test() ->
  ok.
"#,
        );
    }
}
