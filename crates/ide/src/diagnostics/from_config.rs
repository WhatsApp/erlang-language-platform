/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Read a section of the config file and generate diagnostics from it.

use elp_ide_db::DiagnosticCode;
use elp_ide_db::RootDatabase;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_ssr::SsrRule;
use elp_ide_ssr::SsrSearchScope;
use elp_ide_ssr::match_pattern;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use serde::Deserialize;
use serde::Serialize;

use super::Diagnostic;
use super::DiagnosticExtra;
use super::DiagnosticsConfig;
use super::PlaceholderBinding;
use super::Severity;
use super::TypeReplacement;
use super::replace_call;
use super::replace_call::Replacement;
use super::replace_in_spec;
use crate::MFA;
use crate::codemod_helpers::FunctionMatch;

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct LintsFromConfig {
    pub lints: Vec<Lint>,
}

impl LintsFromConfig {
    pub fn get_diagnostics(
        &self,
        acc: &mut Vec<Diagnostic>,
        sema: &Semantic,
        file_id: FileId,
        config: &DiagnosticsConfig,
    ) {
        self.lints
            .iter()
            .for_each(|l| l.get_diagnostics(acc, sema, file_id, config));
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(tag = "type")]
pub enum Lint {
    ReplaceCall(ReplaceCall),
    ReplaceInSpec(ReplaceInSpec),
    LintMatchSsr(MatchSsr),
}

impl Lint {
    pub fn get_diagnostics(
        &self,
        acc: &mut Vec<Diagnostic>,
        sema: &Semantic,
        file_id: FileId,
        config: &DiagnosticsConfig,
    ) {
        match self {
            // `ReplaceCall` and `ReplaceInSpec` do not consult the per-linter
            // config yet; only `LintMatchSsr` can be named, and an unnamed lint
            // has nothing to key a `[linters.<code>]` section on.
            Lint::ReplaceCall(l) => l.get_diagnostics(acc, sema, file_id),
            Lint::ReplaceInSpec(l) => l.get_diagnostics(acc, sema, file_id),
            Lint::LintMatchSsr(l) => l.get_diagnostics(acc, sema, file_id, config),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct ReplaceCall {
    pub matcher: FunctionMatch,
    pub action: ReplaceCallAction,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(tag = "action")]
pub enum ReplaceCallAction {
    Replace(Replacement),
    RemoveFromList,
}

impl ReplaceCall {
    pub fn get_diagnostics(&self, acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
        match &self.action {
            ReplaceCallAction::Replace(replacement) => replace_call::replace_call_site(
                &self.matcher,
                replacement.clone(),
                &replace_call::adhoc_diagnostic,
                acc,
                sema,
                file_id,
            ),
            ReplaceCallAction::RemoveFromList => replace_call::remove_fun_ref_from_list(
                &self.matcher,
                &replace_call::adhoc_diagnostic,
                acc,
                sema,
                file_id,
            ),
        }
    }
}

// ---------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct ReplaceInSpec {
    pub functions: Vec<MFA>,
    pub action: ReplaceInSpecAction,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(tag = "action")]
pub enum ReplaceInSpecAction {
    Replace(TypeReplacement),
}

impl ReplaceInSpec {
    pub fn get_diagnostics(&self, acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
        match &self.action {
            ReplaceInSpecAction::Replace(replace) => match replace {
                TypeReplacement::TypeAliasWithString { from, to } => {
                    replace_in_spec::replace_in_spec(&self.functions, from, to, acc, sema, file_id)
                }
            },
        }
    }
}

// ---------------------------------------------------------------------

/// One SSR pattern belonging to a [`MatchSsr`] lint.
///
/// Unknown keys are rejected: every field here is optional bar `ssr`, so a
/// misspelt `label` would otherwise be dropped in silence, taking the
/// `patternLabel` its consumers key on with it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SsrPattern {
    pub ssr: String,
    /// Optional label, surfaced as `patternLabel` in JSON output. Set via
    /// CLI `LABEL:ssr: PATTERN.` syntax or directly in `.elp_lint.toml`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
    /// Overrides the lint's `description` for matches of this pattern.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
}

impl SsrPattern {
    pub fn new(ssr: impl Into<String>) -> Self {
        Self {
            ssr: ssr.into(),
            label: None,
            message: None,
        }
    }

    pub fn with_label(mut self, label: Option<String>) -> Self {
        self.label = label;
        self
    }
}

/// A lint made of one or more SSR patterns sharing a diagnostic code.
///
/// Two spellings deserialize into this. The singular one takes `ssr_pattern`
/// (plus optional `message` / `pattern_label`) and reports under the generic
/// `ad-hoc:ssr-match` code. The multi-pattern one takes `name` and a `patterns`
/// list, and reports every match under `ad-hoc:<name>` — which is what makes the
/// lint addressable by `--diagnostic-filter`, `% elp:ignore` and
/// `[linters."ad-hoc:<name>"]`.
#[derive(Debug, Clone)]
pub struct MatchSsr {
    /// When set, the diagnostic code is `ad-hoc:<name>` rather than the shared
    /// `ad-hoc:ssr-match`.
    pub name: Option<String>,
    /// Default message for every pattern that does not carry its own.
    pub description: Option<String>,
    /// Where this lint is documented, surfaced as `docPath` in JSON output.
    /// A config-declared lint has no error-index page, so a consumer that wants
    /// to explain or act on a match needs the project to point at its own doc.
    pub doc: Option<String>,
    pub patterns: Vec<SsrPattern>,
    pub strategy: Option<Strategy>,
    pub severity: Option<Severity>,
}

impl Serialize for MatchSsr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;

        // The most either branch can emit: singular is ssr_pattern + message +
        // pattern_label, multi is name + description + doc + patterns, and both
        // add severity + the two strategies. Text formats ignore this hint, but
        // some binary ones rely on it.
        let mut state = serializer.serialize_struct("MatchSsr", 7)?;
        // Emit the singular spelling whenever the lint is expressible in it, so
        // that `elp search --dump-config` output stays as it was.
        if self.is_singular() {
            let pattern = &self.patterns[0];
            state.serialize_field("ssr_pattern", &pattern.ssr)?;
            if let Some(ref message) = pattern.message {
                state.serialize_field("message", message)?;
            }
            if let Some(ref pattern_label) = pattern.label {
                state.serialize_field("pattern_label", pattern_label)?;
            }
        } else {
            if let Some(ref name) = self.name {
                state.serialize_field("name", name)?;
            }
            if let Some(ref description) = self.description {
                state.serialize_field("description", description)?;
            }
            if let Some(ref doc) = self.doc {
                state.serialize_field("doc", doc)?;
            }
            state.serialize_field("patterns", &self.patterns)?;
        }
        if let Some(ref severity) = self.severity {
            state.serialize_field("severity", severity)?;
        }
        if let Some(strategy) = self.strategy {
            // Default strategy is Expand and InvisibleParens
            let is_default = strategy.macros == MacroStrategy::Expand
                && strategy.parens == ParenStrategy::InvisibleParens;

            // Only serialize strategy if it's not the default
            if !is_default {
                // Serialize macro strategy
                let macro_strategy = match strategy.macros {
                    MacroStrategy::DoNotExpand => "no-expand",
                    MacroStrategy::Expand => "expand",
                    MacroStrategy::ExpandButIncludeMacroCall => "visible-expand",
                };
                state.serialize_field("macro_strategy", macro_strategy)?;

                // Serialize paren strategy
                let paren_strategy = match strategy.parens {
                    ParenStrategy::VisibleParens => "visible",
                    ParenStrategy::InvisibleParens => "invisible",
                };
                state.serialize_field("paren_strategy", paren_strategy)?;
            }
        }
        state.end()
    }
}

impl<'de> Deserialize<'de> for MatchSsr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        // Every key below is optional, so without this a misspelt one is
        // dropped in silence: `nmae = "..."` would leave the lint unnamed and
        // reporting under the shared code, losing the addressability that
        // naming it was for. `LintConfig` denies unknown fields for the same
        // reason.
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct MatchSsrHelper {
            #[serde(default)]
            name: Option<String>,
            #[serde(default)]
            description: Option<String>,
            #[serde(default)]
            doc: Option<String>,
            #[serde(default)]
            ssr_pattern: Option<String>,
            #[serde(default)]
            patterns: Option<Vec<SsrPattern>>,
            #[serde(default)]
            message: Option<String>,
            #[serde(default)]
            severity: Option<Severity>,
            #[serde(default)]
            macro_strategy: Option<String>,
            #[serde(default)]
            paren_strategy: Option<String>,
            #[serde(default)]
            pattern_label: Option<String>,
        }

        let helper = MatchSsrHelper::deserialize(deserializer)?;

        let patterns = match (helper.ssr_pattern, helper.patterns) {
            (Some(_), Some(_)) => {
                return Err(D::Error::custom(
                    "specify either 'ssr_pattern' or 'patterns', not both",
                ));
            }
            (None, None) => {
                return Err(D::Error::custom("missing 'ssr_pattern' or 'patterns'"));
            }
            (Some(ssr), None) => vec![SsrPattern {
                ssr,
                label: helper.pattern_label,
                message: helper.message,
            }],
            (None, Some(patterns)) => {
                // `message` and `pattern_label` are per-pattern in this spelling;
                // silently ignoring a top-level one would lose the author's intent.
                if helper.message.is_some() {
                    return Err(D::Error::custom(
                        "'message' cannot be used with 'patterns'; set it per pattern, or use 'description' for the whole lint",
                    ));
                }
                if helper.pattern_label.is_some() {
                    return Err(D::Error::custom(
                        "'pattern_label' cannot be used with 'patterns'; set 'label' per pattern",
                    ));
                }
                if patterns.is_empty() {
                    return Err(D::Error::custom("'patterns' must not be empty"));
                }
                patterns
            }
        };

        if let Some(name) = &helper.name {
            validate_lint_name::<D>(name)?;
        }

        // Validate the SSR patterns by trying to parse them.
        // Use a minimal database for validation
        let db = RootDatabase::default();
        for pattern in &patterns {
            SsrRule::parse_str(&db, &pattern.ssr).map_err(|e| {
                D::Error::custom(format!("invalid SSR pattern '{}': {}", pattern.ssr, e))
            })?;
        }

        // Parse strategy from strings if provided
        let strategy = if helper.macro_strategy.is_some() || helper.paren_strategy.is_some() {
            let macros = match helper.macro_strategy.as_deref() {
                Some("no-expand") => MacroStrategy::DoNotExpand,
                Some("expand") | None => MacroStrategy::Expand,
                Some("visible-expand") => MacroStrategy::ExpandButIncludeMacroCall,
                Some(s) => {
                    return Err(D::Error::custom(format!(
                        "invalid macro strategy '{}'. Valid options are: expand, no-expand, visible-expand",
                        s
                    )));
                }
            };

            let parens = match helper.paren_strategy.as_deref() {
                Some("visible") => ParenStrategy::VisibleParens,
                Some("invisible") | None => ParenStrategy::InvisibleParens,
                Some(s) => {
                    return Err(D::Error::custom(format!(
                        "invalid paren strategy '{}'. Valid options are: visible, invisible",
                        s
                    )));
                }
            };

            Some(Strategy { macros, parens })
        } else {
            None
        };

        Ok(MatchSsr {
            name: helper.name,
            description: helper.description,
            doc: helper.doc,
            patterns,
            strategy,
            severity: helper.severity,
        })
    }
}

/// A lint name becomes the `<name>` in the `ad-hoc:<name>` diagnostic code, so
/// it must be a single whitespace-free token (otherwise `% elp:ignore` could
/// never reference it) and must not shadow an existing code or label, since
/// `DiagnosticCode` resolution is a flat namespace.
fn validate_lint_name<'de, D>(name: &str) -> Result<(), D::Error>
where
    D: serde::Deserializer<'de>,
{
    use serde::de::Error;

    if name.is_empty() {
        return Err(D::Error::custom("lint 'name' must not be empty"));
    }
    if !name
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
    {
        return Err(D::Error::custom(format!(
            "invalid lint name '{name}': only ASCII letters, digits, '_' and '-' are allowed"
        )));
    }
    if DiagnosticCode::maybe_from_string(name).is_some() {
        return Err(D::Error::custom(format!(
            "lint name '{name}' collides with an existing diagnostic code or label"
        )));
    }
    // `maybe_from_string` cannot catch this one: the shared code is recognised
    // through the `ad-hoc:` prefix, so the bare name resolves to nothing. Taking
    // it would make the lint indistinguishable from every unnamed one, which is
    // exactly what naming it was meant to prevent.
    if name == UNNAMED_SSR_CODE {
        return Err(D::Error::custom(format!(
            "lint name '{name}' is reserved: it is the code unnamed SSR lints \
             already report under"
        )));
    }
    Ok(())
}

/// The code an SSR lint reports under when it has no `name`. Shared between
/// `code()` and name validation so the two cannot drift apart.
const UNNAMED_SSR_CODE: &str = "ssr-match";

impl MatchSsr {
    /// MatchSsr from just a pattern; other fields default.
    pub fn from_pattern(ssr_pattern: impl Into<String>) -> Self {
        Self::from_patterns(vec![SsrPattern::new(ssr_pattern)])
    }

    pub fn from_patterns(patterns: Vec<SsrPattern>) -> Self {
        Self {
            name: None,
            description: None,
            doc: None,
            patterns,
            strategy: None,
            severity: None,
        }
    }

    /// Whether this lint round-trips through the singular `ssr_pattern`
    /// spelling.
    fn is_singular(&self) -> bool {
        self.name.is_none()
            && self.description.is_none()
            && self.doc.is_none()
            && self.patterns.len() == 1
    }

    /// The code every match of this lint reports under.
    fn code(&self) -> DiagnosticCode {
        DiagnosticCode::AdHoc(
            self.name
                .clone()
                .unwrap_or_else(|| UNNAMED_SSR_CODE.to_string()),
        )
    }

    /// Mirrors the `[linters.<code>]` handling that `should_run` applies to
    /// compiled linters. The defaults are the current behaviour of ad-hoc
    /// lints, so a config that sets none of these changes nothing.
    fn should_run(&self, config: &DiagnosticsConfig, sema: &Semantic, file_id: FileId) -> bool {
        let code = self.code();
        let lint_config = config.lint_config.as_ref();

        let is_enabled = lint_config
            .and_then(|c| c.get_is_enabled_override(&code))
            .unwrap_or(true);
        if !is_enabled {
            return false;
        }

        // Each dimension is checked directly rather than through
        // `DiagnosticConditions::enabled`, whose generated rule ANDs the
        // per-linter setting with the global `--include-generated`. Ad-hoc lints
        // ran on every file before this, so borrowing that rule would silently
        // stop existing ones reporting on generated code. Only an explicit
        // `include_generated = false` does that here, which is the deliberate
        // difference from compiled linters.
        let is_generated = sema.db.generated_status(file_id).is_generated();
        let include_generated = lint_config
            .and_then(|c| c.get_include_generated_override(&code))
            .unwrap_or(true);
        if is_generated && !include_generated {
            return false;
        }

        let is_test = sema
            .db
            .is_test_suite_or_test_helper(file_id)
            .unwrap_or(false);
        let include_tests = lint_config
            .and_then(|c| c.get_include_tests_override(&code))
            .unwrap_or(true);
        if is_test && !include_tests {
            return false;
        }

        let experimental = lint_config
            .and_then(|c| c.get_experimental_override(&code))
            .unwrap_or(false);
        if experimental && !config.experimental {
            return false;
        }

        true
    }

    pub fn get_diagnostics(
        &self,
        acc: &mut Vec<Diagnostic>,
        sema: &Semantic,
        file_id: FileId,
        config: &DiagnosticsConfig,
    ) {
        if !self.should_run(config, sema, file_id) {
            return;
        }

        let strategy = self.strategy.unwrap_or(Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        });

        let code = self.code();
        let severity = config
            .lint_config
            .as_ref()
            .and_then(|c| c.get_severity_override(&code))
            .or(self.severity)
            .unwrap_or(Severity::WeakWarning);

        for pattern in &self.patterns {
            let scope = SsrSearchScope::WholeFile(file_id);
            let matches = match_pattern(sema, strategy, &pattern.ssr, scope).in_file(file_id);

            for matched in matches.matches {
                let message = pattern
                    .message
                    .clone()
                    .or_else(|| self.description.clone())
                    .unwrap_or_else(|| format!("SSR pattern matched: {}", pattern.ssr));

                let placeholders = collect_placeholder_bindings(sema, &matched);
                let extra = DiagnosticExtra::Ssr {
                    pattern_label: pattern.label.clone(),
                    placeholders,
                };

                let diag = Diagnostic::new(code.clone(), message, matched.range.range)
                    .with_severity(severity)
                    .with_doc_path_override(self.doc.clone())
                    .with_extra(extra);
                acc.push(diag);
            }
        }
    }
}

/// One binding per occurrence (repeated `_@A` binds twice). Strips `_@` and
/// sorts by `(name, start)` for deterministic output.
fn collect_placeholder_bindings(
    sema: &Semantic,
    matched: &elp_ide_ssr::Match,
) -> Vec<PlaceholderBinding> {
    let mut bindings: Vec<PlaceholderBinding> = matched
        .placeholder_bindings(sema)
        .map(|(name, text, range)| {
            let name_str = name.as_str();
            PlaceholderBinding {
                name: name_str.strip_prefix("_@").unwrap_or(name_str).to_owned(),
                text,
                range,
            }
        })
        .collect();
    bindings.sort_by(|a, b| {
        a.name
            .cmp(&b.name)
            .then_with(|| a.range.start().cmp(&b.range.start()))
    });
    bindings
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use elp_ide_db::DiagnosticCode;
    use elp_ide_db::RootDatabase;
    use elp_ide_db::elp_base_db::FileId;
    use elp_ide_db::elp_base_db::fixture::WithFixture;
    use expect_test::expect;
    use hir::Semantic;
    use hir::Strategy;
    use hir::fold::MacroStrategy;
    use hir::fold::ParenStrategy;

    use super::Diagnostic;
    use super::DiagnosticExtra;
    use super::DiagnosticsConfig;
    use super::Lint;
    use super::LintsFromConfig;
    use super::MatchSsr;
    use super::ReplaceCall;
    use super::ReplaceCallAction;
    use super::ReplaceInSpec;
    use super::ReplaceInSpecAction;
    use super::Severity;
    use super::SsrPattern;
    use crate::codemod_helpers::FunctionMatch;
    use crate::codemod_helpers::MFA;
    use crate::diagnostics::LintConfig;
    use crate::diagnostics::LinterConfig;
    use crate::diagnostics::TypeReplacement;
    use crate::diagnostics::replace_call::Replacement;

    /// The two `[[ad_hoc_lints.lints]]` examples in
    /// `website/docs/get-started/configure-project/elp-lint-toml.md`, verbatim.
    /// Keep the two copies in step: these are the configs users write against,
    /// and nothing else would notice the documented form drifting from the
    /// accepted one.
    const DOCUMENTED_SINGLE_PATTERN: &str = r#"
[[ad_hoc_lints.lints]]
type = "LintMatchSsr"
ssr_pattern = "ssr: lists:reverse(lists:reverse(_@List))."
message = "Double reversal is a no-op"
severity = "warning"
"#;

    const DOCUMENTED_NAMED_LINT: &str = r#"
[[ad_hoc_lints.lints]]
type = "LintMatchSsr"
name = "banned_config_key"
description = "This configuration key is no longer supported"
doc = "docs/lints/banned_config_key.md"
severity = "warning"
patterns = [
  { ssr = "ssr: legacy_timeout.", label = "legacy_timeout_atom" },
  { ssr = 'ssr: <<"legacy_timeout">>.', label = "legacy_timeout_binary" },
  { ssr = "ssr: retry_forever.", label = "retry_forever_atom",
    message = "Unbounded retries are not allowed" },
]
"#;

    fn only_ssr_lint(config: &LintConfig) -> &MatchSsr {
        match &config.ad_hoc_lints.lints[..] {
            [Lint::LintMatchSsr(match_ssr)] => match_ssr,
            other => panic!("expected exactly one SSR lint, got {other:?}"),
        }
    }

    fn diagnostics_for(lint: &MatchSsr, sema: &Semantic, file_id: FileId) -> Vec<Diagnostic> {
        diagnostics_for_with(lint, sema, file_id, &DiagnosticsConfig::default())
    }

    fn diagnostics_for_with(
        lint: &MatchSsr,
        sema: &Semantic,
        file_id: FileId,
        config: &DiagnosticsConfig,
    ) -> Vec<Diagnostic> {
        let mut acc = Vec::new();
        lint.get_diagnostics(&mut acc, sema, file_id, config);
        acc
    }

    /// The singular (`ssr_pattern`) spelling, which most tests below exercise.
    fn match_ssr(ssr: &str, message: Option<&str>) -> MatchSsr {
        MatchSsr::from_patterns(vec![SsrPattern {
            ssr: ssr.to_string(),
            label: None,
            message: message.map(str::to_string),
        }])
    }

    fn match_ssr_with(
        ssr: &str,
        message: Option<&str>,
        strategy: Option<Strategy>,
        severity: Option<crate::diagnostics::Severity>,
    ) -> MatchSsr {
        let mut lint = match_ssr(ssr, message);
        lint.strategy = strategy;
        lint.severity = severity;
        lint
    }

    #[test]
    fn serde_serialize_function_match_mfa() {
        expect![[r#"
            type = "MFA"
            mfa = "mod:fun/3"
        "#]]
        .assert_eq(
            &toml::to_string::<FunctionMatch>(&FunctionMatch::MFA {
                mfa: MFA::new("mod", "fun", 3),
            })
            .unwrap(),
        );
    }

    #[test]
    fn serde_deserialize_function_match_mfa() {
        let function_match: FunctionMatch = toml::from_str(
            r#"
              type = "MFA"
              mfa = "mod:fun/3"
             "#,
        )
        .unwrap();

        expect![[r#"
            MFA {
                mfa: MFA {
                    module: "mod",
                    name: "fun",
                    arity: 3,
                },
            }
        "#]]
        .assert_debug_eq(&function_match);
    }

    #[test]
    fn serde_deserialize_function_match_mfa_arity_bogus() {
        expect![[r#"
            Err(
                Error {
                    message: "invalid MFA 'mod:fun/x'",
                    input: None,
                    keys: [],
                    span: None,
                },
            )
        "#]]
        .assert_debug_eq(&toml::from_str::<FunctionMatch>(
            r#"
              type = "MFA"
              mfa = "mod:fun/x"
             "#,
        ));
    }

    #[test]
    fn serde_deserialize_function_match_mfa_bad_format() {
        expect![[r#"
            Err(
                Error {
                    message: "invalid MFA 'junk'",
                    input: None,
                    keys: [],
                    span: None,
                },
            )
        "#]]
        .assert_debug_eq(&toml::from_str::<FunctionMatch>(
            r#"
              type = "MFA"
              mfa = "junk"
             "#,
        ));
    }

    #[test]
    fn serde_serialize_function_match_mf() {
        expect![[r#"
            type = "MF"
            module = "mod"
            name = "fun"
        "#]]
        .assert_eq(&toml::to_string::<FunctionMatch>(&FunctionMatch::mf("mod", "fun")).unwrap());
    }

    #[test]
    fn serde_serialize_replace_call() {
        expect![[r#"
            [matcher]
            type = "MF"
            module = "mod_a"
            name = "func"

            [action]
            action = "Replace"
            type = "UseOk"
        "#]]
        .assert_eq(
            &toml::to_string::<ReplaceCall>(&ReplaceCall {
                matcher: FunctionMatch::mf("mod_a", "func"),
                action: ReplaceCallAction::Replace(Replacement::UseOk),
            })
            .unwrap(),
        );
    }

    #[test]
    fn serde_deserialize_replace_call() {
        let replace_call: ReplaceCall = toml::from_str(
            r#"
                [matcher]
                type = "MF"
                module = "mod_a"
                name = "func"

                [action]
                action = "Replace"
                type = "UseOk"
             "#,
        )
        .unwrap();

        expect![[r#"
            ReplaceCall {
                matcher: MF {
                    module: "mod_a",
                    name: "func",
                },
                action: Replace(
                    UseOk,
                ),
            }
        "#]]
        .assert_debug_eq(&replace_call);
    }

    #[test]
    fn serde_serialize_replace_call_use_call_arg() {
        expect![[r#"
            [matcher]
            type = "M"
            module = "mod_a"

            [action]
            action = "Replace"
            type = "UseCallArg"
            n = 5
        "#]]
        .assert_eq(
            &toml::to_string::<ReplaceCall>(&ReplaceCall {
                matcher: FunctionMatch::m("mod_a"),
                action: ReplaceCallAction::Replace(Replacement::UseCallArg { n: 5 }),
            })
            .unwrap(),
        );
    }

    #[test]
    fn serde_serialize_replace_call_invocation() {
        expect![[r#"
            [matcher]
            type = "M"
            module = "mod_a"

            [action]
            action = "Replace"
            type = "Invocation"
            replacement = "modu:fn"
        "#]]
        .assert_eq(
            &toml::to_string::<ReplaceCall>(&ReplaceCall {
                matcher: FunctionMatch::m("mod_a"),
                action: ReplaceCallAction::Replace(Replacement::Invocation {
                    replacement: "modu:fn".to_owned(),
                }),
            })
            .unwrap(),
        );
    }

    #[test]
    fn serde_serialize_replace_call_permutation() {
        expect![[r#"
            [matcher]
            type = "M"
            module = "mod_a"

            [action]
            action = "Replace"
            type = "ArgsPermutation"
            perm = [1, 2, 3]
        "#]]
        .assert_eq(
            &toml::to_string::<ReplaceCall>(&ReplaceCall {
                matcher: FunctionMatch::m("mod_a"),
                action: ReplaceCallAction::Replace(Replacement::ArgsPermutation {
                    perm: vec![1, 2, 3],
                }),
            })
            .unwrap(),
        );
    }

    #[test]
    fn serde_serialize_replace_in_spec() {
        expect![[r#"
            functions = ["modu:fn/3"]

            [action]
            action = "Replace"
            type = "TypeAliasWithString"
            from = "modu:one/0"
            to = "modu:other()"
        "#]]
        .assert_eq(
            &toml::to_string::<ReplaceInSpec>(&ReplaceInSpec {
                functions: vec!["modu:fn/3".try_into().unwrap()],
                action: ReplaceInSpecAction::Replace(TypeReplacement::TypeAliasWithString {
                    from: "modu:one/0".try_into().unwrap(),
                    to: "modu:other()".to_string(),
                }),
            })
            .unwrap(),
        );
    }
    #[test]
    fn serde_deserialize_replace_in_spec() {
        let replace_in_spec: ReplaceInSpec = toml::from_str(
            r#"
              functions = ["modu:fn/3"]

              [action]
              action = "Replace"
              type = "TypeAliasWithString"
              from = "modu:one/0"
              to = "modu:other()"
             "#,
        )
        .unwrap();

        expect![[r#"
            ReplaceInSpec {
                functions: [
                    MFA {
                        module: "modu",
                        name: "fn",
                        arity: 3,
                    },
                ],
                action: Replace(
                    TypeAliasWithString {
                        from: MFA {
                            module: "modu",
                            name: "one",
                            arity: 0,
                        },
                        to: "modu:other()",
                    },
                ),
            }
        "#]]
        .assert_debug_eq(&replace_in_spec);
    }

    #[test]
    fn serde_serialize_lints_from_config() {
        let result = toml::to_string::<LintsFromConfig>(&LintsFromConfig {
            lints: vec![Lint::ReplaceCall(ReplaceCall {
                matcher: FunctionMatch::mf("mod_a", "func"),
                action: ReplaceCallAction::Replace(Replacement::UseOk),
            })],
        })
        .unwrap();
        expect![[r#"
            [[lints]]
            type = "ReplaceCall"

            [lints.matcher]
            type = "MF"
            module = "mod_a"
            name = "func"

            [lints.action]
            action = "Replace"
            type = "UseOk"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_deserialize_lints_from_config() {
        let lints: LintsFromConfig = toml::from_str(
            r#"
            [[lints]]
            type = "ReplaceCall"

            [lints.matcher]
            type = "MF"
            module = "mod_a"
            name = "func"

            [lints.action]
            action = "Replace"
            type = "UseOk"
             "#,
        )
        .unwrap();

        expect![[r#"
            LintsFromConfig {
                lints: [
                    ReplaceCall(
                        ReplaceCall {
                            matcher: MF {
                                module: "mod_a",
                                name: "func",
                            },
                            action: Replace(
                                UseOk,
                            ),
                        },
                    ),
                ],
            }
        "#]]
        .assert_debug_eq(&lints);
    }

    #[test]
    fn serde_serialize_lints_from_config_2() {
        let result = toml::to_string::<LintsFromConfig>(&LintsFromConfig {
            lints: vec![Lint::ReplaceInSpec(ReplaceInSpec {
                functions: vec!["modu:fn/3".try_into().unwrap()],
                action: ReplaceInSpecAction::Replace(TypeReplacement::TypeAliasWithString {
                    from: "modu:one/0".try_into().unwrap(),
                    to: "modu:other()".to_string(),
                }),
            })],
        })
        .unwrap();
        expect![[r#"
            [[lints]]
            type = "ReplaceInSpec"
            functions = ["modu:fn/3"]

            [lints.action]
            action = "Replace"
            type = "TypeAliasWithString"
            from = "modu:one/0"
            to = "modu:other()"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_serialize_match_ssr() {
        let result =
            toml::to_string::<MatchSsr>(&match_ssr("ssr: _@A = 10.", Some("Found pattern")))
                .unwrap();
        expect![[r#"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_deserialize_match_ssr() {
        let match_ssr: MatchSsr = toml::from_str(
            r#"
              ssr_pattern = "ssr: _@A = 10."
              message = "Found pattern"
             "#,
        )
        .unwrap();

        expect![[r#"
            MatchSsr {
                name: None,
                description: None,
                doc: None,
                patterns: [
                    SsrPattern {
                        ssr: "ssr: _@A = 10.",
                        label: None,
                        message: Some(
                            "Found pattern",
                        ),
                    },
                ],
                strategy: None,
                severity: None,
            }
        "#]]
        .assert_debug_eq(&match_ssr);
    }

    /// `pattern_label` round-trips through TOML so `--dump-config` preserves
    /// it and `.elp_lint.toml` can set it directly.
    #[test]
    fn serde_match_ssr_pattern_label_round_trips() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let with_label = MatchSsr::from_patterns(vec![SsrPattern {
            ssr: "ssr: _@A = 10.".to_string(),
            label: Some("my_label".to_string()),
            message: Some("Found pattern".to_string()),
        }]);
        let serialized = toml::to_string::<MatchSsr>(&with_label).unwrap();
        expect![[r#"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            pattern_label = "my_label"
        "#]]
        .assert_eq(&serialized);

        let round_tripped: MatchSsr = toml::from_str(&serialized).unwrap();
        assert_eq_expected!(Some("my_label"), round_tripped.patterns[0].label.as_deref());
    }

    #[test]
    fn serde_serialize_lint_match_ssr() {
        let result = toml::to_string::<LintsFromConfig>(&LintsFromConfig {
            lints: vec![Lint::LintMatchSsr(match_ssr(
                "ssr: _@A = 10.",
                Some("Found pattern"),
            ))],
        })
        .unwrap();
        expect![[r#"
            [[lints]]
            type = "LintMatchSsr"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_serialize_lint_match_ssr_with_strategy() {
        let result = toml::to_string::<LintsFromConfig>(&LintsFromConfig {
            lints: vec![Lint::LintMatchSsr(match_ssr_with(
                "ssr: _@A = 10.",
                Some("Found pattern"),
                Some(Strategy {
                    macros: MacroStrategy::Expand,
                    parens: ParenStrategy::InvisibleParens,
                }),
                None,
            ))],
        })
        .unwrap();
        expect![[r#"
            [[lints]]
            type = "LintMatchSsr"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_serialize_lint_match_ssr_with_non_default_strategy() {
        let result = toml::to_string::<LintsFromConfig>(&LintsFromConfig {
            lints: vec![Lint::LintMatchSsr(match_ssr_with(
                "ssr: _@A = 10.",
                Some("Found pattern"),
                Some(Strategy {
                    macros: MacroStrategy::DoNotExpand,
                    parens: ParenStrategy::VisibleParens,
                }),
                None,
            ))],
        })
        .unwrap();
        expect![[r#"
            [[lints]]
            type = "LintMatchSsr"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            macro_strategy = "no-expand"
            paren_strategy = "visible"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_deserialize_lint_match_ssr_with_strategy() {
        let match_ssr: LintsFromConfig = toml::from_str(
            r#"
              [[lints]]
              type = "LintMatchSsr"
              ssr_pattern = "ssr: _@A = 10."
              message = "Found pattern"
              macro_strategy = "visible-expand"
              paren_strategy = "visible"
             "#,
        )
        .unwrap();

        expect![[r#"
            LintsFromConfig {
                lints: [
                    LintMatchSsr(
                        MatchSsr {
                            name: None,
                            description: None,
                            doc: None,
                            patterns: [
                                SsrPattern {
                                    ssr: "ssr: _@A = 10.",
                                    label: None,
                                    message: Some(
                                        "Found pattern",
                                    ),
                                },
                            ],
                            strategy: Some(
                                Strategy {
                                    macros: ExpandButIncludeMacroCall,
                                    parens: VisibleParens,
                                },
                            ),
                            severity: None,
                        },
                    ),
                ],
            }
        "#]]
        .assert_debug_eq(&match_ssr);
    }

    #[test]
    fn serde_serialize_match_ssr_with_severity_error() {
        use crate::diagnostics::Severity;
        let result = toml::to_string::<MatchSsr>(&match_ssr_with(
            "ssr: _@A = 10.",
            Some("Found pattern"),
            None,
            Some(Severity::Error),
        ))
        .unwrap();
        expect![[r#"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            severity = "error"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_serialize_match_ssr_with_severity_warning() {
        use crate::diagnostics::Severity;
        let result = toml::to_string::<MatchSsr>(&match_ssr_with(
            "ssr: _@A = 10.",
            Some("Found pattern"),
            None,
            Some(Severity::Warning),
        ))
        .unwrap();
        expect![[r#"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            severity = "warning"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_serialize_match_ssr_with_severity_weak() {
        use crate::diagnostics::Severity;
        let result = toml::to_string::<MatchSsr>(&match_ssr_with(
            "ssr: _@A = 10.",
            Some("Found pattern"),
            None,
            Some(Severity::WeakWarning),
        ))
        .unwrap();
        expect![[r#"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            severity = "weak"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_serialize_match_ssr_with_severity_info() {
        use crate::diagnostics::Severity;
        let result = toml::to_string::<MatchSsr>(&match_ssr_with(
            "ssr: _@A = 10.",
            Some("Found pattern"),
            None,
            Some(Severity::Information),
        ))
        .unwrap();
        expect![[r#"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            severity = "info"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_deserialize_match_ssr_with_severity() {
        use crate::diagnostics::Severity;
        let match_ssr: MatchSsr = toml::from_str(
            r#"
              ssr_pattern = "ssr: _@A = 10."
              message = "Found pattern"
              severity = "error"
             "#,
        )
        .unwrap();

        assert_eq!(match_ssr.severity, Some(Severity::Error));
    }

    #[test]
    fn serde_deserialize_lint_match_ssr_with_severity() {
        use crate::diagnostics::Severity;
        let lints: LintsFromConfig = toml::from_str(
            r#"
              [[lints]]
              type = "LintMatchSsr"
              ssr_pattern = "ssr: _@A = 10."
              message = "Found pattern"
              severity = "warning"
             "#,
        )
        .unwrap();

        match &lints.lints[0] {
            Lint::LintMatchSsr(match_ssr) => {
                assert_eq!(match_ssr.severity, Some(Severity::Warning));
            }
            _ => panic!("Expected LintMatchSsr"),
        }
    }

    #[test]
    fn serde_serialize_lint_match_ssr_with_severity_and_strategy() {
        use crate::diagnostics::Severity;
        let result = toml::to_string::<LintsFromConfig>(&LintsFromConfig {
            lints: vec![Lint::LintMatchSsr(match_ssr_with(
                "ssr: _@A = 10.",
                Some("Found pattern"),
                Some(Strategy {
                    macros: MacroStrategy::DoNotExpand,
                    parens: ParenStrategy::VisibleParens,
                }),
                Some(Severity::Error),
            ))],
        })
        .unwrap();
        expect![[r#"
            [[lints]]
            type = "LintMatchSsr"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            severity = "error"
            macro_strategy = "no-expand"
            paren_strategy = "visible"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_deserialize_lint_match_ssr_with_severity_and_strategy() {
        let lints: LintsFromConfig = toml::from_str(
            r#"
              [[lints]]
              type = "LintMatchSsr"
              ssr_pattern = "ssr: _@A = 10."
              message = "Found pattern"
              severity = "error"
              macro_strategy = "no-expand"
              paren_strategy = "visible"
             "#,
        )
        .unwrap();

        expect![[r#"
            LintsFromConfig {
                lints: [
                    LintMatchSsr(
                        MatchSsr {
                            name: None,
                            description: None,
                            doc: None,
                            patterns: [
                                SsrPattern {
                                    ssr: "ssr: _@A = 10.",
                                    label: None,
                                    message: Some(
                                        "Found pattern",
                                    ),
                                },
                            ],
                            strategy: Some(
                                Strategy {
                                    macros: DoNotExpand,
                                    parens: VisibleParens,
                                },
                            ),
                            severity: Some(
                                Error,
                            ),
                        },
                    ),
                ],
            }
        "#]]
        .assert_debug_eq(&lints);
    }

    /// Regression: `{_@A, _@A}` previously panicked through the singular
    /// `placeholder_text` accessor. Verify one binding per occurrence.
    /// The point of the `ad-hoc:<name>` code spelling: a suppression comment can
    /// name it. Exercised through the full diagnostics path, because it is the
    /// post-pass in `native_diagnostics` that applies suppression — the unit
    /// helpers here call `get_diagnostics` directly and never reach it.
    #[test]
    fn an_ignore_comment_suppresses_an_ad_hoc_lint() {
        let lints = LintsFromConfig {
            lints: vec![Lint::LintMatchSsr(MatchSsr::from_pattern("ssr: ok."))],
        };
        crate::tests::check_diagnostics_with_config(
            crate::diagnostics::DiagnosticsConfig::default().set_lints_from_config(&lints),
            r#"
            //- /src/main.erl
            -module(main).
            f() -> ok.
            %%     ^^ weak: ad-hoc:ssr-match: SSR pattern matched: ssr: ok.
            %% elp:ignore ad-hoc:ssr-match
            g() -> ok.
            "#,
        );
    }

    #[test]
    fn ssr_diagnostic_handles_repeated_placeholder() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let (db, file_id) = RootDatabase::with_single_file("t() -> X = 1, {X, X}.");
        let sema = Semantic::new(&db);
        let lint = MatchSsr::from_pattern("ssr: {_@A, _@A}.");
        let acc = diagnostics_for(&lint, &sema, file_id);
        assert_eq_expected!(1, acc.len());
        let Some(DiagnosticExtra::Ssr { placeholders, .. }) = &acc[0].extra else {
            panic!("expected DiagnosticExtra::Ssr, got {:?}", acc[0].extra);
        };
        // Two bindings for `_@A` (one per occurrence), both naming `A`.
        assert_eq_expected!(2, placeholders.len());
        assert!(placeholders.iter().all(|b| b.name == "A" && b.text == "X"));
    }

    /// The multi-pattern spelling round-trips, and does not collapse back into
    /// the singular one.
    #[test]
    fn serde_match_ssr_named_multi_pattern_round_trips() {
        let lints: LintsFromConfig = toml::from_str(
            r#"
              [[lints]]
              type = "LintMatchSsr"
              name = "my_lint"
              description = "Found something"
              severity = "warning"
              patterns = [
                { ssr = "ssr: legacy_timeout.", label = "atom" },
                { ssr = "ssr: <<\"legacy_timeout\">>.", label = "binary", message = "as a binary" },
              ]
             "#,
        )
        .unwrap();

        expect![[r#"
            [[lints]]
            type = "LintMatchSsr"
            name = "my_lint"
            description = "Found something"
            severity = "warning"

            [[lints.patterns]]
            ssr = "ssr: legacy_timeout."
            label = "atom"

            [[lints.patterns]]
            ssr = 'ssr: <<"legacy_timeout">>.'
            label = "binary"
            message = "as a binary"
        "#]]
        .assert_eq(&toml::to_string::<LintsFromConfig>(&lints).unwrap());
    }

    /// Every pattern of a named lint reports under `ad-hoc:<name>`, and each
    /// carries its own label.
    #[test]
    fn ssr_named_lint_reports_all_patterns_under_one_code() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let (db, file_id) = RootDatabase::with_single_file("t() -> {foo, bar}.");
        let sema = Semantic::new(&db);
        let mut lint = MatchSsr::from_patterns(vec![
            SsrPattern::new("ssr: foo.").with_label(Some("first".to_string())),
            SsrPattern::new("ssr: bar.").with_label(Some("second".to_string())),
        ]);
        lint.name = Some("my_lint".to_string());
        lint.description = Some("Found something".to_string());

        let acc = diagnostics_for(&lint, &sema, file_id);

        assert_eq_expected!(2, acc.len());
        for diag in &acc {
            assert_eq_expected!("ad-hoc:my_lint", diag.code.as_code());
            assert_eq_expected!("Found something", diag.message);
        }
        let labels: Vec<_> = acc
            .iter()
            .filter_map(|d| match &d.extra {
                Some(DiagnosticExtra::Ssr { pattern_label, .. }) => pattern_label.clone(),
                _ => None,
            })
            .collect();
        let expected = vec!["first".to_string(), "second".to_string()];
        assert_eq_expected!(expected, labels);
    }

    /// A per-pattern `message` wins over the lint-wide `description`.
    #[test]
    fn ssr_pattern_message_overrides_description() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let (db, file_id) = RootDatabase::with_single_file("t() -> ok.");
        let sema = Semantic::new(&db);
        let mut lint = MatchSsr::from_patterns(vec![SsrPattern {
            ssr: "ssr: ok.".to_string(),
            label: None,
            message: Some("specific".to_string()),
        }]);
        lint.description = Some("generic".to_string());

        let acc = diagnostics_for(&lint, &sema, file_id);
        assert_eq_expected!(1, acc.len());
        assert_eq_expected!("specific", acc[0].message);
    }

    /// An unnamed lint keeps reporting under the shared `ad-hoc:ssr-match`
    /// code, so existing configs are unaffected.
    #[test]
    fn ssr_unnamed_lint_keeps_shared_code() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let (db, file_id) = RootDatabase::with_single_file("t() -> ok.");
        let sema = Semantic::new(&db);
        let acc = diagnostics_for(&MatchSsr::from_pattern("ssr: ok."), &sema, file_id);
        assert_eq_expected!(1, acc.len());
        assert_eq_expected!("ad-hoc:ssr-match", acc[0].code.as_code());
    }

    /// The documented single-pattern config parses, and means what the prose
    /// beside it says: no name, so the shared code.
    #[test]
    fn documented_single_pattern_config_parses() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let config: LintConfig = toml::from_str(DOCUMENTED_SINGLE_PATTERN).unwrap();
        let lint = only_ssr_lint(&config);

        assert_eq_expected!(None, lint.name.as_deref());
        assert_eq_expected!("ad-hoc:ssr-match", lint.code().as_code());
        assert_eq_expected!(1, lint.patterns.len());
        assert_eq_expected!(
            Some("Double reversal is a no-op"),
            lint.patterns[0].message.as_deref()
        );
        assert_eq_expected!(Some(Severity::Warning), lint.severity);
    }

    /// The documented named config parses, means what the prose says, and
    /// survives a write/read round-trip — `elp search --dump-config` emits these
    /// files, so the written form has to be one the reader accepts.
    #[test]
    fn documented_named_config_parses_and_round_trips() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let config: LintConfig = toml::from_str(DOCUMENTED_NAMED_LINT).unwrap();
        let lint = only_ssr_lint(&config);

        assert_eq_expected!(Some("banned_config_key"), lint.name.as_deref());
        assert_eq_expected!("ad-hoc:banned_config_key", lint.code().as_code());
        assert_eq_expected!(
            Some("This configuration key is no longer supported"),
            lint.description.as_deref()
        );
        assert_eq_expected!(Some("docs/lints/banned_config_key.md"), lint.doc.as_deref());
        assert_eq_expected!(Some(Severity::Warning), lint.severity);

        let labels: Vec<_> = lint
            .patterns
            .iter()
            .map(|pattern| pattern.label.as_deref())
            .collect();
        let expected_labels = vec![
            Some("legacy_timeout_atom"),
            Some("legacy_timeout_binary"),
            Some("retry_forever_atom"),
        ];
        assert_eq_expected!(expected_labels, labels);
        // Only the third carries its own message; the rest fall back to `description`.
        assert_eq_expected!(
            Some("Unbounded retries are not allowed"),
            lint.patterns[2].message.as_deref()
        );

        // Writing it back produces something the reader accepts, unchanged.
        let written = toml::to_string(&lint).unwrap();
        let reread: MatchSsr = toml::from_str(&written).unwrap();
        assert_eq_expected!(written, toml::to_string(&reread).unwrap());
        expect![[r#"
            name = "banned_config_key"
            description = "This configuration key is no longer supported"
            doc = "docs/lints/banned_config_key.md"
            severity = "warning"

            [[patterns]]
            ssr = "ssr: legacy_timeout."
            label = "legacy_timeout_atom"

            [[patterns]]
            ssr = 'ssr: <<"legacy_timeout">>.'
            label = "legacy_timeout_binary"

            [[patterns]]
            ssr = "ssr: retry_forever."
            label = "retry_forever_atom"
            message = "Unbounded retries are not allowed"
        "#]]
        .assert_eq(&written);
    }

    /// `doc` reaches the diagnostic, which is what lets a consumer of the JSON
    /// output find the project's own documentation for a config-declared lint.
    #[test]
    fn ssr_doc_reaches_the_diagnostic() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let (db, file_id) = RootDatabase::with_single_file("t() -> ok.");
        let sema = Semantic::new(&db);
        let mut lint = MatchSsr::from_pattern("ssr: ok.");
        lint.name = Some("my_lint".to_string());
        lint.doc = Some("docs/my_lint.md".to_string());

        let acc = diagnostics_for(&lint, &sema, file_id);
        assert_eq_expected!(1, acc.len());
        assert_eq_expected!(Some("docs/my_lint.md"), acc[0].doc_path_override.as_deref());
    }

    /// Without `doc` there is no override, so the error-index path derived from
    /// the code is used (which is `None` for ad-hoc codes).
    #[test]
    fn ssr_without_doc_has_no_override() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let (db, file_id) = RootDatabase::with_single_file("t() -> ok.");
        let sema = Semantic::new(&db);
        let acc = diagnostics_for(&MatchSsr::from_pattern("ssr: ok."), &sema, file_id);
        assert_eq_expected!(1, acc.len());
        assert_eq_expected!(None, acc[0].doc_path_override.as_deref());
    }

    /// `doc` alone is enough to leave the singular spelling, otherwise it would
    /// be silently dropped on serialization.
    #[test]
    fn serde_match_ssr_doc_round_trips() {
        let lint: MatchSsr = toml::from_str(
            r#"
              doc = "docs/my_lint.md"
              ssr_pattern = "ssr: ok."
             "#,
        )
        .unwrap();
        expect![[r#"
            doc = "docs/my_lint.md"

            [[patterns]]
            ssr = "ssr: ok."
        "#]]
        .assert_eq(&toml::to_string::<MatchSsr>(&lint).unwrap());
    }

    fn named_lint(name: &str, ssr: &str) -> MatchSsr {
        let mut lint = MatchSsr::from_pattern(ssr);
        lint.name = Some(name.to_string());
        lint
    }

    /// Build a config carrying one `[linters.<code>]` section for `name`.
    fn config_for(name: &str, linter_config: LinterConfig) -> DiagnosticsConfig {
        let mut lint_config = LintConfig::default();
        lint_config
            .linters
            .insert(DiagnosticCode::AdHoc(name.to_string()), linter_config);
        DiagnosticsConfig::default()
            .configure_diagnostics(&lint_config, &[], &[])
            .unwrap()
    }

    #[test]
    fn linter_config_can_disable_a_named_lint() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let (db, file_id) = RootDatabase::with_single_file("t() -> ok.");
        let sema = Semantic::new(&db);
        let lint = named_lint("my_lint", "ssr: ok.");

        assert_eq_expected!(1, diagnostics_for(&lint, &sema, file_id).len());

        let config = config_for(
            "my_lint",
            LinterConfig {
                is_enabled: Some(false),
                ..Default::default()
            },
        );
        assert_eq_expected!(
            0,
            diagnostics_for_with(&lint, &sema, file_id, &config).len()
        );
    }

    /// Ad-hoc lints ran on generated files before per-linter config existed, and
    /// still do. Compiled linters additionally require the global
    /// `--include-generated`; borrowing that here would silently stop existing
    /// lints reporting, so only an explicit `include_generated = false` does.
    #[test]
    fn generated_files_are_still_linted_unless_configured_off() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        // `generated_status` scans the file for the marker, so it has to be in
        // the fixture's own text. Built with `format!` so this source file is
        // not itself taken for generated, as elsewhere in the codebase.
        let fixture = format!(
            "
//- /src/main.erl
t() -> ok.
//- /src/main_generated.erl
%% {}generated
t() -> ok.
",
            "@"
        );
        let (db, files, _) = RootDatabase::with_many_files(&fixture);
        let sema = Semantic::new(&db);
        let lint = named_lint("my_lint", "ssr: ok.");
        // Guard against the assertions below passing vacuously.
        assert!(
            sema.db.generated_status(files[1]).is_generated(),
            "fixture is not actually marked generated"
        );

        // Default config: `include_generated` is false on it, exactly as during
        // a plain `elp lint` run without `--include-generated`.
        let counts: Vec<_> = files
            .iter()
            .map(|file_id| diagnostics_for(&lint, &sema, *file_id).len())
            .collect();
        let expected = vec![1, 1];
        assert_eq_expected!(expected, counts);

        let config = config_for(
            "my_lint",
            LinterConfig {
                include_generated: Some(false),
                ..Default::default()
            },
        );
        let counts: Vec<_> = files
            .iter()
            .map(|file_id| diagnostics_for_with(&lint, &sema, *file_id, &config).len())
            .collect();
        // src still reported, the generated module is not.
        let expected = vec![1, 0];
        assert_eq_expected!(expected, counts);
    }

    #[test]
    fn linter_config_can_override_severity() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let (db, file_id) = RootDatabase::with_single_file("t() -> ok.");
        let sema = Semantic::new(&db);
        let lint = named_lint("my_lint", "ssr: ok.");

        // Without an override the lint's own severity applies.
        assert_eq_expected!(
            Severity::WeakWarning,
            diagnostics_for(&lint, &sema, file_id)[0].severity
        );

        let config = config_for(
            "my_lint",
            LinterConfig {
                severity: Some(Severity::Error),
                ..Default::default()
            },
        );
        assert_eq_expected!(
            Severity::Error,
            diagnostics_for_with(&lint, &sema, file_id, &config)[0].severity
        );
    }

    /// Ad-hoc lints run on test modules by default, as they always have. Only an
    /// explicit `include_tests = false` changes that.
    #[test]
    fn linter_config_can_exclude_test_modules() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let (db, files, _) = RootDatabase::with_many_files(
            r#"
//- /src/main.erl
t() -> ok.
//- /test/main_SUITE.erl extra:test
t() -> ok.
"#,
        );
        let sema = Semantic::new(&db);
        let lint = named_lint("my_lint", "ssr: ok.");

        for file_id in &files {
            assert_eq_expected!(1, diagnostics_for(&lint, &sema, *file_id).len());
        }

        let config = config_for(
            "my_lint",
            LinterConfig {
                include_tests: Some(false),
                ..Default::default()
            },
        );
        let counts: Vec<_> = files
            .iter()
            .map(|file_id| diagnostics_for_with(&lint, &sema, *file_id, &config).len())
            .collect();
        // src still reported, the suite is not.
        let expected = vec![1, 0];
        assert_eq_expected!(expected, counts);
    }

    /// Every way a `LintMatchSsr` entry is rejected, as one table rather than a
    /// test apiece: each case is a config and a phrase its error must carry.
    /// Rejection is the whole point of this validation, so the cases are the
    /// interesting part and a list of them reads better than nine near-identical
    /// functions.
    #[test]
    fn serde_match_ssr_rejects_invalid_configs() {
        for (case, config, expected_error) in [
            (
                "both spellings at once",
                "ssr_pattern = 'ssr: ok.'\npatterns = [{ ssr = 'ssr: ok.' }]",
                "specify either 'ssr_pattern' or 'patterns', not both",
            ),
            (
                "neither spelling",
                "name = 'my_lint'",
                "missing 'ssr_pattern' or 'patterns'",
            ),
            (
                "empty patterns",
                "patterns = []",
                "'patterns' must not be empty",
            ),
            (
                "top-level message beside patterns",
                "message = 'oops'\npatterns = [{ ssr = 'ssr: ok.' }]",
                "'message' cannot be used with 'patterns'",
            ),
            (
                "top-level pattern_label beside patterns",
                "pattern_label = 'oops'\npatterns = [{ ssr = 'ssr: ok.' }]",
                "'pattern_label' cannot be used with 'patterns'",
            ),
            (
                "misspelt name",
                "nmae = 'my_lint'\nssr_pattern = 'ssr: ok.'",
                "unknown field `nmae`",
            ),
            (
                "misspelt description",
                "descripton = 'oops'\nssr_pattern = 'ssr: ok.'",
                "unknown field `descripton`",
            ),
            (
                "misspelt label within a pattern",
                "name = 'my_lint'\npatterns = [{ ssr = 'ssr: ok.', lable = 'oops' }]",
                "unknown field `lable`",
            ),
            (
                "empty name",
                "name = ''\nssr_pattern = 'ssr: ok.'",
                "must not be empty",
            ),
            (
                "name with whitespace",
                "name = 'my lint'\nssr_pattern = 'ssr: ok.'",
                "invalid lint name",
            ),
            (
                "name colliding with a built-in code",
                "name = 'no_catch'\nssr_pattern = 'ssr: ok.'",
                "collides with an existing diagnostic code",
            ),
            (
                "name taking the code unnamed lints use",
                "name = 'ssr-match'\nssr_pattern = 'ssr: ok.'",
                "is reserved",
            ),
            (
                "unparseable pattern, named so it can be found",
                "name = 'my_lint'\npatterns = [{ ssr = 'ssr: ok.' }, { ssr = 'ssr: {_@@A, _@@B}.' }]",
                "invalid SSR pattern 'ssr: {_@@A, _@@B}.'",
            ),
        ] {
            let err = toml::from_str::<MatchSsr>(config)
                .expect_err(case)
                .to_string();
            assert!(
                err.contains(expected_error),
                "{case}: expected an error containing {expected_error:?}, got: {err}"
            );
        }
    }

    /// The tag of the `Lint` enum must still reach the variant, which
    /// `deny_unknown_fields` could plausibly have broken.
    #[test]
    fn serde_lint_tag_survives_deny_unknown_fields() {
        let lints: LintsFromConfig = toml::from_str(
            r#"
              [[lints]]
              type = "LintMatchSsr"
              name = "my_lint"
              ssr_pattern = "ssr: ok."
             "#,
        )
        .unwrap();
        match &lints.lints[0] {
            Lint::LintMatchSsr(match_ssr) => {
                assert_eq!(match_ssr.name.as_deref(), Some("my_lint"))
            }
            other => panic!("expected LintMatchSsr, got {other:?}"),
        }
    }

    #[test]
    fn ssr_diagnostic_propagates_pattern_label() {
        use elp_ide_db::elp_base_db::assert_eq_expected;
        let (db, file_id) = RootDatabase::with_single_file("t() -> ok.");
        let sema = Semantic::new(&db);
        let lint = MatchSsr::from_patterns(vec![
            SsrPattern::new("ssr: ok.").with_label(Some("ok_literal".to_string())),
        ]);
        let acc = diagnostics_for(&lint, &sema, file_id);
        assert_eq_expected!(1, acc.len());
        let Some(DiagnosticExtra::Ssr { pattern_label, .. }) = &acc[0].extra else {
            panic!("expected DiagnosticExtra::Ssr, got {:?}", acc[0].extra);
        };
        assert_eq_expected!(Some("ok_literal"), pattern_label.as_deref());
    }
}
