/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::BTreeSet;
use std::fmt;
use std::sync::Arc;

use anyhow::Result;
use anyhow::bail;
use elp_eqwalizer::EqwalizerDiagnostic;
use elp_ide_assists::AssistConfig;
use elp_ide_assists::AssistId;
use elp_ide_assists::AssistKind;
use elp_ide_assists::AssistResolveStrategy;
use elp_ide_assists::GroupLabel;
use elp_ide_db::EqwalizerDatabase;
use elp_ide_db::EqwalizerDiagnostics;
use elp_ide_db::ErlAstDatabase;
use elp_ide_db::LineCol;
use elp_ide_db::LineIndex;
use elp_ide_db::LineIndexDatabase;
use elp_ide_db::assists::Assist;
use elp_ide_db::assists::AssistContextDiagnostic;
use elp_ide_db::assists::AssistContextDiagnosticCode;
use elp_ide_db::common_test::CommonTestDatabase;
use elp_ide_db::common_test::CommonTestInfo;
use elp_ide_db::diagnostic_code::Namespace;
use elp_ide_db::docs::DocDatabase;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::ProjectId;
use elp_ide_db::erlang_service::DiagnosticLocation;
use elp_ide_db::erlang_service::ParseError;
use elp_ide_db::metadata::Kind;
use elp_ide_db::metadata::Metadata;
use elp_ide_db::metadata::Source;
use elp_ide_db::source_change::SourceChange;
use elp_syntax::NodeOrToken;
use elp_syntax::Parse;
use elp_syntax::SourceFile;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use elp_syntax::algo;
use elp_syntax::ast;
use elp_syntax::ast::AstNode;
use elp_syntax::ast::HasLabel;
use elp_syntax::ast::edit;
use elp_syntax::ast::edit::IndentLevel;
use elp_syntax::ast::edit::start_of_line;
use elp_syntax::label::Label;
use elp_syntax::ted::Element;
use elp_text_edit::TextEdit;
use elp_types_db::TypedSemantic;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::InFile;
use hir::Semantic;
use hir::db::DefDatabase;
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use serde::Deserialize;
use serde::Serialize;

use crate::FunctionMatch;
use crate::RootDatabase;
use crate::SourceDatabase;
use crate::codemod_helpers::UseRange;
use crate::common_test;
use crate::diagnostics::helpers::DiagnosticTemplate;
use crate::diagnostics::helpers::FunctionCallDiagnostic;
use crate::diagnostics::helpers::check_used_functions;

mod application_env;
mod atoms_exhaustion;
mod binary_string_to_sigil;
mod boolean_precedence;
mod cross_node_eval;
mod debugging_function;
mod dependent_header;
mod deprecated_function;
mod duplicate_module;
mod edoc;
mod effect_free_statement;
mod equality_check_with_unnecessary_operator;
mod eqwalizer_assists;
mod expression_can_be_simplified;
mod from_config;
mod head_mismatch;
mod helpers;
mod inefficient_enumerate;
mod inefficient_flatlength;
mod inefficient_last;
mod macro_precedence_suprise;
mod map_find_to_syntax;
mod map_insertion_to_syntax;
mod meck;
// @fb-only
mod missing_compile_warn_missing_spec;
mod missing_separator;
mod misspelled_attribute;
mod module_mismatch;
mod mutable_variable;
mod no_catch;
mod no_dialyzer_attribute;
mod no_error_logger;
mod no_garbage_collect;
mod no_nowarn_suppressions;
mod no_size;
mod nonstandard_integer_formatting;
mod record_tuple_match;
mod redundant_assignment;
mod replace_call;
mod replace_in_spec;
mod sets_version_2;
mod simplify_negation;
mod trivial_match;
mod undefined_function;
mod undefined_macro;
mod undocumented_function;
mod undocumented_module;
mod unnecessary_fold_to_build_map;
mod unnecessary_map_from_list_around_comprehension;
mod unnecessary_map_to_list_in_comprehension;
mod unspecific_include;
mod unused_function_args;
mod unused_include;
mod unused_macro;
mod unused_record_field;

pub use elp_ide_db::DiagnosticCode;
pub use from_config::Lint;
pub use from_config::LintsFromConfig;
pub use from_config::ReplaceCall;
pub use from_config::ReplaceCallAction;
pub use replace_call::Replacement;
pub use replace_in_spec::TypeReplacement;

use self::eqwalizer_assists::add_eqwalizer_assists;

/// Macro to create lazily-evaluated function matches for linters
/// This centralizes the lazy_static logic to avoid repetition in every linter
#[macro_export]
macro_rules! lazy_function_matches {
    ($($matches:expr),* $(,)?) => {
        {
            lazy_static::lazy_static! {
                static ref MATCHES: Vec<$crate::FunctionMatch> = vec![$($matches),*]
                    .into_iter()
                    .flatten()
                    .collect();
            }
            MATCHES.clone()
        }
    };
}

pub const DIAGNOSTIC_WHOLE_FILE_RANGE: TextRange = TextRange::empty(TextSize::new(0));

/// A diagnostic may have a tag, which the client is allowed to use
/// when rendereing the item at its range. e.g. grey for Unused,
/// strikethough for Deprecated
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagnosticTag {
    None,
    Unused,
    Deprecated,
}

impl Default for DiagnosticTag {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug, Clone, Default)]
pub struct Diagnostic {
    pub message: String,
    pub range: TextRange,
    pub severity: Severity,
    pub cli_severity: Option<Severity>,
    pub tag: DiagnosticTag,
    pub categories: FxHashSet<Category>,
    pub fixes: Option<Vec<Assist>>,
    pub related_info: Option<Vec<RelatedInformation>>,
    pub code: DiagnosticCode,
    pub code_doc_uri: Option<String>,
}

impl Diagnostic {
    pub fn new(code: DiagnosticCode, message: impl Into<String>, range: TextRange) -> Diagnostic {
        let message = message.into();
        Diagnostic {
            code: code.clone(),
            message,
            range,
            severity: Severity::Error,
            cli_severity: None,
            tag: DiagnosticTag::None,
            categories: FxHashSet::default(),
            fixes: None,
            related_info: None,
            code_doc_uri: code.as_uri(),
        }
    }

    pub(crate) fn with_related(
        mut self,
        related_info: Option<Vec<RelatedInformation>>,
    ) -> Diagnostic {
        self.related_info = related_info;
        self
    }

    pub(crate) fn as_related(&self) -> RelatedInformation {
        RelatedInformation {
            range: self.range,
            message: self.message.clone(),
        }
    }

    fn error(code: DiagnosticCode, range: TextRange, message: String) -> Self {
        Self::new(code, message, range).with_severity(Severity::Error)
    }

    fn warning(code: DiagnosticCode, range: TextRange, message: String) -> Self {
        Self::new(code, message, range).with_severity(Severity::Warning)
    }

    pub(crate) fn with_severity(mut self, severity: Severity) -> Diagnostic {
        self.severity = severity;
        self
    }

    pub(crate) fn with_cli_severity(mut self, severity: Severity) -> Diagnostic {
        self.cli_severity = Some(severity);
        self
    }

    pub(crate) fn unused(mut self) -> Diagnostic {
        self.tag = DiagnosticTag::Unused;
        self
    }

    pub(crate) fn deprecated(mut self) -> Diagnostic {
        self.tag = DiagnosticTag::Deprecated;
        self
    }

    pub(crate) fn with_uri(mut self, uri: Option<String>) -> Diagnostic {
        self.code_doc_uri = uri;
        self
    }

    pub(crate) fn with_fixes(mut self, fixes: Option<Vec<Assist>>) -> Diagnostic {
        self.fixes = fixes;
        self
    }

    pub(crate) fn add_fix(&mut self, fix: Assist) {
        if let Some(fixes) = &mut self.fixes {
            fixes.push(fix);
        } else {
            self.fixes = Some(vec![fix]);
        }
    }

    pub(crate) fn experimental(self) -> Diagnostic {
        self.add_categories([Category::Experimental])
    }

    pub fn has_category(&self, category: Category) -> bool {
        self.categories.contains(&category)
    }

    pub(crate) fn add_categories<I: IntoIterator<Item = Category>>(
        mut self,
        iter: I,
    ) -> Diagnostic {
        self.categories.extend(iter);
        self
    }

    pub(crate) fn should_be_suppressed(
        &self,
        metadata: &Metadata,
        config: &DiagnosticsConfig,
    ) -> bool {
        metadata.by_source(Source::Elp).any(|annotation| {
            if config.include_suppressed && annotation.kind == Kind::Fixme {
                return false;
            }
            annotation.codes.contains(&self.code)
                && (annotation.suppression_range.contains(self.range.start())
                    || self.range == DIAGNOSTIC_WHOLE_FILE_RANGE)
        })
    }

    pub(crate) fn with_ignore_fix(mut self, sema: &Semantic, file_id: FileId) -> Diagnostic {
        let mut builder = TextEdit::builder();
        let parsed = sema.parse(file_id);
        if let Some(token) = parsed
            .value
            .syntax()
            .token_at_offset(self.range.start())
            .right_biased()
        {
            let indent = IndentLevel::from_token(&token);

            let mut offset = start_of_line(&token);
            let mut suffix = "";
            if self.range == DIAGNOSTIC_WHOLE_FILE_RANGE {
                // Change the location to be just before the module form if it exists
                let form_list = sema.form_list(file_id);
                if let Some(module_attribute) = form_list.module_attribute() {
                    let ma_form = module_attribute.form_id.get(&parsed.value);
                    if let Some(ma_token) = parsed
                        .value
                        .syntax()
                        .token_at_offset(ma_form.syntax().text_range().start())
                        .right_biased()
                    {
                        offset = start_of_line(&ma_token);
                    }
                } else {
                    suffix = "\n";
                }
            }
            let text = format!(
                "\n{}% elp:ignore {}{}",
                indent,
                self.code.as_labeled_code(),
                suffix
            );

            builder.insert(offset, text);
            let edit = builder.finish();
            let source_change = SourceChange::from_text_edit(file_id, edit);
            let ignore_fix = Assist {
                id: AssistId("ignore_problem", AssistKind::QuickFix),
                label: Label::new("Ignore problem"),
                group: Some(GroupLabel::ignore()),
                target: self.range,
                source_change: Some(source_change),
                user_input: None,
                original_diagnostic: None,
            };
            match &mut self.fixes {
                Some(fixes) => fixes.push(ignore_fix),
                None => self.fixes = Some(vec![ignore_fix]),
            };
        }
        self
    }

    pub fn print(&self, line_index: &LineIndex, use_cli_severity: bool) -> String {
        let start = line_index.line_col(self.range.start());
        let end = line_index.line_col(self.range.end());

        format!(
            "{}:{}-{}:{}::[{:?}] [{}] {}",
            start.line,
            start.col_utf16,
            end.line,
            end.col_utf16,
            self.severity(use_cli_severity),
            self.code,
            self.message
        )
    }

    pub fn severity(&self, use_cli_severity: bool) -> Severity {
        if use_cli_severity {
            match self.cli_severity {
                Some(severity) => severity,
                None => self.severity,
            }
        } else {
            self.severity
        }
    }

    pub fn as_assist_context_diagnostic(&self) -> AssistContextDiagnostic {
        // We must be careful with the AssistContextDiagnosticCode. It
        // is not a straight mapping to an ElpDiagnosticCode, because
        // it breaks out certain of the Erlang Service error codes
        let code = if let Some(code) =
            AssistContextDiagnosticCode::maybe_from_string(self.code.as_code().as_str())
        {
            code
        } else {
            AssistContextDiagnosticCode::ElpDiagnostic(self.code.clone())
        };

        AssistContextDiagnostic::new(code, self.message.clone(), self.range)
    }

    pub fn get_diagnostic_fixes(&self, db: &RootDatabase, file_id: FileId) -> Vec<Assist> {
        let range = FileRange {
            file_id,
            range: self.range,
        };
        let context_diagnostic = self.as_assist_context_diagnostic();
        let code_action_assists = elp_ide_assists::assists(
            db,
            &AssistConfig {
                snippet_cap: None,
                allowed: None,
            },
            AssistResolveStrategy::All,
            range,
            &[context_diagnostic],
            None,
        );
        // Make sure the assists are related to the original diagnostic.
        let context_diagnostic = self.as_assist_context_diagnostic();
        let assists = code_action_assists
            .into_iter()
            .filter(|assist| assist.original_diagnostic == Some(context_diagnostic.clone()));
        if let Some(fixes) = &self.fixes {
            assists.chain(fixes.clone()).collect_vec()
        } else {
            assists.collect_vec()
        }
    }
}

#[derive(Debug, Clone)]
pub struct RelatedInformation {
    pub range: TextRange,
    pub message: String,
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({}, {:?} {:?} {:?})",
            self.message, self.range, self.severity, self.code
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    // `WeakWarning` maps onto a Notice warning when used in the LSP
    // environment, and in VS Code this means it does not show up in
    // the problems pane, has an unobtrusive underline, but does show
    // up on hover if the cursor is placed on it.
    WeakWarning,
    Information,
}

impl Default for Severity {
    fn default() -> Self {
        Self::Warning // Pick one
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Category {
    Experimental,

    // During codemods, this diagnostic should be used automatically as rewrite-rule
    // to clean-up the code
    SimplificationRule,
}

// ---------------------------------------------------------------------

pub trait AdhocSemanticDiagnostics:
    Fn(&mut Vec<Diagnostic>, &Semantic, FileId, FileKind) + std::panic::RefUnwindSafe + Sync
{
}
impl<F> AdhocSemanticDiagnostics for F where
    F: Fn(&mut Vec<Diagnostic>, &Semantic, FileId, FileKind) + std::panic::RefUnwindSafe + Sync
{
}

// A trait that simplifies writing linters matching function calls
pub(crate) trait FunctionCallLinter {
    // A unique identifier for the linter.
    fn id(&self) -> DiagnosticCode;

    // A plain-text description for the linter. Displayed to the end user.
    fn description(&self) -> String;

    // The severity for the lint issue. It defaults to `Warning`.
    fn severity(&self) -> Severity {
        Severity::Warning
    }

    // Specify if the linter issues can be suppressed via a `% elp:ignore` comment.
    fn can_be_suppressed(&self) -> bool {
        true
    }

    // Specify if the linter should only run when the `--experimental` flag is specified.
    fn is_experimental(&self) -> bool {
        false
    }

    // Specify if the linter is enabled by default.
    fn is_enabled(&self) -> bool {
        true
    }

    // Specify if the linter should process generated files.
    fn should_process_generated_files(&self) -> bool {
        false
    }

    // Specify if the linter should process generated test files (including test helpers)
    fn should_process_test_files(&self) -> bool {
        true
    }

    // Specify the list of functions the linter should emit issues for
    fn matches_functions(&self) -> Vec<FunctionMatch> {
        vec![]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticConditions {
    pub experimental: bool,
    pub include_generated: bool,
    pub include_tests: bool,
    /// By default this diagnostic is disabled. It must be explicitly enabled in
    /// `LintConfig.enabled_lints`
    pub default_disabled: bool,
}

impl DiagnosticConditions {
    fn enabled(&self, config: &DiagnosticsConfig, is_generated: bool, is_test: bool) -> bool {
        let generated_ok = (self.include_generated && config.include_generated) || !is_generated;
        let test_ok = self.include_tests || !is_test;
        let experimental_ok = config.experimental || !self.experimental;

        generated_ok && test_ok && experimental_ok
    }
}

#[derive(Clone)]
pub struct DiagnosticDescriptor<'a> {
    conditions: DiagnosticConditions,
    checker: &'a dyn AdhocSemanticDiagnostics,
}

// ---------------------------------------------------------------------

#[derive(Default, Clone, Debug)]
pub struct EnabledDiagnostics {
    enable_all: bool,
    enabled: FxHashSet<DiagnosticCode>,
    edoc: bool,
}

impl EnabledDiagnostics {
    pub fn new() -> EnabledDiagnostics {
        EnabledDiagnostics {
            enable_all: false,
            enabled: FxHashSet::default(),
            edoc: false,
        }
    }
    pub fn from_set(enabled: FxHashSet<DiagnosticCode>) -> EnabledDiagnostics {
        EnabledDiagnostics {
            enable_all: false,
            enabled,
            edoc: false,
        }
    }
    pub fn enable_all() -> EnabledDiagnostics {
        EnabledDiagnostics {
            enable_all: true,
            enabled: FxHashSet::default(),
            edoc: false,
        }
    }

    pub fn set_edoc(&mut self, value: bool) -> &EnabledDiagnostics {
        self.edoc = value;
        self
    }

    pub fn enable(&mut self, code: DiagnosticCode) -> &EnabledDiagnostics {
        self.enabled.insert(code);
        self
    }

    pub fn contains(&self, code: &DiagnosticCode) -> bool {
        if self.enable_all {
            true
        } else if code.as_namespace() == Some(Namespace::EDoc) {
            self.edoc
        } else {
            self.enabled.contains(code)
        }
    }

    pub fn all_enabled(&self) -> bool {
        self.enable_all
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FallBackToAll {
    Yes,
    No,
}

#[derive(Default, Clone, Debug)]
pub struct DiagnosticsConfig {
    pub experimental: bool,
    pub disabled: FxHashSet<DiagnosticCode>,
    pub enabled: EnabledDiagnostics,
    pub lints_from_config: LintsFromConfig,
    pub include_generated: bool,
    pub include_suppressed: bool,
    pub include_otp: bool,
    pub include_edoc: bool,
    pub use_cli_severity: bool,
    /// Used in `elp lint` to request erlang service diagnostics if
    /// needed.
    pub request_erlang_service_diagnostics: bool,
}

impl DiagnosticsConfig {
    pub fn configure_diagnostics(
        mut self,
        lint_config: &LintConfig,
        diagnostic_filter: &Option<String>,
        diagnostic_ignore: &Option<String>,
        fall_back_to_all: FallBackToAll,
    ) -> Result<DiagnosticsConfig> {
        let mut allowed_diagnostics: FxHashSet<DiagnosticCode> = lint_config
            .enabled_lints
            .iter()
            .cloned()
            .collect::<FxHashSet<_>>();
        let mut disabled_diagnostics: FxHashSet<DiagnosticCode> =
            lint_config.disabled_lints.iter().cloned().collect();

        if let Some(diagnostic_ignore) = diagnostic_ignore {
            let diagnostic_ignore = DiagnosticCode::from(diagnostic_ignore.as_str());
            // Make sure we do not mask the one we explicitly asked for
            allowed_diagnostics.remove(&diagnostic_ignore);
            disabled_diagnostics.insert(diagnostic_ignore);
        }

        if let Some(diagnostic_filter) = diagnostic_filter {
            // We have replaced L1500 with W0020. Generate an error if we get L1500.
            if diagnostic_filter == "L1500" {
                bail!("Code L1500 has been superseded by W0020");
            }

            let diagnostic_filter = DiagnosticCode::from(diagnostic_filter.as_str());
            // Make sure we do not mask the one we explicitly asked for
            disabled_diagnostics.remove(&diagnostic_filter);
            allowed_diagnostics.insert(diagnostic_filter);
        }

        // Make sure the enabled ones win out over disabled if a lint appears in both
        disabled_diagnostics.retain(|d| !allowed_diagnostics.contains(d));

        if allowed_diagnostics.is_empty() && fall_back_to_all == FallBackToAll::No {
            bail!("No diagnostics enabled. Use --diagnostic-filter to specify one.");
        }

        self.disabled = disabled_diagnostics;
        if allowed_diagnostics.is_empty() && fall_back_to_all == FallBackToAll::Yes {
            self.enabled = EnabledDiagnostics::enable_all();
        } else {
            self.enabled = EnabledDiagnostics::from_set(allowed_diagnostics);
        }
        self.lints_from_config = lint_config.ad_hoc_lints.clone();
        self.request_erlang_service_diagnostics = self.request_erlang_service_diagnostics();
        Ok(self)
    }

    pub fn set_experimental(mut self, value: bool) -> DiagnosticsConfig {
        self.experimental = value;
        self
    }

    pub fn set_include_otp(mut self, value: bool) -> DiagnosticsConfig {
        self.include_otp = value;
        self
    }

    pub fn set_include_generated(mut self, value: bool) -> DiagnosticsConfig {
        self.include_generated = value;
        self
    }

    pub fn set_include_suppressed(mut self, value: bool) -> DiagnosticsConfig {
        self.include_suppressed = value;
        self
    }

    pub fn set_include_edoc(mut self, value: bool) -> DiagnosticsConfig {
        self.include_edoc = value;
        self.enabled.set_edoc(value);
        self
    }

    pub fn set_use_cli_severity(mut self, value: bool) -> DiagnosticsConfig {
        self.use_cli_severity = value;
        self
    }

    pub fn enable(mut self, code: DiagnosticCode) -> DiagnosticsConfig {
        self.enabled.enable(code);
        self
    }

    pub fn disable(mut self, code: DiagnosticCode) -> DiagnosticsConfig {
        self.disabled.insert(code);
        self
    }

    pub fn set_lints_from_config(
        mut self,
        lints_from_config: &LintsFromConfig,
    ) -> DiagnosticsConfig {
        self.lints_from_config = lints_from_config.clone();
        self
    }

    /// If any diagnostics are enabled that are produced by the erlang
    /// service, tell `elp lint` to request diagnostics from that source.
    fn request_erlang_service_diagnostics(&self) -> bool {
        self.enabled.enable_all
            || self.enabled.enabled.iter().any(|code| match code {
                DiagnosticCode::ErlangService(_) => true,
                _ => false,
            })
    }
}

// ---------------------------------------------------------------------

/// Configuration file format for lints. Deserialized from .toml
/// initially.  But could by anything supported by serde.
#[derive(Deserialize, Serialize, Default, Debug)]
pub struct LintConfig {
    #[serde(default)]
    pub enabled_lints: Vec<DiagnosticCode>,
    #[serde(default)]
    pub disabled_lints: Vec<DiagnosticCode>,
    #[serde(default)]
    pub ad_hoc_lints: LintsFromConfig,
}

// ---------------------------------------------------------------------

pub type Labeled = FxHashMap<Option<DiagnosticLabel>, Vec<Diagnostic>>;

/// Label for a given diagnostic, giving an MFA as a string if the
/// diagnostic is in a FunDecl, or the range of the enclosing form
/// otherwise.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DiagnosticLabel {
    MFA(Label),
    Range(TextRange),
}

#[derive(Debug, Clone)]
#[derive(Default)]
pub struct LabeledDiagnostics {
    pub normal: Vec<Diagnostic>,
    /// Syntax error diagnostics labeled by the name/arity of the function enclosing them
    pub labeled_syntax_errors: Labeled,
    /// "Undefined XXX" diagnostics labeled by the name/arity of XXX.
    pub labeled_undefined_errors: Labeled,
}

impl LabeledDiagnostics {
    pub fn new(diagnostics: Vec<Diagnostic>) -> LabeledDiagnostics {
        LabeledDiagnostics {
            normal: diagnostics,
            labeled_syntax_errors: FxHashMap::default(),
            labeled_undefined_errors: FxHashMap::default(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> + '_ {
        self.normal
            .iter()
            .chain(self.labeled_syntax_errors.values().flatten())
            .chain(self.labeled_undefined_errors.values().flatten())
    }

    pub fn is_empty(&self) -> bool {
        self.normal.is_empty()
            && self.labeled_syntax_errors.is_empty()
            && self.labeled_undefined_errors.is_empty()
    }

    pub fn len(&self) -> usize {
        self.normal.len() + self.labeled_syntax_errors.len() + self.labeled_undefined_errors.len()
    }

    pub fn extend<I: IntoIterator<Item = Diagnostic>>(&mut self, iter: I) {
        self.normal.extend(iter)
    }
}

pub fn eqwalizer_to_diagnostic(
    sema: &Semantic,
    file_id: FileId,
    d: &EqwalizerDiagnostic,
    eqwalizer_enabled: bool,
) -> Diagnostic {
    let range = d.range;
    let severity = if eqwalizer_enabled {
        Severity::Error
    } else {
        Severity::Information
    };
    let explanation = match &d.explanation {
        Some(s) => format!("\n\n{s}"),
        None => "".to_string(),
    };
    let message = format!(
        "{}{}{}\n        See {}",
        d.expr_string(),
        d.message,
        explanation,
        d.uri
    );
    let mut diagnostic = Diagnostic {
        range,
        severity,
        cli_severity: None,
        tag: DiagnosticTag::None,
        code: DiagnosticCode::Eqwalizer(d.code.clone()),
        message,
        categories: FxHashSet::default(),
        fixes: None,
        related_info: None,
        code_doc_uri: Some(d.uri.clone()),
    };
    add_eqwalizer_assists(sema, file_id, d, &mut diagnostic);
    diagnostic
}

/// Main entry point to calculate ELP-native diagnostics for a file
pub fn native_diagnostics(
    db: &RootDatabase,
    config: &DiagnosticsConfig,
    adhoc_semantic_diagnostics: &Vec<&dyn AdhocSemanticDiagnostics>,
    file_id: FileId,
) -> LabeledDiagnostics {
    lazy_static! {
        static ref EXTENSIONS: FxHashSet<FileKind> = FxHashSet::from_iter(vec![
            FileKind::SrcModule,
            FileKind::TestModule,
            FileKind::Header
        ]);
    };
    let parse = db.parse(file_id);

    let file_kind = db.file_kind(file_id);
    let report_diagnostics = EXTENSIONS.contains(&file_kind);

    let mut res = Vec::new();

    let labeled_syntax_errors = if report_diagnostics {
        let sema = Semantic::new(db);

        if file_kind.is_module() {
            no_module_definition_diagnostic(&mut res, &parse);
            if config.include_generated || !db.is_generated(file_id) {
                unused_include::unused_includes(&sema, db, &mut res, file_id);
            }
        }

        res.append(&mut form_missing_separator_diagnostics(&parse));

        adhoc_semantic_diagnostics
            .iter()
            .for_each(|f| f(&mut res, &sema, file_id, file_kind));
        config
            .lints_from_config
            .get_diagnostics(&mut res, &sema, file_id);
        // @fb-only
        syntax_diagnostics(&sema, &parse, &mut res, file_id);
        diagnostics_from_descriptors(
            &mut res,
            &sema,
            file_id,
            file_kind,
            config,
            &diagnostics_descriptors(),
        );
        diagnostics_from_linters(&mut res, &sema, file_id, config, linters());

        let parse_diagnostics = parse.errors().iter().take(128).map(|err| {
            let (code, message) = match err {
                elp_syntax::SyntaxError::Error(_) => {
                    (DiagnosticCode::SyntaxError, "Syntax Error".to_string())
                }
                elp_syntax::SyntaxError::Missing(m, _) => (
                    DiagnosticCode::Missing("missing".to_string()),
                    format!("Missing '{m}'"),
                ),
            };
            Diagnostic::error(code, widen_range(err.range()), message)
        });
        let source_file = db.parse(file_id).tree();
        label_syntax_errors(&source_file, parse_diagnostics)
    } else {
        FxHashMap::default()
    };
    let metadata = db.elp_metadata(file_id);
    // TODO: can we  ever disable DiagnosticCode::SyntaxError?
    //       In which case we must check labeled_syntax_errors
    res.retain(|d| {
        !config.disabled.contains(&d.code)
            && (config.experimental && d.has_category(Category::Experimental)
                || !d.has_category(Category::Experimental))
            && !d.should_be_suppressed(&metadata, config)
    });

    LabeledDiagnostics {
        normal: res,
        labeled_syntax_errors,
        labeled_undefined_errors: FxHashMap::default(),
    }
}

pub fn diagnostics_descriptors<'a>() -> Vec<&'a DiagnosticDescriptor<'a>> {
    vec![
        &unused_function_args::DESCRIPTOR,
        &trivial_match::DESCRIPTOR,
        &redundant_assignment::DESCRIPTOR,
        &unused_macro::DESCRIPTOR,
        &unused_record_field::DESCRIPTOR,
        &mutable_variable::DESCRIPTOR,
        &effect_free_statement::DESCRIPTOR,
        &simplify_negation::DESCRIPTOR,
        &inefficient_last::DESCRIPTOR,
        &inefficient_flatlength::DESCRIPTOR,
        &inefficient_enumerate::DESCRIPTOR,
        &equality_check_with_unnecessary_operator::DESCRIPTOR,
        &map_insertion_to_syntax::DESCRIPTOR,
        &nonstandard_integer_formatting::DESCRIPTOR,
        &unnecessary_map_from_list_around_comprehension::DESCRIPTOR,
        &unnecessary_map_to_list_in_comprehension::DESCRIPTOR,
        &unnecessary_fold_to_build_map::DESCRIPTOR,
        &map_find_to_syntax::DESCRIPTOR,
        &expression_can_be_simplified::DESCRIPTOR,
        &application_env::DESCRIPTOR,
        &missing_compile_warn_missing_spec::DESCRIPTOR,
        &dependent_header::DESCRIPTOR,
        &deprecated_function::DESCRIPTOR,
        &undefined_function::DESCRIPTOR,
        &head_mismatch::DESCRIPTOR_SEMANTIC,
        &missing_separator::DESCRIPTOR,
        &cross_node_eval::DESCRIPTOR,
        &atoms_exhaustion::DESCRIPTOR,
        &boolean_precedence::DESCRIPTOR,
        &record_tuple_match::DESCRIPTOR,
        &unspecific_include::DESCRIPTOR,
        &edoc::DESCRIPTOR,
        &macro_precedence_suprise::DESCRIPTOR,
        &undocumented_function::DESCRIPTOR,
        &debugging_function::DESCRIPTOR,
        &duplicate_module::DESCRIPTOR,
        &undocumented_module::DESCRIPTOR,
        &no_dialyzer_attribute::DESCRIPTOR,
        &binary_string_to_sigil::DESCRIPTOR,
        &no_catch::DESCRIPTOR,
        &no_nowarn_suppressions::DESCRIPTOR,
    ]
}

pub fn diagnostics_from_descriptors(
    res: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
    file_kind: FileKind,
    config: &DiagnosticsConfig,
    descriptors: &[&DiagnosticDescriptor],
) {
    let is_generated = sema.db.is_generated(file_id);
    let is_test = sema
        .db
        .is_test_suite_or_test_helper(file_id)
        .unwrap_or(false);
    descriptors.iter().for_each(|descriptor| {
        if descriptor.conditions.enabled(config, is_generated, is_test) {
            if descriptor.conditions.default_disabled {
                // Filter the returned diagnostics to ensure they are
                // enabled
                let mut diags: Vec<Diagnostic> = Vec::default();
                (descriptor.checker)(&mut diags, sema, file_id, file_kind);
                for diag in diags {
                    if config.enabled.contains(&diag.code) {
                        res.push(diag);
                    }
                }
            } else {
                (descriptor.checker)(res, sema, file_id, file_kind);
            }
        }
    });
}

/// Registry for function call linters that enables single AST traversal
pub(crate) fn linters() -> Vec<&'static dyn FunctionCallLinter> {
    let mut linters: Vec<&'static dyn FunctionCallLinter> = vec![
        &sets_version_2::LINTER,
        &no_garbage_collect::LINTER,
        &no_size::LINTER,
        &no_error_logger::LINTER,
    ];
    // @fb-only
    linters
}

fn diagnostics_from_linters(
    res: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
    config: &DiagnosticsConfig,
    linters: Vec<&'static dyn FunctionCallLinter>,
) {
    let is_generated = sema.db.is_generated(file_id);
    let is_test = sema
        .db
        .is_test_suite_or_test_helper(file_id)
        .unwrap_or(false);

    let mut specs = Vec::new();
    for linter in linters {
        let conditions = DiagnosticConditions {
            experimental: linter.is_experimental(),
            include_generated: linter.should_process_generated_files(),
            include_tests: linter.should_process_test_files(),
            default_disabled: !linter.is_enabled(),
        };
        if conditions.enabled(config, is_generated, is_test) {
            let diagnostic_template = DiagnosticTemplate {
                code: linter.id(),
                message: linter.description(),
                severity: linter.severity(),
                with_ignore_fix: linter.can_be_suppressed(),
                use_range: UseRange::NameOnly,
            };
            let spec = vec![FunctionCallDiagnostic {
                diagnostic_template,
                matches: linter.matches_functions(),
            }];
            specs.extend(spec);
        }
    }
    if !specs.is_empty() {
        check_used_functions(sema, file_id, &specs, res);
    }
}

fn label_syntax_errors(
    source_file: &SourceFile,
    diagnostics: impl Iterator<Item = Diagnostic>,
) -> Labeled {
    let mut map: Labeled = FxHashMap::default();
    diagnostics.for_each(|d| {
        map.entry(diagnostic_label(&d, source_file))
            .or_default()
            .push(d);
    });
    map
}

/// Label a syntax error diagnostic with either its enclosing
/// `FunDecl` as an MFA string, the range of an enclosing form, if
/// any.
fn diagnostic_label(d: &Diagnostic, source_file: &SourceFile) -> Option<DiagnosticLabel> {
    if d.code.is_syntax_error() {
        let syntax = source_file
            .syntax()
            .token_at_offset(d.range.start())
            .right_biased()
            .and_then(|t| t.parent())?;

        if let Some(fun_decl) = syntax.ancestors().find_map(ast::FunDecl::cast) {
            let fun_label = fun_decl.label()?;
            return Some(DiagnosticLabel::MFA(fun_label));
        } else if let Some(form) = syntax.ancestors().find_map(ast::Form::cast) {
            return Some(DiagnosticLabel::Range(form.syntax().text_range()));
        } else if syntax.ancestors().any(|s| s.kind() == SyntaxKind::ERROR) {
            // we are in an error block after a fun decl, find the preceding form
            let preceding_form = source_file
                .forms()
                .filter(|f| f.syntax().text_range().end() < syntax.text_range().start())
                .last()?;
            // Check it is a FunDecl
            let fun_decl = ast::FunDecl::cast(preceding_form.syntax().clone())?;
            let fun_label = fun_decl.label()?;
            return Some(DiagnosticLabel::MFA(fun_label));
        }
    }
    None
}

/// Promote a zero-width `TextRange` to have width one
fn widen_range(range: TextRange) -> TextRange {
    if range.start() == range.end() {
        TextRange::new(range.start(), range.end() + TextSize::from(1))
    } else {
        range
    }
}

pub fn syntax_diagnostics(
    sema: &Semantic,
    parse: &Parse<ast::SourceFile>,
    res: &mut Vec<Diagnostic>,
    file_id: FileId,
) {
    misspelled_attribute::misspelled_attribute(sema, res, file_id);
    for node in parse.tree().syntax().descendants() {
        head_mismatch::head_mismatch(res, file_id, &node);
        module_mismatch::module_mismatch(sema, res, file_id, &node);
    }
}

pub fn filter_diagnostics(diagnostics: Vec<Diagnostic>, code: DiagnosticCode) -> Vec<Diagnostic> {
    diagnostics.into_iter().filter(|d| d.code == code).collect()
}

fn no_module_definition_diagnostic(
    diagnostics: &mut Vec<Diagnostic>,
    parse: &Parse<ast::SourceFile>,
) {
    let mut report = |range| {
        let diagnostic =
            Diagnostic::new(DiagnosticCode::MissingModule, "no module definition", range);
        diagnostics.push(diagnostic);
    };
    for form in parse.tree().forms() {
        match form {
            ast::Form::PreprocessorDirective(_) => {
                continue; // skip any directives
            }
            ast::Form::FileAttribute(_) => {
                continue; // skip
            }
            ast::Form::ModuleAttribute(_) => {
                break;
            }
            other_form => {
                report(other_form.syntax().text_range());
                break;
            }
        }
    }
}

fn form_missing_separator_diagnostics(parse: &Parse<ast::SourceFile>) -> Vec<Diagnostic> {
    parse
        .tree()
        .forms()
        .flat_map(|form: ast::Form| match form {
            ast::Form::ExportAttribute(f) => {
                check_missing_sep(f.funs(), SyntaxKind::ANON_COMMA, ",", "missing_comma")
            }
            ast::Form::ExportTypeAttribute(f) => {
                check_missing_sep(f.types(), SyntaxKind::ANON_COMMA, ",", "missing_comma")
            }
            ast::Form::ImportAttribute(f) => {
                check_missing_sep(f.funs(), SyntaxKind::ANON_COMMA, ",", "missing_comma")
            }
            ast::Form::RecordDecl(f) => record_decl_check_missing_comma(f),
            ast::Form::TypeAlias(f) => {
                let args = f
                    .name()
                    .and_then(|name| name.args())
                    .into_iter()
                    .flat_map(|args| args.args());
                check_missing_sep(args, SyntaxKind::ANON_COMMA, ",", "missing_comma")
            }
            ast::Form::Opaque(f) => {
                let args = f
                    .name()
                    .and_then(|name| name.args())
                    .into_iter()
                    .flat_map(|args| args.args());
                check_missing_sep(args, SyntaxKind::ANON_COMMA, ",", "missing_comma")
            }
            _ => vec![],
        })
        .collect()
}

fn check_missing_sep<Node: AstNode + std::fmt::Debug>(
    nodes: impl Iterator<Item = Node>,
    separator: SyntaxKind,
    item: &'static str,
    code: &'static str,
) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];

    for node in nodes.skip(1) {
        let syntax = node.syntax();
        if let Some(previous) = non_whitespace_prev_token(syntax) {
            if previous.kind() != separator {
                diagnostics.push(make_missing_diagnostic(
                    previous.text_range(),
                    item,
                    code.to_string(),
                ))
            }
        }
    }

    diagnostics
}

fn record_decl_check_missing_comma(record: ast::RecordDecl) -> Vec<Diagnostic> {
    if let Some(name) = record.name() {
        if let Some(next) = non_whitespace_next_token(name.syntax()) {
            if next.kind() != SyntaxKind::ANON_COMMA {
                return vec![make_missing_diagnostic(
                    name.syntax().text_range(),
                    ",",
                    "missing_comma".to_string(),
                )];
            }
        }
    }

    vec![]
}

fn non_whitespace_next_token(node: &SyntaxNode) -> Option<NodeOrToken> {
    let tok = node.last_token()?;
    let r = edit::next_tokens(tok).skip(1).find(|tok| {
        if let Some(node) = tok.syntax_element().as_token() {
            node.kind() != SyntaxKind::WHITESPACE && node.kind() != SyntaxKind::COMMENT
        } else {
            false
        }
    });
    r.map(NodeOrToken::Token)
}

fn non_whitespace_prev_token(node: &SyntaxNode) -> Option<NodeOrToken> {
    let tok = node.first_token()?;
    let r = edit::prev_tokens(tok).skip(1).find(|tok| {
        if let Some(node) = tok.syntax_element().as_token() {
            node.kind() != SyntaxKind::WHITESPACE && node.kind() != SyntaxKind::COMMENT
        } else {
            false
        }
    });
    r.map(NodeOrToken::Token)
}

pub(crate) fn make_missing_diagnostic(
    range: TextRange,
    item: &'static str,
    code: String,
) -> Diagnostic {
    let message = format!("Missing '{item}'");
    Diagnostic::new(DiagnosticCode::Missing(code), message, range).with_severity(Severity::Warning)
}

pub(crate) fn make_unexpected_diagnostic(
    range: TextRange,
    item: &'static str,
    code: String,
) -> Diagnostic {
    let message = format!("Unexpected '{item}'");
    Diagnostic::new(DiagnosticCode::Unexpected(code), message, range)
        .with_severity(Severity::Warning)
}

#[derive(Debug, PartialEq, Eq)]
pub enum RemoveElpReported {
    Yes,
    No,
}

pub fn erlang_service_diagnostics(
    db: &RootDatabase,
    file_id: FileId,
    config: &DiagnosticsConfig,
    remove_elp_reported: RemoveElpReported,
) -> Vec<(FileId, LabeledDiagnostics)> {
    lazy_static! {
        static ref EXTENSIONS: FxHashSet<FileKind> = FxHashSet::from_iter(vec![
            FileKind::SrcModule,
            FileKind::Escript,
            FileKind::TestModule,
            FileKind::Header
        ]);
    };
    let file_kind = db.file_kind(file_id);
    let report_diagnostics = EXTENSIONS.contains(&file_kind);

    if report_diagnostics && (config.include_generated || !db.is_generated(file_id)) {
        let res = db.module_ast(file_id);

        // We use a BTreeSet of a tuple because neither ParseError nor
        // Diagnostic nor TextRange has an Ord instance
        let mut error_info: BTreeSet<(FileId, TextSize, TextSize, String, String)> =
            BTreeSet::default();
        let mut warning_info: BTreeSet<(FileId, TextSize, TextSize, String, String)> =
            BTreeSet::default();

        res.errors
            .iter()
            .filter_map(|d| parse_error_to_diagnostic_info(db, file_id, d))
            .for_each(|val| {
                error_info.insert(val);
            });
        res.warnings
            .iter()
            .filter_map(|d| parse_error_to_diagnostic_info(db, file_id, d))
            .for_each(|val| {
                warning_info.insert(val);
            });

        let diags: Vec<(FileId, Diagnostic)> = error_info
            .into_iter()
            .map(|(file_id, start, end, code, msg)| {
                (
                    file_id,
                    tag_erlang_service_diagnostic(
                        Diagnostic::new(
                            DiagnosticCode::ErlangService(code),
                            msg,
                            TextRange::new(start, end),
                        )
                        .with_severity(Severity::Error),
                    ),
                )
            })
            .chain(
                warning_info
                    .into_iter()
                    .map(|(file_id, start, end, code, msg)| {
                        (
                            file_id,
                            tag_erlang_service_diagnostic(
                                Diagnostic::new(
                                    DiagnosticCode::ErlangService(code),
                                    msg,
                                    TextRange::new(start, end),
                                )
                                .with_severity(Severity::Warning),
                            ),
                        )
                    }),
            )
            .collect();

        // Remove diagnostics kinds already reported by ELP, and add
        // any ELP-generated assists to those that remain
        let file_kind = db.file_kind(file_id);
        let diags: Vec<(FileId, Diagnostic)> = if remove_elp_reported == RemoveElpReported::Yes {
            diags
                .into_iter()
                .filter(|(_, d)| !is_implemented_in_elp(&d.code, file_kind))
                .collect()
        } else {
            diags
        };

        let metadata = db.elp_metadata(file_id);
        let diags = diags
            .into_iter()
            .filter(|(_file_id, d)| {
                !d.should_be_suppressed(&metadata, config) && !config.disabled.contains(&d.code)
            })
            .map(|(file_id, d)| {
                (
                    file_id,
                    add_elp_assists_to_erlang_service_diagnostic(db, file_id, d),
                )
            })
            .collect_vec();
        let diags = if diags.is_empty() {
            // If there are no diagnostics reported, return an empty list
            // against the `file_id` to clear the list of diagnostics for
            // the file.
            vec![(file_id, vec![])]
        } else {
            let mut diags_map: FxHashMap<FileId, Vec<Diagnostic>> = FxHashMap::default();
            diags.into_iter().for_each(|(file_id, diag)| {
                diags_map
                    .entry(file_id)
                    .and_modify(|existing| existing.push(diag.clone()))
                    .or_insert(vec![diag.clone()]);
            });
            diags_map.into_iter().collect()
        };

        label_erlang_service_diagnostics(db, diags)
    } else {
        vec![]
    }
}

fn tag_erlang_service_diagnostic(d: Diagnostic) -> Diagnostic {
    if is_erlang_service_unused_x_diagnostic(&d.code) {
        d.unused()
    } else if is_erlang_service_deprecated_diagnostic(&d.code) {
        d.deprecated()
    } else {
        d
    }
}

fn add_elp_assists_to_erlang_service_diagnostic(
    db: &RootDatabase,
    file_id: FileId,
    d: Diagnostic,
) -> Diagnostic {
    match &d.code {
        DiagnosticCode::ErlangService(s) => match s.as_str() {
            "E1507" | "E1508" => {
                let mut d = d.clone();
                undefined_macro::add_assist(&Semantic::new(db), file_id, &mut d);
                d
            }
            _ => d,
        },
        _ => d,
    }
}

/// We split erlang service diagnostics into syntax errors and others.
///
/// The syntax errors we label with either the MFA string of an
/// enclosing `ast::Fundecl`, or the range of an enclosing form, if
/// any.
///
/// Any of the others with references to undefined function or
/// undefined spec get labeled with the corresponding MFA string.
/// This is so we can tie them up to ELP native parse errors occurring
/// in a function that has the same label.
fn label_erlang_service_diagnostics(
    db: &RootDatabase,
    diagnostics: Vec<(FileId, Vec<Diagnostic>)>,
) -> Vec<(FileId, LabeledDiagnostics)> {
    diagnostics
        .into_iter()
        .map(|(file_id, ds)| {
            let source_file = db.parse(file_id).tree();
            let mut labeled_undefined_errors: Labeled = FxHashMap::default();
            let mut labeled_syntax_errors: Labeled = FxHashMap::default();
            ds.into_iter().for_each(|d| {
                if d.code.is_syntax_error() {
                    labeled_syntax_errors
                        .entry(diagnostic_label(&d, &source_file))
                        .or_default()
                        .push(d);
                } else {
                    labeled_undefined_errors
                        .entry(erlang_service_label(&d))
                        .or_default()
                        .push(d);
                }
            });
            (
                file_id,
                LabeledDiagnostics {
                    normal: Vec::default(),
                    labeled_syntax_errors,
                    labeled_undefined_errors,
                },
            )
        })
        .collect_vec()
}

pub fn eqwalizer_diagnostics(db: &RootDatabase, file_id: FileId) -> Option<Vec<Diagnostic>> {
    let eqwalizer_diagnostics = db.eqwalizer_diagnostics(file_id)?;
    // Because of the way db.eqwalizer_diagnostics() is implemented,
    // we only get diagnostics if it is enabled.
    let eqwalizer_enabled = true;
    let sema = Semantic::new(db);
    Some(
        eqwalizer_diagnostics
            .iter()
            .map(|d| eqwalizer_to_diagnostic(&sema, file_id, d, eqwalizer_enabled))
            .collect(),
    )
}

pub fn to_standard_diagnostics(
    db: &RootDatabase,
    project_id: ProjectId,
    diagnostics: EqwalizerDiagnostics,
) -> Option<Vec<(FileId, Vec<Diagnostic>)>> {
    let sema = Semantic::new(db);
    let module_index = db.module_index(project_id);

    let mut res = FxHashMap::default();
    if let elp_eqwalizer::EqwalizerDiagnostics::Diagnostics { errors, .. } = diagnostics {
        errors
            .iter()
            .map(|(module, ds)| {
                for d in ds {
                    if let Some(file_id) = module_index.file_for_module(module.as_str()) {
                        let value = res.entry(file_id).or_insert(Vec::new());
                        value.push(eqwalizer_to_diagnostic(&sema, file_id, d, true))
                    }
                }
            })
            .collect()
    }
    Some(res.into_iter().collect())
}

pub fn eqwalizer_stats(
    db: &RootDatabase,
    project_id: ProjectId,
    file_id: FileId,
) -> Option<Vec<Diagnostic>> {
    let eqwalizer_diagnostics = db.eqwalizer_stats(project_id, file_id)?;
    // Report as info
    let eqwalizer_enabled = false;
    let sema = Semantic::new(db);
    Some(
        eqwalizer_diagnostics
            .iter()
            .map(|d| eqwalizer_to_diagnostic(&sema, file_id, d, eqwalizer_enabled))
            .collect(),
    )
}

pub fn edoc_diagnostics(
    db: &RootDatabase,
    file_id: FileId,
    config: &DiagnosticsConfig,
) -> Vec<(FileId, Vec<Diagnostic>)> {
    if !config.include_generated && db.is_generated(file_id) {
        return vec![];
    }

    // We use a BTreeSet of a tuple because neither ParseError nor
    // Diagnostic nor TextRange has an Ord instance
    let mut error_info: BTreeSet<(FileId, TextSize, TextSize, String, String)> =
        BTreeSet::default();
    let mut warning_info: BTreeSet<(FileId, TextSize, TextSize, String, String)> =
        BTreeSet::default();

    // If the file cannot be parsed, it does not really make sense to run EDoc,
    // so let's return early.
    let ast = db.module_ast(file_id);
    if !ast.is_ok() {
        return vec![];
    };

    let res = db.file_doc(file_id);
    let line_index = db.file_line_index(file_id);

    res.diagnostics.iter().for_each(|d| {
        // While line number in EDoc diagnostics are 1 based,
        // EDoc can return some error messages for the entire module with
        // a default location of 0.
        // We normalize it to 1, so it can be correctly displayed on the first line of the module.
        // See: https://github.com/erlang/otp/blob/f9e367c1992735164b0e6c96881c35a30890aed2/lib/edoc/src/edoc.erl#L778-L782
        let line = if d.line == 0 { 1 } else { d.line };
        let start = line_index
            .safe_offset(LineCol {
                line: line - 1,
                col_utf16: 0,
            })
            .unwrap_or(TextSize::from(0));
        let end = line_index
            .safe_offset(LineCol { line, col_utf16: 0 })
            .unwrap_or(TextSize::from(0));
        let message = &d.message;
        let val = (file_id, start, end, d.code.clone(), message.clone());
        match d.severity.as_str() {
            "error" => {
                error_info.insert(val);
            }
            "warning" => {
                warning_info.insert(val);
            }
            _ => (),
        }
    });

    let metadata = db.elp_metadata(file_id);
    let diags: Vec<(FileId, Diagnostic)> = error_info
        .into_iter()
        .map(|(file_id, start, end, code, msg)| {
            (
                file_id,
                Diagnostic::new(
                    DiagnosticCode::ErlangService(code),
                    msg,
                    TextRange::new(start, end),
                )
                .with_severity(Severity::Warning),
            )
        })
        .chain(
            warning_info
                .into_iter()
                .map(|(file_id, start, end, code, msg)| {
                    (
                        file_id,
                        Diagnostic::new(
                            DiagnosticCode::ErlangService(code),
                            msg,
                            TextRange::new(start, end),
                        )
                        .with_severity(Severity::Warning),
                    )
                }),
        )
        .collect();

    if diags.is_empty() {
        // If there are no diagnostics reported, return an empty list
        // against the `file_id` to clear the list of diagnostics for
        // the file.
        vec![(file_id, vec![])]
    } else {
        let mut diags_map: FxHashMap<FileId, Vec<Diagnostic>> = FxHashMap::default();
        diags
            .into_iter()
            .filter(|(_file_id, d)| {
                !d.should_be_suppressed(&metadata, config) && !config.disabled.contains(&d.code)
            })
            .for_each(|(file_id, diag)| {
                diags_map
                    .entry(file_id)
                    .and_modify(|existing| existing.push(diag.clone()))
                    .or_insert(vec![diag.clone()]);
            });
        diags_map.into_iter().collect()
    }
}

pub fn ct_info(db: &RootDatabase, file_id: FileId) -> Arc<CommonTestInfo> {
    if db.file_kind(file_id) != FileKind::TestModule {
        return Arc::new(CommonTestInfo::Skipped);
    }

    db.ct_info(file_id)
}

/// Diagnostics requiring the erlang_service
pub fn ct_diagnostics(
    db: &RootDatabase,
    file_id: FileId,
    config: &DiagnosticsConfig,
) -> Vec<Diagnostic> {
    let mut res: Vec<Diagnostic> = Vec::new();
    let sema = Semantic::new(db);

    meck::missing_no_link_in_init_per_suite(&mut res, &sema, file_id);

    match &*ct_info(db, file_id) {
        CommonTestInfo::Result { all, groups } => {
            let testcases = common_test::runnable_names(&sema, file_id, all, groups).ok();
            common_test::unreachable_test(&mut res, &sema, file_id, &testcases);
            // @fb-only
        }
        CommonTestInfo::EvalError(_error) => {
            // The error currently does not contain anything useful, so we ignore it
            common_test::ct_info_eval_error(&mut res, &sema, file_id);
        }
        _ => (),
    };
    let metadata = db.elp_metadata(file_id);
    res.into_iter()
        .filter(|d| !d.should_be_suppressed(&metadata, config))
        .collect()
}

/// Match the message part of the diagnostics produced by the
/// erlang_service but already implemented natively in ELP
pub fn is_implemented_in_elp(code: &DiagnosticCode, file_kind: FileKind) -> bool {
    if file_kind == FileKind::Escript {
        false
    } else {
        match code {
            DiagnosticCode::ErlangService(s) => match s.as_str() {
                "P1700" => true, // "head mismatch"
                "L1201" => true, // "no module definition"
                _ => false,
            },
            _ => false,
        }
    }
}

pub fn is_erlang_service_syntax_error(code: &DiagnosticCode) -> bool {
    match code {
        DiagnosticCode::ErlangService(s) => match s.as_str() {
            "P1711" => true, // Syntax error
            _ => false,
        },
        _ => false,
    }
}

pub fn is_erlang_service_unused_x_diagnostic(code: &DiagnosticCode) -> bool {
    match code {
        DiagnosticCode::ErlangService(s) => match s.as_str() {
            "L1226" => true, // Unused import
            "L1230" => true, // Unused function
            "L1260" => true, // Unused record
            "L1268" => true, // Unused var
            "L1296" => true, // Unused type
            _ => false,
        },
        _ => false,
    }
}

pub fn is_erlang_service_deprecated_diagnostic(code: &DiagnosticCode) -> bool {
    match code {
        DiagnosticCode::ErlangService(s) => match s.as_str() {
            "L1235" => true, // Deprecated
            "L1236" => true, // Deprecated
            "L1237" => true, // Deprecated type
            "L1238" => true, // Deprecated type
            "L1312" => true, // Deprecated builtin type
            "L1319" => true, // Deprecated callback
            "L1320" => true, // Deprecated callback
            _ => false,
        },
        _ => false,
    }
}

fn parse_error_to_diagnostic_info(
    db: &RootDatabase,
    file_id: FileId,
    parse_error: &ParseError,
) -> Option<(FileId, TextSize, TextSize, String, String)> {
    match parse_error.location {
        Some(DiagnosticLocation::Included {
            directive_location,
            error_location,
        }) => included_file_file_id(db, file_id, directive_location).map(|included_file_id| {
            (
                included_file_id,
                error_location.start(),
                error_location.end(),
                parse_error.code.clone(),
                parse_error.msg.clone(),
            )
        }),
        Some(DiagnosticLocation::Normal(range)) => {
            let default_range = (
                file_id,
                range.start(),
                range.end(),
                parse_error.code.clone(),
                parse_error.msg.clone(),
            );
            match parse_error.code.as_str() {
                // For certain warnings, OTP returns a diagnostic with a wide range (e.g. a full record definition)
                // That can be very verbose and distracting, so we try restricting the range to the relevant parts only.
                "L1227" => {
                    let name = function_undefined_from_message(&parse_error.msg);
                    match exported_function_name_range(db, file_id, name, range) {
                        Some(name_range) => Some((
                            file_id,
                            name_range.start(),
                            name_range.end(),
                            parse_error.code.clone(),
                            parse_error.msg.clone(),
                        )),
                        None => Some(default_range),
                    }
                }
                "L1295" => {
                    let name = type_undefined_from_message(&parse_error.msg);
                    match exported_type_name_range(db, file_id, name, range) {
                        Some(name_range) => Some((
                            file_id,
                            name_range.start(),
                            name_range.end(),
                            parse_error.code.clone(),
                            parse_error.msg.clone(),
                        )),
                        None => Some(default_range),
                    }
                }
                "L1230" | "L1309" => match function_name_range(db, file_id, range) {
                    Some(name_range) => Some((
                        file_id,
                        name_range.start(),
                        name_range.end(),
                        parse_error.code.clone(),
                        parse_error.msg.clone(),
                    )),
                    None => Some(default_range),
                },
                "L1296" => match type_alias_name_range(db, file_id, range) {
                    Some(name_range) => Some((
                        file_id,
                        name_range.start(),
                        name_range.end(),
                        parse_error.code.clone(),
                        parse_error.msg.clone(),
                    )),
                    None => Some(default_range),
                },
                "L1308" => match spec_name_range(db, file_id, range) {
                    Some(name_range) => Some((
                        file_id,
                        name_range.start(),
                        name_range.end(),
                        parse_error.code.clone(),
                        parse_error.msg.clone(),
                    )),
                    None => Some(default_range),
                },
                "L1260" => match record_name_range(db, file_id, range) {
                    Some(name_range) => Some((
                        file_id,
                        name_range.start(),
                        name_range.end(),
                        parse_error.code.clone(),
                        parse_error.msg.clone(),
                    )),
                    None => Some(default_range),
                },
                _ => Some(default_range),
            }
        }
        None => Some((
            file_id,
            TextSize::default(),
            TextSize::default(),
            parse_error.code.clone(),
            parse_error.msg.clone(),
        )),
    }
}

fn exported_function_name_range(
    db: &RootDatabase,
    file_id: FileId,
    name: Option<String>,
    range: TextRange,
) -> Option<TextRange> {
    let name = name?;
    let sema = Semantic::new(db);
    let source_file = sema.parse(file_id);
    let export = algo::find_node_at_offset::<ast::ExportAttribute>(
        source_file.value.syntax(),
        range.start(),
    )?;
    export.funs().find_map(|fun| {
        if fun.to_string() == name {
            Some(fun.syntax().text_range())
        } else {
            None
        }
    })
}

fn exported_type_name_range(
    db: &RootDatabase,
    file_id: FileId,
    name: Option<String>,
    range: TextRange,
) -> Option<TextRange> {
    let name = name?;
    let sema = Semantic::new(db);
    let source_file = sema.parse(file_id);
    let export_type = algo::find_node_at_offset::<ast::ExportTypeAttribute>(
        source_file.value.syntax(),
        range.start(),
    )?;
    export_type.types().find_map(|fun| {
        if fun.to_string() == name {
            Some(fun.syntax().text_range())
        } else {
            None
        }
    })
}

fn function_name_range(db: &RootDatabase, file_id: FileId, range: TextRange) -> Option<TextRange> {
    let sema = Semantic::new(db);
    let source_file = sema.parse(file_id);
    let function =
        algo::find_node_at_offset::<ast::FunDecl>(source_file.value.syntax(), range.start())?;
    Some(function.name()?.syntax().text_range())
}

fn record_name_range(db: &RootDatabase, file_id: FileId, range: TextRange) -> Option<TextRange> {
    let sema = Semantic::new(db);
    let source_file = sema.parse(file_id);
    let record =
        algo::find_node_at_offset::<ast::RecordDecl>(source_file.value.syntax(), range.start())?;
    Some(record.name()?.syntax().text_range())
}

fn spec_name_range(db: &RootDatabase, file_id: FileId, range: TextRange) -> Option<TextRange> {
    let sema = Semantic::new(db);
    let source_file = sema.parse(file_id);
    let spec = algo::find_node_at_offset::<ast::Spec>(source_file.value.syntax(), range.start())?;
    Some(spec.fun()?.syntax().text_range())
}

fn type_alias_name_range(
    db: &RootDatabase,
    file_id: FileId,
    range: TextRange,
) -> Option<TextRange> {
    let sema = Semantic::new(db);
    let source_file = sema.parse(file_id);
    let type_alias =
        algo::find_node_at_offset::<ast::TypeAlias>(source_file.value.syntax(), range.start())?;
    Some(type_alias.name()?.name()?.syntax().text_range())
}

/// For an error in an included file, find the include directive, work
/// out what include file it refers to, get its FileId
pub fn included_file_file_id(
    db: &RootDatabase,
    file_id: FileId,
    directive_range: TextRange,
) -> Option<FileId> {
    let parsed = db.parse(file_id);
    let form_list = db.file_form_list(file_id);
    let include = form_list.includes().find_map(|(idx, include)| {
        let form = include.form_id().get(&parsed.tree());
        if form.syntax().text_range().contains(directive_range.start()) {
            db.resolve_include(InFile::new(file_id, idx))
        } else {
            None
        }
    })?;
    Some(include)
}

/// Given syntax errors from ELP native and erlang service, combine
/// them by discarding any erlang service syntax errors for a form if
/// ELP has reported any too.  This is done by merging labels, with
/// ELP native taking priority.
fn combine_syntax_errors(native: &Labeled, erlang_service: &Labeled) -> Labeled {
    let mut res = native.clone();
    erlang_service.iter().for_each(|(label, diags)| {
        if label.is_some() {
            res.entry(label.clone()).or_insert(diags.clone());
        } else {
            // If there is no enclosing form, keep both sets
            res.entry(label.clone())
                .or_default()
                .extend(diags.iter().cloned());
        }
    });
    res
}

/// Combine the ELP and erlang_service diagnostics.  In particular,
/// flatten any cascading diagnostics if possible.
pub fn attach_related_diagnostics(
    native: LabeledDiagnostics,
    erlang_service: LabeledDiagnostics,
) -> Vec<Diagnostic> {
    // Requirements
    // - Both ELP and Erlang Service report syntax errors. Ensure only
    //   one of these sources is used per form, giving ELP priority.
    // - Erlang Service reports "undefined XXX" errors wherever
    //   a function with a syntax error is used.  In this case, make
    //   the "undefined" error a related diagnostic to they syntax
    //   error, rather than a stand alone one.

    // There is a many-to-many relationship between syntax errors and
    // related info, because there can be more than one syntax error
    // in a given function, and the function may be used in multiple
    // places so generate many "undefined XXX" diagnostics.  So we
    // keep track of "undefined XXX" diagnostics that have been used
    // as related info, then remove them at the end.

    // Step one.
    // Harmonise the ELP native and erlang service syntax errors.
    let combined_labeled_syntax_errors = combine_syntax_errors(
        &native.labeled_syntax_errors,
        &erlang_service.labeled_syntax_errors,
    );

    // Step two.
    // - Attach related info to any syntax errors in a given MFA that
    //   cause an "undefined MFA" diagnostic.
    // - Mark the MFA as being dealt with, and so added to the `to_remove` list.
    let mut undefineds_to_remove: FxHashSet<&Option<DiagnosticLabel>> = FxHashSet::default();
    let syntax_errors_with_related = combined_labeled_syntax_errors
        .iter()
        .flat_map(|(mfa_label, syntax_error_diags)| {
            if let Some(related) = erlang_service.labeled_undefined_errors.get(mfa_label) {
                undefineds_to_remove.insert(mfa_label);
                let related_info = related.iter().map(|d| d.as_related()).collect_vec();
                syntax_error_diags
                    .iter()
                    .map(|d| d.clone().with_related(Some(related_info.clone())))
                    .collect_vec()
            } else {
                syntax_error_diags.to_vec()
            }
        })
        .collect_vec();

    // Step 3.
    // Filter the "undefined XXX" syntax errors to remove the ones
    // that will be reported as related information in a syntax error
    // diagnostic.
    let erlang_service_undefined_not_related = erlang_service
        .labeled_undefined_errors
        .iter()
        .filter(|(k, _)| !undefineds_to_remove.contains(k))
        .flat_map(|(_, v)| v);

    native
        .normal
        .into_iter()
        .chain(erlang_service.normal)
        .chain(syntax_errors_with_related)
        .chain(erlang_service_undefined_not_related.cloned())
        // TODO:AZ: consider returning an iterator
        .collect_vec()
}

fn erlang_service_label(diagnostic: &Diagnostic) -> Option<DiagnosticLabel> {
    match &diagnostic.code {
        DiagnosticCode::ErlangService(s) => match s.as_str() {
            "L1227" => function_undefined_from_message(&diagnostic.message)
                .map(|s| DiagnosticLabel::MFA(Label::new_raw(s))),
            "L1308" => spec_for_undefined_function_from_message(&diagnostic.message)
                .map(|s| DiagnosticLabel::MFA(Label::new_raw(s))),
            _ => None,
        },
        _ => None,
    }
}

pub fn function_undefined_from_message(s: &str) -> Option<String> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^function ([^\s]+) undefined$").unwrap();
    }
    RE.captures_iter(s).next().map(|c| c[1].to_string())
}

pub fn type_undefined_from_message(s: &str) -> Option<String> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^type ([^\s]+) undefined$").unwrap();
    }
    RE.captures_iter(s).next().map(|c| c[1].to_string())
}

pub fn spec_for_undefined_function_from_message(s: &str) -> Option<String> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^spec for undefined function ([^\s]+)$").unwrap();
    }
    RE.captures_iter(s).next().map(|c| c[1].to_string())
}

// ---------------------------------------------------------------------

// To run the tests via cargo
// cargo test --package elp_ide --lib
#[cfg(test)]
mod tests {
    use elp_project_model::otp::otp_supported_by_eqwalizer;
    use elp_project_model::otp::supports_eep59_doc_attributes;
    use expect_test::expect;

    use super::*;
    use crate::codemod_helpers::FunctionMatch;
    use crate::codemod_helpers::MFA;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests::check_diagnostics;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_diagnostics_with_config_and_ad_hoc;
    use crate::tests::check_diagnostics_with_config_and_extra;
    use crate::tests::check_specific_fix;

    #[test]
    fn syntax_error() {
        check_diagnostics(
            r#"
-module(main).
foo() -> XX 3.0.
    %%   ^^ error: Syntax Error
"#,
        );
    }

    #[test]
    fn export_attribute_missing_comma() {
        check_diagnostics(
            r#"
-module(main).
-export([foo/0 bar/1]).
    %%       ^ warning: Missing ','
"#,
        );
    }

    #[test]
    fn export_type_attribute_missing_comma() {
        check_diagnostics(
            r#"
-module(main).
-export_type([foo/0 bar/1]).
         %%       ^ warning: Missing ','
"#,
        );
    }

    #[test]
    fn import_attribute_missing_comma() {
        check_diagnostics(
            r#"
-module(main).
-import(bb, [foo/0 bar/1]).
         %%      ^ warning: Missing ','
"#,
        );
    }

    #[test]
    fn type_decl_missing_comma() {
        check_diagnostics(
            r#"
-module(main).
-type foo(A B) :: [A,B].
       %% ^ warning: Missing ','
"#,
        );
    }

    #[test]
    fn record_decl_missing_comma() {
        check_diagnostics(
            r#"
-module(main).
-record(foo  {f1, f2 = 3}).
     %% ^^^ warning: Missing ','
main(X) ->
  {X#foo.f1, X#foo.f2}.
"#,
        );
    }

    #[test]
    fn record_decl_no_warning() {
        check_diagnostics(
            r#"
-module(main).
-define(NAME, name).
-record(?NAME, {}).
"#,
        )
    }

    // #[test]
    // fn define_type_missing_comma() {
    //     let mut parser = Parser::new();
    //     let text = concat!("-define(foo,  [?F1, ?F2]).");

    //     let source_fn = |range: Range<usize>| text[range.start..range.end].to_string();
    //     let parsed = Arc::new(to_sourcefile(&parser.parse(&text), &source_fn));
    //     let d = crate::diagnostics::form_missing_separator_diagnostics(parsed);
    //     assert_eq!(
    //         format!("{:?}", d),
    //         "[Diagnostic { message: \"Missing ','\", range: 8..11, severity: Warning, code: Some(DiagnosticCode(\"missing_comma\")) }]"
    //     )
    // }

    #[test]
    fn fun_decl_module_decl_ok() {
        check_diagnostics(
            r#"
-file("main.erl",1).
-define(baz,4).
-module(main).
foo(2)->?baz.
"#,
        );
    }

    #[test]
    fn fun_decl_module_decl_missing() {
        check_diagnostics(
            r#"
  -file("foo.erl",1).
  -define(baz,4).
  foo(2)->?baz.
%%^^^^^^^^^^^^^ error: no module definition
"#,
        );
    }

    #[test]
    fn fun_decl_module_decl_missing_2() {
        check_diagnostics(
            r#"
  baz(1)->4.
%%^^^^^^^^^^ error: no module definition
  foo(2)->3.
"#,
        );
    }

    #[test]
    fn fun_decl_module_decl_after_preprocessor() {
        check_diagnostics(
            r#"
-ifndef(snmpm_net_if_mt).
-module(main).
-endif.
baz(1)->4.
"#,
        );
    }

    #[test]
    fn filter_diagnostics() {
        let diag1 = DiagnosticCode::ErlangService("P1700".to_string());
        let diag2 = DiagnosticCode::ErlangService("L1201".to_string());
        let diag3 = DiagnosticCode::ErlangService("P1711".to_string());
        let diagk = DiagnosticCode::ErlangService("another diagnostic".to_string());
        let diags = vec![diag1, diag2, diag3.clone(), diagk.clone()];
        assert_eq!(
            diags
                .into_iter()
                .filter(|d| !is_implemented_in_elp(d, FileKind::SrcModule))
                .collect::<Vec<_>>(),
            vec![diag3, diagk]
        );
    }

    #[test]
    fn filter_diagnostics_escript() {
        let diag1 = DiagnosticCode::ErlangService("P1700".to_string());
        let diag2 = DiagnosticCode::ErlangService("L1201".to_string());
        let diag3 = DiagnosticCode::ErlangService("P1711".to_string());
        let diagk = DiagnosticCode::ErlangService("another diagnostic".to_string());
        let diags = vec![diag1.clone(), diag2.clone(), diag3.clone(), diagk.clone()];
        assert_eq!(
            diags
                .into_iter()
                .filter(|d| !is_implemented_in_elp(d, FileKind::Escript))
                .collect::<Vec<_>>(),
            vec![diag1, diag2, diag3, diagk]
        );
    }

    #[test]
    fn filter_experimental() {
        let config = DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction);
        check_diagnostics_with_config_and_ad_hoc(
            DiagnosticsConfig {
                experimental: true,
                ..config.clone()
            },
            &vec![&|acc, sema, file_id, _ext| {
                replace_call::replace_call_site(
                    &FunctionMatch::MFA {
                        mfa: MFA {
                            module: "foo".into(),
                            name: "bar".into(),
                            arity: 0,
                        },
                    },
                    replace_call::Replacement::UseOk,
                    &replace_call::adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            r#"
            //- /src/main.erl
            -module(main).

            do_foo() ->
              X = foo:bar(),
            %%    ^^^^^^^^^ 💡 weak: 'foo:bar/0' called
              X.
            //- /src/foo.erl
            -module(foo).
            -export([bar/0]).
            bar() -> ok.
            "#,
        );
        check_diagnostics_with_config(
            DiagnosticsConfig {
                experimental: false,
                ..config
            },
            r#"
            -module(main).

            do_foo() ->
              X = foo:bar(),
              X.
            "#,
        )
    }

    #[test]
    fn label_syntax_error_not_function() {
        let fixture_str = r#"
  -module(main).
  -record(person, {(name + XXX)}).
%%                 ^^^^^^^ error: Syntax Error
%%                            ^ error: Syntax Error
"#;
        check_diagnostics(fixture_str);
        let diagnostic = Diagnostic::error(
            DiagnosticCode::SyntaxError,
            TextRange::new(36.into(), 43.into()),
            "Syntax Error".to_owned(),
        );
        let source_file = SourceFile::parse_text(fixture_str);
        expect![[r#"
            Some(
                Range(
                    20..52,
                ),
            )
        "#]]
        .assert_debug_eq(&diagnostic_label(&diagnostic, &source_file.tree()));
    }

    #[test]
    fn elp_ignore_1() {
        check_diagnostics(
            r#"
  baz(1)->4.
%%^^^^^^^^^^ error: no module definition
  foo(2)->3.
"#,
        );
    }

    #[test]
    fn elp_ignore_2() {
        check_diagnostics(
            r#"
 %% elp:ignore L1201
  baz(1)->4.
  foo(2)->3.
"#,
        );
    }

    #[test]
    fn elp_ignore_3() {
        check_diagnostics(
            r#"
 %% elp:ignore L1201

  baz(1)->4.
%%^^^^^^^^^^ error: no module definition
  foo(2)->3.
"#,
        );
    }

    #[test]
    fn elp_ignore_4() {
        check_diagnostics(
            r#"
             -module(main).

             baz()->
               Foo = 1,
             %%^^^ 💡 warning: match is redundant
               % elp:ignore W0007
               Bar = 2,
               ok.
             "#,
        );
    }

    #[test]
    fn elp_ignore_5() {
        check_diagnostics(
            r#"
             -module(main).

             baz()->
               Foo = 1,
             %%^^^ 💡 warning: match is redundant
               % elp:ignore W0007

               Bar = 2,
             %%^^^ 💡 warning: match is redundant
               ok.
             "#,
        );
    }

    #[test]
    fn edoc_diagnostics() {
        check_diagnostics(
            r#"
             //- edoc
             //- /main/src/main_edoc.erl app:main
             % @unknown
             %%<^^^^^^^^  warning: tag @unknown not recognized.
             -module(main_edoc).

             "#,
        );
    }

    #[test]
    fn group_related_diagnostics_1() {
        let labeled_undefined_errors = FxHashMap::from_iter([(
            Some(DiagnosticLabel::MFA(Label::new_raw("foo/0"))),
            vec![
                Diagnostic {
                    message: "function foo/0 undefined".to_string(),
                    range: TextRange::new(21.into(), 43.into()),
                    severity: Severity::Error,
                    cli_severity: None,
                    tag: DiagnosticTag::None,
                    categories: FxHashSet::default(),
                    fixes: None,
                    related_info: None,
                    code: "L1227".into(),
                    code_doc_uri: None,
                },
                Diagnostic {
                    message: "function foo/0 undefined".to_string(),
                    range: TextRange::new(74.into(), 79.into()),
                    severity: Severity::Error,
                    cli_severity: None,
                    tag: DiagnosticTag::None,
                    categories: FxHashSet::default(),
                    fixes: None,
                    related_info: None,
                    code: "L1227".into(),
                    code_doc_uri: None,
                },
                Diagnostic {
                    message: "spec for undefined function foo/0".to_string(),
                    range: TextRange::new(82.into(), 99.into()),
                    severity: Severity::Error,
                    cli_severity: None,
                    tag: DiagnosticTag::None,
                    categories: FxHashSet::default(),
                    fixes: None,
                    related_info: None,
                    code: "L1308".into(),
                    code_doc_uri: None,
                },
            ],
        )]);
        let labeled_syntax_errors = FxHashMap::from_iter([(
            Some(DiagnosticLabel::MFA(Label::new_raw("foo/0"))),
            vec![Diagnostic {
                message: "syntax error before: '->'".to_string(),
                range: TextRange::new(106.into(), 108.into()),
                severity: Severity::Error,
                cli_severity: None,
                tag: DiagnosticTag::None,
                categories: FxHashSet::default(),
                fixes: None,
                related_info: None,
                code: "P1711".into(),
                code_doc_uri: None,
            }],
        )]);
        let extra_diags = LabeledDiagnostics {
            normal: vec![],
            labeled_syntax_errors,
            labeled_undefined_errors,
        };

        let config = DiagnosticsConfig::default();
        check_diagnostics_with_config_and_extra(
            config,
            &extra_diags,
            r#"
             -module(main).

             -export([foo/0,bar/0]).

             -spec bar() -> ok.
             bar() -> foo().

             -spec foo() -> ok.
             foo( -> ok. %%
             %%  ^ error: Missing ')'
            "#,
        );
    }

    #[test]
    fn group_related_diagnostics_elp_only() {
        // Demonstrate that ELP does not pick up a syntax error in the
        // spec, same code as in test_projects/diagnostics/app_a/src/syntax.erl
        check_diagnostics(
            r#"
             -module(main).

             -spec blah(Pred :: fun ((T) -> erlang:boolean), List :: [T]) -> erlang:boolean().
             blah(0, _Y) -> 1;
             blah(X, _Y) -> X + 1.
            "#,
        );
    }

    #[test]
    fn group_related_diagnostics_export_unknown() {
        check_diagnostics(
            r#"
             //- erlang_service
             //- /src/a_mod.erl app:app_a
             -module(a_mod).
             -export([foo/0]).

             foo() -> syntax error oops.
             %%              ^^^^^ error: syntax error before: error
            "#,
        );
    }

    #[test]
    fn group_related_diagnostics_wtf() {
        // Note: the cascade fails because ELP errors out to such an
        // extent we do not even see bar() as a function.
        check_diagnostics(
            r#"
             //- erlang_service
             //- native
             //- /src/a_mod.erl app:app_a
             -module(a_mod).
             -export([foo/0]).

             foo() ->
                  bar().
             %%   ^^^^^ error: function bar/0 undefined

             bar() -> !!! %% syntax error
             %%<^^^^^^^^^ error: Syntax Error
            "#,
        );
    }

    #[test]
    fn check_specific_fix_works() {
        check_specific_fix(
            "Remove match",
            r#"
             -module(main).

             baz()->
               Fo~o = 1.
             %%^^^ 💡 warning: match is redundant
             "#,
            expect![[r#"
             -module(main).

             baz()->
               1.
             "#]],
        );
    }

    #[test]
    #[should_panic]
    fn check_specific_fix_mismatch() {
        check_specific_fix(
            "mismatched diagnostic message",
            r#"
             -module(main).

             baz()->
               Fo~o = 1.
             %%^^^^^^^ 💡 warning: match is redundant
             "#,
            expect![[r#"
             -module(main).

             baz()->
               1.
             %%^^^^^^^ 💡 warning: match is redundant
             "#]],
        );
    }

    #[test]
    fn test_eqwalizer_diagnostics() {
        if otp_supported_by_eqwalizer() {
            check_diagnostics(
                r#"
            //- eqwalizer
            //- /play/src/bar1e.erl app:play
                -module(bar1e).

                -spec baz() -> ok.
                baz() -> something_else.
                %%       ^^^^^^^^^^^^^^ 💡 error: eqwalizer: incompatible_types
            "#,
            );
        }
    }

    #[test]
    fn test_nested_syntax_errors() {
        check_diagnostics(
            r#"
            -module(main).
            run() ->
                ExitCode =
                    try
                        Root = project_root(),
                        to_exit_code(run1(Root)),
                    catch
                        _:Reason -> to_exit_code(Reason)
            %%          ^^^^^^^^^^^  error: Syntax Error
                    end,
            %%      ^^^  error: Syntax Error

                erlang:halt(ExitCode).
            "#,
        );
    }

    #[test]
    fn restricted_range_for_undefined_function_diagnostic() {
        check_diagnostics(
            r#"
//- erlang_service
  -module(main).
  -export([foo/0, bar/0]).
%%                ^^^^^  error: function bar/0 undefined
  foo() -> ok.
"#,
        );
    }

    #[test]
    fn restricted_range_for_undefined_type_diagnostic() {
        check_diagnostics(
            r#"
//- erlang_service
  -module(main).
  -export_type([foo/0, bar/3]).
%%                     ^^^^^  error: type bar/3 undefined
  -type foo() :: integer().
"#,
        );
    }

    #[test]
    fn restricted_range_for_spec_for_undefined_function_diagnostic() {
        check_diagnostics(
            r#"
//- erlang_service
    -module(main).
    -export([foo/0]).
    -spec bar() -> ok.
%%        ^^^  error: spec for undefined function bar/0
    foo() -> ok.
"#,
        );
    }

    #[test]
    fn restricted_range_for_unused_type_diagnostic() {
        check_diagnostics(
            r#"
//- erlang_service
    -module(main).

    -type foo() :: ok.
%%        ^^^  warning: type foo() is unused
"#,
        );

        check_diagnostics(
            r#"
//- erlang_service
    -module(main).

    -type foo(A, B) :: {A, B}.
%%        ^^^  warning: type foo(_,_) is unused
"#,
        );
    }

    #[test]
    fn edoc_with_maybe_operator() {
        check_diagnostics(
            r#"
//- edoc
  -module(main).
  -export([listen_port/2]).
  listen_port(Port, Options) ->
    maybe
        {ok, ListenSocket} ?= inet_tcp:listen(Port, Options),
        {ok, Address} ?= inet:sockname(ListenSocket),
        {ok, {ListenSocket, Address}}
    end.
"#,
        );
    }

    // https://github.com/WhatsApp/erlang-language-platform/issues/24
    #[test]
    fn byte_index_2001_not_a_char_boundary() {
        check_diagnostics(
            r#"
    %%  öööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööö
    %% öööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööö
    %% öööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööö
    %% öööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööö
    %% öööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööö
    %% öööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööö
    %% öööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööö
    %% öööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööö
    -module(main).
    -spec main() -> ok.
    main() -> ok.
"#,
        );
    }

    // https://github.com/WhatsApp/erlang-language-platform/issues/39
    #[test]
    fn can_ignore_parse_transform_error() {
        check_diagnostics(
            r#"
            //- erlang_service
            //- /my_app/src/a_file.erl
              -module(a_file).
              % elp:ignore L0002
              -compile({parse_transform, epipe}).
              foo() -> ok.

            "#,
        );
    }

    #[test]
    fn file_not_found_doc_attribute_warning() {
        check_diagnostics(
            r#"
            //- erlang_service
            //- native
            //- /my_app/src/a_file.erl
              -module(a_file).

              -doc {file,"../../doc/src/info.md"}.
             %%          ^^^^^^^^^^^^^^^^^^^^^^^ warning: can't find doc file "../../doc/src/info.md"

            "#,
        );
    }

    #[test]
    fn file_not_found_doc_attribute_warning_in_erlang() {
        check_diagnostics(
            r#"
            //- erlang_service
            //- native
            //- /my_app/src/erlang.erl
              -module(erlang).

              -doc {file,"../../doc/src/info.md"}.
             %%          ^^^^^^^^^^^^^^^^^^^^^^^ warning: can't find doc file "../../doc/src/info.md"
            "#,
        );
    }

    #[test]
    fn erlang_service_include_resolution() {
        check_diagnostics(
            r#"
               //- erlang_service
               //- /src/main.erl
                   -module(main).
                   -export([foo/0]).
                   -include("header.hrl").

                   foo() -> bar().

               //- /src/header.hrl
                   bar() -> ok.
            "#,
        );
    }

    #[test]
    fn erlang_service_include_resolution_doc() {
        check_diagnostics(
            r#"
            //- erlang_service
            //- native
            //- /my_app/src/a_file.erl
              -module(a_file).

              -doc {file,"../doc/info.md"}.

            //- /my_app/doc/info.md
              % Doc stuff included in file

            "#,
        );
    }

    #[test]
    fn erlang_service_file_open_encoding() {
        check_diagnostics(
            // Note: \~ gets replaced by ~ in the fixture parsing
            r#"
               //- erlang_service
               //- /src/main.erl
                   -module(main).
                   -export([foo/0]).

                   foo() ->
                     \~"\"\\µA\"" = \~/"\\µA"/
                     X = 3.
                  %% ^ error: syntax error before: X
            "#,
        );
    }

    #[test]
    fn erlang_service_strict_generators() {
        check_diagnostics(
            r#"
               //- erlang_service
               //- /src/main.erl
                   -module(main).
                   -export([foo/3]).

                   foo(List, Bytes, Map) ->
                       [X || X <:- List, X >= 5],
                       << Byte || <<Byte>> <:= Bytes, Byte >= 5>>,
                       #{KK => VV || KK := VV <:- Map}.
            "#,
        );
    }

    #[test]
    fn edoc_generic_diagnostics_suppressed() {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::ErlangService("O0000".to_string()));
        if supports_eep59_doc_attributes() {
            check_diagnostics_with_config(
                config,
                r#"
                //- edoc
                //- /src/a_mod.erl app:app_a
                -module(a_mod).
                -export([foo/0]).

                % @docc
                %%<^^^^^ warning: tag @docc not recognized.
                foo() -> \~"foo".
                "#,
            );
        } else {
            // In previous versions of OTP, the EDoc stops at the first error, so no other diagnostics are reported
            check_diagnostics_with_config(
                config,
                r#"
                //- edoc
                //- /src/a_mod.erl app:app_a
                -module(a_mod).
                -export([foo/0]).

                % @docc
                foo() -> \~"foo".
                "#,
            );
        }
    }
}
