/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeSet;
use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

use elp_ide_assists::AssistId;
use elp_ide_assists::AssistKind;
use elp_ide_db::assists::Assist;
use elp_ide_db::docs::DocDatabase;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::erlang_service;
use elp_ide_db::erlang_service::DiagnosticLocation;
use elp_ide_db::erlang_service::Location;
use elp_ide_db::erlang_service::ParseError;
use elp_ide_db::erlang_service::StartLocation;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::ErlAstDatabase;
use elp_ide_db::LineCol;
use elp_ide_db::LineIndex;
use elp_ide_db::LineIndexDatabase;
use elp_syntax::algo;
use elp_syntax::ast;
use elp_syntax::ast::edit;
use elp_syntax::ast::edit::start_of_line;
use elp_syntax::ast::edit::IndentLevel;
use elp_syntax::ast::AstNode;
use elp_syntax::ast::HasLabel;
use elp_syntax::label::Label;
use elp_syntax::ted::Element;
use elp_syntax::NodeOrToken;
use elp_syntax::Parse;
use elp_syntax::SourceFile;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::db::MinDefDatabase;
use hir::InFile;
use hir::Semantic;
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use serde::de;
use serde::Deserialize;
use serde::Deserializer;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use text_edit::TextEdit;

use crate::common_test;
// @fb-only: use crate::meta_only::MetaOnlyDiagnosticCode;
use crate::RootDatabase;
use crate::SourceDatabase;

mod application_env;
mod cross_node_eval;
mod dependent_header;
mod deprecated_function;
mod effect_free_statement;
mod from_config;
mod head_mismatch;
// @fb-only: mod meta_only;
mod missing_compile_warn_missing_spec;
mod misspelled_attribute;
mod module_mismatch;
mod mutable_variable;
mod redundant_assignment;
mod replace_call;
mod trivial_match;
mod unused_function_args;
mod unused_include;
mod unused_macro;
mod unused_record_field;

pub use from_config::Lint;
pub use from_config::LintsFromConfig;
pub use from_config::ReplaceCall;
pub use from_config::ReplaceCallAction;
pub use replace_call::Replacement;

#[derive(Debug, Clone)]
// For the doc please refer to
// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/
pub struct Diagnostic {
    pub message: String,
    pub range: TextRange,
    pub severity: Severity,
    pub categories: HashSet<Category>,
    pub fixes: Option<Vec<Assist>>,
    pub related_info: Option<Vec<RelatedInformation>>,
    pub code: DiagnosticCode,
    pub uri: Option<String>,
}

// @fb-only: pub const BASE_URL: &str = crate::meta_only::BASE_URL;
pub const BASE_URL: &str = "https://whatsapp.github.io/erlang-language-platform/docs"; // @oss-only

impl Diagnostic {
    pub(crate) fn new(
        code: DiagnosticCode,
        message: impl Into<String>,
        range: TextRange,
    ) -> Diagnostic {
        let message = message.into();
        Diagnostic {
            code: code.clone(),
            message,
            range,
            severity: Severity::Error,
            categories: HashSet::new(),
            fixes: None,
            related_info: None,
            uri: code.as_uri(),
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

    pub(crate) fn with_uri(mut self, uri: Option<String>) -> Diagnostic {
        self.uri = uri;
        self
    }

    pub(crate) fn with_fixes(mut self, fixes: Option<Vec<Assist>>) -> Diagnostic {
        self.fixes = fixes;
        self
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

    pub(crate) fn should_be_ignored(&self, line_index: &LineIndex, source: &SyntaxNode) -> bool {
        match prev_line_comment_text(line_index, source, self.range.start()) {
            Some(comment) => comment_contains_ignore_code(&comment, &self.code),
            None => false,
        }
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
            let text = format!("\n{}% elp:ignore {}", indent, self.code.as_labeled_code(),);

            let offset = start_of_line(&token);
            builder.insert(offset, text);
            let edit = builder.finish();
            let source_change = SourceChange::from_text_edit(file_id, edit);
            let ignore_fix = Assist {
                id: AssistId("ignore_problem", AssistKind::QuickFix),
                label: Label::new("Ignore problem"),
                group: None,
                target: self.range,
                source_change: Some(source_change),
                user_input: None,
            };
            match &mut self.fixes {
                Some(fixes) => fixes.push(ignore_fix),
                None => self.fixes = Some(vec![ignore_fix]),
            };
        }
        self
    }

    pub fn print(&self, line_index: &LineIndex) -> String {
        let start = line_index.line_col(self.range.start());
        let end = line_index.line_col(self.range.end());
        format!(
            "{}:{}-{}:{}::[{:?}] [{}] {}",
            start.line,
            start.col_utf16,
            end.line,
            end.col_utf16,
            self.severity,
            self.code,
            self.message
        )
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Ignore {
    pub codes: Vec<DiagnosticCode>,
    pub suppression_range: TextRange,
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

#[derive(Debug, Copy, Clone)]
pub enum Severity {
    Error,
    Warning,
    // `WeakWarning` maps onto a Notice warning when used in the LSP
    // environment, and in VS Code this means it does not show up in
    // the problems pane, has an unobtrusive underline, but does show
    // up on hover if the cursor is placed on it.
    WeakWarning,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Category {
    Experimental,

    // During codemods, this diagnostic should be used automatically as rewrite-rule
    // to clean-up the code
    SimplificationRule,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, EnumIter)]
// pub struct DiagnosticCode(pub String);
pub enum DiagnosticCode {
    DefaultCodeForEnumIter,
    HeadMismatch,
    MissingModule,
    ModuleMismatch,
    UnusedInclude,
    BoundVarInPattern,
    UnusedMacro,
    UnusedRecordField,
    MutableVarBug,
    SyntaxError,
    Missing(String),
    StatementHasNoEffect,
    TrivialMatch,
    UnusedFunctionArg,
    RedundantAssignment,
    UnreachableTest,
    ApplicationGetEnv,
    MissingCompileWarnMissingSpec,
    MisspelledAttribute,
    CrossNodeEval,
    DependentHeader,
    DeprecatedFunction,

    // Wrapper for erlang service diagnostic codes
    ErlangService(String),
    // Used for ad-hoc diagnostics via lints/codemods
    AdHoc(String),
    // @fb-only: MetaOnly(MetaOnlyDiagnosticCode),
}

impl<'de> Deserialize<'de> for DiagnosticCode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        FromStr::from_str(&s).map_err(de::Error::custom)
    }
}

impl serde::Serialize for DiagnosticCode {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_code().as_str())
    }
}

impl Default for DiagnosticCode {
    fn default() -> Self {
        DiagnosticCode::DefaultCodeForEnumIter
    }
}

impl DiagnosticCode {
    pub fn as_code(&self) -> String {
        match self {
            DiagnosticCode::DefaultCodeForEnumIter => "DEFAULT-UNUSED-CONSTRUCTOR".to_string(),
            DiagnosticCode::MissingModule => "L1201".to_string(),
            DiagnosticCode::UnusedInclude => "L1500".to_string(), // Unused file
            DiagnosticCode::HeadMismatch => "P1700".to_string(),  // "head-mismatch"
            DiagnosticCode::SyntaxError => "P1711".to_string(),
            DiagnosticCode::BoundVarInPattern => "W0000".to_string(),
            DiagnosticCode::ModuleMismatch => "W0001".to_string(), // "module-mismatch"
            DiagnosticCode::UnusedMacro => "W0002".to_string(),    // "unused-macro"
            DiagnosticCode::UnusedRecordField => "W0003".to_string(), // unused-record-field
            DiagnosticCode::Missing(_) => "W0004".to_string(), // epp had missing_comma and missing_parenthesis
            DiagnosticCode::MutableVarBug => "W0005".to_string(), // mutable-variable
            DiagnosticCode::StatementHasNoEffect => "W0006".to_string(), // statement-has-no-effect
            DiagnosticCode::TrivialMatch => "W0007".to_string(), // trivial-match
            DiagnosticCode::UnreachableTest => "W0008".to_string(),
            DiagnosticCode::RedundantAssignment => "W0009".to_string(), // redundant-assignment
            DiagnosticCode::UnusedFunctionArg => "W0010".to_string(),   // unused-function-arg
            DiagnosticCode::ApplicationGetEnv => "W0011".to_string(),   // application_get_env
            DiagnosticCode::MissingCompileWarnMissingSpec => "W0012".to_string(),
            DiagnosticCode::MisspelledAttribute => "W0013".to_string(), // misspelled-attribute
            DiagnosticCode::CrossNodeEval => "W0014".to_string(),       // cross-node-eval
            DiagnosticCode::DependentHeader => "W0015".to_string(),     // dependent-header
            DiagnosticCode::DeprecatedFunction => "W0016".to_string(),  // deprecated-function
            DiagnosticCode::ErlangService(c) => c.to_string(),
            DiagnosticCode::AdHoc(c) => format!("ad-hoc: {c}").to_string(),
            // @fb-only: DiagnosticCode::MetaOnly(c) => c.as_code(),
        }
    }

    pub fn as_label(&self) -> String {
        match self {
            DiagnosticCode::DefaultCodeForEnumIter => "DEFAULT-UNUSED-CONSTRUCTOR".to_string(),
            DiagnosticCode::MissingModule => "missing_module".to_string(),
            DiagnosticCode::UnusedInclude => "unused_include".to_string(),
            DiagnosticCode::HeadMismatch => "head_mismatch".to_string(),
            DiagnosticCode::SyntaxError => "syntax_error".to_string(),
            DiagnosticCode::BoundVarInPattern => "bound_var_in_pattern".to_string(),
            DiagnosticCode::ModuleMismatch => "module_mismatch".to_string(),
            DiagnosticCode::UnusedMacro => "unused_macro".to_string(),
            DiagnosticCode::UnusedRecordField => "unused_record_field".to_string(),
            DiagnosticCode::Missing(_) => "missing_comma_or_parenthesis".to_string(),
            DiagnosticCode::MutableVarBug => "mutable_variable_bug".to_string(),
            DiagnosticCode::StatementHasNoEffect => "statement_has_no_effect".to_string(),
            DiagnosticCode::TrivialMatch => "trivial_match".to_string(),
            DiagnosticCode::UnusedFunctionArg => "unused_function_arg".to_string(),
            DiagnosticCode::RedundantAssignment => "redundant_assignment".to_string(),
            DiagnosticCode::UnreachableTest => "unreachable_test".to_string(),
            DiagnosticCode::MissingCompileWarnMissingSpec => {
                // Match the name in the original
                "compile-warn-missing-spec".to_string()
            }
            DiagnosticCode::ApplicationGetEnv => "application_get_env".to_string(),
            DiagnosticCode::MisspelledAttribute => "misspelled_attribute".to_string(),
            DiagnosticCode::CrossNodeEval => "cross_node_eval".to_string(),
            DiagnosticCode::DependentHeader => "dependent_header".to_string(),
            DiagnosticCode::DeprecatedFunction => "deprecated_function".to_string(),
            DiagnosticCode::ErlangService(c) => c.to_string(),
            DiagnosticCode::AdHoc(c) => format!("ad-hoc: {c}").to_string(),
            // @fb-only: DiagnosticCode::MetaOnly(c) => c.as_label(),
        }
    }

    pub fn as_labeled_code(&self) -> String {
        format!("{} ({})", self.as_code(), self.as_label())
    }

    pub fn maybe_from_string(s: &String) -> Option<DiagnosticCode> {
        if let Some(r) = DIAGNOSTIC_CODE_LOOKUPS.get(s) {
            Some(r.clone())
        } else {
            // Look for ErlangService and AdHoc
            if let Some(code) = Self::is_adhoc(s) {
                Some(DiagnosticCode::AdHoc(code))
            } else {
                Self::is_erlang_service(s).map(DiagnosticCode::ErlangService)
            }
        }
    }

    pub fn namespace(code: &String) -> Option<String> {
        let first = code.to_string().chars().next()?;
        Some(first.to_lowercase().to_string())
    }

    pub fn as_namespace(&self) -> Option<String> {
        match self {
            DiagnosticCode::DefaultCodeForEnumIter => None,
            DiagnosticCode::AdHoc(_) => None,
            // @fb-only: DiagnosticCode::MetaOnly(_) => None,
            DiagnosticCode::ErlangService(code) => Self::namespace(code),
            _ => Self::namespace(&self.as_code()),
        }
    }

    pub fn as_uri(&self) -> Option<String> {
        let namespace = self.as_namespace()?;
        let code = self.as_code();
        Some(format!(
            "{}/erlang-error-index/{namespace}/{code}",
            BASE_URL.to_string()
        ))
    }

    /// Check if the diagnostic label is for an AdHoc one.
    fn is_adhoc(s: &str) -> Option<String> {
        // Looking for something like "ad-hoc: ad-hoc-title-1"
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^ad-hoc: ([^\s]+)$").unwrap();
        }
        RE.captures_iter(s).next().map(|c| c[1].to_string())
    }

    /// Check if the diagnostic label is for an ErlangService one.
    fn is_erlang_service(s: &str) -> Option<String> {
        // Looing for something like "L0008"
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^([A-Z]+[0-9]{4})$").unwrap();
        }
        RE.captures_iter(s).next().map(|c| c[1].to_string())
    }
}

lazy_static! {
    static ref DIAGNOSTIC_CODE_LOOKUPS: FxHashMap<String, DiagnosticCode> = {
        let mut res = FxHashMap::default();
        for code in DiagnosticCode::iter() {
            res.insert(code.as_code(), code.clone());
            res.insert(code.as_label(), code.clone());
        }
        res
    };
}

impl FromStr for DiagnosticCode {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(code) = DiagnosticCode::maybe_from_string(&s.to_string()) {
            Ok(code)
        } else {
            Err(format!("Unknown DiagnosticCode: '{s}'"))
        }
    }
}
impl From<&str> for DiagnosticCode {
    fn from(str: &str) -> Self {
        match DiagnosticCode::from_str(str) {
            Ok(c) => c,
            Err(err) => panic!("{err}"),
        }
    }
}

impl fmt::Display for DiagnosticCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_code())
    }
}

pub trait AdhocSemanticDiagnostics:
    Fn(&mut Vec<Diagnostic>, &Semantic, FileId, FileKind) + std::panic::RefUnwindSafe + Sync
{
}
impl<F> AdhocSemanticDiagnostics for F where
    F: Fn(&mut Vec<Diagnostic>, &Semantic, FileId, FileKind) + std::panic::RefUnwindSafe + Sync
{
}

#[derive(Default, Clone)]
pub struct DiagnosticsConfig<'a> {
    pub disable_experimental: bool,
    pub disabled: FxHashSet<DiagnosticCode>,
    pub adhoc_semantic_diagnostics: Vec<&'a dyn AdhocSemanticDiagnostics>,
    pub lints_from_config: LintsFromConfig,
}

impl<'a> DiagnosticsConfig<'a> {
    pub fn new(
        disable_experimental: bool,
        disabled: FxHashSet<DiagnosticCode>,
        adhoc_semantic_diagnostics: Vec<&'a dyn AdhocSemanticDiagnostics>,
    ) -> DiagnosticsConfig<'a> {
        DiagnosticsConfig {
            disable_experimental,
            disabled,
            adhoc_semantic_diagnostics,
            lints_from_config: LintsFromConfig::default(),
        }
    }

    pub fn disable(mut self, code: DiagnosticCode) -> DiagnosticsConfig<'a> {
        self.disabled.insert(code);
        self
    }

    pub fn from_config(mut self, lints_from_config: &LintsFromConfig) -> DiagnosticsConfig<'a> {
        self.lints_from_config = lints_from_config.clone();
        self
    }
}

pub type Labeled<T> = FxHashMap<Option<Label>, Vec<T>>;

#[derive(Debug, Default, Clone)]
pub struct LabeledDiagnostics<D> {
    pub normal: Vec<D>,
    pub labeled: Labeled<D>,
}

impl<D> LabeledDiagnostics<D> {
    pub fn default() -> LabeledDiagnostics<D> {
        LabeledDiagnostics {
            normal: vec![],
            labeled: FxHashMap::default(),
        }
    }

    pub fn new(diagnostics: Vec<D>) -> LabeledDiagnostics<D> {
        LabeledDiagnostics {
            normal: diagnostics,
            labeled: FxHashMap::default(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &D> + '_ {
        self.normal.iter().chain(self.labeled.values().flatten())
    }

    pub fn is_empty(&self) -> bool {
        self.normal.is_empty() && self.labeled.is_empty()
    }

    pub fn len(&self) -> usize {
        self.normal.len() + self.labeled.len()
    }

    pub fn convert<T>(&self, convert: &dyn Fn(&D) -> T) -> LabeledDiagnostics<T> {
        LabeledDiagnostics {
            normal: self.normal.iter().map(convert).collect_vec(),
            labeled: FxHashMap::from_iter(
                self.labeled
                    .iter()
                    .map(|(k, v)| (k.clone(), v.iter().map(convert).collect_vec())),
            ),
        }
    }
}

pub fn diagnostics(
    db: &RootDatabase,
    config: &DiagnosticsConfig,
    file_id: FileId,
    include_generated: bool,
) -> LabeledDiagnostics<Diagnostic> {
    lazy_static! {
        static ref EXTENSIONS: Vec<FileKind> = vec![FileKind::Module, FileKind::Header];
    };
    let parse = db.parse(file_id);
    let root_id = db.file_source_root(file_id);
    let root = db.source_root(root_id);
    let path = root.path_for_file(&file_id).unwrap();

    let file_kind = db.file_kind(file_id);
    let report_diagnostics = EXTENSIONS.iter().any(|it| it == &file_kind);

    let mut res = Vec::new();

    let syntax_errors_by_function = if report_diagnostics {
        let is_erl_module = file_kind == FileKind::Module;
        let sema = Semantic::new(db);

        if is_erl_module {
            no_module_definition_diagnostic(&mut res, &parse);
            if include_generated || !db.is_generated(file_id) {
                unused_include::unused_includes(&sema, db, &mut res, file_id);
            }
            let is_test_suite = match path.name_and_extension() {
                Some((name, _)) => name.ends_with("_SUITE"),
                _ => false,
            };
            if is_test_suite {
                common_test::unreachable_test(&mut res, &sema, file_id)
            }
        }

        res.append(&mut form_missing_separator_diagnostics(&parse));

        config
            .adhoc_semantic_diagnostics
            .iter()
            .for_each(|f| f(&mut res, &sema, file_id, file_kind));
        semantic_diagnostics(
            &mut res,
            &sema,
            file_id,
            file_kind,
            config.disable_experimental,
        );
        syntax_diagnostics(db, &parse, &mut res, file_id);

        let parse_diagnostics = parse.errors().iter().take(128).map(|err| {
            Diagnostic::error(
                DiagnosticCode::SyntaxError,
                widen_range(err.range()),
                format!("Syntax Error: {}", err),
            )
        });
        let source_file = db.parse(file_id).tree();
        group_syntax_errors(&source_file, parse_diagnostics)
    } else {
        FxHashMap::default()
    };
    let line_index = db.file_line_index(file_id);
    // TODO: can we  ever disable DiagnosticCode::SyntaxError?
    //       In which case we must check syntax_errors_by_function
    res.retain(|d| {
        !config.disabled.contains(&d.code)
            && !(config.disable_experimental && d.has_category(Category::Experimental))
            && !d.should_be_ignored(&line_index, &parse.syntax_node())
    });

    LabeledDiagnostics {
        normal: res,
        labeled: syntax_errors_by_function,
    }
}

fn group_syntax_errors(
    source_file: &SourceFile,
    diagnostics: impl Iterator<Item = Diagnostic>,
) -> Labeled<Diagnostic> {
    let mut map: Labeled<Diagnostic> = FxHashMap::default();
    diagnostics.for_each(|d| {
        map.entry(function_label(&d, source_file))
            .or_default()
            .push(d);
    });
    map
}

fn function_label(d: &Diagnostic, source_file: &SourceFile) -> Option<Label> {
    if d.code == DiagnosticCode::SyntaxError {
        if let Some(syntax) = source_file
            .syntax()
            .token_at_offset(d.range.start())
            .right_biased()
            .and_then(|t| t.parent())
        {
            let fun_decl = syntax.ancestors().find_map(ast::FunDecl::cast)?;
            let fun_label = fun_decl.label()?;
            return Some(fun_label);
        };
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

pub fn semantic_diagnostics(
    res: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
    file_kind: FileKind,
    disable_experimental: bool,
) {
    // TODO: disable this check when T151727890 and T151605845 are resolved
    if !disable_experimental {
        unused_function_args::unused_function_args(res, sema, file_id);
        redundant_assignment::redundant_assignment(res, sema, file_id);
        trivial_match::trivial_match(res, sema, file_id);
    }
    unused_macro::unused_macro(res, sema, file_id, file_kind);
    unused_record_field::unused_record_field(res, sema, file_id, file_kind);
    mutable_variable::mutable_variable_bug(res, sema, file_id);
    effect_free_statement::effect_free_statement(res, sema, file_id);
    application_env::application_env(res, sema, file_id);
    // @fb-only: meta_only::diagnostics(res, sema, file_id);
    missing_compile_warn_missing_spec::missing_compile_warn_missing_spec(res, sema, file_id);
    cross_node_eval::cross_node_eval(res, sema, file_id);
    dependent_header::dependent_header(res, sema, file_id, file_kind);
    deprecated_function::deprecated_function(res, sema, file_id);
}

pub fn syntax_diagnostics(
    db: &RootDatabase,
    parse: &Parse<ast::SourceFile>,
    res: &mut Vec<Diagnostic>,
    file_id: FileId,
) {
    misspelled_attribute::misspelled_attribute(res, db, file_id);
    for node in parse.tree().syntax().descendants() {
        head_mismatch::head_mismatch(res, file_id, &node);
        module_mismatch::module_mismatch(res, db, file_id, &node);
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
            ast::Form::FunDecl(f) => {
                check_missing_sep(f.clauses(), SyntaxKind::ANON_SEMI, ";", "missing_semi")
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

fn comment_contains_ignore_code(comment: &str, code: &DiagnosticCode) -> bool {
    let pattern = "% elp:ignore";
    match comment.find(pattern) {
        Some(start) => {
            let comment = comment[start..].to_string();
            comment
                .split_whitespace()
                .any(|code_str| match DiagnosticCode::from_str(code_str) {
                    Ok(code_comment) => *code == code_comment,
                    Err(_) => false,
                })
        }
        _ => false,
    }
}

fn prev_line(line_index: &LineIndex, current_line: u32) -> Option<TextSize> {
    match current_line {
        0 => None,
        _ => line_index.line_at(current_line as usize - 1),
    }
}

fn prev_line_comment_text(
    line_index: &LineIndex,
    source: &SyntaxNode,
    offset: TextSize,
) -> Option<String> {
    let current_line = line_index.line_col(offset).line;
    let prev_line = prev_line(line_index, current_line)?;
    // Temporary for T153426323
    let _pctx = stdx::panic_context::enter("\nprev_line_comment_text".to_string());
    let token = source.token_at_offset(prev_line).left_biased()?;
    let prev_line_range = TextRange::new(prev_line, offset);
    let node_or_token = token
        .siblings_with_tokens(elp_syntax::Direction::Next)
        .filter(|node| prev_line_range.contains_range(node.text_range()))
        .find(|node| node.kind() == SyntaxKind::COMMENT)?;
    Some(node_or_token.to_string())
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

fn make_missing_diagnostic(range: TextRange, item: &'static str, code: String) -> Diagnostic {
    let message = format!("Missing '{}'", item);
    Diagnostic::new(DiagnosticCode::Missing(code), message, range).with_severity(Severity::Warning)
}

pub fn erlang_service_diagnostics(
    db: &RootDatabase,
    file_id: FileId,
) -> Vec<(FileId, LabeledDiagnostics<Diagnostic>)> {
    // Use the same format as eqwalizer, so we can re-use the salsa cache entry
    let format = erlang_service::Format::OffsetEtf;

    let res = db.module_ast(file_id, format);

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
            // Temporary for T148094436
            let _pctx = stdx::panic_context::enter("\nerlang_service_diagnostics:1".to_string());
            (
                file_id,
                Diagnostic::new(
                    DiagnosticCode::ErlangService(code),
                    msg,
                    TextRange::new(start, end),
                )
                .with_severity(Severity::Error),
            )
        })
        .chain(
            warning_info
                .into_iter()
                .map(|(file_id, start, end, code, msg)| {
                    // Temporary for T148094436
                    let _pctx =
                        stdx::panic_context::enter("\nerlang_service_diagnostics:2".to_string());
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

    // Remove diagnostics already reported by ELP
    let file_kind = db.file_kind(file_id);
    let diags: Vec<(FileId, Diagnostic)> = diags
        .into_iter()
        .filter(|(_, d)| !is_implemented_in_elp(&d.code, file_kind))
        .collect();
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
        diags_map
            .into_iter()
            .map(|(file_id, ds)| (file_id, ds))
            .collect()
    };
    label_erlang_service_diagnostics(diags)
}

/// We label erlang service diagnostics with any references to
/// undefined function or undefined spec.  This is so we can tie them
/// up to ELP native parse errors occurring in a function that has the
/// same label.
fn label_erlang_service_diagnostics(
    diagnostics: Vec<(FileId, Vec<Diagnostic>)>,
) -> Vec<(FileId, LabeledDiagnostics<Diagnostic>)> {
    diagnostics
        .into_iter()
        .map(|(file_id, ds)| {
            let mut labeled: Labeled<Diagnostic> = FxHashMap::default();
            ds.into_iter().for_each(|d| {
                labeled.entry(erlang_service_label(&d)).or_default().push(d);
            });
            (
                file_id.clone(),
                LabeledDiagnostics {
                    normal: Vec::default(),
                    labeled,
                },
            )
        })
        .collect_vec()
}

pub fn edoc_diagnostics(db: &RootDatabase, file_id: FileId) -> Vec<(FileId, Vec<Diagnostic>)> {
    // We use a BTreeSet of a tuple because neither ParseError nor
    // Diagnostic nor TextRange has an Ord instance
    let mut error_info: BTreeSet<(FileId, TextSize, TextSize, String, String)> =
        BTreeSet::default();
    let mut warning_info: BTreeSet<(FileId, TextSize, TextSize, String, String)> =
        BTreeSet::default();

    // If the file cannot be parsed, it does not really make sense to run EDoc,
    // so let's return early.
    // Use the same format as eqwalizer, so we can re-use the salsa cache entry.
    let format = erlang_service::Format::OffsetEtf;
    let ast = db.module_ast(file_id, format);
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

    let diags: Vec<(FileId, Diagnostic)> = error_info
        .into_iter()
        .map(|(file_id, start, end, code, msg)| {
            // Temporary for T148094436
            let _pctx = stdx::panic_context::enter("\nedoc_diagnostics:1".to_string());
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
                    // Temporary for T148094436
                    let _pctx = stdx::panic_context::enter("\nedoc_diagnostics:2".to_string());
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
        diags.into_iter().for_each(|(file_id, diag)| {
            diags_map
                .entry(file_id)
                .and_modify(|existing| existing.push(diag.clone()))
                .or_insert(vec![diag.clone()]);
        });
        diags_map
            .into_iter()
            .map(|(file_id, ds)| (file_id, ds))
            .collect()
    }
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
                "P1711" => true, // Syntax error
                _ => false,
            },
            _ => false,
        }
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
        }) => included_file_file_id(db, file_id, Location::TextRange(directive_location)).map(
            |included_file_id| {
                (
                    included_file_id,
                    error_location.start(),
                    error_location.end(),
                    parse_error.code.clone(),
                    parse_error.msg.clone(),
                )
            },
        ),
        Some(DiagnosticLocation::Normal(Location::TextRange(range))) => {
            let default_range = (
                file_id,
                range.start(),
                range.end(),
                parse_error.code.clone(),
                parse_error.msg.clone(),
            );
            match parse_error.code.as_str() {
                // For certain warnings, OTP returns a diagnostic for the entire definition of a function or record.
                // That can be very verbose and distracting, so we try restricting the range to the function/record name only.
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
        Some(DiagnosticLocation::Normal(Location::StartLocation(StartLocation {
            line: _,
            column: _,
        }))) => {
            log::error!(
                "Expecting TextRange, erlang_service provided Location: {:?}",
                parse_error.location
            );
            Some((
                file_id,
                TextSize::default(),
                TextSize::default(),
                parse_error.code.clone(),
                parse_error.msg.clone(),
            ))
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

/// For an error in an included file, find the include directive, work
/// out what include file it refers to, get its FileId
pub fn included_file_file_id(
    db: &RootDatabase,
    file_id: FileId,
    directive_location: Location,
) -> Option<FileId> {
    let line_index = db.file_line_index(file_id);

    let directive_range = location_range(directive_location, &line_index);
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

fn location_range(location: Location, line_index: &LineIndex) -> TextRange {
    match location {
        Location::TextRange(range) => range,
        Location::StartLocation(StartLocation { line, column }) => {
            let line_col = LineCol {
                line,
                col_utf16: column,
            };
            // Temporary for T147609435
            let _pctx = stdx::panic_context::enter("\ndiagnostics::location_range".to_string());
            let pos = line_index.offset(line_col);
            TextRange::new(pos, pos)
        }
    }
}

/// Combine the ELP and erlang_service diagnostics.  In particular,
/// flatten any cascading diagnostics if possible.
pub fn attach_related_diagnostics(
    native: LabeledDiagnostics<Diagnostic>,
    erlang_service: &LabeledDiagnostics<Diagnostic>,
) -> Vec<Diagnostic> {
    // `native` is labelled with the MFA of functions having syntax
    // errors in them. `erlang_service` is labelled with the MFA of
    // undefined functions.  For each labeled one in `native`, add the
    // corresponding ones from `erlang_service`, remember the label,
    // and when done delete all `erlang_service` diagnostics with that
    // label.

    let mut to_remove: FxHashSet<&Option<Label>> = FxHashSet::default();
    let updated = native
        .labeled
        .iter()
        .flat_map(|(label, diags)| {
            if let Some(related) = erlang_service.labeled.get(label) {
                to_remove.insert(label);
                let related_info = related.iter().map(|d| d.as_related()).collect_vec();
                let updated = diags
                    .iter()
                    .map(|d| d.clone().with_related(Some(related_info.clone())))
                    .collect_vec();
                updated
            } else {
                diags.to_vec()
            }
        })
        .collect_vec();

    let mut es = erlang_service.labeled.clone();
    es.retain(|k, _v| !to_remove.contains(k));

    native
        .normal
        .into_iter()
        .chain(updated)
        .chain(erlang_service.normal.iter().cloned())
        .chain(es.values().flatten().cloned())
        .collect_vec()
}

fn erlang_service_label(diagnostic: &Diagnostic) -> Option<Label> {
    match &diagnostic.code {
        DiagnosticCode::ErlangService(s) => match s.as_str() {
            "L1227" => {
                function_undefined_from_message(&diagnostic.message).map(|l| Label::new_raw(l))
            }
            "L1308" => spec_for_undefined_function_from_message(&diagnostic.message)
                .map(|l| Label::new_raw(l)),
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
    use elp_syntax::ast;
    use expect_test::expect;

    use super::*;
    use crate::codemod_helpers::FunctionMatch;
    use crate::codemod_helpers::MFA;
    use crate::tests::check_diagnostics;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_diagnostics_with_config_and_extra;
    use crate::tests::check_specific_fix;

    #[test]
    fn fun_decl_missing_semi_no_warning() {
        let text = concat!("foo(2)->3.");

        let parsed = ast::SourceFile::parse_text(text);
        let d = form_missing_separator_diagnostics(&parsed);
        assert_eq!(format!("{:?}", d), "[]")
    }

    #[test]
    fn fun_decl_missing_semi_no_warning_2() {
        let text = concat!("foo(1)->2;\n", "foo(2)->3.");

        let parsed = ast::SourceFile::parse_text(text);
        let d = form_missing_separator_diagnostics(&parsed);
        assert_eq!(format!("{:?}", d), "[]")
    }

    #[test]
    fn fun_decl_missing_semi_1() {
        check_diagnostics(
            r#"
   -module(main).
   foo(1)->2
        %% ^ warning: Missing ';'
   foo(2)->3.
"#,
        );
    }

    #[test]
    fn fun_decl_missing_semi_2() {
        check_diagnostics(
            r#"
   -module(main).
   foo(1)->2;
   foo(2)->3
        %% ^ warning: Missing ';'
   foo(3)->4.
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
        let diags = vec![diag1, diag2, diag3, diagk.clone()];
        assert_eq!(
            diags
                .into_iter()
                .filter(|d| !is_implemented_in_elp(&d, FileKind::Module))
                .collect::<Vec<_>>(),
            vec![diagk]
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
                .filter(|d| !is_implemented_in_elp(&d, FileKind::Escript))
                .collect::<Vec<_>>(),
            vec![diag1, diag2, diag3, diagk]
        );
    }

    #[test]
    fn filter_experimental() {
        let mut config = DiagnosticsConfig {
            disable_experimental: false,
            disabled: FxHashSet::default(),
            adhoc_semantic_diagnostics: vec![&|acc, sema, file_id, _ext| {
                replace_call::replace_call_site(
                    &FunctionMatch::MFA(MFA {
                        module: "foo".into(),
                        name: "bar".into(),
                        arity: 0,
                    }),
                    replace_call::Replacement::UseOk,
                    &replace_call::adhoc_diagnostic,
                    acc,
                    sema,
                    file_id,
                )
            }],
            lints_from_config: LintsFromConfig::default(),
        };
        config
            .disabled
            .insert(DiagnosticCode::MissingCompileWarnMissingSpec);
        check_diagnostics_with_config(
            DiagnosticsConfig {
                disable_experimental: false,
                ..config.clone()
            },
            r#"
            //- /src/main.erl
            -module(main).

            do_foo() ->
              X = foo:bar(),
            %%    ^^^^^^^^^ 💡 weak: 'foo:bar/0' called
              X.
            //- /src/foo.erl
            -module(foo).
            bar() -> ok.
            "#,
        );
        check_diagnostics_with_config(
            DiagnosticsConfig {
                disable_experimental: true,
                ..config.clone()
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
    fn from_string_1() {
        let strings = vec!["W0008", "unreachable_test"];
        let codes = strings
            .iter()
            .map(|s| DiagnosticCode::maybe_from_string(&s.to_string()))
            .collect::<Vec<_>>();
        expect![[r#"
            [
                Some(
                    UnreachableTest,
                ),
                Some(
                    UnreachableTest,
                ),
            ]
        "#]]
        .assert_debug_eq(&codes);
    }

    #[test]
    fn from_string_2() {
        let strings = vec![
            DiagnosticCode::AdHoc("ad-hoc-title-1".to_string()).as_label(),
            DiagnosticCode::AdHoc("ad-hoc-title-2".to_string()).as_code(),
        ];
        let codes = strings
            .iter()
            .map(|s| DiagnosticCode::maybe_from_string(&s.to_string()))
            .collect::<Vec<_>>();
        expect![[r#"
            [
                Some(
                    AdHoc(
                        "ad-hoc-title-1",
                    ),
                ),
                Some(
                    AdHoc(
                        "ad-hoc-title-2",
                    ),
                ),
            ]
        "#]]
        .assert_debug_eq(&codes);
    }

    #[test]
    fn from_string_3() {
        let strings = vec!["C1000", "L1213"];
        let codes = strings
            .iter()
            .map(|s| DiagnosticCode::maybe_from_string(&s.to_string()))
            .collect::<Vec<_>>();
        expect![[r#"
            [
                Some(
                    ErlangService(
                        "C1000",
                    ),
                ),
                Some(
                    ErlangService(
                        "L1213",
                    ),
                ),
            ]
        "#]]
        .assert_debug_eq(&codes);
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
             %%^^^^^^^ 💡 warning: match is redundant
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
             %%^^^^^^^ 💡 warning: match is redundant
               % elp:ignore W0007

               Bar = 2,
             %%^^^^^^^ 💡 warning: match is redundant
               ok.
             "#,
        );
    }

    #[test]
    fn group_related_diagnostics() {
        let labeled = FxHashMap::from_iter([(
            Some(Label::new_raw("foo/0")),
            vec![
                Diagnostic {
                    message: "function foo/0 undefined".to_string(),
                    range: TextRange::new(21.into(), 43.into()),
                    severity: Severity::Error,
                    categories: HashSet::default(),
                    fixes: None,
                    related_info: None,
                    code: "L1227".into(),
                    uri: None,
                },
                Diagnostic {
                    message: "function foo/0 undefined".to_string(),
                    range: TextRange::new(74.into(), 79.into()),
                    severity: Severity::Error,
                    categories: HashSet::default(),
                    fixes: None,
                    related_info: None,
                    code: "L1227".into(),
                    uri: None,
                },
                Diagnostic {
                    message: "spec for undefined function foo/0".to_string(),
                    range: TextRange::new(82.into(), 99.into()),
                    severity: Severity::Error,
                    categories: HashSet::default(),
                    fixes: None,
                    related_info: None,
                    code: "L1308".into(),
                    uri: None,
                },
            ],
        )]);
        let extra_diags = LabeledDiagnostics {
            normal: vec![Diagnostic {
                message: "syntax error before: '->'".to_string(),
                range: TextRange::new(106.into(), 108.into()),
                severity: Severity::Error,
                categories: HashSet::default(),
                fixes: None,
                related_info: None,
                code: "P1711".into(),
                uri: None,
            }],
            labeled,
        };

        let config =
            DiagnosticsConfig::default().disable(DiagnosticCode::MissingCompileWarnMissingSpec);
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
             %%  ^ error: Syntax Error: Missing )
             %%        ^^ error: syntax error before: '->'
            "#,
        );
    }

    #[test]
    fn serde_serialize_diagnostic_code() {
        assert_eq!(
            toml::to_string::<DiagnosticCode>(&DiagnosticCode::CrossNodeEval),
            Ok("\"W0014\"".to_string())
        );
    }

    #[derive(Deserialize, Debug)]
    struct Config {
        #[allow(dead_code)]
        enabled: DiagnosticCode,
    }

    #[test]
    fn serde_deserialize_diagnostic_code_1() {
        let config: Config = toml::from_str(r#"enabled = 'W0014'"#).unwrap();

        expect![[r#"
            Config {
                enabled: CrossNodeEval,
            }
        "#]]
        .assert_debug_eq(&config);
    }

    #[test]
    fn serde_deserialize_diagnostic_code_2() {
        let config: Config = toml::from_str(r#"enabled = 'cross_node_eval'"#).unwrap();

        expect![[r#"
            Config {
                enabled: CrossNodeEval,
            }
        "#]]
        .assert_debug_eq(&config);
    }

    #[test]
    fn check_specific_fix_works() {
        check_specific_fix(
            "match is redundant",
            r#"
             -module(main).

             baz()->
               Fo~o = 1.
             %%^^^^^^^ 💡 warning: match is redundant
             "#,
            r#"
             -module(main).

             baz()->
               1.
             %%^^^^^^^ 💡 warning: match is redundant
             "#,
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
            r#"
             -module(main).

             baz()->
               1.
             %%^^^^^^^ 💡 warning: match is redundant
             "#,
        );
    }
}
