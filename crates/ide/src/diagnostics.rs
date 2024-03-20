/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeSet;
use std::fmt;
use std::ops::RangeInclusive;
use std::sync::Arc;

use elp_eqwalizer::EqwalizerDiagnostic;
use elp_ide_assists::AssistId;
use elp_ide_assists::AssistKind;
use elp_ide_assists::GroupLabel;
use elp_ide_db::assists::Assist;
use elp_ide_db::common_test::CommonTestDatabase;
use elp_ide_db::common_test::CommonTestInfo;
use elp_ide_db::docs::DocDatabase;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::ProjectId;
use elp_ide_db::erlang_service;
use elp_ide_db::erlang_service::DiagnosticLocation;
use elp_ide_db::erlang_service::ParseError;
use elp_ide_db::metadata::Metadata;
use elp_ide_db::metadata::Source;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::EqwalizerDatabase;
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
use elp_types_db::TypedSemantic;
use erlang_service::CompileOption;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::db::DefDatabase;
use hir::InFile;
use hir::Semantic;
use itertools::Itertools;
use lazy_static::lazy_static;
use range_set::RangeSet;
use regex::Regex;
use text_edit::TextEdit;

use crate::common_test;
use crate::RootDatabase;
use crate::SourceDatabase;

mod application_env;
mod atoms_exhaustion;
mod cross_node_eval;
mod dependent_header;
mod deprecated_function;
mod effect_free_statement;
mod eqwalizer_assists;
mod expression_can_be_simplified;
mod from_config;
mod head_mismatch;
mod meck;
// @fb-only: mod meta_only;
mod missing_compile_warn_missing_spec;
mod missing_separator;
mod misspelled_attribute;
mod module_mismatch;
mod mutable_variable;
mod redundant_assignment;
mod replace_call;
mod trivial_match;
mod undefined_function;
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

use self::eqwalizer_assists::add_eqwalizer_assists;

#[derive(Debug, Clone, Default)]
pub struct Diagnostic {
    pub message: String,
    pub range: TextRange,
    pub severity: Severity,
    pub categories: FxHashSet<Category>,
    pub fixes: Option<Vec<Assist>>,
    pub related_info: Option<Vec<RelatedInformation>>,
    pub code: DiagnosticCode,
    pub code_doc_uri: Option<String>,
    // Used to combine syntax errors with erlang_service ones. If we
    // have a syntax error in the range, we filter out erlang_service
    // ones in the same range. We set it to the range of the enclosing form.
    pub form_range: Option<TextRange>,
}

pub fn group_label_ignore() -> GroupLabel {
    GroupLabel("ignore".into())
}

impl Diagnostic {
    pub fn new(code: DiagnosticCode, message: impl Into<String>, range: TextRange) -> Diagnostic {
        let message = message.into();
        Diagnostic {
            code: code.clone(),
            message,
            range,
            severity: Severity::Error,
            categories: FxHashSet::default(),
            fixes: None,
            related_info: None,
            code_doc_uri: code.as_uri(),
            form_range: None,
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

    pub(crate) fn with_form_range(mut self, form_range: Option<TextRange>) -> Diagnostic {
        self.form_range = form_range;
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

    pub(crate) fn should_be_ignored(&self, metadata: &Metadata) -> bool {
        metadata.by_source(Source::Elp).any(|annotation| {
            annotation.codes.contains(&self.code)
                && annotation.suppression_range.contains(self.range.start())
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
            let text = format!("\n{}% elp:ignore {}", indent, self.code.as_labeled_code(),);

            let offset = start_of_line(&token);
            builder.insert(offset, text);
            let edit = builder.finish();
            let source_change = SourceChange::from_text_edit(file_id, edit);
            let ignore_fix = Assist {
                id: AssistId("ignore_problem", AssistKind::QuickFix),
                label: Label::new("Ignore problem"),
                group: Some(group_label_ignore()),
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
    pub experimental: bool,
    pub disabled: FxHashSet<DiagnosticCode>,
    pub adhoc_semantic_diagnostics: Vec<&'a dyn AdhocSemanticDiagnostics>,
    pub lints_from_config: Arc<LintsFromConfig>,
    pub include_generated: bool,
    pub compile_options: Vec<CompileOption>,
}

impl<'a> DiagnosticsConfig<'a> {
    pub fn set_experimental(mut self, value: bool) -> DiagnosticsConfig<'a> {
        self.experimental = value;
        self
    }

    pub fn set_include_generated(mut self, value: bool) -> DiagnosticsConfig<'a> {
        self.include_generated = value;
        self
    }

    pub fn set_compile_options(mut self, options: Vec<CompileOption>) -> DiagnosticsConfig<'a> {
        self.compile_options = options;
        self
    }

    pub fn set_ad_hoc_semantic_diagnostics(
        mut self,
        diagnostics: Vec<&'a dyn AdhocSemanticDiagnostics>,
    ) -> DiagnosticsConfig<'a> {
        self.adhoc_semantic_diagnostics = diagnostics;
        self
    }

    pub fn disable(mut self, code: DiagnosticCode) -> DiagnosticsConfig<'a> {
        self.disabled.insert(code);
        self
    }

    pub fn from_config(
        mut self,
        lints_from_config: &Arc<LintsFromConfig>,
    ) -> DiagnosticsConfig<'a> {
        self.lints_from_config = lints_from_config.clone();
        self
    }
}

pub type Labeled = FxHashMap<Option<Label>, Vec<Diagnostic>>;

pub type TextRangeSet = RangeSet<[RangeInclusive<u32>; 1]>;

#[derive(Debug, Clone)]
pub struct LabeledDiagnostics {
    pub normal: Vec<Diagnostic>,
    pub syntax_error_form_ranges: TextRangeSet,
    /// Syntax error diagnostics labeled by the name/arity of the function enclosing them
    pub labeled_syntax_errors: Labeled,
    /// "Undefined XXX" diagnostics labeled by the name/arity of XXX.
    pub labeled_undefined_errors: Labeled,
}

impl Default for LabeledDiagnostics {
    fn default() -> Self {
        Self {
            syntax_error_form_ranges: RangeSet::from_elements(vec![]),
            normal: Default::default(),
            labeled_syntax_errors: Default::default(),
            labeled_undefined_errors: Default::default(),
        }
    }
}

impl LabeledDiagnostics {
    pub fn new(diagnostics: Vec<Diagnostic>) -> LabeledDiagnostics {
        LabeledDiagnostics {
            syntax_error_form_ranges: RangeSet::from_elements(vec![]),
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
        self.normal.extend(iter.into_iter())
    }
}

/// Convert a `TextRange` into a form suitable for a `TextRangeSet`
fn to_range(range: &TextRange) -> RangeInclusive<u32> {
    RangeInclusive::new(range.start().into(), range.end().into())
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
        Some(s) => format!("\n\n{}", s),
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
        code: DiagnosticCode::Eqwalizer(d.code.clone()),
        message,
        categories: FxHashSet::default(),
        fixes: None,
        related_info: None,
        code_doc_uri: Some(d.uri.clone()),
        form_range: None,
    };
    add_eqwalizer_assists(sema, file_id, d, &mut diagnostic);
    diagnostic
}

/// Main entry point to calculate ELP-native diagnostics for a file
pub fn native_diagnostics(
    db: &RootDatabase,
    config: &DiagnosticsConfig,
    file_id: FileId,
) -> LabeledDiagnostics {
    lazy_static! {
        static ref EXTENSIONS: Vec<FileKind> =
            vec![FileKind::SrcModule, FileKind::TestModule, FileKind::Header];
    };
    let parse = db.parse(file_id);

    let file_kind = db.file_kind(file_id);
    let report_diagnostics = EXTENSIONS.iter().any(|it| it == &file_kind);

    let mut res = Vec::new();

    let (labeled_syntax_errors, syntax_error_form_ranges) = if report_diagnostics {
        let sema = Semantic::new(db);

        if file_kind.is_module() {
            no_module_definition_diagnostic(&mut res, &parse);
            if config.include_generated || !db.is_generated(file_id) {
                unused_include::unused_includes(&sema, db, &mut res, file_id);
            }
        }

        res.append(&mut form_missing_separator_diagnostics(&parse));

        config
            .adhoc_semantic_diagnostics
            .iter()
            .for_each(|f| f(&mut res, &sema, file_id, file_kind));
        config
            .lints_from_config
            .get_diagnostics(&mut res, &sema, file_id);
        semantic_diagnostics(&mut res, &sema, file_id, file_kind, config);
        // @fb-only: meta_only::diagnostics(&mut res, &sema, file_id);
        syntax_diagnostics(&sema, &parse, &mut res, file_id);

        let parse_diagnostics = parse.errors().iter().take(128).map(|err| {
            let (code, message) = match err {
                elp_syntax::SyntaxError::Error(_) => {
                    (DiagnosticCode::SyntaxError, "Syntax Error".to_string())
                }
                elp_syntax::SyntaxError::Missing(m, _) => (
                    DiagnosticCode::Missing("missing".to_string()),
                    format!("Missing '{}'", m),
                ),
            };
            Diagnostic::error(code, widen_range(err.range()), message)
                .with_form_range(get_form_range(&parse.syntax_node(), err.range()))
        });
        let source_file = db.parse(file_id).tree();
        group_syntax_errors(&source_file, parse_diagnostics)
    } else {
        (FxHashMap::default(), RangeSet::from_elements(vec![]))
    };
    let metadata = db.elp_metadata(file_id);
    // TODO: can we  ever disable DiagnosticCode::SyntaxError?
    //       In which case we must check labeled_syntax_errors
    res.retain(|d| {
        !config.disabled.contains(&d.code)
            && (config.experimental && d.has_category(Category::Experimental)
                || !d.has_category(Category::Experimental))
            && !d.should_be_ignored(&metadata)
    });

    LabeledDiagnostics {
        syntax_error_form_ranges,
        normal: res,
        labeled_syntax_errors,
        labeled_undefined_errors: FxHashMap::default(),
    }
}

/// Get the range of the form enclosing the passed-in range, if there
/// is one.
fn get_form_range(syntax: &SyntaxNode, range: TextRange) -> Option<TextRange> {
    algo::find_node_at_offset::<ast::Form>(syntax, range.start()).map(|n| n.syntax().text_range())
}

fn group_syntax_errors(
    source_file: &SourceFile,
    diagnostics: impl Iterator<Item = Diagnostic>,
) -> (Labeled, TextRangeSet) {
    let mut map: Labeled = FxHashMap::default();
    let mut ranges = RangeSet::from_elements(vec![]);
    diagnostics.for_each(|d| {
        if let Some(range) = d.form_range {
            ranges.insert_range(to_range(&range));
        }
        map.entry(function_label(&d, source_file))
            .or_default()
            .push(d);
    });
    (map, ranges)
}

fn function_label(d: &Diagnostic, source_file: &SourceFile) -> Option<Label> {
    if d.code.is_syntax_error() {
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
    config: &DiagnosticsConfig,
) {
    if config.include_generated || !sema.db.is_generated(file_id) {
        // TODO: disable this check when T151727890 and T151605845 are resolved
        if config.experimental {
            unused_function_args::unused_function_args(res, sema, file_id);
            redundant_assignment::redundant_assignment(res, sema, file_id);
            trivial_match::trivial_match(res, sema, file_id);
        }
        unused_macro::unused_macro(res, sema, file_id, file_kind);
        unused_record_field::unused_record_field(res, sema, file_id, file_kind);
        mutable_variable::mutable_variable_bug(res, sema, file_id);
        effect_free_statement::effect_free_statement(res, sema, file_id);
        expression_can_be_simplified::diagnostic(res, sema, file_id);
        application_env::application_env(res, sema, file_id);
        missing_compile_warn_missing_spec::missing_compile_warn_missing_spec(res, sema, file_id);
        cross_node_eval::cross_node_eval(res, sema, file_id);
        dependent_header::dependent_header(res, sema, file_id, file_kind);
        deprecated_function::deprecated_function(res, sema, file_id);
        undefined_function::undefined_function(res, sema, file_id);
        head_mismatch::head_mismatch_semantic(res, sema, file_id);
        missing_separator::missing_separator_semantic(res, sema, file_id);
        atoms_exhaustion::atoms_exhaustion(res, sema, file_id);
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
    let message = format!("Missing '{}'", item);
    Diagnostic::new(DiagnosticCode::Missing(code), message, range).with_severity(Severity::Warning)
}

pub(crate) fn make_unexpected_diagnostic(
    range: TextRange,
    item: &'static str,
    code: String,
) -> Diagnostic {
    let message = format!("Unexpected '{}'", item);
    Diagnostic::new(DiagnosticCode::Unexpected(code), message, range)
        .with_severity(Severity::Warning)
}

pub fn erlang_service_diagnostics(
    db: &RootDatabase,
    file_id: FileId,
    config: &DiagnosticsConfig,
) -> Vec<(FileId, LabeledDiagnostics)> {
    if config.include_generated || !db.is_generated(file_id) {
        // Use the same format as eqwalizer, so we can re-use the salsa cache entry
        let format = erlang_service::Format::OffsetEtf;

        let res = db.module_ast(file_id, format, config.compile_options.clone());

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
            diags_map.into_iter().collect()
        };
        label_erlang_service_diagnostics(diags)
    } else {
        vec![]
    }
}

/// We label erlang service diagnostics with any references to
/// undefined function or undefined spec.  This is so we can tie them
/// up to ELP native parse errors occurring in a function that has the
/// same label.
fn label_erlang_service_diagnostics(
    diagnostics: Vec<(FileId, Vec<Diagnostic>)>,
) -> Vec<(FileId, LabeledDiagnostics)> {
    diagnostics
        .into_iter()
        .map(|(file_id, ds)| {
            let mut labeled_undefined_errors: Labeled = FxHashMap::default();
            ds.into_iter().for_each(|d| {
                labeled_undefined_errors
                    .entry(erlang_service_label(&d))
                    .or_default()
                    .push(d);
            });
            (
                file_id,
                LabeledDiagnostics {
                    syntax_error_form_ranges: RangeSet::from_elements(vec![]),
                    normal: Vec::default(),
                    labeled_syntax_errors: FxHashMap::default(),
                    labeled_undefined_errors,
                },
            )
        })
        .collect_vec()
}

pub fn eqwalizer_diagnostics(
    db: &RootDatabase,
    file_id: FileId,
    include_generated: bool,
) -> Option<Vec<Diagnostic>> {
    let eqwalizer_diagnostics = db.eqwalizer_diagnostics(file_id, include_generated)?;
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
    let ast = db.module_ast(file_id, format, vec![]);
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
        diags.into_iter().for_each(|(file_id, diag)| {
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

    // If the file cannot be parsed, return early.
    // Use the same format as eqwalizer, so we can re-use the salsa cache entry.
    let format = erlang_service::Format::OffsetEtf;
    let ast = db.module_ast(file_id, format, vec![]);
    if !ast.is_ok() {
        return Arc::new(CommonTestInfo::BadAST);
    }

    db.ct_info(file_id)
}

/// Diagnostics requiring the erlang_service
pub fn ct_diagnostics(db: &RootDatabase, file_id: FileId) -> Vec<Diagnostic> {
    let mut res: Vec<Diagnostic> = Vec::new();
    let sema = Semantic::new(db);

    meck::missing_no_link_in_init_per_suite(&mut res, &sema, file_id);

    match &*ct_info(db, file_id) {
        CommonTestInfo::Result { all, groups } => {
            let testcases =
                common_test::runnable_names(&sema, file_id, all.clone(), groups.clone()).ok();
            common_test::unreachable_test(&mut res, &sema, file_id, &testcases);
            // @fb-only: meta_only::ct_diagnostics(&mut res, &sema, file_id, testcases);
        }
        CommonTestInfo::EvalError(_error) => {
            // The error currently does not contain anything useful, so we ignore it
            common_test::ct_info_eval_error(&mut res, &sema, file_id);
        }
        _ => (),
    };
    let metadata = db.elp_metadata(file_id);
    res.into_iter()
        .filter(|d| !d.should_be_ignored(&metadata))
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

/// Combine the ELP and erlang_service diagnostics.  In particular,
/// flatten any cascading diagnostics if possible.
pub fn attach_related_diagnostics(
    native: LabeledDiagnostics,
    erlang_service: &LabeledDiagnostics,
) -> Vec<Diagnostic> {
    // `native` is labelled with the MFA of functions having syntax
    // errors in them. `erlang_service` is labelled with the MFA of
    // undefined functions.  For each labeled one in `native`, add the
    // corresponding ones from `erlang_service`, remember the label,
    // and when done delete all `erlang_service` diagnostics with that
    // label.

    // There is a many-to-many relationship between syntax errors and
    // related info, because there can be more than one syntax error
    // in a given function.  So we have to keep the original lookup
    // intact, then remove ones that are used at least once at the
    // end.
    let mut to_remove: FxHashSet<&Option<Label>> = FxHashSet::default();
    let updated = native
        .labeled_syntax_errors
        .iter()
        .flat_map(|(label, diags)| {
            if let Some(related) = erlang_service.labeled_undefined_errors.get(label) {
                to_remove.insert(label);
                let related_info = related.iter().map(|d| d.as_related()).collect_vec();
                diags
                    .iter()
                    .map(|d| d.clone().with_related(Some(related_info.clone())))
                    .collect_vec()
            } else {
                diags.to_vec()
            }
        })
        .collect_vec();

    let es = erlang_service
        .labeled_undefined_errors
        .iter()
        .filter(|(k, _)| !to_remove.contains(k))
        .flat_map(|(_, v)| v)
        .filter(|d| !already_reported(&native.syntax_error_form_ranges, &d.range));

    native
        .normal
        .into_iter()
        .chain(updated)
        .chain(es.cloned())
        // TODO:AZ: consider returning an iterator
        .collect_vec()
}

/// Both ELP and the erlang_service produce syntax errors. However,
/// the ELP grammar is deliberately more permissive, to allow
/// analysing even slightly broken code.  This means it does not
/// report all errors.
/// But when ELP does report errors, they are generally more informative.
/// We solve this problem by
/// * recording the `TextRange` of the form containing each ELP syntax
///   error.
/// * Combining all of these into a `RangeSet`
/// * Recording the originating `TextRange` of every `diagnostics::Diagnostic`
/// * Filter out any `erlang_service` ones that are in one of the ELP form ranges.
pub fn already_reported(syntax_error_form_ranges: &TextRangeSet, range: &TextRange) -> bool {
    syntax_error_form_ranges.contains((*range).start().into())
}

fn erlang_service_label(diagnostic: &Diagnostic) -> Option<Label> {
    match &diagnostic.code {
        DiagnosticCode::ErlangService(s) => match s.as_str() {
            "L1227" => function_undefined_from_message(&diagnostic.message).map(Label::new_raw),
            "L1308" => {
                spec_for_undefined_function_from_message(&diagnostic.message).map(Label::new_raw)
            }
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
    use super::*;
    use crate::codemod_helpers::FunctionMatch;
    use crate::codemod_helpers::MFA;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests::check_diagnostics;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_diagnostics_with_config_and_extra;
    use crate::tests::check_specific_fix;

    #[test]
    fn syntax_error() {
        check_diagnostics(
            r#"
-module(main).
foo() -> XX 3.
    %%      ^ error: Syntax Error
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
        let config = DiagnosticsConfig::default()
            .set_ad_hoc_semantic_diagnostics(vec![&|acc, sema, file_id, _ext| {
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
            }])
            .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
            .disable(DiagnosticCode::UndefinedFunction);
        check_diagnostics_with_config(
            DiagnosticsConfig {
                experimental: true,
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
             //- /main/src/main.erl app:main
             % @unknown
             %%<^^^^^^^^  warning: tag @unknown not recognized.
             -module(main).

             "#,
        );
    }

    #[test]
    fn group_related_diagnostics_1() {
        let labeled_undefined_errors = FxHashMap::from_iter([(
            Some(Label::new_raw("foo/0")),
            vec![
                Diagnostic {
                    message: "function foo/0 undefined".to_string(),
                    range: TextRange::new(21.into(), 43.into()),
                    severity: Severity::Error,
                    categories: FxHashSet::default(),
                    fixes: None,
                    related_info: None,
                    code: "L1227".into(),
                    code_doc_uri: None,
                    form_range: None,
                },
                Diagnostic {
                    message: "function foo/0 undefined".to_string(),
                    range: TextRange::new(74.into(), 79.into()),
                    severity: Severity::Error,
                    categories: FxHashSet::default(),
                    fixes: None,
                    related_info: None,
                    code: "L1227".into(),
                    code_doc_uri: None,
                    form_range: None,
                },
                Diagnostic {
                    message: "spec for undefined function foo/0".to_string(),
                    range: TextRange::new(82.into(), 99.into()),
                    severity: Severity::Error,
                    categories: FxHashSet::default(),
                    fixes: None,
                    related_info: None,
                    code: "L1308".into(),
                    code_doc_uri: None,
                    form_range: None,
                },
            ],
        )]);
        let extra_diags = LabeledDiagnostics {
            syntax_error_form_ranges: RangeSet::from_elements(vec![]),
            normal: vec![Diagnostic {
                message: "syntax error before: '->'".to_string(),
                range: TextRange::new(106.into(), 108.into()),
                severity: Severity::Error,
                categories: FxHashSet::default(),
                fixes: None,
                related_info: None,
                code: "P1711".into(),
                code_doc_uri: None,
                form_range: None,
            }],
            labeled_syntax_errors: FxHashMap::default(),
            labeled_undefined_errors,
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
    fn check_specific_fix_works() {
        check_specific_fix(
            "Remove match",
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

    #[test]
    fn test_eqwalizer_diagnostics() {
        check_diagnostics(
            r#"
            //- eqwalizer
            //- /play/src/bar.erl app:play
                -module(bar).

                -spec baz() -> ok.
                baz() -> something_else.
                %%       ^^^^^^^^^^^^^^ 💡 error: eqwalizer: incompatible_types
            "#,
        );
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
}
