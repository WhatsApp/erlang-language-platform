/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::path::Path;
use std::str::FromStr;

use anyhow::Result;
use anyhow::anyhow;
use elp_ide::TextRange;
use elp_ide::TextSize;
use elp_ide::diagnostics::Diagnostic;
use elp_ide::diagnostics::DiagnosticCode;
use elp_ide::diagnostics::DiagnosticTag;
use elp_ide::diagnostics::RelatedInformation;
use elp_ide::diagnostics::Severity;
use elp_ide::elp_ide_db::EqwalizerDiagnostic;
use elp_ide::elp_ide_db::LineIndex;
use elp_ide::elp_ide_db::assists::AssistContextDiagnostic;
use elp_ide::elp_ide_db::assists::AssistContextDiagnosticCode;
use elp_ide::elp_ide_db::elp_base_db::AbsPath;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use lsp_types::DiagnosticRelatedInformation;
use lsp_types::Location;
use lsp_types::Url;
use paths::Utf8Component;
use paths::Utf8Prefix;

use crate::arc_types;
use crate::from_proto;

pub fn abs_path(url: &lsp_types::Url) -> Result<AbsPathBuf> {
    let path = url
        .to_file_path()
        .map_err(|()| anyhow!("url '{}' is not a file", url))?;
    Ok(AbsPathBuf::assert_utf8(path))
}

pub fn vfs_path(url: &lsp_types::Url) -> Result<VfsPath> {
    abs_path(url).map(VfsPath::from)
}

pub fn range(line_index: &LineIndex, range: TextRange) -> lsp_types::Range {
    let start = position(line_index, range.start());
    let end = position(line_index, range.end());
    lsp_types::Range::new(start, end)
}

pub fn position(line_index: &LineIndex, offset: TextSize) -> lsp_types::Position {
    let line_col = line_index.line_col(offset);
    lsp_types::Position::new(line_col.line, line_col.col_utf16)
}

pub fn diagnostic_severity(severity: Severity) -> lsp_types::DiagnosticSeverity {
    match severity {
        Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
        Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
        Severity::WeakWarning => lsp_types::DiagnosticSeverity::HINT,
        Severity::Information => lsp_types::DiagnosticSeverity::INFORMATION,
    }
}

pub fn ide_to_lsp_diagnostic(
    line_index: &LineIndex,
    url: &Url,
    d: &Diagnostic,
) -> lsp_types::Diagnostic {
    let code_description = match &d.code_doc_uri {
        Some(uri) => match lsp_types::Url::parse(uri) {
            Ok(href) => Some(lsp_types::CodeDescription { href }),
            Err(_) => None,
        },
        None => None,
    };
    let (source, code) = match &d.code {
        DiagnosticCode::Eqwalizer(code) => (Some("eqWAlizer".into()), code.clone()),
        _ => (Some("elp".into()), d.code.to_string()),
    };
    lsp_types::Diagnostic {
        range: range(line_index, d.range),
        severity: Some(diagnostic_severity(d.severity)),
        code: Some(lsp_types::NumberOrString::String(code)),
        code_description,
        source,
        message: d.message.clone(),
        related_information: from_related(line_index, url, &d.related_info),
        tags: d.tag.as_ref().map(lsp_diagnostic_tags),
        data: None,
    }
}

fn lsp_diagnostic_tags(d: &DiagnosticTag) -> Vec<lsp_types::DiagnosticTag> {
    match d {
        DiagnosticTag::Unused => vec![lsp_types::DiagnosticTag::UNNECESSARY],
        DiagnosticTag::Deprecated => vec![lsp_types::DiagnosticTag::DEPRECATED],
    }
}

pub fn lsp_to_assist_context_diagnostic(
    line_index: &LineIndex,
    d: lsp_types::Diagnostic,
) -> Option<AssistContextDiagnostic> {
    let range = from_proto::safe_text_range(line_index, d.range)?;
    if let Some(lsp_types::NumberOrString::String(code)) = d.code {
        match AssistContextDiagnosticCode::from_str(&code) {
            Ok(code) => Some(AssistContextDiagnostic::new(code, d.message, range)),
            Err(_) => None,
        }
    } else {
        None
    }
}

pub fn eqwalizer_to_arc_diagnostic(
    d: &EqwalizerDiagnostic,
    line_index: &LineIndex,
    relative_path: &Path,
    eqwalizer_enabled: bool,
) -> arc_types::Diagnostic {
    let pos = position(line_index, d.range.start());
    let line_num = pos.line + 1;
    let character = Some(pos.character + 1);
    let severity = if eqwalizer_enabled {
        arc_types::Severity::Error
    } else {
        // We use Severity::Disabled so that we have the ability in our arc linter to choose
        // to display lints for *new* files with errors that are not opted in (T118466310).
        // See comment at the top of eqwalizer_cli.rs for more information.
        arc_types::Severity::Disabled
    };
    // formatting: https://fburl.com/max_wiki_link_to_phabricator_rich_text
    let explanation = match &d.explanation {
        Some(s) => format!("```\n{s}\n```"),
        None => "".to_string(),
    };
    let link = format!("> [docs on `{}`]({})", d.code, d.uri);
    let message = format!(
        "```lang=error,counterexample
{}
{}
```
{}
{}",
        d.expr_string(),
        d.message,
        explanation,
        link
    );
    let name = format!("eqWAlizer: {}", d.code);
    arc_types::Diagnostic::new(
        relative_path,
        line_num,
        character,
        severity,
        name,
        message,
        d.expression.clone(),
        None,
    )
}

fn from_related(
    line_index: &LineIndex,
    url: &Url,
    r: &Option<Vec<RelatedInformation>>,
) -> Option<Vec<DiagnosticRelatedInformation>> {
    r.as_ref().map(|ri| {
        ri.iter()
            .map(|i| {
                let location = Location {
                    range: range(line_index, i.range),
                    uri: url.clone(),
                };
                DiagnosticRelatedInformation {
                    location,
                    message: i.message.clone(),
                }
            })
            .collect()
    })
}

// Taken from rust-analyzer to_proto.rs

/// Returns a `Url` object from a given path, will lowercase drive letters if present.
/// This will only happen when processing windows paths.
///
/// When processing non-windows path, this is essentially the same as `Url::from_file_path`.
pub(crate) fn url_from_abs_path(path: &AbsPath) -> lsp_types::Url {
    let url = lsp_types::Url::from_file_path(path).unwrap();
    match path.components().next() {
        Some(Utf8Component::Prefix(prefix))
            if matches!(
                prefix.kind(),
                Utf8Prefix::Disk(_) | Utf8Prefix::VerbatimDisk(_)
            ) =>
        {
            // Need to lowercase driver letter
        }
        _ => return url,
    }

    let driver_letter_range = {
        let mut segments = url.as_str().splitn(3, ':');
        let start = match segments.next() {
            Some(scheme) => scheme.len() + ':'.len_utf8(),
            None => return url,
        };
        match segments.next() {
            Some(drive_letter) => start..(start + drive_letter.len()),
            None => return url,
        }
    };

    // Note: lowercasing the `path` itself doesn't help, the `Url::parse`
    // machinery *also* canonicalizes the drive letter. So, just massage the
    // string in place.
    let mut url: String = url.into();
    url[driver_letter_range].make_ascii_lowercase();
    lsp_types::Url::parse(&url).unwrap()
}

fn ide_to_arc_severity(severity: Severity) -> arc_types::Severity {
    match severity {
        Severity::Error => arc_types::Severity::Error,
        Severity::Warning => arc_types::Severity::Warning,
        Severity::WeakWarning => arc_types::Severity::Disabled,
        Severity::Information => arc_types::Severity::Advice,
    }
}

pub fn ide_to_arc_diagnostic(
    line_index: &LineIndex,
    path: &Path,
    diagnostic: &Diagnostic,
    use_cli_severity: bool,
) -> arc_types::Diagnostic {
    let pos = position(line_index, diagnostic.range.start());
    let line_num = pos.line + 1;
    let character = Some(pos.character + 1);
    let message = diagnostic.message.clone();
    let description = match diagnostic.code.as_uri() {
        Some(uri) => format!("{message}\n\nFor more information see: {uri}"),
        None => message,
    };
    let severity = diagnostic.severity(use_cli_severity);
    let doc_path = diagnostic.code.as_doc_path();
    arc_types::Diagnostic::new(
        path,
        line_num,
        character,
        ide_to_arc_severity(severity),
        diagnostic.code.as_labeled_code(),
        description,
        None,
        doc_path,
    )
}
