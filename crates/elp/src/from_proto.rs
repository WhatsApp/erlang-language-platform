/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Conversion lsp_types types to ELP specific ones.

use elp_ide::elp_ide_assists::AssistKind;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FilePosition;
use elp_ide::elp_ide_db::elp_base_db::FileRange;
use elp_ide::elp_ide_db::LineCol;
use elp_ide::elp_ide_db::LineIndex;
use elp_ide::TextRange;
use elp_ide::TextSize;

use crate::snapshot::Snapshot;
use crate::Result;

pub(crate) fn offset(line_index: &LineIndex, position: lsp_types::Position) -> TextSize {
    let line_col = LineCol {
        line: position.line,
        col_utf16: position.character,
    };
    // Temporary for T147609435
    let _pctx = stdx::panic_context::enter("\nfrom_proto::offset".to_string());
    line_index.offset(line_col)
}

pub(crate) fn text_range(line_index: &LineIndex, range: lsp_types::Range) -> TextRange {
    let start = offset(line_index, range.start);
    let end = offset(line_index, range.end);
    TextRange::new(start, end)
}

pub(crate) fn safe_offset(
    line_index: &LineIndex,
    position: lsp_types::Position,
) -> Option<TextSize> {
    let line_col = LineCol {
        line: position.line,
        col_utf16: position.character,
    };
    line_index.safe_offset(line_col)
}

/// If we receive an LSP Range from a possibly earlier version of the
/// file, it may not map into our current file, or may not have start < end
pub(crate) fn safe_text_range(
    line_index: &LineIndex,
    range: lsp_types::Range,
) -> Option<TextRange> {
    // TODO: Remove the logging once we know that we have averted the problem (T147609435)
    let start = if let Some(offset) = safe_offset(line_index, range.start) {
        offset
    } else {
        log::warn!("from_proto::safe_text_range failed for {:?}", range.start);
        return None;
    };
    let end = if let Some(offset) = safe_offset(line_index, range.end) {
        offset
    } else {
        log::warn!("from_proto::safe_text_range failed for {:?}", range.end);
        return None;
    };
    if start <= end {
        Some(TextRange::new(start, end))
    } else {
        None
    }
}

pub(crate) fn file_id(snap: &Snapshot, url: &lsp_types::Url) -> Result<FileId> {
    snap.url_to_file_id(url)
}

pub(crate) fn file_position(
    snap: &Snapshot,
    tdpp: lsp_types::TextDocumentPositionParams,
) -> Result<FilePosition> {
    let file_id = snap.url_to_file_id(&tdpp.text_document.uri)?;
    let line_index = snap.analysis.line_index(file_id)?;
    let offset = offset(&line_index, tdpp.position);
    Ok(FilePosition { file_id, offset })
}

pub(crate) fn file_range(
    snap: &Snapshot,
    text_document_identifier: lsp_types::TextDocumentIdentifier,
    range: lsp_types::Range,
) -> Result<FileRange> {
    let file_id = snap.url_to_file_id(&text_document_identifier.uri)?;
    let line_index = snap.analysis.line_index(file_id)?;
    let range =
        safe_text_range(&line_index, range).ok_or(anyhow::anyhow!("invalid range: {:?}", range))?;
    Ok(FileRange { file_id, range })
}

pub(crate) fn assist_kind(kind: lsp_types::CodeActionKind) -> Option<AssistKind> {
    let assist_kind = match &kind {
        k if k == &lsp_types::CodeActionKind::EMPTY => AssistKind::None,
        k if k == &lsp_types::CodeActionKind::QUICKFIX => AssistKind::QuickFix,
        k if k == &lsp_types::CodeActionKind::REFACTOR => AssistKind::Refactor,
        k if k == &lsp_types::CodeActionKind::REFACTOR_EXTRACT => AssistKind::RefactorExtract,
        k if k == &lsp_types::CodeActionKind::REFACTOR_INLINE => AssistKind::RefactorInline,
        k if k == &lsp_types::CodeActionKind::REFACTOR_REWRITE => AssistKind::RefactorRewrite,
        _ => return None,
    };

    Some(assist_kind)
}
