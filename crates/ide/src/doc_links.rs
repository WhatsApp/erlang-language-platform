/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::RootDatabase;
use elp_ide_db::SymbolClass;
use elp_syntax::AstNode;
use hir::InFile;
use hir::Semantic;

mod otp_links;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocLink {
    pub uri: String,
    pub title: String,
}

/// Retrieve a link to documentation for the given symbol.
pub(crate) fn external_docs(db: &RootDatabase, position: &FilePosition) -> Option<Vec<DocLink>> {
    let sema = Semantic::new(db);
    let source_file = sema.parse(position.file_id);

    let token = source_file
        .value
        .syntax()
        .token_at_offset(position.offset)
        .left_biased()?;

    let doc_links = SymbolClass::classify(&sema, InFile::new(position.file_id, token))?
        .iter()
        .filter_map(|def| otp_links::links(&sema, def))
        .flatten()
        .collect();
    Some(doc_links)
}

#[cfg(test)]
mod tests {
    use crate::fixture;

    pub(crate) fn check_links(fixture: &str, expected_links: Vec<&str>) {
        let (analysis, position) = fixture::position(fixture);
        let actual_links: Vec<String> = analysis
            .external_docs(position)
            .ok()
            .unwrap()
            .unwrap()
            .iter()
            .map(|link| link.uri.clone())
            .collect();
        assert_eq!(actual_links, expected_links);
    }
}
