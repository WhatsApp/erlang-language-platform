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

// @fb-only

// @fb-only
mod otp_links;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocLink {
    pub uri: String,
    pub title: String,
}

/// Retrieve a link to documentation for the given symbol.
pub(crate) fn external_docs(db: &RootDatabase, position: &FilePosition) -> Option<Vec<DocLink>> {
    let sema = Semantic::new(db);
    let file_id = position.file_id;
    let source_file = sema.parse(file_id);

    let node = source_file.value.syntax();
    let token = node.token_at_offset(position.offset).left_biased()?;

    let mut doc_links = Vec::new();
    let in_file_token = InFile::new(file_id, token);
    if let Some(class) = SymbolClass::classify(&sema, in_file_token.clone()) {
        class.iter().for_each(|def| {
            otp_links::links(&mut doc_links, &sema, &def);
            // @fb-only
        });
    }
    // @fb-only
    Some(doc_links)
}

#[cfg(test)]
pub(crate) mod tests {
    use crate::fixture;

    pub(crate) fn check_links(fixture: &str, expected_links: Vec<&str>) {
        let (analysis, position, _) = fixture::position(fixture);
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
