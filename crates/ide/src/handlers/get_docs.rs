/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::RootDatabase;
use elp_ide_db::docs::Doc;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::find_best_token;
use hir::Semantic;

pub(crate) fn get_doc_at_position(
    db: &RootDatabase,
    position: FilePosition,
) -> Option<(Doc, Option<FileRange>)> {
    let sema = Semantic::new(db);
    let docs = elp_ide_db::docs::Documentation::new(db, &sema);
    let token = find_best_token(&sema, position)?;

    let range = FileRange {
        file_id: token.file_id,
        range: token.value.text_range(),
    };
    let doc = Doc::from_reference(&docs, &token);
    doc.map(|d| (d, Some(range)))
}
