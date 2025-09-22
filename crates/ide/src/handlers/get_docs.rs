/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::RootDatabase;
use elp_ide_db::docs::Doc;
use elp_syntax::SyntaxToken;
use hir::InFile;
use hir::Semantic;

pub(crate) fn get_doc_for_token(
    db: &RootDatabase,
    sema: &Semantic,
    token: &InFile<SyntaxToken>,
) -> Option<Doc> {
    let docs = elp_ide_db::docs::Documentation::new(db, sema);
    Doc::from_reference(&docs, token)
}
