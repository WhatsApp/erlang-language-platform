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
use elp_ide_db::elp_base_db::FilePosition;

use crate::doc_links::DocLink;
use crate::doc_links::external_docs;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HoverActionsConfig {
    pub doc_links: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HoverAction {
    DocLink(DocLink),
}

pub(crate) fn actions(
    db: &RootDatabase,
    position: FilePosition,
    config: &HoverActionsConfig,
) -> Vec<HoverAction> {
    let mut res = Vec::new();
    if config.doc_links {
        doc_links_actions(&mut res, db, position);
    }
    res
}

fn doc_links_actions(res: &mut Vec<HoverAction>, db: &RootDatabase, position: FilePosition) {
    let doc_links = external_docs(db, &position).unwrap_or_default();
    for doc_link in doc_links {
        res.push(HoverAction::DocLink(doc_link));
    }
}

#[cfg(test)]
mod tests {

    use crate::HoverAction;
    use crate::HoverActionsConfig;
    use crate::doc_links::DocLink;
    use crate::fixture;

    #[track_caller]
    fn check(fixture: &str, expected: Vec<HoverAction>) {
        let (analysis, position, _) = fixture::position(fixture);
        let actual = analysis
            .hover_actions(position, &HoverActionsConfig { doc_links: true })
            .unwrap();
        assert_eq!(
            expected, actual,
            "\nExpected:\n{expected:#?}\n\nActual:\n{actual:#?}"
        )
    }

    #[test]
    fn docs() {
        check(
            r#"
//- /opt/lib/stdlib-3.17/src/lists.erl otp_app:/opt/lib/stdlib-3.17
-module(lists).
-export([reverse/1]).
reverse([]) -> [].

//- /src/main.erl
-module(main).
main() ->
  list~s:reverse([]).
    "#,
            vec![HoverAction::DocLink(DocLink {
                title: "lists".to_string(),
                uri: "https://erlang.org/doc/man/lists.html".to_string(),
            })],
        );
    }
}
