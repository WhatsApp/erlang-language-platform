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

#[cfg(test)]
mod tests {
    use elp_project_model::otp::supports_eep59_doc_attributes;

    use crate::fixture;

    #[track_caller]
    fn check(fixture: &str, expected: &str) {
        let (analysis, fixture) = fixture::with_fixture(fixture);
        let docs = analysis
            .get_docs_at_position(fixture.position())
            .unwrap()
            .unwrap();
        let doc = docs.doc.unwrap();
        assert_eq!(doc.markdown_text(), expected);
    }

    #[test]
    fn local_type() {
        if supports_eep59_doc_attributes() {
            check(
                r#"
-module(main).
-export([main/0]).
-doc """
My integer
""".
-type my_integer() :: integer().

-spec main() -> my_in~teger().
main() -> 42.
"#,
                "\
```erlang
-type my_integer() :: integer().
```

-----

My integer",
            );
        }
    }

    #[test]
    fn remote_type() {
        if supports_eep59_doc_attributes() {
            check(
                r#"
//- /src/main.erl
-module(main).
-export([main/0]).
-spec main() -> other:my_in~teger().
main() -> 42.

//- /src/other.erl
-module(other).
-doc """
My integer
""".
-export_type([my_integer/0]).
-type my_integer() :: integer().
"#,
                "\
```erlang
-type my_integer() :: integer().
```

-----

My integer",
            );
        }
    }

    #[test]
    fn local_function() {
        if supports_eep59_doc_attributes() {
            check(
                r#"
-module(main).
-export([main/0]).

-doc """
My function
""".
-spec main() -> ok.
main() -> ok.

-spec caller() -> ok.
caller() -> ma~in().
"#,
                "\
```erlang
-spec main() -> ok.
```

-----

My function",
            );
        }
    }
}
