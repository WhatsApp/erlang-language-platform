/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Linters for detecting eqwalizer escape hatches.
//!
//! These linters detect usage of eqwalizer escape hatches that allow bypassing
//! type checking. While sometimes necessary, excessive use indicates potential
//! type safety issues that should be addressed.

use elp_ide_db::LineIndex;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::metadata;
use elp_ide_db::metadata::Kind;
use elp_ide_db::metadata::Source;
use elp_ide_ssr::SsrSearchScope;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use lazy_static::lazy_static;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;

fn find_eqwalizer_comments(
    sema: &Semantic,
    file_id: FileId,
    kind: Kind,
) -> Option<Vec<GenericLinterMatchContext<()>>> {
    let file_text = sema.db.file_text(file_id);
    let line_index = LineIndex::new(&file_text);
    let source = sema.db.parse(file_id);
    let metadata = metadata::collect_metadata(&line_index, &file_text, &source);

    let res: Vec<_> = metadata
        .by_source(Source::Eqwalizer)
        .filter(|ann| ann.kind == kind)
        .map(|ann| GenericLinterMatchContext {
            range: FileRange {
                file_id,
                range: ann.comment_range,
            },
            context: (),
        })
        .collect();
    Some(res)
}

// -----------------------------------------------------------------------------
// EqwalizerFixmeLinter
// -----------------------------------------------------------------------------

pub(crate) struct EqwalizerFixmeLinter;

impl Linter for EqwalizerFixmeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::EqwalizerFixme
    }

    fn description(&self) -> &'static str {
        "The `eqwalizer:fixme` comment suppresses eqwalizer type errors on the following line. \
         Consider fixing the underlying type issue instead of suppressing it."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }

    fn should_process_test_files(&self) -> bool {
        false
    }
}

impl GenericLinter for EqwalizerFixmeLinter {
    type Context = ();

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        find_eqwalizer_comments(sema, file_id, Kind::Fixme)
    }
}

pub static EQWALIZER_FIXME_LINTER: EqwalizerFixmeLinter = EqwalizerFixmeLinter;

// -----------------------------------------------------------------------------
// EqwalizerIgnoreLinter
// -----------------------------------------------------------------------------

pub(crate) struct EqwalizerIgnoreLinter;

impl Linter for EqwalizerIgnoreLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::EqwalizerIgnore
    }

    fn description(&self) -> &'static str {
        "The `eqwalizer:ignore` comment suppresses eqwalizer type errors on the following line. \
         Consider fixing the underlying type issue instead of suppressing it."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }

    fn should_process_test_files(&self) -> bool {
        false
    }
}

impl GenericLinter for EqwalizerIgnoreLinter {
    type Context = ();

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        find_eqwalizer_comments(sema, file_id, Kind::Ignore)
    }
}

pub static EQWALIZER_IGNORE_LINTER: EqwalizerIgnoreLinter = EqwalizerIgnoreLinter;

// -----------------------------------------------------------------------------
// UncheckedCastLinter
// -----------------------------------------------------------------------------

pub(crate) struct UncheckedCastLinter;

impl Linter for UncheckedCastLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UncheckedCast
    }

    fn description(&self) -> &'static str {
        "The `?UNCHECKED_CAST` macro bypasses eqwalizer type checking by asserting a value \
         has a specific type without verification. Consider using ?CHECKED_CAST as a proper type-safe alternative if possible."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::WeakWarning
    }

    fn should_process_test_files(&self) -> bool {
        false
    }
}

impl SsrPatternsLinter for UncheckedCastLinter {
    type Context = ();

    fn patterns(&self) -> &'static [(String, Self::Context)] {
        lazy_static! {
            static ref PATTERNS: Vec<(String, ())> =
                vec![("ssr: ?UNCHECKED_CAST(_@Expr, _@Type).".to_string(), ())];
        }
        &PATTERNS
    }

    fn strategy(&self) -> Strategy {
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        }
    }

    fn scope(&self, file_id: FileId) -> SsrSearchScope {
        SsrSearchScope::WholeFile(file_id)
    }
}

pub static UNCHECKED_CAST_LINTER: UncheckedCastLinter = UncheckedCastLinter;

#[cfg(test)]
mod tests {
    use crate::tests::check_diagnostics;

    #[test]
    fn test_eqwalizer_fixme() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-export([test/1]).

-spec test(atom()) -> ok.
test(A) ->
  % eqwalizer:fixme
%%^^^^^^^^^^^^^^^^^ 💡 weak: W0073: The `eqwalizer:fixme` comment suppresses eqwalizer type errors on the following line. Consider fixing the underlying type issue instead of suppressing it.
  _ = A + 1,
  ok.
"#,
        )
    }

    #[test]
    fn test_eqwalizer_ignore() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-export([test/0]).

% eqwalizer:ignore
%%<^^^^^^^^^^^^^^^ 💡 weak: W0074: The `eqwalizer:ignore` comment suppresses eqwalizer type errors on the following line. Consider fixing the underlying type issue instead of suppressing it.
-type loop() :: loop().

-spec test() -> ok.
test() -> ok.
"#,
        )
    }

    #[test]
    fn test_unchecked_cast() {
        check_diagnostics(
            r#"
//- /include/eqwalizer.hrl
-define(UNCHECKED_CAST(Expr, _Type), Expr).

//- /src/main.erl
-module(main).
-include("eqwalizer.hrl").
-export([test/1]).

-spec test(atom()) -> ok.
test(A) ->
  ?UNCHECKED_CAST(A, ok).
%%^^^^^^^^^^^^^^^^^^^^^^ 💡 weak: W0075: The `?UNCHECKED_CAST` macro bypasses eqwalizer type checking by asserting a value has a specific type without verification. Consider using ?CHECKED_CAST as a proper type-safe alternative if possible.
"#,
        )
    }
}
