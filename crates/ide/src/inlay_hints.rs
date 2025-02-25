/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::{self};

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::RootDatabase;
use elp_syntax::TextRange;
use hir::Semantic;
use itertools::Itertools;
use smallvec::smallvec;
use smallvec::SmallVec;
mod param_name;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InlayHintsConfig {
    pub parameter_hints: bool,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InlayKind {
    Parameter,
}

#[derive(Debug)]
pub struct InlayHint {
    /// The text range this inlay hint applies to.
    pub range: TextRange,
    /// The kind of this inlay hint. This is used to determine side and padding of the hint for
    /// rendering purposes.
    pub kind: InlayKind,
    /// The actual label to show in the inlay hint.
    pub label: InlayHintLabel,
}

#[derive(Debug)]
pub enum InlayTooltip {
    String(String),
    Markdown(String),
}

#[derive(Default)]
pub struct InlayHintLabel {
    pub parts: SmallVec<[InlayHintLabelPart; 1]>,
}

impl InlayHintLabel {
    pub fn simple(
        s: impl Into<String>,
        tooltip: Option<InlayTooltip>,
        linked_location: Option<FileRange>,
    ) -> InlayHintLabel {
        InlayHintLabel {
            parts: smallvec![InlayHintLabelPart {
                text: s.into(),
                linked_location,
                tooltip,
            }],
        }
    }

    pub fn prepend_str(&mut self, s: &str) {
        match &mut *self.parts {
            [
                InlayHintLabelPart {
                    text,
                    linked_location: None,
                    tooltip: None,
                },
                ..,
            ] => text.insert_str(0, s),
            _ => self.parts.insert(
                0,
                InlayHintLabelPart {
                    text: s.into(),
                    linked_location: None,
                    tooltip: None,
                },
            ),
        }
    }

    pub fn append_str(&mut self, s: &str) {
        match &mut *self.parts {
            [
                ..,
                InlayHintLabelPart {
                    text,
                    linked_location: None,
                    tooltip: None,
                },
            ] => text.push_str(s),
            _ => self.parts.push(InlayHintLabelPart {
                text: s.into(),
                linked_location: None,
                tooltip: None,
            }),
        }
    }
}

impl From<String> for InlayHintLabel {
    fn from(s: String) -> Self {
        Self {
            parts: smallvec![InlayHintLabelPart {
                text: s,
                linked_location: None,
                tooltip: None,
            }],
        }
    }
}

impl From<&str> for InlayHintLabel {
    fn from(s: &str) -> Self {
        Self {
            parts: smallvec![InlayHintLabelPart {
                text: s.into(),
                linked_location: None,
                tooltip: None,
            }],
        }
    }
}

impl fmt::Display for InlayHintLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.parts.iter().map(|part| &part.text).format(""))
    }
}

impl fmt::Debug for InlayHintLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(&self.parts).finish()
    }
}

pub struct InlayHintLabelPart {
    pub text: String,
    /// Source location represented by this label part. The client will use this to fetch the part's
    /// hover tooltip, and Ctrl+Clicking the label part will navigate to the definition the location
    /// refers to (not necessarily the location itself).
    /// When setting this, no tooltip must be set on the containing hint, or VS Code will display
    /// them both.
    pub linked_location: Option<FileRange>,
    /// The tooltip to show when hovering over the inlay hint, this may invoke other actions like
    /// hover requests to show.
    pub tooltip: Option<InlayTooltip>,
}

impl fmt::Debug for InlayHintLabelPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self {
                text,
                linked_location: None,
                tooltip: None,
            } => text.fmt(f),
            Self {
                text,
                linked_location,
                tooltip,
            } => f
                .debug_struct("InlayHintLabelPart")
                .field("text", text)
                .field("linked_location", linked_location)
                .field(
                    "tooltip",
                    &tooltip.as_ref().map_or("", |it| match it {
                        InlayTooltip::String(it) | InlayTooltip::Markdown(it) => it,
                    }),
                )
                .finish(),
        }
    }
}

// Feature: Inlay Hints
//
// ELP shows additional information inline with the source code.
// Editors usually render this using read-only virtual text snippets interspersed with code.
//
// Available hints are:
//
// * names of function arguments
pub(crate) fn inlay_hints(
    db: &RootDatabase,
    file_id: FileId,
    range_limit: Option<TextRange>,
    config: &InlayHintsConfig,
) -> Vec<InlayHint> {
    let _p = tracing::info_span!("inlay_hints").entered();
    let sema = Semantic::new(db);

    let mut acc = Vec::new();

    param_name::hints(&mut acc, &sema, config, file_id, range_limit);

    acc
}

#[cfg(test)]
mod tests {
    use elp_ide_db::elp_base_db::extract_annotations;
    use itertools::Itertools;

    use crate::fixture;
    use crate::inlay_hints::InlayHintsConfig;

    pub(super) const DISABLED_CONFIG: InlayHintsConfig = InlayHintsConfig {
        parameter_hints: false,
    };

    #[track_caller]
    pub(super) fn check_with_config(config: InlayHintsConfig, fixture: &str) {
        let (analysis, pos, _) = fixture::position(fixture);
        let mut expected = extract_annotations(&analysis.file_text(pos.file_id).unwrap());
        let inlay_hints = analysis.inlay_hints(&config, pos.file_id, None).unwrap();
        let actual = inlay_hints
            .into_iter()
            .map(|it| (it.range, it.label.to_string()))
            .sorted_by_key(|(range, _)| range.start())
            .collect::<Vec<_>>();
        expected.sort_by_key(|(range, _)| range.start());

        assert_eq!(
            expected, actual,
            "\nExpected:\n{expected:#?}\n\nActual:\n{actual:#?}"
        );
    }

    #[test]
    fn hints_disabled() {
        check_with_config(
            InlayHintsConfig { ..DISABLED_CONFIG },
            r#"
-module(main).~
sum(A, B) -> A + B.
main() -> _X = sum(1, 2).
"#,
        );
    }
}
