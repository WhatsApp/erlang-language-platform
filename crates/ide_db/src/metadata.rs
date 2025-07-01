/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::convert::TryInto;
use std::fmt;

use elp_syntax::Parse;
use elp_syntax::SourceFile;
use elp_syntax::SyntaxKind;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use fxhash::FxHashSet;

use crate::DiagnosticCode;
use crate::LineIndex;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Metadata {
    pub annotations: Vec<Annotation>,
}

impl Metadata {
    pub fn by_source(&self, source: Source) -> impl Iterator<Item = &Annotation> + '_ {
        self.annotations
            .iter()
            .filter(move |ann| ann.source == source)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Pattern {
    source: Source,
    kind: Kind,
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "% {}:{}", self.source, self.kind)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Source {
    Eqwalizer,
    Elp,
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Source::Eqwalizer => write!(f, "eqwalizer"),
            Source::Elp => write!(f, "elp"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Ignore,
    Fixme,
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Kind::Ignore => write!(f, "ignore"),
            Kind::Fixme => write!(f, "fixme"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    pub source: Source,
    pub kind: Kind,
    pub comment: String,
    pub comment_range: TextRange,
    pub suppression_range: TextRange,
    pub codes: FxHashSet<DiagnosticCode>,
}

impl From<Metadata> for eetf::Term {
    fn from(val: Metadata) -> Self {
        let eqwalizer_annotations: Vec<eetf::Term> =
            val.by_source(Source::Eqwalizer).map(|f| f.into()).collect();
        eetf::List::from(vec![
            eetf::Tuple::from(vec![
                eetf::Atom::from("eqwalizer_fixmes").into(),
                eetf::List::from(eqwalizer_annotations).into(),
            ])
            .into(),
        ])
        .into()
    }
}

// serialize as:
// {FixmeCommentStart, FixmeCommentEnd, SuppressionRangeStart, SuppressionRangeEnd, IsIgnore}
impl From<&Annotation> for eetf::Term {
    fn from(val: &Annotation) -> Self {
        let to_term = |n: TextSize| -> eetf::Term {
            let n: u32 = n.into();
            // eetf::FixInteger holds an i32, which means
            // we can support files with about 2 million LOC
            // otherwise we blow up (calculation based on 1000 chars() per line)
            let n: i32 = n.try_into().unwrap();
            eetf::FixInteger::from(n).into()
        };
        let is_ignore = val.kind == Kind::Ignore;
        eetf::Tuple::from(vec![
            to_term(val.comment_range.start()),
            to_term(val.comment_range.end()),
            to_term(val.suppression_range.start()),
            to_term(val.suppression_range.end()),
            eetf::Atom {
                name: is_ignore.to_string(),
            }
            .into(),
        ])
        .into()
    }
}

pub fn collect_metadata(
    line_index: &LineIndex,
    file_text: &str,
    source: &Parse<SourceFile>,
) -> Metadata {
    let patterns = vec![
        Pattern {
            source: Source::Eqwalizer,
            kind: Kind::Ignore,
        },
        Pattern {
            source: Source::Eqwalizer,
            kind: Kind::Fixme,
        },
        Pattern {
            source: Source::Elp,
            kind: Kind::Ignore,
        },
        Pattern {
            source: Source::Elp,
            kind: Kind::Fixme,
        },
    ];
    let mut annotations = Vec::new();
    for pattern in patterns {
        let pattern_string = pattern.to_string();
        let len = pattern_string.len();
        for (i, _) in file_text.match_indices(&pattern_string) {
            let pattern_start = TextSize::from(i as u32);
            let pattern_end = TextSize::from((i + len) as u32);
            let line_num = line_index.line_col(pattern_start).line;
            if let Some(token) = source
                .syntax_node()
                .token_at_offset(pattern_end)
                .left_biased()
            {
                if token.kind() == SyntaxKind::COMMENT {
                    let suppression_range = get_suppression_range(line_index, line_num, file_text);
                    let comment = token.to_string();
                    let comment_range = TextRange::new(pattern_start, pattern_end);
                    let codes = comment
                        .split_whitespace()
                        .filter_map(DiagnosticCode::maybe_from_string)
                        .collect();

                    annotations.push(Annotation {
                        comment,
                        comment_range,
                        suppression_range,
                        codes,
                        source: pattern.source,
                        kind: pattern.kind,
                    });
                }
            }
        }
    }
    Metadata { annotations }
}

fn line_start(line_index: &LineIndex, line_num: u32, text: &str) -> TextSize {
    line_index.line_at(line_num as usize).unwrap_or_else(
        // end of last line
        || TextSize::from(text.chars().count() as u32),
    )
}

fn get_suppression_range(line_index: &LineIndex, line_num: u32, text: &str) -> TextRange {
    let start = line_start(line_index, line_num + 1, text);
    let end = line_start(line_index, line_num + 2, text);
    TextRange::new(start, end)
}
