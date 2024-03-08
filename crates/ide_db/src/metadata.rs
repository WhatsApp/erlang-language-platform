/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::TryInto;

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
    pub eqwalizer_fixmes: Vec<Fixme>,
    pub elp_fixmes: Vec<Fixme>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fixme {
    pub codes: FxHashSet<DiagnosticCode>,
    pub comment: String,
    pub comment_range: TextRange,
    pub suppression_range: TextRange,
    pub is_ignore: bool,
}

impl From<Metadata> for eetf::Term {
    fn from(val: Metadata) -> Self {
        let eqwalizer_fixmes: Vec<eetf::Term> =
            val.eqwalizer_fixmes.into_iter().map(|f| f.into()).collect();
        let elp_fixmes: Vec<eetf::Term> = val.elp_fixmes.into_iter().map(|f| f.into()).collect();
        eetf::List::from(vec![
            eetf::Tuple::from(vec![
                eetf::Atom::from("eqwalizer_fixmes").into(),
                eetf::List::from(eqwalizer_fixmes).into(),
            ])
            .into(),
            eetf::Tuple::from(vec![
                eetf::Atom::from("elp_fixmes").into(),
                eetf::List::from(elp_fixmes).into(),
            ])
            .into(),
        ])
        .into()
    }
}

// serialize as:
// {FixmeCommentStart, FixmeCommentEnd, SuppressionRangeStart, SuppressionRangeEnd, IsIgnore}
impl From<Fixme> for eetf::Term {
    fn from(val: Fixme) -> Self {
        let to_term = |n: TextSize| -> eetf::Term {
            let n: u32 = n.into();
            // eetf::FixInteger holds an i32, which means
            // we can support files with about 2 million LOC
            // otherwise we blow up (calculation based on 1000 chars() per line)
            let n: i32 = n.try_into().unwrap();
            eetf::FixInteger::from(n).into()
        };
        eetf::Tuple::from(vec![
            to_term(val.comment_range.start()),
            to_term(val.comment_range.end()),
            to_term(val.suppression_range.start()),
            to_term(val.suppression_range.end()),
            eetf::Atom {
                name: val.is_ignore.to_string(),
            }
            .into(),
        ])
        .into()
    }
}

pub fn metadata(line_index: &LineIndex, file_text: &str, source: &Parse<SourceFile>) -> Metadata {
    Metadata {
        eqwalizer_fixmes: collect_fixmes(
            line_index,
            file_text,
            source,
            vec![("% eqwalizer:fixme", false), ("% eqwalizer:ignore", true)],
        ),
        elp_fixmes: collect_fixmes(
            line_index,
            file_text,
            source,
            vec![("% elp:fixme", false), ("% elp:ignore", true)],
        ),
    }
}

fn collect_fixmes(
    line_index: &LineIndex,
    file_text: &str,
    source: &Parse<SourceFile>,
    pats: Vec<(&str, bool)>,
) -> Vec<Fixme> {
    let mut fixmes = Vec::new();
    for (pat, is_ignore) in pats {
        let len = pat.len();
        for (i, _) in file_text.match_indices(pat) {
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
                        .filter_map(|word| DiagnosticCode::maybe_from_string(&word.to_string()))
                        .collect();

                    fixmes.push(Fixme {
                        comment,
                        comment_range,
                        suppression_range,
                        codes,
                        is_ignore,
                    });
                }
            }
        }
    }
    fixmes
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
