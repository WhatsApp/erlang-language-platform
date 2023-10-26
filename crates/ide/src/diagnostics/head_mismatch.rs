/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_syntax::ast;
use elp_syntax::ast::AstNode;
use elp_syntax::syntax_node::SyntaxNode;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use text_edit::TextEdit;

use super::DiagnosticCode;
use crate::diagnostics::RelatedInformation;
use crate::fix;
use crate::Diagnostic;

// Diagnostic: head-mismatch (P1700)
//
// Diagnostic for mismatches between the clauses of a function declaration.

pub(crate) fn head_mismatch(
    acc: &mut Vec<Diagnostic>,
    file_id: FileId,
    node: &SyntaxNode,
) -> Option<()> {
    head_mismatch_fundecl(acc, file_id, node);
    head_mismatch_anonymous_fun(acc, file_id, node);
    Some(())
}

pub(crate) fn head_mismatch_fundecl(
    acc: &mut Vec<Diagnostic>,
    file_id: FileId,
    node: &SyntaxNode,
) -> Option<()> {
    let f = ast::FunDecl::cast(node.clone())?;
    let heads: Vec<HeadInfo> = fundecl_heads(f);
    Name {}.validate_fundecl_attr(file_id, &heads, acc);
    Arity {}.validate_fundecl_attr(file_id, &heads, acc);
    Some(())
}

pub(crate) fn head_mismatch_anonymous_fun(
    acc: &mut Vec<Diagnostic>,
    file_id: FileId,
    node: &SyntaxNode,
) -> Option<()> {
    let f = ast::AnonymousFun::cast(node.clone())?;
    let heads: Vec<HeadInfo> = anonymous_fun_heads(f);
    Name {}.validate_fundecl_attr(file_id, &heads, acc);
    Arity {}.validate_fundecl_attr(file_id, &heads, acc);
    Some(())
}

type HeadInfo = (String, TextRange, usize, TextRange);

trait Validate<A>
where
    A: Eq,
    A: Hash,
    A: Clone,
    A: std::fmt::Display,
{
    fn get_attr(self, head: &HeadInfo) -> A;
    fn get_loc(self, head: &HeadInfo) -> TextRange;
    fn make_diagnostic(
        self,
        file_id: FileId,
        attr: &A,
        hattr: &A,
        attr_loc: TextRange,
        ref_loc: TextRange,
    ) -> Diagnostic;

    // Actually does the work
    fn validate_fundecl_attr(
        self,
        file_id: FileId,
        heads: &[HeadInfo],
        errors: &mut Vec<Diagnostic>,
    ) -> Option<()>
    where
        Self: Sized,
        Self: Copy,
    {
        // Find the mismatched arity with the lowest number of locations.

        // 1. Create a map of names to locations
        let mut attrs: FxHashMap<A, Vec<TextRange>> = FxHashMap::default();

        if attrs.len() == 1 {
            // Only one attr in the funDecl, nothing more to be done
            return Some(());
        }

        for head in heads {
            let attr = self.get_attr(head);
            let attr_loc = self.get_loc(head);
            match attrs.get(&attr) {
                None => {
                    attrs.insert(attr.clone(), vec![attr_loc]);
                }
                Some(ranges) => {
                    let mut ranges = ranges.clone();
                    ranges.push(attr_loc);
                    attrs.insert(attr.clone(), ranges);
                }
            }
        }

        // 2. Find the attrs with the highest count.  On a tie, take the
        // one occuring earliest in the file
        let mut highest = None;
        for (attr, locations) in attrs {
            match highest {
                None => highest = Some((attr.clone(), locations.clone())),
                Some((ref _cur_attr, ref cur_highest)) => {
                    if locations.len() == cur_highest.len() {
                        // Keep the one closet to the beginning
                        let mut locs = locations.clone();
                        let mut cur = cur_highest.clone();
                        locs.sort_by_key(|a| a.start());
                        cur.sort_by_key(|a| a.start());
                        if locs[0].start() < cur[0].start() {
                            highest = Some((attr.clone(), locations.clone()));
                        }
                    } else if locations.len() > cur_highest.len() {
                        highest = Some((attr.clone(), locations.clone()));
                    }
                }
            }
        }

        // 3. Report mismatch for all not highest, against earliest
        // occurrence of highest
        let (hattr, hlocs) = highest?;
        let mut hlocs = hlocs;
        hlocs.sort_by_key(|a| a.start());
        let ref_loc = hlocs[0];
        for head in heads {
            let attr = self.get_attr(head);
            let attr_loc = self.get_loc(head);
            if hattr != attr {
                errors.push(self.make_diagnostic(file_id, &attr, &hattr, attr_loc, ref_loc));
            }
        }

        None
    }
}

#[derive(Copy, Clone)]
struct Name {}
#[derive(Copy, Clone)]
struct Arity {}

impl Validate<String> for Name {
    fn get_attr(self, head: &HeadInfo) -> String {
        head.0.clone()
    }

    fn get_loc(self, head: &HeadInfo) -> TextRange {
        head.1
    }

    fn make_diagnostic(
        self,
        file_id: FileId,
        attr: &String,
        hattr: &String,
        attr_loc: TextRange,
        ref_loc: TextRange,
    ) -> Diagnostic {
        let mut edit_builder = TextEdit::builder();
        edit_builder.delete(attr_loc);
        edit_builder.insert(attr_loc.start(), hattr.clone());
        let edit = edit_builder.finish();

        Diagnostic::new(
            super::DiagnosticCode::HeadMismatch,
            format!("head mismatch '{}' vs '{}'", attr, hattr),
            attr_loc,
        )
        .with_related(Some(vec![RelatedInformation {
            range: ref_loc,
            message: "Mismatched clause name".to_string(),
        }]))
        .with_fixes(Some(vec![fix(
            "fix_head_mismatch",
            "Fix head mismatch",
            SourceChange::from_text_edit(file_id, edit),
            attr_loc,
        )]))
    }
}

impl Validate<usize> for Arity {
    fn get_attr(self, head: &HeadInfo) -> usize {
        head.2
    }

    fn get_loc(self, head: &HeadInfo) -> TextRange {
        head.3
    }

    fn make_diagnostic(
        self,
        _file_id: FileId,
        attr: &usize,
        hattr: &usize,
        attr_loc: TextRange,
        ref_loc: TextRange,
    ) -> Diagnostic {
        Diagnostic::new(
            DiagnosticCode::HeadMismatch,
            format!("head arity mismatch {} vs {}", attr, hattr),
            attr_loc,
        )
        .with_related(Some(vec![RelatedInformation {
            range: ref_loc,
            message: "Mismatched clause".to_string(),
        }]))
    }
}

fn fundecl_heads(fun_decl: ast::FunDecl) -> Vec<HeadInfo> {
    fun_decl
        .clauses()
        .flat_map(|clause| match clause {
            ast::FunctionOrMacroClause::FunctionClause(clause) => Some(clause),
            ast::FunctionOrMacroClause::MacroCallExpr(_) => None,
        })
        .flat_map(|clause| {
            let name = match clause.name()? {
                ast::Name::Atom(name) => name,
                ast::Name::MacroCallExpr(_) | ast::Name::Var(_) => return None,
            };
            let clause_name = name.text()?;
            let clause_arity = clause.args()?.args().count();
            Some((
                clause_name,
                name.syntax().text_range(),
                clause_arity,
                clause.syntax().text_range(),
            ))
        })
        .collect()
}

fn anonymous_fun_heads(fun: ast::AnonymousFun) -> Vec<HeadInfo> {
    fun.clauses()
        .flat_map(|clause| {
            let clause_name = match clause.name() {
                Some(n) => n.text().to_string(),
                None => "".to_string(),
            };
            let name_location = match clause.name() {
                Some(n) => n.syntax().text_range(),
                None => clause.syntax().text_range(),
            };
            let clause_arity = clause.args()?.args().count();
            Some((
                clause_name,
                name_location,
                clause_arity,
                clause.syntax().text_range(),
            ))
        })
        .collect()
}

// To run the tests via cargo
// cargo test --package elp_ide --lib
#[cfg(test)]
mod tests {
    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    // The followings tests exercice head_mismatch function indirectly.

    #[test]
    fn test_head_mismatch() {
        check_diagnostics(
            r#"
    -module(main).
    foo(0) -> 1;
    boo(1) -> 2.
 %% ^^^ 💡 error: head mismatch 'boo' vs 'foo'
            "#,
        );
        check_fix(
            r#"
    -module(main).
    foo(0) -> 1;
    bo~o(1) -> 2.
            "#,
            r#"
    -module(main).
    foo(0) -> 1;
    foo(1) -> 2.
            "#,
        );
    }

    #[test]
    fn test_head_no_mismatch() {
        // No head mismatch.
        check_diagnostics(
            r#"
    -module(main).
    foo(0) -> 1;
    foo(1) -> 2.
            "#,
        )
    }

    #[test]
    fn test_head_mismatch_majority() {
        check_diagnostics(
            r#"
    -module(main).
    foo(0) -> 1;
 %% ^^^ 💡 error: head mismatch 'foo' vs 'boo'
    boo(1) -> 2;
    boo(2) -> 3.
            "#,
        );
        check_fix(
            r#"
    -module(main).
    fo~o(0) -> 1;
    boo(1) -> 2;
    boo(2) -> 3.
            "#,
            r#"
    -module(main).
    boo(0) -> 1;
    boo(1) -> 2;
    boo(2) -> 3.
            "#,
        );
    }

    #[test]
    fn test_head_mismatch_arity() {
        check_diagnostics(
            r#"
    -module(main).
    foo(0) -> 1;
    foo(1,0) -> 2.
 %% ^^^^^^^^^^^^^ error: head arity mismatch 2 vs 1
            "#,
        );
    }

    #[test]
    fn test_head_mismatch_arity_majority() {
        check_diagnostics(
            r#"
    -module(main).
    foo(2,0) -> 3;
    foo(0) -> 1;
 %% ^^^^^^^^^^^ error: head arity mismatch 1 vs 2
    foo(1,0) -> 2.
            "#,
        );
    }

    #[test]
    fn test_head_mismatch_quoted_atom() {
        check_diagnostics(
            r#"
   -module(main).
   foo(0) -> 1;
   'foo'(1) -> 2.
            "#,
        );
    }

    #[test]
    fn test_head_mismatch_syntax_error() {
        check_diagnostics(
            r#"
   -module(main).
   foo() ->
       F = fun
           (0) -> ok;
           A(N) -> ok
        %% ^ 💡 error: head mismatch 'A' vs ''
       end,
       F().
            "#,
        );
    }
}
