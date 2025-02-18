/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_base_db::FileId;
use elp_base_db::SourceDatabase;
use elp_syntax::ast;

mod body;
pub mod db;
mod def_map;
mod diagnostics;
pub mod edoc;
mod expr;
pub mod fold;
pub mod form_list;
mod include;
mod intern;
mod macro_exp;
mod module_data;
mod name;
pub mod resolver;
pub mod sema;
#[cfg(test)]
mod test_db;

pub use body::AnyAttribute;
pub use body::AttributeBody;
pub use body::Body;
pub use body::BodyOrigin;
pub use body::BodySourceMap;
pub use body::DefineBody;
pub use body::ExprSource;
pub use body::FoldBody;
pub use body::FunctionBody;
pub use body::FunctionClauseBody;
pub use body::InFileAstPtr;
pub use body::MacroSource;
pub use body::RecordBody;
pub use body::SpecBody;
pub use body::SpecOrCallback;
pub use body::SsrBody;
pub use body::SsrPatternId;
pub use body::SsrPatternIds;
pub use body::TypeBody;
pub use def_map::DefMap;
pub use def_map::FunctionDefId;
pub use diagnostics::Diagnostic;
pub use diagnostics::DiagnosticMessage;
pub use expr::AnyExpr;
pub use expr::AnyExprId;
pub use expr::AnyExprRef;
pub use expr::BasedInteger;
pub use expr::BinarySeg;
pub use expr::CRClause;
pub use expr::CallTarget;
pub use expr::CatchClause;
pub use expr::Clause;
pub use expr::ClauseId;
pub use expr::ComprehensionBuilder;
pub use expr::ComprehensionExpr;
pub use expr::Expr;
pub use expr::ExprId;
pub use expr::FunType;
pub use expr::IfClause;
pub use expr::ListType;
pub use expr::Literal;
pub use expr::MacroCallName;
pub use expr::MapOp;
pub use expr::MaybeExpr;
pub use expr::Pat;
pub use expr::PatId;
pub use expr::ReceiveAfter;
pub use expr::RecordFieldBody;
pub use expr::SpecSig;
pub use expr::SsrPlaceholder;
pub use expr::Term;
pub use expr::TermId;
pub use expr::TypeExpr;
pub use expr::TypeExprId;
pub use fold::FoldCtx;
pub use fold::On;
pub use fold::Strategy;
pub use form_list::Attribute;
pub use form_list::AttributeId;
pub use form_list::Behaviour;
pub use form_list::BehaviourId;
pub use form_list::Callback;
pub use form_list::CallbackId;
pub use form_list::CompileOption;
pub use form_list::CompileOptionId;
pub use form_list::Define;
pub use form_list::DefineId;
pub use form_list::Export;
pub use form_list::ExportId;
pub use form_list::FaEntry;
pub use form_list::FaEntryId;
pub use form_list::FormId;
pub use form_list::FormIdx;
pub use form_list::FormList;
pub use form_list::FunctionClause;
pub use form_list::FunctionClauseId;
pub use form_list::Import;
pub use form_list::ImportId;
pub use form_list::IncludeAttribute;
pub use form_list::IncludeAttributeId;
pub use form_list::ModuleAttribute;
pub use form_list::ModuleAttributeId;
pub use form_list::OptionalCallbacks;
pub use form_list::OptionalCallbacksId;
pub use form_list::PPCondition;
pub use form_list::PPConditionId;
pub use form_list::PPDirective;
pub use form_list::PPDirectiveId;
pub use form_list::ParamName;
pub use form_list::Record;
pub use form_list::RecordField;
pub use form_list::RecordFieldId;
pub use form_list::RecordId;
pub use form_list::Spec;
pub use form_list::SpecId;
pub use form_list::TypeAlias;
pub use form_list::TypeAliasId;
pub use form_list::TypeExport;
pub use form_list::TypeExportId;
pub use intern::Atom;
pub use intern::SsrSource;
pub use intern::Var;
pub use macro_exp::ResolvedMacro;
pub use module_data::CallbackDef;
pub use module_data::DefineDef;
pub use module_data::File;
pub use module_data::FunctionClauseDef;
pub use module_data::FunctionDef;
pub use module_data::Module;
pub use module_data::RecordDef;
pub use module_data::RecordFieldDef;
pub use module_data::SpecArgName;
pub use module_data::SpecDef;
pub use module_data::TypeAliasDef;
pub use module_data::TypeAliasSource;
pub use module_data::VarDef;
pub use name::known;
// @fb-only
pub use name::AsName;
pub use name::MacroName;
pub use name::Name;
pub use name::NameArity;
pub use sema::AtomDef;
pub use sema::CallDef;
pub use sema::DefinitionOrReference;
pub use sema::FaDef;
pub use sema::InFunctionBody;
pub use sema::InFunctionClauseBody;
pub use sema::MacroCallDef;
pub use sema::ScopeAnalysis;
pub use sema::Semantic;

/// `InFile<T>` stores a value of `T` inside a particular file.
///
/// Typical usages are:
///
/// * `InFile<SyntaxNode>` -- syntax node in a file
/// * `InFile<ast::FnDef>` -- ast node in a file
/// * `InFile<TextSize>` -- offset in a file
/// * `InFile<IncludeAttributeId>` -- `-include` in a file
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct InFile<T> {
    pub file_id: FileId,
    pub value: T,
}

impl<T> InFile<T> {
    pub fn new(file_id: FileId, value: T) -> InFile<T> {
        InFile { file_id, value }
    }

    pub fn with_value<U>(&self, value: U) -> InFile<U> {
        InFile::new(self.file_id, value)
    }

    pub fn map<F: FnOnce(T) -> U, U>(self, f: F) -> InFile<U> {
        InFile::new(self.file_id, f(self.value))
    }

    pub fn as_ref(&self) -> InFile<&T> {
        self.with_value(&self.value)
    }

    pub fn file_syntax(&self, db: &dyn SourceDatabase) -> ast::SourceFile {
        db.parse(self.file_id).tree()
    }
}

impl<T: Clone> InFile<&T> {
    pub fn cloned(&self) -> InFile<T> {
        self.with_value(self.value.clone())
    }
}

impl<T> InFile<Option<T>> {
    pub fn transpose(self) -> Option<InFile<T>> {
        self.value.map(|value| InFile::new(self.file_id, value))
    }
}

// ---------------------------------------------------------------------

/// HIR index.  Uniquely identifies any specific HIR item in a file
/// file. Use globally as `InFile<HirIdx>`.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct HirIdx {
    pub body_origin: BodyOrigin,
    pub idx: AnyExprId,
}

impl HirIdx {
    pub fn as_expr_id(&self) -> Option<ExprId> {
        match self.idx {
            AnyExprId::Expr(id) => Some(id),
            _ => None,
        }
    }

    /// This function is used to print a representation of the HIR AST
    /// corresponding to the given `HirIdx`.  It is used for debugging
    /// and testing.
    pub fn tree_print(&self, sema: &Semantic) -> String {
        match self.body_origin {
            BodyOrigin::Invalid(_) => "BodyOrigin::Invalid".to_string(),
            BodyOrigin::FormIdx { file_id, form_id } => match form_id {
                FormIdx::ModuleAttribute(_) => todo!(),
                FormIdx::FunctionClause(fun_idx) => {
                    let body = sema.db.function_clause_body(InFile::new(file_id, fun_idx));
                    body.body.tree_print_any_expr(sema.db.upcast(), self.idx)
                }
                _ => format!(
                    "HirIdx::tree_print not implemented for FormIdx '{:?}'",
                    form_id
                )
                .to_string(),
            },
            BodyOrigin::Define {
                file_id: _,
                define_id: _,
            } => format!(
                "HirIdx::tree_print not implemented for BodyOrigin::Define '{:?}'",
                self.body_origin
            )
            .to_string(),
        }
    }
}

// ---------------------------------------------------------------------
/// `InSsr<T>` stores a value of `T` inside a particular Ssr definition.
/// Based on `InFile`
///
/// Typical usages are:
///
/// * `InSsr<SsrId>` -- node in a ssr file
/// * `InSsr<ast::FnDef>` -- ast node in a ssr file
/// * `InSsr<TextSize>` -- offset in a ssr file
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct InSsr<T> {
    pub ssr_source: SsrSource,
    pub value: T,
}

impl<T> InSsr<T> {
    pub fn new(ssr_source: SsrSource, value: T) -> InSsr<T> {
        InSsr { ssr_source, value }
    }

    pub fn with_value<U>(&self, value: U) -> InSsr<U> {
        InSsr::new(self.ssr_source, value)
    }

    pub fn map<F: FnOnce(T) -> U, U>(self, f: F) -> InSsr<U> {
        InSsr::new(self.ssr_source, f(self.value))
    }

    pub fn as_ref(&self) -> InSsr<&T> {
        self.with_value(&self.value)
    }
}

impl<T: Clone> InSsr<&T> {
    pub fn cloned(&self) -> InSsr<T> {
        self.with_value(self.value.clone())
    }
}

impl<T> InSsr<Option<T>> {
    pub fn transpose(self) -> Option<InSsr<T>> {
        self.value.map(|value| InSsr::new(self.ssr_source, value))
    }
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use expect_test::expect;
    use la_arena::Idx;
    use la_arena::RawIdx;

    use crate::expr::ClauseId;
    use crate::test_db::TestDB;
    use crate::AnyExprId;
    use crate::FunctionBody;
    use crate::FunctionDefId;
    use crate::HirIdx;
    use crate::InFile;
    use crate::Semantic;

    #[test]
    fn print_fun_expr() {
        let fixture_str = r#"
              bar() ->
                begin
                  A = B + 3,
                  [A|A],
                  Y = ~A,
                  catch A,
                  begin
                    A,
                    Y = 6
                  end,
                  A
                end.
              "#;

        let (db, file_id, _range_or_offset) = TestDB::with_range_or_offset(fixture_str);
        let sema = Semantic::new(&db);
        let function_def_id: FunctionDefId = FunctionDefId::new(Idx::from_raw(RawIdx::from(0)));

        let (body, _body_map) = FunctionBody::function_body_with_source_query(
            &db,
            InFile {
                file_id,
                value: function_def_id,
            },
        );

        let idx = ClauseId::from_raw(RawIdx::from(0));
        let clause = &body[idx];
        let expr_id = AnyExprId::Expr(clause.clause.exprs[0]);
        let hir_idx = HirIdx {
            body_origin: clause.body.origin,
            idx: expr_id,
        };

        expect![[r#"

            Expr<17>:Expr::Block {
                Expr<4>:Expr::Match {
                    lhs
                        Pat<0>:Pat::Var(A)
                    rhs
                        Expr<3>:Expr::BinaryOp {
                            lhs
                                Expr<1>:Expr::Var(B)
                            rhs
                                Expr<2>:Literal(Integer(3))
                            op
                                ArithOp(Add),
                        }
                },
                Expr<7>:Expr::List {
                    exprs
                        Expr<5>:Expr::Var(A),
                    tail
                        Expr<6>:Expr::Var(A),
                },
                Expr<9>:Expr::Match {
                    lhs
                        Pat<1>:Pat::Var(Y)
                    rhs
                        Expr<8>:Expr::Var(A)
                },
                Expr<11>:Expr::Catch {
                    expr
                        Expr<10>:Expr::Var(A)
                },
                Expr<15>:Expr::Block {
                    Expr<12>:Expr::Var(A),
                    Expr<14>:Expr::Match {
                        lhs
                            Pat<2>:Pat::Var(Y)
                        rhs
                            Expr<13>:Literal(Integer(6))
                    },
                },
                Expr<16>:Expr::Var(A),
            }
        "#]]
        .assert_eq(&hir_idx.tree_print(&sema));
    }
}
