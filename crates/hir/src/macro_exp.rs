/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_base_db::FileId;
use elp_base_db::salsa::Cycle;
use elp_syntax::ast;

use crate::Define;
use crate::DefineId;
use crate::InFile;
use crate::MacroName;
use crate::ModuleAttribute;
use crate::Name;
use crate::PPDirective;
use crate::body::SSR_SOURCE_FILE_ID;
use crate::db::DefDatabase;
use crate::db::DefDatabaseData;
use crate::form_list::FormListData;
use crate::known;
use crate::name::AsName;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
pub enum BuiltInMacro {
    FILE,
    FUNCTION_NAME,
    FUNCTION_ARITY,
    LINE,
    MODULE,
    MODULE_STRING,
    MACHINE,
    OTP_RELEASE,
}

impl BuiltInMacro {
    pub fn name(&self) -> MacroName {
        let name = match self {
            BuiltInMacro::FILE => known::FILE,
            BuiltInMacro::FUNCTION_NAME => known::FUNCTION_NAME,
            BuiltInMacro::FUNCTION_ARITY => known::FUNCTION_ARITY,
            BuiltInMacro::LINE => known::LINE,
            BuiltInMacro::MODULE => known::MODULE,
            BuiltInMacro::MODULE_STRING => known::MODULE_STRING,
            BuiltInMacro::MACHINE => known::MACHINE,
            BuiltInMacro::OTP_RELEASE => known::OTP_RELEASE,
        };
        MacroName::new(name, None)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ResolvedMacro {
    BuiltIn(BuiltInMacro),
    User(InFile<DefineId>),
}

impl ResolvedMacro {
    pub fn name(&self, db: &dyn DefDatabase) -> MacroName {
        match self {
            ResolvedMacro::BuiltIn(built_in) => built_in.name(),
            ResolvedMacro::User(def) => {
                let form_list = db.file_form_list(def.file_id);
                form_list[def.value].name.clone()
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum MacroResolution {
    Resolved(InFile<DefineId>),
    Undef,
    Unresolved,
}

pub(crate) fn resolve_query(
    db: &dyn DefDatabase,
    file_id: FileId,
    name: MacroName,
) -> Option<ResolvedMacro> {
    if let Some(value) = resolve_built_in(&name) {
        return value.map(ResolvedMacro::BuiltIn);
    }

    match db.local_resolve_macro(file_id, name) {
        MacroResolution::Resolved(resolved) => Some(ResolvedMacro::User(resolved)),
        MacroResolution::Undef => None,
        MacroResolution::Unresolved => None,
    }
}

fn resolve_built_in(name: &MacroName) -> Option<Option<BuiltInMacro>> {
    let built_in = match name.name().as_str() {
        "FILE" => Some(BuiltInMacro::FILE),
        "FUNCTION_NAME" => Some(BuiltInMacro::FUNCTION_NAME),
        "FUNCTION_ARITY" => Some(BuiltInMacro::FUNCTION_ARITY),
        "LINE" => Some(BuiltInMacro::LINE),
        "MODULE" => Some(BuiltInMacro::MODULE),
        "MODULE_STRING" => Some(BuiltInMacro::MODULE_STRING),
        "MACHINE" => Some(BuiltInMacro::MACHINE),
        "OTP_RELEASE" => Some(BuiltInMacro::OTP_RELEASE),
        _ => None,
    };

    match built_in {
        Some(BuiltInMacro::FUNCTION_NAME) => return Some(built_in),
        _ => {
            if built_in.is_some() {
                if name.arity().is_none() {
                    return Some(built_in);
                } else {
                    return Some(None);
                }
            }
        }
    }

    None
}

pub(crate) fn local_resolve_query(
    db: &dyn DefDatabase,
    file_id: FileId,
    name: MacroName,
) -> MacroResolution {
    if file_id == SSR_SOURCE_FILE_ID {
        return MacroResolution::Unresolved;
    }
    let form_list = db.file_form_list(file_id);

    for (_idx, directive) in form_list.pp_stack().iter().rev() {
        match directive {
            PPDirective::Define(idx) => {
                let define = &form_list[*idx];
                if define.name == name {
                    return MacroResolution::Resolved(InFile::new(file_id, *idx));
                }
            }
            PPDirective::Undef {
                name: undefed,
                cond: _,
                form_id: _,
            } if undefed == name.name() => {
                return MacroResolution::Undef;
            }
            PPDirective::Undef { .. } => {}
            PPDirective::Include(idx) => {
                if let Some(resolved) = db.resolve_include(InFile::new(file_id, *idx)) {
                    match db.local_resolve_macro(resolved, name.clone()) {
                        MacroResolution::Resolved(resolved) => {
                            return MacroResolution::Resolved(resolved);
                        }
                        MacroResolution::Undef => return MacroResolution::Undef,
                        MacroResolution::Unresolved => {}
                    }
                }
            }
        }
    }

    MacroResolution::Unresolved
}

// This handles the case of headers accidentally forming cycles during macro resolution.
pub(crate) fn recover_cycle(
    _db: &dyn DefDatabase,
    _cycle: &Cycle,
    _data: DefDatabaseData,
    _file_id: FileId,
    _name: MacroName,
) -> MacroResolution {
    MacroResolution::Unresolved
}

pub struct MacroExpCtx<'a> {
    _db: &'a dyn DefDatabase,
    form_list: &'a FormListData,
}

impl<'a> MacroExpCtx<'a> {
    pub(crate) fn new(form_list: &'a FormListData, db: &'a dyn DefDatabase) -> Self {
        MacroExpCtx { form_list, _db: db }
    }

    pub fn expand_atom(
        &self,
        macro_call: &ast::MacroCallExpr,
        source_file: &ast::SourceFile,
    ) -> Option<Name> {
        if let Some(built_in) = self.expand_built_in(macro_call) {
            return Some(built_in);
        }

        match self.find_replacement(macro_call, source_file)? {
            ast::MacroDefReplacement::Expr(ast::Expr::ExprMax(ast::ExprMax::Atom(atom))) => {
                Some(atom.as_name())
            }
            ast::MacroDefReplacement::Expr(_) => None,
            ast::MacroDefReplacement::ReplacementCrClauses(_) => None,
            ast::MacroDefReplacement::ReplacementFunctionClauses(_) => None,
            ast::MacroDefReplacement::ReplacementGuardAnd(_) => None,
            ast::MacroDefReplacement::ReplacementGuardOr(_) => None,
            ast::MacroDefReplacement::ReplacementExprGuard(_) => None,
            ast::MacroDefReplacement::ReplacementParens(_) => None,
        }
    }

    pub fn expand_built_in(&self, macro_call: &ast::MacroCallExpr) -> Option<Name> {
        let macro_name = macro_name(macro_call)?;
        match resolve_built_in(&macro_name) {
            Some(Some(BuiltInMacro::MODULE)) => {
                let module = self.find_module_attribute()?;
                Some(module.name.clone())
            }
            _ => None,
        }
    }

    pub fn find_module_attribute(&self) -> Option<&ModuleAttribute> {
        let (_idx, module) = self.form_list.module_attribute.iter().next()?;
        Some(module)
    }

    pub fn find_define(&self, macro_call: &ast::MacroCallExpr) -> Option<&Define> {
        let target = macro_name(macro_call)?;

        for (_idx, directive) in self.form_list.pp_directives.iter().rev() {
            // TODO: evaluate conditions
            match directive {
                PPDirective::Define(idx) => {
                    let define = &self.form_list.defines[*idx];
                    if define.name == target {
                        return Some(define);
                    }
                }
                PPDirective::Undef {
                    name,
                    cond: _,
                    form_id: _,
                } if name == target.name() => {
                    // TODO: diagnostic it was explicitly undefed
                    return None;
                }
                PPDirective::Undef { .. } => {}
                // TODO: recurse into includes
                PPDirective::Include { .. } => return None,
            }
        }

        // TODO: diagnostic no definition found
        None
    }

    pub fn find_defines_by_name(&self, name: &ast::MacroName) -> Vec<&Define> {
        let target = name.as_name();
        let mut defines = vec![];

        for (_idx, directive) in self.form_list.pp_directives.iter().rev() {
            // TODO: evaluate conditions
            match directive {
                PPDirective::Define(idx) => {
                    let define = &self.form_list.defines[*idx];
                    if define.name.name() == &target {
                        defines.push(define);
                    }
                }
                PPDirective::Undef { .. } => {}
                // TODO: recurse into includes
                PPDirective::Include { .. } => break,
            }
        }

        defines
    }

    fn find_replacement(
        &self,
        call: &ast::MacroCallExpr,
        source_file: &ast::SourceFile,
    ) -> Option<ast::MacroDefReplacement> {
        self.find_define(call)?
            .form_id
            .get(source_file)
            .replacement()
    }
}

pub fn macro_name(macro_call: &ast::MacroCallExpr) -> Option<MacroName> {
    let name = macro_call.name()?.as_name();
    let arity = macro_call
        .args()
        .and_then(|args| args.args().count().try_into().ok());
    Some(MacroName::new(name, arity))
}

#[cfg(test)]
mod tests {
    use elp_base_db::FileRange;
    use elp_base_db::RootQueryDb;
    use elp_base_db::fixture::ChangeFixture;
    use elp_base_db::fixture::WithFixture;
    use elp_syntax::AstNode;
    use elp_syntax::algo;
    use elp_syntax::ast;

    use super::*;
    use crate::DefineDef;
    use crate::File;
    use crate::test_db::TestDB;

    #[track_caller]
    fn resolve_macro(fixture: &str) -> (Option<ResolvedMacro>, TestDB, ChangeFixture) {
        let (db, fixture) = TestDB::with_fixture(fixture);
        let position = fixture.position();

        let parsed = db.parse(position.file_id);
        assert!(
            parsed.errors().is_empty(),
            "test must not contain parse errors, got: {:?}",
            parsed.errors()
        );

        let macro_call =
            algo::find_node_at_offset::<ast::MacroCallExpr>(&parsed.syntax_node(), position.offset)
                .expect("macro call marked with ~ not found");
        let name = macro_name(&macro_call).unwrap();

        (db.resolve_macro(position.file_id, name), db, fixture)
    }

    #[track_caller]
    fn check_built_in(fixture: &str, expected: BuiltInMacro) {
        let (resolved, _db, _fixture) = resolve_macro(fixture);

        assert_eq!(resolved, Some(ResolvedMacro::BuiltIn(expected)));
    }

    #[track_caller]
    fn check_user(fixture: &str) {
        let (resolved, db, fixture) = resolve_macro(fixture);
        let annos = fixture.annotations();
        assert_eq!(annos.len(), 1);
        let (expected_range, _) = annos[0];

        let resolved = match resolved.expect("failed to resolve macro") {
            ResolvedMacro::BuiltIn(built_in) => panic!(
                "expected to resolve to a custom macro, got {:?} instead",
                built_in
            ),
            ResolvedMacro::User(def) => def,
        };
        let def = DefineDef {
            file: File {
                file_id: resolved.file_id,
            },
            define: db.file_form_list(resolved.file_id)[resolved.value].clone(),
        };
        let found_range = FileRange {
            file_id: resolved.file_id,
            range: def.source(&db).syntax().text_range(),
        };

        assert_eq!(expected_range, found_range);
    }

    #[test]
    fn test_line() {
        check_built_in(
            r#"
-define(LINE, ignored).
bar() -> ?~LINE.
"#,
            BuiltInMacro::LINE,
        );
    }

    #[test]
    fn test_line_paren() {
        let (resolved, _db, _fixture) = resolve_macro(
            r#"
-define(LINE, ignored).
bar() -> ?~LINE().
"#,
        );
        assert_eq!(resolved, None);
    }

    #[test]
    fn test_file() {
        check_built_in(
            r#"
-define(FILE, ignored).
bar() -> ?~FILE.
"#,
            BuiltInMacro::FILE,
        );
    }

    #[test]
    fn test_function_name() {
        check_built_in(
            r#"
-define(FUNCTION_NAME, ignored).
bar() -> ?~FUNCTION_NAME.
"#,
            BuiltInMacro::FUNCTION_NAME,
        );
    }

    #[test]
    fn test_function_arity() {
        check_built_in(
            r#"
-define(FUNCTION_ARITY, ignored).
bar() -> ?~FUNCTION_ARITY.
"#,
            BuiltInMacro::FUNCTION_ARITY,
        );
    }

    #[test]
    fn test_module() {
        check_built_in(
            r#"
-define(MODULE, ignored).
bar() -> ?~MODULE.
"#,
            BuiltInMacro::MODULE,
        );
    }

    #[test]
    fn test_module_string() {
        check_built_in(
            r#"
-define(MODULE_STRING, ignored).
bar() -> ?~MODULE_STRING.
"#,
            BuiltInMacro::MODULE_STRING,
        );
    }

    #[test]
    fn test_machine() {
        check_built_in(
            r#"
-define(MACHINE, ignored).
bar() -> ?~MACHINE.
"#,
            BuiltInMacro::MACHINE,
        );
    }

    #[test]
    fn test_otp_release() {
        check_built_in(
            r#"
-define(OTP_RELEASE, ignored).
bar() -> ?~OTP_RELEASE.
"#,
            BuiltInMacro::OTP_RELEASE,
        );
    }

    #[test]
    fn test_resolve_in_include() {
        check_user(
            r#"
//- /src/include.hrl
-define(MACRO, wrong).
-define(MACRO(_), wrong).
   -define(MACRO(), right).
%% ^^^^^^^^^^^^^^^^^^^^^^^^

//- /src/main.erl
-module(main).

-include("include.hrl").

foo() -> ?~MACRO().
"#,
        );
    }

    #[test]
    fn test_resolve_with_undef() {
        check_user(
            r#"
-define(MACRO, wrong).
-undef(MACRO).
   -define(MACRO, right).
%% ^^^^^^^^^^^^^^^^^^^^^^

foo() -> ?~MACRO.
"#,
        );
    }

    #[test]
    fn test_recursive_fails_cleanly() {
        let (resolved, _db, _fixture) = resolve_macro(
            r#"
//- /src/include.hrl
-define(FOO, _).
-include("include.hrl").

//- /src/main.erl
-module(main).
-include("include.hrl").
foo() -> ?~FOO.
"#,
        );
        assert_eq!(resolved, None);
    }

    #[test]
    fn test_expression_with_guard() {
        check_user(
            r#"
   -define(MACRO, Error when Error == oops).
%% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

foo(X) ->
   case X of
     ?~MACRO -> ok
   end.
"#,
        );
    }
}
