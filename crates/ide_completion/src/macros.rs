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
use elp_base_db::IncludeCtx;
use elp_base_db::SourceDatabase;
use elp_base_db::VfsPath;
use elp_base_db::path_for_file;
use elp_syntax::AstNode;
use elp_syntax::algo;
use elp_syntax::ast;
use fxhash::FxHashSet;
use hir::DefineId;
use hir::InFile;
use hir::MacroName;
use hir::Name;
use hir::Semantic;
use hir::known;

use crate::Completion;
use crate::Contents;
use crate::Ctx;
use crate::DoneFlag;
use crate::IncludeFile;
use crate::Kind;
use crate::helpers;

pub(crate) fn add_completions(
    acc: &mut Vec<Completion>,
    Ctx {
        file_position,
        parsed,
        sema,
        trigger,
        ..
    }: &Ctx,
) -> DoneFlag {
    match trigger {
        Some('?') | None => (),
        _ => return false,
    };

    let node = parsed.value.syntax();
    match algo::find_node_at_offset::<ast::MacroCallExpr>(node, file_position.offset) {
        None => false,
        Some(call) => {
            let prefix =
                match algo::find_node_at_offset::<ast::MacroName>(node, file_position.offset) {
                    Some(prefix) => prefix.text(),
                    None => call.name().map(|n| n.to_string()),
                }
                .unwrap_or_default();
            let def_map = sema.def_map(file_position.file_id);
            let user_defined = def_map
                .get_macros()
                .keys()
                .filter(|macro_name| macro_name.name().starts_with(&prefix))
                .map(|name| {
                    macro_name_to_completion(sema, file_position.file_id, name, None, false)
                });

            let mut known_macros = macro_index_completion(sema, file_position.file_id, &prefix);
            // `known_macros` is a set, make sure we have no duplicates
            // with `user_defined` ones The user-defined take
            // precedence, so we will not insert an include if it is
            // already visible.
            known_macros.extend(user_defined);
            acc.extend(known_macros);

            let built_in = BUILT_IN;
            let predefined = built_in
                .iter()
                .filter(|name| name.starts_with(&prefix))
                .map(built_in_macro_name_to_completion);
            acc.extend(predefined);

            // If we have a trigger character, it means we are completing a macro name. No need to compute other completions.
            trigger.is_some()
        }
    }
}

fn macro_index_completion(sema: &Semantic, file_id: FileId, prefix: &str) -> FxHashSet<Completion> {
    if let Some(project_id) = sema.db.file_project_id(file_id) {
        let index = sema.macro_define_index(project_id);
        index
            .complete(prefix)
            .iter()
            .flat_map(|(_chars, defines)| {
                let with_app = defines.len() > 1;
                defines
                    .iter()
                    .map(move |define| macro_define_as_completion(sema, file_id, define, with_app))
            })
            .flatten()
            .collect()
    } else {
        FxHashSet::default()
    }
}

fn macro_define_as_completion(
    sema: &Semantic<'_>,
    file_id: FileId,
    define: &InFile<DefineId>,
    with_app: bool,
) -> Option<Completion> {
    let include_path = path_for_file(sema.db, define.file_id)?;
    let include = get_include_file(sema.db, file_id, define.file_id, include_path.clone())?;
    let form_list = sema.form_list(define.file_id);
    let define = &form_list[define.value];
    Some(macro_name_to_completion(
        sema,
        file_id,
        &define.name,
        Some(include),
        with_app,
    ))
}

fn macro_name_to_completion(
    sema: &Semantic,
    file_id: FileId,
    macro_name: &MacroName,
    include: Option<IncludeFile>,
    with_app: bool,
) -> Completion {
    let (additional_edit, app_name) = if let Some(inc) = include {
        (
            inc.insert_position_if_needed(sema, file_id)
                .map(|pos| (pos, inc.clone())),
            if with_app { Some(inc.app_name) } else { None },
        )
    } else {
        (None, None)
    };
    let label = if let Some(app_name) = app_name {
        format!("{} ({})", macro_name, app_name)
    } else {
        macro_name.to_string()
    };
    match macro_name.arity() {
        Some(arity) => {
            let contents = helpers::format_call(macro_name.name(), arity);
            Completion {
                label,
                kind: Kind::Macro,
                contents,
                position: None,
                sort_text: None,
                deprecated: false,
                additional_edit,
            }
        }
        None => Completion {
            label,
            kind: Kind::Macro,
            contents: Contents::SameAsLabel,
            position: None,
            sort_text: None,
            deprecated: false,
            additional_edit,
        },
    }
}

fn built_in_macro_name_to_completion(name: &Name) -> Completion {
    Completion {
        label: name.to_string(),
        kind: Kind::Macro,
        contents: Contents::SameAsLabel,
        position: None,
        sort_text: None,
        deprecated: false,
        additional_edit: None,
    }
}

const BUILT_IN: [Name; 8] = [
    known::FILE,
    known::FUNCTION_NAME,
    known::FUNCTION_ARITY,
    known::LINE,
    known::MODULE,
    known::MODULE_STRING,
    known::MACHINE,
    known::OTP_RELEASE,
];

// TODO: should this be done in the resolve step? Or, cached in the table
// First make it work.
pub fn get_include_file(
    db: &dyn SourceDatabase,
    file_id: FileId,
    included_file_id: FileId,
    include_path: VfsPath,
) -> Option<IncludeFile> {
    // This function is the inverse of `base_db::include::resolve_remote_query`.
    // If the result is passed to that function, it should return `included_file_id`.
    let include_path = include_path.as_path()?;

    let inc_app_data = db.file_app_data(included_file_id)?;
    let candidate_path = include_path.strip_prefix(inc_app_data.dir.as_path())?;

    // Validate that the path is in the local include list for the app
    if !inc_app_data
        .include_dirs
        .iter()
        .any(|dir| include_path.strip_prefix(dir).is_some())
    {
        return None;
    }

    let candidate = format!("{}/{}", inc_app_data.name, candidate_path.as_str());
    // Check that it is valid
    let resolved_file_id = IncludeCtx::new(db, file_id).resolve_include_lib(&candidate)?;
    if resolved_file_id == included_file_id {
        // We have an equivalent include
        Some(IncludeFile {
            include_lib: true,
            path: candidate,
            app_name: inc_app_data.name.to_string(),
        })
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use expect_test::Expect;
    use expect_test::expect;

    use crate::tests::get_completions;
    use crate::tests::render_completions;

    fn check(code: &str, trigger: Option<char>, expect: Expect) {
        let completions = get_completions(code, trigger);
        let actual = &render_completions(completions);
        expect.assert_eq(actual);
    }

    #[test]
    fn test_user_defined_macros() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");

        check(
            r#"
    -module(sample1).
    -define(FOO, 1).
    -define(FOO(), 1).
    -define(FOO(X, Y, Z), {X, Y, Z}).
    -define(FOB, 1).
    -define(BAR, 1).
    foo() -> ?FO~
    "#,
            Some('?'),
            expect![[r#"
                {label:FOB, kind:Macro, contents:SameAsLabel, position:None}
                {label:FOO, kind:Macro, contents:SameAsLabel, position:None}
                {label:FOO/0, kind:Macro, contents:Snippet("FOO()"), position:None}
                {label:FOO/3, kind:Macro, contents:Snippet("FOO(${1:Arg1}, ${2:Arg2}, ${3:Arg3})"), position:None}"#]],
        );

        check(
            r#"
    -module(sample1).
    -define(FOO, 1).
    -define(BAR, 1).
    foo() -> ?FO~
    "#,
            None,
            expect!["{label:FOO, kind:Macro, contents:SameAsLabel, position:None}"],
        );

        check(
            r#"
    -module(sample1).
    -define(FOO, ok).
    -define(BAR, 1).
    spec foo() -> ?FO~.
    foo() -> ok.
    "#,
            None,
            expect!["{label:FOO, kind:Macro, contents:SameAsLabel, position:None}"],
        );
    }

    #[test]
    fn test_predefined_macros() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");
        check(
            r#"
    -module(sample1).
    -define(F_USER_DEFINED, 1).
    foo() -> ?F~
    "#,
            Some('?'),
            expect![[r#"
                {label:FILE, kind:Macro, contents:SameAsLabel, position:None}
                {label:FUNCTION_ARITY, kind:Macro, contents:SameAsLabel, position:None}
                {label:FUNCTION_NAME, kind:Macro, contents:SameAsLabel, position:None}
                {label:F_USER_DEFINED, kind:Macro, contents:SameAsLabel, position:None}"#]],
        );

        check(
            r#"
    -module(sample1).
    foo() -> ?M~
    "#,
            Some('?'),
            expect![[r#"
                {label:MACHINE, kind:Macro, contents:SameAsLabel, position:None}
                {label:MODULE, kind:Macro, contents:SameAsLabel, position:None}
                {label:MODULE_STRING, kind:Macro, contents:SameAsLabel, position:None}"#]],
        );

        check(
            r#"
    -module(sample1).
    foo() -> ?L~
    "#,
            Some('?'),
            expect!["{label:LINE, kind:Macro, contents:SameAsLabel, position:None}"],
        );

        check(
            r#"
    -module(sample1).
    foo() -> ?O~
    "#,
            Some('?'),
            expect!["{label:OTP_RELEASE, kind:Macro, contents:SameAsLabel, position:None}"],
        );
    }

    // -----------------------------------------------------------------

    #[test]
    fn well_known_macros_import() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");
        check(
            r#"
            //- /app/src/sample1.erl app:app
            -module(sample1).
            foo() -> ?asse~
            //- /another-app/include/inc.hrl app:another include_path:/another-app/include
            -define(assertEqual(A,B), A =:= B).
    "#,
            Some('?'),
            expect![[
                r#"{label:assertEqual/2, kind:Macro, contents:Snippet("assertEqual(${1:Arg1}, ${2:Arg2})"), position:None, include:18:"-include_lib(\"another/include/inc.hrl\")."}"#
            ]],
        );
    }

    #[test]
    fn well_known_macros_no_args_import() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");
        check(
            r#"
            //- /app/src/sample1.erl app:app
            -module(sample1).
            foo() -> ?CON~
            //- /another-app/include/inc.hrl app:another include_path:/another-app/include
            -define(CONSTANT, constant).
    "#,
            Some('?'),
            expect![[
                r#"{label:CONSTANT, kind:Macro, contents:SameAsLabel, position:None, include:18:"-include_lib(\"another/include/inc.hrl\")."}"#
            ]],
        );
    }

    #[test]
    fn well_known_macros_no_import() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");
        check(
            r#"
            //- /app/src/sample1.erl app:app
            -module(sample1).
            -include_lib("another/include/inc.hrl").
            foo() -> ?asse~
            //- /another-app/include/inc.hrl app:another include_path:/another-app/include
            -define(assertEqual(A,B), A =:= B).
    "#,
            Some('?'),
            expect![[r#"
                {label:assertEqual/2, kind:Macro, contents:Snippet("assertEqual(${1:Arg1}, ${2:Arg2})"), position:None}"#]],
        );
    }

    #[test]
    fn well_known_macros_no_match() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");
        check(
            r#"
    -module(sample1).
    foo() -> ?assb~
    "#,
            Some('?'),
            expect![""],
        );
    }

    #[test]
    fn detect_macros_match() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");
        check(
            r#"
         //- /src/sample1.erl
           -module(sample1).
           foo() -> ?FO~
         //- /src/header.hrl include_path:/src
           -define(FOO,3).
           -define(FOO(X),X+3).
    "#,
            Some('?'),
            expect![[r#"
                {label:FOO, kind:Macro, contents:SameAsLabel, position:None, include:20:"-include_lib(\"test-fixture/src/header.hrl\")."}
                {label:FOO/1, kind:Macro, contents:Snippet("FOO(${1:Arg1})"), position:None, include:20:"-include_lib(\"test-fixture/src/header.hrl\")."}"#]],
        );
    }

    #[test]
    fn detect_macros_multiple_match() {
        assert!(serde_json::to_string(&lsp_types::CompletionItemKind::CONSTANT).unwrap() == "21");
        check(
            r#"
         //- /src/sample1.erl
           -module(sample1).
           foo() -> ?FO~

         //- /app_a/include/header.hrl app:app_a include_path:/app_a/include
           -define(FOO,3).
           -define(FOO(X),X+3).

         //- /app_b/include/header.hrl app:app_b include_path:/app_b/include
           -define(FOO,4).
           -define(FOO(X),X+4).
    "#,
            Some('?'),
            expect![[r#"
                {label:FOO (app_a), kind:Macro, contents:SameAsLabel, position:None, include:20:"-include_lib(\"app_a/include/header.hrl\")."}
                {label:FOO (app_b), kind:Macro, contents:SameAsLabel, position:None, include:20:"-include_lib(\"app_b/include/header.hrl\")."}
                {label:FOO/1 (app_a), kind:Macro, contents:Snippet("FOO(${1:Arg1})"), position:None, include:20:"-include_lib(\"app_a/include/header.hrl\")."}
                {label:FOO/1 (app_b), kind:Macro, contents:Snippet("FOO(${1:Arg1})"), position:None, include:20:"-include_lib(\"app_b/include/header.hrl\")."}"#]],
        );
    }
}
