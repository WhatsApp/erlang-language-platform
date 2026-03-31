/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;

use elp::cli::Fake;
use elp_ide::AnalysisHost;
use elp_ide::elp_ide_db::elp_base_db::fixture::WithFixture;
use elp_project_model::otp::otp_supported_by_eqwalizer;
use elp_project_model::test_fixture::DiagnosticsEnabled;
use expect_test::expect_file;
use fxhash::FxHashSet;

use super::*;
use crate::test_utils::resource_file;

#[test]
fn serialization_test() {
    let mut cli = Fake::default();
    let file_id = FileId::from_raw(10071);
    let location = Location {
        start: 0,
        length: 10,
    };
    let module_name = "test_module";
    let func_name = "test_function";
    let arity = 0;

    let file_facts = vec![FileFact::new(
        file_id,
        "/test/app/src/test_module.erl".into(),
    )];

    let file_line_facts = vec![FileLinesFact::new(file_id, vec![71, 42], true)];

    let decl = FileDeclaration {
        file_id: file_id.into(),
        declarations: vec![Declaration::FunctionDeclaration(
            FuncDecl {
                name: func_name.to_string(),
                arity,
                span: location.clone(),
                exported: false,
                deprecated: false,
            }
            .into(),
        )],
    };

    let xref = XRefFile {
        file_id: file_id.into(),
        xrefs: vec![XRef {
            source: location,
            target: XRefTarget::Function(
                FunctionTarget {
                    file_id: file_id.into(),
                    name: func_name.to_string(),
                    arity,
                }
                .into(),
            ),
        }],
    };

    let module = ModuleFact {
        file_id: file_id.into(),
        name: module_name.to_string(),
        oncall: Some("test_team".to_string()),
        exports: Some(vec![format!("{func_name}/{arity}")]),
        behaviours: Some(vec!["test_behaviour".to_string()]),
        module_doc: Some("Test module documentation".to_string()),
        exdoc_link: Some("https://example.com/docs/test_module.html".to_string()),
    };

    let facts = IndexedFacts {
        file_facts,
        file_line_facts,
        module_facts: vec![module],
        file_declarations: vec![decl],
        xrefs: vec![xref],
    };
    let mut map = FxHashMap::default();
    map.insert(FACTS_FILE.to_string(), facts);
    let args = Glean {
        project: PathBuf::default(),
        module: None,
        to: None,
        v2: true,
        pretty: false,
        multi: false,
        print_metrics: false,
    };
    let mut module_index = FxHashMap::default();
    module_index.insert(file_id.into(), module_name.to_string());

    write_results(map, module_index, &mut cli, &args).expect("success");
    let (out, err) = cli.to_strings();
    let expected = resource_file!("glean/serialization_test.out");
    assert_eq!(expected.data().trim(), &out);
    assert_eq!(err, "")
}

#[test]
fn file_fact_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module2.erl
    -module(glean_module2).
    "#;
    let result = facts_with_annotations(spec).0;
    assert_eq!(result.file_facts.len(), 1);
    let file_fact = &result.file_facts[0];
    assert_eq!(
        file_fact.file_path.as_str(),
        "glean/app_glean/src/glean_module2.erl"
    );
}

#[test]
fn line_fact_with_new_line_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module3.erl
    -module(glean_module3).
    main() ->
        bar.

    "#;
    let result = facts_with_annotations(spec).0;
    assert_eq!(result.file_line_facts.len(), 1);
    let line_fact = &result.file_line_facts[0];
    assert!(line_fact.ends_with_new_line);
    assert_eq!(line_fact.lengths, vec![24, 10, 9, 1]);
}

#[test]
fn line_fact_without_new_line_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module4.erl
    -module(glean_module4).
    main() ->
        bar."#;
    let result = facts_with_annotations(spec).0;
    assert_eq!(result.file_line_facts.len(), 1);
    let line_fact = &result.file_line_facts[0];
    assert!(!line_fact.ends_with_new_line);
    assert_eq!(line_fact.lengths, vec![24, 10, 8]);
}

#[test]
fn declaration_test() {
    if otp_supported_by_eqwalizer() {
        let spec = r#"
    //- /app_glean/src/glean_module5.erl app:app_glean
        -module(glean_module5).
        -export([foo/0, doc_foo/1, depr_foo/1]).
        -export_type([person/1]).
        -deprecated({depr_foo, 1, "use foo/0 instead"}).

        -define(PI, 3.14).
    %%  ^^^^^^^^^^^^^^^^^^ macro/PI/no_arity
        -define(MAX(X, Y), if X > Y -> X; true -> Y end).
    %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ macro/MAX/2

        -type person(Name) :: {name, list(Name)}.
    %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ type/person/1/exported
    %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ doc/-type person(Name) :: {name, list(Name)}.
        -record(user, {name = "" :: string(), notes :: person(pos_integer())}).
    %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ rec/user
    %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ doc/-record(user, {name = "" :: string(), notes :: person(pos_integer())}).

        foo() -> 1.
    %%  ^^^^^^^^^^^ func/foo/0/not_deprecated/exported
        depr_foo(B) -> B.
    %%  ^^^^^^^^^^^^^^^^^ func/depr_foo/1/deprecated/exported
        -spec doc_foo(integer()) -> [integer()].
        doc_foo(Bar) -> [Bar].
    %%  ^^^^^^^^^^^^^^^^^^^^^^ func/doc_foo/1/not_deprecated/exported
    %%  ^^^^^^^^^^^^^^^^^^^^^^ doc/-spec doc_foo(integer()) -> [integer()].
    %%          ^^^ var/Bar :: number()
    %%          ^^^ doc/Bar :: number()
    %%                   ^^^ var/Bar :: number()
    %%                   ^^^ doc/Bar :: number()

        main(A) -> A.
    %%  ^^^^^^^^^^^^^ func/main/1/not_deprecated/not_exported
    "#;
        decl_check(spec);
    }
}

#[test]
fn declaration_types_test() {
    if otp_supported_by_eqwalizer() {
        let spec = r#"
    //- eqwalizer
    //- erlang_service
    //- otp_apps:stdlib
    //- /app_glean/src/glean_module5.erl app:app_glean
        -module(glean_module5).
        foo(B) -> 1.
    %%  ^^^^^^^^^^^^ func/foo/1/not_deprecated/not_exported
        -spec doc_foo(integer() | atom()) -> [integer()].
        doc_foo(Bar) -> A = foo(Bar), [Bar, A].
    %%          ^^^ var/Bar :: number() | atom()
    %%          ^^^ doc/Bar :: number() | atom()
    %%                          ^^^ var/Bar :: number() | atom()
    %%                          ^^^ doc/Bar :: number() | atom()
    %%                                 ^^^ var/Bar :: number() | atom()
    %%                                 ^^^ doc/Bar :: number() | atom()
    %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ func/doc_foo/1/not_deprecated/not_exported
    %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ doc/-spec doc_foo(integer() | atom()) -> [integer()].
    "#;
        decl_check(spec);
    }
}

#[test]
fn declaration_in_header_test() {
    let spec = r#"
    //- /glean/app_glean/include/glean_module5.hrl include_path:/include
    % header
    %%^ header/glean_module5.hrl
        -define(TAU, 6.28).
    %%  ^^^^^^^^^^^^^^^^^^^ macro/TAU/no_arity
        -spec doc_bar(integer()) -> [integer()].
        doc_bar(Bar) -> [Bar].
    %%  ^^^^^^^^^^^^^^^^^^^^^^ func/doc_bar/1/not_deprecated/not_exported
    %%  ^^^^^^^^^^^^^^^^^^^^^^ doc/-spec doc_bar(integer()) -> [integer()].
    //- /glean/app_glean/src/glean_module5.erl
        -module(glean_module5).
    "#;
    decl_check(spec);
}

#[test]
fn xref_call_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module61.erl
    -module(glean_module61).
    foo(Bar) -> Bar + 1.

    //- /glean/app_glean/src/glean_module6.erl
    main() ->
        B = baz(1, 2),
    %%      ^^^ glean_module6.erl/func/baz/2
        F = glean_module61:foo(B),
    %%      ^^^^^^^^^^^^^^^^^^ glean_module61.erl/func/foo/1
        F.
    baz(A, B) ->
        A + B."#;

    xref_check(spec);
}

#[test]
fn xref_export_test() {
    let spec = r#"
    //- /src/glean_module61.erl
    -module(glean_module61).
    -export([foo/1]).
    %%       ^^^^^ glean_module61.erl/func/foo/1
    foo(Bar) -> Bar + 1.
    "#;

    xref_check(spec);
}

#[test]
fn xref_deprecated_test() {
    let spec = r#"
    //- /src/glean_module61.erl
    -module(glean_module61).
    -deprecated([{foo, 1}]).
    %%            ^^^ glean_module61.erl/func/foo/1
    -deprecated([{bar, '_'}]).
    %%            ^^^ glean_module61.erl/func/bar/0
    foo(Bar) -> Bar + 1.
    bar() -> 1.
    bar(A) -> A.
    "#;

    xref_check(spec);
}

#[test]
fn xref_header_test() {
    let spec = r#"
    //- /kernel/include/logger.hrl include_path:/include app:kernel
        % empty

    //- /include/macro.hrl include_path:/include
        % empty

    //- /src/glean_module61.erl
        -module(glean_module61).
        -include_lib("kernel/include/logger.hrl").
    %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ logger.hrl/header
        -include("macro.hrl").
    %%  ^^^^^^^^^^^^^^^^^^^^^^ macro.hrl/header
    "#;

    xref_check(spec);
}

#[test]
fn xref_captured_fun_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module71.erl
    foo(Bar) -> Bar + 1.

    //- /glean/app_glean/src/glean_module7.erl
    main() ->
        Foo = fun glean_module71:foo/1,
    %%        ^^^^^^^^^^^^^^^^^^^^^^^^ glean_module71.erl/func/foo/1
        Baz = fun baz/2.
    %%        ^^^^^^^^^ glean_module7.erl/func/baz/2
    baz(A, B) ->
        A + B."#;

    xref_check(spec);
}

#[test]
fn xref_parametrized_type_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module_parametrized.erl
    -type my_type(X) :: X.
    -type my_integer() :: integer().

    -spec my_function(my_type(my_integer())) -> integer().
    %%                ^^^^^^^ glean_module_parametrized.erl/type/my_type/1
    %%                        ^^^^^^^^^^ glean_module_parametrized.erl/type/my_integer/0
    my_function(X) -> X.
    "#;

    xref_check(spec);
}

#[test]
fn xref_types_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module81.erl
    -type small() ::  {non_neg_integer() | infinity}.

    //- /glean/app_glean/src/glean_module8.erl
    -type huuuge() ::  {non_neg_integer() | infinity}.
    -spec baz(
        A :: huuuge(),
    %%       ^^^^^^ glean_module8.erl/type/huuuge/0
        B :: glean_module81:small()
    %%       ^^^^^^^^^^^^^^^^^^^^ glean_module81.erl/type/small/0
    ) -> huuuge().
    %%   ^^^^^^ glean_module8.erl/type/huuuge/0
    baz(A, B) ->
        A + B."#;

    xref_check(spec);
}

#[test]
fn xref_record_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module9.erl
    -record(query, {
        size :: non_neg_integer()
    }).
    baz(A) ->
        #query{ size = A }.
    %%  ^^^^^^ glean_module9.erl/rec/query/no_urls
    "#;

    xref_check(spec);
}

#[test]
fn xref_record_index_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module10.erl
    -record(stats, {count, time}).
    baz(Time) ->
        [{#stats.count, 1},
    %%    ^^^^^^ glean_module10.erl/rec/stats/no_urls
        {#stats.time, Time}].
    %%   ^^^^^^ glean_module10.erl/rec/stats/no_urls

    "#;

    xref_check(spec);
}

#[test]
fn xref_record_field_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module11.erl
    -record(stats, {count, time}).
    baz(Stats) ->
        Stats#stats.count.
    %%       ^^^^^^ glean_module11.erl/rec/stats/no_urls
    "#;

    xref_check(spec);
}

#[test]
fn xref_record_update_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module12.erl
    -record(stats, {count, time}).
    baz(Stats, NewCnt) ->
        Stats#stats{count = NewCnt}.
    %%       ^^^^^^ glean_module12.erl/rec/stats/no_urls
    "#;

    xref_check(spec);
}

#[test]
fn xref_pat_record_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module13.erl
    -record(stats, {count, time}).
    baz(Stats) ->
        #stats{count = Count, time = Time} = Stats.
    %%  ^^^^^^ glean_module13.erl/rec/stats/no_urls
    "#;

    xref_check(spec);
}

#[test]
fn xref_pat_recordindex() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module14.erl
    -record(rec, {field}).
    foo(#rec.field) -> ok.
    %%  ^^^^ glean_module14.erl/rec/rec/no_urls
    "#;

    xref_check(spec);
}

#[test]
fn xref_record_in_type_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module15.erl
    -record(stats, {count, time}).
    -spec baz() -> #stats{}.
    %%             ^^^^^^ glean_module15.erl/rec/stats/no_urls
    baz() ->
        #stats{count = 1, time = 2}.
    %%  ^^^^^^ glean_module15.erl/rec/stats/no_urls
    "#;

    xref_check(spec);
}

#[test]
fn xref_macro_test() {
    let spec = r#"
    //- /include/macro.hrl include_path:/include
    -define(TAU, 6.28).

    //- /src/macro.erl
        -module(macro).
        -include("macro.hrl").
    %%  ^^^^^^^^^^^^^^^^^^^^^^ macro.hrl/header
        -define(MAX(X, Y), if X > Y -> X; true -> Y end).

        baz(1) -> ?TAU;
    %%             ^^^ macro.erl/macro/TAU/117/no_urls/6.28
        baz(N) -> ?MAX(N, 200).
    %%             ^^^ macro.erl/macro/MAX/137/no_urls/if (N > 200) -> N; true -> 200 end

    "#;
    xref_check(spec);
}

#[test]
fn xref_macro_in_pat_test() {
    let spec = r#"
    //- /src/macro.erl
        -module(macro).
        -define(TAU, 6.28).

        baz(?TAU) -> 1.
    %%       ^^^ macro.erl/macro/TAU/54/no_urls/6.28

    "#;
    xref_check(spec);
}

#[test]
fn xref_macro_in_type_test() {
    let spec = r#"
    //- /src/macro.erl
        -module(macro).
        -define(TYPE, integer()).

        -spec baz(ok) -> ?TYPE.
    %%                    ^^^^ macro.erl/macro/TYPE/73/no_urls/erlang:integer()
        baz(ok) -> 1.

    "#;
    xref_check(spec);
}

#[test]
fn xref_macro_in_term_test() {
    let spec = r#"
    //- /src/macro.erl
        -module(macro).
       -define(FOO(X), X).
       -wild(?FOO(atom)).
    %%        ^^^ macro.erl/macro/FOO/53/no_urls/atom

    "#;
    xref_check(spec);
}

#[test]
fn module_fact_test() {
    let spec = r#"
    //- /src/sample_worker.erl
    %%% This is a module documentation
    %%% It explains what this module does
    -module(sample_worker).
    -oncall("platform_team").
    -behaviour(test_behaviour).
    -export([init/1, handle_task/2]).

    init(Args) -> {ok, Args}.
    handle_task(Task, State) -> {reply, ok, State}.
    internal_helper(X) -> X + 1.
    "#;
    let (facts, _, _, _, _) = facts_with_annotations(spec);
    assert_eq!(facts.module_facts.len(), 1);
    let module_fact = &facts.module_facts[0];
    assert_eq!(module_fact.name, "sample_worker");
    assert_eq!(module_fact.oncall, Some("platform_team".to_string()));
    assert_eq!(
        module_fact.behaviours,
        Some(vec!["test_behaviour".to_string()])
    );
    assert_eq!(module_fact.exports.as_ref().map(|v| v.len()), Some(2));
    for expected in ["handle_task/2", "init/1"] {
        assert!(
            module_fact
                .exports
                .as_ref()
                .unwrap()
                .contains(&expected.to_string())
        );
    }
    assert!(module_fact.module_doc.is_none());
}

#[test]
fn module_fact_multiple_behaviours_test() {
    let spec = r#"
    //- /src/factory_service.erl
    -module(factory_service).
    -oncall("manufacturing_team").
    -behaviour(supervisor).
    -behaviour(test_behaviour1).
    -behaviour(test_behaviour2).

    start_supervision() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    create_product(Type, Config) -> test_behaviour1:produce(Type, Config).
    manage_assembly(Parts) -> test_behaviour2:assemble(Parts).
    "#;
    let (facts, _, _, _, _) = facts_with_annotations(spec);
    assert_eq!(facts.module_facts.len(), 1);
    let module_fact = &facts.module_facts[0];
    assert_eq!(module_fact.name, "factory_service");
    assert_eq!(module_fact.oncall, Some("manufacturing_team".to_string()));
    assert_eq!(module_fact.behaviours.as_ref().map(|v| v.len()), Some(3));
    for expected in ["test_behaviour1", "test_behaviour2", "supervisor"] {
        assert!(
            module_fact
                .behaviours
                .as_ref()
                .unwrap()
                .contains(&expected.to_string())
        );
    }
    assert!(module_fact.exports.is_none());
    assert!(module_fact.module_doc.is_none());
}

#[test]
fn module_fact_no_oncall_test() {
    let spec = r#"
    //- /src/utility_helper.erl
    -module(utility_helper).
    -behaviour(supervisor).
    -export([start_link/0, add_child/2]).

    start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    add_child(ChildSpec, Opts) -> supervisor:start_child(?MODULE, {ChildSpec, Opts}).
    "#;
    let (facts, _, _, _, _) = facts_with_annotations(spec);
    assert_eq!(facts.module_facts.len(), 1);
    let module_fact = &facts.module_facts[0];
    assert_eq!(module_fact.name, "utility_helper");
    assert_eq!(module_fact.oncall, None);
    assert_eq!(module_fact.behaviours, Some(vec!["supervisor".to_string()]));
    assert_eq!(module_fact.exports.as_ref().map(|v| v.len()), Some(2));
    for expected in ["add_child/2", "start_link/0"] {
        assert!(
            module_fact
                .exports
                .as_ref()
                .unwrap()
                .contains(&expected.to_string())
        );
    }
    assert!(module_fact.module_doc.is_none());
}

#[allow(clippy::type_complexity)]
pub(crate) fn facts_with_annotations(
    spec: &str,
) -> (
    IndexedFacts,
    HashMap<GleanFileId, Vec<(TextRange, String)>>,
    HashMap<GleanFileId, String>,
    DiagnosticsEnabled,
    FxHashMap<GleanFileId, String>,
) {
    let config = IndexConfig::default();
    facts_with_annotations_with_config(spec, config)
}

#[allow(clippy::type_complexity)]
fn facts_with_annotations_with_config(
    spec: &str,
    config: IndexConfig,
) -> (
    IndexedFacts,
    HashMap<GleanFileId, Vec<(TextRange, String)>>,
    HashMap<GleanFileId, String>,
    DiagnosticsEnabled,
    FxHashMap<GleanFileId, String>,
) {
    let (db, fixture) = RootDatabase::with_fixture(spec);
    let project_id = ProjectId(0);
    let host = AnalysisHost::new(db);
    let glean = GleanIndexer {
        project_id,
        analysis: host.analysis(),
        module: None,
    };
    let (facts, module_index, _errored_paths) = glean.index(config).expect("success");
    let facts = facts.into_values().next().unwrap();
    let mut expected_by_file: HashMap<GleanFileId, _> = HashMap::new();
    let mut file_names = HashMap::new();
    let db = host.raw_database();
    for file_id in &fixture.files {
        let file_id = *file_id;
        let source_root_id = db.file_source_root(file_id);
        let source_root = db.source_root(source_root_id);
        let path = source_root.path_for_file(&file_id).unwrap();
        let (name, ext) = path.name_and_extension().unwrap();
        let name = format!("{}.{}", name, ext.unwrap());
        file_names.insert(file_id.into(), name);
        let annotations = fixture.annotations_by_file_id(&file_id);
        expected_by_file.insert(file_id.into(), annotations);
    }
    (
        facts,
        expected_by_file,
        file_names,
        fixture.diagnostics_enabled,
        module_index,
    )
}

#[track_caller]
pub(crate) fn xref_check(spec: &str) {
    let (facts, mut expected_by_file, file_names, _d, _) = facts_with_annotations(spec);
    for xref_fact in facts.xrefs {
        let file_id = xref_fact.file_id;
        let mut annotations = expected_by_file
            .remove(&file_id)
            .expect("Annotations should be present");
        for xref in xref_fact.xrefs {
            let range: TextRange = xref.source.clone().into();
            let file_name = file_names
                .get(xref.target.file_id())
                .expect("must be present");
            let label = xref.target.to_string();
            if label.is_empty() {
                continue;
            }
            let label = format!("{file_name}/{label}");
            let tuple = (range, label);
            let idx = annotations
                .iter()
                .position(|a| a == &tuple)
                .unwrap_or_else(|| panic!("Expected to find {:?} in {:?}", &tuple, &annotations));
            annotations.remove(idx);
        }
        assert_eq!(annotations, vec![], "Expected no more annotations");
    }
    assert_eq!(
        expected_by_file,
        HashMap::new(),
        "Expected no more annotations"
    );
}

fn decl_check(spec: &str) {
    let (facts, mut expected_by_file, _, _d, _) = facts_with_annotations(spec);
    let hash_map = &expected_by_file.clone();
    let fixture_files = FxHashSet::from_iter(hash_map.keys());
    for file_decl in facts.file_declarations {
        // Skip files not in the fixture, from OTP
        if fixture_files.contains(&file_decl.file_id) {
            let mut annotations = expected_by_file
                .remove(&file_decl.file_id)
                .expect("Annotations should be present");
            for decl in file_decl.declarations {
                let range: TextRange = decl.span().clone().into();
                let label = decl.to_string();
                let tuple = (range, label);
                let idx = annotations
                    .iter()
                    .position(|a| a == &tuple)
                    .unwrap_or_else(|| {
                        panic!("Expected to find {:?} in {:?}", &tuple, &annotations)
                    });
                annotations.remove(idx);
            }
            assert_eq!(annotations, vec![], "Expected no more annotations");
        }
    }

    assert_eq!(
        expected_by_file,
        HashMap::new(),
        "Expected no more annotations"
    );
}

impl Declaration {
    fn span(&self) -> Location {
        match self {
            Declaration::FunctionDeclaration(decl) => decl.key.span.clone(),
            Declaration::MacroDeclaration(decl) => decl.key.span.clone(),
            Declaration::TypeDeclaration(decl) => decl.key.span.clone(),
            Declaration::RecordDeclaration(decl) => decl.key.span.clone(),
            Declaration::VarDeclaration(decl) => decl.key.span.clone(),
            Declaration::HeaderDeclaration(decl) => Location {
                start: decl.key.span.start + 2,
                length: decl.key.span.length,
            },
            Declaration::DocDeclaration(decl) => decl.key.target.span().clone(),
        }
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Declaration::FunctionDeclaration(decl) => {
                let deprecated = match decl.key.deprecated {
                    true => "deprecated",
                    false => "not_deprecated",
                };
                let exported = match decl.key.exported {
                    true => "exported",
                    false => "not_exported",
                };
                f.write_str(
                    format!(
                        "func/{}/{}/{}/{}",
                        decl.key.name, decl.key.arity, deprecated, exported
                    )
                    .as_str(),
                )
            }
            Declaration::MacroDeclaration(decl) => {
                let arity = match &decl.key.arity {
                    Some(arity) => arity.to_string(),
                    None => "no_arity".to_string(),
                };
                f.write_str(format!("macro/{}/{}", decl.key.name, arity).as_str())
            }
            Declaration::TypeDeclaration(decl) => {
                let exported = match decl.key.exported {
                    true => "exported",
                    false => "not_exported",
                };
                f.write_str(
                    format!("type/{}/{}/{}", decl.key.name, decl.key.arity, exported).as_str(),
                )
            }
            Declaration::RecordDeclaration(decl) => {
                f.write_str(format!("rec/{}", decl.key.name).as_str())
            }
            Declaration::VarDeclaration(decl) => {
                let ttype = decl
                    .key
                    .doc
                    .strip_prefix("```erlang\n")
                    .unwrap()
                    .strip_suffix("\n```")
                    .unwrap()
                    .to_string();
                f.write_str(format!("var/{ttype}").as_str())
            }
            Declaration::HeaderDeclaration(decl) => {
                f.write_str(format!("header/{}", decl.key.name).as_str())
            }
            Declaration::DocDeclaration(decl) => f.write_str(
                format!(
                    "doc/{}",
                    decl.key
                        .text
                        .replace("```erlang\n", "")
                        .replace("\n```", "")
                )
                .as_str(),
            ),
        }
    }
}

impl fmt::Display for XRefTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            XRefTarget::Function(xref) => {
                f.write_str(format!("func/{}/{}", xref.key.name, xref.key.arity).as_str())
            }
            XRefTarget::Macro(xref) => {
                let arity = match &xref.key.arity {
                    Some(arity) => arity.to_string(),
                    None => "no_arity".to_string(),
                };
                // Generate a tag descriptor from tag names
                let url_tag = if xref.key.tagged_urls.is_empty() {
                    "no_urls".to_string()
                } else {
                    let mut tags: Vec<&str> = xref
                        .key
                        .tagged_urls
                        .iter()
                        .map(|u| u.tag.as_str())
                        .collect();
                    tags.sort_unstable();
                    format!("has_{}", tags.join("_"))
                };
                let exp = match &xref.key.expansion {
                    Some(exp) => exp
                        .strip_prefix("```erlang\n")
                        .unwrap()
                        .strip_suffix("\n```")
                        .unwrap()
                        .replace("\n", " ")
                        .split_whitespace()
                        .join(" "),
                    None => "no_exp".to_string(),
                };
                f.write_str(
                    format!("macro/{}/{}/{}/{}", xref.key.name, arity, url_tag, exp).as_str(),
                )
            }
            XRefTarget::Header(_) => f.write_str("header"),
            XRefTarget::Record(xref) => {
                let url_tag = if xref.key.tagged_urls.is_empty() {
                    "no_urls".to_string()
                } else {
                    let mut tags: Vec<&str> = xref
                        .key
                        .tagged_urls
                        .iter()
                        .map(|u| u.tag.as_str())
                        .collect();
                    tags.sort_unstable();
                    format!("has_{}", tags.join("_"))
                };
                f.write_str(format!("rec/{}/{}", xref.key.name, url_tag).as_str())
            }
            XRefTarget::Type(xref) => {
                f.write_str(format!("type/{}/{}", xref.key.name, xref.key.arity).as_str())
            }
            XRefTarget::Var(_) => Ok(()),
        }
    }
}
