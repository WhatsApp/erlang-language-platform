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
use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;
use elp_ide::elp_ide_db::elp_base_db::fixture::WithFixture;
use elp_project_model::otp::Otp;
use elp_project_model::test_fixture::DiagnosticsEnabled;
use expect_test::expect_file;
use fxhash::FxHashSet;

use super::types::glean;
use super::types::parser;
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

    let file_facts = vec![glean::FileFact::new(
        file_id,
        "/test/app/src/test_module.erl".into(),
    )];

    let file_line_facts = vec![glean::FileLinesFact::new(file_id, vec![71, 42], true)];

    let decl = parser::FileDeclaration {
        file_id: file_id.into(),
        declarations: vec![parser::Declaration::FunctionDeclaration(
            parser::FuncDecl {
                name: func_name.to_string(),
                arity,
                span: location.clone(),
                exported: false,
                deprecated: false,
                deprecated_desc: None,
                spec_text: None,
            }
            .into(),
        )],
    };

    let xref = parser::XRefFile {
        file_id: file_id.into(),
        xrefs: vec![parser::XRef {
            source: location,
            target: parser::XRefTarget::Function(
                parser::FunctionTarget {
                    file_id: file_id.into(),
                    name: func_name.to_string(),
                    arity,
                }
                .into(),
            ),
            caller: None,
        }],
    };

    let module = parser::ModuleFact {
        file_id: file_id.into(),
        name: module_name.to_string(),
        oncall: Some("test_team".to_string()),
        exports: Some(vec![format!("{func_name}/{arity}")]),
        behaviours: Some(vec!["test_behaviour".to_string()]),
        module_doc: Some(parser::ModuleDocComment {
            text: "Test module documentation".to_string(),
            span: Location {
                start: 0,
                length: 42,
            },
        }),
        exdoc_link: Some("https://example.com/docs/test_module.html".to_string()),
        module_span: None,
        callbacks: vec![],
        compile_options: vec![],
        on_load_fns: vec![],
        nif_fns: vec![],
        included_files: vec![],
        record_fields: vec![],
        record_def_texts: vec![],
        behaviour_callback_stubs: vec![],
    };

    let facts = IndexedFacts {
        file_facts,
        file_line_facts,
        module_facts: vec![module],
        file_declarations: vec![decl],
        xrefs: vec![xref],
        thrift_annotations: vec![],
    };
    let mut map = FxHashMap::default();
    map.insert(FACTS_FILE.to_string(), facts);
    let args = Glean {
        project: PathBuf::default(),
        module: None,
        to: None,
        schema2: false,
        pretty: false,
        multi: false,
        print_metrics: false,
        source_root: None,
    };
    let mut module_index = FxHashMap::default();
    module_index.insert(file_id.into(), module_name.to_string());

    let app_index = FxHashMap::default();
    let result = IndexResult {
        facts: map,
        module_index,
        app_index,
        app_infos: vec![],
        errored_paths: vec![],
    };
    write_results(result, &mut cli, &args).expect("success");
    let (out, err) = cli.to_strings();
    let expected = resource_file!("glean/serialization_test.out");
    assert_eq!(expected.data().trim(), &out);
    assert_eq!(err, "")
}

#[test]
fn glean_serialization_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module1.erl
    -module(glean_module1).
    -export([hello/1]).
    hello(X) -> X.
    "#;
    let (facts, _, _, _, module_index) = facts_with_annotations(spec);
    let app_index = FxHashMap::default();
    let (glean_facts, _) = facts.into_glean_facts(&module_index, &app_index);

    let predicates: Vec<&str> = glean_facts
        .iter()
        .filter_map(|f| match f {
            glean::Fact::File { facts } if !facts.is_empty() => Some("src.File"),
            glean::Fact::FileLine { facts } if !facts.is_empty() => Some("src.FileLines"),
            glean::Fact::FunctionDeclaration { facts } if !facts.is_empty() => {
                Some("FunctionDeclaration.2")
            }
            glean::Fact::FunctionDefinition { facts } if !facts.is_empty() => {
                Some("FunctionDefinition.2")
            }
            glean::Fact::DeclarationLocation { facts } if !facts.is_empty() => {
                Some("DeclarationLocation.2")
            }
            glean::Fact::FileDeclarations { facts } if !facts.is_empty() => {
                Some("FileDeclarations.2")
            }
            glean::Fact::ModuleDeclaration { facts } if !facts.is_empty() => {
                Some("ModuleDeclaration.2")
            }
            glean::Fact::ModuleDefinition { facts } if !facts.is_empty() => {
                Some("ModuleDefinition.2")
            }
            _ => None,
        })
        .collect();

    for expected in [
        "src.File",
        "src.FileLines",
        "FunctionDeclaration.2",
        "FunctionDefinition.2",
        "DeclarationLocation.2",
        "FileDeclarations.2",
        "ModuleDeclaration.2",
        "ModuleDefinition.2",
    ] {
        assert!(
            predicates.contains(&expected),
            "Expected {expected} in glean output, got: {predicates:?}",
        );
    }
}

#[test]
fn app_info_serialization_test() {
    let app_infos = vec![
        glean::AppInfo {
            name: "my_app".to_string(),
            type_: glean::AppType::FirstParty,
        },
        glean::AppInfo {
            name: "stdlib".to_string(),
            type_: glean::AppType::Otp,
        },
        glean::AppInfo {
            name: "lager".to_string(),
            type_: glean::AppType::ThirdParty,
        },
    ];
    let facts: Vec<glean::Fact> = vec![glean::Fact::AppInfo {
        facts: app_infos.into_iter().map(|ai| Key { key: ai }).collect(),
    }];
    let json = serde_json::to_value(&facts).unwrap();
    let app_info_fact = &json[0]["facts"];

    assert_eq!(app_info_fact[0]["key"]["name"], "my_app");
    assert_eq!(
        app_info_fact[0]["key"]["type_"], 0,
        "FirstParty should serialize as 0"
    );
    assert_eq!(app_info_fact[1]["key"]["name"], "stdlib");
    assert_eq!(
        app_info_fact[1]["key"]["type_"], 1,
        "Otp should serialize as 1"
    );
    assert_eq!(app_info_fact[2]["key"]["name"], "lager");
    assert_eq!(
        app_info_fact[2]["key"]["type_"], 2,
        "ThirdParty should serialize as 2"
    );
}

#[test]
fn parses_glean_json_marker() {
    let comment = r#"%% Glean {"file": "foo/foo.thrift", "kind": "function", "service": "Svc", "name": "bar"}"#;
    let parsed = parse_glean_marker(comment).unwrap();
    assert_eq!(parsed.kind, "function");
    assert_eq!(parsed.name, "bar");
    assert_eq!(parsed.service.as_deref(), Some("Svc"));
    assert_eq!(parsed.file, "foo/foo.thrift");
}

#[test]
fn field_marker_to_thrift_decl() {
    let comment = r#"%% Glean {"file": "foo/foo.thrift", "kind": "field", "container_kind": "struct", "container": "Point", "name": "x"}"#;
    let marker = parse_glean_marker(comment).unwrap();
    assert_eq!(marker.container.as_deref(), Some("Point"));
    assert_eq!(marker.container_kind.as_deref(), Some("struct"));
    let decl = marker_to_thrift_decl(&marker).unwrap();
    let json = serde_json::to_value(&decl).unwrap();
    // fbthrift.FieldDecl: { qname: {file, name=container}, kind: FieldKind, name: field }
    assert_eq!(json["field"]["key"]["qname"]["key"]["name"]["key"], "Point");
    assert_eq!(
        json["field"]["key"]["qname"]["key"]["file"]["key"]["key"],
        "foo/foo.thrift"
    );
    assert_eq!(json["field"]["key"]["kind"], 0); // FIELD_KIND_STRUCT
    assert_eq!(json["field"]["key"]["name"]["key"], "x");
}

#[test]
fn declaration_target_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module1.erl
    -module(glean_module1).
    -export([bar/1]).
    foo(X) -> X + 1.
    bar(X) -> foo(X).
    "#;
    let (facts, _, _, _, module_index) = facts_with_annotations(spec);
    let app_index = FxHashMap::default();
    let (glean_facts, _) = facts.into_glean_facts(&module_index, &app_index);

    let target_json = glean_facts
        .iter()
        .filter_map(|f| match f {
            glean::Fact::DeclarationTarget { facts } => Some(facts),
            _ => None,
        })
        .flatten()
        .map(|t| serde_json::to_value(t).unwrap())
        .collect::<Vec<_>>();
    assert_eq!(target_json.len(), 1, "Expected 1 DeclarationTarget fact");
    let fact = &target_json[0]["key"];
    assert_eq!(fact["source"]["func"]["key"]["fqn"]["name"], "bar");
    assert_eq!(fact["target"]["func"]["key"]["fqn"]["name"], "foo");
}

#[test]
fn erlang_to_thrift_bridge_test() {
    // Mimics generated thrift output: -codegen_source attribute + %% Glean
    // markers, each immediately preceding the declaration it annotates.
    let spec = r#"
    //- /glean/app_glean/src/thrift_test.erl
    -module(thrift_test).
    -codegen_source("foo/foo.thrift").
    -export([const_max/0]).

    %% Glean {"file": "foo/foo.thrift", "kind": "struct", "name": "Point"}
    -type point() :: #{x => integer()}.

    %% Glean {"file": "foo/foo.thrift", "kind": "constant", "name": "MAX"}
    const_max() -> 42.
    "#;
    let (facts, _, _, _, module_index) = facts_with_annotations(spec);
    let app_index = FxHashMap::default();
    let (glean_facts, _) = facts.into_glean_facts(&module_index, &app_index);

    let bridges = glean_facts
        .iter()
        .filter_map(|f| match f {
            glean::Fact::ErlangToThrift { facts } => Some(facts),
            _ => None,
        })
        .flatten()
        .map(|t| serde_json::to_value(t).unwrap())
        .collect::<Vec<_>>();
    assert_eq!(bridges.len(), 2, "Expected 2 ErlangToThrift facts");

    let from_kind = |k: &str| {
        bridges
            .iter()
            .find(|b| b["key"]["from"].get(k).is_some())
            .unwrap_or_else(|| panic!("no bridge fact with `from` kind {k}"))
            .clone()
    };

    // struct marker → generated `-type point()`, to = NamedDecl{kind=struct_=2}
    let ty = from_kind("type_");
    assert_eq!(ty["key"]["from"]["type_"]["key"]["name"], "point");
    assert_eq!(ty["key"]["to"]["named"]["key"]["name"]["kind"], 2);
    assert_eq!(
        ty["key"]["to"]["named"]["key"]["name"]["name"]["key"]["name"]["key"],
        "Point"
    );
    assert_eq!(
        ty["key"]["to"]["named"]["key"]["name"]["name"]["key"]["file"]["key"]["key"],
        "foo/foo.thrift"
    );

    // constant marker → generated accessor fn, to = Constant{name=MAX}
    let func = from_kind("func");
    assert_eq!(
        func["key"]["from"]["func"]["key"]["fqn"]["name"],
        "const_max"
    );
    assert_eq!(
        func["key"]["to"]["constant"]["key"]["name"]["key"]["name"]["key"],
        "MAX"
    );
}

#[test]
fn erlang_to_thrift_field_bridge_test() {
    // A thrift `.hrl`: a `% @codegen_source` comment + a field marker before a record field.
    let spec = r#"
    //- /glean/app_glean/include/thrift_test.hrl
    % @codegen_source foo/foo.thrift
    -record(point, {
        %% Glean {"file": "foo/foo.thrift", "kind": "field", "container_kind": "struct", "container": "Point", "name": "x"}
        x :: integer()
    }).
    "#;
    let (facts, _, _, _, module_index) = facts_with_annotations(spec);
    let app_index = FxHashMap::default();
    let (glean_facts, _) = facts.into_glean_facts(&module_index, &app_index);

    let bridges = glean_facts
        .iter()
        .filter_map(|f| match f {
            glean::Fact::ErlangToThrift { facts } => Some(facts),
            _ => None,
        })
        .flatten()
        .map(|t| serde_json::to_value(t).unwrap())
        .collect::<Vec<_>>();
    assert_eq!(bridges.len(), 1, "Expected 1 ErlangToThrift field fact");

    // field marker → generated record field, to = FieldDecl{container=Point, kind=struct_=0, name=x}
    let field = &bridges[0]["key"];
    assert_eq!(field["from"]["record_field"]["key"]["field_name"], "x");
    assert_eq!(
        field["to"]["field"]["key"]["qname"]["key"]["name"]["key"],
        "Point"
    );
    assert_eq!(field["to"]["field"]["key"]["kind"], 0);
    assert_eq!(field["to"]["field"]["key"]["name"]["key"], "x");
}

#[test]
fn schema2_flag_is_noop_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module1.erl
    -module(glean_module1).
    -export([hello/1]).
    hello(X) -> X.
    "#;
    let (facts, _, _, _, module_index) = facts_with_annotations(spec);
    let build_result = |facts| {
        let mut map = FxHashMap::default();
        map.insert(FACTS_FILE.to_string(), facts);
        IndexResult {
            facts: map,
            module_index: module_index.clone(),
            app_index: FxHashMap::default(),
            app_infos: vec![],
            errored_paths: vec![],
        }
    };
    let args = Glean {
        project: PathBuf::default(),
        module: None,
        to: None,
        schema2: false,
        pretty: false,
        multi: false,
        print_metrics: false,
        source_root: None,
    };

    let mut cli_without_flag = Fake::default();
    write_results(build_result(facts.clone()), &mut cli_without_flag, &args).expect("success");
    let (without_flag_out, _) = cli_without_flag.to_strings();

    let mut cli_with_flag = Fake::default();
    let args_with_schema2 = Glean {
        schema2: true,
        ..args.clone()
    };
    write_results(build_result(facts), &mut cli_with_flag, &args_with_schema2).expect("success");
    let (with_flag_out, _) = cli_with_flag.to_strings();

    assert_eq!(without_flag_out, with_flag_out);
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
fn apply_source_root_no_prefix_leaves_path_unchanged() {
    let expected_path = "lib/stdlib/src/lists.erl";
    assert_eq_expected!(expected_path, apply_source_root(expected_path, None));
    assert_eq_expected!(expected_path, apply_source_root(expected_path, Some("")));
}

#[test]
fn apply_source_root_joins_prefix_with_separator() {
    assert_eq_expected!(
        "prefix/lib/stdlib/src/lists.erl",
        apply_source_root("lib/stdlib/src/lists.erl", Some("prefix"))
    );
}

#[test]
fn apply_source_root_trims_trailing_slash() {
    assert_eq_expected!(
        "prefix/lib/stdlib/src/lists.erl",
        apply_source_root("lib/stdlib/src/lists.erl", Some("prefix/"))
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
    if Otp::supported_by_eqwalizer() {
        let spec = r#"
    //- eqwalizer
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
    %%          ^^^ var/Bar :: integer()
    %%          ^^^ doc/Bar :: integer()
    %%                   ^^^ var/Bar :: integer()
    %%                   ^^^ doc/Bar :: integer()

        main(A) -> A.
    %%  ^^^^^^^^^^^^^ func/main/1/not_deprecated/not_exported
    "#;
        decl_check(spec);
    }
}

#[test]
fn declaration_types_test() {
    if Otp::supported_by_eqwalizer() {
        let spec = r#"
    //- eqwalizer
    //- erlang_service
    //- /app_glean/src/glean_module5.erl app:app_glean
        -module(glean_module5).
        foo(B) -> 1.
    %%  ^^^^^^^^^^^^ func/foo/1/not_deprecated/not_exported
        -spec doc_foo(integer() | atom()) -> [integer()].
        doc_foo(Bar) -> A = foo(Bar), [Bar, A].
    %%          ^^^ var/Bar :: integer() | atom()
    %%          ^^^ doc/Bar :: integer() | atom()
    %%                          ^^^ var/Bar :: integer() | atom()
    %%                          ^^^ doc/Bar :: integer() | atom()
    %%                                 ^^^ var/Bar :: integer() | atom()
    %%                                 ^^^ doc/Bar :: integer() | atom()
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
fn record_field_xref_multifield_test() {
    let spec = r#"
    //- /glean/app_glean/src/glean_module_rf.erl
    -module(glean_module_rf).
    -record(state, {name, age}).
    baz() -> #state{name = a, age = 1}.
    "#;
    let (facts, _, file_names, _, _) = facts_with_annotations(spec);
    let mut xref_labels: Vec<String> = vec![];
    for xref_fact in facts.xrefs {
        for xref in xref_fact.xrefs {
            let file_name = file_names
                .get(xref.target.file_id())
                .unwrap_or(&"?".to_string())
                .clone();
            let label = xref.target.to_string();
            if !label.is_empty() {
                xref_labels.push(format!("{file_name}/{label}"));
            }
        }
    }
    xref_labels.sort();
    assert!(
        xref_labels.contains(&"glean_module_rf.erl/rec/state/no_urls".to_string()),
        "Expected record xref, got: {xref_labels:?}"
    );
    assert!(
        xref_labels.contains(&"glean_module_rf.erl/rec_field/state/name".to_string()),
        "Expected record_field name xref, got: {xref_labels:?}"
    );
    assert!(
        xref_labels.contains(&"glean_module_rf.erl/rec_field/state/age".to_string()),
        "Expected record_field age xref, got: {xref_labels:?}"
    );
}

#[test]
fn callback_xref_test() {
    let spec = r#"
    //- /glean/app_glean/src/my_behaviour.erl
    -module(my_behaviour).
    -callback on_event(term()) -> ok.
    //- /glean/app_glean/src/my_impl.erl
    -module(my_impl).
    -behaviour(my_behaviour).
    on_event(_Event) -> ok.
    "#;
    let (facts, _, file_names, _, _) = facts_with_annotations(spec);
    let mut callback_xrefs: Vec<String> = vec![];
    for xref_fact in &facts.xrefs {
        for xref in &xref_fact.xrefs {
            let label = xref.target.to_string();
            if label.starts_with("callback/") {
                let file_name = file_names
                    .get(xref.target.file_id())
                    .unwrap_or(&"?".to_string())
                    .clone();
                callback_xrefs.push(format!("{file_name}/{label}"));
            }
        }
    }
    assert!(
        callback_xrefs
            .iter()
            .any(|x| x.contains("my_behaviour.erl/callback/on_event/1")),
        "Expected callback xref from on_event to my_behaviour, got: {callback_xrefs:?}"
    );
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
    %%          ^^^^ glean_module9.erl/rec_field/query/size
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
    %%          ^^^^^^ glean_module10.erl/rec_field/stats/count
        {#stats.time, Time}].
    %%   ^^^^^^ glean_module10.erl/rec/stats/no_urls
    %%         ^^^^^ glean_module10.erl/rec_field/stats/time

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
    %%             ^^^^^^ glean_module11.erl/rec_field/stats/count
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
    %%              ^^^^^ glean_module12.erl/rec_field/stats/count
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
    %%         ^^^^^ glean_module13.erl/rec_field/stats/count
    %%                        ^^^^ glean_module13.erl/rec_field/stats/time
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
    %%      ^^^^^^ glean_module14.erl/rec_field/rec/field
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
    %%         ^^^^^ glean_module15.erl/rec_field/stats/count
    %%                    ^^^^ glean_module15.erl/rec_field/stats/time
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
    %%             ^^^ macro.hrl/macro/TAU/no_arity/no_urls/6.28
        baz(N) -> ?MAX(N, 200).
    %%             ^^^ macro.erl/macro/MAX/2/no_urls/if (N > 200) -> N; true -> 200 end

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
    %%       ^^^ macro.erl/macro/TAU/no_arity/no_urls/6.28

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
    %%                    ^^^^ macro.erl/macro/TYPE/no_arity/no_urls/erlang:integer()
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
    %%        ^^^ macro.erl/macro/FOO/1/no_urls/atom

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

#[test]
fn var_xref_decl_span_test() {
    let spec = r#"
    //- /glean/app_glean/src/var_span.erl
    -module(var_span).
    foo(Queues, Table) ->
    %%  ^^^^^^ var_span.erl/var/Queues/23
    %%          ^^^^^ var_span.erl/var/Table/31
        use(Queues),
    %%      ^^^^^^ var_span.erl/var/Queues/23
        use(Table),
    %%      ^^^^^ var_span.erl/var/Table/31
        use(Queues).
    %%      ^^^^^^ var_span.erl/var/Queues/23
    use(_) -> ok.
    "#;
    var_xref_check(spec);
}

#[test]
fn var_xref_multiple_clauses_test() {
    let spec = r#"
    //- /glean/app_glean/src/var_clause.erl
    -module(var_clause).
    bar(X) -> X.
    %%  ^ var_clause.erl/var/X/25
    %%        ^ var_clause.erl/var/X/25
    bar(Y, Y) -> Y.
    %%  ^ var_clause.erl/var/Y/38
    %%     ^ var_clause.erl/var/Y/38
    %%           ^ var_clause.erl/var/Y/38
    "#;
    var_xref_check(spec);
}

#[test]
fn type_definition_text_and_opaque_test() {
    let spec = r#"
    //- /glean/app_glean/src/type_defs.erl
    -module(type_defs).
    -type my_list() :: [integer()].
    -opaque secret() :: {atom(), binary()}.
    foo() -> ok.
    "#;
    let (facts, _, _, _, _) = facts_with_annotations(spec);
    let type_decls: Vec<_> = facts
        .file_declarations
        .iter()
        .flat_map(|fd| &fd.declarations)
        .filter_map(|d| match d {
            parser::Declaration::TypeDeclaration(t) => Some(&t.key),
            _ => None,
        })
        .collect();
    assert_eq!(type_decls.len(), 2);

    let my_list = type_decls.iter().find(|t| t.name == "my_list").unwrap();
    assert!(!my_list.opaque);
    assert!(my_list.definition_text.is_some());
    assert!(
        my_list
            .definition_text
            .as_ref()
            .unwrap()
            .contains("-type my_list")
    );

    let secret = type_decls.iter().find(|t| t.name == "secret").unwrap();
    assert!(secret.opaque);
    assert!(secret.definition_text.is_some());
    assert!(
        secret
            .definition_text
            .as_ref()
            .unwrap()
            .contains("-opaque secret")
    );
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
    let (db, fixture) = RootDatabase::with_fixture(spec);
    let project_id = ProjectId(0);
    let host = AnalysisHost::new(db);
    let glean = GleanIndexer {
        project_id,
        analysis: host.analysis(),
        module: None,
    };
    let IndexResult {
        facts,
        module_index,
        ..
    } = glean.index(IndexConfig::default()).expect("success");
    let facts = facts.into_values().next().unwrap();
    let mut expected_by_file: HashMap<GleanFileId, _> = HashMap::new();
    let mut file_names = HashMap::new();
    let db = host.raw_database();
    for file_id in &fixture.files {
        let file_id = *file_id;
        let source_root_id = db.file_source_root(file_id).source_root_id(db);
        let source_root = db.source_root(source_root_id).source_root(db);
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
                .unwrap_or_else(|| {
                    panic!(
                        "Got computed value {:?}, annotations {:?}",
                        &tuple, &annotations
                    )
                });
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

#[track_caller]
fn var_xref_check(spec: &str) {
    let (facts, mut expected_by_file, file_names, _d, _) = facts_with_annotations(spec);
    for xref_fact in facts.xrefs {
        let file_id = xref_fact.file_id;
        let mut annotations = expected_by_file
            .remove(&file_id)
            .expect("Annotations should be present");
        for xref in xref_fact.xrefs {
            if let parser::XRefTarget::Var(v) = &xref.target {
                let span = match v.key.decl_span_start {
                    Some(s) => s.to_string(),
                    None => continue,
                };
                let range: TextRange = xref.source.clone().into();
                let file_name = file_names
                    .get(xref.target.file_id())
                    .expect("must be present");
                let label = format!("{file_name}/var/{}/{span}", v.key.name);
                let tuple = (range, label);
                let idx = annotations
                    .iter()
                    .position(|a| a == &tuple)
                    .unwrap_or_else(|| {
                        panic!(
                            "Got computed value {:?}, annotations {:?}",
                            &tuple, &annotations
                        )
                    });
                annotations.remove(idx);
            }
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
                let label = decl.to_string();
                if matches!(decl, parser::Declaration::HeaderDeclaration(_)) {
                    let idx = annotations
                        .iter()
                        .position(|a| a.1 == label)
                        .unwrap_or_else(|| {
                            panic!("Expected to find header {:?} in {:?}", &label, &annotations)
                        });
                    annotations.remove(idx);
                } else {
                    let range: TextRange = decl.span().clone().into();
                    let tuple = (range, label);
                    let idx = annotations
                        .iter()
                        .position(|a| a == &tuple)
                        .unwrap_or_else(|| {
                            panic!("Expected to find {:?} in {:?}", &tuple, &annotations)
                        });
                    annotations.remove(idx);
                }
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

impl parser::Declaration {
    fn span(&self) -> Location {
        match self {
            parser::Declaration::FunctionDeclaration(decl) => decl.key.span.clone(),
            parser::Declaration::MacroDeclaration(decl) => decl.key.span.clone(),
            parser::Declaration::TypeDeclaration(decl) => decl.key.span.clone(),
            parser::Declaration::RecordDeclaration(decl) => decl.key.span.clone(),
            parser::Declaration::VarDeclaration(decl) => decl.key.span.clone(),
            parser::Declaration::HeaderDeclaration(decl) => decl.key.span.clone(),
            parser::Declaration::DocDeclaration(decl) => decl.key.target.span().clone(),
        }
    }
}

impl fmt::Display for parser::Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            parser::Declaration::FunctionDeclaration(decl) => {
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
            parser::Declaration::MacroDeclaration(decl) => {
                let arity = match &decl.key.arity {
                    Some(arity) => arity.to_string(),
                    None => "no_arity".to_string(),
                };
                f.write_str(format!("macro/{}/{}", decl.key.name, arity).as_str())
            }
            parser::Declaration::TypeDeclaration(decl) => {
                let exported = match decl.key.exported {
                    true => "exported",
                    false => "not_exported",
                };
                f.write_str(
                    format!("type/{}/{}/{}", decl.key.name, decl.key.arity, exported).as_str(),
                )
            }
            parser::Declaration::RecordDeclaration(decl) => {
                f.write_str(format!("rec/{}", decl.key.name).as_str())
            }
            parser::Declaration::VarDeclaration(decl) => {
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
            parser::Declaration::HeaderDeclaration(decl) => {
                f.write_str(format!("header/{}", decl.key.name).as_str())
            }
            parser::Declaration::DocDeclaration(decl) => f.write_str(
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

impl fmt::Display for parser::XRefTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            parser::XRefTarget::Function(xref) => {
                f.write_str(format!("func/{}/{}", xref.key.name, xref.key.arity).as_str())
            }
            parser::XRefTarget::Macro(xref) => {
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
            parser::XRefTarget::Header(_) => f.write_str("header"),
            parser::XRefTarget::Record(xref) => {
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
            parser::XRefTarget::Type(xref) => {
                f.write_str(format!("type/{}/{}", xref.key.name, xref.key.arity).as_str())
            }
            parser::XRefTarget::Var(_) => Ok(()),
            parser::XRefTarget::RecordField(rf) => f.write_str(
                format!("rec_field/{}/{}", rf.key.record_name, rf.key.field_name).as_str(),
            ),
            parser::XRefTarget::Callback(cb) => {
                f.write_str(format!("callback/{}/{}", cb.key.name, cb.key.arity).as_str())
            }
        }
    }
}
