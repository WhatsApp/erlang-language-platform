/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter;
use std::sync::Arc;

use elp_ide::diagnostics::DiagnosticCode;
use elp_ide::diagnostics::DiagnosticsConfig;
use elp_ide::diagnostics::LintsFromConfig;
use elp_ide::elp_ide_assists::AssistConfig;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::helpers::SnippetCap;
use elp_ide::HoverActionsConfig;
use elp_ide::InlayHintsConfig;
use fxhash::FxHashSet;
use lsp_types::ClientCapabilities;
use serde::de::DeserializeOwned;
use serde_json::json;

// Defines the server-side configuration of ELP. We generate *parts*
// of VS Code's `package.json` config from this.
//
// However, editor specific config, which the server doesn't know
// about, should be specified directly in `package.json`.
//
// To deprecate an option by replacing it with another name use
// `new_name | `old_name` so that we keep parsing the old name.
config_data! {
  struct ConfigData {
      /// Enable support for AI-based completions.
      ai_enable: bool = json! { false },
      /// Whether to show experimental ELP diagnostics that might
      /// have more false positives than usual.
      diagnostics_enableExperimental: bool = json! { false },
      /// List of ELP diagnostics to disable.
      diagnostics_disabled: FxHashSet<String> = json! { [] },
      /// Whether to show Hover Actions.
      hoverActions_enable: bool = json! { false },
      /// Whether to show Hover Actions of type 'docs'. Only applies when
      /// `#elp.hoverActions.enable#` is set.
      hoverActions_docLinks_enable: bool = json! { false },
      /// Whether to show function parameter name inlay hints at the call
      /// site.
      inlayHints_parameterHints_enable: bool = json! { false },
      /// Whether to show Code Lenses in Erlang files.
      lens_enable: bool = json! { false },
      /// Whether to show the `Run` lenses. Only applies when
      /// `#elp.lens.enable#` is set.
      lens_run_enable: bool = json! { false },
      /// Whether to show the `Run Interactive` lenses. Only applies when
      /// `#elp.lens.enable#` is set.
      lens_run_interactive_enable: bool = json! { false },
      /// Display code coverage information when running tests via the
      /// Code Lenses. Only applies when `#elp.lens.enabled` and
      /// `#elp.lens.run.enable#` are set.
      lens_run_coverage_enable: bool = json! { false },
      /// Whether to show the `Debug` lenses. Only applies when
      /// `#elp.lens.enable#` is set.
      lens_debug_enable: bool = json! { false },
      /// Whether to show the `Link` lenses. Only applies when
      /// `#elp.lens.enable#` is set.
      lens_links_enable: bool = json! { false },
      /// Configure LSP-based logging using env_logger syntax.
      log: String = json! { "error" },
      /// Whether to show Signature Help.
      signatureHelp_enable: bool = json! { false },
      /// Display types when hovering over expressions.
      typesOnHover_enable: bool = json! { false },
  }
}

impl Default for ConfigData {
    fn default() -> Self {
        ConfigData::from_json(serde_json::Value::Null)
    }
}

#[derive(Clone, Debug)]
pub struct Config {
    pub root_path: AbsPathBuf,
    pub caps: ClientCapabilities,
    data: ConfigData,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LensConfig {
    pub run: bool,
    pub run_interactive: bool,
    pub run_coverage: bool,
    pub debug: bool,
    pub links: bool,
}

macro_rules! try_ {
    ($expr:expr) => {
        || -> _ { Some($expr) }()
    };
}
macro_rules! try_or {
    ($expr:expr, $or:expr) => {
        try_!($expr).unwrap_or($or)
    };
}

impl Config {
    pub fn new(root_path: AbsPathBuf, caps: ClientCapabilities) -> Config {
        Config {
            root_path,
            caps,
            data: ConfigData::default(),
        }
    }

    pub fn update(&mut self, json: serde_json::Value) {
        log::info!("updating config from JSON: {:#}", json);
        if json.is_null() || json.as_object().map_or(false, |it| it.is_empty()) {
            return;
        }
        self.data = ConfigData::from_json(json);
    }

    pub fn did_save_text_document_dynamic_registration(&self) -> bool {
        let caps = try_or!(
            self.caps.text_document.as_ref()?.synchronization.clone()?,
            Default::default()
        );
        caps.did_save == Some(true) && caps.dynamic_registration == Some(true)
    }

    pub fn did_change_configuration_dynamic_registration(&self) -> bool {
        let caps = try_or!(
            self.caps.workspace.as_ref()?.did_change_configuration?,
            Default::default()
        );
        caps.dynamic_registration == Some(true)
    }

    pub fn code_action_literals(&self) -> bool {
        try_!(
            self.caps
                .text_document
                .as_ref()?
                .code_action
                .as_ref()?
                .code_action_literal_support
                .as_ref()?
        )
        .is_some()
    }

    pub fn code_action_resolve(&self) -> bool {
        try_or!(
            self.caps
                .text_document
                .as_ref()?
                .code_action
                .as_ref()?
                .resolve_support
                .as_ref()?
                .properties
                .as_slice(),
            &[]
        )
        .iter()
        .any(|it| it == "edit")
    }

    pub fn location_link(&self) -> bool {
        try_or!(
            self.caps.text_document.as_ref()?.definition?.link_support?,
            false
        )
    }

    pub fn hierarchical_symbols(&self) -> bool {
        try_or!(
            self.caps
                .text_document
                .as_ref()?
                .document_symbol
                .as_ref()?
                .hierarchical_document_symbol_support?,
            false
        )
    }

    fn experimental(&self, index: &'static str) -> bool {
        try_or!(
            self.caps.experimental.as_ref()?.get(index)?.as_bool()?,
            false
        )
    }

    pub fn enable_experimental_diagnostics(&self) -> bool {
        self.data.diagnostics_enableExperimental
    }

    pub fn diagnostics(&self, lints_from_config: Arc<LintsFromConfig>) -> DiagnosticsConfig {
        // Look up disabled diagnostics using both label and code.
        let mut config = DiagnosticsConfig::default()
            .from_config(&lints_from_config)
            .set_experimental(self.data.diagnostics_enableExperimental);
        for code in self
            .data
            .diagnostics_disabled
            .iter()
            .filter_map(DiagnosticCode::maybe_from_string)
        {
            config = config.disable(code);
        }
        config
    }

    pub fn code_action_group(&self) -> bool {
        self.experimental("codeActionGroup")
    }

    pub fn server_status_notification(&self) -> bool {
        // Under experimental umbrella. Rationale:
        // - Only used for end-to-end tests for now.
        // - Mimic rust-analyzer (at 2021-02-08 revision).
        self.experimental("serverStatusNotification")
    }

    pub fn lens(&self) -> LensConfig {
        LensConfig {
            run: self.data.lens_enable && self.data.lens_run_enable,
            run_interactive: self.data.lens_enable && self.data.lens_run_interactive_enable,
            run_coverage: self.data.lens_enable
                && self.data.lens_run_enable
                && self.data.lens_run_coverage_enable,
            debug: self.data.lens_enable && self.data.lens_debug_enable,
            links: self.data.lens_enable && self.data.lens_links_enable,
        }
    }

    pub fn hover_actions(&self) -> HoverActionsConfig {
        HoverActionsConfig {
            doc_links: self.data.hoverActions_enable && self.data.hoverActions_docLinks_enable,
        }
    }

    pub fn signature_help(&self) -> bool {
        self.data.signatureHelp_enable
    }

    pub fn types_on_hover(&self) -> bool {
        self.data.typesOnHover_enable
    }

    pub fn assist(&self) -> AssistConfig {
        AssistConfig {
            snippet_cap: SnippetCap::new(self.experimental("snippetTextEdit")),
            allowed: None,
        }
    }

    pub fn work_done_progress(&self) -> bool {
        try_or!(self.caps.window.as_ref()?.work_done_progress?, false)
    }

    pub fn ai_enabled(&self) -> bool {
        self.data.ai_enable
    }

    pub fn inlay_hints(&self) -> InlayHintsConfig {
        InlayHintsConfig {
            parameter_hints: self.data.inlayHints_parameterHints_enable,
        }
    }

    pub fn log_filter(&self) -> elp_log::Builder {
        let mut builder = elp_log::Builder::new();
        builder.parse(&self.data.log);
        builder
    }

    // Used for setting up tests
    pub fn ignore_diagnostic(&mut self, diagnostic: DiagnosticCode) {
        self.data.diagnostics_disabled.insert(diagnostic.as_code());
    }

    pub fn json_schema() -> serde_json::Value {
        ConfigData::json_schema()
    }
}

macro_rules! _config_data {
    (struct $name:ident {
        $(
            $(#[doc=$doc:literal])*
            $field:ident $(| $alias:ident)*: $ty:ty = $default:expr,
        )*
    }) => {
        #[allow(non_snake_case)]
        #[derive(Debug, Clone)]
        struct $name { $($field: $ty,)* }
        impl $name {
            fn from_json(mut json: serde_json::Value) -> $name {
                $name {$(
                    $field: get_field(
                        &mut json,
                        stringify!($field),
                        None$(.or(Some(stringify!($alias))))*,
                        $default,
                    ),
                )*}
            }

            fn json_schema() -> serde_json::Value {
                schema(&[
                    $({
                        let field = stringify!($field);
                        let ty = stringify!($ty);

                        (field, ty, &[$($doc),*], $default)
                    },)*
                ])
            }

        }
    };
}
use _config_data as config_data;

fn get_field<T: DeserializeOwned>(
    json: &mut serde_json::Value,
    field: &'static str,
    alias: Option<&'static str>,
    default: serde_json::Value,
) -> T {
    let default = serde_json::from_value(default).unwrap();

    // XXX: check alias first, to work-around the VS Code where it pre-fills the
    // defaults instead of sending an empty object.
    alias
        .into_iter()
        .chain(iter::once(field))
        .find_map(move |field| {
            let mut pointer = field.replace('_', "/");
            pointer.insert(0, '/');
            json.pointer_mut(&pointer)
                .and_then(|it| serde_json::from_value(it.take()).ok())
        })
        .unwrap_or(default)
}
fn schema(
    fields: &[(&'static str, &'static str, &[&str], serde_json::Value)],
) -> serde_json::Value {
    for ((f1, ..), (f2, ..)) in fields.iter().zip(&fields[1..]) {
        fn key(f: &str) -> &str {
            f.split_once('_').map_or(f, |x| x.0)
        }
        assert!(key(f1) <= key(f2), "wrong field order: {:?} {:?}", f1, f2);
    }

    let map = fields
        .iter()
        .map(|(field, ty, doc, default)| {
            let name = format!("elp.{}", field.replace('_', "."));
            let props = field_props(field, ty, doc, default);
            (name, props)
        })
        .collect::<serde_json::Map<_, _>>();
    map.into()
}

fn field_props(
    field: &str,
    ty: &str,
    doc: &[&str],
    default: &serde_json::Value,
) -> serde_json::Value {
    let doc = doc_comment_to_string(doc);
    let doc = doc.trim_end_matches('\n');
    assert!(
        doc.ends_with('.') && doc.starts_with(char::is_uppercase),
        "bad docs for {}: {:?}",
        field,
        doc
    );

    let mut map = serde_json::Map::default();
    macro_rules! set {
        ($($key:literal: $value:tt),*$(,)?) => {{$(
            map.insert($key.into(), serde_json::json!($value));
        )*}};
    }
    set!("markdownDescription": doc);
    set!("default": default);

    match ty {
        "bool" => set!("type": "boolean"),
        "String" => set!("type": "string"),
        "Vec<String>" => set! {
            "type": "array",
            "items": { "type": "string" },
        },
        "Vec<PathBuf>" => set! {
            "type": "array",
            "items": { "type": "string" },
        },
        "FxHashSet<String>" => set! {
            "type": "array",
            "items": { "type": "string" },
            "uniqueItems": true,
        },
        "FxHashMap<String, String>" => set! {
            "type": "object",
        },
        "Option<usize>" => set! {
            "type": ["null", "integer"],
            "minimum": 0,
        },
        "Option<String>" => set! {
            "type": ["null", "string"],
        },
        "Option<PathBuf>" => set! {
            "type": ["null", "string"],
        },
        "Option<bool>" => set! {
            "type": ["null", "boolean"],
        },
        "Option<Vec<String>>" => set! {
            "type": ["null", "array"],
            "items": { "type": "string" },
        },
        _ => panic!("{}: {}", ty, default),
    }

    map.into()
}

fn doc_comment_to_string(doc: &[&str]) -> String {
    doc.iter()
        .map(|it| it.strip_prefix(' ').unwrap_or(it))
        .map(|it| format!("{}\n", it))
        .collect()
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;

    #[test]
    fn generate_package_json_config() {
        let s = Config::json_schema();
        let schema = format!("{:#}", s);
        let mut schema = schema
            .trim_start_matches('{')
            .trim_end_matches('}')
            .replace("\n  ", "\n")
            .trim_start_matches('\n')
            .trim_end()
            .to_string();
        schema.push_str(",\n");

        let s = remove_ws(&schema);

        expect![[r#""elp.ai.enable":{"default":false,"markdownDescription":"EnablesupportforAI-basedcompletions.","type":"boolean"},"elp.diagnostics.disabled":{"default":[],"items":{"type":"string"},"markdownDescription":"ListofELPdiagnosticstodisable.","type":"array","uniqueItems":true},"elp.diagnostics.enableExperimental":{"default":false,"markdownDescription":"WhethertoshowexperimentalELPdiagnosticsthatmight\nhavemorefalsepositivesthanusual.","type":"boolean"},"elp.hoverActions.docLinks.enable":{"default":false,"markdownDescription":"WhethertoshowHoverActionsoftype'docs'.Onlyapplieswhen\n`#elp.hoverActions.enable#`isset.","type":"boolean"},"elp.hoverActions.enable":{"default":false,"markdownDescription":"WhethertoshowHoverActions.","type":"boolean"},"elp.inlayHints.parameterHints.enable":{"default":false,"markdownDescription":"Whethertoshowfunctionparameternameinlayhintsatthecall\nsite.","type":"boolean"},"elp.lens.debug.enable":{"default":false,"markdownDescription":"Whethertoshowthe`Debug`lenses.Onlyapplieswhen\n`#elp.lens.enable#`isset.","type":"boolean"},"elp.lens.enable":{"default":false,"markdownDescription":"WhethertoshowCodeLensesinErlangfiles.","type":"boolean"},"elp.lens.links.enable":{"default":false,"markdownDescription":"Whethertoshowthe`Link`lenses.Onlyapplieswhen\n`#elp.lens.enable#`isset.","type":"boolean"},"elp.lens.run.coverage.enable":{"default":false,"markdownDescription":"Displaycodecoverageinformationwhenrunningtestsviathe\nCodeLenses.Onlyapplieswhen`#elp.lens.enabled`and\n`#elp.lens.run.enable#`areset.","type":"boolean"},"elp.lens.run.enable":{"default":false,"markdownDescription":"Whethertoshowthe`Run`lenses.Onlyapplieswhen\n`#elp.lens.enable#`isset.","type":"boolean"},"elp.lens.run.interactive.enable":{"default":false,"markdownDescription":"Whethertoshowthe`RunInteractive`lenses.Onlyapplieswhen\n`#elp.lens.enable#`isset.","type":"boolean"},"elp.log":{"default":"error","markdownDescription":"ConfigureLSP-basedloggingusingenv_loggersyntax.","type":"string"},"elp.signatureHelp.enable":{"default":false,"markdownDescription":"WhethertoshowSignatureHelp.","type":"boolean"},"elp.typesOnHover.enable":{"default":false,"markdownDescription":"Displaytypeswhenhoveringoverexpressions.","type":"boolean"},"#]]
        .assert_eq(s.as_str());

        expect![[r#"
            "elp.ai.enable": {
              "default": false,
              "markdownDescription": "Enable support for AI-based completions.",
              "type": "boolean"
            },
            "elp.diagnostics.disabled": {
              "default": [],
              "items": {
                "type": "string"
              },
              "markdownDescription": "List of ELP diagnostics to disable.",
              "type": "array",
              "uniqueItems": true
            },
            "elp.diagnostics.enableExperimental": {
              "default": false,
              "markdownDescription": "Whether to show experimental ELP diagnostics that might\nhave more false positives than usual.",
              "type": "boolean"
            },
            "elp.hoverActions.docLinks.enable": {
              "default": false,
              "markdownDescription": "Whether to show Hover Actions of type 'docs'. Only applies when\n`#elp.hoverActions.enable#` is set.",
              "type": "boolean"
            },
            "elp.hoverActions.enable": {
              "default": false,
              "markdownDescription": "Whether to show Hover Actions.",
              "type": "boolean"
            },
            "elp.inlayHints.parameterHints.enable": {
              "default": false,
              "markdownDescription": "Whether to show function parameter name inlay hints at the call\nsite.",
              "type": "boolean"
            },
            "elp.lens.debug.enable": {
              "default": false,
              "markdownDescription": "Whether to show the `Debug` lenses. Only applies when\n`#elp.lens.enable#` is set.",
              "type": "boolean"
            },
            "elp.lens.enable": {
              "default": false,
              "markdownDescription": "Whether to show Code Lenses in Erlang files.",
              "type": "boolean"
            },
            "elp.lens.links.enable": {
              "default": false,
              "markdownDescription": "Whether to show the `Link` lenses. Only applies when\n`#elp.lens.enable#` is set.",
              "type": "boolean"
            },
            "elp.lens.run.coverage.enable": {
              "default": false,
              "markdownDescription": "Display code coverage information when running tests via the\nCode Lenses. Only applies when `#elp.lens.enabled` and\n`#elp.lens.run.enable#` are set.",
              "type": "boolean"
            },
            "elp.lens.run.enable": {
              "default": false,
              "markdownDescription": "Whether to show the `Run` lenses. Only applies when\n`#elp.lens.enable#` is set.",
              "type": "boolean"
            },
            "elp.lens.run.interactive.enable": {
              "default": false,
              "markdownDescription": "Whether to show the `Run Interactive` lenses. Only applies when\n`#elp.lens.enable#` is set.",
              "type": "boolean"
            },
            "elp.log": {
              "default": "error",
              "markdownDescription": "Configure LSP-based logging using env_logger syntax.",
              "type": "string"
            },
            "elp.signatureHelp.enable": {
              "default": false,
              "markdownDescription": "Whether to show Signature Help.",
              "type": "boolean"
            },
            "elp.typesOnHover.enable": {
              "default": false,
              "markdownDescription": "Display types when hovering over expressions.",
              "type": "boolean"
            },
        "#]].assert_eq(schema.as_str());
    }

    fn remove_ws(text: &str) -> String {
        text.replace(char::is_whitespace, "")
    }
}
