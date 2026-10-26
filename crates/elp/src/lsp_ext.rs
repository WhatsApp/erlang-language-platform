/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! ELP extensions to the LSP.

use std::path::PathBuf;

use elp_ide::elp_ide_db::assists::AssistUserInput;
use lsp_types::LspNotificationMethod;
use lsp_types::LspRequestMethod;
use lsp_types::MessageDirection;
use lsp_types::Notification;
use lsp_types::Position;
use lsp_types::Request;
use lsp_types::TextDocumentIdentifier;
use lsp_types::TextDocumentPositionParams;
use serde::Deserialize;
use serde::Serialize;

/// Custom data we put into the generic code action 'data' field to
/// tie a code action back to its original context in ELP.
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CodeActionData {
    pub code_action_params: lsp_types::CodeActionParams,
    pub id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user_input: Option<AssistUserInput>,
}

/// Custom data we put into the generic completion item 'data' field to
/// use it in the respective 'resolve' step
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct CompletionData {
    pub position: TextDocumentPositionParams,
}

// ---------------------------------------------------------------------

pub enum ExpandMacro {}

impl Request for ExpandMacro {
    type Params = ExpandMacroParams;
    type Result = Option<ExpandedMacro>;
    const METHOD: LspRequestMethod<'static> = LspRequestMethod::Custom("elp/expandMacro");
    const MESSAGE_DIRECTION: MessageDirection = MessageDirection::ClientToServer;
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ExpandMacroParams {
    pub text_document: TextDocumentIdentifier,
    pub position: Position,
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ExpandedMacro {
    pub name: String,
    pub expansion: String,
}

// ---------------------------------------------------------------------
pub enum StatusNotification {}

#[derive(Debug, Serialize, Deserialize)]
pub enum Status {
    Loading,
    Running,
    ShuttingDown,
    Invalid,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct StatusParams {
    pub status: Status,
}

impl Notification for StatusNotification {
    type Params = StatusParams;
    const METHOD: LspNotificationMethod<'static> = LspNotificationMethod::Custom("elp/status");
    const MESSAGE_DIRECTION: MessageDirection = MessageDirection::ServerToClient;
}

// ---------------------------------------------------------------------

pub enum Ping {}
impl Request for Ping {
    type Params = Vec<String>;
    type Result = String;
    const METHOD: LspRequestMethod<'static> = LspRequestMethod::Custom("elp/ping");
    const MESSAGE_DIRECTION: MessageDirection = MessageDirection::ClientToServer;
}

// ---------------------------------------------------------------------

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Runnable {
    pub label: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<lsp_types::LocationLink>,
    pub kind: RunnableKind,
    pub args: RunnableArgs,
}

impl Runnable {
    pub fn buck2_test(
        runnable: elp_ide::Runnable,
        mode: Option<String>,
        target: String,
        location: Option<lsp_types::LocationLink>,
        workspace_root: PathBuf,
        coverage_enabled: bool,
    ) -> Self {
        Self {
            label: "Buck2".to_string(),
            location,
            kind: RunnableKind::Buck2,
            args: RunnableArgs::Buck2(Buck2RunnableArgs {
                workspace_root,
                command: "test".to_string(),
                args: runnable.buck2_test_args(mode, target.clone(), coverage_enabled),
                target,
                id: runnable.id(),
            }),
        }
    }

    pub fn buck2_run(
        runnable: elp_ide::Runnable,
        target: String,
        location: Option<lsp_types::LocationLink>,
        workspace_root: PathBuf,
    ) -> Self {
        Self {
            label: "Buck2".to_string(),
            location,
            kind: RunnableKind::Buck2,
            args: RunnableArgs::Buck2(Buck2RunnableArgs {
                workspace_root,
                command: "run".to_string(),
                args: runnable.buck2_run_args(target.clone()),
                target,
                id: runnable.id(),
            }),
        }
    }

    pub fn rebar3_ct(
        runnable: elp_ide::Runnable,
        location: Option<lsp_types::LocationLink>,
        workspace_root: PathBuf,
    ) -> Self {
        Self {
            label: "Rebar3".to_string(),
            location,
            kind: RunnableKind::Rebar3,
            args: RunnableArgs::Rebar3(Rebar3RunnableArgs {
                workspace_root,
                command: "ct".to_string(),
                args: runnable.rebar3_test_args(),
            }),
        }
    }

    pub fn rebar3_shell(
        _runnable: elp_ide::Runnable,
        location: Option<lsp_types::LocationLink>,
        workspace_root: PathBuf,
    ) -> Self {
        Self {
            label: "Rebar3".to_string(),
            location,
            kind: RunnableKind::Rebar3,
            args: RunnableArgs::Rebar3(Rebar3RunnableArgs {
                workspace_root,
                command: "as".to_string(),
                args: vec!["test".to_string(), "shell".to_string()],
            }),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum RunnableKind {
    Buck2,
    Rebar3,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
#[serde(untagged)]
pub enum RunnableArgs {
    Buck2(Buck2RunnableArgs),
    Rebar3(Rebar3RunnableArgs),
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Buck2RunnableArgs {
    pub workspace_root: PathBuf,
    pub command: String,
    pub args: Vec<String>,
    pub target: String,
    pub id: String,
}
#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Rebar3RunnableArgs {
    pub workspace_root: PathBuf,
    pub command: String,
    pub args: Vec<String>,
}

pub enum ExternalDocs {}

impl Request for ExternalDocs {
    type Params = lsp_types::TextDocumentPositionParams;
    type Result = Option<Vec<lsp_types::Uri>>;
    const METHOD: LspRequestMethod<'static> = LspRequestMethod::Custom("experimental/externalDocs");
    const MESSAGE_DIRECTION: MessageDirection = MessageDirection::ClientToServer;
}

pub enum HoverRequest {}

impl Request for HoverRequest {
    type Params = lsp_types::HoverParams;
    type Result = Option<Hover>;
    const METHOD: LspRequestMethod<'static> = lsp_types::HoverRequest::METHOD;
    const MESSAGE_DIRECTION: MessageDirection = lsp_types::HoverRequest::MESSAGE_DIRECTION;
}

#[derive(Debug, PartialEq, Clone, Deserialize, Serialize)]
pub struct Hover {
    #[serde(flatten)]
    pub hover: lsp_types::Hover,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub actions: Vec<CommandLinkGroup>,
}

#[derive(Debug, PartialEq, Clone, Default, Deserialize, Serialize)]
pub struct CommandLinkGroup {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    pub commands: Vec<CommandLink>,
}

#[derive(Debug, PartialEq, Clone, Default, Deserialize, Serialize)]
pub struct CommandLink {
    #[serde(flatten)]
    pub command: lsp_types::Command,
}

#[cfg(test)]
mod tests {
    use elp_base_db::assert_eq_expected;
    use serde_json::json;

    use super::*;

    #[test]
    fn command_link_serializes_one_tooltip() {
        let link = CommandLink {
            command: lsp_types::Command {
                title: "Open docs".to_string(),
                command: "elp.openUri".to_string(),
                tooltip: Some("file:///docs".to_string()),
                arguments: None,
            },
        };

        let expected = json!({
            "title": "Open docs",
            "command": "elp.openUri",
            "tooltip": "file:///docs",
        });
        let actual = serde_json::to_value(link).expect("command link should serialize");
        assert_eq_expected!(expected, actual);
    }
}
