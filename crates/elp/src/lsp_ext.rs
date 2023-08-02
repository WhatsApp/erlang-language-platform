/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! ELP extensions to the LSP.

use std::path::PathBuf;

use elp_ide::elp_ide_db::assists::AssistUserInput;
use lsp_types::notification::Notification;
use lsp_types::request::Request;
use lsp_types::Position;
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
    const METHOD: &'static str = "elp/expandMacro";
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
    const METHOD: &'static str = "elp/status";
}

// ---------------------------------------------------------------------

pub enum Ping {}
impl Request for Ping {
    type Params = Vec<String>;
    type Result = String;
    const METHOD: &'static str = "elp/ping";
}

// ---------------------------------------------------------------------

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Runnable {
    pub label: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<lsp_types::LocationLink>,
    pub kind: RunnableKind,
    pub args: Buck2RunnableArgs,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum RunnableKind {
    Buck2,
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
pub enum ExternalDocs {}

impl Request for ExternalDocs {
    type Params = lsp_types::TextDocumentPositionParams;
    type Result = Option<Vec<lsp_types::Url>>;
    const METHOD: &'static str = "experimental/externalDocs";
}
