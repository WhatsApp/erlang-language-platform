/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use lsp_types::CallHierarchyServerCapability;
use lsp_types::ClientCapabilities;
use lsp_types::CodeActionKind;
use lsp_types::CodeActionOptions;
use lsp_types::CodeActionProviderCapability;
use lsp_types::CodeLensOptions;
use lsp_types::CompletionOptions;
use lsp_types::FoldingRangeProviderCapability;
use lsp_types::HoverProviderCapability;
use lsp_types::InlayHintOptions;
use lsp_types::InlayHintServerCapabilities;
use lsp_types::OneOf;
use lsp_types::RenameOptions;
use lsp_types::SaveOptions;
use lsp_types::SelectionRangeProviderCapability;
use lsp_types::SemanticTokensFullOptions;
use lsp_types::SemanticTokensLegend;
use lsp_types::SemanticTokensOptions;
use lsp_types::ServerCapabilities;
use lsp_types::SignatureHelpOptions;
use lsp_types::TextDocumentSyncCapability;
use lsp_types::TextDocumentSyncKind;
use lsp_types::TextDocumentSyncOptions;
use lsp_types::TypeDefinitionProviderCapability;
use lsp_types::WorkDoneProgressOptions;

use crate::semantic_tokens;

pub fn compute(client: &ClientCapabilities) -> ServerCapabilities {
    ServerCapabilities {
        position_encoding: None,
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::INCREMENTAL),
                will_save: None,
                will_save_wait_until: None,
                save: Some(SaveOptions::default().into()),
            },
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(true),
            trigger_characters: Some(
                [":", "#", "?", ".", "-", "\\"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
            ),
            ..Default::default()
        }),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: None,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
        }),
        definition_provider: Some(OneOf::Left(true)),
        type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
        implementation_provider: None,
        references_provider: Some(OneOf::Left(true)),
        document_highlight_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        code_action_provider: Some(code_action_capabilities(client)),
        // TODO: This will be put behind a GK before shipping
        code_lens_provider: Some(CodeLensOptions {
            resolve_provider: Some(false),
        }),
        document_formatting_provider: None,
        document_range_formatting_provider: None,
        document_on_type_formatting_provider: None,
        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(false),
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
        })),
        document_link_provider: None,
        color_provider: None,
        folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
        declaration_provider: None,
        execute_command_provider: None,
        workspace: None,
        call_hierarchy_provider: Some(CallHierarchyServerCapability::Simple(true)),
        semantic_tokens_provider: Some(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: semantic_tokens::SUPPORTED_TYPES.to_vec(),
                    token_modifiers: semantic_tokens::SUPPORTED_MODIFIERS.to_vec(),
                },

                full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
                range: Some(true),
                work_done_progress_options: Default::default(),
            }
            .into(),
        ),
        moniker_provider: None,
        inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
            InlayHintOptions {
                work_done_progress_options: Default::default(),
                resolve_provider: Some(true),
            },
        ))),
        linked_editing_range_provider: None,
        experimental: None,
    }
}

fn code_action_capabilities(client_caps: &ClientCapabilities) -> CodeActionProviderCapability {
    client_caps
        .text_document
        .as_ref()
        .and_then(|it| it.code_action.as_ref())
        .and_then(|it| it.code_action_literal_support.as_ref())
        .map_or(CodeActionProviderCapability::Simple(true), |_| {
            CodeActionProviderCapability::Options(CodeActionOptions {
                // Advertise support for all built-in CodeActionKinds.
                // Ideally we would base this off of the client capabilities
                // but the client is supposed to fall back gracefully for unknown values.
                code_action_kinds: Some(vec![
                    CodeActionKind::EMPTY,
                    CodeActionKind::QUICKFIX,
                    CodeActionKind::REFACTOR,
                    CodeActionKind::REFACTOR_EXTRACT,
                    CodeActionKind::REFACTOR_INLINE,
                    CodeActionKind::REFACTOR_REWRITE,
                ]),
                resolve_provider: Some(true),
                work_done_progress_options: Default::default(),
            })
        })
}
