/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use lsp_types::CallHierarchyProvider;
use lsp_types::ClientCapabilities;
use lsp_types::CodeActionKind;
use lsp_types::CodeActionOptions;
use lsp_types::CodeActionProvider;
use lsp_types::CodeLensOptions;
use lsp_types::CompletionOptions;
use lsp_types::DefinitionProvider;
use lsp_types::DocumentHighlightProvider;
use lsp_types::DocumentSymbolProvider;
use lsp_types::FoldingRangeProvider;
use lsp_types::Full;
use lsp_types::HoverProvider;
use lsp_types::ImplementationProvider;
use lsp_types::InlayHintOptions;
use lsp_types::InlayHintProvider;
use lsp_types::ReferencesProvider;
use lsp_types::RenameOptions;
use lsp_types::RenameProvider;
use lsp_types::SaveOptions;
use lsp_types::SelectionRangeProvider;
use lsp_types::SemanticTokensLegend;
use lsp_types::SemanticTokensOptions;
use lsp_types::SemanticTokensOptionsRange;
use lsp_types::ServerCapabilities;
use lsp_types::SignatureHelpOptions;
use lsp_types::TextDocumentSync;
use lsp_types::TextDocumentSyncKind;
use lsp_types::TextDocumentSyncOptions;
use lsp_types::TypeDefinitionProvider;
use lsp_types::WorkDoneProgressOptions;
use lsp_types::WorkspaceSymbolProvider;

use crate::semantic_tokens;

pub fn compute(client: &ClientCapabilities) -> ServerCapabilities {
    ServerCapabilities {
        position_encoding: None,
        text_document_sync: Some(TextDocumentSync::Options(TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(TextDocumentSyncKind::Incremental),
            will_save: None,
            will_save_wait_until: None,
            save: Some(SaveOptions::default().into()),
        })),
        notebook_document_sync: None,
        selection_range_provider: Some(SelectionRangeProvider::Bool(true)),
        hover_provider: Some(HoverProvider::Bool(true)),
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
        definition_provider: Some(DefinitionProvider::Bool(true)),
        type_definition_provider: Some(TypeDefinitionProvider::Bool(true)),
        implementation_provider: Some(ImplementationProvider::Bool(true)),
        references_provider: Some(ReferencesProvider::Bool(true)),
        document_highlight_provider: Some(DocumentHighlightProvider::Bool(true)),
        document_symbol_provider: Some(DocumentSymbolProvider::Bool(
            !text_document_symbols_dynamic_registration(client),
        )),
        workspace_symbol_provider: Some(WorkspaceSymbolProvider::Bool(true)),
        code_action_provider: Some(code_action_capabilities(client)),
        // TODO: This will be put behind a GK before shipping
        code_lens_provider: Some(CodeLensOptions {
            resolve_provider: Some(false),
            work_done_progress_options: Default::default(),
        }),
        document_formatting_provider: None,
        document_range_formatting_provider: None,
        document_on_type_formatting_provider: None,
        rename_provider: Some(RenameProvider::RenameOptions(RenameOptions {
            prepare_provider: Some(false),
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
        })),
        document_link_provider: None,
        color_provider: None,
        folding_range_provider: Some(FoldingRangeProvider::Bool(true)),
        declaration_provider: None,
        execute_command_provider: None,
        workspace: None,
        call_hierarchy_provider: Some(CallHierarchyProvider::Bool(true)),
        semantic_tokens_provider: Some(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: semantic_tokens::SUPPORTED_TYPES
                        .iter()
                        .cloned()
                        .map(String::from)
                        .collect(),
                    token_modifiers: semantic_tokens::SUPPORTED_MODIFIERS
                        .iter()
                        .cloned()
                        .map(String::from)
                        .collect(),
                },

                full: Some(Full::SemanticTokensFullDelta(
                    lsp_types::SemanticTokensFullDelta { delta: Some(true) },
                )),
                range: Some(SemanticTokensOptionsRange::Bool(true)),
                work_done_progress_options: Default::default(),
            }
            .into(),
        ),
        moniker_provider: None,
        inlay_hint_provider: Some(InlayHintProvider::InlayHintOptions(InlayHintOptions {
            work_done_progress_options: Default::default(),
            resolve_provider: Some(true),
        })),
        linked_editing_range_provider: None,
        inline_value_provider: None,
        type_hierarchy_provider: None,
        inline_completion_provider: None,
        diagnostic_provider: None,
        experimental: None,
    }
}

pub fn text_document_symbols_dynamic_registration(
    client_capabilities: &ClientCapabilities,
) -> bool {
    let caps = (|| -> _ {
        client_capabilities
            .text_document
            .as_ref()?
            .document_symbol
            .clone()
    })()
    .unwrap_or_default();
    caps.dynamic_registration == Some(true)
}

fn code_action_capabilities(client_caps: &ClientCapabilities) -> CodeActionProvider {
    client_caps
        .text_document
        .as_ref()
        .and_then(|it| it.code_action.as_ref())
        .and_then(|it| it.code_action_literal_support.as_ref())
        .map_or(CodeActionProvider::Bool(true), |_| {
            CodeActionProvider::CodeActionOptions(CodeActionOptions {
                // Advertise support for all built-in CodeActionKinds.
                // Ideally we would base this off of the client capabilities
                // but the client is supposed to fall back gracefully for unknown values.
                code_action_kinds: Some(vec![
                    CodeActionKind::Empty,
                    CodeActionKind::QuickFix,
                    CodeActionKind::Refactor,
                    CodeActionKind::RefactorExtract,
                    CodeActionKind::RefactorInline,
                    CodeActionKind::RefactorRewrite,
                ]),
                resolve_provider: Some(true),
                documentation: None,
                work_done_progress_options: Default::default(),
            })
        })
}

#[cfg(test)]
mod tests {
    use elp_base_db::assert_eq_expected;
    use serde_json::json;

    use super::*;

    #[test]
    fn capabilities_preserve_the_expected_wire_shape() {
        let actual = serde_json::to_value(compute(&ClientCapabilities::default()))
            .expect("server capabilities should serialize");

        for provider in [
            "definitionProvider",
            "typeDefinitionProvider",
            "implementationProvider",
            "referencesProvider",
            "documentHighlightProvider",
            "workspaceSymbolProvider",
            "callHierarchyProvider",
        ] {
            assert_eq_expected!(json!(true), actual[provider]);
        }
        assert_eq_expected!(
            json!([
                "function",
                "macro",
                "namespace",
                "string",
                "struct",
                "typeParameter",
                "variable",
                "generic",
            ]),
            actual["semanticTokensProvider"]["legend"]["tokenTypes"]
        );
        assert_eq_expected!(
            json!([
                "bound",
                "exported_function",
                "exported_type",
                "deprecated_function",
                "type_dynamic",
                "format_specifier",
                "disabled_code",
            ]),
            actual["semanticTokensProvider"]["legend"]["tokenModifiers"]
        );
    }
}
