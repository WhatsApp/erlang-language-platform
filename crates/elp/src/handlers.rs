/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This module is responsible for implementing handlers for Language Server
//! Protocol. The majority of requests are fulfilled by calling into the
//! `ide` crate.

use anyhow::bail;
use anyhow::Result;
use elp_ide::elp_ide_assists::AssistKind;
use elp_ide::elp_ide_assists::AssistResolveStrategy;
use elp_ide::elp_ide_assists::SingleResolve;
use elp_ide::elp_ide_completion::Completion;
use elp_ide::elp_ide_completion::Kind;
use elp_ide::elp_ide_db::assists::AssistContextDiagnostic;
use elp_ide::elp_ide_db::docs::Doc;
use elp_ide::elp_ide_db::elp_base_db::FilePosition;
use elp_ide::elp_ide_db::elp_base_db::FileRange;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::LineIndex;
use elp_ide::elp_ide_db::SymbolKind;
use elp_ide::Cancellable;
use elp_ide::HighlightedRange;
use elp_ide::RangeInfo;
use elp_ide::TextRange;
use itertools::Itertools;
use lsp_server::ErrorCode;
use lsp_types::CallHierarchyIncomingCall;
use lsp_types::CallHierarchyIncomingCallsParams;
use lsp_types::CallHierarchyItem;
use lsp_types::CallHierarchyOutgoingCall;
use lsp_types::CallHierarchyOutgoingCallsParams;
use lsp_types::CallHierarchyPrepareParams;
use lsp_types::CodeLens;
use lsp_types::CompletionItem;
use lsp_types::Diagnostic;
use lsp_types::DocumentSymbol;
use lsp_types::FoldingRange;
use lsp_types::FoldingRangeParams;
use lsp_types::HoverParams;
use lsp_types::RenameParams;
use lsp_types::SemanticTokensDeltaParams;
use lsp_types::SemanticTokensFullDeltaResult;
use lsp_types::SemanticTokensParams;
use lsp_types::SemanticTokensRangeParams;
use lsp_types::SemanticTokensRangeResult;
use lsp_types::SemanticTokensResult;
use lsp_types::SymbolInformation;
use lsp_types::TextDocumentIdentifier;
use lsp_types::Url;
use lsp_types::WorkspaceEdit;

use crate::convert::lsp_to_assist_context_diagnostic;
use crate::from_proto;
use crate::lsp_ext;
use crate::snapshot::Snapshot;
use crate::to_proto;
use crate::LspError;

pub(crate) fn handle_code_action(
    snap: Snapshot,
    params: lsp_types::CodeActionParams,
) -> Result<Option<Vec<lsp_types::CodeActionOrCommand>>> {
    let _p = profile::span("handle_code_action");

    if !snap.config.code_action_literals() {
        // We intentionally don't support command-based actions, as those either
        // require either custom client-code or server-initiated edits. Server
        // initiated edits break causality, so we avoid those.
        return Ok(None);
    }

    let mut frange = from_proto::file_range(&snap, params.text_document.clone(), params.range)?;
    frange.range = snap.analysis.clamp_range(frange.file_id, frange.range)?;

    let mut assists_config = snap.config.assist();
    assists_config.allowed = params
        .context
        .only
        .clone()
        .map(|it| it.into_iter().filter_map(from_proto::assist_kind).collect());

    let mut res: Vec<lsp_types::CodeActionOrCommand> = Vec::new();

    let code_action_resolve_cap = snap.config.code_action_resolve();
    let resolve = if code_action_resolve_cap {
        AssistResolveStrategy::None
    } else {
        AssistResolveStrategy::All
    };

    let file_id = snap.url_to_file_id(&params.text_document.uri)?;
    let line_index = snap.analysis.line_index(file_id)?;
    let diagnostics = params.clone().context.diagnostics;
    let assist_context_diagnostics = to_assist_context_diagnostics(&line_index, diagnostics);
    let assists = snap.analysis.assists_with_fixes(
        &assists_config,
        &snap.config.diagnostics(snap.ad_hoc_lints.clone()),
        resolve,
        frange,
        &assist_context_diagnostics,
        &snap.diagnostics,
        None,
    )?;
    for (index, assist) in assists.into_iter().enumerate() {
        let resolve_data = if code_action_resolve_cap {
            Some((index, params.clone(), assist.user_input.clone()))
        } else {
            None
        };
        let code_action = to_proto::code_action(&snap, assist, resolve_data)?;
        res.push(code_action)
    }

    Ok(Some(res))
}

pub(crate) fn handle_code_action_resolve(
    snap: Snapshot,
    mut code_action: lsp_types::CodeAction,
) -> Result<lsp_types::CodeAction> {
    let _p = profile::span("handle_code_action_resolve");
    let params_raw = match code_action.data.take() {
        Some(it) => it,
        None => bail!("can't resolve code action without data"),
    };

    let params: lsp_ext::CodeActionData = serde::Deserialize::deserialize(params_raw)?;

    let file_id = snap.url_to_file_id(&params.code_action_params.text_document.uri)?;
    let line_index = snap.analysis.line_index(file_id)?;
    // Temporary for T147609435
    let _pctx = stdx::panic_context::enter("\nhandle_code_action_resolve".to_string());
    let range = from_proto::text_range(&line_index, params.code_action_params.range);
    let mut frange = FileRange { file_id, range };
    frange.range = snap.analysis.clamp_range(frange.file_id, frange.range)?;

    let mut assists_config = snap.config.assist();
    assists_config.allowed = params
        .code_action_params
        .context
        .only
        .map(|it| it.into_iter().filter_map(from_proto::assist_kind).collect());

    let (assist_index, assist_resolve) = match parse_action_id(&params.id) {
        Ok(parsed_data) => parsed_data,
        Err(e) => {
            return Err(LspError::new(
                ErrorCode::InvalidParams as i32,
                format!("Failed to parse action id string '{}': {}", params.id, e),
            )
            .into());
        }
    };

    let expected_assist_id = assist_resolve.assist_id.clone();
    let expected_kind = assist_resolve.assist_kind;

    let diagnostics = params.code_action_params.context.diagnostics;
    let assist_context_diagnostics = to_assist_context_diagnostics(&line_index, diagnostics);
    let assists = snap.analysis.assists_with_fixes(
        &assists_config,
        &snap.config.diagnostics(snap.ad_hoc_lints.clone()),
        AssistResolveStrategy::Single(assist_resolve),
        frange,
        &assist_context_diagnostics,
        &snap.diagnostics,
        params.user_input,
    )?;

    let assist = match assists.get(assist_index) {
        Some(assist) => assist,
        None => return Err(LspError::new(
            ErrorCode::InvalidParams as i32,
            format!(
                "Failed to find the assist for index {} provided by the resolve request. Resolve request assist id: {}",
                assist_index, params.id,
            ),
        )
            .into())
    };
    if assist.id.0 != expected_assist_id || assist.id.1 != expected_kind {
        return Err(LspError::new(
            ErrorCode::InvalidParams as i32,
            format!(
                "Mismatching assist at index {} for the resolve parameters given. Resolve request assist id: {}, actual id: {:?}.",
                assist_index, params.id, assist.id
            ),
        )
            .into());
    }
    match to_proto::code_action(&snap, assist.clone(), None)? {
        lsp_types::CodeActionOrCommand::Command(_) => {}
        lsp_types::CodeActionOrCommand::CodeAction(ca) => code_action.edit = ca.edit,
    }
    Ok(code_action)
}

fn parse_action_id(action_id: &str) -> Result<(usize, SingleResolve), String> {
    let id_parts = action_id.split(':').collect_vec();
    match id_parts.as_slice() {
        &[assist_id_string, assist_kind_string, index_string] => {
            let assist_kind: AssistKind = assist_kind_string.parse()?;
            let index: usize = match index_string.parse() {
                Ok(index) => index,
                Err(e) => return Err(format!("Incorrect index string: {}", e)),
            };
            Ok((
                index,
                SingleResolve {
                    assist_id: assist_id_string.to_string(),
                    assist_kind,
                },
            ))
        }
        _ => Err("Action id contains incorrect number of segments".to_string()),
    }
}

pub(crate) fn handle_expand_macro(
    snap: Snapshot,
    params: lsp_ext::ExpandMacroParams,
) -> Result<Option<lsp_ext::ExpandedMacro>> {
    let _p = profile::span("handle_expand_macro");
    let file_id = from_proto::file_id(&snap, &params.text_document.uri)?;
    let line_index = snap.analysis.line_index(file_id)?;
    let mut offset = from_proto::offset(&line_index, params.position);
    offset = snap.analysis.clamp_offset(file_id, offset)?;

    let res = snap
        .analysis
        .expand_macro(FilePosition { file_id, offset })?;
    match res {
        Some(it) => Ok(Some(lsp_ext::ExpandedMacro {
            name: it.name,
            expansion: it.expansion,
        })),
        None => Ok(Some(lsp_ext::ExpandedMacro {
            name: "Expansion Failed".to_string(),
            expansion: "".to_string(),
        })),
    }
}

pub(crate) fn pong(_: Snapshot, _: Vec<String>) -> Result<String> {
    Ok("pong".to_string())
}

pub(crate) fn handle_selection_range(
    snap: Snapshot,
    params: lsp_types::SelectionRangeParams,
) -> Result<Option<Vec<lsp_types::SelectionRange>>> {
    let _p = profile::span("handle_selection_range");
    let file_id = from_proto::file_id(&snap, &params.text_document.uri)?;
    let line_index = snap.analysis.line_index(file_id)?;
    let res: Result<Vec<lsp_types::SelectionRange>> = params
        .positions
        .into_iter()
        .map(|position| {
            let mut offset = from_proto::offset(&line_index, position);
            offset = snap.analysis.clamp_offset(file_id, offset)?;
            let mut ranges = Vec::new();
            {
                let mut range = TextRange::new(offset, offset);
                loop {
                    ranges.push(range);
                    let frange = FileRange { file_id, range };
                    let next = snap.analysis.extend_selection(frange)?;
                    if next == range {
                        break;
                    } else {
                        range = next
                    }
                }
            }
            let mut range = lsp_types::SelectionRange {
                range: to_proto::range(&line_index, *ranges.last().unwrap()),
                parent: None,
            };
            for &r in ranges.iter().rev().skip(1) {
                range = lsp_types::SelectionRange {
                    range: to_proto::range(&line_index, r),
                    parent: Some(Box::new(range)),
                }
            }
            Ok(range)
        })
        .collect();

    Ok(Some(res?))
}

pub(crate) fn handle_goto_definition(
    snap: Snapshot,
    params: lsp_types::GotoDefinitionParams,
) -> Result<Option<lsp_types::GotoDefinitionResponse>> {
    let _p = profile::span("handle_goto_definition");
    let mut position = from_proto::file_position(&snap, params.text_document_position_params)?;
    position.offset = snap
        .analysis
        .clamp_offset(position.file_id, position.offset)?;

    let nav_info = match snap.analysis.goto_definition(position)? {
        None => return Ok(None),
        Some(it) => it,
    };
    let src = FileRange {
        file_id: position.file_id,
        range: nav_info.range,
    };
    let res = to_proto::goto_definition_response(&snap, Some(src), nav_info.info)?;
    Ok(Some(res))
}

pub(crate) fn handle_references(
    snap: Snapshot,
    params: lsp_types::ReferenceParams,
) -> Result<Option<Vec<lsp_types::Location>>> {
    let _p = profile::span("handle_references");
    let mut position = from_proto::file_position(&snap, params.text_document_position)?;
    position.offset = snap
        .analysis
        .clamp_offset(position.file_id, position.offset)?;
    let refs = match snap.analysis.find_all_refs(position)? {
        None => return Ok(None),
        Some(it) => it,
    };
    let include_declaration = params.context.include_declaration;
    let locations = refs
        .into_iter()
        .flat_map(|refs| {
            let decl = if include_declaration {
                to_proto::location_from_nav(&snap, refs.declaration).ok()
            } else {
                None
            };
            refs.references
                .into_iter()
                .flat_map(|(file_id, refs)| {
                    refs.into_iter()
                        .map(move |range| FileRange { file_id, range })
                        .flat_map(|range| to_proto::location(&snap, range).ok())
                })
                .chain(decl)
        })
        .collect();
    Ok(Some(locations))
}

pub(crate) fn handle_completion(
    snap: Snapshot,
    params: lsp_types::CompletionParams,
) -> Result<Option<lsp_types::CompletionResponse>> {
    let _p = profile::span("handle_completion");
    let mut position = from_proto::file_position(&snap, params.text_document_position)?;
    position.offset = snap
        .analysis
        .clamp_offset(position.file_id, position.offset)?;
    let completion_trigger_character = params
        .context
        .and_then(|ctx| ctx.trigger_character)
        .and_then(|s| s.chars().next());

    let ai_receiver =
        if completion_trigger_character.is_none() || completion_trigger_character != Some(':') {
            elp_ai::always(None)
        } else {
            snap.ai_completion(position)?
        };

    let mut completions = snap
        .analysis
        .completions(position, completion_trigger_character)?;

    let ai_result = if let Ok(Some(ai_result)) = ai_receiver.recv() {
        ai_result
    } else {
        return Ok(Some(to_proto::completion_response(snap, completions)));
    };

    if completions.is_empty() {
        completions.push(Completion {
            label: ai_result,
            kind: Kind::AiAssist,
            contents: elp_ide::elp_ide_completion::Contents::SameAsLabel,
            position: None,
            sort_text: Some("\0".to_string()),
            deprecated: false,
        });
    } else {
        for c in completions.iter_mut() {
            let split_char = '/';
            let parts: Vec<&str> = c.label.splitn(2, split_char).collect();
            let fname = parts[0];
            if fname == ai_result {
                c.sort_text = Some("\0".to_string());
                c.kind = Kind::AiAssist;
            } else if fname.starts_with(&ai_result) {
                c.sort_text = Some("\x01".to_string());
                c.kind = Kind::AiAssist;
            }
        }
    }

    Ok(Some(to_proto::completion_response(snap, completions)))
}

pub(crate) fn handle_completion_resolve(
    snap: Snapshot,
    mut original_completion: CompletionItem,
) -> Result<CompletionItem> {
    let _p = profile::span("handle_completion_resolve");

    if let Some(data) = original_completion.clone().data {
        let data: lsp_ext::CompletionData = serde_json::from_value(data)?;
        if let Ok(mut position) = from_proto::file_position(&snap, data.position) {
            position.offset = snap
                .analysis
                .clamp_offset(position.file_id, position.offset)?;
            if let Ok(Some(res)) = snap.analysis.get_docs_at_position(position) {
                let docs = res.0.markdown_text().to_string();
                let documentation =
                    lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
                        kind: lsp_types::MarkupKind::Markdown,
                        value: docs,
                    });
                original_completion.documentation = Some(documentation)
            }
        }
    }
    Ok(original_completion)
}

pub(crate) fn handle_document_symbol(
    snap: Snapshot,
    params: lsp_types::DocumentSymbolParams,
) -> Result<Option<lsp_types::DocumentSymbolResponse>> {
    let _p = profile::span("handle_document_symbol");
    let file_id = from_proto::file_id(&snap, &params.text_document.uri)?;
    let line_index = snap.analysis.line_index(file_id)?;

    let res: Vec<DocumentSymbol> = snap
        .analysis
        .document_symbols(file_id)?
        .iter()
        .map(|symbol| to_proto::document_symbol(&line_index, symbol))
        .collect();
    Ok(Some(res.into()))
}

pub(crate) fn handle_workspace_symbol(
    snap: Snapshot,
    params: lsp_types::WorkspaceSymbolParams,
) -> Result<Option<Vec<SymbolInformation>>> {
    let _p = profile::span("handle_workspace_symbol");

    let mut res = Vec::new();
    for (project_id, _project) in snap.projects.iter().enumerate() {
        let project_id = ProjectId(project_id as u32);
        for nav in snap.analysis.symbol_search(project_id, &params.query)? {
            #[allow(deprecated)]
            let info = SymbolInformation {
                name: nav.name.to_string(),
                kind: to_proto::symbol_kind(nav.kind),
                tags: None,
                location: to_proto::location_from_nav(&snap, nav)?,
                container_name: None,
                deprecated: None,
            };
            res.push(info);
        }
    }
    res.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(Some(res))
}

pub(crate) fn handle_rename(snap: Snapshot, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
    let _p = profile::span("handle_rename");
    let mut position = from_proto::file_position(&snap, params.text_document_position)?;
    position.offset = snap
        .analysis
        .clamp_offset(position.file_id, position.offset)?;

    let change = snap
        .analysis
        .rename(position, &params.new_name)?
        .map_err(to_proto::rename_error)?;

    let workspace_edit = to_proto::workspace_edit(&snap, change)?;
    Ok(Some(workspace_edit))
}

fn to_assist_context_diagnostics(
    line_index: &LineIndex,
    diagnostics: Vec<Diagnostic>,
) -> Vec<AssistContextDiagnostic> {
    diagnostics
        .into_iter()
        .filter_map(|d| lsp_to_assist_context_diagnostic(line_index, d))
        .collect()
}

pub(crate) fn handle_hover(snap: Snapshot, params: HoverParams) -> Result<Option<lsp_ext::Hover>> {
    let _p = profile::span("handle_hover");
    let mut position = from_proto::file_position(&snap, params.text_document_position_params)?;
    position.offset = snap
        .analysis
        .clamp_offset(position.file_id, position.offset)?;
    let query_range = FileRange {
        file_id: position.file_id,
        range: TextRange::empty(position.offset),
    };

    let mut docs: Vec<(Doc, Option<FileRange>)> = Vec::default();

    if snap.config.types_on_hover() {
        if let Some(project_id) = snap.analysis.project_id(position.file_id)? {
            if let Some(type_info) = snap.analysis.type_at_position(project_id, query_range)? {
                let (ty, range) = &*type_info;
                let text = &snap.analysis.file_text(range.file_id)?[range.range];
                let type_doc = Doc::new(format!("```erlang\n{} :: {}\n```\n", text, ty));
                docs.push((type_doc, Some(range.to_owned())));
                let refs = snap.analysis.type_references(project_id, ty)?;
                if !refs.is_empty() {
                    let goto_list = refs
                        .into_iter()
                        .flat_map(|(name, range)| {
                            to_proto::location(&snap, range)
                                .map(|loc| {
                                    format!(
                                        "[{}]({}#L{}-{})",
                                        name,
                                        loc.uri,
                                        loc.range.start.line + 1,
                                        loc.range.end.line + 1
                                    )
                                })
                                .ok()
                        })
                        .join(" | ");
                    let goto_docs = Doc::new(format!("Go to: {}", goto_list));
                    docs.push((goto_docs, None));
                }
            }
        }
    }

    if let Some(hover) = snap.analysis.get_docs_at_position(position)? {
        docs.push(hover);
    }

    if let Some(macro_expansion) = snap.analysis.expand_macro(position)? {
        let hover_info = (
            Doc::new(format!(
                "{}\n\n```erlang\n{}```\n",
                macro_expansion.name, macro_expansion.expansion
            )),
            None,
        );
        docs.push(hover_info);
    }

    let hover_actions_config = snap.config.hover_actions();
    let actions = snap
        .analysis
        .hover_actions(position, &hover_actions_config)?;
    to_proto::hover_response(&snap, combine_docs(&docs), actions)
}

fn combine_docs(docs: &[(Doc, Option<FileRange>)]) -> Option<(Doc, Option<FileRange>)> {
    match docs {
        [] => None,
        [(doc, src_range)] => Some((doc.clone(), *src_range)),
        many => Some((
            Doc::new(
                many.iter()
                    .map(|d| d.0.markdown_text())
                    .collect_vec()
                    .join("\n\n---\n\n"),
            ),
            many.iter().fold(None, |acc, (_, mr)| match (acc, mr) {
                (None, None) => None,
                (None, Some(_)) => *mr,
                (Some(_), None) => acc,
                (Some(a), Some(b)) => Some(FileRange {
                    file_id: a.file_id,
                    range: a.range.cover(b.range),
                }),
            }),
        )),
    }
}

pub(crate) fn handle_folding_range(
    snap: Snapshot,
    params: FoldingRangeParams,
) -> Result<Option<Vec<FoldingRange>>> {
    let _p = profile::span("handle_folding_range");
    let file_id = from_proto::file_id(&snap, &params.text_document.uri)?;
    let folds = snap.analysis.folding_ranges(file_id)?;
    let line_index = snap.analysis.line_index(file_id)?;
    let res = folds
        .into_iter()
        .map(|it| to_proto::folding_range(&line_index, it))
        .collect();
    Ok(Some(res))
}

pub(crate) fn handle_document_highlight(
    snap: Snapshot,
    params: lsp_types::DocumentHighlightParams,
) -> Result<Option<Vec<lsp_types::DocumentHighlight>>> {
    let _p = profile::span("handle_document_highlight");
    let mut position = from_proto::file_position(&snap, params.text_document_position_params)?;
    position.offset = snap
        .analysis
        .clamp_offset(position.file_id, position.offset)?;

    let line_index = snap.analysis.line_index(position.file_id)?;

    let refs = match snap.analysis.highlight_related(position)? {
        None => return Ok(None),
        Some(refs) => refs,
    };
    let res = refs
        .into_iter()
        .map(
            |HighlightedRange { range, category }| lsp_types::DocumentHighlight {
                range: to_proto::range(&line_index, range),
                kind: category.and_then(to_proto::document_highlight_kind),
            },
        )
        .collect();
    Ok(Some(res))
}

pub(crate) fn handle_call_hierarchy_prepare(
    snap: Snapshot,
    params: CallHierarchyPrepareParams,
) -> Result<Option<Vec<CallHierarchyItem>>> {
    let _p = profile::span("handle_call_hierarchy_prepare");
    let mut position = from_proto::file_position(&snap, params.text_document_position_params)?;
    position.offset = snap
        .analysis
        .clamp_offset(position.file_id, position.offset)?;

    let nav_info = match snap.analysis.call_hierarchy_prepare(position)? {
        None => return Ok(None),
        Some(it) => it,
    };

    let RangeInfo {
        range: _,
        info: navs,
    } = nav_info;
    let res = navs
        .into_iter()
        .filter(|it| it.kind == SymbolKind::Function)
        .map(|it| to_proto::call_hierarchy_item(&snap, it))
        .collect::<Result<Vec<_>>>()?;

    Ok(Some(res))
}

pub(crate) fn handle_call_hierarchy_incoming(
    snap: Snapshot,
    params: CallHierarchyIncomingCallsParams,
) -> Result<Option<Vec<CallHierarchyIncomingCall>>> {
    let _p = profile::span("handle_call_hierarchy_incoming");
    let item = params.item;

    let doc = TextDocumentIdentifier::new(item.uri);
    let mut frange = from_proto::file_range(&snap, doc, item.selection_range)?;
    frange.range = snap.analysis.clamp_range(frange.file_id, frange.range)?;

    let fpos = FilePosition {
        file_id: frange.file_id,
        offset: frange.range.start(),
    };

    let call_items = match snap.analysis.incoming_calls(fpos)? {
        None => return Ok(None),
        Some(it) => it,
    };

    let mut res = vec![];

    for call_item in call_items.into_iter() {
        let file_id = call_item.target.file_id;
        let line_index = snap.analysis.line_index(file_id)?;
        let item = to_proto::call_hierarchy_item(&snap, call_item.target)?;
        res.push(CallHierarchyIncomingCall {
            from: item,
            from_ranges: call_item
                .ranges
                .into_iter()
                .map(|it| to_proto::range(&line_index, it))
                .collect(),
        });
    }

    Ok(Some(res))
}

pub(crate) fn handle_call_hierarchy_outgoing(
    snap: Snapshot,
    params: CallHierarchyOutgoingCallsParams,
) -> Result<Option<Vec<CallHierarchyOutgoingCall>>> {
    let _p = profile::span("handle_call_hierarchy_outgoing");
    let item = params.item;

    let doc = TextDocumentIdentifier::new(item.uri);
    let mut frange = from_proto::file_range(&snap, doc, item.selection_range)?;
    frange.range = snap.analysis.clamp_range(frange.file_id, frange.range)?;
    let fpos = FilePosition {
        file_id: frange.file_id,
        offset: frange.range.start(),
    };

    let call_items = match snap.analysis.outgoing_calls(fpos)? {
        None => return Ok(None),
        Some(it) => it,
    };

    let mut res = vec![];

    for call_item in call_items.into_iter() {
        let file_id = call_item.target.file_id;
        let line_index = snap.analysis.line_index(file_id)?;
        let item = to_proto::call_hierarchy_item(&snap, call_item.target)?;
        res.push(CallHierarchyOutgoingCall {
            to: item,
            from_ranges: call_item
                .ranges
                .into_iter()
                .map(|it| to_proto::range(&line_index, it))
                .collect(),
        });
    }

    Ok(Some(res))
}

pub(crate) fn handle_signature_help(
    snap: Snapshot,
    params: lsp_types::SignatureHelpParams,
) -> Result<Option<lsp_types::SignatureHelp>> {
    let _p = profile::span("handle_signature_help");

    if !snap.config.signature_help() {
        // early return before any db query!
        return Ok(None);
    }

    let mut position = from_proto::file_position(&snap, params.text_document_position_params)?;
    position.offset = snap
        .analysis
        .clamp_offset(position.file_id, position.offset)?;

    let (help, active_parameter) = match snap.analysis.signature_help(position)? {
        Some((h, Some(ap))) => (h, ap),
        _ => return Ok(None),
    };
    let res = to_proto::signature_help(help, active_parameter);
    Ok(Some(res))
}

// ---------------------------------------------------------------------

pub(crate) fn handle_semantic_tokens_full(
    snap: Snapshot,
    params: SemanticTokensParams,
) -> Result<Option<SemanticTokensResult>> {
    let _p = profile::span("handle_semantic_tokens_full");

    let file_id = from_proto::file_id(&snap, &params.text_document.uri)?;
    let text = snap.analysis.file_text(file_id)?;
    let line_index = snap.analysis.line_index(file_id)?;

    let highlights = snap.analysis.highlight(file_id)?;
    let semantic_tokens = to_proto::semantic_tokens(&text, &line_index, highlights);

    // Unconditionally cache the tokens
    snap.semantic_tokens_cache
        .lock()
        .insert(params.text_document.uri, semantic_tokens.clone());

    Ok(Some(semantic_tokens.into()))
}

pub(crate) fn handle_semantic_tokens_full_delta(
    snap: Snapshot,
    params: SemanticTokensDeltaParams,
) -> Result<Option<SemanticTokensFullDeltaResult>> {
    let _p = profile::span("handle_semantic_tokens_full_delta");

    let file_id = from_proto::file_id(&snap, &params.text_document.uri)?;
    let text = snap.analysis.file_text(file_id)?;
    let line_index = snap.analysis.line_index(file_id)?;

    let highlights = snap.analysis.highlight(file_id)?;
    let semantic_tokens = to_proto::semantic_tokens(&text, &line_index, highlights);

    let mut cache = snap.semantic_tokens_cache.lock();
    let cached_tokens = cache.entry(params.text_document.uri).or_default();

    if let Some(prev_id) = &cached_tokens.result_id {
        if *prev_id == params.previous_result_id {
            let delta = to_proto::semantic_token_delta(cached_tokens, &semantic_tokens);
            *cached_tokens = semantic_tokens;
            return Ok(Some(delta.into()));
        }
    }

    *cached_tokens = semantic_tokens.clone();

    Ok(Some(semantic_tokens.into()))
}

pub(crate) fn handle_semantic_tokens_range(
    snap: Snapshot,
    params: SemanticTokensRangeParams,
) -> Result<Option<SemanticTokensRangeResult>> {
    let _p = profile::span("handle_semantic_tokens_range");

    let mut frange = from_proto::file_range(&snap, params.text_document, params.range)?;
    frange.range = snap.analysis.clamp_range(frange.file_id, frange.range)?;
    let text = snap.analysis.file_text(frange.file_id)?;
    let line_index = snap.analysis.line_index(frange.file_id)?;

    let highlights = snap.analysis.highlight_range(frange)?;
    let semantic_tokens = to_proto::semantic_tokens(&text, &line_index, highlights);
    Ok(Some(semantic_tokens.into()))
}

pub(crate) fn handle_code_lens(
    snap: Snapshot,
    params: lsp_types::CodeLensParams,
) -> Result<Option<Vec<CodeLens>>> {
    let _p = profile::span("handle_code_lens");

    let mut res = Vec::new();
    let lens_config = snap.config.lens();

    let file_id = from_proto::file_id(&snap, &params.text_document.uri)?;
    let line_index = snap.analysis.line_index(file_id)?;
    if let Ok(Some(project_id)) = snap.analysis.project_id(file_id) {
        let annotations = snap.analysis.annotations(file_id)?;
        if let Some(project_build_data) = snap
            .get_project(project_id)
            .map(|project| project.project_build_data)
        {
            for a in annotations {
                to_proto::code_lens(
                    &mut res,
                    &snap,
                    &lens_config,
                    &line_index,
                    a,
                    &project_build_data,
                );
            }
        }
    }

    Ok(Some(res))
}

pub(crate) fn handle_external_docs(
    snap: Snapshot,
    params: lsp_types::TextDocumentPositionParams,
) -> Result<Option<Vec<lsp_types::Url>>> {
    let _p = profile::span("handle_external_docs");

    let mut position = from_proto::file_position(&snap, params)?;
    position.offset = snap
        .analysis
        .clamp_offset(position.file_id, position.offset)?;

    let docs = snap.analysis.external_docs(position)?;
    Ok(docs.map(|links| {
        links
            .iter()
            .filter_map(|link| Url::parse(&link.uri).ok())
            .collect()
    }))
}

pub(crate) fn handle_inlay_hints(
    snap: Snapshot,
    params: lsp_types::InlayHintParams,
) -> Result<Option<Vec<lsp_types::InlayHint>>> {
    let _p = profile::span("handle_inlay_hints");
    let document_uri = &params.text_document.uri;
    let mut frange = from_proto::file_range(
        &snap,
        TextDocumentIdentifier::new(document_uri.to_owned()),
        params.range,
    )?;
    frange.range = snap.analysis.clamp_range(frange.file_id, frange.range)?;
    let line_index = snap.analysis.line_index(frange.file_id)?;
    let inlay_hints_config = snap.config.inlay_hints();
    Ok(Some(
        snap.analysis
            .inlay_hints(&inlay_hints_config, frange.file_id, Some(frange.range))?
            .into_iter()
            .map(|it| to_proto::inlay_hint(&snap, &line_index, it))
            .collect::<Cancellable<Vec<_>>>()?,
    ))
}

pub(crate) fn handle_inlay_hints_resolve(
    _snap: Snapshot,
    hint: lsp_types::InlayHint,
) -> Result<lsp_types::InlayHint> {
    let _p = profile::span("handle_inlay_hints_resolve");
    Ok(hint)
}

// ---------------------------------------------------------------------
