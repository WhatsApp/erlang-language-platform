/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Conversion of rust-analyzer specific types to lsp_types equivalents.

use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

use elp_ide::elp_ide_assists::Assist;
use elp_ide::elp_ide_assists::AssistKind;
use elp_ide::elp_ide_completion::Completion;
use elp_ide::elp_ide_completion::Contents;
use elp_ide::elp_ide_completion::Kind;
use elp_ide::elp_ide_db::assists::AssistUserInput;
use elp_ide::elp_ide_db::docs::Doc;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_ide::elp_ide_db::elp_base_db::FilePosition;
use elp_ide::elp_ide_db::elp_base_db::FileRange;
use elp_ide::elp_ide_db::rename::RenameError;
use elp_ide::elp_ide_db::source_change::SourceChange;
use elp_ide::elp_ide_db::LineIndex;
use elp_ide::elp_ide_db::ReferenceCategory;
use elp_ide::elp_ide_db::SymbolKind;
use elp_ide::AnnotationKind;
use elp_ide::Cancellable;
use elp_ide::DocLink;
use elp_ide::Fold;
use elp_ide::FoldKind;
use elp_ide::Highlight;
use elp_ide::HlMod;
use elp_ide::HlRange;
use elp_ide::HlTag;
use elp_ide::HoverAction;
use elp_ide::InlayHintLabel;
use elp_ide::InlayHintLabelPart;
use elp_ide::InlayKind;
use elp_ide::NavigationTarget;
use elp_ide::Runnable;
use elp_ide::RunnableKind;
use elp_ide::SignatureHelp;
use elp_ide::TextRange;
use elp_ide::TextSize;
use elp_project_model::ProjectBuildData;
use lsp_types::CompletionItemTag;
use lsp_types::Hover;
use lsp_types::HoverContents;
use lsp_types::MarkupContent;
use lsp_types::MarkupKind;
use text_edit::Indel;
use text_edit::TextEdit;

use crate::config::LensConfig;
use crate::line_endings::LineEndings;
use crate::lsp_ext;
use crate::lsp_ext::CompletionData;
use crate::semantic_tokens;
use crate::snapshot::Snapshot;
use crate::LspError;
use crate::Result;

pub(crate) fn position(line_index: &LineIndex, offset: TextSize) -> lsp_types::Position {
    let line_col = line_index.line_col(offset);
    lsp_types::Position::new(line_col.line, line_col.col_utf16)
}

pub(crate) fn range(line_index: &LineIndex, range: TextRange) -> lsp_types::Range {
    let start = position(line_index, range.start());
    let end = position(line_index, range.end());
    lsp_types::Range::new(start, end)
}

pub(crate) fn symbol_kind(symbol_kind: SymbolKind) -> lsp_types::SymbolKind {
    match symbol_kind {
        SymbolKind::Function => lsp_types::SymbolKind::FUNCTION,
        SymbolKind::Record => lsp_types::SymbolKind::STRUCT,
        SymbolKind::Type => lsp_types::SymbolKind::TYPE_PARAMETER,
        SymbolKind::Define => lsp_types::SymbolKind::CONSTANT,
        SymbolKind::File => lsp_types::SymbolKind::FILE,
        SymbolKind::Module => lsp_types::SymbolKind::MODULE,
        SymbolKind::RecordField => lsp_types::SymbolKind::STRUCT,
        SymbolKind::Variable => lsp_types::SymbolKind::VARIABLE,
        SymbolKind::Callback => lsp_types::SymbolKind::FUNCTION,
    }
}

pub(crate) fn text_edit(
    line_index: &LineIndex,
    line_endings: LineEndings,
    indel: Indel,
) -> lsp_types::TextEdit {
    let range = range(line_index, indel.delete);
    let new_text = line_endings.revert(indel.insert);
    lsp_types::TextEdit { range, new_text }
}

pub(crate) fn url(snap: &Snapshot, file_id: FileId) -> lsp_types::Url {
    snap.file_id_to_url(file_id)
}

pub(crate) fn optional_versioned_text_document_identifier(
    snap: &Snapshot,
    file_id: FileId,
) -> lsp_types::OptionalVersionedTextDocumentIdentifier {
    let url = url(snap, file_id);
    let version = snap.url_file_version(&url);
    lsp_types::OptionalVersionedTextDocumentIdentifier { uri: url, version }
}

pub(crate) fn text_document_edit(
    snap: &Snapshot,
    file_id: FileId,
    edit: TextEdit,
) -> Result<lsp_types::TextDocumentEdit> {
    let text_document = optional_versioned_text_document_identifier(snap, file_id);
    let line_index = snap.analysis.line_index(file_id)?;
    let line_endings = snap.line_endings(file_id);
    let edits: Vec<lsp_types::OneOf<lsp_types::TextEdit, lsp_types::AnnotatedTextEdit>> = edit
        .into_iter()
        .map(|it| lsp_types::OneOf::Left(text_edit(&line_index, line_endings, it)))
        .collect();

    // if snap.analysis.is_library_file(file_id)? && snap.config.change_annotation_support() {
    //     for edit in &mut edits {
    //         edit.annotation_id = Some(outside_workspace_annotation_id())
    //     }
    // }
    Ok(lsp_types::TextDocumentEdit {
        text_document,
        edits,
    })
}

pub(crate) fn workspace_edit(
    snap: &Snapshot,
    source_change: SourceChange,
) -> Result<lsp_types::WorkspaceEdit> {
    let mut edits: Vec<_> = vec![];
    for (file_id, edit) in source_change.source_file_edits {
        // let edit = snippet_text_document_edit(snap, source_change.is_snippet, file_id, edit)?;
        let edit = text_document_edit(snap, file_id, edit)?;
        edits.push(lsp_types::TextDocumentEdit {
            text_document: edit.text_document,
            edits: edit.edits.into_iter().map(From::from).collect(),
        });
    }
    let document_changes = lsp_types::DocumentChanges::Edits(edits);
    let workspace_edit = lsp_types::WorkspaceEdit {
        changes: None,
        document_changes: Some(document_changes),
        change_annotations: None,
    };
    Ok(workspace_edit)
}

pub(crate) fn code_action_kind(kind: AssistKind) -> lsp_types::CodeActionKind {
    match kind {
        AssistKind::None | AssistKind::Generate => lsp_types::CodeActionKind::EMPTY,
        AssistKind::QuickFix => lsp_types::CodeActionKind::QUICKFIX,
        AssistKind::Refactor => lsp_types::CodeActionKind::REFACTOR,
        AssistKind::RefactorExtract => lsp_types::CodeActionKind::REFACTOR_EXTRACT,
        AssistKind::RefactorInline => lsp_types::CodeActionKind::REFACTOR_INLINE,
        AssistKind::RefactorRewrite => lsp_types::CodeActionKind::REFACTOR_REWRITE,
    }
}

pub(crate) fn code_action(
    snap: &Snapshot,
    assist: Assist,
    resolve_data: Option<(usize, lsp_types::CodeActionParams, Option<AssistUserInput>)>,
) -> Result<lsp_types::CodeActionOrCommand> {
    let mut res = lsp_types::CodeAction {
        title: assist.label.to_string(),
        // group: assist
        //     .group
        //     .filter(|_| snap.config.code_action_group())
        //     .map(|gr| gr.0),
        kind: Some(code_action_kind(assist.id.1)),
        edit: None,
        is_preferred: None,
        data: None,
        diagnostics: None,
        command: None,
        disabled: None,
    };
    match (assist.source_change, resolve_data) {
        (Some(it), _) => res.edit = Some(workspace_edit(snap, it)?),
        (None, Some((index, code_action_params, user_input))) => {
            let data = lsp_ext::CodeActionData {
                id: format!("{}:{}:{}", assist.id.0, assist.id.1.name(), index),
                code_action_params,
                user_input,
            };
            res.data = Some(serde_json::value::to_value(data)?);
        }
        (None, None) => {
            stdx::never!("assist should always be resolved if client can't do lazy resolving")
        }
    };
    Ok(lsp_types::CodeActionOrCommand::CodeAction(res))
}

pub(crate) fn location(snap: &Snapshot, file_range: FileRange) -> Cancellable<lsp_types::Location> {
    let url = url(snap, file_range.file_id);
    let line_index = snap.analysis.line_index(file_range.file_id)?;
    let range = range(&line_index, file_range.range);
    let loc = lsp_types::Location::new(url, range);
    Ok(loc)
}

/// Prefer using `location_link`, if the client has the cap.
pub(crate) fn location_from_nav(
    snap: &Snapshot,
    nav: NavigationTarget,
) -> Cancellable<lsp_types::Location> {
    location(snap, nav.file_range())
}

pub(crate) fn location_link(
    snap: &Snapshot,
    src: Option<FileRange>,
    target: NavigationTarget,
) -> Result<lsp_types::LocationLink> {
    let origin_selection_range = match src {
        Some(src) => {
            let line_index = snap.analysis.line_index(src.file_id)?;
            let range = range(&line_index, src.range);
            Some(range)
        }
        None => None,
    };
    let (target_uri, target_range, target_selection_range) = location_info(snap, target)?;
    let res = lsp_types::LocationLink {
        origin_selection_range,
        target_uri,
        target_range,
        target_selection_range,
    };
    Ok(res)
}

fn location_info(
    snap: &Snapshot,
    target: NavigationTarget,
) -> Result<(lsp_types::Url, lsp_types::Range, lsp_types::Range)> {
    let line_index = snap.analysis.line_index(target.file_id)?;

    let target_uri = url(snap, target.file_id);
    let target_range = range(&line_index, target.full_range);
    let target_selection_range = target
        .focus_range
        .map(|it| range(&line_index, it))
        .unwrap_or(target_range);
    Ok((target_uri, target_range, target_selection_range))
}

pub(crate) fn goto_definition_response(
    snap: &Snapshot,
    src: Option<FileRange>,
    targets: Vec<NavigationTarget>,
) -> Result<lsp_types::GotoDefinitionResponse> {
    if snap.config.location_link() {
        let links = targets
            .into_iter()
            .map(|nav| location_link(snap, src, nav))
            .collect::<Result<Vec<_>>>()?;
        Ok(links.into())
    } else {
        let locations = targets
            .into_iter()
            .map(|nav| location_from_nav(snap, nav))
            .collect::<Cancellable<Vec<_>>>()?;
        Ok(locations.into())
    }
}

pub(crate) fn hover_response(
    snap: &Snapshot,
    maybe_doc: Option<(Doc, Option<FileRange>)>,
    actions: Vec<HoverAction>,
) -> Result<Option<lsp_ext::Hover>> {
    let (markup, id_range) = match maybe_doc {
        Some((doc, src_range)) => (doc.markdown_text().to_string(), src_range),
        None => return Result::Ok(None),
    };
    let markup_kind = MarkupKind::Markdown;
    let hover_contents = HoverContents::Markup(MarkupContent {
        kind: markup_kind,
        value: markup,
    });
    let hover_selection_range = match id_range {
        Some(fr) => {
            let line_index = snap.analysis.line_index(fr.file_id)?;
            Some(range(&line_index, fr.range))
        }
        None => None,
    };
    let hover = Hover {
        contents: hover_contents,
        range: hover_selection_range,
    };
    let actions = actions
        .iter()
        .filter_map(|it| match it {
            HoverAction::DocLink(link) => doc_link(link),
        })
        .collect();
    let hover_ext = lsp_ext::Hover { hover, actions };
    Result::Ok(Some(hover_ext))
}

fn doc_link(link: &DocLink) -> Option<lsp_ext::CommandLinkGroup> {
    let command = command::open_uri(&link.uri, &link.title);
    let command_link = lsp_ext::CommandLink {
        tooltip: Some(link.uri.to_string()),
        command,
    };
    let group = lsp_ext::CommandLinkGroup {
        title: Some("Go To Docs:".to_string()),
        commands: vec![command_link],
    };
    Some(group)
}

pub(crate) fn rename_error(err: RenameError) -> crate::LspError {
    request_failed_error(err.to_string())
}

/// A request failed but it was syntactically correct, e.g the
/// method name was known and the parameters were valid. The error
/// message should contain human readable information about why
/// the request failed.
pub(crate) fn request_failed_error(message: String) -> LspError {
    LspError {
        code: lsp_server::ErrorCode::RequestFailed as i32,
        message,
    }
}

pub fn completion_response(
    snap: Snapshot,
    completions: Vec<Completion>,
) -> lsp_types::CompletionResponse {
    let items = completions
        .into_iter()
        .map(|it| completion_item(&snap, it))
        .collect();
    lsp_types::CompletionResponse::Array(items)
}

fn completion_item(snap: &Snapshot, c: Completion) -> lsp_types::CompletionItem {
    use lsp_types::CompletionItemKind as K;
    use Kind::*;

    // Trigger Signature Help after completion for functions
    let command = if c.kind == Function {
        Some(command::trigger_parameter_hints())
    } else {
        None
    };
    let mut tags = Vec::new();
    if c.deprecated {
        tags.push(CompletionItemTag::DEPRECATED);
    };
    lsp_types::CompletionItem {
        label: c.label,
        kind: Some(match c.kind {
            Attribute => K::KEYWORD,
            Behavior => K::INTERFACE,
            Function => K::FUNCTION,
            Keyword => K::KEYWORD,
            Macro => K::CONSTANT,
            Module => K::MODULE,
            Operator => K::OPERATOR,
            RecordField => K::FIELD,
            Record => K::STRUCT,
            Type => K::INTERFACE,
            Variable => K::VARIABLE,
            AiAssist => K::EVENT,
        }),
        detail: None,
        documentation: None,
        deprecated: Some(c.deprecated),
        preselect: None,
        insert_text_format: match c.contents {
            Contents::SameAsLabel | Contents::String(_) => {
                Some(lsp_types::InsertTextFormat::PLAIN_TEXT)
            }
            Contents::Snippet(_) => Some(lsp_types::InsertTextFormat::SNIPPET),
        },
        insert_text_mode: None,
        text_edit: None,
        additional_text_edits: None,
        commit_characters: None,
        data: match completion_item_data(snap, c.position) {
            Some(data) => match serde_json::value::to_value(data) {
                Ok(data) => Some(data),
                Err(_) => None,
            },
            None => None,
        },
        sort_text: c.sort_text,
        filter_text: None,
        insert_text: match c.contents {
            Contents::Snippet(snippet) => Some(snippet),
            Contents::String(string) => Some(string),
            Contents::SameAsLabel => None,
        },
        command,
        tags: if !tags.is_empty() { Some(tags) } else { None },
        label_details: None,
    }
}

fn completion_item_data(snap: &Snapshot, pos: Option<FilePosition>) -> Option<CompletionData> {
    let file_id = pos?.file_id;
    if let Ok(line_index) = snap.analysis.line_index(file_id) {
        let uri = url(snap, file_id);
        let text_document = lsp_types::TextDocumentIdentifier { uri };
        let pos = position(&line_index, pos?.offset);
        let doc_pos = lsp_types::TextDocumentPositionParams::new(text_document, pos);
        Some(lsp_ext::CompletionData { position: doc_pos })
    } else {
        None
    }
}

pub(crate) fn folding_range(line_index: &LineIndex, fold: Fold) -> lsp_types::FoldingRange {
    let kind = match fold.kind {
        FoldKind::Function | FoldKind::Record => Some(lsp_types::FoldingRangeKind::Region),
    };

    let range = range(line_index, fold.range);

    lsp_types::FoldingRange {
        start_line: range.start.line,
        start_character: Some(range.start.character),
        end_line: range.end.line,
        end_character: Some(range.end.character),
        kind,
    }
}

// ---------------------------------------------------------------------

pub(crate) fn call_hierarchy_item(
    snap: &Snapshot,
    target: NavigationTarget,
) -> Result<lsp_types::CallHierarchyItem> {
    let name = target.name.to_string();
    let kind = lsp_types::SymbolKind::FUNCTION;
    let (uri, range, selection_range) = location_info(snap, target)?;
    Ok(lsp_types::CallHierarchyItem {
        name,
        kind,
        tags: None,
        detail: None,
        uri,
        range,
        selection_range,
        data: None,
    })
}

pub(crate) fn signature_help(
    calls_info: Vec<SignatureHelp>,
    active_parameter: usize,
) -> lsp_types::SignatureHelp {
    let mut signatures = Vec::new();
    for call_info in calls_info {
        signatures.push(signature_information(call_info));
    }
    let active_signature = signatures
        .iter()
        .take_while(|sig| match &sig.parameters {
            Some(parameters) => parameters.len() <= active_parameter,
            None => false,
        })
        .count();
    lsp_types::SignatureHelp {
        signatures,
        active_signature: Some(active_signature as u32),
        active_parameter: None,
    }
}

pub(crate) fn signature_information(call_info: SignatureHelp) -> lsp_types::SignatureInformation {
    let label = call_info.signature.clone();
    let parameters = call_info
        .parameter_labels()
        .map(|label| lsp_types::ParameterInformation {
            label: lsp_types::ParameterLabel::Simple(label.to_string()),
            documentation: call_info.parameters_doc.get(label).map(|doc| {
                lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: format!("`{}`: {}", label, doc.clone()),
                })
            }),
        })
        .collect::<Vec<_>>();

    let documentation = call_info.function_doc.map(|doc| {
        lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: doc,
        })
    });

    let active_parameter = call_info.active_parameter.map(|it| it as u32);

    lsp_types::SignatureInformation {
        label,
        documentation,
        parameters: Some(parameters),
        active_parameter,
    }
}

// ---------------------------------------------------------------------

static TOKEN_RESULT_COUNTER: AtomicU32 = AtomicU32::new(1);

pub(crate) fn semantic_tokens(
    text: &str,
    line_index: &LineIndex,
    highlights: Vec<HlRange>,
) -> lsp_types::SemanticTokens {
    let id = TOKEN_RESULT_COUNTER
        .fetch_add(1, Ordering::SeqCst)
        .to_string();
    let mut builder = semantic_tokens::SemanticTokensBuilder::new(id);

    for highlight_range in highlights {
        if highlight_range.highlight.is_empty() {
            continue;
        }

        let (ty, mods) = semantic_token_type_and_modifiers(highlight_range.highlight);
        let token_index = semantic_tokens::type_index(ty);
        let modifier_bitset = mods.0;

        for mut text_range in line_index.lines(highlight_range.range) {
            if text[text_range].ends_with('\n') {
                text_range =
                    TextRange::new(text_range.start(), text_range.end() - TextSize::of('\n'));
            }
            let range = range(line_index, text_range);
            builder.push(range, token_index, modifier_bitset);
        }
    }

    builder.build()
}

pub(crate) fn semantic_token_delta(
    previous: &lsp_types::SemanticTokens,
    current: &lsp_types::SemanticTokens,
) -> lsp_types::SemanticTokensDelta {
    let result_id = current.result_id.clone();
    let edits = semantic_tokens::diff_tokens(&previous.data, &current.data);
    lsp_types::SemanticTokensDelta { result_id, edits }
}

fn semantic_token_type_and_modifiers(
    highlight: Highlight,
) -> (lsp_types::SemanticTokenType, semantic_tokens::ModifierSet) {
    let mut mods = semantic_tokens::ModifierSet::default();
    let type_ = match highlight.tag {
        HlTag::Symbol(symbol) => match symbol {
            SymbolKind::File => semantic_tokens::STRING,
            SymbolKind::Module => semantic_tokens::NAMESPACE,
            SymbolKind::Function => semantic_tokens::FUNCTION,
            SymbolKind::Record => semantic_tokens::STRUCT,
            SymbolKind::RecordField => semantic_tokens::STRUCT,
            SymbolKind::Type => semantic_tokens::TYPE_PARAMETER,
            SymbolKind::Define => semantic_tokens::MACRO,
            SymbolKind::Variable => semantic_tokens::VARIABLE,
            SymbolKind::Callback => semantic_tokens::FUNCTION,
        },
        HlTag::None => semantic_tokens::GENERIC,
    };

    for modifier in highlight.mods.iter() {
        let modifier = match modifier {
            HlMod::Bound => semantic_tokens::BOUND,
            HlMod::ExportedFunction => semantic_tokens::EXPORTED_FUNCTION,
            HlMod::DeprecatedFunction => semantic_tokens::DEPRECATED_FUNCTION,
        };
        mods |= modifier;
    }

    (type_, mods)
}

pub(crate) fn document_highlight_kind(
    category: ReferenceCategory,
) -> Option<lsp_types::DocumentHighlightKind> {
    match category {
        ReferenceCategory::Read => Some(lsp_types::DocumentHighlightKind::READ),
        ReferenceCategory::Write => Some(lsp_types::DocumentHighlightKind::WRITE),
    }
}

pub(crate) fn buck2_test_runnable(
    snap: &Snapshot,
    runnable: Runnable,
    target: String,
    coverage_enabled: bool,
) -> lsp_ext::Runnable {
    let file_id = runnable.nav.file_id;
    let location = location_link(snap, None, runnable.clone().nav).ok();
    lsp_ext::Runnable::buck2_test(
        runnable,
        target,
        location,
        snap.workspace_root(file_id).into(),
        coverage_enabled,
    )
}

pub(crate) fn buck2_run_runnable(
    snap: &Snapshot,
    runnable: Runnable,
    target: String,
) -> lsp_ext::Runnable {
    let file_id = runnable.nav.file_id;
    let location = location_link(snap, None, runnable.clone().nav).ok();
    lsp_ext::Runnable::buck2_run(
        runnable,
        target,
        location,
        snap.workspace_root(file_id).into(),
    )
}

pub(crate) fn code_lens(
    acc: &mut Vec<lsp_types::CodeLens>,
    snap: &Snapshot,
    lens_config: &LensConfig,
    line_index: &LineIndex,
    annotation: elp_ide::Annotation,
    project_build_data: &ProjectBuildData,
) {
    match annotation.kind {
        AnnotationKind::Runnable(run) => {
            let annotation_range = range(line_index, annotation.range);
            let run_title = &run.run_title();
            let run_interactive_title = &run.run_interactive_title();
            let debug_title = &run.debug_title();
            if let ProjectBuildData::Buck(project) = project_build_data {
                let file_id = run.nav.file_id;
                if let Some(file_path) = snap.file_id_to_path(file_id) {
                    if let Some(target) = project.target(&file_path) {
                        let r = buck2_test_runnable(
                            snap,
                            run.clone(),
                            target.clone(),
                            lens_config.run_coverage,
                        );
                        if lens_config.run_interactive {
                            let interactive_r = buck2_run_runnable(snap, run.clone(), target);
                            match run.kind {
                                RunnableKind::Suite => {
                                    let run_command = command::open_interactive(
                                        &interactive_r,
                                        run_interactive_title,
                                    );
                                    acc.push(lsp_types::CodeLens {
                                        range: annotation_range,
                                        command: Some(run_command),
                                        data: None,
                                    });
                                }
                                RunnableKind::Test { .. } => {
                                    let run_command = command::run_interactive(
                                        &interactive_r,
                                        run_interactive_title,
                                    );
                                    acc.push(lsp_types::CodeLens {
                                        range: annotation_range,
                                        command: Some(run_command),
                                        data: None,
                                    });
                                }
                            }
                        }
                        if lens_config.run {
                            let run_command = command::run_single(&r, run_title);
                            acc.push(lsp_types::CodeLens {
                                range: annotation_range,
                                command: Some(run_command),
                                data: None,
                            });
                        }
                        if lens_config.debug {
                            let debug_command = command::debug_single(&r, debug_title);
                            acc.push(lsp_types::CodeLens {
                                range: annotation_range,
                                command: Some(debug_command),
                                data: None,
                            });
                        }
                    }
                }
            }
        }
        AnnotationKind::Link(link) => {
            if lens_config.links {
                let annotation_range = range(line_index, annotation.range);
                let url = link.url;
                let text = link.text;
                let command = command::open_uri(&url, &text);
                acc.push(lsp_types::CodeLens {
                    range: annotation_range,
                    command: Some(command),
                    data: None,
                });
            }
        }
    }
}

pub(crate) mod command {
    use serde_json::to_value;

    use crate::lsp_ext;

    pub(crate) fn open_interactive(
        runnable: &lsp_ext::Runnable,
        title: &str,
    ) -> lsp_types::Command {
        lsp_types::Command {
            title: title.to_string(),
            command: "elp.openInteractive".into(),
            arguments: Some(vec![to_value(runnable).unwrap()]),
        }
    }

    pub(crate) fn run_interactive(runnable: &lsp_ext::Runnable, title: &str) -> lsp_types::Command {
        lsp_types::Command {
            title: title.to_string(),
            command: "elp.runInteractive".into(),
            arguments: Some(vec![to_value(runnable).unwrap()]),
        }
    }

    pub(crate) fn run_single(runnable: &lsp_ext::Runnable, title: &str) -> lsp_types::Command {
        lsp_types::Command {
            title: title.to_string(),
            command: "elp.runSingle".into(),
            arguments: Some(vec![to_value(runnable).unwrap()]),
        }
    }

    pub(crate) fn debug_single(runnable: &lsp_ext::Runnable, title: &str) -> lsp_types::Command {
        lsp_types::Command {
            title: title.to_string(),
            command: "elp.debugSingle".into(),
            arguments: Some(vec![to_value(runnable).unwrap()]),
        }
    }

    pub(crate) fn open_uri(uri: &str, title: &str) -> lsp_types::Command {
        lsp_types::Command {
            title: title.to_string(),
            command: "elp.openUri".into(),
            arguments: Some(vec![to_value(uri).unwrap()]),
        }
    }

    pub(crate) fn trigger_parameter_hints() -> lsp_types::Command {
        lsp_types::Command {
            title: "triggerParameterHints".into(),
            command: "editor.action.triggerParameterHints".into(),
            arguments: None,
        }
    }
}

pub(crate) fn inlay_hint(
    snap: &Snapshot,
    line_index: &LineIndex,
    mut inlay_hint: elp_ide::InlayHint,
) -> Cancellable<lsp_types::InlayHint> {
    match inlay_hint.kind {
        InlayKind::Parameter => inlay_hint.label.append_str(":"),
    }

    let (label, tooltip) = inlay_hint_label(snap, inlay_hint.label)?;

    Ok(lsp_types::InlayHint {
        position: match inlay_hint.kind {
            // before annotated thing
            InlayKind::Parameter => position(line_index, inlay_hint.range.start()),
            // after annotated thing
            // _ => position(line_index, inlay_hint.range.end()),
        },
        padding_left: Some(match inlay_hint.kind {
            InlayKind::Parameter => false,
        }),
        padding_right: Some(match inlay_hint.kind {
            InlayKind::Parameter => true,
        }),
        kind: match inlay_hint.kind {
            InlayKind::Parameter => Some(lsp_types::InlayHintKind::PARAMETER),
        },
        text_edits: None,
        data: None,
        tooltip,
        label,
    })
}

fn inlay_hint_label(
    snap: &Snapshot,
    mut label: InlayHintLabel,
) -> Cancellable<(
    lsp_types::InlayHintLabel,
    Option<lsp_types::InlayHintTooltip>,
)> {
    let res = match &*label.parts {
        [
            InlayHintLabelPart {
                linked_location: None,
                ..
            },
        ] => {
            let InlayHintLabelPart { text, tooltip, .. } = label.parts.pop().unwrap();
            (
                lsp_types::InlayHintLabel::String(text),
                match tooltip {
                    Some(elp_ide::InlayTooltip::String(s)) => {
                        Some(lsp_types::InlayHintTooltip::String(s))
                    }
                    Some(elp_ide::InlayTooltip::Markdown(s)) => Some(
                        lsp_types::InlayHintTooltip::MarkupContent(lsp_types::MarkupContent {
                            kind: lsp_types::MarkupKind::Markdown,
                            value: s,
                        }),
                    ),
                    None => None,
                },
            )
        }
        _ => {
            let parts = label
                .parts
                .into_iter()
                .map(|part| {
                    part.linked_location
                        .map(|range| location(snap, range))
                        .transpose()
                        .map(|location| lsp_types::InlayHintLabelPart {
                            value: part.text,
                            tooltip: match part.tooltip {
                                Some(elp_ide::InlayTooltip::String(s)) => {
                                    Some(lsp_types::InlayHintLabelPartTooltip::String(s))
                                }
                                Some(elp_ide::InlayTooltip::Markdown(s)) => {
                                    Some(lsp_types::InlayHintLabelPartTooltip::MarkupContent(
                                        lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: s,
                                        },
                                    ))
                                }
                                None => None,
                            },
                            location,
                            command: None,
                        })
                })
                .collect::<Cancellable<_>>()?;
            (lsp_types::InlayHintLabel::LabelParts(parts), None)
        }
    };
    Ok(res)
}

#[allow(deprecated)]
pub(crate) fn document_symbol(
    line_index: &LineIndex,
    symbol: &elp_ide::DocumentSymbol,
) -> lsp_types::DocumentSymbol {
    let mut tags = Vec::new();
    if symbol.deprecated {
        tags.push(lsp_types::SymbolTag::DEPRECATED)
    };
    let selection_range = range(line_index, symbol.selection_range);
    let range = range(line_index, symbol.range);
    let children = symbol.children.as_ref().map(|children| {
        children
            .iter()
            .map(|c| document_symbol(line_index, c))
            .collect()
    });
    lsp_types::DocumentSymbol {
        name: symbol.name.clone(),
        detail: symbol.detail.clone(),
        kind: symbol_kind(symbol.kind),
        tags: Some(tags),
        deprecated: Some(false),
        range,
        selection_range,
        children,
    }
}

// ---------------------------------------------------------------------
