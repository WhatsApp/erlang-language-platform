/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

import * as vscode from 'vscode';
import { LanguageClient, RequestType, TextDocumentPositionParams } from 'vscode-languageclient/node';

const externalDocsRequest = new RequestType<TextDocumentPositionParams, string[] | null, void>(
	'experimental/externalDocs');

// Opens the documentation of the element under the cursor in the browser,
// asking which page to open when there are several.
export async function openExternalDocs(client: LanguageClient): Promise<void> {
	const editor = vscode.window.activeTextEditor;
	if (editor?.document.languageId !== 'erlang') {
		await vscode.window.showInformationMessage('Place the cursor on an element in an Erlang file to open its documentation.');
		return;
	}
	const links = await client.sendRequest(
		externalDocsRequest,
		client.code2ProtocolConverter.asTextDocumentPositionParams(editor.document, editor.selection.active),
	) ?? [];
	if (links.length === 0) {
		await vscode.window.showInformationMessage('No documentation available for the selected element.');
		return;
	}
	const link = links.length === 1 ? links[0] : await vscode.window.showQuickPick(links);
	if (link) {
		await vscode.env.openExternal(vscode.Uri.parse(link));
	}
}
