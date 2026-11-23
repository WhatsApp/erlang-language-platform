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
import {
	LanguageClient,
	Position,
	RequestType,
	TextDocumentIdentifier,
} from 'vscode-languageclient/node';

interface ExpandMacroParams {
	textDocument: TextDocumentIdentifier;
	position: Position;
}

interface ExpandedMacro {
	name: string;
	expansion: string;
}

const expandMacroRequest = new RequestType<ExpandMacroParams, ExpandedMacro | null, void>('elp/expandMacro');

// Opens the expansion of the macro under the cursor in a new editor.
export async function expandMacro(client: LanguageClient): Promise<void> {
	const editor = vscode.window.activeTextEditor;
	if (editor?.document.languageId !== 'erlang') {
		await vscode.window.showInformationMessage('Place the cursor on a macro in an Erlang file to expand it.');
		return;
	}
	let expanded: ExpandedMacro | null;
	try {
		expanded = await client.sendRequest(expandMacroRequest, {
			textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(editor.document),
			position: client.code2ProtocolConverter.asPosition(editor.selection.active),
		});
	} catch (error) {
		// For example in a file ELP does not know, such as an expansion opened by this command.
		await vscode.window.showInformationMessage(`Could not expand the macro: ${error}`);
		return;
	}
	if (!expanded?.expansion) {
		await vscode.window.showInformationMessage('There is no macro to expand under the cursor.');
		return;
	}
	const document = await vscode.workspace.openTextDocument({
		language: 'erlang',
		content: `%% Expansion of ${expanded.name}\n\n${expanded.expansion.trim()}\n`,
	});
	await vscode.window.showTextDocument(document, { viewColumn: vscode.ViewColumn.Beside });
}
