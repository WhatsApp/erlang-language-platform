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
import { ElpExtensionApi } from '../extension';

const EXTENSION_ID = 'erlang-language-platform.erlang-language-platform';

// Opens a file of the test workspace and waits until ELP has loaded its project.
export async function openWorkspaceFile(relativePath: string): Promise<vscode.TextDocument> {
	const folder = vscode.workspace.workspaceFolders![0];
	const document = await vscode.workspace.openTextDocument(vscode.Uri.joinPath(folder.uri, relativePath));
	await vscode.window.showTextDocument(document);
	await waitForServerRunning();
	return document;
}

async function waitForServerRunning(): Promise<void> {
	const { serverStatus } = await vscode.extensions.getExtension<ElpExtensionApi>(EXTENSION_ID)!.activate();
	if (serverStatus.current === 'Running') {
		return;
	}
	await new Promise<void>(resolve => {
		const listener = serverStatus.onDidChange(state => {
			if (state === 'Running') {
				listener.dispose();
				resolve();
			}
		});
	});
}

export async function hoverText(document: vscode.TextDocument, position: vscode.Position): Promise<string> {
	const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
		'vscode.executeHoverProvider', document.uri, position);
	return hovers
		.flatMap(hover => hover.contents)
		.map(content => typeof content === 'string' ? content : content.value)
		.join('\n');
}

export function diagnosticCode(diagnostic: vscode.Diagnostic): string | number | undefined {
	return typeof diagnostic.code === 'object' ? diagnostic.code.value : diagnostic.code;
}

// Resolves with the first diagnostic of the document that matches, as soon as
// ELP publishes one.
export function waitForDiagnostic(
	document: vscode.TextDocument,
	matches: (diagnostic: vscode.Diagnostic) => boolean,
): Promise<vscode.Diagnostic> {
	return new Promise(resolve => {
		const check = () => {
			const diagnostic = vscode.languages.getDiagnostics(document.uri).find(matches);
			if (diagnostic) {
				listener.dispose();
				resolve(diagnostic);
			}
		};
		const listener = vscode.languages.onDidChangeDiagnostics(check);
		check();
	});
}
