/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

import * as assert from 'assert';
import * as vscode from 'vscode';
import { openWorkspaceFile, restoringContent } from './helpers';

// Finds the code action with the given title, resolved so that it carries its edit.
async function codeAction(document: vscode.TextDocument, range: vscode.Range, title: string): Promise<vscode.CodeAction> {
	const actions = await vscode.commands.executeCommand<vscode.CodeAction[]>(
		'vscode.executeCodeActionProvider', document.uri, range, undefined, Number.MAX_SAFE_INTEGER);
	const action = actions.find(action => action.title === title);
	assert.ok(action?.edit, `No "${title}" code action among: ${actions.map(action => action.title).join(', ')}`);
	return action;
}

suite('Code actions', () => {
	test('extract function', async () => {
		const document = await openWorkspaceFile('assist_examples/src/extract_function.erl');

		await restoringContent(document, async () => {
			// The assist asks for the name of the new function.
			const window = vscode.window as { showInputBox: typeof vscode.window.showInputBox };
			const showInputBox = window.showInputBox;
			window.showInputBox = async () => 'new_function_name';
			try {
				// `  {ok, Version} = application:get_key(Application, vsn),`
				const action = await codeAction(document, new vscode.Range(6, 0, 7, 0), 'Extract into function');
				await vscode.workspace.applyEdit(action.edit!);
			} finally {
				window.showInputBox = showInputBox;
			}
			assert.ok(document.getText().includes('new_function_name(Application) ->'), document.getText());
		});
	});

	test('add -doc attribute', async () => {
		const document = await openWorkspaceFile('assist_examples/src/add_doc.erl');

		await restoringContent(document, async () => {
			// `version() -> undefined.`
			const action = await codeAction(document, new vscode.Range(4, 0, 4, 0), 'Add -doc attribute');
			await vscode.workspace.applyEdit(action.edit!);
			assert.ok(document.getText().includes('-doc'), document.getText());
		});
	});
});
