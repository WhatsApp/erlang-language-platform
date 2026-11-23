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
import { openWorkspaceFile } from './helpers';

suite('Code completion', () => {
	test('remote module functions are proposed with their arguments', async () => {
		const document = await openWorkspaceFile('assist_examples/src/code_completion.erl');

		// After `    lists:`
		const position = new vscode.Position(9, 10);
		const { items } = await vscode.commands.executeCommand<vscode.CompletionList>(
			'vscode.executeCompletionItemProvider', document.uri, position, ':');

		const all = items.find(item => item.label === 'all/2');
		assert.ok(all, 'lists:all/2 should be proposed');
		assert.strictEqual((all.insertText as vscode.SnippetString).value, 'all(${1:Pred}, ${2:List})');
	});
});
