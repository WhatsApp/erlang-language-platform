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

suite('Commands', () => {
	test('Expand Macro opens the expansion of the macro under the cursor', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');
		// `FOO` in `h() -> ?FOO(3).`
		const cursor = new vscode.Position(22, 9);
		vscode.window.activeTextEditor!.selection = new vscode.Selection(cursor, cursor);

		await vscode.commands.executeCommand('elp.expandMacro');

		const expansion = vscode.window.activeTextEditor!.document;
		assert.strictEqual(expansion.languageId, 'erlang');
		assert.strictEqual(expansion.getText(), '%% Expansion of FOO/1\n\n(3 + 1)\n');
	});
});
