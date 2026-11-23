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

suite('Go to definition', () => {
	test('local function call jumps to the function definition', async () => {
		const document = await openWorkspaceFile('definitions/src/local_def.erl');

		// The `f` in `g() -> f().`
		const call = new vscode.Position(6, 7);
		const [definition] = await vscode.commands.executeCommand<(vscode.Location | vscode.LocationLink)[]>(
			'vscode.executeDefinitionProvider', document.uri, call);

		const [uri, range] = 'targetUri' in definition
			? [definition.targetUri, definition.targetSelectionRange ?? definition.targetRange]
			: [definition.uri, definition.range];
		assert.strictEqual(uri.toString(), document.uri.toString());
		// `f() -> ok.`
		assert.strictEqual(range.start.line, 4);
	});
});
