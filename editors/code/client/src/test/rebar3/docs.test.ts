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
import { openedUris, openWorkspaceFile } from '../helpers';

suite('Docs', () => {
	test('Open docs under cursor opens the documentation of the function under the cursor', async () => {
		await openWorkspaceFile('docs/src/docs.erl');
		// `my_function` in `my_other_function() -> my_function().`
		const cursor = new vscode.Position(9, 26);
		vscode.window.activeTextEditor!.selection = new vscode.Selection(cursor, cursor);

		const uris = await openedUris(() => vscode.commands.executeCommand('elp.externalDocs'));

		// The page is under the `base_url` the rebar3 workspace sets in `.elp.toml`.
		assert.deepStrictEqual(uris.map(uri => uri.toString(true)), ['https://docs.example.com/docs.html#my_function/0']);
	});
});
