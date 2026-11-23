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
import { openedUris, openWorkspaceFile } from './helpers';

suite('Links', () => {
	test('elp.openUri opens its URI in the browser', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');

		const uris = await openedUris(
			() => vscode.commands.executeCommand('elp.openUri', 'https://erlang.org/doc/man/lists.html#reverse/1'));

		assert.deepStrictEqual(uris.map(uri => uri.toString(true)), ['https://erlang.org/doc/man/lists.html#reverse/1']);
	});
});
