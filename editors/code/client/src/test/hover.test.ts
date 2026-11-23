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
import { hoverText, openWorkspaceFile } from './helpers';

suite('Hover', () => {
	test('shows the documentation of a remote function', async () => {
		const document = await openWorkspaceFile('hover/src/doc_examples.erl');

		// `reverse` in `lists:reverse(L)`
		const text = await hoverText(document, new vscode.Position(15, 11));
		assert.ok(text.includes('in `List1` in reverse order'), text);
	});
});
