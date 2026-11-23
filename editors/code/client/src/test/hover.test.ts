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
import { hoverText, openWorkspaceFile, waitForDiagnostic } from './helpers';

suite('Hover', () => {
	test('shows the documentation of a remote function', async () => {
		const document = await openWorkspaceFile('hover/src/doc_examples.erl');

		// `reverse` in `lists:reverse(L)`
		const text = await hoverText(document, new vscode.Position(15, 11));
		assert.ok(text.includes('in `List1` in reverse order'), text);
	});

	test('shows the type of a variable', async () => {
		const document = await openWorkspaceFile('single_errors/src/types_on_hover.erl');
		// Types come from eqWAlizer, which reports on this module too.
		await waitForDiagnostic(document, diagnostic => diagnostic.source === 'eqWAlizer');

		// `Value` in `foo(Value) -> Value.`
		const text = await hoverText(document, new vscode.Position(7, 4));
		assert.ok(text.includes('Value :: my_type'), text);
	});
});
