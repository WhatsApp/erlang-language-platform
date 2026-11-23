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
import { renderActions } from '../hover';
import { hoverText, openedUris, openWorkspaceFile, waitForDiagnostic } from './helpers';

suite('Hover', () => {
	test('shows the documentation of a remote function', async () => {
		const document = await openWorkspaceFile('hover/src/doc_examples.erl');

		// `reverse` in `lists:reverse(L)`
		const text = await hoverText(document, new vscode.Position(15, 11));
		assert.ok(text.includes('in `List1` in reverse order'), text);
	});

	test('shows a link to the documentation of a remote function', async () => {
		const document = await openWorkspaceFile('hover/src/doc_examples.erl');

		// `reverse` in `lists:reverse(L)`
		const text = await hoverText(document, new vscode.Position(15, 11));
		assert.ok(text.includes('Docs for: [lists:reverse/1](command:elp.openUri?'), text);
	});

	test('opens the documentation link of a remote function', async () => {
		const document = await openWorkspaceFile('hover/src/doc_examples.erl');

		// `reverse` in `lists:reverse(L)`
		const text = await hoverText(document, new vscode.Position(15, 11));
		const link = text.match(/\(command:elp\.openUri\?([^ )]+)/);
		assert.ok(link, text);
		const args = JSON.parse(decodeURIComponent(link[1]));
		const uris = await openedUris(() => vscode.commands.executeCommand('elp.openUri', ...args));
		assert.deepStrictEqual(uris.map(uri => uri.toString(true)), ['https://erlang.org/doc/man/lists.html#reverse/1']);
	});

	test('renders text from the source as a single link', () => {
		const injected = 'x](command:elp.openUri?%5B%22https%3A%2F%2Fattacker.example%22%5D) [y';
		const markdown = renderActions([{
			title: `Oncall (${injected})`,
			commands: [{
				title: injected,
				command: 'elp.openUri',
				arguments: [`https://docs.example.com/${injected}`],
				tooltip: `"\n${injected}`,
			}],
		}]).value;

		// Without the escaped characters, only the delimiters of one link remain.
		const unescaped = markdown.replace(/\\./g, '');
		assert.strictEqual(unescaped.match(/[[\]()]/g)?.join(''), '[]()', markdown);
		assert.ok(markdown.includes('](command:elp.openUri?'), markdown);
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
