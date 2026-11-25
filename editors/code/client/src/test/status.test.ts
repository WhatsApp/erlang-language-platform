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
import { State } from 'vscode-languageclient/node';
import { extensionApi, openWorkspaceFile } from './helpers';

suite('Status bar', () => {
	test('links to the logs and to pinging the server', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');
		const { serverStatus } = await extensionApi();

		const tooltip = serverStatus.statusBarItem.tooltip as vscode.MarkdownString;
		const commands = [...tooltip.value.matchAll(/\]\(command:([\w.]+)/g)].map(match => match[1]);
		assert.deepStrictEqual(commands, ['elp.openExtensionLogs', 'elp.openLanguageClientLogs', 'elp.pingAndShow']);
		assert.ok(tooltip.isTrusted);
		const registered = await vscode.commands.getCommands(true);
		assert.deepStrictEqual(commands.filter(command => !registered.includes(command)), []);
	});

	test('shows an error background while the language client is not running', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');
		const { serverStatus } = await extensionApi();
		assert.strictEqual(serverStatus.statusBarItem.backgroundColor, undefined);

		serverStatus.updateClientState(State.Stopped);
		try {
			assert.deepStrictEqual(serverStatus.statusBarItem.backgroundColor, new vscode.ThemeColor('statusBarItem.errorBackground'));
		} finally {
			serverStatus.updateClientState(State.Running);
		}
		assert.strictEqual(serverStatus.statusBarItem.backgroundColor, undefined);
	});
});
