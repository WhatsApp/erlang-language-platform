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
import { openWorkspaceFile, serverRunning } from '../helpers';

suite('Generated files', () => {
	test('Go to definition reaches a record in a header that Buck2 generates', async () => {
		// ELP loads a Buck2 project in two stages, and only the second one
		// generates files, after which ELP reports `Running` again.
		const loaded = serverRunning(2);
		const document = await openWorkspaceFile('app_a/src/example_usage.erl');
		await loaded;

		// `user_info` in `#user_info{`
		const [definition] = await vscode.commands.executeCommand<(vscode.Location | vscode.LocationLink)[]>(
			'vscode.executeDefinitionProvider', document.uri, new vscode.Position(27, 7));

		const uri = 'targetUri' in definition ? definition.targetUri : definition.uri;
		assert.ok(uri.fsPath.includes('/buck-out/'), uri.fsPath);
		assert.ok(uri.fsPath.endsWith('/example_service_types.hrl'), uri.fsPath);
		// `-include_lib("example_service_generated/include/example_service_types.hrl").`
		const includeErrors = vscode.languages.getDiagnostics(document.uri)
			.filter(diagnostic => diagnostic.range.start.line === 9 && diagnostic.severity === vscode.DiagnosticSeverity.Error);
		assert.deepStrictEqual(includeErrors, []);
	});
});
