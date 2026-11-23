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
import { diagnosticCode, openWorkspaceFile, waitForDiagnostic } from './helpers';

suite('Diagnostics', () => {
	test('unused macro is reported', async () => {
		const document = await openWorkspaceFile('single_errors/src/unused_macro.erl');

		const diagnostic = await waitForDiagnostic(document, diagnostic => diagnosticCode(diagnostic) === 'W0002');
		// `-define(MEANING_OF_LIFE, 42).`
		assert.strictEqual(diagnostic.range.start.line, 2);
		assert.strictEqual(diagnostic.severity, vscode.DiagnosticSeverity.Warning);
	});
});
