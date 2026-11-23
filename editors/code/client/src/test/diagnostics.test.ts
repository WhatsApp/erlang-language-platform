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
import { diagnosticCode, openWorkspaceFile, replaceText, restoringContent, waitForDiagnostic } from './helpers';

suite('Diagnostics', () => {
	test('unused macro is reported', async () => {
		const document = await openWorkspaceFile('single_errors/src/unused_macro.erl');

		const diagnostic = await waitForDiagnostic(document, diagnostic => diagnosticCode(diagnostic) === 'W0002');
		// `-define(MEANING_OF_LIFE, 42).`
		assert.strictEqual(diagnostic.range.start.line, 2);
		assert.strictEqual(diagnostic.severity, vscode.DiagnosticSeverity.Warning);
	});

	test('head mismatch is reported', async () => {
		const document = await openWorkspaceFile('assist_examples/src/head_mismatch.erl');

		const diagnostic = await waitForDiagnostic(document, diagnostic => diagnostic.message.includes('head mismatch'));
		// `foo(X) -> X.`
		assert.strictEqual(diagnostic.range.start.line, 5);
		assert.strictEqual(diagnostic.severity, vscode.DiagnosticSeverity.Error);
	});

	test('head mismatch is updated as you type', async () => {
		const document = await openWorkspaceFile('assist_examples/src/head_mismatch.erl');

		await restoringContent(document, async () => {
			// `bar(1) -> 2;` becomes `baz(1) -> 2;`
			await replaceText(document, new vscode.Range(4, 2, 4, 3), 'z');
			await waitForDiagnostic(document, diagnostic => diagnostic.message.includes("'baz' vs 'bar'"));
		});
	});

	test('syntax errors are reported as you type', async () => {
		const document = await openWorkspaceFile('single_errors/src/as_you_type.erl');

		await restoringContent(document, async () => {
			await replaceText(document, new vscode.Range(1, 0, 1, 0), 'this');
			const diagnostic = await waitForDiagnostic(document, diagnostic => diagnosticCode(diagnostic) === 'P1711');
			assert.strictEqual(diagnostic.range.start.line, 1);
		});
	});

	test('compiler errors are reported on save', async () => {
		const document = await openWorkspaceFile('single_errors/src/compiler_diagnostics.erl');
		// The first diagnostics pass is over before the edit.
		await waitForDiagnostic(document, diagnostic => diagnosticCode(diagnostic) === 'W0012');

		await restoringContent(document, async () => {
			// `-behaviour(application).` becomes `-beha viour(application).`
			await replaceText(document, new vscode.Range(3, 5, 3, 5), ' ');
			await document.save();
			const diagnostic = await waitForDiagnostic(document, diagnostic => diagnosticCode(diagnostic) === 'P1702');
			assert.strictEqual(diagnostic.range.start.line, 3);
		});
	});

	test('misspelled attributes are reported on save', async () => {
		const document = await openWorkspaceFile('single_errors/src/compiler_diagnostics.erl');

		await restoringContent(document, async () => {
			// `-behaviour(application).` becomes `-behaiour(application).`
			await replaceText(document, new vscode.Range(3, 5, 3, 6), '');
			await document.save();
			const diagnostic = await waitForDiagnostic(document, diagnostic => diagnosticCode(diagnostic) === 'W0013');
			assert.strictEqual(diagnostic.range.start.line, 3);
		});
	});

	test('eqWAlizer reports a spec mismatch', async () => {
		const document = await openWorkspaceFile('single_errors/src/spec_mismatch.erl');

		const diagnostic = await waitForDiagnostic(
			document, diagnostic => diagnostic.message.includes("Context expected type: 'ok'"));
		// `    green.`
		assert.strictEqual(diagnostic.range.start.line, 7);
	});

	test('eqWAlizer diagnostics are updated on save', async () => {
		const document = await openWorkspaceFile('single_errors/src/spec_mismatch.erl');
		await waitForDiagnostic(document, diagnostic => diagnostic.message.includes("Expression has type:   'green'"));

		await restoringContent(document, async () => {
			// `    green.` becomes `    green1.`
			await replaceText(document, new vscode.Range(7, 9, 7, 9), '1');
			await document.save();
			await waitForDiagnostic(document, diagnostic => diagnostic.message.includes("Expression has type:   'green1'"));
		});
	});
});
