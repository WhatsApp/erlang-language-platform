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
import { codeLens, executeLens, firstStop, openWorkspaceFile, otpRelease, Stop, withBreakpoint } from './helpers';

const SUITE = 'lenses/test/lenses_SUITE.erl';
// `Value = value(),` in `main/1`
const BREAKPOINT_LINE = 6;

suite('Test lenses', () => {
	test('Run All Tests runs a Buck2 suite with buck2 test', async () => {
		const document = await openWorkspaceFile(SUITE);
		const lens = await codeLens(document, 'Run All Tests');

		const started = new Promise<vscode.TaskExecution>(resolve => {
			const listener = vscode.tasks.onDidStartTask(event => {
				listener.dispose();
				resolve(event.execution);
			});
		});
		await executeLens(lens);
		const execution = await started;
		execution.terminate();

		// The test runner arguments that follow depend on the build of ELP.
		const process = execution.task.execution as vscode.ProcessExecution;
		assert.strictEqual(process.process, 'buck2');
		assert.deepStrictEqual(
			process.args.slice(0, 3), ['--client-metadata=id=elp_vscode', 'test', 'root//end_to_end/lenses:lenses_SUITE']);
	});

	test('Debug stops the test of a Buck2 suite at a breakpoint', async function () {
		// EDB needs OTP 28 or later.
		if (otpRelease() < 28) {
			this.skip();
		}
		this.timeout(600000);
		const document = await openWorkspaceFile(SUITE);
		const lens = await codeLens(document, 'Debug');

		let stop: Stop | undefined;
		await withBreakpoint(document, BREAKPOINT_LINE, async () => {
			stop = await firstStop(() => executeLens(lens));
		});

		assert.deepStrictEqual(stop, { reason: 'breakpoint', path: document.uri.fsPath, line: BREAKPOINT_LINE + 1 });
	});
});
