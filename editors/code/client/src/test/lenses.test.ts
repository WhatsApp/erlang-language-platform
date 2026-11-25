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
import { openWorkspaceFile } from './helpers';

// The runnables ELP attaches to the lenses of a Common Test suite in a Buck2 project.
function buck2Runnable(command: string, args: string[]) {
	return {
		label: 'Buck2',
		kind: 'buck2',
		args: {
			workspaceRoot: vscode.workspace.workspaceFolders![0].uri.fsPath,
			command,
			args,
			target: 'root//app_a:app_a_SUITE',
			id: 'app_a_SUITE - .my_case',
		},
	};
}

// Runs a lens command and returns the process of the task it starts, stopping it.
async function startedProcess(command: string, runnable: unknown): Promise<vscode.ProcessExecution> {
	const started = new Promise<vscode.TaskExecution>(resolve => {
		const listener = vscode.tasks.onDidStartTask(event => {
			listener.dispose();
			resolve(event.execution);
		});
	});
	await vscode.commands.executeCommand(command, runnable);
	const execution = await started;
	execution.terminate();
	return execution.task.execution as vscode.ProcessExecution;
}

// Opens stand-ins for the shells named `names`, runs Run in REPL on `runnable`
// and returns what each shell received.
async function sentToShells(names: string[], runnable: unknown): Promise<string[]> {
	const inputs = names.map(() => '');
	const received = new vscode.EventEmitter<void>();
	const shells: vscode.Terminal[] = [];
	try {
		for (const [index, name] of names.entries()) {
			const opened = new vscode.EventEmitter<void>();
			const shell = vscode.window.createTerminal({
				name,
				pty: {
					onDidWrite: new vscode.EventEmitter<string>().event,
					open: () => opened.fire(),
					close: () => undefined,
					handleInput: data => {
						inputs[index] += data;
						received.fire();
					},
				},
			});
			shells.push(shell);
			shell.show();
			await new Promise(resolve => opened.event(resolve));
		}
		const sent = new Promise(resolve => received.event(resolve));
		await vscode.commands.executeCommand('elp.runInteractive', runnable);
		await sent;
	} finally {
		shells.forEach(shell => shell.dispose());
	}
	return inputs.map(input => input.trim());
}

suite('Lenses', () => {
	test('Run runs a Buck2 test with buck2 test', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');
		const args = ['root//app_a:app_a_SUITE', '--', '--regex', '.my_case$'];

		const process = await startedProcess('elp.runSingle', buck2Runnable('test', args));

		assert.strictEqual(process.process, 'buck2');
		assert.deepStrictEqual(process.args, ['--client-metadata=id=elp_vscode', 'test', ...args]);
		assert.strictEqual(process.options?.cwd, vscode.workspace.workspaceFolders![0].uri.fsPath);
	});

	test('Run in REPL opens a Buck2 suite with buck2 run', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');

		const process = await startedProcess('elp.openInteractive', buck2Runnable('run', ['root//app_a:app_a_SUITE']));

		assert.strictEqual(process.process, 'buck2');
		assert.deepStrictEqual(process.args, ['--client-metadata=id=elp_vscode', 'run', 'root//app_a:app_a_SUITE']);
	});

	test('Run in REPL runs a Buck2 test in the open shell of its suite', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');
		const inputs = await sentToShells(
			['Buck2 REPL (root//app_b:app_b_SUITE)', 'Buck2 REPL (root//app_a:app_a_SUITE)'],
			buck2Runnable('run', ['app_a_SUITE - .my_case']),
		);

		assert.deepStrictEqual(inputs, ['', 'test:run("app_a_SUITE - .my_case$").']);
	});

	test('Run in REPL runs a Buck2 test in the open Debug REPL', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');
		const inputs = await sentToShells(['EDB Shell'], buck2Runnable('run', ['app_a_SUITE - .my_case']));

		assert.deepStrictEqual(inputs, ['test:run("app_a_SUITE - .my_case$").']);
	});

	test('Run in REPL on a Buck2 test offers to open the suite with buck2 run', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');
		// There is no shell open, and the user accepts to open one.
		const window = vscode.window as { showInformationMessage: (message: string, ...items: string[]) => Thenable<string | undefined> };
		const showInformationMessage = window.showInformationMessage;
		window.showInformationMessage = async (_message, ...items) => items.find(item => item === 'Open REPL');
		let process;
		try {
			process = await startedProcess('elp.runInteractive', buck2Runnable('run', ['app_a_SUITE - .my_case']));
		} finally {
			window.showInformationMessage = showInformationMessage;
		}

		assert.strictEqual(process.process, 'buck2');
		assert.deepStrictEqual(process.args, ['--client-metadata=id=elp_vscode', 'run', 'root//app_a:app_a_SUITE']);
	});
});
