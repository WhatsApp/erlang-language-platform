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
import * as fs from 'fs';
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

// Runs a Debug lens command and returns the configuration it starts debugging with.
async function debugConfiguration(command: string, runnable: unknown): Promise<vscode.DebugConfiguration> {
	const debug = vscode.debug as { startDebugging: typeof vscode.debug.startDebugging };
	const startDebugging = debug.startDebugging;
	let configuration: vscode.DebugConfiguration | undefined;
	debug.startDebugging = async (_folder, nameOrConfiguration) => {
		configuration = nameOrConfiguration as vscode.DebugConfiguration;
		return true;
	};
	try {
		await vscode.commands.executeCommand(command, runnable);
	} finally {
		debug.startDebugging = startDebugging;
	}
	assert.ok(configuration, `${command} did not start debugging`);
	return configuration;
}

// Runs `body` with the user settings `values` of `section`, then removes them.
async function withSettings(section: string, values: Record<string, unknown>, body: () => Promise<void>): Promise<void> {
	const config = vscode.workspace.getConfiguration(section);
	for (const [key, value] of Object.entries(values)) {
		await config.update(key, value, vscode.ConfigurationTarget.Global);
	}
	try {
		await body();
	} finally {
		for (const key of Object.keys(values)) {
			await config.update(key, undefined, vscode.ConfigurationTarget.Global);
		}
	}
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

	test('Debug debugs a Buck2 test in the test shell of its suite, opened by the script of the extension', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');
		const args = ['root//app_a:app_a_SUITE', '--', '--regex', '.my_case$'];

		const configuration = await debugConfiguration('elp.debugSingle', buck2Runnable('test', args));

		assert.strictEqual(configuration.type, 'erlang-edb');
		assert.strictEqual(configuration.request, 'launch');
		assert.deepStrictEqual(
			configuration.config, { nameDomain: 'shortnames', nodeInitCodeInEnvVar: 'EDB_DAP_DEBUGGEE_INIT', timeout: 300 });
		const terminal = configuration.runInTerminal;
		assert.strictEqual(terminal.cwd, vscode.workspace.workspaceFolders![0].uri.fsPath);
		const [shell, script, ...scriptArgs] = terminal.args;
		assert.strictEqual(shell, 'bash');
		assert.ok(script.endsWith('/bin/run-buck2-test.sh') && fs.existsSync(script), script);
		assert.deepStrictEqual(scriptArgs, [
			'--node-setup-code-from-env=EDB_DAP_DEBUGGEE_INIT', '--interactive=false', 'root//app_a:app_a_SUITE', 'app_a_SUITE - .my_case$',
		]);
	});

	test('Debug on a suite opens the test shell of a Buck2 suite under the debugger', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');

		const configuration = await debugConfiguration('elp.debugInteractive', buck2Runnable('test', ['root//app_a:app_a_SUITE']));

		assert.deepStrictEqual(configuration.runInTerminal.args.slice(-2), ['--interactive=true', 'root//app_a:app_a_SUITE']);
	});

	test('Debug builds a Buck2 test in the mode of erlangDap.buck2Mode', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');
		let configuration: vscode.DebugConfiguration | undefined;
		await withSettings('erlangDap', { buck2Mode: '@root//mode/dbg' }, async () => {
			configuration = await debugConfiguration('elp.debugSingle', buck2Runnable('test', ['root//app_a:app_a_SUITE']));
		});

		assert.deepStrictEqual(configuration!.runInTerminal.args.slice(2, 5), [
			'--node-setup-code-from-env=EDB_DAP_DEBUGGEE_INIT', '--mode=@root//mode/dbg', '--interactive=false',
		]);
	});

	test('Debug runs the command of erlangDap.runInTerminalArgsDebug with the debugger configuration of erlangDap.debuggerConfig', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');
		let configuration: vscode.DebugConfiguration | undefined;
		const settings = {
			runInTerminalArgsDebug: ' buck2  run root//tools:debug -- ',
			debuggerConfig: { nameDomain: 'longnames', nodeInitCodeInEnvVar: 'INIT', timeout: 60 },
		};
		await withSettings('erlangDap', settings, async () => {
			configuration = await debugConfiguration('elp.debugSingle', buck2Runnable('test', ['root//app_a:app_a_SUITE']));
		});

		assert.deepStrictEqual(configuration!.runInTerminal.args, [
			'buck2', 'run', 'root//tools:debug', '--', '--interactive=false', 'root//app_a:app_a_SUITE', 'app_a_SUITE - .my_case$',
		]);
		assert.deepStrictEqual(configuration!.config, { nameDomain: 'longnames', nodeInitCodeInEnvVar: 'INIT', timeout: 60 });
	});

	test('Debug starts the "Buck2 Debug (EDB)" launch configuration instead', async () => {
		await openWorkspaceFile('hover/src/doc_examples.erl');
		let configuration: vscode.DebugConfiguration | undefined;
		const launch = {
			type: 'erlang-edb',
			request: 'launch',
			name: 'Buck2 Debug (EDB)',
			runInTerminal: { kind: 'integrated', args: ['my-test-shell', '--verbose'] },
			config: { nameDomain: 'longnames' },
		};
		await withSettings('', { launch: { configurations: [launch] } }, async () => {
			configuration = await debugConfiguration('elp.debugInteractive', buck2Runnable('test', ['root//app_a:app_a_SUITE']));
		});

		assert.deepStrictEqual(
			configuration!.runInTerminal.args, ['my-test-shell', '--verbose', '--interactive=true', 'root//app_a:app_a_SUITE']);
		assert.deepStrictEqual(configuration!.config, { nameDomain: 'longnames' });
	});
});
