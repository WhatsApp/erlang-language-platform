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
import { execFileSync } from 'child_process';
import * as vscode from 'vscode';
import { ElpExtensionApi } from '../extension';

const EXTENSION_ID = 'erlang-language-platform.erlang-language-platform';

// The API the extension returns from `activate`.
export async function extensionApi(): Promise<ElpExtensionApi> {
	return await vscode.extensions.getExtension<ElpExtensionApi>(EXTENSION_ID)!.activate();
}

// Opens a file of the test workspace and waits until ELP has loaded its project.
export async function openWorkspaceFile(relativePath: string): Promise<vscode.TextDocument> {
	const folder = vscode.workspace.workspaceFolders![0];
	const document = await vscode.workspace.openTextDocument(vscode.Uri.joinPath(folder.uri, relativePath));
	await vscode.window.showTextDocument(document);
	await waitForServerRunning();
	return document;
}

// Resolves once ELP has reported `Running` `times` times from now on.
export async function serverRunning(times: number): Promise<void> {
	const { serverStatus } = await extensionApi();
	let count = 0;
	await new Promise<void>(resolve => {
		const listener = serverStatus.onDidChange(state => {
			if (state === 'Running' && ++count === times) {
				listener.dispose();
				resolve();
			}
		});
	});
}

async function waitForServerRunning(): Promise<void> {
	const { serverStatus } = await extensionApi();
	if (serverStatus.current === 'Running') {
		return;
	}
	await new Promise<void>(resolve => {
		const listener = serverStatus.onDidChange(state => {
			if (state === 'Running') {
				listener.dispose();
				resolve();
			}
		});
	});
}

export async function hoverText(document: vscode.TextDocument, position: vscode.Position): Promise<string> {
	const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
		'vscode.executeHoverProvider', document.uri, position);
	return hovers
		.flatMap(hover => hover.contents)
		.map(content => typeof content === 'string' ? content : content.value)
		.join('\n');
}

export function diagnosticCode(diagnostic: vscode.Diagnostic): string | number | undefined {
	return typeof diagnostic.code === 'object' ? diagnostic.code.value : diagnostic.code;
}

// Resolves with what `pick` returns for the diagnostics of the document, as
// soon as it returns something.
function waitForDiagnostics<T>(
	document: vscode.TextDocument,
	pick: (diagnostics: vscode.Diagnostic[]) => T | undefined,
): Promise<T> {
	return new Promise(resolve => {
		const check = () => {
			const picked = pick(vscode.languages.getDiagnostics(document.uri));
			if (picked !== undefined) {
				listener.dispose();
				resolve(picked);
			}
		};
		const listener = vscode.languages.onDidChangeDiagnostics(check);
		check();
	});
}

// Resolves with the first diagnostic of the document that matches, as soon as
// ELP publishes one.
export function waitForDiagnostic(
	document: vscode.TextDocument,
	matches: (diagnostic: vscode.Diagnostic) => boolean,
): Promise<vscode.Diagnostic> {
	return waitForDiagnostics(document, diagnostics => diagnostics.find(matches));
}

// Resolves as soon as no diagnostic of the document matches, such as once ELP
// replaces the diagnostics it published before a reload.
export async function waitForNoDiagnostic(
	document: vscode.TextDocument,
	matches: (diagnostic: vscode.Diagnostic) => boolean,
): Promise<void> {
	await waitForDiagnostics(document, diagnostics => diagnostics.some(matches) ? undefined : true);
}

export async function replaceText(document: vscode.TextDocument, range: vscode.Range, text: string): Promise<void> {
	const edit = new vscode.WorkspaceEdit();
	edit.replace(document.uri, range, text);
	if (!await vscode.workspace.applyEdit(edit)) {
		throw new Error(`Could not edit ${document.uri.fsPath}`);
	}
}

// Runs `body`, then puts the document back to its original content on disk,
// for the tests that use the same file after it.
export async function restoringContent(document: vscode.TextDocument, body: () => Promise<void>): Promise<void> {
	const original = document.getText();
	try {
		await body();
	} finally {
		await replaceText(document, new vscode.Range(0, 0, document.lineCount, 0), original);
		if (!await document.save()) {
			throw new Error(`Could not save ${document.uri.fsPath}`);
		}
	}
}

// Runs `body` with `vscode.env.openExternal` recording the URIs it is asked to
// open instead of opening them.
export async function openedUris(body: () => Thenable<unknown>): Promise<vscode.Uri[]> {
	const env = vscode.env as { openExternal: typeof vscode.env.openExternal };
	const openExternal = env.openExternal;
	const uris: vscode.Uri[] = [];
	env.openExternal = async uri => {
		uris.push(uri);
		return true;
	};
	try {
		await body();
	} finally {
		env.openExternal = openExternal;
	}
	return uris;
}

// The lens whose title ends with `title` among those ELP shows in `document`.
// ELP shows no lenses while it reloads the project, as it does for the second
// load stage of a Buck2 project, so this asks again for up to a minute.
export async function codeLens(document: vscode.TextDocument, title: string): Promise<vscode.CodeLens> {
	const deadline = Date.now() + 60000;
	for (;;) {
		const lenses = await vscode.commands.executeCommand<vscode.CodeLens[]>(
			'vscode.executeCodeLensProvider', document.uri, 100);
		const lens = lenses.find(lens => lens.command?.title.endsWith(title));
		if (lens !== undefined) {
			return lens;
		}
		assert.ok(Date.now() < deadline, `No "${title}" lens among: ${lenses.map(lens => lens.command?.title).join(', ')}`);
		await new Promise(resolve => setTimeout(resolve, 500));
	}
}

export async function executeLens(lens: vscode.CodeLens): Promise<void> {
	await vscode.commands.executeCommand(lens.command!.command, ...(lens.command!.arguments ?? []));
}

// The major OTP release of the `erl` on the `PATH`.
export function otpRelease(): number {
	const release = execFileSync('erl', ['-noshell', '-eval', 'io:put_chars(erlang:system_info(otp_release)), halt().']);
	return parseInt(release.toString(), 10);
}

export interface Stop {
	reason: string;
	path: string | undefined;
	line: number;
}

// Runs `start`, which starts an EDB debug session, and returns where the
// debugger first stops, then stops debugging. Fails if the session ends
// before the debugger stops.
export async function firstStop(start: () => Promise<void>): Promise<Stop> {
	let tracker: vscode.Disposable | undefined;
	let terminated: vscode.Disposable | undefined;
	const stopped = new Promise<{ session: vscode.DebugSession; reason: string; threadId: number }>((resolve, reject) => {
		tracker = vscode.debug.registerDebugAdapterTrackerFactory('erlang-edb', {
			createDebugAdapterTracker: session => ({
				onDidSendMessage: message => {
					if (message.type === 'event' && message.event === 'stopped') {
						resolve({ session, reason: message.body.reason, threadId: message.body.threadId });
					}
				},
			}),
		});
		terminated = vscode.debug.onDidTerminateDebugSession(session => {
			if (session.type === 'erlang-edb') {
				reject(new Error(`Debug session "${session.name}" ended before the debugger stopped`));
			}
		});
	});
	try {
		await start();
		const { session, reason, threadId } = await stopped;
		const { stackFrames } = await session.customRequest('stackTrace', { threadId });
		await vscode.debug.stopDebugging(session);
		return { reason, path: stackFrames[0].source?.path, line: stackFrames[0].line };
	} finally {
		tracker?.dispose();
		terminated?.dispose();
	}
}

// Runs `body` with a breakpoint at `line` (0-based) of `document`.
export async function withBreakpoint(
	document: vscode.TextDocument,
	line: number,
	body: () => Promise<void>,
): Promise<void> {
	const breakpoint = new vscode.SourceBreakpoint(new vscode.Location(document.uri, new vscode.Position(line, 0)));
	vscode.debug.addBreakpoints([breakpoint]);
	try {
		await body();
	} finally {
		vscode.debug.removeBreakpoints([breakpoint]);
	}
}
