/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

import * as vscode from 'vscode';
import {
	ClientCapabilities,
	FeatureState,
	LanguageClient,
	NotificationType,
	State,
	StaticFeature,
} from 'vscode-languageclient/node';

export type ServerStatusState = 'Loading' | 'Running' | 'ShuttingDown' | 'Invalid';

interface ServerStatusParams {
	status: ServerStatusState;
}

const serverStatusNotification = new NotificationType<ServerStatusParams>('elp/status');

// Shows the last status ELP reported while an Erlang file is active, on an
// error background while the language client is not running. The tooltip
// links to the logs and pings the server.
export class ServerStatus implements vscode.Disposable {
	readonly statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left);
	private readonly onDidChangeEmitter = new vscode.EventEmitter<ServerStatusState>();
	private readonly editorListener: vscode.Disposable;
	private state?: ServerStatusState;

	readonly onDidChange = this.onDidChangeEmitter.event;

	constructor() {
		this.statusBarItem.text = 'ELP';
		const tooltip = new vscode.MarkdownString(
			'[$(terminal) Open Extension Logs](command:elp.openExtensionLogs "Open the Extension logs")\n\n' +
			'[$(terminal) Open Language Client Logs](command:elp.openLanguageClientLogs "Open the Language Client logs")\n\n' +
			'[$(megaphone) Ping Language Server](command:elp.pingAndShow "Ping Language Server")',
			true,
		);
		tooltip.isTrusted = true;
		this.statusBarItem.tooltip = tooltip;
		this.updateVisibility(vscode.window.activeTextEditor);
		this.editorListener = vscode.window.onDidChangeActiveTextEditor(editor => this.updateVisibility(editor));
	}

	get current(): ServerStatusState | undefined {
		return this.state;
	}

	updateClientState(state: State): void {
		this.statusBarItem.backgroundColor =
			state === State.Running ? undefined : new vscode.ThemeColor('statusBarItem.errorBackground');
	}

	update(state: ServerStatusState): void {
		this.state = state;
		this.statusBarItem.text = `ELP: ${state}`;
		this.onDidChangeEmitter.fire(state);
	}

	dispose(): void {
		this.editorListener.dispose();
		this.statusBarItem.dispose();
		this.onDidChangeEmitter.dispose();
	}

	private updateVisibility(editor: vscode.TextEditor | undefined): void {
		if (editor?.document.languageId === 'erlang') {
			this.statusBarItem.show();
		} else {
			this.statusBarItem.hide();
		}
	}
}

// Opts in to `elp/status` notifications. The client disposes its features
// when it stops and initializes them again when it restarts.
class ServerStatusFeature implements StaticFeature {
	private listener?: vscode.Disposable;

	constructor(private readonly client: LanguageClient, private readonly status: ServerStatus) { }

	fillClientCapabilities(capabilities: ClientCapabilities): void {
		capabilities.experimental = { ...capabilities.experimental, serverStatusNotification: true };
	}

	initialize(): void {
		this.listener = this.client.onNotification(serverStatusNotification, ({ status }) => this.status.update(status));
	}

	getState(): FeatureState {
		return { kind: 'static' };
	}

	dispose(): void {
		this.listener?.dispose();
		this.listener = undefined;
	}
}

export function registerServerStatus(context: vscode.ExtensionContext, client: LanguageClient): ServerStatus {
	const status = new ServerStatus();
	context.subscriptions.push(status, client.onDidChangeState(({ newState }) => status.updateClientState(newState)));
	client.registerFeature(new ServerStatusFeature(client, status));
	return status;
}
