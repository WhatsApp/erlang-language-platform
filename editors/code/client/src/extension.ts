/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Based on the Microsoft template code at https://github.com/Microsoft/vscode-extension-samples
import * as vscode from 'vscode';
import { activateDebugger } from './debugger';
import { registerCommands } from './commands';
import { outputChannel } from './logging';
import * as path from 'path';

import {
	LanguageClient,
	LanguageClientOptions,
	ResolveCodeActionSignature,
	ServerOptions
} from 'vscode-languageclient/node';
import { CodeAction317, resolveCodeActionData } from './lspExtensions';

let client: LanguageClient;

export const ELP = 'elpClient';

export function activate(context: vscode.ExtensionContext) {
	const log = outputChannel();

	// Options to control the language server
	const config = vscode.workspace.getConfiguration(ELP);
	let serverPath = config.get<string>("serverPath");
	if (serverPath === "") {
		serverPath = context.asAbsolutePath(
			path.join('bin', 'elp')
		);
	}
	const serverArgs = config.get<string>("serverArgs", "server");
	const serverOptions: ServerOptions = {
		command: serverPath,
		args: serverArgs.split(" "),
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'erlang' }],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
		},
		middleware: {
			// Used to intercept "rename" requests, allowing the user to provide a custom new name
			resolveCodeAction: async (action: vscode.CodeAction, token: vscode.CancellationToken, next: ResolveCodeActionSignature): Promise<vscode.CodeAction> => {
				// The VS Code type definitions still do not take `data` into account, even if that's part of LSP 3.17
				// Therefore, we convert it to a custom type, operate on it and convert it back
				const action317 = action as unknown as CodeAction317;
				if ('data' in action317) {
					action317.data = await resolveCodeActionData(action317.data);
				}
				return next(action as unknown as vscode.CodeAction, token);
			}
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'elp',
		'Erlang Language Platform',
		serverOptions,
		clientOptions
	);

	log.appendLine('Registering commands');
	registerCommands(context, client);

	// Activate the DAP Debugger
	log.appendLine('Activating debugger');
	activateDebugger(context);

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
