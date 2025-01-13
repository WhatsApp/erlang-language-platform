/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Based on the Microsoft template code at https://github.com/Microsoft/vscode-extension-samples
import { workspace, ExtensionContext } from 'vscode';
import { activateDebugger } from './debugger';
import * as path from 'path';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions
} from 'vscode-languageclient/node';

let client: LanguageClient;

export const ELP = 'elpClient';

export function activate(context: ExtensionContext) {

	// Options to control the language server
	const config = workspace.getConfiguration(ELP);
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
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'elp',
		'Erlang Language Platform',
		serverOptions,
		clientOptions
	);

	// Activate the DAP Debugger
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
