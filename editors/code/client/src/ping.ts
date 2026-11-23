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
import { LanguageClient, RequestType } from 'vscode-languageclient/node';

// The client sends a single parameter by position, as the `["ping"]` ELP expects.
const pingRequest = new RequestType<string, string, void>('elp/ping');

const PING_TIMEOUT_MS = 5000;

// Resolves with the server's answer, or rejects if it takes longer than
// `PING_TIMEOUT_MS`, e.g. because the server is stuck.
export function ping(client: LanguageClient): Promise<string> {
	let timer: NodeJS.Timeout | undefined;
	const timeout = new Promise<never>((_resolve, reject) => {
		timer = setTimeout(() => reject(new Error(`No answer within ${PING_TIMEOUT_MS}ms`)), PING_TIMEOUT_MS);
	});
	return Promise.race([client.sendRequest(pingRequest, 'ping'), timeout]).finally(() => clearTimeout(timer));
}

export async function pingAndShow(client: LanguageClient): Promise<void> {
	try {
		await vscode.window.showInformationMessage(`Erlang language server answered: ${await ping(client)}`);
	} catch (error) {
		await vscode.window.showErrorMessage(`Error while pinging the Erlang language server: ${error}`);
	}
}
