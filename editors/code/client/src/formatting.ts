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
import * as fs from 'fs';
import * as path from 'path';
import { spawn } from 'child_process';
import * as dapConfig from './dapConfig';
import { outputChannel } from './logging';

const ERLFMT_CONFIG = 'elpClient.erlfmt';

const log = outputChannel();

// Only nag once about a missing erlfmt, since formatting can be triggered on every save.
// Reset once erlfmt runs, so a later breakage is reported again.
let reportedMissingErlfmt = false;

export function registerFormatter(context: vscode.ExtensionContext) {
    const bundledErlfmt = vscode.Uri.joinPath(context.extensionUri, 'bin', 'erlfmt').fsPath;
    const selector: vscode.DocumentSelector = { language: 'erlang' };
    context.subscriptions.push(
        vscode.languages.registerDocumentFormattingEditProvider(selector, {
            provideDocumentFormattingEdits: (document, _options, token) =>
                format(document, bundledErlfmt, [], token),
        }),
        vscode.languages.registerDocumentRangeFormattingEditProvider(selector, {
            // erlfmt formats whole top-level forms, so it may touch lines just outside the range.
            provideDocumentRangeFormattingEdits: (document, range, _options, token) =>
                format(document, bundledErlfmt, [`--range=${erlfmtRange(range)}`], token),
        }),
    );
}

// The configured executable, else the escript bundled with the extension (run through `escript`
// so that it also works on Windows), else `erlfmt` from the `PATH`.
async function erlfmtCommand(
    configured: string,
    bundledErlfmt: string,
): Promise<{ command: string; args: string[] }> {
    if (configured !== '') {
        return { command: configured, args: [] };
    }
    try {
        await fs.promises.access(bundledErlfmt);
        return { command: 'escript', args: [bundledErlfmt] };
    } catch {
        return { command: 'erlfmt', args: [] };
    }
}

// erlfmt takes 1-based inclusive lines. A selection of whole lines ends at column 0 of the
// line after the last selected one, which must not be included.
function erlfmtRange(range: vscode.Range): string {
    const endLine =
        range.end.character === 0 && range.end.line > range.start.line
            ? range.end.line - 1
            : range.end.line;
    return `${range.start.line + 1},${endLine + 1}`;
}

async function format(
    document: vscode.TextDocument,
    bundledErlfmt: string,
    rangeArgs: string[],
    token: vscode.CancellationToken,
): Promise<vscode.TextEdit[]> {
    const config = vscode.workspace.getConfiguration(ERLFMT_CONFIG, document);
    const erlfmt = await erlfmtCommand(config.get<string>('path') || '', bundledErlfmt);
    const command = erlfmt.command;
    const args = [...erlfmt.args, ...config.get<string[]>('args', []), ...rangeArgs, '-'];
    const input = document.getText();

    log.appendLine(`Formatting ${document.uri.fsPath}: ${command} ${args.join(' ')}`);
    const cwd = document.uri.scheme === 'file' ? path.dirname(document.uri.fsPath) : undefined;
    const result = await run(command, args, input, cwd, token);
    if (result.kind === 'cancelled') {
        return [];
    }
    if (result.kind === 'spawn_error') {
        log.appendLine(`Could not run erlfmt: ${result.message}`);
        if (!reportedMissingErlfmt) {
            reportedMissingErlfmt = true;
            void vscode.window.showErrorMessage(
                `Could not run erlfmt (${command}): ${result.message}. ` +
                `Check "${ERLFMT_CONFIG}.path" and "erlangDap.erlangInstallationPath".`,
            );
        }
        return [];
    }
    reportedMissingErlfmt = false;
    if (result.stderr !== '') {
        log.appendLine(result.stderr);
    }
    if (result.code !== 0) {
        log.appendLine(`erlfmt exited with code ${result.code}`);
        return [];
    }
    if (result.stdout === input || (result.stdout === '' && input !== '')) {
        return [];
    }
    const fullRange = document.validateRange(
        new vscode.Range(0, 0, Number.MAX_SAFE_INTEGER, Number.MAX_SAFE_INTEGER),
    );
    return [vscode.TextEdit.replace(fullRange, result.stdout)];
}

type RunResult =
    | { kind: 'exited'; code: number | null; stdout: string; stderr: string }
    | { kind: 'spawn_error'; message: string }
    | { kind: 'cancelled' };

function run(
    command: string,
    args: string[],
    input: string,
    cwd: string | undefined,
    token: vscode.CancellationToken,
): Promise<RunResult> {
    return new Promise(resolve => {
        const child = spawn(command, args, {
            cwd,
            env: { ...process.env, PATH: dapConfig.withErlangInstallationPath() },
        });
        const stdout: Buffer[] = [];
        const stderr: Buffer[] = [];
        const cancellation = token.onCancellationRequested(() => {
            child.kill();
            resolve({ kind: 'cancelled' });
        });
        child.stdout.on('data', (data: Buffer) => stdout.push(data));
        child.stderr.on('data', (data: Buffer) => stderr.push(data));
        child.on('error', err => {
            cancellation.dispose();
            resolve({ kind: 'spawn_error', message: err.message });
        });
        child.on('close', code => {
            cancellation.dispose();
            resolve({
                kind: 'exited',
                code,
                stdout: Buffer.concat(stdout).toString('utf8'),
                stderr: Buffer.concat(stderr).toString('utf8'),
            });
        });
        // erlfmt may exit before reading all of stdin (e.g. on bad arguments); the exit code reports that.
        child.stdin.on('error', () => undefined);
        child.stdin.end(input);
    });
}
