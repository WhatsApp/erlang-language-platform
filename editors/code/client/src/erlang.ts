/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

import * as path from 'path';
import * as vscode from 'vscode';

function erlangInstallationPath(): string {
    const configured = vscode.workspace
        .getConfiguration('elpClient')
        .get<string>('erlangInstallationPath') || '';
    if (configured !== '') {
        return configured;
    }
    // Deprecated name, from when only the debugger used this setting.
    return vscode.workspace.getConfiguration('erlangDap').get<string>('erlangInstallationPath') || '';
}

// The `PATH` for processes that need Erlang: the language server, the debugger, tests and erlfmt.
export function pathWithErlangInstallation(): string {
    const currentPath = process.env.PATH || '';
    const installationPath = erlangInstallationPath();
    if (installationPath === '') {
        return currentPath;
    }
    return [installationPath, currentPath].join(path.delimiter);
}
