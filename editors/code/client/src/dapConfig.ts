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

export const DAP_CONFIG = 'erlangDap';

export function withErlangInstallationPath(): string {
    const dapConfig = vscode.workspace.getConfiguration(DAP_CONFIG);
    const erlangInstallationPath = dapConfig.get<string>('erlangInstallationPath') || '';
    if (erlangInstallationPath != "") {
        const path = (process.env.PATH || '').split(':');
        path.unshift(erlangInstallationPath);
        return path.join(':');
    } else {
        return process.env.PATH;
    }
}

export function command(): string {
    const dapConfig = vscode.workspace.getConfiguration(DAP_CONFIG);
    return dapConfig.get<string>('command') || '';
}
