/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import * as vscode from 'vscode';

const DEBUG_TYPE = 'erlang-edb';
const CONFIG = 'erlangDap';

interface EdbDebugConfiguration extends vscode.DebugConfiguration {
    launchCommand: {
        command: string;
        arguments?: Array<string>;
        cwd?: string;
    };
    targetNode: {
        name: string;
        cookie?: string;
    };
    stripSourcePrefix?: string;
}

export function activateDebugger(context: vscode.ExtensionContext) {
    const provider = new EDBConfigurationProvider();
    context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider(DEBUG_TYPE, provider));

    const factory = new DebugAdapterExecutableFactory(context.extensionUri);
    context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory(DEBUG_TYPE, factory));
}

class DebugAdapterExecutableFactory implements vscode.DebugAdapterDescriptorFactory {

    extensionUri: vscode.Uri;

    constructor(extensionUri: vscode.Uri) {
        this.extensionUri = extensionUri;
    }

    createDebugAdapterDescriptor(_session: vscode.DebugSession, executable: vscode.DebugAdapterExecutable | undefined): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
        const dapConfig = vscode.workspace.getConfiguration(CONFIG);
        const commandString = dapConfig.get<string>('command') || '';
        let command: string;
        let args: string[];
        if (commandString.length > 0) {
            command = commandString.split(' ')[0];
            args = commandString.split(' ').slice(1);
        } else {
            command = vscode.Uri.joinPath(this.extensionUri, 'bin', 'edb').toString();
            args = ['dap'];
        }
        let path = (process.env.PATH || '').split(':');
        const erlangInstallationPath = dapConfig.get<string>('erlangInstallationPath') || '';
        if (erlangInstallationPath != "") {
            path.unshift(erlangInstallationPath);
        }
        const options = {env: {PATH: path.join(':')}};
        executable = new vscode.DebugAdapterExecutable(command, args, options);
        return executable;
    }
}

class EDBConfigurationProvider implements vscode.DebugConfigurationProvider {

    resolveDebugConfiguration(folder: vscode.WorkspaceFolder | undefined, config: vscode.DebugConfiguration, token?: vscode.CancellationToken): vscode.ProviderResult<vscode.DebugConfiguration> {

        // if launch.json is missing or empty
        if (!config.type && !config.request && !config.name) {
            const editor = vscode.window.activeTextEditor;
            if (editor && editor.document.languageId === 'erlang') {
                config.type = DEBUG_TYPE;
                config.name = 'Erlang EDB';
                config.request = 'launch';
                config.launchCommand = {
                    cwd: "${workspaceFolder}",
                    command: "rebar3",
                    arguments: [
                        "shell",
                        "--sname",
                        "debuggee"
                    ]
                };
                config.targetNode = {
                    name: "debuggee"
                };
            }
        }

        return config;
    }
}
