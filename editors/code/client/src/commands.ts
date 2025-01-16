/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import * as vscode from 'vscode';
import * as dapConfig from './dapConfig';

export type Runnable = {
    label: string;
    location?: vscode.LocationLink;
} & (Rebar3Runnable);

type Rebar3Runnable = {
    kind: 'rebar3';
    args: Rebar3RunnableArgs;
};

export type Rebar3RunnableArgs = {
    command: string;
    args: string[];
} & CommonRunnableArgs;

export type CommonRunnableArgs = {
    workspaceRoot: string;
};

export function registerCommands(context: vscode.ExtensionContext) {
    // elp.runSingle
    context.subscriptions.push(
        vscode.commands.registerCommand(
            'elp.runSingle',
            async (runnable: Runnable) => {
                await runSingle(runnable);
            },
        ),
    );
}

export function runSingle(runnable: Runnable): Thenable<vscode.TaskExecution> {
    const task = createTask(runnable);
    task.group = vscode.TaskGroup.Build;
    task.presentationOptions = {
        reveal: vscode.TaskRevealKind.Always,
        panel: vscode.TaskPanelKind.Dedicated,
        clear: true,
    };
    const command = [runnable.args.command, ...runnable.args.args].join(' ');
    return vscode.tasks.executeTask(task);
}

export function createTask(runnable: Runnable): vscode.Task {
    const command = runnable.kind;
    const args = [runnable.args.command, ...runnable.args.args];

    const definition: TaskDefinition = {
        type: runnable.kind,
        command,
        args,
        cwd: runnable.args.workspaceRoot,
        env: { "PATH": dapConfig.withErlangInstallationPath() },
    };

    const task = buildTask(definition, runnable.label, command, args);

    task.presentationOptions.clear = true;
    task.presentationOptions.focus = false;

    return task;
}

export interface TaskDefinition extends vscode.TaskDefinition {
    command?: string;
    args?: string[];
    cwd?: string;
    env?: { [key: string]: string };
}

export function buildTask(
    definition: TaskDefinition,
    name: string,
    command: string,
    args: string[],
): vscode.Task {
    const task_source = 'elp';
    const exec = new vscode.ProcessExecution(command, args, definition);
    return new vscode.Task(definition, vscode.TaskScope.Workspace, name, task_source, exec, []);
}
