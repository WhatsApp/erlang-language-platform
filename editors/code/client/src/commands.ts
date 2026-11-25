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
import { pathWithErlangInstallation } from './erlang';
import * as edbDebugger from './debugger';
import { LanguageClient } from 'vscode-languageclient/node';
import { openExternalDocs } from './docs';
import { outputChannel } from './logging';
import { expandMacro } from './macro';
import { ping, pingAndShow } from './ping';

export type Runnable = {
    label: string;
    location?: vscode.LocationLink;
} & (Buck2Runnable | Rebar3Runnable);

type Buck2Runnable = {
    kind: 'buck2';
    args: Buck2RunnableArgs;
};

type Rebar3Runnable = {
    kind: 'rebar3';
    args: Rebar3RunnableArgs;
};

export type Buck2RunnableArgs = {
    command: string;
    args: string[];
    target: string;
    id: string;
} & CommonRunnableArgs;

export type Rebar3RunnableArgs = {
    command: string;
    args: string[];
} & CommonRunnableArgs;

export type CommonRunnableArgs = {
    workspaceRoot: string;
};

const log = outputChannel();

const EDB_SHELL_TITLE = "EDB Shell";
const REBAR3_SHELL_TITLE = "elp: Rebar3";
const BUCK2_SHELL_TITLE = "Buck2 REPL";

export function registerCommands(context: vscode.ExtensionContext, client: LanguageClient) {
    context.subscriptions.push(
        vscode.commands.registerCommand(
            'elp.runSingle',
            async (runnable: Runnable) => {
                await runSingle(runnable);
            },
        ),
        vscode.commands.registerCommand(
            'elp.debugSingle',
            async (runnable: Runnable) => {
                await debug(runnable, false);
            },
        ),
        vscode.commands.registerCommand(
            'elp.openInteractive',
            async (runnable: Runnable) => {
                await openInteractive(runnable);
            },
        ),
        vscode.commands.registerCommand(
            'elp.debugInteractive',
            async (runnable: Runnable) => {
                await debug(runnable, true);
            },
        ),
        vscode.commands.registerCommand(
            'elp.runInteractive',
            async (runnable: Runnable) => {
                await runInteractive(runnable);
            },
        ),
        vscode.commands.registerCommand(
            'elp.expandMacro',
            async () => {
                await expandMacro(client);
            },
        ),
        vscode.commands.registerCommand(
            'elp.externalDocs',
            async () => {
                await openExternalDocs(client);
            },
        ),
        vscode.commands.registerCommand(
            'elp.openExtensionLogs',
            () => log.show(),
        ),
        vscode.commands.registerCommand(
            'elp.openLanguageClientLogs',
            () => client.outputChannel.show(),
        ),
        vscode.commands.registerCommand(
            'elp.openUri',
            async (uri: string) => {
                await vscode.env.openExternal(vscode.Uri.parse(uri));
            },
        ),
        vscode.commands.registerCommand(
            'elp.ping',
            () => ping(client),
        ),
        vscode.commands.registerCommand(
            'elp.pingAndShow',
            async () => {
                await pingAndShow(client);
            },
        ),
        vscode.commands.registerCommand(
            'elp.restartServer',
            async () => {
                await client.restart();
            },
        ),
    );
}

export function runSingle(runnable: Runnable): Thenable<vscode.TaskExecution> {
    log.appendLine(`Run single: ${runnable.kind}`);
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

export async function runInteractive(runnable: Runnable): Promise<void> {
  if (runnable.kind === 'buck2') {
    await runInBuck2Shell(runnable);
    return;
  }
  const terminal = vscode.window.terminals.find(
    terminal => terminal.name.includes(REBAR3_SHELL_TITLE) || terminal.name.includes(EDB_SHELL_TITLE),
  );
  const run_cmd = `r3:do(\"ct ${runnable.args.args.join(" ")}\").`;
  if (terminal) {
    terminal.show();
    terminal.sendText(run_cmd);
  } else {
    await vscode.window
      .showInformationMessage('No active REPL. Open one, first.', 'Open REPL')
      .then(async selection => {
        if (selection === 'Open REPL') {
          await vscode.commands.executeCommand("elp.openInteractive");
        }
      });
  }
}

function buck2ShellTitle(target: string): string {
  return `${BUCK2_SHELL_TITLE} (${target})`;
}

// Runs the test in the shell that `buck2 run` opens for the test suite, or in
// the Debug REPL, with the `test` module of the Erlang prelude. The runnable's
// argument is the test id, which `test:run/1` matches as a regex.
async function runInBuck2Shell(runnable: Runnable & Buck2Runnable): Promise<void> {
  const title = buck2ShellTitle(runnable.args.target);
  const terminal = vscode.window.terminals.find(
    terminal =>
      (terminal.name.includes(title) || terminal.name.includes(EDB_SHELL_TITLE)) && terminal.exitStatus === undefined,
  );
  if (terminal) {
    terminal.show();
    terminal.sendText(`test:run("${runnable.args.args.join()}$").`);
    return;
  }
  const selection = await vscode.window.showInformationMessage('No active REPL. Open one, first.', 'Open REPL');
  if (selection === 'Open REPL') {
    await openInteractive({ ...runnable, args: { ...runnable.args, args: [runnable.args.target] } });
  }
}

export function openInteractive(runnable: Runnable): Thenable<vscode.TaskExecution> {
  // The shell's terminal takes the task name, which is how `runInBuck2Shell` finds it.
  const task = createTask(runnable.kind === 'buck2' ? { ...runnable, label: buck2ShellTitle(runnable.args.target) } : runnable);
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
    if (runnable.kind === 'buck2') {
        args.unshift('--client-metadata=id=elp_vscode');
    }

    const definition: TaskDefinition = {
        type: runnable.kind,
        command,
        args,
        cwd: runnable.args.workspaceRoot,
        env: { "PATH": pathWithErlangInstallation() },
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

export async function debug(runnable: Runnable, interactive: boolean): Promise<void> {
    log.appendLine(`Debug: ${runnable.kind}, interactive: ${interactive}`);
    if (runnable.kind === 'buck2') {
        await vscode.window.showErrorMessage('Debugging Buck2 tests is not supported yet.');
        return;
    }
    const cmd = interactive
        ? 'ok'
        : `Agent = self(), spawn(fun() -> catch gen_server:call(Agent, {cmd, default, ct, \"${runnable.args.args.join(" ")}\"}, infinity), erlang:halt(0) end)`;
    const debugConfiguration = {
        type: edbDebugger.DEBUG_TYPE,
        name: 'Erlang EDB',
        request: 'launch',
        runInTerminal: {
            kind: "integrated",
            title: EDB_SHELL_TITLE,
            cwd: "${workspaceFolder}",
            args: ["bash", "-c", "rebar3 as test shell --eval \"$EDB_DAP_NODE_INIT, $REBAR3_SHELL_CT_RUN_CMD\""],
            env: {
                REBAR3_SHELL_CT_RUN_CMD: cmd,
                "PATH": pathWithErlangInstallation(),
            },
            argsCanBeInterpretedByShell: false,
        },
        config: {
            nameDomain: "shortnames",
            nodeInitCodeInEnvVar: "EDB_DAP_NODE_INIT",
            timeout: 60,
        },
    };
    log.appendLine(JSON.stringify(debugConfiguration, null, 2));
    await vscode.debug.startDebugging(undefined, debugConfiguration);
}
