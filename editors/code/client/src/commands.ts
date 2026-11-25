/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

import { execFile } from 'child_process';
import { promisify } from 'util';
import * as vscode from 'vscode';
import { pathWithErlangInstallation } from './erlang';
import * as edbDebugger from './debugger';
import { LanguageClient } from 'vscode-languageclient/node';
import { openExternalDocs } from './docs';
import { outputChannel } from './logging';
import { Telemetry } from './telemetry';
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

export function registerCommands(
    context: vscode.ExtensionContext,
    client: LanguageClient,
    telemetry: Telemetry,
) {
    context.subscriptions.push(
        vscode.commands.registerCommand(
            'elp.runSingle',
            async (runnable: Runnable) => {
                telemetry.logEvent('ErlangLens', { source: 'runSingle', runnable });
                await runSingle(runnable);
            },
        ),
        vscode.commands.registerCommand(
            'elp.debugSingle',
            async (runnable: Runnable) => {
                telemetry.logEvent('ErlangLens', { source: 'debugSingle', runnable });
                await debug(runnable, false, context.extensionUri);
            },
        ),
        vscode.commands.registerCommand(
            'elp.openInteractive',
            async (runnable: Runnable) => {
                telemetry.logEvent('ErlangLens', { source: 'openInteractive', runnable });
                await openInteractive(runnable);
            },
        ),
        vscode.commands.registerCommand(
            'elp.debugInteractive',
            async (runnable: Runnable) => {
                telemetry.logEvent('ErlangLens', { source: 'debugInteractive', runnable });
                await debug(runnable, true, context.extensionUri);
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
                telemetry.logEvent('ErlangHoverAction', { source: 'openUri', uri });
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
                telemetry.logEvent('ErlangCommand', { source: 'restartServer' }, true);
                await restartServer(client);
            },
        ),
    );
}

export async function restartServer(client: LanguageClient): Promise<void> {
    // We deliberately don't use `client.restart()`: it does `await stop(); start()`,
    // so when ELP is wedged and doesn't answer the LSP `shutdown`/`exit` handshake,
    // `stop()` rejects on timeout and `start()` is never reached -- leaving the user
    // with no server at all, which is the exact situation restart needs to recover.
    // Instead, stop with a short timeout (the process is force-killed regardless) and
    // always start afterwards. After a failed stop the client is in `Stopped` state
    // with its connection cleared, so `start()` is valid and spawns a fresh server.
    try {
        await client.stop(500);
    } catch (err) {
        log.appendLine(`ELP stop() failed during restart, starting a fresh server anyway: ${String(err)}`);
    }
    await client.start();
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

export async function debug(runnable: Runnable, interactive: boolean, extensionUri: vscode.Uri): Promise<void> {
    log.appendLine(`Debug: ${runnable.kind}, interactive: ${interactive}`);
    if (runnable.kind === 'buck2') {
        const base = userBuck2DebugConfiguration() ?? defaultBuck2DebugConfiguration(extensionUri);
        const cwd = await buck2ProjectRoot(runnable.args.workspaceRoot);
        const debugConfiguration = buck2DebugConfiguration(base, runnable, interactive, cwd);
        log.appendLine(JSON.stringify(debugConfiguration, null, 2));
        await vscode.debug.startDebugging(undefined, debugConfiguration);
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

// A launch configuration with this name replaces the default configuration of
// the Buck2 Debug lenses.
const BUCK2_DEBUG_CONFIGURATION_NAME = 'Buck2 Debug (EDB)';

interface EdbConfig {
    nameDomain: string;
    nodeInitCodeInEnvVar?: string;
    timeout?: number;
    stripSourcePrefix?: string;
}

const DEFAULT_EDB_CONFIG: EdbConfig = {
    nameDomain: 'shortnames',
    nodeInitCodeInEnvVar: 'EDB_DAP_DEBUGGEE_INIT',
    // Includes building the suite.
    timeout: 300,
};

function userBuck2DebugConfiguration(): vscode.DebugConfiguration | undefined {
    for (const folder of vscode.workspace.workspaceFolders ?? []) {
        const configurations =
            vscode.workspace.getConfiguration('launch', folder.uri).get<vscode.DebugConfiguration[]>('configurations') ?? [];
        const configuration = configurations.find(
            c => c.type === edbDebugger.DEBUG_TYPE && c.request === 'launch' && c.name === BUCK2_DEBUG_CONFIGURATION_NAME,
        );
        if (configuration) {
            return configuration;
        }
    }
    return undefined;
}

// The Buck2 Debug lenses run the `bin/run-buck2-test.sh` script of the
// extension, or the command of `erlangDap.runInTerminalArgsDebug`.
export function defaultBuck2DebugConfiguration(extensionUri: vscode.Uri): vscode.DebugConfiguration {
    const settings = vscode.workspace.getConfiguration('erlangDap');
    const config: EdbConfig = { ...DEFAULT_EDB_CONFIG, ...settings.get<Partial<EdbConfig>>('debuggerConfig') };
    const command = (settings.get<string>('runInTerminalArgsDebug') || '').trim();
    const mode = settings.get<string>('buck2Mode') || '';
    const args = command
        ? command.split(/\s+/)
        : [
            'bash',
            vscode.Uri.joinPath(extensionUri, 'bin', 'run-buck2-test.sh').fsPath,
            `--node-setup-code-from-env=${config.nodeInitCodeInEnvVar}`,
            ...(mode ? [`--mode=${mode}`] : []),
        ];
    return {
        type: edbDebugger.DEBUG_TYPE,
        name: BUCK2_DEBUG_CONFIGURATION_NAME,
        request: 'launch',
        runInTerminal: { kind: 'integrated', args, argsCanBeInterpretedByShell: false },
        config,
    };
}

// Adds the test of `runnable` to the command of `base`, which then runs it
// and stops, or opens the test shell of its suite when `interactive`.
//
// `cwd` is the root of the Buck2 project, which the source paths in the
// compiled modules are relative to.
export function buck2DebugConfiguration(
    base: vscode.DebugConfiguration,
    runnable: Runnable & Buck2Runnable,
    interactive: boolean,
    cwd: string,
): vscode.DebugConfiguration {
    const { target, id } = runnable.args;
    const testArgs = interactive ? ['--interactive=true', target] : ['--interactive=false', target, `${id}$`];
    return {
        ...base,
        runInTerminal: {
            ...base.runInTerminal,
            title: base.runInTerminal?.title ?? (interactive ? EDB_SHELL_TITLE : `EDB - ${id}`),
            cwd,
            args: [...(base.runInTerminal?.args ?? []), ...testArgs],
            env: { PATH: pathWithErlangInstallation(), ...base.runInTerminal?.env },
        },
    };
}

// The root of the Buck2 project `dir` is in, or `dir` if Buck2 cannot tell.
async function buck2ProjectRoot(dir: string): Promise<string> {
    try {
        const { stdout } = await promisify(execFile)('buck2', ['--client-metadata=id=elp_vscode', 'root', '--kind=project'], {
            cwd: dir,
            env: { ...process.env, PATH: pathWithErlangInstallation() },
        });
        return stdout.trim() || dir;
    } catch (error) {
        log.appendLine(`Could not find the Buck2 project root of ${dir}: ${error}`);
        return dir;
    }
}
