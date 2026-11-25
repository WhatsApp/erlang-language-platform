/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

import { defineConfig } from '@vscode/test-cli';
import { cpSync, mkdtempSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

const endToEnd = join(import.meta.dirname, '../../test/test_projects/end_to_end');
const launchArgs = ['--disable-extensions'];

// A copy of `end_to_end` without its Buck2 configuration, which ELP loads as a
// rebar3 project of the apps the rebar3 tests use, with a documentation URL.
// Breakpoints need modules compiled with `beam_debug_info`.
const rebar3Workspace = mkdtempSync(join(tmpdir(), 'elp-vscode-test-rebar3-'));
cpSync(endToEnd, rebar3Workspace, { recursive: true });
writeFileSync(join(rebar3Workspace, '.elp.toml'), '[docs]\nbase_url = "https://docs.example.com/"\n');
writeFileSync(join(rebar3Workspace, 'rebar.config'), `{project_app_dirs, ["definitions", "docs", "lenses"]}.
{erl_opts, [debug_info, beam_debug_info]}.
`);

const mocha = {
	ui: 'tdd',
	timeout: 120000,
};

export default defineConfig([
	{
		label: 'buck2',
		files: 'client/out/test/*.test.js',
		workspaceFolder: endToEnd,
		launchArgs,
		mocha,
	},
	{
		label: 'buck2-codegen',
		files: 'client/out/test/codegen/*.test.js',
		workspaceFolder: join(endToEnd, '../codegen_test'),
		launchArgs,
		mocha,
	},
	{
		label: 'rebar3',
		files: ['client/out/test/rebar3/*.test.js', 'client/out/test/definition.test.js'],
		workspaceFolder: rebar3Workspace,
		launchArgs,
		mocha,
	},
]);
