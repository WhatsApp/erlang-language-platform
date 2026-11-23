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

const testProject = join(import.meta.dirname, '../../test/test_projects/end_to_end');
const apps = ['assist_examples', 'definitions', 'hover'];

// `end_to_end` is configured as a Buck project. The copy lists its apps
// instead, so that ELP loads them as a single project without Buck.
const workspaceFolder = mkdtempSync(join(tmpdir(), 'elp-vscode-test-'));
cpSync(testProject, workspaceFolder, { recursive: true });
writeFileSync(join(workspaceFolder, '.elp.toml'), `[build_info]\napps = ${JSON.stringify(apps)}\n`);

export default defineConfig({
	files: 'client/out/test/**/*.test.js',
	workspaceFolder,
	launchArgs: ['--disable-extensions'],
	mocha: {
		ui: 'tdd',
		timeout: 120000,
	},
});
