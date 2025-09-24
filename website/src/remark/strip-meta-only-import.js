/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// With Docusaurus 3, the build fails if we import a file that doesn't exist.
// Hence, we use a remark plugin to strip meta-only import statements.
// The plugin is enabled for external builds in the docusaurus.config.js file.
// For reference on how these plugins work, see:
// https://docusaurus.io/docs/markdown-features/plugins
// https://unifiedjs.com/learn/recipe/remove-node/

import {SKIP, visit} from 'unist-util-visit';

const plugin = (options) => {
  const transformer = async (ast) => {
    // Match on import statements
    visit(ast, 'mdxjsEsm', (node, index, parent) => {
        // Multiple imports can be part of the same node (one per line)
        if (node.value && node.value.includes('/fb/')) {
            const lines = node.value.split('\n');
            const nonFbLines = lines.filter(line => !line.includes('/fb/'));
            if (nonFbLines.length === 0) {
                // Remove the entire node if it only contains fb imports
               return [SKIP, index]
            }
            // Replace the node with the non-fb imports
            node.value = nonFbLines.join('\n');
        }
    });
  };
  return transformer;
};

export default plugin;
