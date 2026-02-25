/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

import {themes as prismThemes} from 'prism-react-renderer';
import stripMetaOnlyImport from './src/remark/strip-meta-only-import.js';
import * as fs from 'fs';
import * as path from 'path';

const {fbContent} = require('docusaurus-plugin-internaldocs-fb/internal');

// Eqwalizer docs paths (relative to website directory)
const EQWALIZER_DOCS_PATH_INTERNAL = '../../eqwalizer/docs';
const EQWALIZER_DOCS_PATH_EXTERNAL = '../eqwalizer/docs';

// Check if eqwalizer docs exist
const eqwalizerDocsPath = fbContent({
  internal: path.resolve(__dirname, EQWALIZER_DOCS_PATH_INTERNAL),
  external: path.resolve(__dirname, EQWALIZER_DOCS_PATH_EXTERNAL),
});
const eqwalizerDocsExist = fs.existsSync(eqwalizerDocsPath);

// With JSDoc @type annotations, IDEs can provide config autocompletion
/** @type {import('@docusaurus/types').DocusaurusConfig} */
(module.exports = {
  title: 'ELP',
  tagline: 'The Erlang Language Platform',
  url: 'https://whatsapp.github.io',
  baseUrl: '/erlang-language-platform/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'throw',
  trailingSlash: true,
  favicon: 'img/elp_icon_color.svg',
  organizationName: 'whatsapp',
  projectName: 'erlang-language-platform',

  presets: [
    [
      'docusaurus-plugin-internaldocs-fb/docusaurus-preset',
      /** @type {import('docusaurus-plugin-internaldocs-fb').PresetOptions} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          remarkPlugins: fbContent({
            internal: [],
            external: [stripMetaOnlyImport]
          }),
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
        sitemap: {
        },
      }),
    ],
  ],

  plugins: [
    // Only include eqwalizer plugin if docs exist
    ...(eqwalizerDocsExist
      ? [
          [
            '@docusaurus/plugin-content-docs',
            {
              id: 'eqwalizer',
              path: fbContent({
                internal: EQWALIZER_DOCS_PATH_INTERNAL,
                external: EQWALIZER_DOCS_PATH_EXTERNAL,
              }),
              routeBasePath: 'docs/eqwalizer',
              sidebarPath: require.resolve('./sidebarsEqwalizer.js'),
              editUrl: undefined,
            },
          ],
        ]
      : []),
  ],

  markdown: ({
    // Use mdx for `.mdx` files and commonmark for `.md` files
    format: 'detect',
  }),

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        logo: {
          alt: 'ELP Logo',
          src: 'img/elp_logo_color.svg',
        },
        items: [
          {
            type: 'doc',
            docId: 'get-started/get-started',
            position: 'left',
            label: 'Get Started',
          },
          {
            type: 'doc',
            docId: 'feature-gallery',
            position: 'left',
            label: 'Feature Gallery',
          },
          {
            type: 'doc',
            docId: 'contributing/contributing',
            position: 'left',
            label: 'Contributing',
          },
          {
            type: 'doc',
            docId: 'erlang-error-index/erlang-error-index',
            position: 'left',
            label: 'Erlang Error Index',
          },
          ...(eqwalizerDocsExist
            ? [
                {
                  type: 'doc',
                  docsPluginId: 'eqwalizer',
                  docId: 'reference',
                  position: 'left',
                  label: 'eqWAlizer',
                },
              ]
            : []),
          {
            href: 'https://github.com/whatsapp/erlang-language-platform',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Docs',
            items: [
              {
                label: 'Get Started',
                to: '/docs/get-started',
              },
              {
                label: 'Architecture',
                to: '/docs/architecture',
              },
              {
                label: 'Erlang Error Index',
                to: '/docs/erlang-error-index',
              },
              ...(eqwalizerDocsExist
                ? [
                    {
                      label: 'eqWAlizer',
                      to: '/docs/eqwalizer/reference',
                    },
                  ]
                : []),
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'GitHub Issues',
                href: 'https://github.com/whatsapp/erlang-language-platform/issues',
              }
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'GitHub',
                href: 'https://github.com/whatsapp/erlang-language-platform',
              },
              {
                label: 'Contributing',
                href: 'https://github.com/WhatsApp/erlang-language-platform/blob/main/CONTRIBUTING.md',
              },
              {
                label: 'Code Of Conduct',
                href: 'https://github.com/WhatsApp/erlang-language-platform/blob/main/CODE_OF_CONDUCT.md',
              },
              {
                label: 'Terms of Use',
                href: 'https://opensource.fb.com/legal/terms',
              },
              {
                label: 'Privacy Policy',
                href: 'https://opensource.fb.com/legal/privacy',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Meta Platforms, Inc. Built with Docusaurus.`,
      },
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.dracula,
        additionalLanguages: ['erlang'],
      },
    }),
});
