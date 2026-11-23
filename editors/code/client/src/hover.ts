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
import {
	Command,
	Hover,
	HoverParams,
	HoverRequest,
	LanguageClient,
	RequestType,
} from 'vscode-languageclient/node';

type CommandLink = Command & { tooltip?: string };

interface CommandLinkGroup {
	title?: string;
	commands: CommandLink[];
}

// Links run only commands with plain ids, such as `elp.openUri`.
const COMMAND_ID = /^[\w.-]+$/;

// ELP extends the hover response with actions, rendered as command links.
const hoverRequest = new RequestType<HoverParams, (Hover & { actions?: CommandLinkGroup[] }) | null, void>(
	HoverRequest.method);

export async function provideHover(
	client: LanguageClient,
	document: vscode.TextDocument,
	position: vscode.Position,
	token: vscode.CancellationToken,
): Promise<vscode.Hover | null | undefined> {
	try {
		const result = await client.sendRequest(
			hoverRequest, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token);
		if (!result) {
			return null;
		}
		const hover = client.protocol2CodeConverter.asHover(result);
		if (result.actions?.length) {
			hover.contents.push(renderActions(result.actions));
		}
		return hover;
	} catch (error) {
		return client.handleFailedRequest(HoverRequest.type, token, error, null);
	}
}

export function renderActions(groups: CommandLinkGroup[]): vscode.MarkdownString {
	const valid = groups
		.map(group => ({ ...group, commands: group.commands.filter(link => COMMAND_ID.test(link.command)) }))
		.filter(group => group.commands.length > 0);
	const markdown = new vscode.MarkdownString(valid
		.map(group => (group.title ? `${escapeMarkdown(group.title)} ` : '') + group.commands.map(renderCommand).join(' | '))
		.join('\n\n---\n\n'));
	markdown.isTrusted = { enabledCommands: valid.flatMap(group => group.commands.map(link => link.command)) };
	return markdown;
}

// Titles, tooltips and arguments can contain text from the source, which must
// not end the link early or add links of its own.
function renderCommand(link: CommandLink): string {
	const args = encodeURIComponent(JSON.stringify(link.arguments ?? []))
		.replace(/[!'()*~]/g, char => `%${char.charCodeAt(0).toString(16).toUpperCase()}`);
	const tooltip = link.tooltip ? ` "${escapeMarkdown(link.tooltip.replace(/\s+/g, ' '))}"` : '';
	return `[${escapeMarkdown(link.title)}](command:${link.command}?${args}${tooltip})`;
}

function escapeMarkdown(text: string): string {
	return text.replace(/[\\`*_{}[\]()#+\-.!|<>"']/g, '\\$&');
}
