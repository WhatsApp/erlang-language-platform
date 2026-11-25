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
import { LanguageClient, LanguageClientOptions } from 'vscode-languageclient/node';
// @fb-only: import { createMetaTelemetry } from './meta/telemetry';

export type TelemetryExtras = Record<string, unknown>;

export interface Telemetry {
    logEvent(eventName: string, extras?: TelemetryExtras, isHumanTriggered?: boolean): void;
    // The options of a language client cannot change after it is created, so this runs before.
    instrumentClientOptions(
        options: LanguageClientOptions,
        client: () => LanguageClient,
    ): LanguageClientOptions;
    // Forwards the `telemetry/event` notifications sent by the language server.
    trackServerEvents(client: LanguageClient): void;
}

const noTelemetry: Telemetry = {
    logEvent: () => undefined,
    instrumentClientOptions: options => options,
    trackServerEvents: () => undefined,
};

// The open-source build of the extension collects no telemetry.
export function createTelemetry(context: vscode.ExtensionContext): Telemetry {
    // @fb-only: return createMetaTelemetry(context) ?? noTelemetry;
    return noTelemetry; // @oss-only
}
