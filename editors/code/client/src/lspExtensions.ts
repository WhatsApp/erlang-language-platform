/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import * as vscode from 'vscode';
import { CodeAction, CodeActionParams } from "vscode-languageclient";

// Resolve Code Actions

export enum UserInputType {
    Atom = 'Atom',
    Variable = 'Variable',
    String = 'String'
}

export interface UserInput {
    input_type: UserInputType;
    prompt: string;
    value: string;
};

export function isValid(input_type: UserInputType, value: string) {
    switch (input_type) {
        case UserInputType.Atom:
            return (/^[a-z][_a-zA-Z0-9@]*$/.test(value) || /^['][_a-zA-Z0-9@]*[']$/.test(value))
        case UserInputType.Variable:
            return /^[A-Z][_a-zA-Z0-9@]*$/.test(value);
        case UserInputType.String:
            return true
    } 
}

export function defaultValue(input_type: UserInputType) {
    switch (input_type) {
        case UserInputType.Atom:
            return 'new_name';
        case UserInputType.Variable:
            return 'NewName';
        case UserInputType.String:
            return 'New String'
    }
}

export interface CodeActionData {
    id: string;
    codeActionParams: CodeActionParams;
    userInput: UserInput;
}

// The @types/vscode do not include the optional 'data' field, which is part of LSP 3.17
export interface CodeAction317 extends CodeAction {
    data?: CodeActionData;
}

export async function resolveCodeActionData(data: CodeActionData): Promise<CodeActionData> {
    if ('userInput' in data) {
        data.userInput.value = await vscode.window.showInputBox({
            value: data.userInput.value ?? defaultValue(data.userInput.input_type),
            prompt: data.userInput.prompt ?? '',
            validateInput: value => {
                if (isValid(data.userInput.input_type, value)) {
                    return null
                }
                return `Invalid name for Erlang type '${data.userInput.input_type}'`;
            },
        });
    }
    return data
}
