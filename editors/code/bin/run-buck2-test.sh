#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Launches the Erlang test shell of a Buck2 Common Test suite, to be used from
# VS Code when debugging tests.
#
# The Debug lenses always pass the target and, for a single test, the test
# regex as positional parameters. Everything else uses named parameters.
#
# Important options:
#   --interactive=false runs all tests matching the regex and finishes
#   --node-setup-code=ERL_CODE executes ERL_CODE on the test node, before running any test
#   --node-setup-code-from-env=VAR does the same with the code in environment variable VAR
#   --mode=MODE builds the suite in Buck2 mode MODE, in which the shell also rebuilds changed modules
set -euo pipefail

main() {
    interactive="true"
    node_setup_code=""
    buck2_target=""
    test_regex=""
    mode=""

    while [[ $# -gt 0 ]]; do
        local ARG="$1"; shift
        local VAL=""
        case "$ARG" in
            --*=*)
                VAL="${ARG#*=}"
                ARG="${ARG%%=*}"
                ;;
            --*)
                if [[ $# -gt 0 ]]; then
                    VAL="$1"; shift
                fi
                ;;
        esac
        case "$ARG" in
            --interactive)
                [[ -n "$VAL" ]] || usage "$ARG needs an argument"
                [[ "$VAL" == "true" ]] || [[ "$VAL" == "false" ]] || usage "$ARG: invalid value"
                interactive="$VAL"
                ;;
            --node-setup-code)
                [[ -n "$VAL" ]] || usage "$ARG needs an argument"
                node_setup_code="$VAL"
                ;;
            --node-setup-code-from-env)
                [[ -n "$VAL" ]] || usage "$ARG needs an argument"
                [[ -n "${!VAL+x}" ]] || usage "$ARG: environment variable '$VAL' is not defined"
                node_setup_code="${!VAL}"
                ;;
            --mode)
                [[ -n "$VAL" ]] || usage "$ARG needs an argument"
                mode="$VAL"
                ;;
            *)
                if [[ -z "$buck2_target" ]]; then
                    buck2_target="$ARG"
                elif [[ -z "$test_regex" ]]; then
                    test_regex="$ARG"
                else
                    usage "Unexpected '$ARG'"
                fi
                ;;
        esac
    done

    [[ -n "$buck2_target" ]] || usage "BUCK2_TARGET missing"

    if [[ "$interactive" == "false" ]] && [[ -z "$test_regex" ]]; then
        test_regex=".*"
    fi

    cd "$(buck2 --client-metadata=id=elp_vscode root --kind=project)" || exit

    node="elp_debug_$$_$RANDOM@localhost"
    cookie="elp_debug_$RANDOM"

    if [[ -n "$node_setup_code" ]]; then
        preamble="test:ensure_initialized(),ok=erpc:call('$node', fun() -> $node_setup_code end)"
    else
        preamble='test:ensure_initialized()'
    fi
    [[ -z "$test_regex" ]] || preamble="$preamble,test:run(\"$test_regex\")"
    case "$interactive" in
        true) preamble="$preamble,test:info(),test:start_shell()";;
        false) preamble="$preamble,erlang:halt()" ;;
    esac

    node_config="{shortnames, '$node', '$cookie'}"

    set --                                                      \
      -test_cli_lib node_config "$node_config"                  \
      -test_cli_lib debugger_mode true                          \
      -test_cli_lib preamble "$(printf '~"%q."' "$preamble")"

    if [[ -n "$mode" ]]; then
        exec buck2 --client-metadata=id=elp_vscode run          \
          "$mode"                                               \
          "$buck2_target"                                       \
          --                                                    \
          "$@"                                                  \
          -buck2_shell_utils buck2_args "[~\"$mode\"]"
    fi
    exec buck2 --client-metadata=id=elp_vscode run "$buck2_target" -- "$@"
}

usage() {
    [[ $# -eq 0 ]] || echo >&2 "Error: $*"
    echo >&2 "Usage: ${0##*/} [--interactive true|false] [--node-setup-code ERL_CODE] [--node-setup-code-from-env VAR] [--mode MODE] BUCK2_TARGET [TEST_REGEX]"
    exit 1
}

main "$@"
