#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Refresh expect-test snapshots for every ELP crate by running each
# `:<name>-update-expect` target (see //whatsapp/elp/crates:defs.bzl). The list
# is discovered dynamically, so crates that gain snapshot tests later are picked
# up automatically.
#
# Usage:
#   buck2 run //whatsapp/elp/scripts:update-expect-all
#   buck2 run //whatsapp/elp/scripts:update-expect-all -- <test_filter>
#
# An optional <test_filter> is forwarded to every crate; crates without a
# matching test simply run nothing.

set -euo pipefail

mapfile -t targets < <(
    buck2 uquery 'fbcode//whatsapp/elp/crates/...' | grep -- '-update-expect$'
)

if [ "${#targets[@]}" -eq 0 ]; then
    echo "No :*-update-expect targets found." >&2
    exit 1
fi

for target in "${targets[@]}"; do
    echo ">>> ${target}"
    buck2 run "${target}" -- "$@"
done
