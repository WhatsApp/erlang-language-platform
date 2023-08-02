#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

CODE_TESTS_PATH="$(pwd)/client/out/test"
export CODE_TESTS_PATH

CODE_TESTS_WORKSPACE="$(pwd)/client/testFixture"
export CODE_TESTS_WORKSPACE

node "$(pwd)/client/out/test/runTest"
