#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

set -euo pipefail

# Directory for stub files
STUB_DIR="fb/components"

# List of files to touch for OSS build
FILES_TO_TOUCH=(
    "_W0014.md"
)

# Function to display usage
usage() {
    echo "Usage: $0 {generate|cleanup}"
    echo "  generate - Create stub files and run docusaurus build"
    echo "  cleanup  - Remove stub files"
    exit 1
}

# Function to generate stub files
generate_stubs() {
    echo "Generating OSS stub files..."
    # Create stub directory if it doesn't exist
    if [[ ! -d "$STUB_DIR" ]]; then
        echo "Creating stub directory: $STUB_DIR"
        mkdir -p "$STUB_DIR"
    fi

    for file in "${FILES_TO_TOUCH[@]}"; do
        echo "Creating stub: $STUB_DIR/$file"
        touch "$STUB_DIR/$file"
    done
}

# Function to cleanup stub files
cleanup_stubs() {
    echo "Cleaning up stub files..."
    for file in "${FILES_TO_TOUCH[@]}"; do
        if [[ -f "$STUB_DIR/$file" ]]; then
            echo "Removing stub: $STUB_DIR/$file"
            rm -f "$STUB_DIR/$file"
        fi
    done

    # Remove stub directory if it exists and is empty
    if [[ -d "$STUB_DIR" ]]; then
        if [[ -z "$(ls -A "$STUB_DIR" 2>/dev/null)" ]]; then
            echo "Removing empty stub directory: $STUB_DIR"
            rmdir "$STUB_DIR"
        else
            echo "Stub directory $STUB_DIR is not empty, leaving it in place"
        fi
    fi
    echo "Cleanup complete!"
}

# Main logic
case "${1:-}" in
    generate)
        generate_stubs
        ;;
    cleanup)
        cleanup_stubs
        ;;
    *)
        usage
        ;;
esac
