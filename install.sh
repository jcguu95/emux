#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TARGET_DIR="$HOME/.local/bin"
BINARY_NAME="emux"

mkdir -p "$TARGET_DIR"
ln -sf "$SCRIPT_DIR/emux" "$TARGET_DIR/$BINARY_NAME"
