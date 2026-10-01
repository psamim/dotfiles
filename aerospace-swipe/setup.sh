#!/usr/bin/env bash

set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
SRC="$HOME/.local/share/aerospace-swipe"
REV=7e06fe41e0deff786a643bf4d56963104c459c12

launchctl unload "$HOME/Library/LaunchAgents/com.acsandmann.swipe.plist" 2>/dev/null || true

rm -rf "$SRC"
git clone https://github.com/acsandmann/aerospace-swipe "$SRC"
git -C "$SRC" checkout "$REV"
git -C "$SRC" apply "$DIR/mouse.patch"
make -C "$SRC" install

mkdir -p "$HOME/.config/aerospace-swipe"
ln -sf "$DIR/config.json" "$HOME/.config/aerospace-swipe/config.json"
