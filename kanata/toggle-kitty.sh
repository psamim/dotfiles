#!/bin/bash
/opt/homebrew/bin/kitten quick-access-terminal
sleep 0.2
osascript -e 'tell application "System Events" to tell process "kitty-quick-access" to set frontmost to true'
