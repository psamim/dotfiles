#!/bin/sh
# Toggle the current tmux window's panes between a horizontal
# arrangement (side-by-side) and a vertical arrangement (stacked).
layout=$(tmux display-message -p '#{window_layout}')
first_bracket=$(printf '%s' "$layout" | grep -o '[{[]' | head -1)

if [ "$first_bracket" = "{" ]; then
	tmux select-layout even-vertical
else
	tmux select-layout even-horizontal
fi
