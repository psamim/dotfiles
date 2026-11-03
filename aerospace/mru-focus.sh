#!/bin/bash
# Usage:
#   mru-focus.sh record   called by on-focus-changed
#   mru-focus.sh next     focus next window in focus history
#   mru-focus.sh prev     focus previous window in focus history
# Presses within TIMEOUT_MS continue the same cycle.
set -u
TIMEOUT_MS=1000

dir="${XDG_CACHE_HOME:-$HOME/.cache}/aerospace-mru"
mkdir -p "$dir"
mru="$dir/mru"           # window ids, most recent first
snapshot="$dir/snapshot" # window order frozen at the start of a cycle
state="$dir/state"       # lines: last press ms, index, workspace
lock="$dir/lock"
touch "$mru" "$snapshot" "$state"

now_ms() { perl -MTime::HiRes=time -e 'printf "%d\n", time * 1000'; }

# aerospace runs callbacks without waiting, so calls can overlap.
lock() {
  local i
  for i in $(seq 50); do
    mkdir "$lock" 2>/dev/null && { trap 'rmdir "$lock"' EXIT; return; }
    sleep 0.02
  done
  # Lock holder probably crashed. Take the lock over.
  trap 'rmdir "$lock"' EXIT
}

cmd="${1:-}"

if [ "$cmd" = record ]; then
  id="$(aerospace list-windows --focused --format '%{window-id}' 2>/dev/null)"
  [ -z "$id" ] && exit 0
  lock
  { echo "$id"; grep -vx "$id" "$mru"; } | head -100 > "$mru.tmp"
  mv "$mru.tmp" "$mru"
  exit 0
fi

[ "$cmd" = next ] || [ "$cmd" = prev ] || { echo "usage: $0 record|next|prev" >&2; exit 1; }

workspace="$(aerospace list-workspaces --focused)"
windows="$(aerospace list-windows --workspace focused --format '%{window-id}')"
[ -z "$windows" ] && exit 0
lock

{ read -r ts; read -r idx; read -r last_workspace; } < "$state"
ts=${ts:-0} idx=${idx:-0} last_workspace=${last_workspace:-}
now=$(now_ms)

if [ $((now - ts)) -ge "$TIMEOUT_MS" ] || [ "$workspace" != "$last_workspace" ]; then
  # Windows in history order, then windows not in history yet.
  { grep -Fx -f <(echo "$windows") "$mru"; echo "$windows"; } | awk '!seen[$0]++' > "$snapshot"
  idx=0
fi

# Drop windows closed during the cycle.
list="$(grep -Fx -f <(echo "$windows") "$snapshot")"
count=$(echo "$list" | grep -c .)
[ "$count" -lt 2 ] && exit 0

if [ "$cmd" = next ]; then idx=$(((idx + 1) % count)); else idx=$(((idx - 1 + count) % count)); fi
target="$(echo "$list" | sed -n "$((idx + 1))p")"

printf '%s\n%s\n%s\n' "$now" "$idx" "$workspace" > "$state"
aerospace focus --window-id "$target"
