#!/bin/bash
set -e

# Usage:
#   rsync_parallel.sh [--parallel=N] [rsync args...]
# 
# Options:
#   --parallel=N	Use N parallel processes for transfer. Defaults to 10.
#
# Notes:
#   * Requires GNU Parallel
#   * Use with ssh-keys. Lots of password prompts will get very annoying.
#   * Does an itemize-changes first, then chunks the resulting file list and launches N parallel
#     rsyncs to transfer a chunk each.
#   * be a little careful with the options you pass through to rsync. Normal ones will work, you 
#     might want to test weird options upfront.
#

if [[ "$1" == --parallel=* ]]; then
	PARALLEL="${1##*=}"
	shift
else
	PARALLEL=10
fi
echo "Using up to $PARALLEL processes for transfer..."

TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

echo "Figuring out file list..."
# sorted by size (descending)
rsync $@ --out-format="%l %n" --no-v --dry-run | sort -n -r > $TMPDIR/files.all

# check for nothing-to-do
TOTAL_FILES=$(cat $TMPDIR/files.all | wc -l)
if [ "$TOTAL_FILES" -eq "0" ]; then
	echo "Nothing to transfer :)"
	exit 0
fi

function array_min {
	# return the (index, value) of the minimum element in the array
	IC=($(tr ' ' '\n' <<<$@ | cat -n | sort -k2,2nr | tail -n1))
	echo $((${IC[0]} - 1)) ${IC[1]}
}

echo "Calculating chunks..."
# declare chunk-size array
for ((I = 0 ; I < PARALLEL ; I++ )); do
	CHUNKS["$I"]=0 
done

# add each file to the emptiest chunk, so they're as balanced by size as possible
while read FSIZE FPATH; do
	MIN=($(array_min ${CHUNKS[@]}))
	CHUNKS["${MIN[0]}"]=$((${CHUNKS["${MIN[0]}"]} + $FSIZE))
	echo $FPATH >> $TMPDIR/chunk.${MIN[0]}
done < $TMPDIR/files.all

find "$TMPDIR" -type f -name "chunk.*" -printf "\n*** %p ***\n" -exec cat {} \;

echo "Starting transfers..."
find "$TMPDIR" -type f -name "chunk.*" | parallel -j $PARALLEL -t --verbose --progress rsync --files-from={} $@
