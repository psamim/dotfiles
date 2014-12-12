#! /bin/sh
layout=`setxkbmap -query | sed -n "s/^layout:\s\+//p"`
variant=`setxkbmap -query | sed -n "s/^variant:\s\+//p"`

[ "$variant" = "" ] && CURRENT="$layout" || CURRENT="$layout:$variant"

if [ "$#" -eq 0 ]; then
    echo $layout
    exit
fi

next=$1

while [ $# -gt 0 ]
do
    keymap=$1
    shift

    if [ "$keymap" = "$CURRENT" ]
    then
        if [ $# -gt 0 ]
        then
            next=$1
            shift
        fi
    fi
done

echo "$next" | grep ":" >/dev/null
if [ $? -eq 0 ]
then
    layout=`echo $next | cut -f1 -d:`
    variant=`echo $next | cut -f2 -d:`
else
    layout=$next
    variant=""
fi

setxkbmap "$layout" "$variant"

[ "$variant" = "" ] && CURRENT="$layout" || CURRENT="$layout:$variant"

echo $CURRENT
