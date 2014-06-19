#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Dbus instance
eval $(dbus-launch --sh-syntax --exit-with-session)

# Keyboard layout
setxkbmap -layout us,ir
setxkbmap -option 'grp:alt_shift_toggle'
setxkbmap -option caps:escape

# Run Emacs if not already running
if ps aux | grep "emacs" > /dev/null
then
    echo "Emacs Running"
else
    emacs --daemon
fi

# Set background
[ -e ~/.fehbg ] && source ~/.fehbg

# Hide mouse when not used
unclutter &

#enable dunst for notify-send
dunst &

# Compton window compositor
/usr/bin/compton -b --config ~/.compton &

xrdb -merge ~/.Xresources
exec i3
