#! /bin/sh
# Keyboard layout

# Also have this under /etc/udev/rules.d/60-usb-keyboard.rules
# ACTION=="add", ATTRS{phys}=="usb-0000:00:14.0-3/input0", RUN="/home/samim/.bin/keyboard_configure.sh" 

export DISPLAY=:0
export XAUTHORITY=/home/samim/.Xauthority 
setxkbmap -layout us,ir -option 'grp:shift_toggle'
setxkbmap -option caps:ctrl_modifier
