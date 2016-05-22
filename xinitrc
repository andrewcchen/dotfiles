#!/bin/bash

xrdb -merge .Xresources
xsetroot -solid black
xsetroot -cursor_name left_ptr
setxkbmap -option compose:menu
setxkbmap -option caps:escape

export XDG_CURRENT_DESKTOP=KDE
export KDE_SESSION_VERSION=5

xscreensaver -no-splash &

nm-applet &
ibus-daemon &
workrave &

exec xmonad
