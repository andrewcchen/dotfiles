#!/bin/bash

xrdb -merge .Xresources
xsetroot -solid black
xsetroot -cursor_name left_ptr
setxkbmap -option compose:menu
setxkbmap -option caps:escape

export _JAVA_AWT_WM_NONREPARENTING=1

export XDG_CURRENT_DESKTOP=KDE
export KDE_SESSION_VERSION=5

export STEAM_RUNTIME=0

export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

xscreensaver -no-splash &

nm-applet &
ibus-daemon -dx &
workrave &

exec xmonad
