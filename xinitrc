#!/bin/bash

xrdb -merge .Xresources
xsetroot -solid black
xsetroot -cursor_name left_ptr

export _JAVA_AWT_WM_NONREPARENTING=1

export XDG_CURRENT_DESKTOP=KDE
export KDE_SESSION_VERSION=5

export STEAM_RUNTIME=0

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
fcitx &

xscreensaver -no-splash &

nm-applet &
workrave &

(sleep 5; xkbcomp -I$HOME/.xkb $HOME/.xkb/keymap.xkb $DISPLAY) &

exec xmonad
