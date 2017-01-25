#!/bin/sh

xrdb -merge .Xresources
xsetroot -solid black
xsetroot -cursor_name left_ptr

/etc/udev/rules.d/monitor-hotplug.sh

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
parcellite &
workrave &
redshift-gtk &

setxkbmap custom
xcape -t 200 -e 'Hyper_L=space'

exec xmonad
