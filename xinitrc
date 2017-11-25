#!/bin/sh

source $HOME/.profile

xrdb -merge .Xresources
xsetroot -solid black
xsetroot -cursor_name left_ptr

/etc/udev/rules.d/monitor-hotplug.sh

export _JAVA_AWT_WM_NONREPARENTING=1

export XDG_CURRENT_DESKTOP=KDE
export KDE_SESSION_VERSION=5

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
fcitx &

xscreensaver -no-splash &

dunst &
nm-applet &
redshift-gtk &
kglobalaccel &
kmix --keepvisibility &

setxkbmap custom
xcape -t 200 -e 'Hyper_L=space'

trayer --edge top --align left --expand true --distance 1266 --distancefrom left --widthtype pixel --width 100 --height 19 --transparent true --alpha 0 --tint  0x00000000 --SetDockType true --SetPartialStrut true &

exec xmonad
