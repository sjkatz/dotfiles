#!/bin/bash

xrdb -merge .Xresources

trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 5 --height 17 --transparent true --alpha 0 --tint 0x000000 &

#gnome-screensaver 
#gnome-settings-daemon

# Fire up any apps
xscreensaver -no-splash &

#if [ -x /usr/bin/gnome-power-manager ] ; then
   #sleep 1
   #gnome-power-manager
#fi

if [ -x /usr/bin/nm-applet ] ; then
   (sleep 1; nm-applet --sm-disable &)
fi

eval $(gnome-keyring-daemon --start)
export GNOME_KEYRING_SOCKET
export GNOME_KEYRING_PID

#kmix --keepvisibility
feh --bg-scale /usr/share/backgrounds/The_Forbidden_City_by_Daniel_Mathis.jpg &

chromium-browser --proxy-auto-detect &

exec xmonad
