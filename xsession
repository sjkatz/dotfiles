#!/bin/bash

# Link to resources
#bash .xrandr_run
xrdb -merge .Xresources

# Setup Trayer
trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 10 --height 17 --transparent false --alpha 0 --tint 0x000000 &

# Fire up any apps
xscreensaver -no-splash &
#gnome-screensaver &
gnome-settings-daemon &
#gnome-session 

# Run Power Manager
if [ -x /usr/bin/gnome-power-manager ] ; then
#   sleep 1
   gnome-power-manager &
fi

# Run Network Manager
if [ -x /usr/bin/nm-applet ] ; then
    (sleep 1; nm-applet --sm-disable &)
fi

if [ -x /usr/bin/gnome-sound-applet ] ; then
#   sleep 1
   gnome-sound-applet &
fi

# Handle Gnome KeyRing
eval $(gnome-keyring-daemon --start)
export GNOME_KEYRING_SOCKET
export GNOME_KEYRING_PID

# Set Background image
feh --bg-scale /usr/share/backgrounds/The_Forbidden_City_by_Daniel_Mathis.jpg &

# Run some start-up apps
chromium-browser --proxy-auto-detect &

# Run XMonad
exec xmonad
