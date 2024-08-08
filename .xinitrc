#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/Xresources
sysmodmap=/etc/X11/xinit/Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then







    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then







    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# turn off Display Power Management Service (DPMS)
xset -dpms
setterm -blank 0 -powerdown 0

# turn off black Screensaver
xset s off

pipewire &
parcellite &
dunst &
mpd &
redshift &
syncthing -no-browser &
/usr/libexec/polkit-gnome-authentication-agent-1 &
redshift -l -33.9166485:151.2233364 &
flameshot &
gentoo-pipewire-launcher &
gnome-keyring-agent &
killall picom
picom -b &
xrandr --output DisplayPort-1 --mode 1920x1080 --rate 165 --primary --output DisplayPort-0 --left-of DisplayPort-1
gnome-keyring-daemon &
hsetroot -cover ~/.config/wallpaper.png &

dbus-run-session bspwm
