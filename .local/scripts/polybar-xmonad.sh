#!/usr/bin/env bash
# From https://gitlab.com/dwt1/dotfiles/-/blob/master/.local/bin/polybar-xmonad
# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do :; done

polybar --reload
echo "Bars launched..."
