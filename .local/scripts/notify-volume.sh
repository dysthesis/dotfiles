#!/bin/sh
notify-send -a "Volume" -r 91190 -t 800 "Current volume" "$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{print int($2 * 100)}')%"
