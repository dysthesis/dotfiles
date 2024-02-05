#!/usr/bin/env bash
#
# Original by @adi1090x modified by @sdushantha
#

dir="$HOME/.config/rofi/"
rofi_command="rofi -theme $dir/themes/powermenu.rasi"

# Options
shutdown="⏻"
reboot="󰜉"
lock=""
suspend=""
logout="󰗼"

# Variable passed to rofi
options="$lock\n$shutdown\n$reboot\n$logout"

chosen="$(echo -e "$options" | $rofi_command -dmenu -selected-row 0)"
case $chosen in
$shutdown)
    poweroff
  ;;
$reboot)
    reboot
  ;;
$lock)
  betterlockscreen -l -u /home/apollyon/Downloads/Wallpapers/wallhaven-6dwmmw.png
  ;;
$logout)
    i3-msg exit
  ;;
esac
