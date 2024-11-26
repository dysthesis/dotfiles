#!/bin/sh
hide_all_except_current() {
    for id in $(bspc query -d focused -N -n .floating.sticky.!hidden); do
        bspc query --node $id -T | grep -qv $name && bspc node $id --flag hidden=on
    done
}
if xdotool search --onlyvisible --class "$1" >/dev/null 2>&1; then
    xdotool search --class "$1" windowunmap
elif xdotool search --class "$1" >/dev/null 2>&1; then
    xdotool search --class "$1" windowmap
else
    case "$1" in
    notes)
        wezterm start --class=notes -e sh -c 'tmux attach-session -t Notes || tmux new-session -s Notes -c ~/Documents/Notes/'
        ;;
    term)
        wezterm start --class=term
        ;;
    btop)
        wezterm start --class=btop -- btop
        ;;
    files)
        wezterm start --class=files -- yazi
        ;;
    Signal)
        signal-desktop
        ;;
    esac
fi
