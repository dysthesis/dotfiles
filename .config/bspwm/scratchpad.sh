#!/bin/sh
hide_all_except_current(){
    for id in $(bspc query -d focused -N -n .floating.sticky.!hidden)
    do
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
            st -g 180x30 -c notes -e sh -c 'tmux attach-session -t Notes || tmux new-session -s Notes -c ~/Documents/Notes/'
            ;;
        term)
            st -g 180x30 -c term
            ;;
        btop)
            st -g 180x30 -c btop -e btop
            ;;
        music)
            st -g 180x30 -c music -e ncmpcpp
            ;;
        task)
            st -g 180x30 -c task -e taskwarrior-tui
            ;;
        files)
            st -g 180x30 -c files -e yazi
            ;;
        calendar)
            st -g 180x30 -c calendar -e ikhal
            ;;
        Signal)
            signal-desktop
            ;;
    esac
fi
