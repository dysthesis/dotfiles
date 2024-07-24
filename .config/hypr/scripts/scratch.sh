#!/usr/bin/env dash

windows_in(){
hyprctl clients -j | jq ".[] | select(.workspace.name == \"special:$1\" )"
}

toggle_scratchpad(){
    workspace_name="$1"
    cmd="$2"

    windows=$( windows_in "$workspace_name" )
    # If not on latest , check the edit history of this post
    if [ -z "$windows" ];then
        hyprctl dispatch "exec [workspace special:$workspace_name] $cmd"
        else
        hyprctl dispatch togglespecialworkspace "$workspace_name"
    fi
}

if [ "$1" = "term" ]; then
    toggle_scratchpad $1 "kitty"
elif [ "$1" = "fm" ]; then
    toggle_scratchpad $1 "kitty -- yazi"
elif [ "$1" = "signal" ]; then
    toggle_scratchpad $1 "signal-desktop"
elif [ "$1" = "task" ]; then
    toggle_scratchpad $1 "kitty -- taskwarrior-tui"
elif [ "$1" = "btop" ]; then
    toggle_scratchpad $1 "kitty -- btop"
fi
