#!/bin/sh

# case "$1" in
#
#     term)
#         entry="st -g 180x30 -c term"
#         ;;
#     notes)
#         entry="st -g 180x30 -c notes -e sh -c tmux attach-session -t notes || tmux new-session -s notes -c ~/Documents/Notes/"
#         ;;
#     task)
#         entry="st -g 180x30 -c task -e taskwarrior-tui"
#         ;;
#     files)
#         entry="st -g 180x30 -c files -e yazi"
#         ;;
# esac



if xdotool search --onlyvisible --class "$1" >/dev/null 2>&1; then
    xdotool search --class "$1" windowunmap
elif xdotool search --class "$1" >/dev/null 2>&1; then
    xdotool search --class "$1" windowmap
else
    case "$1" in
        notes)
            st -g 180x30 -c notes -e sh -c tmux attach-session -t notes || tmux new-session -s notes -c ~/Documents/Notes/
            ;;
        term)
            st -g 180x30 -c term
            ;;
        task)
            st -g 180x30 -c task -e taskwarrior-tui
            ;;
        files)
            st -g 180x30 -c files -e yazi
            ;;
    esac
fi
