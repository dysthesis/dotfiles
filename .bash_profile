#
# ~/.bash_profile
#

export ZK_NOTEBOOK_DIR=~/Documents/Notes
export VDIRSYNCER_CONFIG=/home/demiurge/.config/vdirsyncer/config
export EDITOR=nvim
export PATH=~/.local/share/flatpak/exports/bin:$PATH
export TERM=screen-256color
export ELECTRON_OZONE_PLATFORM_HINT=wayland
export SXHKD_SHELL sh

if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
	exec startx
fi

[[ -f ~/.bashrc ]] && . ~/.bashrc
