#
# ~/.bash_profile
#

export QT_QPA_PLATFORMTHEME="qt5ct"
export ZK_NOTEBOOK_DIR=~/Documents/Notes
export VDIRSYNCER_CONFIG=/home/demiurge/.config/vdirsyncer/config
export EDITOR=nvim
export PATH=~/.local/share/flatpak/exports/bin:$PATH

if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
	exec startx
fi

[[ -f ~/.bashrc ]] && . ~/.bashrc
