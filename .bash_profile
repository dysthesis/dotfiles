#
# ~/.bash_profile
#

export QT_QPA_PLATFORMTHEME="qt5ct"

if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
	exec startx
fi

[[ -f ~/.bashrc ]] && . ~/.bashrc
