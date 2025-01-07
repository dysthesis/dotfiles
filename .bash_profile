#
# ~/.bash_profile
#

export ZK_NOTEBOOK_DIR=~/Documents/Notes
export VDIRSYNCER_CONFIG=/home/demiurge/.config/vdirsyncer/config
export EDITOR=nvim
export PATH=~/.local/share/flatpak/exports/bin:$PATH
export ELECTRON_OZONE_PLATFORM_HINT=wayland
export SXHKD_SHELL sh

if [ "$TERM" != "tmux-256color" ] && [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
	exec ~/.config/dwl/start
fi

[[ -f ~/.bashrc ]] && . ~/.bashrc

if [ -e /home/demiurge/.nix-profile/etc/profile.d/nix.sh ]; then . /home/demiurge/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
. "$HOME/.cargo/env"
