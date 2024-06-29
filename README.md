These are dotfiles for my PC running Arch Linux, with the following details:

- **Window manager:** XMonad
- **Bar:** XMobar
- **Notification daemon:** dunst
- **Editor:** Neovim
- **Terminal emulator:** st-flexipatch

# Replicating

As per [this guide on the Arch wiki](https://wiki.archlinux.org/title/Dotfiles#Tracking_dotfiles_directly_with_Git), these dotfiles can be reinstated on a new system as so:

```bash
git clone --recurse-submodules --bare git@github.com:dysthesis/dotfiles.git ~/.dotfiles

# If on fish, the -s flag can be added to the following command to persist the alias
alias dotfiles='/usr/bin/git --git-dir="$HOME/.dotfiles/" --work-tree="$HOME"'

dotfiles checkout

dotfiles config --local status.showUntrackedFiles no
```
