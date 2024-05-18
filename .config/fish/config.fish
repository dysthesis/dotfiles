if status is-interactive
    starship init fish | source
    set -x GPG_TTY (tty)
    set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
    gpgconf --launch gpg-agent
    task calendar
    task summary
    task list
end

zoxide init fish | source

alias sudo=doas

# Neovim
alias vim=nvim
alias v=nvim

# Doom emacs
alias doom='~/.config/emacs/bin/doom'

# Zoxide
alias cd=z

# Eza (better ls)
alias ls='eza --icons'
alias ll='eza --icons -l'
alias la='eza --icons -la'

# Taskwarrior
alias t=task
alias tn='clear;task next'
alias ta='task add'
alias tan='task add scheduled:today'
alias tat='task add scheduled:tomorrow until:sch+14d'
alias tm='task modify'
