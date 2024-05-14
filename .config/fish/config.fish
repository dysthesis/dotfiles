if status is-interactive
    starship init fish | source
    set -x GPG_TTY (tty)
    set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
    gpgconf --launch gpg-agent
    fastfetch
end

zoxide init fish | source

alias sudo=doas
alias cd=z
alias ls='eza --icons'
alias ll='eza --icons -l'
alias la='eza --icons -la'
