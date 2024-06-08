if status is-interactive
    starship init fish | source
    set -x GPG_TTY (tty)
    set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
    gpgconf --launch gpg-agent
    khal calendar
    task next limit:5
end

zoxide init fish | source

# Neovim
alias vim=nvim
alias v=nvim

# Doom emacs
alias doom='~/.config/emacs/bin/doom'

# Zoxide
alias cd=z

# Bat (better cat)
alias cat=bat

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

function gpull
    set remotes (git remote)
    for remote in $remotes
        echo "Pulling from $remote..."
        git pull $remote main
    end
end

function gpush
    set remotes (git remote)
    for remote in $remotes
        echo "Pushing to remote $remote"
        git push $remote main
    end
end
function dotpull
    set -l git_cmd 'git --git-dir="$HOME/.dotfiles/" --work-tree="$HOME"'
    set -l remotes (eval $git_cmd remote)
    set -l current_branch (eval $git_cmd symbolic-ref --short HEAD)
    for remote in $remotes
        echo "Pulling from $remote..."
        eval $git_cmd pull $remote $current_branch
    end
end
function dotpush
    set -l git_cmd 'git --git-dir="$HOME/.dotfiles/" --work-tree="$HOME"'
    set -l remotes (eval $git_cmd remote)
    set -l current_branch (eval $git_cmd symbolic-ref --short HEAD)
    for remote in $remotes
        echo "Pushing from $remote..."
        eval $git_cmd push $remote $current_branch
    end
end


# Variables
export VDIRSYNCER_CONFIG=$HOME/.config/vdirsyncer/config
export ZK_NOTEBOOK_DIR=~/Documents/Notes
