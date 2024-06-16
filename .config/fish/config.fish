if status is-interactive
    starship init fish | source
    zoxide init fish --cmd cd | source
    set -x GPG_TTY (tty)
    set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
    gpgconf --launch gpg-agent
    khal list now
    task next
end

zoxide init fish | source

# Neovim
alias vim=nvim
alias v=nvim

# Doom emacs
alias doom='~/.config/emacs/bin/doom'

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

# yt-fzf
alias subs='ytfzf -t -c SI --sort'

# automatically clone and set up mirror
function gclone -d "Clone a repo name and set up both mirrors to GitHub and Codeberg"
    set repo $argv
    set username 'dysthesis'
    set forges 'git@github.com' 'git@codeberg.org'

    echo "Cloning repository $repo"
    echo "Getting from initial repository at $forges[1]:$username/$repo.git"
    echo "" 

    git clone $forges[1]:$username/$repo.git
    cd $repo

    echo "Removing the 'origin' remote"
    git remote rm origin
    echo ""

    echo "Forges to set up:"
    for f in $forges
        echo "- $f"
    end

    echo ""
    
    for f in $forges
        echo "Setting up forge $f"
        set forge_url $f:$username/$repo.git
        set forge_name $(echo "$f" | sed 's/.*@\(.*\)\..*/\1/')
        echo "Adding remote $forge_name with URL $forge_url"
        echo "" 
        git remote add $forge_name $forge_url
    end
    # sync
    gpull
    gpush
    cd ..
end

function gpull -d "Pull from all mirrors"
    set remotes (git remote)
    set -l current_branch (eval /usr/bin/git symbolic-ref --short HEAD)
    for remote in $remotes
        echo -e "\nPulling from $remote/$current_branch..."
        git pull $remote $current_branch
    end
end

function gpush -d "Push to all mirrors"
    set remotes (git remote)
    set -l current_branch (eval /usr/bin/git symbolic-ref --short HEAD)
    for remote in $remotes
        echo -e "\nPushing to remote $remote/$current_branch"
        git push $remote $current_branch
    end
end

function dotpull
    set -l git_cmd 'git --git-dir="$HOME/.dotfiles/" --work-tree="$HOME"'
    set -l remotes (eval $git_cmd remote)
    set -l current_branch (eval $git_cmd symbolic-ref --short HEAD)
    for remote in $remotes
        echo -e "\nPulling from $remote..."
        eval $git_cmd pull $remote $current_branch
    end
end

function dotpush
    set -l git_cmd 'git --git-dir="$HOME/.dotfiles/" --work-tree="$HOME"'
    set -l remotes (eval $git_cmd remote)
    set -l current_branch (eval $git_cmd symbolic-ref --short HEAD)
    for remote in $remotes
        echo -e "\nPushing to $remote..."
        eval $git_cmd push $remote $current_branch
    end
end


# Variables
export VDIRSYNCER_CONFIG=$HOME/.config/vdirsyncer/config
export ZK_NOTEBOOK_DIR=~/Documents/Notes
