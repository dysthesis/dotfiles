if status is-interactive
    atuin init fish | source
    starship init fish | source
    enable_transience
    zoxide init fish --cmd cd | source
    set -x GPG_TTY (tty)
    set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
    gpgconf --launch gpg-agent
    khal list now tomorrow
    task next
end

alias sudo=doas
alias ':q'=exit
alias ping=gping
alias notes='tmux attach-session -t Notes || tmux new-session -s Notes -c ~/Documents/Notes/'

# UNSW stuff
alias cse="ssh z5437039@login.cse.unsw.edu.au"

zoxide init fish | source

# Make temporary directory
alias temp="cd $(mktemp -d)"

# Podman
alias docker=podman

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
alias subs='ytfzf -c SI --sort'
alias torsubs='torsocks ytfzf -c SI --sort'

alias yt='mpv --ytdl-format=bestvideo+bestaudio'

# automatically clone and set up mirror
function gclone -d "Clone a repo name and set up both mirrors to GitHub and Codeberg"
    set repo $argv
    set username 'dysthesis'
    set forges 'git@github.com' 'git@codeberg.org'

    echo "Cloning repository $repo" &&
    echo "Getting from initial repository at $forges[1]:$username/$repo.git" &&
    echo "" &&

    git clone $forges[1]:$username/$repo.git &&
    cd $repo &&

    echo "Removing the 'origin' remote" &&
    git remote rm origin &&
    echo "" &&

    echo "Forges to set up:" &&
    for f in $forges
        echo "- $f"
    end &&

    echo "" &&
    
    for f in $forges
        echo "Setting up forge $f"
        set forge_url $f:$username/$repo.git
        set forge_name $(echo "$f" | sed 's/.*@\(.*\)\..*/\1/')
        echo "Adding remote $forge_name with URL $forge_url"
        echo "" 
        git remote add $forge_name $forge_url
    end &&
    # sync
    gpull &&
    gpush &&
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


# Functions needed for !! and !$
function __history_previous_command
  switch (commandline -t)
  case "!"
    commandline -t $history[1]; commandline -f repaint
  case "*"
    commandline -i !
  end
end

function __history_previous_command_arguments
  switch (commandline -t)
  case "!"
    commandline -t ""
    commandline -f history-token-search-backward
  case "*"
    commandline -i '$'
  end
end

# The bindings for !! and !$
if [ "$fish_key_bindings" = "fish_vi_key_bindings" ];
  bind -Minsert ! __history_previous_command
  bind -Minsert '$' __history_previous_command_arguments
else
  bind ! __history_previous_command
  bind '$' __history_previous_command_arguments
end

# Function for creating a backup file
# ex: backup file.txt
# result: copies file as file.txt.bak
function backup --argument filename
    cp $filename $filename.bak
end
# Variables
export VDIRSYNCER_CONFIG=$HOME/.config/vdirsyncer/config
export ZK_NOTEBOOK_DIR=~/Documents/Notes
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$HOME/.local/share/flatpak/exports/bin:$HOME/.local/bin:$PATH"
export EDITOR=nvim
