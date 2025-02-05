if status is-interactive
    atuin init fish | source
    starship init fish | source
    enable_transience
    zoxide init fish --cmd cd | source
    #source "$GHOSTTY_RESOURCES_DIR"/shell-integration/fish/vendor_conf.d/ghostty-shell-integration.fish

    # set -x GPG_TTY (tty)
    # set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
    # gpgconf --launch gpg-agent
    # khal list now tomorrow
    # task next
end

alias sudo=doas
# Git stuff
alias gc="git commit -S -am"

alias ':q'=exit
alias notes='tmux attach-session -t Notes || tmux new-session -s Notes -c ~/Documents/Notes/'

# UNSW stuff
alias cse="ssh z5437039@login.cse.unsw.edu.au"

# Make temporary directory
alias temp="cd $(mktemp -d)"

# Podman
alias docker=podman

# Neovim
alias vim=nvim
alias v=nvim

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

alias torsubs='torsocks ytfzf -c SI --sort'
alias subs='ytfzf -t -T kitty -c S --sort'

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
# export PATH="$HOME/.cargo/bin:$HOME/.cabal/bin:$HOME/.ghcup/bin:$HOME/.local/share/flatpak/exports/bin:$HOME/.local/bin:$PATH"
export EDITOR=nvim

# ____   _________________________   ____________________________________
# 7  7   7  _  77     77  7  77  7   7  7  77     77      77     77  _  7
# |  |   |  _  ||  ___!|   __!|  |   |  |  ||  ___!!__  __!|  ___!|    _|
# |  !___|  7  ||  7___|     ||  !___|  |  |!__   7  7  7  |  __|_|  _ \
# |     7|  |  ||     7|  7  ||     7|  !  |7     |  |  |  |     7|  7  |
# !_____!!__!__!!_____!!__!__!!_____!!_____!!_____!  !__!  !_____!!__!__!
#
#  Name: lackluster.nvim
#  License: MIT
#  Maintainer: Duncan Marsh (slugbyte@slugbyte.com)
#  Repository: https://github.com/slugbyte/lackluster.nvim

# copy this code in to your fish config
set -gx fish_color_end 7a7a7a
set -gx fish_color_error ffaa88
set -gx fish_color_quote 708090
set -gx fish_color_param aaaaaa
set -gx fish_color_option aaaaaa
set -gx fish_color_normal CCCCCC
set -gx fish_color_escape 789978
set -gx fish_color_comment 555555
set -gx fish_color_command CCCCCC
set -gx fish_color_keyword 7a7a7a
set -gx fish_color_operator 7788aa
set -gx fish_color_redirection ffaa88
set -gx fish_color_autosuggestion 2a2a2a
set -gx fish_color_selection --background=555555
set -gx fish_color_search_match --background=555555
set -gx fish_pager_color_prefix 999999
set -gx fish_pager_color_progress 555555
set -gx fish_pager_color_completion cccccc
set -gx fish_pager_color_description 7a7a7a
set -gx fish_pager_color_selected_background --background=555555
