function dots --wraps='/usr/bin/git --git-dir="$HOME/.dotfiles/" --work-tree="$HOME"' --description 'Manage dotfiles'
  /usr/bin/git --git-dir="$HOME/.dotfiles/" --work-tree="$HOME" $argv
        
end
