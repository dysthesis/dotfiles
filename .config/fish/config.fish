if status is-interactive
	starship init fish | source
	zoxide init fish | source
	neofetch
end

alias sudo='doas'
alias doom='~/.config/emacs/bin/doom'
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias ls='eza --icons'
alias ll='eza -l --icons'
alias la='eza -la --icons'
alias lst='eza --tree --icons'
alias llt='eza -l --tree --icons'
alias lat='eza -la --tree --icons'
alias cse-ssh='ssh z5437039@login9.cse.unsw.edu.au'

function ya
	set tmp (mktemp -t "yazi-cwd.XXXXX")
	yazi $argv --cwd-file="$tmp"
	if set cwd (cat -- "$tmp"); and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
		cd -- "$cwd"
	end
	rm -f -- "$tmp"
end
