# zsh completions
fpath=(/home/davide/.zsh/zsh-completions/src $fpath)
source $HOME/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh
autoload -Uz compinit
compinit

# thefuck
eval $(thefuck --alias)

# alias
alias open=xdg-open
alias locate="locate -bi"
alias rn="termite -e ranger . &"

# look for new exe in path
zstyle ':completion:*' rehash true

# navigate menu with tab
zstyle ':completion:*' menu select

# auto complete aliases
setopt COMPLETE_ALIASES

# Disable C-s
stty -ixon

# source .zprofile
source $HOME/.zprofile

# command not found
source /usr/share/doc/pkgfile/command-not-found.zsh

# auto suggestions
source $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# syntax highlight
source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# history substring search
source $HOME/.zsh/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# ranger
ranger() {
	if [ -z "$RANGER_LEVEL" ]; then
		/usr/bin/ranger "$@"
	else
		exit
	fi
}

# fzf
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh
export FZF_COMPLETION_TRIGGER='``'
export FZF_DEFAULT_OPTS="--bind '?:toggle-preview' --preview 'highlight -O ansi -l {}' --preview-window 'right:hidden'"
