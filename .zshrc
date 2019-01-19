# zsh completions
fpath=($HOME/.zsh/zsh-completions/src $HOME/.zsh/custom-completions/ $fpath)
source $HOME/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh
autoload -Uz compinit
compinit -d ~/.cache/zsh/zcompdump

# thefuck
eval $(thefuck --alias)

# Default editor
export EDITOR="emacsclient -t -a ''"

# alias
alias open=xdg-open
alias locate="locate -bi"
alias rn="termite -e ranger . &"
alias trz=trizen
alias edit="emacsclient -t -a ''"

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
export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--bind '?:toggle-preview' --preview 'highlight -O ansi -l {}' --preview-window 'right:hidden'"

_fzf_compgen_path() {
    fd --hidden --follow --exclude ".git" . "$1"
}

_fzf_compgen_dir() {
    fd --type d --hidden --follow --exclude ".git" . "$1"
}

# pip
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache
export PIP_REQUIRE_VIRTUALENV=true
gpip() {
    PIP_REQUIRE_VIRTUALENV="" pip "$@"
}

# Completion for kitty
kitty + complete setup zsh | source /dev/stdin
