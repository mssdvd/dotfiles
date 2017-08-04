 fpath=(/home/davide/.zsh/zsh-completions/src $fpath)
source $HOME/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh
autoload -Uz compinit
compinit
eval $(thefuck --alias)
alias open=xdg-open
alias locate="locate -bi"
zstyle ':completion:*' rehash true
zstyle ':completion:*' menu select
setopt COMPLETE_ALIASES
stty -ixon
source $HOME/.zprofile
source /usr/share/doc/pkgfile/command-not-found.zsh
source $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOME/.zsh/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
