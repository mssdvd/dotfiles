# zsh completions
export RUST_SYSROOT=/home/davide/.rustup/toolchains/stable-x86_64-unknown-linux-gnu
fpath=($RUST_SYSROOT/share/zsh/site-functions/ $HOME/.zsh/zsh-completions/src $HOME/.zsh/custom-completions/ $fpath)
# source $HOME/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh
autoload -Uz compinit
compinit -d ~/.cache/zsh/zcompdump

unsetopt nomatch

export CLASSPATH=/usr/share/java/junit.jar

# Default editor
export EDITOR="emacsclient -t -a ''"

export UNITS_ENGLISH="US"

# alias
alias open=xdg-open
alias locate="locate -bi"
alias rn="termite -e ranger . &"
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

rga-fzf() {
	RG_PREFIX="rga --files-with-matches"
	local file
	file="$(
		FZF_DEFAULT_COMMAND="$RG_PREFIX '$1'" \
			fzf --sort --preview="[[ ! -z {} ]] && rga --pretty --context 5 {q} {}" \
				--phony -q "$1" \
				--bind "change:reload:$RG_PREFIX {q}" \
				--preview-window="70%:wrap"
	)" &&
	echo "opening $file" &&
	xdg-open "$file"
}

# fzf
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh
export FZF_COMPLETION_TRIGGER='``'
export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow -E .git -E .wine"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--bind '?:toggle-preview' --preview 'highlight -O ansi -l {}' --preview-window 'right:hidden'"

_fzf_compgen_path() {
    fd --hidden --follow --exclude ".git" . "$1"
}

_fzf_compgen_dir() {
    fd --type d --hidden --follow --exclude ".git" . "$1"
}

# Completion for kitty
kitty + complete setup zsh | source /dev/stdin

function yta() {
    mpv --ytdl-format=bestaudio ytdl://ytsearch:"$*"
}

vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# export DISPLAY=:1
# export GDK_BACKEND=wayland
# export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
# export _JAVA_AWT_WM_NONREPARENTING=1
# export QT_QPA_PLATFORMTHEME="qt5ct"

if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent -t 1h > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [[ ! "$SSH_AUTH_SOCK" ]]; then
    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
fi

# Autostart sway
if [[ -z $DISPLAY && $(tty) == /dev/tty1 && $XDG_SESSION_TYPE == tty ]]; then
	exec sway-run.sh
fi
