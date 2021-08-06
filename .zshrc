# zsh completions
export RUST_SYSROOT=/home/davide/.rustup/toolchains/stable-x86_64-unknown-linux-gnu
fpath=($RUST_SYSROOT/share/zsh/site-functions/ $fpath)

# https://github.com/sorin-ionescu/prezto/blob/b01f02aa5c6714430647a4ee854149e9a336270a/modules/completion/init.zsh#L31-L41
autoload -Uz compinit
_comp_files=($HOME/.zcompdump(Nm-12))
if (( $#_comp_files )); then
  compinit -i -C
else
  compinit -i
fi
unset _comp_files


unsetopt nomatch

export CLASSPATH=/usr/share/java/junit.jar

# Default editor
export EDITOR="emacsclient -t -a ''"

export UNITS_ENGLISH="US"

# alias
alias open=xdg-open
alias locate="locate -bi"
alias rn="termite -e ranger . &"

function e {
    if [ -z "$@" ]
    then
        emacsclient -t -a '' -- .
    else
        emacsclient -t -a '' -- "$@"
    fi
}

function E {
    if [ -z "$@" ]
    then
        emacsclient -c -a '' -- . &
    else
        emacsclient -c -a '' -- "$@" &
    fi
}

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
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# syntax highlight
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# history substring search
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
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

# OSC 7
_urlencode() {
	local length="${#1}"
	for (( i = 0; i < length; i++ )); do
		local c="${1:$i:1}"
		case $c in
			%) printf '%%%02X' "'$c" ;;
			*) printf "%s" "$c" ;;
		esac
	done
}

osc7_cwd() {
	printf '\e]7;file://%s%s\e\\' "$HOSTNAME" "$(_urlencode "$PWD")"
}

autoload -Uz add-zsh-hook
add-zsh-hook -Uz chpwd osc7_cwd

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
