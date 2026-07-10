set fish_greeting

set -gx BROWSER "firefox"
set -gx DEBUGINFOD_URLS "https://debuginfod.archlinux.org/"
set -gx EDITOR "emacsclient -t --alternate-editor="
set -gx MAKEFLAGS "-j$(nproc)"
set -gx LEDGER_FILE "$HOME/ledger/ledger.ldg"
set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"
# set -gx LESS "-i"

function last_history_item; echo $history[1]; end
abbr -a !! --position anywhere --function last_history_item

alias qemu qemu-system-x86_64
alias ssh "env TERM=xterm-256color ssh"
alias yt yt-dlp
alias ll "ls -lh --color=auto"

fzf --fish | FZF_CTRL_T_COMMAND= source
bind \ex fzf-file-widget
