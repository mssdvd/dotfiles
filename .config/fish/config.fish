set fish_greeting

fish_add_path $HOME/{.local/bin, bin, .cargo/bin, go/bin}

set -gx EDITOR "emacsclient -t -a ''"
set -gx MAKEFLAGS "-j$(nproc)"
set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"
set -gx LESS "-i"

alias qemu qemu-system-x86_64
alias ssh "env TERM=xterm-256color ssh"
alias yt yt-dlp

if status is-login; and test -z "$DISPLAY"; and test $(tty) = "/dev/tty1"
    exec sway-run.sh
end
