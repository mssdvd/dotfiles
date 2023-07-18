set fish_greeting

fish_add_path $HOME/{.local/bin, bin, .cargo/bin, go/bin}

set -gx EDITOR "emacsclient -t -a ''"
set -gx MAKEFLAGS "-j$(nproc)"

alias qemu qemu-system-x86_64
alias ssh "env TERM=xterm-256color ssh"
alias yt yt-dlp

if ! pgrep -u "$USER" ssh-agent > /dev/null
    ssh-agent -c -t 1h > "$XDG_RUNTIME_DIR/ssh-agent.env"
end
if ! set -q SSH_AUTH_SOCK
    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
end

if status is-login; and test -z "$DISPLAY"; and test $(tty) = "/dev/tty1"
    exec sway-run.sh
end
