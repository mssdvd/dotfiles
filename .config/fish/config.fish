set fish_greeting

fish_add_path $HOME/{.local/bin, bin, .cargo/bin, go/bin}

set -gx EDITOR emacsclient -t -a ''

if ! pgrep -u "$USER" ssh-agent > /dev/null
    ssh-agent -c -t 1h > "$XDG_RUNTIME_DIR/ssh-agent.env"
end
if ! set -q SSH_AUTH_SOCK
    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
end
