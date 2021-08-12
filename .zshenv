typeset -U PATH path
path=( ~/.local/bin ~/bin ~/.cargo/bin ~/go/bin $path[@] )
export PATH

export RIPGREP_CONFIG_PATH=$HOME/.config/ripgreprc
