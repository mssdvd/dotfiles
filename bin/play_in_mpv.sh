#! /bin/sh

if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
    URL="$(wl-paste)"
else
    URL="$(xclip -o)"
fi

exec mpv --force-window=immediate --demuxer-max-bytes=3GiB "$@" -- "$URL"
