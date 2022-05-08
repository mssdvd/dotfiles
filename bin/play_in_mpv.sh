#! /bin/sh

if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
    URL="$(wl-paste)"
else
    URL="$(xclip -o)"
fi

exec mpv --speed=2.0 --force-window=immediate --demuxer-max-bytes=3GiB "$@" -- "$URL"
