#! /bin/sh

if [ "$XDG_SESSION_TYPE" == "wayland" ]; then
    URL="$(wl-paste)"
else
    URL="$(xclip -o)"
fi

mpv --speed=2.0 --force-window=immediate -- $URL
