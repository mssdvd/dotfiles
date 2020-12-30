#! /bin/sh


if [ "$XDG_SESSION_TYPE" == "wayland" ]; then
    URL="$(wl-paste)"
else
    URL="$(xclip -o)"
fi

OUTPUT="$1"

youtube-dl "${URL}" -o "${OUTPUT}"
