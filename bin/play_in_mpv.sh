#! /bin/bash

# URL="$(xclip -selection primary -o)"
# mpv $URL

if [ "$XDG_SESSION_TYPE" == "wayland" ]; then
    URL="$(wl-paste)"
else
    URL="$(xclip -o)"
fi

mpv $URL
