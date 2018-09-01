#! /bin/bash

URL="$(xclip -selection primary -o)"
mpv $URL

if [ $? -eq 2 ]; then
    URL="$(xclip -o)"
    mpv $URL
fi
