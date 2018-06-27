#! /bin/bash

URL="$(xclip -selection primary -o)"
echo $URL
mpv "${URL}"
