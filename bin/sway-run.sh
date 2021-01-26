#!/bin/sh

# Session
export TERM=alacritty
export XDG_SESSION_TYPE=wayland
export XDG_SESSION_DESKTOP=sway
export XDG_CURRENT_DESKTOP=sway
export MOZ_ENABLE_WAYLAND=1
export CLUTTER_BACKEND=wayland
export QT_QPA_PLATFORM=wayland-egl
export ECORE_EVAS_ENGINE=wayland-egl
export ELM_ENGINE=wayland_egl
export SDL_VIDEODRIVER=wayland
export _JAVA_AWT_WM_NONREPARENTING=1
export NO_AT_BRIDGE=1

export XDG_DESKTOP_DIR="$HOME"
export XDG_DOWNLOAD_DIR="$HOME/downloads"
export XDG_DOCUMENTS_DIR="$HOME/documents"
export XDG_MUSIC_DIR="$HOME/music"
export XDG_PICTURES_DIR="$HOME/pictures"
export XDG_VIDEOS_DIR="$HOME/videos"

eval $(ssh-agent)

exec systemd-cat --identifier=sway sway $@
