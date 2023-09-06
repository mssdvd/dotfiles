#!/bin/sh

color_scheme=${1:-$(gsettings get org.gnome.desktop.interface color-scheme)}

if [ "$color_scheme" = "dark" ] || [ "$color_scheme" = "'prefer-light'" ]; then
	gtk3_theme='Adwaita-dark'
	color_scheme='prefer-dark'
	emacs_theme='modus-vivendi'
elif [ "$color_scheme" = "light" ] || [ "$color_scheme" = "'prefer-dark'" ]; then
	gtk3_theme='Adwaita'
	color_scheme='prefer-light'
	emacs_theme='modus-operandi'
else
    exit 1
fi

gsettings set org.gnome.desktop.interface gtk-theme "$gtk3_theme"
gsettings set org.gnome.desktop.interface color-scheme "$color_scheme"
emacsclient --eval "(modus-themes-load-theme '$emacs_theme)" >/dev/null
