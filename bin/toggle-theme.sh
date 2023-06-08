#!/bin/sh

gtk_theme=$(gsettings get org.gnome.desktop.interface gtk-theme)

if [ "$gtk_theme" = "'Adwaita'" ]; then
	# Dark themes
	gtk_theme='Adwaita-dark'
	gtk4_color_scheme='prefer-dark'
else
	# Light themes
	gtk_theme='Adwaita'
	gtk4_color_scheme='prefer-light'
fi

gsettings set org.gnome.desktop.interface gtk-theme $gtk_theme
gsettings set org.gnome.desktop.interface color-scheme $gtk4_color_scheme
emacsclient --eval "(modus-themes-toggle)" >/dev/null
