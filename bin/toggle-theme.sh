#!/bin/sh

gtk_theme=$(gsettings get org.gnome.desktop.interface gtk-theme)

if [ "$gtk_theme" = "'Adwaita'" ]; then
	# Dark themes
	gtk_theme='Adwaita-dark'
	gtk4_color_scheme='prefer-dark'
	emacs_theme='modus-vivendi'
else
	# Light themes
	gtk_theme='Adwaita'
	gtk4_color_scheme='prefer-light'
	emacs_theme='modus-operandi'
fi

gsettings set org.gnome.desktop.interface gtk-theme $gtk_theme
gsettings set org.gnome.desktop.interface color-scheme $gtk4_color_scheme
emacsclient --eval "(progn (require 'modus-themes)(load-theme '$emacs_theme :no-confirm))" >/dev/null
