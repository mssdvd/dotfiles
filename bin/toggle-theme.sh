#!/bin/sh

gtk_theme=$(gsettings get org.gnome.desktop.interface gtk-theme)

if [ "$gtk_theme" = "'Adwaita'" ]
then
    # Dark themes
    gtk_theme='Adwaita-dark'
    emacs_theme='vivendi'
else
    # Light themes
    gtk_theme='Adwaita'
    emacs_theme='operandi'
fi

gsettings set org.gnome.desktop.interface gtk-theme $gtk_theme
emacsclient --eval "(progn (modus-themes-load-themes)(modus-themes-load-$emacs_theme))"
