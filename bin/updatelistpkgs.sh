#! /bin/sh

pacman -Qq > $(xdg-user-dir DOCUMENTS)/dotfiles/package_list.txt
pacman -Qeq > $(xdg-user-dir DOCUMENTS)/dotfiles/package_exp_list.txt
pacman -Qmq > $(xdg-user-dir DOCUMENTS)/dotfiles/package_aur_list.txt
