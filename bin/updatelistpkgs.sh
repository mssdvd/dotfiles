#! /bin/sh

pacman -Qq > ~/.dotfiles/package_list.txt
pacman -Qeq > ~/.dotfiles/package_exp_list.txt
pacman -Qmq > ~/.dotfiles/package_aur_list.txt
