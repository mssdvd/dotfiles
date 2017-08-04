#! /bin/sh

cd $(xdg-user-dir DOCUMENTS)/dotfiles
git submodule update --init --recursive
