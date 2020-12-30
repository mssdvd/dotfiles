#! /bin/bash

emacs -Q -D -r -T "emacs calc" -f full-calc --eval="(progn (setq calc-group-digits t select-enable-primary t) (set-frame-font \"-BE5n-Iosevka-semibold-normal-normal-*-23-*-*-*-d-0-iso10646-1\"))"
