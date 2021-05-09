#!/bin/sh
emacs -Q -D -r -T "emacs calc" -f full-calc --eval="(progn (setq
                      calc-group-digits t select-enable-primary
                      t)(set-face-attribute 'default nil :family
                      \"Iosevka\" :weight 'semi-bold :height 181))"
