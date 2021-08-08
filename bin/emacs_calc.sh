#!/bin/sh
emacs --quick --basic-display --reverse-video --title="emacs calc" --funcall=full-calc \
      --eval="(progn (setq calc-group-digits t
                           native-comp-deferred-compilation nil
                           native-comp-async-report-warnings-errors nil
                           select-enable-primary t)
                     (set-face-attribute 'default nil
                                         :family \"Iosevka\"
                                         :weight 'semi-bold
                                         :height 180)
                     (local-set-key (kbd \"q\") 'kill-emacs))"
