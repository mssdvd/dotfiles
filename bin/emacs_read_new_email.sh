#!/bin/sh

emacsclient -c -a '' --eval "(progn
                               (require 'notmuch)
                                 (if (string= (shell-command-to-string \"notmuch count tag:unread and tag:inbox\") \"0\n\")
                                     (notmuch)
                                   (notmuch-search \"tag:unread and tag:inbox\" t)))"
