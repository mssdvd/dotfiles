#!/bin/sh

# emacsclient -c -a '' --eval "(progn
#                                (require 'notmuch)
#                                  (if (string= (shell-command-to-string \"notmuch count tag:unread\") \"0\n\")
#                                      (notmuch)
#                                    (notmuch-search \"tag:unread\" t)))"
emacsclient -c -a '' --eval "(progn
                               (require 'mu4e)
                               (+mu4e-view-unread-emails-maybe))"
