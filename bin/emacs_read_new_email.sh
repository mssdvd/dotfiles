#!/bin/sh

emacsclient -c -a '' --eval "(progn (require 'notmuch)(notmuch-search \"tag:unread\" t))"
