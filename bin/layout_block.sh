#! /bin/bash

if [ -n "$BLOCK_BUTTON" ]; then
    (setxkbmap -query | grep -q "layout:\s\+us") && setxkbmap it || setxkbmap us -variant euro  -option compose:ralt ;
fi


LAYOUT=$(setxkbmap -query | grep layout | awk '{print $2}')
echo $LAYOUT
