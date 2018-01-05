#!/usr/bin/sh

for var in "$@"
do
    echo $((16#$var))
done
