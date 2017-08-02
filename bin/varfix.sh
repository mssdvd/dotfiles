#!/bin/bash

/usr/bin/pkgfile -l $1 | awk '{print $2}' | grep -v var > /tmp/files
test -s /tmp/files || exit 1

while IFS= read -r f; do
    if [[ ! -e $f ]]; then
        exit 1
    fi
done < /tmp/files
echo $1
