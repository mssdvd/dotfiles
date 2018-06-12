#! /bin/bash

case "$BLOCK_BUTTON" in
    1) light -A 5 ;;
    2) light -S 1 ;;
    3) light -U 5 ;;
    4) light -A 5 ;;
    5) light -U 5 ;;
esac

LIGHT=$(light)
echo $(LC_ALL=C /usr/bin/printf "%.0f" "$LIGHT")%
