#!/bin/sh

output=$(swaymsg -t get_outputs | jq -r '.[] | select(.focused == true) | .name')

if [ "$output" = "eDP-1" ]
then exec /bin/light "$@"
fi

pgrep ddcutil
if [ $? -ne 1 ]
then exit 1
fi


level=$(ddcutil getvcp 10 -t | cut -d' ' -f4)

case "$1" in
    -A)
        level=$((level + "$2"))
        ;;
    -U)
        level=$((level - "$2"))
        ;;
    -S)
        level=$(printf "%.0f\n" "$2")
        ;;
    *)
        ddcutil getvcp 10 -t | cut -d' ' -f4
        exit 0
        ;;
esac

ddcutil setvcp 10 "$level"
