#!/usr/bin/sh

if [ "$(id -u)" -ne 0 ]; then
	echo "Permission denied"
	exit 1
fi

echo 100 >/sys/class/power_supply/BAT0/charge_control_end_threshold
echo 99 >/sys/class/power_supply/BAT0/charge_control_start_threshold
