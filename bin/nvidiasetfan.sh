#! /bin/sh

TEMP=$(nvidia-smi --query-gpu=temperature.gpu --format=csv,noheader)

FAN=0

if [ "$TEMP" -ge "38" ]; then
	FAN=25
	if [ "$TEMP" -ge "45" ]; then
		FAN=35
		if [ "$TEMP" -ge "50" ]; then
			FAN=40
			if [ "$TEMP" -ge "60" ]; then
				FAN=50
				if [ "$TEMP" -ge "80" ]; then
					FAN=100
				fi
			fi
		fi
	fi
fi

nvidia-settings -a "[gpu:0]/GPUFanControlState=1" -a "[fan:0]/GPUTargetFanSpeed=$FAN"	 
