#! /bin/sh

kill -15 $(pgrep nvidiasetfantim | grep -v $$) &> /dev/null 
while true; do /home/davide/bin/nvidiasetfan.sh &> /dev/null; sleep 30; done
