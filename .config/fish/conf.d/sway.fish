set TTY (tty)
if test -z "$DISPLAY"; and test $TTY = "/dev/tty1"
    exec sway-run.sh
end
