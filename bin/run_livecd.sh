#! /bin/bash

exec qemu-system-x86_64 -enable-kvm -smp cores=4,threads=8 -m 3G -cpu host -usb -device usb-tablet -cdrom "$@"
