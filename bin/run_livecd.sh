#!/bin/sh

args=(
    -machine type=q35,accel=kvm
    -smp cores=4,threads=2
    -m 3G
    -nic user,model=virtio
    -cpu host
    -vga virtio
    -device intel-hda
    -device hda-duplex
    -usb
    -device usb-tablet
    -display gtk,gl=on
    -object rng-random,id=rng0,filename=/dev/urandom
    -device virtio-rng-pci,rng=rng0
)

exec qemu-system-x86_64 "${args[@]}" -cdrom "$@"
