#!/bin/sh

args=(
    -m 3G
    -machine type=q35,accel=kvm
    -cpu host,hv_relaxed,hv_spinlocks=0x1fff,hv_vapic,hv_time
    -smp cores=4,threads=2
    -net nic,model=virtio
    -net user,smb=$HOME/vm/share
    -audiodev pa,id=snd0
    -device ich9-intel-hda
    -device hda-output,audiodev=snd0
    -rtc base=localtime
    -vga qxl
    -device virtio-serial
    -usb -device usb-tablet
    -display gtk,zoom-to-fit=on
    -drive driver=qcow2,file=$HOME/vm/win10.qcow2,if=virtio,aio=native,cache.direct=on,l2-cache-size=8M
)
exec qemu-system-x86_64 "${args[@]}" "$@"
