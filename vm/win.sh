#!/bin/bash

VM_DIR="$HOME/vm"

SPICE_PORT=5924

qemu-system-x86_64 \
    -daemonize \
    -m 4G \
    -machine type=q35,accel=kvm \
    -cpu host,hv_relaxed,hv_spinlocks=0x1fff,hv_vapic,hv_time \
    -smp cores=4,threads=2 \
    -net nic,model=virtio \
    -net user,smb="$VM_DIR"/share \
    -audiodev pa,id=snd0 \
    -device ich9-intel-hda \
    -device hda-output,audiodev=snd0 \
    -rtc base=localtime \
    -vga qxl \
    -device virtio-serial \
    -usb -device usb-tablet \
    -spice port=${SPICE_PORT},disable-ticketing=on \
    -chardev spicevmc,id=vdagent,name=vdagent \
    -device virtserialport,chardev=vdagent,name=com.redhat.spice.0 \
    -drive if=pflash,format=raw,readonly=on,file=/usr/share/edk2-ovmf/x64/OVMF_CODE.fd \
    -drive if=pflash,format=raw,file="$VM_DIR"/uefi_vars.fd \
    -drive driver=qcow2,file="$VM_DIR"/win11.qcow2,if=virtio,aio=native,cache.direct=on,l2-cache-size=8M \
    "$@"

exec spicy --title Windows 127.0.0.1 -p ${SPICE_PORT}
