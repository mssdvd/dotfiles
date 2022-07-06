#!/bin/sh

VM_DIR=$HOME/vm

qemu-system-x86_64 \
    -m 4G \
    -machine type=q35,accel=kvm \
    -cpu host,hv_relaxed,hv_spinlocks=0x1fff,hv_vapic,hv_time \
    -smp cores=4,threads=2 \
    -net nic,model=virtio \
    -net user,smb="$VM_DIR"/share \
    -device intel-hda -device hda-duplex \
    -rtc base=localtime \
    -usb -device usb-tablet \
    -device ich9-usb-ehci1,id=usb \
    -device ich9-usb-uhci1,masterbus=usb.0,firstport=0,multifunction=on \
    -device ich9-usb-uhci2,masterbus=usb.0,firstport=2 \
    -device ich9-usb-uhci3,masterbus=usb.0,firstport=4 \
    -device virtio-serial-pci \
    -device virtserialport,chardev=spicechannel0,name=com.redhat.spice.0 \
    -chardev spicevmc,id=spicechannel0,name=vdagent \
    -display spice-app,gl=on \
    -chardev spicevmc,name=usbredir,id=usbredirchardev1 -device usb-redir,chardev=usbredirchardev1,id=usbredirdev1 \
    -chardev spicevmc,name=usbredir,id=usbredirchardev2 -device usb-redir,chardev=usbredirchardev2,id=usbredirdev2 \
    -chardev spicevmc,name=usbredir,id=usbredirchardev3 -device usb-redir,chardev=usbredirchardev3,id=usbredirdev3 \
    -vga none -device qxl-vga,vgamem_mb=64 \
    -drive if=pflash,format=raw,readonly=on,file=/usr/share/edk2-ovmf/x64/OVMF_CODE.fd \
    -drive if=pflash,format=raw,file="$VM_DIR"/uefi_vars.fd \
    -drive driver=qcow2,file="$VM_DIR"/win11.qcow2,if=virtio,aio=native,cache.direct=on,l2-cache-size=8M \
    "$@"
