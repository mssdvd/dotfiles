#!/bin/sh

echo "Checking System updates"
paru
echo "Checking Firmware updates"
fwupdmgr refresh; fwupdmgr get-updates
echo "Checking Rust updates"
rustup update
echo "Checking Cargo updates"
cargo install-update -a
echo "Checking Submodules updates"
updatesubmodules.sh
echo "Checking Flatpak updates"
flatpak update
echo "Running needstart"
needrestart
