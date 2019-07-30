#! /bin/sh

echo "Checking Submodules updates"
updatesubmodules.sh
echo "Checking Firmware updates"
fwupdmgr refresh; fwupdmgr get-updates
echo "Checking Rust updates"
rustup update
echo "Checking Cargo updates"
cargo install-update -a
echo "Checking System updates"
yay
