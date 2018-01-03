#!/usr/bin/sh

~/Android/Sdk/tools/emulator -avd $(~/Android/Sdk/tools/emulator -list-avds | head -1) &
