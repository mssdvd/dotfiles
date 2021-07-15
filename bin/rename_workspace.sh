#!/bin/sh
set -e

workspace=$(swaymsg -t get_workspaces | jq -r '.[] | select(.focused == true) | .name')
workspace_number=$(echo $workspace | cut -d' ' -f1)

swaymsg rename workspace $workspace to "${workspace_number} ${@}"
