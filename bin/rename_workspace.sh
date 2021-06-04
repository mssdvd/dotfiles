#!/bin/sh
set -e

workspace=$(swaymsg -t get_workspaces| jq -r '.[] | select(.focused == true) | .name')
workspace_number=$(echo $workspace | awk '{print $1}')

swaymsg rename workspace $workspace to "${workspace_number} ${@}"
