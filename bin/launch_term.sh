#!/bin/sh
exec $TERM -e $(basename "$0" | sed -e 's/_.*$/\L/' | tr '[:upper:]' '[:lower:]')
