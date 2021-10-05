#!/bin/sh
exec $TERM -e $(echo $(basename $0) | sed -e 's/_.*$/\L/' | tr '[:upper:]' '[:lower:]')
