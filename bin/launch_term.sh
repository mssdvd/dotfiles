#!/bin/sh
exec $TERM $(echo $(basename $0) | sed -e 's/_.*$/\L/' | tr '[:upper:]' '[:lower:]')
