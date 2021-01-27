#!/bin/sh
$TERM -e $(echo $(basename $0) | sed -e 's/_.*$/\L/' | tr '[:upper:]' '[:lower:]')
