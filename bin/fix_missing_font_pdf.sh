#! /bin/sh

file="$1"

/usr/bin/gs \
  -o "${file}.repaired" \
  -dPDFSETTINGS=/prepress \
  -sDEVICE=pdfwrite \
  "$file"

mv -f "${file}.repaired" "$file"
