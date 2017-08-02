#! /bin/sh

if [ -z "$1" ]
then
	echo "Codice board mancante!!!"
else
	platformio init --ide emacs --board $1
fi

