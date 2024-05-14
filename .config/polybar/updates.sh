#!/bin/sh
b=$(emerge -pquDU @world | grep '\[ebuild' | wc -l)
if [[ "$b" -ne "0" ]]; then
	printf ""
fi
