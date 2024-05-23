#!/bin/bash
if [[ -z $(task ready) ]]; then
	echo "No active task"
else
	echo "$(task _get $(task next limit:1 | tail -n +4 | head -n 1 | sed 's/^ //' | cut -d ' ' -f1).description)"
fi
