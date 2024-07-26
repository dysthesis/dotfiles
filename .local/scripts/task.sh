#!/bin/dash

if [ -z is_ready ]
then
  printf "No tasks"
else
  next_id=$(timeout 10s task next limit:1 | tail -n +4 | head -n 1 | sed 's/^ //' | cut -d ' ' -f1)
  next_desc=$(timeout 10s task _get ${next_id}.description)
  next_due=$(timeout 10s task _get ${next_id}.due | cut -dT -f1)
  printf "$next_desc due $next_due"
fi
