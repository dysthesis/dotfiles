#!/bin/sh

TIME=12h

task status:pending tags.hasnt:notified due.before:now+$TIME export |  
jq -r ".[].description" |  
while IFS= read -r line  
do  
   notify-send "A task is due in less than $TIME" "$line"
done

task status:pending tags.hasnt:notified due.before:now+$TIME modify +notified
