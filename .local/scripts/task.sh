#!/bin/dash

next_desc=$(task rc.verbose: rc.report.next.columns:description rc.report.next.labels:1 limit:1 next)
next_due=$(task rc.verbose: rc.report.next.columns:due.relative rc.report.next.labels:1 limit:1 next)
echo "$next_desc, due in $next_due"
