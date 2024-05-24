#!/bin/sh

# Get the current date in YYYY-MM-DD format
current_date=$(date +%d/%m/%Y)

# Get the list of events for today
events=$(khal list now)

# Check if the events list is empty or only contains the header
if echo "$events" | grep -q "$current_date"; then
	# Get the next event by selecting the first line after the header
	next_event=$(echo "$events" | sed -n '2p')
	if [ -z "$next_event" ]; then
		echo "No events today"
	else
		echo "$next_event"
	fi
else
	echo "No events today"
fi
