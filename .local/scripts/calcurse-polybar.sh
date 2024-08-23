#!/bin/sh

# Get the current and next day in the desired format
TODAY=$(date +"%a, %b %d")
TOMORROW=$(date -d "tomorrow" +"%a, %b %d")

# Retrieve the description of the next event
DESCRIPTION=$(calcurse -d 7 --format-apt='%m' | sed -n 2p)

# Retrieve the time of the next event and replace with "Today" or "Tomorrow"
TIME=$(calcurse -n --format-apt='%(start:%a, %b %d) %(start:%H:%M) - %(end:%H:%M)\n' | sed -n 2p | sed -e "s/$TODAY/Today,/" -e "s/$TOMORROW/Tomorrow,/")

# Function to print usage
print_usage() {
	echo "Usage: $0 {description|time|both}"
	exit 1
}

# Check the argument passed and print accordingly
case "$1" in
description)
	echo "$DESCRIPTION"
	;;
time)
	echo "$TIME"
	;;
both)
	echo "$DESCRIPTION"
	echo "$TIME"
	;;
*)
	print_usage
	;;
esac
