#!/bin/sh

# Path to the synced .ics file
ICS_FILE=~/Sync/calendar.ics

# Export calcurse calendar to .ics
calcurse -x > $ICS_FILE

# Import any changes from .ics to calcurse
calcurse -i $ICS_FILE
