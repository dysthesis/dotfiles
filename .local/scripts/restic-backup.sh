#!/bin/sh

# Check if restic backup is already running
if [ -n "$(pgrep 'restic' | grep 'restic backup')" ]; then
  echo 'restic is already running...' 1>&2
  exit 0
fi

set -e
set -v

# Export environment variables
export RESTIC_REPOSITORY='/mnt/backup'
export RESTIC_PASSWORD_COMMAND='pass backup/restic'
export RESTIC_COMPRESSION='auto'
export RESTIC_CACHE_DIR="$HOME/.cache/restic"
export DIRECTORY="$HOME"
export LOG_FILE="$HOME/.local/logs/restic_backup.log"

echo "\n------------------------------------------------------------------------\n" >> $LOG_FILE
echo "Backing up $DIRECTORY from $(whoami)@$(uname -n) at $(date +"%H:%M, %a %b %d")" >> $LOG_FILE
# Create cache directory if it doesn't exist
mkdir -p "${RESTIC_CACHE_DIR}" 2>> $LOG_FILE >> $LOG_FILE

# Run restic commands
echo "\nUnlocking the repository at $RESTIC_REPOSITORY" >> $LOG_FILE
restic unlock >> $LOG_FILE 2>> $LOG_FILE

echo "\nBacking up $DIRECTORY to $RESTIC_REPOSITORY" >> $LOG_FILE
restic backup "$DIRECTORY" --tag scheduled >> $LOG_FILE 2>> $LOG_FILE

echo "\nChecking repository" >> $LOG_FILE
restic check --with-cache --read-data-subset=5G >> $LOG_FILE 2>> $LOG_FILE
restic forget --prune --keep-hourly 24 --keep-daily 30 --keep-monthly 6 --keep-weekly 4 --keep-yearly 3 2>> $LOG_FILE >> $LOG_FILE

echo "\nDone!" >> $LOG_FILE
# Send notification
notify-send "Restic backup" "Backed up $DIRECTORY"
