#!/bin/sh

TIME=$(date +"%H:%M, %a %b %d")
NOTES_DIR="$HOME/Documents/Notes/"
LOGS_DIR="$HOME/.local/logs/notes_backup.log"

echo "------------------------------------------------------------------------\n" >> $LOGS_DIR
cd $NOTES_DIR 2>> $LOGS_DIR >> $LOGS_DIR
echo "Initialising backup of notes at directory $(pwd) at $TIME" >> $LOGS_DIR

BRANCH=$(git symbolic-ref --short HEAD)
REMOTES=$(git remote)

echo "On branch $BRANCH with remotes:\n" >> $LOGS_DIR
for remote in $REMOTES
do
	echo "- $remote\n" >> $LOGS_DIR
done

for remote in $REMOTES
do
	echo "Pulling from $remote/$BRANCH\n" >> $LOGS_DIR
	git pull $remote $BRANCH 2>> $LOGS_DIR >> $LOGS_DIR
done

echo "Adding files in the current working directory." >> $LOGS_DIR
git add . 2>> $LOGS_DIR >> $LOGS_DIR

FROM="$(whoami)@$(uname -n)"
COMMIT_MESSAGE="Automatic backup at $TIME from $FROM"
echo "Committing the current changes with the message '$COMMIT_MESSAGE'" >> $LOGS_DIR
git commit -am "$COMMIT_MESSAGE" 2>> $LOGS_DIR >> $LOGS_DIR

for remote in $REMOTES
do
	 echo "Pushing to $remote/$BRANCH\n" >> $LOGS_DIR
	 git push $remote $BRANCH 2>> $LOGS_DIR >> $LOGS_DIR
done

echo "\n------------------------------------------------------------------------\n" >> $LOGS_DIR
