#!/bin/sh

TIME=$(date +"%H:%M, %a %b %d")
PASS_DIR="$HOME/.password-store/"
LOGS_DIR="$HOME/.local/logs/pass_backup.log"

echo "------------------------------------------------------------------------\n" >> $LOGS_DIR
cd $PASS_DIR 2>> $LOGS_DIR >> $LOGS_DIR
echo "Initialising backup of password store at directory $(pwd) at $TIME" >> $LOGS_DIR

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

for remote in $REMOTES
do
	 echo "Pushing to $remote/$BRANCH\n" >> $LOGS_DIR
	 git push $remote $BRANCH 2>> $LOGS_DIR >> $LOGS_DIR
done

echo "\n------------------------------------------------------------------------\n" >> $LOGS_DIR
