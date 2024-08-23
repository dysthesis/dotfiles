#!/bin/sh

CALCURSE_OUTPUT=$(calcurse -n)

parse_calcurse_output() {
	local output
	output="$1"
	local time
	# Check if output is empty
	if [ -z "$output" ]; then
		HOUR=""
		MINUTE=""
		DESC=""
		return
	fi
	time=$(printf '%s' "$output" | grep -o '\[[0-9]\{2\}:[0-9]\{2\}\]')
	HOUR=$(printf '%s' "$time" | cut -d':' -f1 | tr -d '[]')
	MINUTE=$(printf '%s' "$time" | cut -d':' -f2 | tr -d '[]')
	DESC=$(printf '%s' "$output" | sed 's/.*\[[0-9]\{2\}:[0-9]\{2\}\] //' | sed -n 2p)
}

print_time_until_event() {
	local hours_left minutes_left
	hours_left="$1"
	minutes_left="$2"
	# Handle empty hour and minute cases
	if [ -z "$hours_left" ] || [ -z "$minutes_left" ]; then
		return
	fi

	if [ "$hours_left" -ne 0 ]; then
		printf 'in %d hour' "$hours_left"
		[ "$hours_left" -gt 1 ] && printf 's'
		[ "$minutes_left" -ne 0 ] && printf ' and '
	fi

	if [ "$minutes_left" -ne 0 ]; then
		printf 'in %d minute' "$minutes_left"
		[ "$minutes_left" -gt 1 ] && printf 's'
	fi
	printf '\n'
}

main() {
	if [ -z "$1" ]; then
		printf 'Usage: %s time|desc\n' "$0" >&2
		return 1
	fi

	parse_calcurse_output "$CALCURSE_OUTPUT"

	case "$1" in
	time)
		print_time_until_event "$HOUR" "$MINUTE"
		;;
	desc)
		printf '%s\n' "$DESC"
		;;
	*)
		printf 'Invalid argument. Use "time" or "desc".\n' >&2
		return 1
		;;
	esac
}

main "$@"
