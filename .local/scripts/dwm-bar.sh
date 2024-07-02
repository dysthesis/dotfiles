#!/bin/dash

# ^c$var^ = fg color
# ^b$var^ = bg color

interval=0

# load colors
cpu() {
  cpu_val=$(grep -o "^[^ ]*" /proc/loadavg)

  printf "^c#000000^ ^b#f9e2af^  "
  printf "^c#ffffff^ ^b#000000^ $cpu_val"
}

pkg_updates() {
  updates=$({ timeout 20 doas xbps-install -un 2>/dev/null || true; } | wc -l) # void
  #updates=$({ timeout 20 checkupdates 2>/dev/null || true; } | wc -l) # arch
  # updates=$({ timeout 20 aptitude search '~U' 2>/dev/null || true; } | wc -l)  # apt (ubuntu, debian etc)
  
  printf "  ^c#000000^ ^b#ABE9B3^  "
  if [ -z "$updates" ]; then
    printf "^c#ffffff^ ^b#000000^ Fully Updated"
  else
    printf "^c#ffffff^ ^b#000000^ $updates"" updates"
  fi
}

battery() {
  get_capacity="$(cat /sys/class/power_supply/BAT1/capacity)"
  printf "^c#96CDFB^   $get_capacity"
}

brightness() {
  printf "^c#F28FAD^   "
  printf "^c#F28FAD^%.0f\n" $(cat /sys/class/backlight/*/brightness)
}

mem() {
  printf "^c#000000^^b#cba6f7^   "
  printf "^b#000000^ ^c#ffffff^ $(free -h | awk '/^Mem/ { print $3 }' | sed s/i//g)"
}

wlan() {
	case "$(cat /sys/class/net/wl*/operstate 2>/dev/null)" in
	up) printf "^c#000000^ ^b#96CDFB^ 󰤨 ^d^%s" " ^c#96CDFB^Connected" ;;
	down) printf "^c#000000^ ^b#96CDFB^ 󰤭 ^d^%s" " ^c#96CDFB^Disconnected" ;;
	esac
}

clock() {
	printf "^c#000000^ ^b#83bae8^ 󱑆  "
	printf "^c#ffffff^^b#000000^ $(date '+%H:%M')  "
}

while true; do

  [ $interval = 0 ] || [ $(($interval % 3600)) = 0 ] && updates=$(pkg_updates)
  interval=$((interval + 1))

  sleep 1 && xsetroot -name "$updates $(cpu) $(mem) $(clock)"
done
