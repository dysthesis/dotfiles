#!/bin/dash

# ^c$var^ = fg color
# ^b$var^ = bg color

interval=0

# load colors
cpu() {
  cpu_val=$(grep -o "^[^ ]*" /proc/loadavg)

  printf "^c#000000^ ^b#f9e2af^  "
  printf "^c#ffffff^ ^b#000000^ $cpu_val "
}

mem() {
  printf "^c#000000^^b#cba6f7^   "
  printf "^b#000000^ ^c#ffffff^$(free -h | awk '/^Mem/ { print $3 }' | sed s/i//g)"
}

clock() {
  printf "^c#000000^ ^b#83bae8^ 󱑆  "
  printf "^c#ffffff^^b#000000^ $(date "+%Y-%m-%d %H:%M")  "
}

taskwarrior() {
  printf "^b#a6e3a1^^c#000000^   "
  is_ready=$(timeout 10s task ready)
  if [ -z is_ready ]
  then
    printf "^c#ffffff^^b#000000^ No tasks "
  else
    next_id=$(timeout 10s task next limit:1 | tail -n +4 | head -n 1 | sed 's/^ //' | cut -d ' ' -f1)
    next_desc=$(timeout 10s task _get ${next_id}.description)
    next_due=$(timeout 10s task _get ${next_id}.due | cut -dT -f1)
    printf "^c#ffffff^^b#000000^  $next_desc due $next_due"
  fi
}

battery() {
  printf "^b#a6e3a1^^c#000000^   "
  get_capacity="$(cat /sys/class/power_supply/BAT1/capacity)"
  printf "^c#ffffff^^b#000000^  $get_capacity%s" " %"
}

brightness() {
  printf "^b#f2cdcd^^c#000000^   "
  printf "^c#ffffff^^b#000000^  %.0f%s\n" $(cat /sys/class/backlight/*/brightness) " % "
}

while true; do
  [ $interval = 0 ] || [ $(($interval % 3600)) = 0 ] 
  interval=$((interval + 1))

  sleep 1 && xsetroot -name "$(brightness) $(battery) $(cpu) $(mem) $(clock)"
done
