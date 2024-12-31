#!/bin/dash

interval=0

# load colors
cpu() {
  cpu_val=$(grep -o "^[^ ]*" /proc/loadavg)

  printf "  ^fg(cba6f7) ^fg()"
  printf "$cpu_val"
}

mem() {
  printf "  ^fg(89b4fa) ^fg()"
  printf " $(free -h | awk '/^Mem/ { print $3 }' | sed s/i//g)"
}

clock() {
  printf "  ^fg(b4befe)󱑆 ^fg()"
  printf "$(date "+%Y-%m-%d %H:%M")"
}

taskwarrior() {
  printf "   "
  is_ready=$(timeout 10s task ready)
  if [ -z is_ready ]
  then
    printf " No tasks "
  else
    next_id=$(timeout 10s task next limit:1 | tail -n +4 | head -n 1 | sed 's/^ //' | cut -d ' ' -f1)
    next_desc=$(timeout 10s task _get ${next_id}.description)
    next_due=$(timeout 10s task _get ${next_id}.due | cut -dT -f1)
    printf "  $next_desc due $next_due"
  fi
}

battery() {
  printf "  ^fg(a6e3a1) ^fg()"
  get_capacity="$(cat /sys/class/power_supply/BAT1/capacity)"
  printf "$get_capacity%s" " %"
}

volume() {
  printf "  ^fg(eba0ac)  ^fg()"
  echo "$(echo "scale=2; $(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{print $2}') * 100" | bc  | cut -d '.' -f 1) %"
}

brightness() {
  printf "  ^fg(f9e2af) ^fg()"
  echo "$(echo "scale=2; $(cat /sys/class/backlight/*/brightness) / 255 * 100" | bc | cut -d '.' -f 1) %"
}

DELIMITER="  ^fg(313244)|^fg()"
while true; do
  [ $interval = 0 ] || [ $(($interval % 3600)) = 0 ] 
  interval=$((interval + 1))

  sleep 1 && echo "$(volume)$DELIMITER$(brightness)$DELIMITER$(battery)$DELIMITER$(cpu)$DELIMITER$(mem)$DELIMITER$(clock)"
done
