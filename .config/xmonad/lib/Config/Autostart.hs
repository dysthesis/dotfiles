module Config.Autostart (myStartupHook) where

import XMonad (X)
import XMonad.Core (spawn)
import XMonad.Util.SpawnOnce (spawnOnce)

myStartupHook :: X ()
myStartupHook = do
        -- proper monitor layout
        spawnOnce "xrandr --output DisplayPort-1 --mode 1920x1080 --rate 165 --primary --output DisplayPort-0 --left-of DisplayPort-1"

        -- Tint the screen yellow at night to prevent eye strain
        spawnOnce "redshift -l -33.9166485:151.2233364"

        -- Notification daemon
        spawnOnce "dunst"

        -- Clipboard manager
        spawnOnce "parcellite"

        -- Screenshot util
        spawnOnce "flameshot"

        -- Auto mount attached drives
        spawnOnce "udiskie"

        -- Polkit agent
        spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"

        -- Set the wallpaper
        spawn "hsetroot -cover ~/.config/wallpaper.png"
