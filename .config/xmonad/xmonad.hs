import XMonad

{-- Utilities --}
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)

{-- Layouts --}
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.MultiToggle (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))

{-- Hooks --}
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)

{-- System --}

import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen, isInProperty)
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)

import Config.Layout
import Config.Prompt
import Config.Scratchpads
import Config.Search
import Config.XMobar

import Utils.Taskwarrior

{-- VARIABLES:
 - Define some basic settings for XMonad here. This includes the modifier keys, default terminal emulator, window borders, etc. --}
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "st"

myLauncher :: String
myLauncher = "dmenu_run -p ' \62645  '"

myBorderWidth :: Dimension
myBorderWidth = 1

myNormColor :: String
myNormColor = "#0f0f0f"

myFocusColor :: String
myFocusColor = "#FFFFFF"

myEditor :: String
myEditor = "nvim"

{-- KEYBINDINGS:
 - The following keybindings are to be used with the `additionalKeysP` function
 - provided by XMonad.Util.EZConfig to provide a simpler syntax --}

myKeys :: [(String, X ())]
myKeys =
  {-- XMONAD --}

  [ ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  , ("M-r", spawn myLauncher)
  , ("M-<Return>", spawn myTerminal)
  , ("M-q", kill)
  , ("M-n", nextScreen)
  , ("M-S-n", shiftNextScreen)
  , ("M-t f", sendMessage $ Toggle NBFULL)
  , ("<XF86AudioRaiseVolume>", spawn increaseVolCmd)
  , ("<XF86AudioLowerVolume>", spawn decreaseVolCmd)
  , ("M-p", spawn "flameshot gui")
  , ("M-a t", taskPrompt myXPConfig)
  ]
    -- Append search engines to the keybinding list
    ++ myLayoutKeybinds
    ++ mySearchKebinds
    ++ myScratchpadKeybinds
 where
  audioDelta = 5 -- configures how much each command should change the volume by
  increaseVolCmd = "wpctl set-volume @DEFAULT_AUDIO_SINK@ " ++ show audioDelta ++ "%+"
  decreaseVolCmd = "wpctl set-volume @DEFAULT_AUDIO_SINK@ " ++ show audioDelta ++ "%-"

{-- MANAGE HOOK --}
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ isDialog --> doCenterFloat
    , isFileChooserDialog --> doCenterFloat
    , isPopup --> doCenterFloat
    , isGtk4Modal --> doCenterFloat
    , isGtk4Dialog --> doCenterFloat
    , isSplash --> doCenterFloat
    , isFullscreen --> doFullFloat
    , className =? "confirm" --> doFloat
    , className =? "file_progress" --> doFloat
    , className =? "dialog" --> doFloat
    , className =? "download" --> doFloat
    , className =? "error" --> doFloat
    , className =? "firefox" --> doShift "1"
    , className =? "FreeTube" --> doShift "3"
    , className =? "mpv" --> doShift "3"
    , className =? "vesktop" --> doShift "2"
    , className =? "Element" --> doShift "2"
    , className =? "Zathura" --> doShift "4"
    , className =? "thunderbird" --> doShift "5"
    , -- , className =? "virt-manager" --> doShift "6"
      className =? "Virt-manager" --> doShift "6"
    , className =? "steam" --> doShift "7"
    ]
    <+> myScratchpadManageHook
 where
  isRole = stringProperty "WM_WINDOW_ROLE"
  isFileChooserDialog = isRole =? "GtkFileChooserDialog"
  isPopup = isRole =? "pop-up"
  isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
  isGtk4Dialog = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"
  isGtk4Modal = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_MODAL"

{-- STARTUP --}
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

myHandleEventHook = windowedFullscreenFixEventHook <> swallowEventHook (className =? "st-256color" <||> className =? "Alacritty") (return True)

myConfig =
  def
    { modMask = myModMask
    , terminal = myTerminal
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormColor
    , focusedBorderColor = myFocusColor
    , layoutHook = myLayout
    , -- , workspaces = myWorkspaces
      startupHook = myStartupHook
    , manageHook = myManageHook <+> manageDocks
    , handleEventHook = myHandleEventHook
    -- , handleEventHook =
    --     handleEventHook def
    }
    `additionalKeysP` myKeys

main :: IO ()
main =
  do
    xmonad
    . docks
    . ewmhFullscreen
    . ewmh
    . fullscreenSupport
    $ xmobarProp myConfig
