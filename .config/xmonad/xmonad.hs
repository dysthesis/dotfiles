{-- PRAGMAS --}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

import XMonad
import XMonad.StackSet qualified as W

{-- Utilities --}

import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Types (Direction2D (D, L, R, U))

{-- Layouts --}

import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))

{-- Hooks --}

import XMonad.Actions.Promote (promote)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)

{-- System --}
import System.Environment (getEnv)
import System.IO.Unsafe (unsafeDupablePerformIO)
import XMonad.Actions.TopicSpace (Dir, Topic, TopicConfig (defaultTopic, defaultTopicAction, topicActions, topicDirs), TopicItem (TI), currentTopicAction, currentTopicDir, inHome, noAction, switchTopic, tiActions, tiDirs, topicNames)
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doFullFloat)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.SubLayouts (GroupMsg (UnMerge), pullGroup, subLayout, subTabbed)
import XMonad.Layout.Tabbed (Theme (activeBorderColor, activeBorderWidth, activeColor, activeTextColor, decoHeight, decoWidth, fontName, inactiveBorderColor, inactiveBorderWidth, inactiveColor, inactiveTextColor), addTabs, shrinkText, tabbed)
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Prompt (XPConfig (alwaysHighlight, autoComplete, bgColor, bgHLight, borderColor, fgColor, fgHLight, font, height, position, searchPredicate, sorter), XPPosition (Top))
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.OrgMode (orgPrompt)
import XMonad.Prompt.Workspace (workspacePrompt)
import XMonad.Util.Hacks qualified as Hacks
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadAction, namedScratchpadManageHook)
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.MultiToggle (mkToggle, EOT (EOT), (??), Toggle (Toggle))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import XMonad.Layout.Gaps (GapMessage(ToggleGaps))

{-- VARIABLES:
 - Define some basic settings for XMonad here. This includes the modifier keys, default terminal emulator, window borders, etc. --}
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "st"

myLauncher :: String
myLauncher = "rofi -show drun -sorting-method fzf -sort"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor = "#0f0f0f"

myFocusColor :: String
myFocusColor = "#FFFFFF"

myEditor :: String
myEditor = "nvim"

{-- KEYBINDINGS:
 - The following keybindings are to be used with the `additionalKeysP` function
 - provided by XMonad.Util.EZConfig to provide a simpler syntax --}

-- Switch to a certain layout.
switchToLayout :: String -> X ()
switchToLayout = sendMessage . JumpToLayout

myKeys :: [(String, X ())]
myKeys =
  [ ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  , ("M-<Return>", spawn myTerminal)
  , ("M-r", spawn myLauncher)
  , ("M-w", spawn "rofi -show window")
  , ("M-S-c", spawn "rofi -show calc")
  , ("M-o", spawn "rofi -show power-menu -modi power-menu:~/.local/bin/rofi-power-menu")
  , ("M-q", kill)
  , ("M-t f", sendMessage $ Toggle NBFULL)
  , ("M-t b", sendMessage ToggleGaps >> spawn "polybar-msg cmd toggle")
  , ("M-s t", namedScratchpadAction myScratchpads "terminal")
  , ("M-s b", namedScratchpadAction myScratchpads "btop")
  , ("M-s i", namedScratchpadAction myScratchpads "irc")
  , ("M-s f", namedScratchpadAction myScratchpads "fm")
  , ("M-s s", namedScratchpadAction myScratchpads "signal")
  , ("M-s m", namedScratchpadAction myScratchpads "music")
  , ("M-s n", namedScratchpadAction myScratchpads "notes")
  , ("M-p",   spawn "flameshot gui")
  , ("M-S-p", promote)
  -- Layout keybinds
  , ("M-; t", switchToLayout "Spacing Tabbed Tall")
  , ("M-; w", switchToLayout "Mirror Spacing Tall")
  , ("M-; f", switchToLayout "monocle")
  , ("M-; 3", switchToLayout "Spacing ThreeCol")
  , ("M-; a", switchToLayout "Tabbed Simplest")
  , ("M-S-a", sendMessage $ pullGroup L)
  , ("M-S-d", sendMessage $ pullGroup R)
  , ("M-S-w", sendMessage $ pullGroup U)
  , ("M-S-s", sendMessage $ pullGroup D)
  , ("M-S-u", withFocused (sendMessage . UnMerge))
  ]

{- Scratchpads -}
myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "terminal" spawnTerm findTerm manageTerm
  , NS "btop" spawnBtop findBtop manageBtop
  , NS "irc" spawnIrc findIrc manageIrc
  , NS "fm" spawnFM findFM manageFM
  , NS "notes" spawnNotes findNotes manageNotes
  , NS "signal" spawnSignal findSignal manageSignal
  , NS "music" spawnMusic findMusic manageMusic
  ]
 where
  spawnTerm = "st -c scratchpad"
  findTerm = className =? "scratchpad"
  manageTerm = customFloating $ W.RationalRect l t w h
   where
    h = 0.9
    w = 0.9
    t = 0.95 - h
    l = 0.95 - w
  spawnBtop = "st -c btop -e btop"
  findBtop = className =? "btop"
  manageBtop = customFloating $ W.RationalRect l t w h
   where
    h = 0.9
    w = 0.9
    t = 0.95 - h
    l = 0.95 - w
  spawnIrc = "st -c irc -e weechat"
  findIrc = className =? "irc"
  manageIrc = customFloating $ W.RationalRect l t w h
   where
    h = 0.9
    w = 0.9
    t = 0.95 - h
    l = 0.95 - w
  spawnFM = "st -c fm -e yazi"
  findFM = className =? "fm"
  manageFM = customFloating $ W.RationalRect l t w h
   where
    h = 0.9
    w = 0.9
    t = 0.95 - h
    l = 0.95 - w
  spawnNotes = "st -c notes -e tmux new-session -s notes -c ~/Documents/Episteme/"
  findNotes = className =? "notes"
  manageNotes = customFloating $ W.RationalRect l t w h
   where
    h = 0.9
    w = 0.9
    t = 0.95 - h
    l = 0.95 - w
  spawnSignal = "signal-desktop"
  findSignal = className =? "Signal"
  manageSignal = customFloating $ W.RationalRect l t w h
   where
    h = 0.9
    w = 0.9
    t = 0.95 - h
    l = 0.95 - w
  spawnMusic = "st -c music -e ncmpcpp"
  findMusic = className =? "music"
  manageMusic = customFloating $ W.RationalRect l t w h
   where
    h = 0.9
    w = 0.9
    t = 0.95 - h
    l = 0.95 - w

myTabConfig :: Theme
myTabConfig =
  def
    { activeColor = "#C9D4FF"
    , activeBorderColor = "#C9D4FF"
    , activeTextColor = "#000000"
    , activeBorderWidth = 0
    , inactiveColor = "#000000"
    , inactiveBorderColor = "#000000"
    , inactiveTextColor = "#ffffff"
    , inactiveBorderWidth = 2
    , fontName = "xft:JetBrainsMono Nerd Font:size=10:antialias=true:hinting=true"
    , decoHeight = 12
    , decoWidth = maxBound
    }

myLayout = avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ tiled ||| Mirror tiled ||| monocle ||| threeCol ||| tabs
 where
  {-- Here are some custom layouts --}
  tabs = tabbed shrinkText myTabConfig
  tiled = spacing gaps $ windowNavigation $ subTabbed $ boringWindows $ Tall nmaster delta ratio
  threeCol = spacing gaps $ ThreeColMid nmaster delta ratio
  monocle = renamed [Replace "monocle"]
            $ smartBorders
            $ windowNavigation
            $ addTabs shrinkText myTabConfig
            $ subLayout [] (smartBorders Simplest) Full

  {-- and here are some general configurations for all of these layouts. --}
  nmaster = 1     -- Default number of windows in the master pane
  ratio = 1 / 2   -- Default proportion of screen occupied by master pane
  delta = 1 / 100 -- Percent of screen to increment by when resizing panes
  gaps = 8        -- Size of window gaps

{-- PROMPT --}
myXPConfig :: XPConfig
myXPConfig =
  def
    { font = "xft:JetBrainsMono Nerd Font:size=10:antialias=true:hinting=true"
    , bgColor = "#000000"
    , fgColor = "#FFFFFF"
    , bgHLight = "#C9D4FF"
    , fgHLight = "#000000"
    , borderColor = "#000000"
    , position = Top
    , -- , autoComplete = Just 100000 -- This is what's causing prompt entries to automatically execute
      height = 32
    , searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    , alwaysHighlight = True
    }

{-- MANAGE HOOK --}
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ isDialog                    --> doFloat
    , isFullscreen                --> doFullFloat
    , className =? "confirm"      --> doFloat
    , className =? "file_progress"--> doFloat
    , className =? "dialog"       --> doFloat
    , className =? "download"     --> doFloat
    , className =? "error"        --> doFloat
    , className =? "firefox"      --> doShift "1"
    , className =? "FreeTube"     --> doShift "3"
    , className =? "vesktop"      --> doShift "2"
    , className =? "Element"      --> doShift "2"
    , className =? "thunderbird"  --> doShift "4"
    , className =? "steam"        --> doShift "6"
    ]
    <+> namedScratchpadManageHook myScratchpads

{-- STARTUP --}
myStartupHook :: X ()
myStartupHook = do
  -- proper monitor layout
  spawnOnce "xrandr --output DisplayPort-1 --mode 1920x1080 --rate 165 --primary --output DisplayPort-0 --left-of DisplayPort-1"
  spawnOnce "picom -b"
  spawnOnce "redshift -l -33.9166485:151.2233364"
  spawnOnce "dunst"
  spawnOnce "flameshot"
  spawn "~/.local/bin/polybar-xmonad.sh"
  spawn "killall conky"
  spawn "hsetroot -cover ~/.config/wallpaper.png"

myConfig =
  def
    { modMask = myModMask
    , terminal = myTerminal
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormColor
    , focusedBorderColor = myFocusColor
    , layoutHook = myLayout
    , startupHook = myStartupHook
    , manageHook = myManageHook <+> manageDocks
    , handleEventHook =
        handleEventHook def
          <> Hacks.trayerPaddingXmobarEventHook
    }
    `additionalKeysP` myKeys

main :: IO ()
main =
  do
    xmonad
    . docks
    . ewmh
    . ewmhFullscreen
    . fullscreenSupport
    $ myConfig
