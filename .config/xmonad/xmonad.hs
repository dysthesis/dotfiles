{-- PRAGMAS --}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans  #-}

import XMonad
import qualified XMonad.StackSet as W

{-- Utilities --}
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce ( spawnOnce )
import XMonad.Util.ClickableWorkspaces ( clickablePP )

{-- Layouts --}
import XMonad.Layout.ThreeColumns ( ThreeCol(ThreeColMid) )
import XMonad.Layout.Spacing ( spacing )
import XMonad.Layout.Fullscreen ( fullscreenSupport )

{-- Hooks --}
import XMonad.Hooks.StatusBar
    ( statusBarProp, withEasySB, StatusBarConfig, statusBarPropTo, withSB )
import XMonad.Hooks.StatusBar.PP
    ( PP(ppLayout, ppCurrent, ppVisibleNoWindows, ppVisible, ppHidden,
         ppHiddenNoWindows, ppUrgent, ppTitle, ppSep, ppTitleSanitize,
         ppWsSep),
      wrap,
      xmobarColor,
      xmobarFont,
      xmobarStrip, shorten )
import XMonad.Hooks.EwmhDesktops ( ewmh, ewmhFullscreen )
import XMonad.Hooks.ManageDocks ( docks, avoidStruts, manageDocks )

import XMonad.Actions.Promote ( promote )

{-- System --}
import System.Environment (getEnv)
import System.IO.Unsafe (unsafeDupablePerformIO)
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadManageHook, namedScratchpadAction)
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.Prompt (XPConfig (font, bgColor, fgColor, bgHLight, fgHLight, position, borderColor, autoComplete, height, sorter, searchPredicate, alwaysHighlight), XPPosition (Top))
import XMonad.Prompt.OrgMode (orgPrompt)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Layout.Tabbed (Theme (activeColor, activeBorderColor, activeTextColor, activeBorderWidth, inactiveColor, inactiveTextColor, inactiveBorderColor, inactiveBorderWidth, fontName, decoHeight, decoWidth), shrinkText, tabbed)
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Actions.TopicSpace (TopicItem (TI), inHome, Dir, currentTopicDir, TopicConfig (topicDirs, defaultTopicAction, topicActions, defaultTopic), tiDirs, tiActions, Topic, switchTopic, currentTopicAction, topicNames, noAction)
import XMonad.Prompt.Workspace (workspacePrompt)
import XMonad.Actions.Search (promptSearch, hoogle, scholar, SearchEngine, searchEngine)
import XMonad.Util.Ungrab ( unGrab )
{-- VARIABLES:
 - Define some basic settings for XMonad here. This includes the modifier keys, default terminal emulator, window borders, etc. --}
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "wezterm"

myLauncher :: String
myLauncher = "rofi -show drun"

myBorderWidth :: Dimension
myBorderWidth = 3

myNormColor :: String
myNormColor = "#0f0f0f"

myFocusColor :: String
myFocusColor = "#FFFFFF"

{- My XMobar directory -}
myHomeDir :: String
myHomeDir = unsafeDupablePerformIO (getEnv "HOME")

-- myXmobar :: String
-- myXmobar = "xmobar " ++ myHomeDir ++ "/.config/xmobar/xmobar.hs"

myEditor :: String
myEditor = "emacsclient -c -a 'emacs'"

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
  , ("M-<Backspace>", spawn "~/.config/rofi/scripts/rofi-powermenu.sh")
  , ("M-q", kill)
  , ("<XF86AudioLowerVolume>", spawn "~/.config/scripts/volume.sh d")
  , ("<XF86AudioRaiseVolume>", spawn "~/.config/scripts/volume.sh i")
  , ("M-e", spawn myEditor)
  , ("M-f f", spawn "firefox")
  , ("M-f u", spawn "firefox --profile /home/apollyon/.mozilla/firefox/University")
  , ("M-s t", namedScratchpadAction myScratchpads "terminal")
  , ("M-s b", namedScratchpadAction myScratchpads "btop")
  , ("M-s i", namedScratchpadAction myScratchpads "irc")
  , ("M-z", namedScratchpadAction myScratchpads "signal")
  , ("M-a", namedScratchpadAction myScratchpads "agenda")
  , ("M-s m", namedScratchpadAction myScratchpads "music")
  , ("M-p", unGrab *> spawn "flameshot gui")
  , ("M-c", orgPrompt myXPConfig "TODO" "~/Org/GTD/inbox.org")
  , ("M-S-p", promote)
  -- Layout keybinds
  , ("M-; t", switchToLayout "Spacing Tall")
  , ("M-; w", switchToLayout "Mirror Spacing Tall")
  , ("M-; f", switchToLayout "Full")
  , ("M-; 3", switchToLayout "Spacing ThreeCol")
  , ("M-; a", switchToLayout "Tabbed Simplest")
  -- TopicSpaces
  , ("M-n"        , spawnShell)
  , ("M-S-a"      , currentTopicAction myTopicConfig)
  , ("M-g"        , promptedGoto)
  , ("M-S-g"      , promptedShift)

  -- Search
  , ("M-f g", promptSearch myXPConfig gentooWiki)
  , ("M-f a", promptSearch myXPConfig archWiki)
  , ("M-f h", promptSearch myXPConfig hoogle)
  , ("M-f s", promptSearch myXPConfig scholar)
  ]

{- Topics -}
topicItems :: [TopicItem]
topicItems =
  [ inHome  "1:WEB"   (spawn "firefox")
  , TI      "2:UNI"   "Documents/University"    (  spawn "firefox --profile /home/apollyon/.mozilla/firefox/University"
                                                *> spawn myEditor)
  , inHome  "3:CHAT"                            (  spawn "flatpak run io.github.spacingbat3.webcord"
                                                *> spawn "element-desktop")
  , inHome  "4:VID"                             (  spawn "freetube-bin")
  , inHome  "5:MAIL"                            (  spawn "thunderbird")
  , TI            "6:DOTS"  ".config"                    spawnShell
  , noAction      "7:READ"  "Documents/Books"
  , inHome  "8:VIRT"                            (  spawn "virt-manager")
  , TI      "9:DL"    "Download"                (  spawn "nicotine")
  , inHome  "10:GAME"                           (  spawn "steam")
  ]


myTopicConfig :: TopicConfig
myTopicConfig = def
  { topicDirs          = tiDirs    topicItems
  , topicActions       = tiActions topicItems
  , defaultTopicAction = const (pure ()) -- by default, do nothing
  , defaultTopic       = "1:WEB"         -- fallback
  }

-- Helper functions relevant to topics
spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "wezterm start --always-new-process --cwd" ++ dir

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

{- Search engines -}

archWiki :: SearchEngine
archWiki = searchEngine "Arch Linux Wiki" "https://wiki.archlinux.org/index.php?title=Special:Search&search="

gentooWiki :: SearchEngine
gentooWiki = searchEngine "Gentoo Linux Wiki" "https://wiki.gentoo.org/index.php?title=Special:Search&search="

{- Scratchpads -}
myScratchpads :: [NamedScratchpad]
myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "btop" spawnBtop findBtop manageBtop
                , NS "irc" spawnIrc findIrc manageIrc
                , NS "signal" spawnSignal findSignal manageSignal
                , NS "agenda" spawnAgenda findAgenda manageAgenda
                , NS "music" spawnMusic findMusic manageMusic]
  where
    spawnTerm = "wezterm start --class scratchpad"
    findTerm = className =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w
    spawnBtop = "wezterm start --class btop -- btop"
    findBtop = className =? "btop"
    manageBtop = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w
    spawnIrc = "wezterm start --class irc -- weechat"
    findIrc = className =? "irc"
    manageIrc = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w
    spawnSignal = "signal-desktop"
    findSignal = className =? "Signal"
    manageSignal = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w
    spawnAgenda = "emacsclient -c -a 'emacs' --eval '(dysthesis/agenda)' --frame-parameters='(quote (name . \"agenda-scratchpad\"))'"
    findAgenda = title =? "agenda-scratchpad"
    manageAgenda = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w
    spawnMusic = "wezterm start --class music -- ncmpcpp"
    findMusic = className =? "music"
    manageMusic = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w

myTabConfig :: Theme
myTabConfig = def
  { activeColor = "#C9D4FF"
  , activeBorderColor = "#C9D4FF"
  , activeTextColor = "#000000"
  , activeBorderWidth = 0
  , inactiveColor = "#000000"
  , inactiveBorderColor = "#000000"
  , inactiveTextColor = "#ffffff"
  , inactiveBorderWidth = 2
  , fontName = "xft:JetBrainsMono Nerd Font:size=10:antialias=true:hinting=true"
  , decoHeight = 24
  , decoWidth = maxBound
  }

myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| threeCol ||| tabs
  where
    tabs = tabbed shrinkText myTabConfig
    tiled   = spacing gaps $ Tall nmaster delta ratio
    threeCol = spacing gaps $ ThreeColMid nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
    gaps    = 8      -- Size of window gaps

{-- PROMPT --}
myXPConfig :: XPConfig
myXPConfig = def
  { font = "xft:JetBrainsMono Nerd Font:size=10:antialias=true:hinting=true"
  , bgColor = "#000000"
  , fgColor = "#FFFFFF"
  , bgHLight = "#C9D4FF"
  , fgHLight = "#000000"
  , borderColor = "#000000"
  , position = Top
  -- , autoComplete = Just 100000 -- This is what's causing prompt entries to automatically execute
  , height = 32
  , searchPredicate = fuzzyMatch
  , sorter = fuzzySort
  , alwaysHighlight = True
  }

{-- XMOBAR --}
xmobar0 = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0 ~/.config/xmobar/xmobar0.hs" (pure myXmobarPP)
xmobar1 = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 1 ~/.config/xmobar/xmobar1.hs" (pure myXmobarPP)

myXmobarPP :: PP
myXmobarPP = def
    { ppCurrent          = xmobarColor "#CEACE8" "#0f0f0f:5" . xmobarFont 4
    , ppVisibleNoWindows = Just (xmobarColor "#A9B1D6" "#0f0f0f:5")
    , ppVisible          = xmobarColor "#A3CBE7" "#0f0f0f:5"
    , ppHidden           = xmobarColor "#DDE8FF" "#0f0f0f:5"
    -- , ppHiddenNoWindows  = xmobarColor "#6B7089" "#0f0f0f:5"  -- Uncomment this line to also show hidden workspaces without windows.
    , ppUrgent           = xmobarColor "#E5A3A1" "#0f0f0f:5" . wrap "!" "!"
    , ppTitle            = xmobarColor "#ffffff" "#0f0f0f:5" {- . shorten 49 -}
    , ppSep              = wrapSep " "
    , ppTitleSanitize    = xmobarStrip . shorten 30
    , ppWsSep            = xmobarColor "" "#0f0f0f:5" "   "
    , ppLayout           = xmobarColor "#B4E3AD" "#0f0f0f:5"
    -- , ppLayout           = xmobarColor "#0f0f0f" ""
    --                        . (\case
    --                            "Spacing Tall"        -> "<icon=tiled.xpm/>"
    --                            "Mirror Spacing Tall" -> "<icon=mirrortiled.xpm/>"
    --                            "Full"        -> "<icon=full.xpm/>"
    --                          )
    }
    where
        wrapSep :: String -> String
        wrapSep = wrap 
            (xmobarColor "#0f0f0f" "#000000:6" (xmobarFont 2 "\xe0b4"))
            (xmobarColor "#0f0f0f" "#000000:6" (xmobarFont 2 "\xe0b6"))

{-- MANAGE HOOK --}
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp"             --> doFloat
    , isDialog                        --> doFloat
    , className =? "FreeTube"         --> doShift "4:VID"
    , className =? "WebCord"          --> doShift "3:CHAT"
    , className =? "Element"          --> doShift "3:CHAT"
    , className =? "thunderbird"      --> doShift "5:MAIL"
    , className =? "steam"            --> doShift "10:GAME"
    ] <+> namedScratchpadManageHook myScratchpads

{-- STARTUP --}
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "~/.config/scripts/autostart.sh"
  spawnOnce "sleep 1 && trayer-srg --monitor primary --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x000000 --height 28"

myConfig = def
  { modMask = myModMask
  , terminal = myTerminal
  , borderWidth = myBorderWidth
  , normalBorderColor = myNormColor
  , focusedBorderColor = myFocusColor
  , layoutHook = myLayout
  , startupHook = myStartupHook
  , workspaces = topicNames topicItems
  , manageHook = myManageHook <+> manageDocks
  , handleEventHook = handleEventHook def
                 <> Hacks.trayerPaddingXmobarEventHook
  } `additionalKeysP` myKeys

main :: IO ()
main = do
  xmonad
  . docks
  . ewmh
  . ewmhFullscreen
  . fullscreenSupport
  $ withSB (xmobar0 <> xmobar1)
  myConfig
