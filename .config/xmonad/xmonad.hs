{-# LANGUAGE LambdaCase #-}

import XMonad
import qualified XMonad.StackSet as W

{-- Utilities --}
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.EZConfig (additionalKeysP)
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadAction, namedScratchpadManageHook, scratchpadWorkspaceTag)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Types (Direction2D (D, L, R, U))

{-- Layouts --}

import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.Gaps (GapMessage (ToggleGaps))
import XMonad.Layout.MultiToggle (EOT (EOT), Toggle (Toggle), mkToggle, (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.SubLayouts (GroupMsg (UnMerge), pullGroup, subLayout, subTabbed)
import XMonad.Layout.Tabbed (Theme (activeBorderColor, activeBorderWidth, activeColor, activeTextColor, decoHeight, decoWidth, fontName, inactiveBorderColor, inactiveBorderWidth, inactiveColor, inactiveTextColor), addTabs, shrinkText, tabbed)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.WindowNavigation (windowNavigation)

{-- Hooks --}
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)

{-- System --}
import System.Environment (getEnv)
import System.Process (callCommand, readProcess)
import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen)
import XMonad.Actions.DynamicProjects (Project (Project, projectDirectory, projectName, projectStartHook), dynamicProjects, switchProjectPrompt)
import XMonad.Actions.Search (SearchEngine, hackage, hoogle, promptSearch, searchEngine)
import XMonad.Hooks.DynamicLog (PP (ppSort), xmobarPP)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen, isInProperty)
import XMonad.Hooks.StatusBar (StatusBarConfig, dynamicSBs, statusBarProp, statusBarPropTo, withEasySB)
import XMonad.Hooks.StatusBar.PP (PP (ppCurrent, ppExtras, ppHidden, ppLayout, ppOrder, ppOutput, ppSep, ppTitle, ppTitleSanitize, ppUrgent, ppVisible, ppVisibleNoWindows, ppWsSep), filterOutWsPP, shorten, wrap, xmobarBorder, xmobarColor, xmobarFont, xmobarStrip)
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Layout.BinarySpacePartition (ResizeDirectional (..), Rotate (Rotate), Swap (Swap), emptyBSP)
import qualified XMonad.Layout.Renamed as XLR
import XMonad.Prompt (XPConfig (alwaysHighlight, autoComplete, bgColor, bgHLight, borderColor, fgColor, fgHLight, font, height, position, searchPredicate, sorter), XPPosition (Bottom))
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Input (inputPrompt, (?+))
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)

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

-- Helper function to switch to a certain layout.
switchToLayout :: String -> X ()
switchToLayout = sendMessage . JumpToLayout

myKeys :: [(String, X ())]
myKeys =
    [ ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
    , ("M-<Return>", spawn myTerminal)
    , ("M-r", spawn myLauncher)
    , ("M-q", kill)
    , ("M-t f", sendMessage $ Toggle NBFULL)
    , ("M-t b", sendMessage ToggleGaps >> spawn "polybar-msg cmd toggle")
    , ("M-n", nextScreen)
    , ("M-S-n", shiftNextScreen)
    , -- Scratchpads
      ("M-s t", namedScratchpadAction myScratchpads "terminal")
    , ("M-s b", namedScratchpadAction myScratchpads "btop")
    , ("M-s i", namedScratchpadAction myScratchpads "irc")
    , ("M-s f", namedScratchpadAction myScratchpads "fm")
    , ("M-s s", namedScratchpadAction myScratchpads "signal")
    , ("M-s n", namedScratchpadAction myScratchpads "notes")
    , ("M-s v", namedScratchpadAction myScratchpads "vit")
    , ("M-s c", namedScratchpadAction myScratchpads "khal")
    , ("M-p", spawn "flameshot gui")
    , ("M-a t", taskPrompt myXPConfig)
    , ("M-f a", promptSearch myXPConfig archWiki)
    , ("M-f g", promptSearch myXPConfig gentooWiki)
    , ("M-f h", promptSearch myXPConfig hackage)
    , ("M-f o", promptSearch myXPConfig hoogle)
    , ("M-f b", promptSearch myXPConfig braveSearch)
    , -- Layout keybinds
      ("M-; t", switchToLayout "Spacing Tabbed Tall")
    , ("M-; w", switchToLayout "Mirror Spacing Tall")
    , ("M-; f", switchToLayout "monocle")
    , ("M-; 3", switchToLayout "Spacing ThreeCol")
    , ("M-; a", switchToLayout "Tabbed Simplest")
    , ("M-; b", switchToLayout "BSP")
    , ("M-S-a", sendMessage $ pullGroup L)
    , ("M-S-d", sendMessage $ pullGroup R)
    , ("M-S-w", sendMessage $ pullGroup U)
    , ("M-S-s", sendMessage $ pullGroup D)
    , ("M-S-u", withFocused (sendMessage . UnMerge))
    , ("M-M1-h", sendMessage $ ExpandTowards L)
    , ("M-M1-l", sendMessage $ ShrinkFrom L)
    , ("M-M1-k", sendMessage $ ExpandTowards U)
    , ("M-M1-j", sendMessage $ ShrinkFrom U)
    , ("M-M1-C-h", sendMessage $ ShrinkFrom R)
    , ("M-M1-C-l", sendMessage $ ExpandTowards R)
    , ("M-M1-C-k", sendMessage $ ShrinkFrom D)
    , ("M-M1-C-j", sendMessage $ ExpandTowards D)
    , ("M-M1-S", sendMessage Swap)
    , ("M-M1-s", sendMessage Rotate)
    , -- Projects
      ("M-g p", switchProjectPrompt myXPConfig)
    ]

{-- Search engines --}
archWiki :: SearchEngine
archWiki = searchEngine "Arch Linux Wiki" "https://wiki.archlinux.org/index.php?search="

gentooWiki :: SearchEngine
gentooWiki = searchEngine "Gentoo Linux Wiki" "https://wiki.gentoo.org/index.php?title=Special:Search&search="

braveSearch :: SearchEngine
braveSearch = searchEngine "Brave Search" "https://search.brave.com/search?q="

{-- Scratchpads --}
myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ NS "terminal" spawnTerm findTerm manageTerm
    , NS "btop" spawnBtop findBtop manageBtop
    , NS "irc" spawnIrc findIrc manageIrc
    , NS "fm" spawnFM findFM manageFM
    , NS "notes" spawnNotes findNotes manageNotes
    , NS "signal" spawnSignal findSignal manageSignal
    , NS "vit" spawnVit findVit manageVit
    , NS "khal" spawnKhal findKhal manageKhal
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
    spawnNotes = "st -c notes -e tmux new-session -s notes -c ~/Documents/Notes/"
    findNotes = className =? "notes"
    manageNotes = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnVit = "st -c vit -e vit"
    findVit = className =? "vit"
    manageVit = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnKhal = "st -c khal -e ikhal"
    findKhal = className =? "khal"
    manageKhal = customFloating $ W.RationalRect l t w h
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

{-- Projects --}
myProjects :: [Project]
myProjects =
    [ Project
        { projectName = "Browser"
        , projectDirectory = "~/"
        , projectStartHook = Just $ do spawn "firefox"
        }
    , Project
        { projectName = "Messaging"
        , projectDirectory = "~/"
        , projectStartHook = Just $ do
            spawn "vesktop-bin"
            spawn "element-desktop"
        }
    ]

{-- XMobar --}
-- Create a command to spawn xmobar for the given screen ID
-- xmobarCmd :: ScreenId -> String
-- xmobarCmd screen = "xmobar ~/.config/xmobar/xmobar.hs -x " ++ show screen
--
-- -- Spawn an xmobar for the given screen
-- barSpawner :: ScreenId -> IO StatusBarConfig
-- barSpawner screen = pure $ statusBarProp (xmobarCmd screen) (pure myXmobarPP)

xmobarProp = withEasySB (statusBarProp "xmobar -x 0 ~/.config/xmobar/xmobar.hs" (pure (filterOutWsPP [scratchpadWorkspaceTag] myXmobarPP))) toggleStrutsKey
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{modMask = m} = (m, xK_b)

myXmobarPP :: PP
myXmobarPP =
    def
        { ppSep = grey "  \xf01d9  "
        , ppCurrent = blue . wrap "" "" . xmobarBorder "Top" "#89b4fa" 3
        , -- , ppCurrent = blue
          ppHidden = grey
        , ppVisible = white
        , ppWsSep = "  "
        , ppTitleSanitize = xmobarStrip . shorten 30 -- `shorten` defines the max length
        , ppTitle = wrap "\xf0570 " ""
        , ppLayout =
            white
                . ( \case
                        "Spacing Tabbed Tall" -> "<icon=tiled.xpm/>"
                        "Mirror Spacing Tabbed Tall" -> "<icon=mirrortiled.xpm/>"
                        "Full" -> "<icon=full.xpm/>"
                        "monocle" -> "<icon=monocle.xpm/>"
                        "Spacing ThreeCol" -> "<icon=threecol.xpm/>"
                        "Tabbed Simplest" -> "<icon=tabbed.xpm/>"
                        "BSP" -> "<icon=bsp.xpm/>"
                  )
        }
  where
    grey = xmobarColor "#6c7086" ""
    white = xmobarColor "#ffffff" ""
    blue = xmobarColor "#89b4fa" ""

{-- Tabbed layouts --}
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

-- Here, `mkToggle` (NBFULL ?? NOBORDERS ?? EOT) is used to enable fullscreen toggling.
myLayout = avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ bsp ||| tiled ||| Mirror tiled ||| monocle ||| threeCol ||| tabs
  where
    {-- Here are some custom layouts --}
    tabs = tabbed shrinkText myTabConfig
    tiled = spacing gaps $ windowNavigation $ subTabbed $ boringWindows $ Tall nmaster delta ratio
    threeCol = spacing gaps $ ThreeColMid nmaster delta ratio
    bsp =
        renamed [XLR.Replace "BSP"] $
            avoidStruts $
                windowNavigation $
                    addTabs shrinkText myTabConfig $
                        subLayout [] tabs $
                            spacing gaps emptyBSP
    monocle =
        renamed [Replace "monocle"] $
            smartBorders $
                windowNavigation $
                    addTabs shrinkText myTabConfig $
                        subLayout [] (smartBorders Simplest) Full

    {-- and here are some general configurations for all of these layouts. --}
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 1 / 100 -- Percent of screen to increment by when resizing panes
    gaps = 8 -- Size of window gaps

{-- PROMPT --}
myXPConfig :: XPConfig
myXPConfig =
    def
        { font = "xft:JetBrainsMono Nerd Font:size=10:antialias=true:hinting=true"
        , bgColor = "#000000"
        , fgColor = "#FFFFFF"
        , bgHLight = "#89b4fa"
        , fgHLight = "#000000"
        , borderColor = "#000000"
        , position = Bottom
        , height = 34
        , searchPredicate = fuzzyMatch
        , sorter = fuzzySort
        , alwaysHighlight = True
        }

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
        <+> namedScratchpadManageHook myScratchpads
  where
    isRole = stringProperty "WM_WINDOW_ROLE"
    isFileChooserDialog = isRole =? "GtkFileChooserDialog"
    isPopup = isRole =? "pop-up"
    isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
    isGtk4Dialog = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"
    isGtk4Modal = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_MODAL"

{-- Prompts --}
addTaskAndNotify :: String -> X ()
addTaskAndNotify task = do
    let command = "task add " ++ task
    output <- io $ readProcess "sh" ["-c", command] ""
    io $ callCommand $ "notify-send 'Task Added' " ++ show output
taskPrompt :: XPConfig -> X ()
taskPrompt config = inputPrompt config "Task" ?+ addTaskAndNotify

{-- STARTUP --}
myStartupHook :: X ()
myStartupHook = do
    -- proper monitor layout
    spawnOnce "xrandr --output DisplayPort-1 --mode 1920x1080 --rate 165 --primary --output DisplayPort-0 --left-of DisplayPort-1"
    -- spawnOnce "picom -b"
    spawnOnce "redshift -l -33.9166485:151.2233364"
    spawnOnce "dunst"
    spawnOnce "parcellite"
    spawnOnce "flameshot"
    spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
    -- spawn "~/.local/bin/polybar-xmonad.sh"
    spawn "killall conky"
    spawn "hsetroot -cover ~/.config/wallpaper.png"

myWorkspaces :: [String]
myWorkspaces = [" \xf269 ", " \xf0b79 ", " \xf16a ", " \xf4b5 ", " \xeb1c ", " \xea7a ", " \xf1b6 ", " \xf03bd ", " \xf0feb "]

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
        $ xmobarProp
        $ dynamicProjects
            myProjects
            myConfig
