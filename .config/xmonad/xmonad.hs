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
import XMonad.Actions.Search (SearchEngine, hackage, hoogle, promptSearch, searchEngine, rustStd, cratesIo, alpha, aur, stackage)
import XMonad.Hooks.DynamicLog (PP (ppSort), xmobarPP)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen, isInProperty)
import XMonad.Hooks.StatusBar (StatusBarConfig, dynamicSBs, statusBarProp, statusBarPropTo, withEasySB)
import XMonad.Hooks.StatusBar.PP (PP (ppCurrent, ppExtras, ppHidden, ppLayout, ppOrder, ppOutput, ppSep, ppTitle, ppTitleSanitize, ppUrgent, ppVisible, ppVisibleNoWindows, ppWsSep), filterOutWsPP, shorten, wrap, xmobarBorder, xmobarColor, xmobarFont, xmobarStrip)
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Layout.BinarySpacePartition (ResizeDirectional (..), Rotate (Rotate), Swap (Swap), emptyBSP)
import qualified XMonad.Layout.Renamed as XLR
import XMonad.Prompt (XPConfig (alwaysHighlight, autoComplete, bgColor, bgHLight, borderColor, fgColor, fgHLight, font, height, historySize, maxComplRows, position, searchPredicate, sorter), XPPosition (Bottom))
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Input (inputPrompt, (?+))
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)
import Data.Maybe (fromMaybe)

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

    {-- XMONAD --}

    [ ("M-S-r", spawn "xmonad --recompile && xmonad --restart")

    -- Layout keybinds
    , ("M-; t", switchToLayout "Spacing Tabbed Tall")
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

    {-- DESKTOP --}
    , ("M-r", spawn myLauncher)
    , ("M-<Return>", spawn myTerminal)
    , ("M-q", kill)
    , ("M-n", nextScreen)
    , ("M-S-n", shiftNextScreen)
    , ("M-t f", sendMessage $ Toggle NBFULL)
    , ("<XF86AudioRaiseVolume>", spawn increaseVolCmd)
    , ("<XF86AudioLowerVolume>", spawn decreaseVolCmd)
    , ("M-p", spawn "flameshot gui")
    , ("M-t b", sendMessage ToggleGaps >> spawn "polybar-msg cmd toggle")

    {-- SCRATCHPADS --}

    , ("M-s t", namedScratchpadAction myScratchpads "terminal")
    , ("M-s b", namedScratchpadAction myScratchpads "btop")
    , ("M-s i", namedScratchpadAction myScratchpads "irc")
    , ("M-s f", namedScratchpadAction myScratchpads "fm")
    , ("M-s s", namedScratchpadAction myScratchpads "signal")
    , ("M-s n", namedScratchpadAction myScratchpads "notes")
    , ("M-s c", namedScratchpadAction myScratchpads "khal")

    {-- TASKWARRIOR --}

    , ("M-a t", taskPrompt myXPConfig)

    {-- SEARCH KEYBINDS --}

    -- Miscelaneous
    , ("M-f w", promptSearch myXPConfig alpha)

    -- General
    , ("M-f b", promptSearch myXPConfig braveSearch)
    , ("M-f s", promptSearch myXPConfig searx)

    -- Projects
    , ("M-g p", switchProjectPrompt myXPConfig)
    ]
    -- Append search engines to the keybinding list
    ++ [("M-f r " ++ prefix, promptSearch myXPConfig engine) | (prefix, engine) <- rustSearchList]
    ++ [("M-f h " ++ prefix, promptSearch myXPConfig engine) | (prefix, engine) <- haskellSearchList]
    ++ [("M-f a " ++ prefix, promptSearch myXPConfig engine) | (prefix, engine) <- archSearchList]
    ++ [("M-f g " ++ prefix, promptSearch myXPConfig engine) | (prefix, engine) <- gentooSearchList]
    where
      audioDelta = 5 -- configures how much each command should change the volume by
      increaseVolCmd = "wpctl set-volume @DEFAULT_AUDIO_SINK@ " ++ show audioDelta ++ "%+"
      decreaseVolCmd = "wpctl set-volume @DEFAULT_AUDIO_SINK@ " ++ show audioDelta ++ "%-"

{-- Search engines --}
type SearchList =
  [( String         -- Keybind predicate
   , SearchEngine   -- Search engine to use
   )]

haskellSearchList :: SearchList
haskellSearchList =
  [ ("h", hoogle)
  , ("w", haskellWiki)
  , ("s", stackage)
  ]
  where
    haskellWiki = searchEngine "Haskell Wiki" "https://wiki.haskell.org/index.php?title=Special:Search&search="

rustSearchList :: SearchList
rustSearchList =
  [ ("s", rustStd)
  , ("c", cratesIo)
  ]

archSearchList :: SearchList
archSearchList =
  [ ("w", archWiki)
  , ("u", aur)
  , ("p", archRepo)
  ]
  where
    archWiki = searchEngine "Arch Linux Wiki" "https://wiki.archlinux.org/index.php?search="
    archRepo = searchEngine "Arch Linux Repository" "https://archlinux.org/packages/?sort=&q="

gentooSearchList :: SearchList
gentooSearchList =
  [ ("w", gentooWiki)
  , ("p", gentooPackages)
  ]
  where
    gentooWiki = searchEngine "Gentoo Linux Wiki" "https://wiki.gentoo.org/index.php?title=Special:Search&search="
    gentooPackages = searchEngine "Gentoo Linux Packages" "https://packages.gentoo.org/packages/search?q="

braveSearch :: SearchEngine
braveSearch = searchEngine "Brave Search" "https://search.brave.com/search?q="

searx :: SearchEngine
searx = searchEngine "SearXNG" "https://searx.be/?preferences=eJx1WEuP5DYO_jVbl8IUspsFghzqFCDXLLC5G7RE2xpLokaSq8r960P6UZbaPYc2Sh8piuJbrSBjT9FguvfoMYK9WPD9BD3ewfKCFFi8o7_AlEmRCxYz3nui3uLFOOZrQqTXfP87TnhxmAfS9__99f-_Lwk6TAhRDfdfLnlAh_dkZP8lYppsTg35xuOzydDe_wSb8KLJNEwk-8B4T8p8G6b2xjglBfFbyjOrYqk3ijQ-vmmI42UV2aw00fGi0GeMDVjTe8e_V81AP8Ar1M2m0nrgjwnj3BjfZJN5_8JpfGe8ySxTRbJ2BdddorFaLTazIIsqb4IGyiPO6a6xA77bRZsEreXj0PfGs3V_76FvmkTKgL061Ab-9Z8_wHtIV5FsHtg0nbGYBA7j1ZkYKZYYX_PK32vKFEtmD4q8hqYxWZYxG8W_xTMLOY1Ns7mWl62xRv6a5mE0knC0oa04sjZ9f8ingJ7dkbAQuuh_U0rdNBaSVAgRO4zIht7UYROmxCBfXRnBV-ypH4Y9Xxw7Y_i0vK7HFccKyAHDkHxF0hStwUp7GPxAXQlpxA-Oh8ZNyahl_TDgMxuqEK11f2XfieMN-VTuf5rRaMhQYqs3HATm5C8jmb08V6r8-1XI73Qkow-rdtaoMZYMEfGaqMtPiHjVJnJsSZStZuyi8aOB0rFrCl6DhVnCIh2yS4ojNnQqfOQ4fKPEICeRL43bB9K6tNMAbQT5bDoM-Np-GddPpe7GQ3ECB1iEOF-pu_IRPedzeYxQMUfwybImpb24doAK4LdDHH0MpqI_XWtLwM8ARZzqOWEZjRK4EQMVlgmgRuhNytsZwQRO0WMLh--W6MUxb_AckAdpFfH1tipkA1c0jsBdgal1osGRHOseNydTWydhMLAr91Z4BaQcXuWzSc00zpQpDTSKOffbc101moOK8zMXRslymaQGmMrzSnRTffKJYyoNZUbSlKe2tPobeacbGDs7krQq2B7GIRXrp2nnKsEsvsBrjtTPqdgSjaf8_DFRbS4BE01RndGAakmJn8CHKwSWymfy_Jn7QbN0vhIl20VwYMNQqayNyh_k68rw66-_vQq_4IcHVzLEqZ17dGmPEsT4ycxLBeamOkodfWJbegBUpa-sz6EbJ84mrNL2gYkTBKvwwjHW8btCJ3ErvGfBIYAzXeUqjh-9JGWxVezpuPCWXFzC3ssL-rqVLklljZ9eV9n7bnwv8yjd13L8KHDhCEXhDmK4uiP6_vP6fL8FrQyxIKcLtya3kxpxrzBqwG4kyVlib8W1Z0mDnOsWlTJkkw4uTSwkXnn-2QQhN01pIG8GPkX-eqoEvdHzFQraSe2CVl2ywJ8o8RYPDdDR992cbyxzqnv2DR5YN8-F3qyrmdLhE15ncfZyyZ6nMGiPxXF9qW4txn6nEemIoAuLrV3vUxBJHzwZYsOri27YyTAbzoWQLMQyvAYQD61C1v64KbC1SN0euhnPAg1NpeQC223x3XDJPnaNPCJAKiq1RedmLvTOTTKnLNWqHihXjilx1foJjfth_hlNJK-94TN5a-li22QKjT6-baRCSc6LR5VhltqU8RZ3QzngEVaT_4meb_LAvYbb0Bcc-j0kmJeyNOnDhj649zE8MuE-vcg0ADxVySSXMFeFfqdxN-axDLYJsKbzdIKFbwTi4oaYefzbZ8CgpUYf-wK_CqTrbVQj7xJM5di5DCC3RffjCsFEHo1aKIzKs5lisxivZQjblQhz2KvfjyePtKXRF6CO8RU6pcMKnyI_AkfnteWyk8qpkC2oTT65JFLOksk8XhIWWck9mKtw7SKudWok7qydpedevdM4tZPP054_YivPE-H6Nvoi_9IUME7p7eCE7HB--3yVq0mm_fCpY-cqRWcYiOq2yUBtQOkfnwEJliX-d0HCJInEMXQ72boknixeEmFi69edayctMXGEhpsdV2JOzvdQreuOoNEf2nHN5k5djdXyQgjGUj64PDzEruV0E7U3Y3FLepmRPI-y1zR78qxE4fTv4Raexalc4XMnwRtPgfPgZ9HSyzc3PsHagY_3pdVyjjdTyJsn52xhcy5Bj_oZKMDZ_Ct8btoLXHmWy9pRsNZBcpj2pt7m_xYP5DD2t572of54-Qc78byS7jKjv27b6qbAqom9RIuhKlJC2zXGd3SiMHfDk4QaTxRpBA0_s9hfnDDvUenrs6WgbqSToIH7Ak-h2HAZtPB-ihwMBM3675ln5KfJWcW49MOGX-TM4yS9L_wO4Sp5_weHnvNQ&q="

{-- Scratchpads --}

-- Generic function to generate NamedScratchpad given a name, command, and class
scratchpad :: 
  String              -- Scratchpad name (to define keybind)
  -> String           -- Command to spawn 
  -> String           -- Resulting window class (for XMonad to find and manage) 
  -> NamedScratchpad
scratchpad name cmd windowClass = NS name cmd find manage
  where
    find = className =? windowClass

    -- Define scratchpad geometry
    manage = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w

-- Function to generate NamedScratchpads for terminal programs
-- Leave `cmd` empty (Nothing) to spawn a blank terminal
-- Leave `class` empty (Nothing) to set the class to be the same as the name
termScratchpad ::
  String          -- Scratchpad name
  -> Maybe String -- Scratchpad command
  -> Maybe String -- Scratchpad class
  -> NamedScratchpad
termScratchpad 
  name cmd windowClass = scratchpad name command cName
  where
    command = "st -c " ++ show cName ++ verb  -- Use `st` as our terminal
      where
        verb = case cmd of
          Just x -> " -e " ++ show x          -- If `cmd` exists, add an argument to spawn it inside the terminal
          Nothing -> ""
    cName = fromMaybe name windowClass        -- If windowClass is Nothing, use the scratchpad name instead as the class


myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ termScratchpad 
      "terminal" 
      Nothing 
      Nothing
    , termScratchpad 
      "btop" 
      ( Just "btop" ) 
      Nothing
    , termScratchpad
      "irc"
      ( Just "weechat" )
      Nothing
    , termScratchpad
      "fm"
      ( Just "yazi" )
      Nothing
    , termScratchpad
      "notes"
      ( Just "tmux new-session -s notes -c ~/Documents/Notes/" )
      Nothing
    , termScratchpad
      "khal"
      ( Just "ikhal" )
      Nothing
    , scratchpad 
      "signal" 
      "signal-desktop" 
      "Signal"
    ]

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

{-- XMOBAR --}

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

{-- TABBED LAYOUTS --}

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
        , decoHeight = 14
        , decoWidth = maxBound
        }

-- Here, `mkToggle` (NBFULL ?? NOBORDERS ?? EOT) is used to enable fullscreen toggling.
myLayout =
    avoidStruts $
        mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ -- Add the option to toggle fullscreen (no gaps nor borders) on any layout
             -- Add the option to toggle fullscreen (no gaps nor borders) on any layout
            bsp -- Binary space partition (spiral, bspwm style)
                ||| tiled -- Master and stack layout (vertical)
                ||| Mirror tiled -- Master and stack layout (horizontal)
                ||| monocle -- almost-fullscreen
                ||| threeCol -- three columns of window, one large one on the centre and two smaller ones on each side
                ||| tabs -- fullscreen with tabs
  where
    {-- Here are some custom layouts --}
    tabs = tabbed shrinkText myTabConfig
    tiled =
      spacing gaps $                    -- add gaps to the layout
                          -- add gaps to the layout
      windowNavigation $                -- simplifies window navigation keybindings 
                      -- simplifies window navigation keybindings 
      addTabs shrinkText myTabConfig $  -- add tabbed sublayout
        -- add tabbed sublayout
      boringWindows $                   -- skips navigation for non-visible windws 
      Tall                              -- use the Tall layout as the base for this custom layout 
        nmaster                         -- define how many windows can be in the master stack 
        delta                           -- define how much the ratio of window sizes can be incremented each time
        ratio                           -- define the initial ratio of window sizes
    threeCol =
      spacing gaps $                    -- add gaps to the layout
                          -- add gaps to the layout
      addTabs shrinkText myTabConfig $  -- simplifies window navigation keybindings 
        -- simplifies window navigation keybindings 
      boringWindows $                   -- skips navigation for non-visible windws 
        ThreeColMid                     -- use the Tall layout as the base for this custom layout 
          nmaster                       -- define how many windows can be in the master stack 
          delta                         -- define how much the ratio of window sizes can be incremented each time
          ratio                         -- define the initial ratio of window sizes
    bsp =
        renamed [XLR.Replace "BSP"] $
        smartBorders $
        windowNavigation $
        addTabs shrinkText myTabConfig $
        subLayout [] tabs $
        spacing
          gaps
          emptyBSP
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
        , historySize = 10
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
