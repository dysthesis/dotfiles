module Config.Layout (myLayout, myLayoutKeybinds) where

import XMonad (X, sendMessage, withFocused)
import XMonad.Hooks.ManageDocks (Direction2D (D, L, R, U), avoidStruts)
import XMonad.Layout (Full (Full), JumpToLayout (JumpToLayout), Mirror (Mirror), Tall (Tall), (|||))
import XMonad.Layout.BinarySpacePartition (ResizeDirectional (ExpandTowards, ShrinkFrom), Rotate (Rotate), Swap (Swap), emptyBSP)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.Renamed qualified as XLR
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.SubLayouts (GroupMsg (UnMerge), pullGroup, subLayout)
import XMonad.Layout.Tabbed (Theme (activeBorderColor, activeBorderWidth, activeColor, activeTextColor, decoHeight, decoWidth, fontName, inactiveBorderColor, inactiveBorderWidth, inactiveColor, inactiveTextColor), addTabs, def, shrinkText, tabbed)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.WindowNavigation (windowNavigation)

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
                spacing gaps $ -- add gaps to the layout
                -- add gaps to the layout
                        windowNavigation $ -- simplifies window navigation keybindings
                        -- simplifies window navigation keybindings
                                addTabs shrinkText myTabConfig $ -- add tabbed sublayout
                                -- add tabbed sublayout
                                        boringWindows $ -- skips navigation for non-visible windws
                                                Tall -- use the Tall layout as the base for this custom layout
                                                        nmaster -- define how many windows can be in the master stack
                                                        delta -- define how much the ratio of window sizes can be incremented each time
                                                        ratio -- define the initial ratio of window sizes
        threeCol =
                spacing gaps $ -- add gaps to the layout
                -- add gaps to the layout
                        addTabs shrinkText myTabConfig $ -- simplifies window navigation keybindings
                        -- simplifies window navigation keybindings
                                boringWindows $ -- skips navigation for non-visible windws
                                        ThreeColMid -- use the Tall layout as the base for this custom layout
                                                nmaster -- define how many windows can be in the master stack
                                                delta -- define how much the ratio of window sizes can be incremented each time
                                                ratio -- define the initial ratio of window sizes
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

-- Helper function to switch to a certain layout.
switchToLayout :: String -> X ()
switchToLayout = sendMessage . JumpToLayout

myLayoutKeybinds :: [(String, X ())]
myLayoutKeybinds =
        [ ("M-S-a", sendMessage $ pullGroup L)
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
        , ("M-; t", switchToLayout "Spacing Tabbed Tall")
        , ("M-; w", switchToLayout "Mirror Spacing Tall")
        , ("M-; f", switchToLayout "monocle")
        , ("M-; 3", switchToLayout "Spacing ThreeCol")
        , ("M-; a", switchToLayout "Tabbed Simplest")
        , ("M-; b", switchToLayout "BSP")
        ]
