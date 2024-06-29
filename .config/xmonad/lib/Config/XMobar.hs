{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Config.XMobar (xmobarProp) where

import XMonad (KeyMask, KeySym, Layout, XConfig (XConfig, modMask), xK_b)
import XMonad.Hooks.DynamicLog (PP (ppSort), xmobarPP)
import XMonad.Hooks.StatusBar (StatusBarConfig, dynamicSBs, statusBarProp, statusBarPropTo, withEasySB)
import XMonad.Hooks.StatusBar.PP (PP (ppCurrent, ppExtras, ppHidden, ppLayout, ppOrder, ppOutput, ppSep, ppTitle, ppTitleSanitize, ppUrgent, ppVisible, ppVisibleNoWindows, ppWsSep), def, filterOutWsPP, shorten, wrap, xmobarBorder, xmobarColor, xmobarFont, xmobarStrip)
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

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
