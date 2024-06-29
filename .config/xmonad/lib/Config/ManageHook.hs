module Config.ManageHook (myManageHook) where

import Config.Scratchpads (myScratchpadManageHook)
import XMonad (ManageHook)
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen, isInProperty)
import XMonad.ManageHook (className, composeAll, doFloat, doShift, stringProperty, (-->), (<+>), (=?))

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
                <+> manageDocks
    where
        isRole = stringProperty "WM_WINDOW_ROLE"
        isFileChooserDialog = isRole =? "GtkFileChooserDialog"
        isPopup = isRole =? "pop-up"
        isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
        isGtk4Dialog = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"
        isGtk4Modal = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_MODAL"
