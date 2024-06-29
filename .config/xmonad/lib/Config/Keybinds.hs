module Config.Keybinds (DefaultPrograms (..), myKeys) where

import XMonad (X, sendMessage)
import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen)
import XMonad.Core (spawn)
import XMonad.Operations (kill)

import XMonad.Layout.MultiToggle (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))

import Config.Layout (myLayoutKeybinds)
import Config.Scratchpads (myScratchpadKeybinds)
import Config.Search (mySearchKebinds)
import Utils.Taskwarrior (taskwarriorKeybinds)

{-- KEYBINDINGS:
 - The following keybindings are to be used with the `additionalKeysP` function
 - provided by XMonad.Util.EZConfig to provide a simpler syntax --}

data DefaultPrograms = DefaultPrograms
        { term :: String
        , launcher :: String
        , editor :: String
        , screenshot :: String
        }

myKeys :: DefaultPrograms -> [(String, X ())]
myKeys defaults =
        [ ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
        , ("M-r", spawn (launcher defaults))
        , ("M-<Return>", spawn (term defaults))
        , ("M-q", kill)
        , ("M-n", nextScreen)
        , ("M-S-n", shiftNextScreen)
        , ("M-t f", sendMessage $ Toggle NBFULL)
        , ("<XF86AudioRaiseVolume>", spawn increaseVolCmd)
        , ("<XF86AudioLowerVolume>", spawn decreaseVolCmd)
        , ("M-p", spawn (screenshot defaults))
        ]
                -- Append search engines to the keybinding list
                ++ myLayoutKeybinds
                ++ mySearchKebinds
                ++ myScratchpadKeybinds
                ++ taskwarriorKeybinds
    where
        audioDelta = 5 -- configures how much each command should change the volume by
        increaseVolCmd = "wpctl set-volume @DEFAULT_AUDIO_SINK@ " ++ show audioDelta ++ "%+"
        decreaseVolCmd = "wpctl set-volume @DEFAULT_AUDIO_SINK@ " ++ show audioDelta ++ "%-"
