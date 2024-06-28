module Config.Scratchpads where

import Data.Maybe (fromMaybe)
import XMonad (X, className, (=?))
import XMonad.StackSet qualified as W
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadAction, namedScratchpadManageHook)

data ScratchpadType
        = Term
                (Maybe String) -- Command
        | Gui
                String -- Class
                String -- Command

data Scratchpad = Scratchpad
        { name :: String
        , prefix :: Char
        , category :: ScratchpadType
        }

-- Generic function to generate NamedScratchpad given a name, command, and class
scratchpad ::
        String -> -- Scratchpad name (to define keybind)
        String -> -- Command to spawn
        String -> -- Resulting window class (for XMonad to find and manage)
        NamedScratchpad
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
        String -> -- Scratchpad name
        Maybe String -> -- Scratchpad command
        Maybe String -> -- Scratchpad class
        NamedScratchpad
termScratchpad
        name
        cmd
        windowClass = scratchpad name command cName
            where
                command = "st -c " ++ cName ++ verb -- Use `st` as our terminal
                    where
                        verb = case cmd of
                                Just x -> " -e " ++ x -- If `cmd` exists, add an argument to spawn it inside the terminal
                                Nothing -> ""
                cName = fromMaybe name windowClass -- If windowClass is Nothing, use the scratchpad name instead as the class

toScratchpad :: Scratchpad -> NamedScratchpad
toScratchpad s =
        case category s of
                Term command -> termScratchpad (name s) command Nothing
                Gui cName command -> scratchpad (name s) command cName

toScratchKeybinds :: Scratchpad -> (String, X ())
toScratchKeybinds s =
        (cmd <> " " <> [prefix s], namedScratchpadAction myScratchpads (name s))
    where
        cmd = "M-s" -- Key prefix for scratchpads

scratchpadList :: [Scratchpad]
scratchpadList =
        [ Scratchpad
                { name = "terminal"
                , prefix = 't'
                , category = Term Nothing
                }
        , Scratchpad
                { name = "btop"
                , prefix = 'b'
                , category = Term (Just "btop")
                }
        , Scratchpad
                { name = "irc"
                , prefix = 'i'
                , category = Term (Just "weechat")
                }
        , Scratchpad
                { name = "task"
                , prefix = 'd'
                , category = Term (Just "taskwarrior-tui")
                }
        , Scratchpad
                { name = "fm"
                , prefix = 'f'
                , category = Term (Just "yazi")
                }
        , Scratchpad
                { name = "notes"
                , prefix = 'n'
                , category = Term (Just "sh -c 'tmux attach-session -t notes || tmux new-session -s notes -c ~/Documents/Notes/'")
                }
        , Scratchpad
                { name = "khal"
                , prefix = 'c'
                , category = Term (Just "ikhal")
                }
        , Scratchpad
                { name = "signal"
                , prefix = 's'
                , category = Gui "Signal" "signal-desktop"
                }
        ]

myScratchpads :: [NamedScratchpad]
myScratchpads =
        map toScratchpad scratchpadList
myScratchpadKeybinds :: [(String, X ())]
myScratchpadKeybinds = map toScratchKeybinds scratchpadList

myScratchpadManageHook = namedScratchpadManageHook myScratchpads
