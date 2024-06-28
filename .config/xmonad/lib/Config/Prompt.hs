module Config.Prompt where

import XMonad.Prompt (XPConfig (alwaysHighlight, bgColor, bgHLight, borderColor, fgColor, fgHLight, font, height, historySize, position, searchPredicate, sorter), XPPosition (Bottom), def)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)

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
