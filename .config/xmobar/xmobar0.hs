import System.Environment        (getEnv)
import System.IO.Unsafe          (unsafeDupablePerformIO)

import XMonad.Hooks.StatusBar.PP (wrap, xmobarColor, xmobarFont)
import Xmobar
    ( defaultConfig,
      xmobar,
      Border(FullB),
      Config(alignSep, template, commands, font, additionalFonts,
             textOffsets, bgColor, fgColor, borderColor, border, borderWidth,
             position, alpha, overrideRedirect, lowerOnStart, hideOnStart,
             allDesktops, persistent, iconRoot, iconOffset, sepChar),
      XPosition(Static, height, xpos, ypos, width),
      Command(Com),
      Date(Date),
      Monitors(WeatherX, Network, K10Temp),
      XMonadLog(XPropertyLog),
      Runnable(..) )

formatbg, foreground, borderc, background :: String
formatbg   = "#0F0F0F"         {- Lighter Grey -}
foreground = "#FFFFFF"         {- White -}
background = "#000000"         {- Grey -}
borderc    = "#000000"         {- Dark Purple -}

red, blue, green, magenta, cyan :: String -> String
red        = xmobarColor "#E5A3A1" (formatbg <> ":5")
blue       = xmobarColor "#A3CBE7" (formatbg <> ":5")
green      = xmobarColor "#B4E3AD" (formatbg <> ":5")
magenta    = xmobarColor "#CEACE8" (formatbg <> ":5")
cyan       = xmobarColor "#C9D4FF" (formatbg <> ":5")

myHomeDir :: String
myHomeDir = unsafeDupablePerformIO (getEnv "HOME") 

main :: IO ()
main = xmobar =<< myConfig

myConfig :: IO Config
myConfig = do 
    pure baseConfig
        { template = 
            wrap "  " " " (xmobarColor "#CEACE8" "" (xmobarFont 4 "<icon=haskell_20.xpm/> "))
            <> inWrapper (xmobarFont 4 "%_XMONAD_LOG_1%")
            <> "}{"
            <> concatMap inWrapper
                [ blue    (xmobarFont 4 "%YSSY%")
                , red     (xmobarFont 4 "\xf1eb %wlan0%")     {- Received and sent analytics -}
                , cyan    (xmobarFont 4 "\xf2c7 %k10temp%")    {- CPU temperature             -}
                , green   (xmobarFont 4 "%vol%")        {- Volume percentage           -}
                , magenta (xmobarFont 4 "%date%")       {- Time-}
                ]
            <> xmobarFont 4 "%_XMONAD_TRAYPAD%"
        , commands = myCommands
        }
    where
        inWrapper :: String -> String
        inWrapper = wrap 
            (xmobarColor formatbg (background <> ":7") (xmobarFont 2 "\xe0b6"))
            (xmobarColor formatbg (background <> ":7") (xmobarFont 2 "\xe0b4") <> " ")


myCommands :: [Runnable]
myCommands = 
    [ Run $ XPropertyLog "_XMONAD_LOG_1"
    , Run $ Network "wlan0" 
    [ "-t"
    , "\xf433 <rx> kb \xf431 <tx> kb"
    ] 10
    , Run $ K10Temp "0000:00:18.3"
    [ "-t"
    , "<Tdie>°C"
    ] 10
    , Run $ Date "\xf017 %a, %d %b %H:%M" "date" 10
    , Run $ Com (myHomeDir <> "/.config/xmonad/scripts/volume.sh" ) ["vol"] "vol" 1
    , Run $ WeatherX "YSSY"
      [ ("clear", "\xf522")
      , ("sunny", "\xf522")
      , ("mostly clear", "\xe21d")
      , ("mostly sunny", "\xe21d")
      , ("partly sunny", "\xe21d")
      , ("fair", "🌑")
      , ("cloudy","\xf0590")
      , ("overcast","\xe30c")
      , ("partly cloudy", "\xf0595")
      , ("mostly cloudy", "\xf0595")
      , ("considerable cloudiness", "\xf0595")]
      ["-t", "<fn=2><skyConditionS></fn> <tempC>°C"]
      18000
    , Run $ XPropertyLog "_XMONAD_TRAYPAD"
    ]

baseConfig :: Config
baseConfig = defaultConfig
    { font            =   "xft:JetBrainsMono Nerd Font:size=10:antialias=true:hinting=true"
    , additionalFonts = [ "xft:JetBrainsMono Nerd Font:size=10:antialias=true:hinting=true"
                        , "xft:JetBrainsMono Nerd Font:size=12:antialias=true:hinting=true"
                        , "xft:JetBrainsMono Nerd Font:size=10:antialias=true:hinting=true"
                        , "xft:JetBrainsMono Nerd Font:size=10:antialias=true:hinting=true"
                        , "xft:JetBrainsMono Nerd Font:size=10:antialias=true:hinting=true"
                        ]     --      --    
    , textOffsets      = [20, 22, 22, 21, 22]
--  , textOffsets      = [0, 0, 0, 0, 0]
    , bgColor          = background 
    , fgColor          = foreground
    , borderColor      = borderc
    , border           = FullB 
    , borderWidth      = 1
    {-
    , position         = Static { xpos = 13, ypos = 1034, width = 1893, height = 32 } Bottom Padded
    , position         = Static { xpos = 0, ypos = 1048, width = 1920, height = 32 } Bottom Flat
    , position         = Static { xpos = 0, ypos = 0, width = 1920, height = 32 } Top Flat
    -}
    , position         = Static { xpos = 1920, ypos = 0, width = 1920, height = 32 }
--  , position         = Static { xpos = 0, ypos = 0, width = 2560, height = 32 }
    , alpha            = 255
    , overrideRedirect = False
    , lowerOnStart     = True
    , hideOnStart      = False
    , allDesktops      = False
    , persistent       = True
    , iconRoot         = myHomeDir ++ "/.config/xmonad/icons"
    , iconOffset       = -1
    , sepChar  = "%"
    , alignSep = "}{"
    }
