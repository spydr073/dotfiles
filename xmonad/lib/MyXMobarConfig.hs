
-- ────────────────────────────────────────────────────────────────────────────────────────────────
--
--    ██╗  ██╗███╗   ███╗ ██████╗ ██████╗  █████╗ ██████╗
--    ╚██╗██╔╝████╗ ████║██╔═══██╗██╔══██╗██╔══██╗██╔══██╗
--     ╚███╔╝ ██╔████╔██║██║   ██║██████╔╝███████║██████╔╝
--     ██╔██╗ ██║╚██╔╝██║██║   ██║██╔══██╗██╔══██║██╔══██╗
--    ██╔╝ ██╗██║ ╚═╝ ██║╚██████╔╝██████╔╝██║  ██║██║  ██║
--    ╚═╝  ╚═╝╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝
--
-- ────────────────────────────────────────────────────────────────────────────────────────────────


-----------------------------------------------------------------------------------------[ Module ]
--{1

module MyXMobarConfig
  ( myConfig

  , left_div
  , right_div

  , left_cap
  , right_cap

  , tag_left_start
  , tag_left
  , tag_left_end
  , tag_center
  , tag_right_start
  , tag_right
  , tag_right_end
  ) where


import XMobarUtils
import Themes

--}

---------------------------------------------------------------------------------------[ Settings ]
--{1

shell       = "/run/current-system/sw/bin/zsh"

left_div    = icon "left_inside_div"
right_div   = icon "right_inside_div"

left_cap    = icon "left_round"
right_cap   = icon "right_round"

gradient    = [ "--low"    , myfgLowColor_bar  ++ "," ++ mybgColor_bar
              , "--normal" , myfgMedColor_bar  ++ "," ++ mybgColor_bar
              , "--high"   , myfgHighColor_bar ++ "," ++ mybgColor_bar
              ]

--}

-------------------------------------------------------------------------------------[ Formatting ]
--{1

tag_left_start =
  \c -> (color myfgTagColor mybgTagColor left_cap) ++
        (color myfgColor_bar mybgColor_bar c)

tag_left =
  \c -> (color myfgTagColor mybgTagColor left_div) ++
        (color myfgColor_bar mybgColor_bar c)

tag_left_end =
  \i c -> (color myfgTagColor mybgTagColor left_div) ++
          (color myfgColor_bar mybgColor_bar (" " ++ (icon i) ++ " ")) ++
          (color myfgColor_bar mybgColor_bar c) ++
          (color myfgTagColor mybgTagColor right_cap)

tag_center =
  \c -> (color myfgTagColor mybgTagColor left_cap) ++
        (color myfgColor_bar mybgColor_bar c) ++
        (color myfgTagColor mybgTagColor right_cap)

tag_right_start =
  \i c -> (color myfgTagColor mybgTagColor left_cap) ++
          (color myfgColor_bar mybgColor_bar (" " ++ (icon i) ++ " ")) ++
          (color myfgColor_bar mybgColor_bar c)

tag_right =
  \i c -> (color myfgTagColor mybgTagColor right_div) ++
          (color myfgColor_bar mybgColor_bar (" " ++ (icon i) ++ " ")) ++
          (color myfgColor_bar mybgColor_bar c)

tag_right_end =
  \c -> (color myfgTagColor mybgTagColor right_div) ++
        (color myfgColor_bar mybgColor_bar c) ++
        (color myfgTagColor mybgTagColor right_cap) ++
        (color mybgTagColor mybgTagColor "  ")

--}

-----------------------------------------------------------------------------------------[ Config ]
--{1

myConfig = XMobar {
  font             = "xft:Anonymous Pro:size=11:bold:antialias=true"
    --font             = "xft:Terminus:size=10:bold:antialias=true"
    --font             = "xft:Bitstream Vera Sans Mono:size=10:bold:antialias=true"
  , additionalFonts  = []
  , iconOffset       = -1
  , textOffset       = -1
  , iconRoot         = "/home/spydr/dotfiles/xmonad/info_bars/xbm/"
  , pickBroadest     = True

  , fgColor          = myfgColor_bar
  , bgColor          = mybgTagColor
  , borderColor      = myBorderColor_bar
  , borderWidth      = 4

  , position         = "TopSize L 100 28"
  , border           = "BottomB"
  , alpha            = 255

  , sepChar          = '%'
  , alignSep         = ('}', '{')

  , lowerOnStart     = True
  , hideOnStart      = False
  , allDesktops      = True
  , overrideRedirect = True
  , persistent       = True

  , template         = [ "  "
                       , (color myfgTagColor mybgTagColor left_cap)
                       , "%StdinReader%"
                       , tag_left_end "spkr" "%volume% "
                       , "}"
                       , "%date%"
                       , "{"
                       , "%multicpu%"
                       , "%memory%"
                       , "%coretemp%"
                       , "%dynnetwork%"
                       , "%battery%"
                       , "  "
                       ]

  , commands        =
      [ STDIN

      , DateCmd (tag_center " %a %F %H:%M %z ") 10

      , UsrCmd  shell
                [ "-c" , "/home/spydr/dotfiles/xmonad/scripts/volume.sh get" ]
                "volume"
                3

      , XBarCmd "DynNetwork"
                ([ "--template" , (tag_right "net_up" "<tx> ") ++
                                  (tag_right "net_down" "<rx> ")
                 , "--minwidth" , "4"
                 , "--Low"      , "1000" -- units: kB/s
                 , "--High"     , "5000" -- units: kB/s
                 ] ++ gradient)
                10

      , XBarCmd "MultiCpu"
                ([ "--template" , tag_right_start "cpu" "<autototal> "
                 , "--minwidth" , "2"
                 , "--Low"      , "50" -- units: %
                 , "--High"     , "85" -- units: %
                 ] ++ gradient)
                10

      , XBarCmd "CoreTemp"
                ([ "--template" , tag_right "temp" "<core0> "
                 , "--minwidth" , "2"
                 , "--Low"      , "70" -- units: °C
                 , "--High"     , "80" -- units: °C
                 ] ++ gradient)
                50

      , XBarCmd "Memory"
                ([ "--template" , tag_right "mem" "<usedratio> "
                 , "--minwidth" , "2"
                 , "--Low"      , "30" -- units: %
                 , "--High"     , "90" -- units: %
                 ] ++ gradient)
                10

      , XBarCmd "Battery"
                [ "--template" , tag_right_end " <acstatus> "
                , "--minwidth" , "2"
                , "--Low"      , "20" -- units: %
                , "--High"     , "80" -- units: %
                , "--"           -- battery specific options
                , "-o"         , (icon "zap") ++ " [<left>,<timeleft>]"  -- discharge
                , "-O"         , (icon "plug") ++ " [<left>,<timeleft>]" -- charge
                , "-i"         , (icon "full_bat")                      -- full
                ]
                50

      ]
  }

--}



