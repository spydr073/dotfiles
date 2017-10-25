
-- ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Module ]
--{{{1
module Themes
  ( myTheme
  , myBorderWidth

  , myNormalBorderColor
  , myFocusedBorderColor
  , myfgColor
  , mybgColor
  , mybgColor_edge
  , mybgColor_tag
  , mybgColor_urgent
  , myfgColor_current
  , myfgColor_hidden
  , myfgColor_visible
  , myfgColor_noWindows

  , myfgColor_bar
  , mybgColor_bar
  , myfgTagColor
  , mybgTagColor
  , myBorderColor_bar
  , myfgLowColor_bar
  , myfgMedColor_bar
  , myfgHighColor_bar
  ) where
--}}}


-- ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Imports ]
--{{{1
import XMonad
--import XMonad.Prompt
import XMonad.Layout.Decoration
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicHooks
import XMonad.Util.Loggers
import XMonad.Util.WorkspaceCompare
--}}}


-- ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Settings ]
--{{{1
myBorderWidth :: Dimension
myBorderWidth = 4

myFont :: String
myFont = "terminus"
--}}}


-- ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Colors ]
--{{{1
-- XMonad Colors
myNormalBorderColor  = "#404450"
myFocusedBorderColor = "#504440"
myfgColor            = "#777777"

mybgColor_edge       = "#4A0404"
mybgColor            = "#1c1b1a"
mybgColor_tag        = "#222222"
mybgColor_urgent     = "#440000"

myfgColor_current    = "#224422"
myfgColor_hidden     = "#444422"
myfgColor_visible    = "#224444"
myfgColor_noWindows  = "#555555"


-- XMobar Colors
myfgColor_bar        = "#607d8b"
mybgColor_bar        = "#282c32"

myfgTagColor         = mybgColor_bar
mybgTagColor         = "#000000"
myBorderColor_bar    = "#152427"

myfgLowColor_bar     = "#4a5d23"
myfgMedColor_bar     = "#b8860b"
myfgHighColor_bar    = "#7c1c05"


-- 8-Bit Color Theme
colorBlk1 = "#252525" --Black
colorBlk2 = "#404040"
colorWht1 = "#dddddd" --White
colorWht2 = "#707070"
colorRed1 = "#953331" --Red
colorRed2 = "#8D4A48"
colorGrn1 = "#546A29" --Green
colorGrn2 = "#7E9960"
colorYlw1 = "#909737" --Yellow
colorYlw2 = "#9CA554"
colorBlu1 = "#385E6B" --Blue
colorBlu2 = "#5C737C"
colorMag1 = "#7F355E" --Magenta
colorMag2 = "#95618B"
colorCyn1 = "#34676F" --Cyan
colorCyn2 = "#5D858A"
--}}}


-- ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Themes ]
--{{{1
myTheme :: Theme
myTheme = def
  { fontName            = myFont
  , inactiveBorderColor = colorBlk2
  , inactiveColor       = colorBlk1
  , inactiveTextColor   = colorWht2
  , activeBorderColor   = colorBlk2
  , activeColor         = colorBlk2
  , activeTextColor     = colorWht1
  , urgentBorderColor   = colorRed1
  , urgentTextColor     = colorRed2
  , decoHeight          = 18
  }
--}}}



