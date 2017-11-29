
-- ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Module ]
--{{{1
module Hooks
  ( myStartupHook
  , myLogHook
  , myManageHook
  , myLayoutHook
  ) where
--}}}


-- ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Imports ]
--{{{1
import System.IO
import XMonad

import XMonad.ManageHook
import XMonad.Util.SpawnOnce
import XMonad.Actions.CopyWindow

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.Minimize

import XMonad.Layout
import XMonad.Layout.LayoutModifier
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SimpleFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Named

import qualified XMonad.StackSet as W

import Settings
import Themes
import MyXMobarConfig
--}}}


-- ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Statusbar Functions ]
--{{{1
wrapIcon :: String -> String
wrapIcon icon = "<icon=" ++ myIconDir ++ icon ++ "/>"


makeTag :: String -> String
makeTag s = (wrapColor (wrapIcon "left_inside_div.xbm")) ++
            (pad s)
  where wrapColor i = "<fc=" ++ myfgTagColor ++ "," ++ mybgTagColor ++ ">"
                      ++ i ++ "</fc>"
        pad s = "  " ++ s ++ "  "
--}}}


-- ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Hooks ]
--{{{1
-- Log Hook {{{2
--myLogHook :: Handle -> PP
myLogHook h = dynamicLogWithPP $ def
  { ppOrder           = \(ws:l:t:_) -> [ws,l]
  , ppUrgent          = xmobarColor mybgColor mybgColor_urgent

  , ppCurrent         = xmobarColor myfgColor_current mybgColor_bar .
                          (\ _ -> wrapIcon "dot_full_16.xbm")
  , ppHidden          = xmobarColor myfgColor_hidden mybgColor_bar .
                          (\ _ -> wrapIcon "dot_full_16.xbm")
  , ppVisible         = xmobarColor myfgColor_visible mybgColor_bar .
                          (\ _ -> wrapIcon "dot_full_16.xbm")
  , ppHiddenNoWindows = xmobarColor myfgColor_noWindows mybgColor_bar .
                          (\ _ -> wrapIcon "dot_empty_16.xbm")

  , ppLayout          = xmobarColor myfgColor mybgColor_bar .
                          (\s ->
                            case s of
                                 "Default"  -> makeTag $ wrapIcon "layout_tall.xbm"
                                 "Full"     -> makeTag $ wrapIcon "layout_full.xbm"
                                 "Float"    -> makeTag $ wrapIcon "mouse.xbm"
                                 "Gimp"     -> makeTag $ "[G]"
                                 _          -> makeTag $ "[?]"
                          )

  , ppSep             = ""
  , ppWsSep           = ""
  , ppOutput          = hPutStrLn h
}
--}}}

-- Startup Hook {{{2
--myStartupHook :: Handle -> X ()
myStartupHook =  --spawn "compton --config ~/dotfiles/compton/compton.conf" <+>
                 setWMName "LG3D"
--}}}

-- Mangage Hook {{{2
-- use xprop to get window information
myManageHook :: ManageHook
myManageHook =
  (composeAll . concat $
  [ [ className    =? x --> doCenterFloat'  | x <- floatClasses     ]
  , [ title        =? x --> doCenterFloat'  | x <- floatTitles      ]

  , [ className    =? x --> doShift y       | (x,y) <- shiftClasses ]
  , [ title        =? x --> doShift y       | (x,y) <- shiftTitles  ]

  , [ title        =? x --> doCenterFloat'  | x <- urgent           ]
  , [ isDialog          --> doCenterFloat'                          ]
  , [ isFullscreen      --> doFullFloat'                            ]
--  , [ return True       --> doMaster                                ]
  ])
  where
    -- Important windows get floated to the center of ALL workspaces
    urgent = [ ]

    -- Programs that spawn floated
    floatClasses = [ "Xmessage", "Eclipse", "zathura", "Thunar"]
    floatTitles  = []

    -- Programs that spawn within a given workspace
    shiftClasses = [("Gimp", "5")]
    shiftTitles  = []

    doCenterFloat' = doCenterFloat
    doFloatAt' x y = doFloatAt x y
    doSideFloat' p = doSideFloat p
    doRectFloat' r = doRectFloat r
    doFullFloat' = doFullFloat
--}}}

-- Layout Hook {{{2
myLayoutHook = onWorkspaces ["5"] gimpLayoutFirst $
               onWorkspaces ["1", "8"] floatLayoutFirst $
               defaultLayoutOrder
  where
    gimpLayoutFirst =
          named "Gimp" gimp
      ||| named "Default" tiled
      ||| named "Float" float
      ||| named "Full" full

    floatLayoutFirst =
          named "Float" float
      ||| named "Full" full
      ||| named "Default" tiled
      ||| named "Padded" padded

    defaultLayoutOrder =
          named "Default" tiled
      ||| named "Float" float
      ||| named "Full" full
      ||| named "Padded" padded

    tiled = smartSpacing 3 (Tall 1 (3/100) (1/3))
    padded = spacing 4 (Tall 1 (3/100) (1/3))
    float = simplestFloat
    full = noBorders Full
    gimp = withIM (0.11)
                  (Role "gimp-toolbox")
                  (reflectHoriz (withIM (0.15)
                                        (Role "gimp-dock")
                                        (Mirror (Tall 1 (3/100) (1/2))
                                            ||| Full)))
--}}}
--}}}



