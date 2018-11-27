
-----------------------------------------------------------------------------------------[ Module ]
--{1

module Hooks
  ( myStartupHook
  , myLogHook
  , myManageHook
  , myLayoutHook
  ) where

import System.IO

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.IM
import XMonad.Layout.Named

import Settings
import Themes
import MyXMobarConfig

--}

------------------------------------------------------------------------------------------[ Utils ]
--{1

wrapIcon :: String -> String
wrapIcon icon = "<icon=" ++ myIconDir ++ icon ++ "/>"


makeTag :: String -> String
makeTag s = (wrapColor (wrapIcon "left_inside_div.xbm")) ++
            (pad s)
  where wrapColor i = "<fc=" ++ myfgTagColor ++ "," ++ mybgTagColor ++ ">"
                      ++ i ++ "</fc>"
        pad s = "  " ++ s ++ "  "

--}

------------------------------------------------------------------------------------------[ Hooks ]
--{1

--{2 Log

--myLogHook :: Handle -> PP
myLogHook h = dynamicLogWithPP $ def
  { ppOrder           = \(ws:l:t:_) -> [ws,l]
  --, ppUrgent          = xmobarColor mybgColor mybgColor_urgent

  , ppCurrent         = xmobarColor myfgColor_current mybgColor_bar .
                          (\ _ -> wrapIcon "dot_full.xbm")
  , ppHidden          = xmobarColor myfgColor_hidden mybgColor_bar .
                          (\ _ -> wrapIcon "dot_half.xbm")
  , ppVisible         = xmobarColor myfgColor_visible mybgColor_bar .
                          (\ _ -> wrapIcon "dot_center.xbm")
  , ppHiddenNoWindows = xmobarColor myfgColor_noWindows mybgColor_bar .
                          (\ _ -> wrapIcon "dot_empty.xbm")


  , ppLayout          = xmobarColor myfgColor mybgColor_bar .
                          (\s ->
                            case s of
                                 "Default"  -> makeTag $ wrapIcon "layout_tall.xbm"
                                 "Full"     -> makeTag $ wrapIcon "layout_full.xbm"
                                 "Float"    -> makeTag $ wrapIcon "layout_mouse.xbm"
                                 "Gimp"     -> makeTag $ "[G]"
                                 _          -> makeTag $ "[?]"
                          )

  , ppSep             = ""
  , ppWsSep           = ""
  , ppOutput          = hPutStrLn h
  }

--}

--{2 Startup

--myStartupHook :: Handle -> X ()
myStartupHook =  setWMName "LG3D"

--}

--{2 Mangagement

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
  --, [ return True       --> doMaster                                ]
  ])
  where
    -- Important windows get floated to the center of ALL workspaces
    urgent = [ ]

    -- Programs that spawn floated
    floatClasses = [ "Xmessage", "Eclipse", "Thunar", "Electron", "Arandr", "Pavucontrol" ]
    floatTitles  = []

    -- Programs that spawn within a given workspace
    shiftClasses = [("Gimp", "5")]
    shiftTitles  = []

    doCenterFloat' = doCenterFloat
    doFloatAt' x y = doFloatAt x y
    doSideFloat' p = doSideFloat p
    doRectFloat' r = doRectFloat r
    doFullFloat'   = doFullFloat

--}

--{2 Layout

myLayoutHook = onWorkspaces ["1", "8"] floatFirst
             $ onWorkspaces ["3"] fullFirst
             $ onWorkspaces ["5"] gimpFirst
             $ defaultOrder

  where

    floatFirst = named "Float" float
             ||| named "Full" full
             ||| named "Default" tiled

    fullFirst = named "Full" full
            ||| named "Default" tiled
            ||| named "Float" float

    gimpFirst = named "Gimp" gimp
            ||| named "Default" tiled
            ||| named "Float" float
            ||| named "Full" full

    defaultOrder = named "Default" tiled
               ||| named "Float" float
               ||| named "Full" full

    tiled = smartBorders $ smartSpacing 16 (Tall 1 (3/100) (0.5))

    float = simplestFloat

    full  = noBorders Full

    gimp  = withIM (0.11)
                   (Role "gimp-toolbox")
                   (reflectHoriz (withIM (0.15)
                                         (Role "gimp-dock")
                                         (Mirror (Tall 1 (3/100) (1/2))
                                             ||| Full)))

--}

--}



