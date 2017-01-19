
-- Imports {{{1

import System.IO
import System.Exit

import Control.Monad

import Data.Monoid
import Data.List
import Data.Maybe
import qualified Data.Map as M

import Foreign.C.Types

import XMonad  -- hiding ( (|||) )
import XMonad.Core

import XMonad.ManageHook

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.DecorationMadness
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ButtonDecoration
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.BoringWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Spacing
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.Cross
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Reflect
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Named
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.TwoPane
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.Spacing
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize

import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.Promote
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.WindowBringer
import XMonad.Actions.CopyWindow
import XMonad.Actions.UpdatePointer

import XMonad.Util.Loggers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WindowProperties
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W


--}}}



-- Default Programs {{{1
---------------------------------------------------------------------------------------------------

myTerminal :: String
myTerminal = "/run/current-system/sw/bin/termite"
--myTerminal = "/run/current-system/sw/bin/st"
--myTerminal = "/run/current-system/sw/bin/st -f \"VerilySerifMono:size=12\""
--myTerminal = "/usr/local/bin/st -f \"Anonymous Pro:size=14\""
--myTerminal = "/usr/local/bin/st -f \":size=12\""

myScreensaver:: String
myScreensaver = "/run/current-system/sw/bin/xscreensaver-command --lock"

mySelectScreenshot :: String
mySelectScreenshot = "scrot -s"

myScreenshot :: String
myScreenshot = "scrot"

myLauncher :: String
myLauncher = "$(dmenu_run -nf '#b19cd9' -nb '#222222' -sf '#add8e6' -sb '#444444' \
             \-x 750 -y 400 -w 400 -l 15 -p '> ')"

--}}}



-- Settings {{{1
---------------------------------------------------------------------------------------------------

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..8 :: Int]


-- Crazy ass type signiture... so just let the compiler infer it
myLayoutHook = onWorkspaces ["5"] gimpLayoutFirst $
               onWorkspaces ["1", "8"] floatLayoutFirst $
               defaultLayoutOrder
  where
    -- orderings
    gimpLayoutFirst = named "Gimp" gimp
                  ||| named "Default" tiled
                  ||| named "Float" float
                  ||| named "Full" full
    floatLayoutFirst = named "Float" float
                   ||| named "Full" full
                   ||| named "Default" tiled
    defaultLayoutOrder = named "Default" tiled
                     ||| named "Float" float
                     ||| named "Full" full

    -- layouts
    tiled = maximize $ boringWindows $ avoidStruts $ noBorders $
            smartSpacing 3 (Tall 1 (3/100) (1/3))
            --smartSpacing 3 (Tall 1 (3/100) (1/2))
    float =  avoidStruts $ simplestFloat
    full = smartBorders . avoidStruts $ Full
    gimp = withIM (0.11)
                  (Role "gimp-toolbox")
                  (reflectHoriz (withIM (0.15)
                                        (Role "gimp-dock")
                                        (Mirror (Tall 1 (3/100) (1/2)) ||| Full)))


-- use 'xprop' to determine properties
myManageHook = (composeAll . concat $
  [ [ title                      =? x --> doCenterFloatToAll | x <- important ]
  , [ className                  =? x --> doShift "5"        | x <- cShift5   ]
  , [ className                  =? x --> doShift "8"        | x <- cShift8   ]
  ]) <+> (composeOne . concat $
  [ [ className                  =? x  -?> doCenterFloat'    | x <- cCenterF  ]
  , [ title                      =? x  -?> doCenterFloat'    | x <- tCenterF  ]
  , [ className                  =? x  -?> doCenterFloat'    | x <- pcFloat   ]
  , [ title                      =? x  -?> doCenterFloat'    | x <- ptCenterF ]
  , [ className                  =? x  -?> doFloat'          | x <- cFloat    ]
  , [ title                      =? x  -?> doFloat'          | x <- tFloat    ]
  , [ className =? "" <&&> title =? "" -?> doFullFloat'                       ]
  , [ isFullscreen                     -?> doFullFloat'                       ]
  , [ isDialog                         -?> doCenterFloat'                     ]
--, [ return True                      -?> doF W.swapDown                     ]
  , [ return True                      -?> doMaster                           ]
  ]) where
       -- program families that shift
       important   = ["XMPP Message Error"]
       cShift5     = ["Gimp"]
       cShift8     = ["Genymotion", "Genymotion Player"]

       -- program families that float
       cCenterF    = ["Kaffeine", "MPlayer", "xine", "Xmessage", "Xdialog",
                      "Arandr", "qpdfview", "Xfe", "Display", "feh", "termite",
                      "wicd-client.py"]
       tCenterF    = ["Plugins", "Add-ons"]
       ptCenterF   = ["About", "Open File", "Save As"]
       pcFloat     = ["Xfce4-"]
       cFloat      = ["Xfrun4", "Wine", "Nm-applet", "Envy24control", "Audacious",
                      "Orage", "X64"]
       tFloat      = ["glxgears"]

       -- actions
       doMaster           = doF W.shiftMaster -- new windows go to top
       doFloat'           = doFloat -- <+> doMaster
       doCenterFloat'     = doCenterFloat -- <+> doMaster
       doFullFloat'       = doFullFloat -- <+> doMaster
       doCopyToAll        = ask >>= doF . \w ->
                             (\ws -> foldr ($) ws (map (copyWindow w) myWorkspaces))
       doCenterFloatToAll = doCopyToAll <+> doCenterFloat'



-- icon directory
iconDir :: String
iconDir = "/home/spydr/.xmonad/info_bars/xbm/"

mybgColor :: String
mybgColor = "#000000"

mybgColor_tag :: String
--mybgColor_tag = "#222244"
--mybgColor_tag = "#332233"
mybgColor_tag = "#232b2b"

myfgColor :: String
myfgColor = "#555555"

mybgColor_urgent :: String
mybgColor_urgent = "#990000"

myfgColor_current :: String
myfgColor_current = "#4a6023"

myfgColor_visible :: String
myfgColor_visible = "#666633"

myfgColor_hidden :: String
myfgColor_hidden = "#555555"

myfgColor_noWindows :: String
myfgColor_noWindows = "#555555"

wrapIcon :: String -> String
wrapIcon icon = "<icon=" ++ iconDir ++ icon ++ "/>"

makeTag :: String -> String
makeTag s = (wrapColor (wrapIcon "corner-lt.xbm")) ++
            (pad s) ++
            (wrapColor (wrapIcon "corner-rb.xbm"))
  where wrapColor c = "<fc=" ++ mybgColor ++ "," ++ mybgColor_tag ++ ">" ++ c ++ "</fc>"
        pad s = " " ++ s ++ " "


myLogHook :: Handle -> X ()
myLogHook h = (dynamicLogWithPP $ defaultPP {
    ppOrder           = \(ws:l:t:_) -> [ws,l]
  , ppUrgent          = xmobarColor mybgColor mybgColor_urgent

  , ppCurrent         = xmobarColor myfgColor_current mybgColor .
                          (\ _ -> wrapIcon "dot_full.xbm")
  , ppHidden          = xmobarColor myfgColor_hidden mybgColor .
                          (\ _ -> wrapIcon "dot_full.xbm")
  , ppVisible         = xmobarColor myfgColor_visible mybgColor .
                          (\ _ -> wrapIcon "dot_full.xbm")
  , ppHiddenNoWindows = xmobarColor myfgColor_noWindows mybgColor .
                          (\ _ -> wrapIcon "dot_empty.xbm")

  , ppLayout          = xmobarColor myfgColor mybgColor_tag .
                          (\s ->
                            case s of
                                 "Default"  -> makeTag $ wrapIcon "layout_tall.xbm"
                                 "Full"     -> makeTag $ wrapIcon "layout_full.xbm"
                                 "Float"    -> makeTag $ wrapIcon "mouse.xbm"
                                 "Gimp"     -> makeTag $ "[G]"
                                 _          -> makeTag $ "[?]"
                          )

  , ppSep             = "  "
  , ppWsSep           = "  "
  , ppOutput          = hPutStrLn h
})

--}}}



-- Appearance {{{1
---------------------------------------------------------------------------------------------------

myBorderWidth :: Dimension
myBorderWidth = 1

myNormalBorderColor :: String
myNormalBorderColor  = "#555555"

myFocusedBorderColor :: String
myFocusedBorderColor = "#550000"

xmobarTitleColor :: String
xmobarTitleColor = "#007700"

xmobarCurrentWorkspaceColor :: String
xmobarCurrentWorkspaceColor = "#550000"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig :: Theme
tabConfig = defaultTheme { activeBorderColor   = "#7C7C7C"
                         , activeTextColor     = "#CEFFAC"
                         , activeColor         = "#000000"
                         , inactiveBorderColor = "#7C7C7C"
                         , inactiveTextColor   = "#EEEEEE"
                         , inactiveColor       = "#000000"
                         }

--}}}



-- Keybindings {{{1
---------------------------------------------------------------------------------------------------

-- mod1Mask -> "left alt"
-- mod3Mask -> "right alt"
-- mod4Mask -> "windows key"
myModMask :: Foreign.C.Types.CUInt
myModMask = mod4Mask


myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [

    -- Program Bindings {{{2

    -- Spawn default terminal.
    ((modMask, xK_Return),
     spawn $ XMonad.terminal conf)

    -- Run default screensaver.
  , ((modMask .|. controlMask, xK_l),
     spawn myScreensaver)

    -- Spawn default launcher.
  , ((modMask, xK_p),
     spawn myLauncher)

    -- Take a selective screenshot.
  , ((modMask .|. shiftMask, xK_p),
     spawn mySelectScreenshot)

    -- Take a full screenshot
  , ((modMask .|. controlMask .|. shiftMask, xK_p),
     spawn myScreenshot)

    -- Mute volume.
  , ((0, 0x1008ff12),
     spawn "amixer -c PCH -q set Master toggle")

    -- Decrease volume.
  , ((0, 0x1008ff11),
     spawn "amixer -c PCH -q set Master 2%-")

    -- Increase volume.
  , ((0, 0x1008ff13),
     spawn "amixer -c PCH -q set Master 2%+")

    -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")

    -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")

    -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")

    -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

    -- Adjust screen brightness
  , ((0, 0x1008ff02),
     spawn "/home/spydr/.xmonad/scripts/increase-backlight.sh")
  , ((0, 0x1008ff03),
     spawn "/home/spydr/.xmonad/scripts/decrease-backlight.sh")

  --}}}

  -- Window management bindings{{{2

    -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

    -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

    -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

    -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

    -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

    -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

    -- Swap the focused window and the master window.
  , ((modMask .|. shiftMask, xK_Return),
     windows W.swapMaster)

    -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

    -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

    -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

    -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

    -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

    -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

    -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

    -- Restart xmonad.
  , ((modMask, xK_q),
     --spawn "xmonad --recompile;killall xmobar;xmonad --restart")
     restart "xmonad" True)

  --}}}

  ]

  ++

  -- Workspace navigation {{{2

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_8]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

  -- }}}

  ++

  -- Screen navigation {{{2

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

  --}}}

--}}}



-- Mouse Bindings {{{1
---------------------------------------------------------------------------------------------------

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

--}}}



-- Main {{{1

main :: IO ()
main = do
  xmobar <- spawnPipe "/run/current-system/sw/bin/xmobar /home/spydr/.xmonad/info_bars/xmobar.hs"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
      terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook <+> manageHook defaultConfig
    , handleEventHook    = fullscreenEventHook
    , logHook            = (myLogHook xmobar) -- <+> (fadeInactiveLogHook 0.8)
    , startupHook        = setWMName "LG3D"
  }

--}}}



