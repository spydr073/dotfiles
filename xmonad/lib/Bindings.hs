
-----------------------------------------------------------------------------------------[ Module ]
--{1

module Bindings
  ( myKeyBindings
  , myMouseBindings
  ) where

import XMonad
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W

import System.Exit
import qualified Data.Map as M

import Settings

--}

---------------------------------------------------------------------------------------[ Keyboard ]
--{1

myKeyBindings conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    --{2 Programs

    -- Spawn default terminal.
    ((modMask, xK_Return),
     spawn $ XMonad.terminal conf)

    -- Lock the screen
  , ((modMask .|. controlMask, xK_l),
     spawn myLockScreen)

    -- Spawn default launcher.
  , ((modMask, xK_p),
     spawn myLauncher)

    -- Take a selective screenshot.
  , ((modMask .|. shiftMask, xK_s),
     spawn mySelectScreenshot)

    -- Take a full screenshot
  , ((modMask .|. controlMask .|. shiftMask, xK_S),
     spawn myScreenshot)

    -- Adjust volume level.
  , ((0, 0x1008ff11),
     spawn "/home/spydr/dotfiles/xmonad/scripts/volume.sh decrease")
  , ((0, 0x1008ff13),
     spawn "/home/spydr/dotfiles/xmonad/scripts/volume.sh increase")

    -- Adjust screen brightness
  , ((0, 0x1008ff02),
     spawn "light -A 10")
  , ((0, 0x1008ff03),
     spawn "light -U 10")

  --}

  --{2 Window management

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

    -- Toggle the status bar.
  , ((modMask, xK_b), sendMessage ToggleStruts)

    -- Quit xmonad
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

    -- Restart xmonad and xmobar
  , ((modMask .|. shiftMask, xK_r),
     spawn "/home/spydr/dotfiles/scripts/bin/reloadXMonad.sh")

  --}
  ]
  ++
  --{2 Workspaces
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_8]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]
  --}
   ++
  --{2 Screens
  -- mod-{q,w,e}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{q,w,e}, Move client to screen 1, 2, or 3
  [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_q, xK_w, xK_e] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]
  --}

--}

------------------------------------------------------------------------------------------[ Mouse ]
--{1

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ -- Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))
  , ((0, 6),
     (\w -> focus w >> mouseMoveWindow w))

    -- Raise the window to the top of the stack
  , ((modMask, button2),
     (\w -> focus w >> windows W.swapMaster))

    -- Set the window to floating mode and resize by dragging
  , ((modMask, button3),
     (\w -> focus w >> mouseResizeWindow w))
  , ((0, 7),
     (\w -> focus w >> mouseResizeWindow w))
  ]

--}


