
-----------------------------------------------------------------------------------------[ Module ]
--{1

module Settings
  ( myHomeDir
  , myScripts
  , myIconDir

  , myModMask
  , myFocusFollowsMouse
  , myClickJustFocuses
  , myWorkspaces

  , myTerminal
  , myLockScreen
  , myScreenshot
  , mySelectScreenshot
  , myLauncher
  , myStatusBar
  ) where

import XMonad

import Foreign.C.Types

--}

------------------------------------------------------------------------------------------[ Paths ]
--{1

username  = "spydr"
myHomeDir = "/home/" ++ username ++ "/"
myScripts = myHomeDir ++ "dotfiles/xmonad/scripts/"
myIconDir = myHomeDir ++ "dotfiles/xmonad/info_bars/xbm/"

--}

------------------------------------------------------------------------------------[ XMonad Vars ]
--{1

myModMask :: Foreign.C.Types.CUInt
myModMask = mod4Mask

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses  = True

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..8 :: Int]

--}

-------------------------------------------------------------------------------[ Default Programs ]
--{1

myTerminal :: String
myTerminal = "/run/current-system/sw/bin/termite"

myLockScreen:: String
myLockScreen = "/run/current-system/sw/bin/slimlock"

myScreenshot :: String
myScreenshot = "scrot"

mySelectScreenshot :: String
mySelectScreenshot = "scrot -s"

myLauncher :: String
myLauncher = "$(dmenu_run -nf '#b19cd9' -nb '#222222' -sf '#add8e6' -sb '#444444' \
             \-x 750 -y 400 -w 400 -l 15 -p '> ')"

myStatusBar :: String
myStatusBar = "/run/current-system/sw/bin/xmobar " ++
              myHomeDir ++ "dotfiles/xmonad/info_bars/xmobar.conf"

--}



