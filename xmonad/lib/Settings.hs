
-----------------------------------------------------------------------------------------[ Module ]
--{1

module Settings
  ( myHomeDir
  , myScripts
  , myIconDir
  , myBinDir

  , myModMask
  , myFocusFollowsMouse
  , myClickJustFocuses
  , myWorkspaces

  , myTerminal
  , myLockScreen
  , myScreenshot
  , mySelectScreenshot
  , myLauncher
  , myClipboard
  , myStatusBar
  , myConfirm
  ) where

import XMonad
import XMonad.Util.Run

import Control.Monad

import Foreign.C.Types

--}

------------------------------------------------------------------------------------------[ Paths ]
--{1

username  = "spydr"
myHomeDir = "/home/" ++ username ++ "/"
myScripts = myHomeDir ++ "dotfiles/xmonad/scripts/"
myIconDir = myHomeDir ++ "dotfiles/xmonad/icons/xbm_16/"
myBinDir  = myHomeDir ++ "dotfiles/scripts/bin/"

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

screenWidth :: Int
screenWidth = 1920

screenHeight :: Int
screenHeight = 1080

--}

---------------------------------------------------------------------------------[ Util Functions ]
--{1

shellCmd :: [String] -> String
shellCmd args = "$(" ++ unwords args ++ ")"

--}

-------------------------------------------------------------------------------[ Default Programs ]
--{1

myTerminal :: String
myTerminal = "/run/current-system/sw/bin/st"

myLockScreen:: String
myLockScreen = myHomeDir ++ "dotfiles/scripts/bin/lock.sh"

myScreenshot :: String
myScreenshot = "scrot"

mySelectScreenshot :: String
mySelectScreenshot = "scrot -s"

myLauncher :: String
myLauncher = let fn = 12   -- font size
                 w  = 400  -- width in pixels
                 h  = 20   -- height in lines
             in shellCmd [ myBinDir ++ "dmenu_whitelist_run"
                         , "-fn" , "'Source Code Pro for Powerline:pixelsize=" ++ show fn ++ "'"
                         , "-nf" , "'#b19cd9'"
                         , "-nb" , "'#222222'"
                         , "-sf" , "'#add8e6'"
                         , "-sb" , "'#444444'"
                         , "-x"  , show $ (div screenWidth 2) - (div w 2)
                         , "-y"  , show $ (div screenHeight 2) - (div (h * fn) 2)
                         , "-w"  , show w
                         , "-l"  , show h
                         , "-p"  , "'> '"
                         ]

myConfirm :: String -> X () -> X ()
myConfirm msg f = (runProcessWithInput ("dmenu")
                                       (cmdOpts msg)
                                       (unlines $ ["Yes", "No"]))
              >>= (\r -> when (r=="Yes\n") f)
  where
    cmdOpts :: String -> [String]
    cmdOpts msg = let fn = 16
                      h  = 30
                      w  = 400
                  in [ "-fn" , show ("Source Code Pro for Powerline:pixelsize=" ++ show fn)
                     , "-nf" , "#b19cd9"
                     , "-nb" , "#222222"
                     , "-sf" , "#add8e6"
                     , "-sb" , "#444444"
                     , "-x"  , show $ (div screenWidth 2) - (div w 2)
                     , "-y"  , show $ (div screenHeight 2) - (div (h * fn) 2)
                     , "-w"  , show w
                     , "-l"  , show h
                     , "-p"  , msg
                     ]

myClipboard :: String
myClipboard = let fn  = 12   -- font size
                  w   = 600  -- width in pixels
                  pad = 26   -- padding in pixels
              in shellCmd [ "clipmenu"
                          , "-fn" , "'Source Code Pro for Powerline:pixelsize=" ++ show fn ++ "'"
                          , "-nf" , "'#b19cd9'"
                          , "-nb" , "'#222222'"
                          , "-sf" , "'#add8e6'"
                          , "-sb" , "'#444444'"
                          , "-x"  , show $ screenWidth - w
                          , "-y"  , show $ pad
                          , "-w"  , show w
                          , "-l"  , show $ ((fromIntegral screenHeight) - (2 * pad))
                                         / (1.52 * fn)
                          , "-p"  , "'CLIP: '"
                          ]

myStatusBar :: String
myStatusBar = "/run/current-system/sw/bin/xmobar " ++
              myHomeDir ++ "dotfiles/xmonad/info_bars/xmobar.conf"

--}



