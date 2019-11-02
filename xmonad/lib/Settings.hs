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
myLockScreen = myHomeDir ++ "dotfiles/scripts/bin/lockscreen"

myScreenshot :: String
myScreenshot = "scrot"

mySelectScreenshot :: String
mySelectScreenshot = "scrot -s"

dmenuBox :: Int -> Int -> Int -> String -> [String]
dmenuBox fn w h msg =
    [ "-fn" , show $ "Source Code Pro for Powerline:pixelsize=" ++ show fn
    , "-x"  , show $ (div screenWidth 2) - (div w 2)
    , "-y"  , show $ (div screenHeight 4)
    , "-w"  , show w
    , "-l"  , show h
    , "-p"  , msg
    ]

myLauncher :: String
myLauncher = shellCmd $ [ myBinDir ++ "dmenu_whitelist_run"
                        , "-z"
                        , "-q"
                        , "-o"   , "0.8"
                        , "-dim" , "0.5"
                        , "-nf"  , show "#b19cd9"
                        , "-nb"  , show "#222222"
                        , "-sf"  , show "#add8e6"
                        , "-sb"  , show "#444444"
                        ] ++ (dmenuBox 12 400 20 $ show "> ")

myConfirm :: String -> X () -> X ()
myConfirm msg f = (runProcessWithInput ("dmenu")
                                       (cmdOpts msg)
                                       (unlines ["Yes", "No"]))
              >>= (\r -> when (r=="Yes\n") f)
  where
    cmdOpts :: String -> [String]
    cmdOpts msg = [ "-z"
                  , "-o"   , "0.8"
                  , "-dim" , "0.5"
                  , "-nf"  , "#b19cd9"
                  , "-nb"  , "#666666"
                  , "-sf"  , "#add8e6"
                  , "-sb"  , "#99235a"
                  ] ++ (dmenuBox 24 400 30 msg)

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



