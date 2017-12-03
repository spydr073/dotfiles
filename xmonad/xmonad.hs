
-- ────────────────────────────────────────────────────────────────────────────────────────────────
--
--    ██╗  ██╗███╗   ███╗ ██████╗ ███╗   ██╗ █████╗ ██████╗
--    ╚██╗██╔╝████╗ ████║██╔═══██╗████╗  ██║██╔══██╗██╔══██╗
--     ╚███╔╝ ██╔████╔██║██║   ██║██╔██╗ ██║███████║██║  ██║
--     ██╔██╗ ██║╚██╔╝██║██║   ██║██║╚██╗██║██╔══██║██║  ██║
--    ██╔╝ ██╗██║ ╚═╝ ██║╚██████╔╝██║ ╚████║██║  ██║██████╔╝
--    ╚═╝  ╚═╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝  ╚═╝╚═════╝
--
-- ────────────────────────────────────────────────────────────────────────────────────────────────

----------------------------------------------------------------------------------------[ Imports ]
--{1

import XMonad

import XMonad.Util.Run

import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks

import Settings
import Hooks
import Bindings
import Themes

--}

-------------------------------------------------------------------------------------------[ Main ]
--{1

main :: IO ()
main = do
  xmobar <- spawnPipe myStatusBar
  xmonad $ def {
      terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys               = myKeyBindings
    , mouseBindings      = myMouseBindings
    , manageHook         = manageDocks <+> myManageHook
    , layoutHook         = avoidStruts $ myLayoutHook
    , handleEventHook    = handleEventHook def <+> docksEventHook
    , logHook            = myLogHook xmobar
    , startupHook        = myStartupHook
  }

--}


