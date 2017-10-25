
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



-- ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Imports ]
--{{{1
import XMonad
import Data.Monoid
import System.IO
import System.Exit

import XMonad.Util.EZConfig
import XMonad.Util.Run

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.Minimize

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.TopicSpace
import XMonad.Actions.WithAll

import XMonad.Layout.NoBorders

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.Set        as S

import Settings
import Hooks
import Bindings
import Themes
--}}}


-- ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Main ]
--{{{1
main :: IO ()
main = do
  xmobar <- spawnPipe myStatusBar
  xmonad $ withUrgencyHook NoUrgencyHook $ def {
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
--}}}


