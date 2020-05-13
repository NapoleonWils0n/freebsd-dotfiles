--
-- An example, simple ~/.xmonad/xmonad.hs file.
-- It overrides a few basic settings, reusing all the other defaults.
--

import XMonad
import Data.Monoid
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- config
import XMonad.Config.Desktop

-- system
import System.IO (hPutStrLn)

-- util
-- import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

-- layout 
import XMonad.Layout.NoBorders

-- config
myModMask       = mod4Mask  -- Sets modkey to super/windows key
myTerminal      = "urxvt"   -- Sets default terminal
myBorderWidth   = 1         -- Sets border width for windows
myNormalBorderColor = "#cccccc"
myFocusedBorderColor = "#ff0000"

-- main
main = do
    xmproc <- spawnPipe "/usr/local/bin/xmobar -x 0 /home/djwilcox/.config/xmobar/xmobarrc"
    xmonad $ ewmh desktopConfig
        { manageHook = manageDocks <+> manageHook desktopConfig
        , startupHook        = myStartupHook
        , layoutHook         = myLayout
        , borderWidth        = myBorderWidth
        , terminal           = myTerminal
        , modMask            = myModMask
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
    		    }

-- Startup hook
myStartupHook = do
      spawnOnce "urxvtd &"

-- layout
myLayout = avoidStruts ( smartBorders (tiled) ||| Mirror tiled ||| smartBorders (Full) ) ||| smartBorders (Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
