import XMonad
import XMonad.Config.Desktop
import Data.Monoid
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- system
import System.IO (hPutStrLn)

-- util
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat) 
import XMonad.Hooks.Place (placeHook, withGaps, smart)

-- layout 
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing (spacing) 
import XMonad.Layout.GridVariants (Grid(Grid))

-- config
myModMask       = mod4Mask  -- Sets modkey to super/windows key
myTerminal      = "urxvt"   -- Sets default terminal
myBorderWidth   = 2         -- Sets border width for windows
myNormalBorderColor = "#839496"
myFocusedBorderColor = "#268BD2"
myppCurrent = "#c3e88d"
myppVisible = "#c3e88d"
myppHidden = "#82AAFF"
myppHiddenNoWindows = "#F07178"
myppTitle = "#d0d0d0"
myppSep = "#666666"
myppUrgent = "#C45500"
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

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
                        { ppOutput = \x -> hPutStrLn xmproc x
                        , ppCurrent = xmobarColor myppCurrent "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor myppVisible ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor myppHidden "" . wrap "+" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor  myppHiddenNoWindows ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor  myppTitle "" . shorten 80     -- Title of active window in xmobar
                        , ppSep =  "<fc=myppSep> | </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor  myppUrgent "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
                    }

-- Startup hook
myStartupHook = do
      spawnOnce "urxvtd &"
      spawnOnce "feh --no-fehbg --bg-center --image-bg '#353535' '/home/djwilcox/.wallpaper/freebsd.png'"

-- layout
myLayout = avoidStruts ( monocle ||| tiled ||| grid  ) ||| monocle
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = spacing 12 $ Tall nmaster delta ratio
     
     -- grid
     grid = spacing 12 $ Grid (16/10) 

     -- monocle
     monocle = smartBorders (Full)

     -- The default number of windows in the master pane
     nmaster = 1
     
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
