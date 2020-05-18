-------------------------------------------------------------------------------
--                  __  ____  __                       _                     --
--                  \ \/ /  \/  | ___  _ __   __ _  __| |                    --
--                   \  /| |\/| |/ _ \| '_ \ / _` |/ _` |                    --
--                   /  \| |  | | (_) | | | | (_| | (_| |                    --
--                  /_/\_\_|  |_|\___/|_| |_|\__,_|\__,_|                    --
--                                                                           --
-------------------------------------------------------------------------------

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
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)  

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, docksStartupHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat, doRectFloat) 
import XMonad.Hooks.Place (placeHook, withGaps, smart)

-- actions
import XMonad.Actions.CopyWindow

-- layout 
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing (spacing) 
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ResizableTile

------------------------------------------------------------------------
-- config
------------------------------------------------------------------------

myModMask       = mod4Mask  -- Sets modkey to super/windows key
myTerminal      = "urxvtc"   -- Sets default terminal
myBorderWidth   = 2         -- Sets border width for windows
myNormalBorderColor = "#839496"
myFocusedBorderColor = "#268BD2"
myppCurrent = "#268BD2"
myppVisible = "#268BD2"
myppHidden = "#B58900"
myppHiddenNoWindows = "#93A1A1"
myppTitle = "#FDF6E3"
myppUrgent = "#DC322F"
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- Startup hook
------------------------------------------------------------------------

myStartupHook = do
      spawnOnce "urxvtd &"
      spawnOnce "feh --no-fehbg --bg-center --image-bg '#353535' '/home/djwilcox/.wallpaper/freebsd.png'"

------------------------------------------------------------------------
-- layout
------------------------------------------------------------------------
-- using toggleStruts with monocle
myLayout = avoidStruts ( full ||| tiled ||| grid )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled = spacing 12 $ ResizableTall 1 (3/100) (1/2) []    

     -- grid
     grid = spacing 12 $ Grid (16/10) 

     -- full
     full = smartBorders (Full)

     -- The default number of windows in the master pane
     nmaster = 1
     
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:
------------------------------------------------------------------------

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "mpv"            --> doRectFloat (W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , title =? "Picture-In-Picture" --> doFloat
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------
myKeys =
    [("S-C-a", windows copyToAll)   -- copy window to all workspaces
     , ("S-C-z", killAllOtherCopies)  -- kill copies of window on other workspaces
     , ("M-a", sendMessage MirrorExpand)
     , ("M-z", sendMessage MirrorShrink)
     , ("M-s", sendMessage ToggleStruts)
    ]

------------------------------------------------------------------------
-- main
------------------------------------------------------------------------

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
                        , ppSep =  "<fc=#586E75> | </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor  myppUrgent "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
                    } `additionalKeysP`         myKeys

------------------------------------------------------------------------
-- help
------------------------------------------------------------------------

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
