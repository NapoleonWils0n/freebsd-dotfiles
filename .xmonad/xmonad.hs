--
-- An example, simple ~/.xmonad/xmonad.hs file.
-- It overrides a few basic settings, reusing all the other defaults.
--

import XMonad
import XMonad.Config.Desktop
import Data.Monoid
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- layout 
import XMonad.Layout.NoBorders

myModMask       = mod4Mask  -- Sets modkey to super/windows key
myTerminal      = "urxvt"      -- Sets default terminal
myBorderWidth   = 1         -- Sets border width for windows
myLayout = tiled ||| Mirror tiled ||| noBorders (Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


-- main
main = xmonad $ def
    { borderWidth        = myBorderWidth
    , terminal           = myTerminal
    , modMask            = myModMask
    , layoutHook         = myLayout
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#ff0000" }
