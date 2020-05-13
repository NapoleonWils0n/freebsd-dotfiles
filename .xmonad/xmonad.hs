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

main = xmonad $ def
    { borderWidth        = myBorderWidth
    , terminal           = myTerminal
    , modMask            = myModMask
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#ff0000" }
