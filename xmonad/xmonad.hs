import XMonad
import XMonad.Config.Desktop

main :: IO ()
main = xmonad desktopConfig
    { terminal = "kitty -1"
    , modMask  = mod4Mask
    }

