import XMonad
import XMonad.Hooks.DynamicLog (xmobar)

-- import XMonad.Util.Run (hPutStrLn, safeSpawn, spawnPipe, unsafeSpawn)

myConfig =
  def
    { modMask = mod4Mask,
      terminal = "st -e tmux",
      focusedBorderColor = "#affe69",
      borderWidth = 2
    }



main = xmonad =<< xmobar myConfig
