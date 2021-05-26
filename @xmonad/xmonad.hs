import qualified Data.Map                      as M
import           Data.Monoid                    ( All
                                                , Endo
                                                )
import           System.Exit                    ( exitSuccess )
import           XMonad
import           XMonad.Actions.DwmPromote      ( dwmpromote )
import           XMonad.Actions.GridSelect      ( bringSelected
                                                , goToSelected
                                                , gridselect
                                                )
import           XMonad.Hooks.EwmhDesktops      ( ewmhDesktopsEventHook )
import           XMonad.Hooks.ManageDocks       ( Direction2D(D, L, R, U)
                                                , ToggleStruts(ToggleStruts)
                                                , avoidStruts
                                                , docks
                                                )
import           XMonad.Layout.Gaps             ( gaps )
import           XMonad.Layout.LayoutHints      ( layoutHintsWithPlacement )
import           XMonad.Layout.SimplestFloat    ( simplestFloat )
import           XMonad.Layout.TabBarDecoration ( simpleTabBar )
import qualified XMonad.StackSet               as W
import           XMonad.Util.Run                ( spawnPipe )


spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect def lst >>= flip whenJust spawn

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig { XMonad.modMask = modm } =
  M.fromList
    $  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
       , ((modm, xK_p)                   , spawn "dmenu_run")
       , ((modm .|. shiftMask, xK_c)     , kill)
       , ((modm, xK_space)               , sendMessage NextLayout)
       , ((modm .|. shiftMask, xK_space) , setLayout $ XMonad.layoutHook conf)
       , ((modm, xK_n)                   , refresh)
       , ((modm, xK_Tab)                 , windows W.focusDown)
       , ((modm, xK_j)                   , windows W.focusDown)
       , ((modm, xK_k)                   , windows W.focusUp)
       , ((modm, xK_m)                   , windows W.focusMaster)
       , ((modm, xK_Return)              , dwmpromote)
       , ((modm .|. shiftMask, xK_j)     , windows W.swapDown)
       , ((modm .|. shiftMask, xK_k)     , windows W.swapUp)
       , ((modm, xK_h)                   , sendMessage Shrink)
       , ((modm, xK_l)                   , sendMessage Expand)
       , ((modm, xK_t)                   , withFocused $ windows . W.sink)
       , ((modm, xK_comma)               , sendMessage (IncMasterN 1))
       , ((modm, xK_period)              , sendMessage (IncMasterN (-1)))
       , ((modm, xK_b)                   , sendMessage ToggleStruts)
       , ((modm .|. shiftMask, xK_q)     , io exitSuccess)
       , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
       , ( (modm, xK_s)
         , spawnSelected'
           [ ("st"                , "st")
           , ("Brave"             , "brave")
           , ("MµPDF"             , "mupdf")
           , ("Obsidian"          , "obsidian")
           , ("Visual Studio Code", "code")
           , ("lite"              , "lite")
           , ("DeaDBeeF"          , "deadbeef")
           , ("Spotify"           , "spotify")
           ]
         )
       , ((modm, xK_grave)    , goToSelected def)
       , ((modm, xK_BackSpace), bringSelected def)
       ]
    ++ [ ((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]
    ++ [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_u, xK_i, xK_o] [0 ..]
       , (f  , m ) <- [(W.view, 0), (W.shift, shiftMask)]
       ]

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList
        -- mod-button1, Set the window to floating mode and move by dragging
  [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  , -- mod-button2, Raise the window to the top of the stack
    ((modm, button2), \w -> focus w >> windows W.shiftMaster)
  , -- mod-button3, Set the window to floating mode and resize by dragging
    ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  ]

-- you may also bind events to the mouse scroll wheel (button4 and button5)

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
  [ className =? "MPlayer" --> doFloat
  , resource =? "desktop_window" --> doIgnore
  , resource =? "kdesktop" --> doIgnore
  ]


main :: IO ()
main = do
  xmproc <- spawnPipe "/home/xha/.cabal/bin/xmobar -x 0 /home/xha/.config/xmobar/xmobarrc"
  xmonad $ docks def
    { terminal           = "st"
    , focusFollowsMouse  = True
    , borderWidth        = 3
    , modMask            = mod4Mask
    , workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , normalBorderColor  = "#C9D6DD"
    , focusedBorderColor = "#F3E04E"
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , layoutHook = let tiled   = myGaps $ layoutHintsWithPlacement (0, 0) $ Tall nmaster delta ratio
                       nmaster = 1
                       ratio   = toRational (sqrt 5 - 1) / 2
                       delta   = 3 / 100
                       myGaps  = gaps [(U, gapSize), (R, gapSize), (L, gapSize), (D, gapSize)]
                       gapSize = 10
                   in  avoidStruts tiled
                       ||| avoidStruts (Mirror tiled)
                       ||| Full
                       ||| simpleTabBar tiled
                       ||| simplestFloat
    , manageHook         = myManageHook
    , handleEventHook    = ewmhDesktopsEventHook
    , logHook            = return ()
    , startupHook        = return ()
    }
