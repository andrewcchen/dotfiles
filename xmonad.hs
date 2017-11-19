{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import XMonad
import XMonad.Config.Kde (kdeConfig)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, ppOutput, ppTitle, xmobarColor, shorten)
import XMonad.Hooks.EwmhDesktops(ewmh, fullscreenEventHook, ewmhDesktopsEventHook)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docksEventHook)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Layout (Tall(Tall), Full(Full))
import XMonad.Layout.NoBorders --(lessBorders, Ambiguity(OnlyFloat))
import XMonad.Layout.ToggleLayouts (toggleLayouts, ToggleLayout(ToggleLayout))
import XMonad.Layout.ThreeColumns --(ThreeCol(ThreeCol))
import XMonad.Util.EZConfig --(additionalKeys)
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_MonBrightnessUp, xF86XK_MonBrightnessDown)
import System.IO (hPutStrLn)
import Text.Printf (printf)

import XMonad.Layout
import qualified XMonad.StackSet as W
import Control.Monad

data TallDelta a = TallDelta !Int !Rational !Rational deriving (Show, Read)

instance LayoutClass TallDelta a where
    pureLayout (TallDelta delta _ frac) r s = zip ws rs
        where ws = W.integrate s
              rs = tile frac r nmaster total
              nmaster = min total $ max 0 $ total `quot` 2 + delta
              total = length ws

    pureMessage (TallDelta delta inc frac) m =
        msum [ fmap resize (fromMessage m)
             , fmap incmastern (fromMessage m)]
        where resize Shrink = TallDelta delta inc (max 0 $ frac-inc)
              resize Expand = TallDelta delta inc (min 1 $ frac+inc)
              incmastern (IncMasterN d) = TallDelta (delta+d) inc frac

    description _ = "Tall"

main = do
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"
    mapM spawn myExecute
    xmonad $ ewmh $ def -- kdeConfig
        { handleEventHook = myHandleEventHook
        , layoutHook = myLayoutHook
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" -- . shorten 50
            }
        , manageHook = myManageHook
        , modMask = myModMask
        , terminal = "konsole --workdir ~"
        , workspaces = myWorkspaces
        } `additionalKeys` myAdditionalKeys
        --  `removeMouseBindings`
        --    [ (myModMask, button1)
        --    , (myModMask, button3)
        --    ]


myRunOnce exe = printf fmt exe exe
    where fmt = "if [ -z \"$(pidof %s)\" ]; then exec %s; fi"

myExecute =
    --[ "exec setxkbmap custom"
    --, "sleep 1; exec xcape -t 200 -e Hyper_L=space"
    [ myRunOnce "keepassxc"
    ]

myHandleEventHook = docksEventHook
                <+> fullscreenEventHook
                <+> ewmhDesktopsEventHook
                <+> handleEventHook def

layout = toggleLayouts Full $ twoCol ||| Mirror twoCol ||| threeCol
    where
    twoCol = TallDelta 0 (4/100) (1/2)
    threeCol = ThreeColMid 1 (4/100) (2/5)

myLayoutHook = lessBorders Screen
             $ avoidStruts
             $ layout

myManageHook = manageDocks
           <+> (className =? "trayer" --> doIgnore)
           <+> (className =? "plasmashell" --> doFloat)
           <+> (title =? "Auto-Type - KeePassXC" --> doFloat)
           -- KeePassXC starts with this title, before changing it immediately
           <+> (title =? "KeePassXC" --> doShift "F12")
           <+> (className =? "discord" --> doShift "F11")
           <+> (className =? "konversation" --> doShift "F10")
           <+> (isFullscreen --> doFullFloat)
           <+> manageHook def

myModMask = mod1Mask -- Alt

myWorkspaces = map show [1..9] ++ (map snd myExtraWorkspaces)

myExtraWorkspaces =
    [ (xK_0, "10")
    , (xK_minus, "11")
    , (xK_equal, "12")
    , (xK_F1, "F1")
    , (xK_F2, "F2")
    , (xK_F3, "F3")
    , (xK_F4, "F4")
    , (xK_F5, "F5")
    , (xK_F6, "F6")
    , (xK_F7, "F7")
    , (xK_F8, "F8")
    , (xK_F9, "F9")
    , (xK_F10, "F10")
    , (xK_F11, "F11")
    , (xK_F12, "F12")
    ]

myFocusedDoFullFloat windowset =
    case W.stack $ W.workspace $ W.current windowset of
        Nothing -> windowset
        Just stack -> W.float (W.focus stack) (W.RationalRect 0 0 1 1) windowset

myAdditionalKeys =
    [ ((myModMask .|. shiftMask, xK_q), return ())
    , ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; systemctl suspend")
    -- , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
    -- , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    , ((myModMask, xK_f), sendMessage ToggleLayout)
    , ((myModMask .|. shiftMask, xK_f), windows myFocusedDoFullFloat)
    ] ++ [
        ((myModMask, key), (windows $ W.greedyView ws))
        | (key, ws) <- myExtraWorkspaces
    ] ++ [
        ((myModMask .|. shiftMask, key), (windows $ W.shift ws))
        | (key, ws) <- myExtraWorkspaces
    ]
