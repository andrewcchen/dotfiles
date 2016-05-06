import XMonad
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, ppOutput, ppTitle, xmobarColor, shorten)
import XMonad.Hooks.EwmhDesktops(ewmh, fullscreenEventHook, ewmhDesktopsEventHook)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docksEventHook)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Layout.NoBorders (lessBorders, Ambiguity(OnlyFloat))
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute)
import System.IO

main = do
	xmproc <- spawnPipe "xmobar ~/.xmobarrc"
	mapM spawn myExecute
	xmonad $ ewmh $ defaultConfig
		{ handleEventHook = myHandleEventHook
		, layoutHook = myLayoutHook
		, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "green" "" . shorten 64
			}
		, manageHook = myManageHook
		, modMask = myModMask
		, terminal = "konsole"
		, workspaces = myWorkspaces
		} `additionalKeys` myAdditionalKeys

myExecute =
	[ "sh -c \"killall trayer; exec trayer --edge top --align left --expand true --distance 1230 --distancefrom left --widthtype pixel --width 136 --height 16 --transparent true --tint 0x00000000 --SetDockType true --SetPartialStrut true\""
	, "sh -c \"if [ -z \\\"$(pidof keepassx2)\\\" ]; then exec keepassx2; fi\""
	]

myHandleEventHook = docksEventHook
                <+> fullscreenEventHook
                <+> ewmhDesktopsEventHook
                <+> handleEventHook defaultConfig

myLayoutHook = lessBorders OnlyFloat
             $ avoidStruts
             $ layoutHook defaultConfig

infixl 4 <$$>
(<$$>) = flip fmap

myManageHook = manageDocks
           <+> (appName =? "trayer" --> doIgnore)
           <+> (title <$$> (\t -> " - KeePassX" `isSuffixOf` t && "kdbx" `isInfixOf` t) --> doShift "F12")
           <+> (title =? "Kerbal Space Program" --> doFullFloat)
           <+> (title <$$> isPrefixOf "CKAN " --> doIgnore)
           <+> (isFullscreen --> doFullFloat)
           <+> manageHook defaultConfig

myModMask = mod4Mask

myWorkspaces = map show [1..9] ++ (map snd myExtraWorkspaces)

myExtraWorkspaces =
	[ (xK_0, "0")
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
	, ((myModMask, xK_z), spawn "xscreensaver-command -lock")
	, ((myModMask .|. shiftMask, xK_z), spawn "sh -c \"xscreensaver-command -lock; systemctl suspend\"")
	, ((myModMask, xK_f), windows myFocusedDoFullFloat)
	--, ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2-")
	--, ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+")
	--, ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
	] ++ [
		((myModMask, key), (windows $ W.greedyView ws))
		| (key, ws) <- myExtraWorkspaces
	] ++ [
		((myModMask .|. shiftMask, key), (windows $ W.shift ws))
		| (key, ws) <- myExtraWorkspaces
	]
