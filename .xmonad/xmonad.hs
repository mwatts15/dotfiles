{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
import XMonad hiding ( (|||) )
import qualified XMonad as X
import Data.List (find, nub, intersperse, unlines)
import Data.Text (strip, pack, unpack) 

import qualified Data.Map as M
import System.Exit
import System.Process (readProcess)
import System.IO
import qualified XMonad.Config.Desktop as DeskConf
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Accordion
import XMonad.Layout.Spiral
import XMonad.Layout.Circle
import XMonad.Layout.Dishes
import XMonad.Layout.TwoPane
import XMonad.Layout.SubLayouts
import XMonad.Layout.BoringWindows as BW hiding (focusMaster)
import LayoutModifier
--import XMonad.Layout.Tabbed
import Tabbed
import XMonad.Actions.WindowBringer hiding (actionMenu, menuArgs)
import Decoration( DecorationMsg( SetTheme ) )

import XMonad.Layout.WindowNavigation
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Dmenu
import XMonad.Util.WindowProperties
import XMonad.Util.Themes
import Control.Monad (liftM2,liftM)
import XMonad.StackSet hiding (focus)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeys,additionalKeysP,removeKeys,removeKeysP,additionalMouseBindings)
import Graphics.X11.ExtraTypes
import XMonad.Actions.DynamicWorkspaces
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Actions.CopyWindow(copyWindow, kill1)

import XMonad.Actions.TagWindows

get_index :: (Eq a) => a -> [a] -> Int
get_index _ [] = (- 1)
get_index n xs = length (takeWhile (/= n) xs)

myOldTags = ["1","2","3","4","5","6","7","8","9","0"]
myTags = ["1","2","3","4","5","6","7","8","9","0"]
myNumbers = "一二三四五六七八九十"
myWorkspaces = [[a] ++ ":" ++ b | (a, b) <- zip myNumbers myTags]
get_ws x = [myNumbers !! k ] ++ ":" ++ (myTags !! k)
           where 
            k = (get_index x myOldTags)
myScreens = [('\'', 1), (',', 0),
             ('a',  2), ('o', 3)]
{-
    This is my replacement for XMonadPrompt using dmenu
    It makes use of System.IO for working with handles 
    and of System.Process for running dmenu, getting 
    input, and writing output
-}

{- Note that only the second item in an xprop class name list will actually work -}
manage_hook = composeAll
    [ title =? "irssi" --> doShift (get_ws "3")
    , className =? "xmessage" --> doFloat
    , resource =? "gimp" --> doSink
    , className =? "sun-awt-X11-XFramePeer" --> doFloat
    , className =? "Zenity" --> doFloat
    , className =? "arduino" --> doFloat
    , className =? "Xephyr" --> doShift (get_ws "6")
    , className =? "Xephyr" --> doSink
    , transience'
    , manageDocks
    ]

doSink :: ManageHook
doSink = ask >>= \w -> liftX (reveal w) >> doF (W.sink w)
--manage_hook = transience . manageHook

myMod = mod4Mask
myBar = "xmobar"

myPP lightOrDark = xmobarPP 
    {   ppCurrent = xmobarColor "#268bd2" ""
    ,   ppUrgent  = xmobarColor "#ff69b4" ""
    ,   ppOrder   = \(ws:_:t:_) -> [ws,t]
    ,   ppWsSep   = "  "
    ,   ppHidden  = xmobarColor "gray50" "" 
    ,   ppHiddenNoWindows  = xmobarColor (if lightOrDark == "solarized-light" then "gray70" else "gray30") "" . ((:[]) . head)
    ,   ppTitle   = xmobarColor "gray50" "" 
    ,   ppSep     = xmobarColor "#c0c0c0" "" " :: "
    }

listrotate (x:xs) = xs ++ [x]
listrotater xs = [last xs] ++ init xs

modWindowStack :: ([a] -> [a]) -> StackSet ao b a d e -> StackSet ao b a d e
modWindowStack operator stackset = W.modify Nothing (differentiate . operator . integrate) stackset

findWindow predicate stackset = ((find predicate) . integrate) stackset

hide' Nothing = refresh
hide' (Just w) = hide w

rotateWindows = modWindowStack listrotate
rotateWindowsr = modWindowStack listrotater

readThemeIO = do
    handle <- openBinaryFile "/home/markw/.dynamic-colors/colorscheme" ReadMode ;
    s <- hGetLine handle ;
    hClose handle ;
    return s

readTheme = do
    handle <- io $ openBinaryFile "/home/markw/.dynamic-colors/colorscheme" ReadMode ;
    s <- io $ hGetLine handle ;
    io $ hClose handle ;
    return s

myLogHook = do 
    s <- readTheme ;
    dynamicLogWithPP (myPP s)
              >> updatePointer (0.5, 0.5) (0, 0)

focusSideStack stack = focusWindow (head $ tail $ W.index stack) stack

mySwapMaster our_stack
        | focused /= master = shiftMaster
        | otherwise         = focusMaster . swapDown
        where focused = elim $ W.peek our_stack
              master  = head $ W.index our_stack
              elim (Just a) = a

--switch_win our_stack = (W.focusWindow (get_alt_win our_stack)) our_stack
swap_win our_stack = (mySwapMaster our_stack) our_stack
bury (x:xs) = xs ++ [x] 
-- for modifying stack with list functions
modify_stack f = W.differentiate . f . W.integrate
bury_win = (W.modify Nothing $ modify_stack bury) . shiftMaster
myDecoTheme theme = 
    if theme == "solarized-dark"
    then
        def { activeColor         = "#002b36"
            , inactiveColor       = "#03151a"
            , activeBorderColor   = "#002b36"
            , inactiveBorderColor = "#002b36"
            , activeTextColor     = "#268bd2"
            , inactiveTextColor   = "gray50"
            , fontName            = "xft:Noto Sans Mono CJK JP:pixelsize=10"
            , decoHeight          = 16
            } 
    else
        def { activeColor         = "#fdf6e3"
            , inactiveColor       = "#eee8d5"
            , activeBorderColor   = "#fdf6e3"
            , inactiveBorderColor = "#fdf6e3"
            , activeTextColor     = "#268bd2"
            , inactiveTextColor   = "gray50"
            , fontName            = "xft:Noto Sans Mono CJK JP:pixelsize=10"
            , decoHeight          = 16
            } 

myTab theme = tabbed shrinkText $ myDecoTheme theme
--myLayout :: ModifiedLayout AvoidStruts q a
myLayout = (avoidStruts . smartBorders) $ 
           (tall ||| wide ||| tp ||| (myTab "solarized-dark"))
           where tall = (Tall 1 step ratio)
                 wide = (Mirror (Tall 1 step ratio))
                 tp = (TwoPane step ratio)
                 ratio = (60/100)
                 step  = (10/100)
--myLayoutX theme = myTab theme
--myLayoutX :: (LayoutModifier m a, LayoutClass l a) => [Char] -> LayoutClass (ModifiedLayout m l) a
--myLayoutX theme = (avoidStruts . smartBorders) $ myTab theme

-- | Conditionally run an action, using a 'X' event to decide
-- | bind the result of dmenu and run the action on the result

dmenuDo :: M.Map String a -> a -> (a -> X()) -> X()
dmenuDo choices d action = (dmenuMap choices) >>=
    \choice -> case choice of
                    Just c -> action c
                    Nothing -> action d

gotoWorkspace w = do s <- gets windowset
                     if tagMember w s
                        then windows $ greedyView w
                        else addWorkspace w

myTerm = "my-term"

-- Copied from WindowBringer module because I needed to make my own for bringing windows as copies
-- | Calls dmenuMap to grab the appropriate Window, and hands it off to action
--   if found.
actionMenu :: String -> [String] -> (Window -> X.WindowSet -> X.WindowSet) -> X ()
actionMenu menuCmd menuArgs action = windowMap >>= menuMapFunction >>= flip X.whenJust (windows . action)
    where
      menuMapFunction :: M.Map String a -> X (Maybe a)
      menuMapFunction selectionMap = menuMapArgs menuCmd menuArgs selectionMap

copyWindowToCurrentWS w ws = copyWindow w (W.currentTag ws) ws

copyMenuArgs' :: String -> [String] -> X ()
copyMenuArgs' menuCmd menuArgs = actionMenu menuCmd menuArgs copyWindowToCurrentWS

tagMenuArgs' :: String -> [String] -> X ()
tagMenuArgs' cmd args = tagComplList >>= menuArgs cmd args >>= (withFocused . addTag)

gotoTaggedMenu :: X ()
gotoTaggedMenu = tagComplList >>= (menuArgs "my_dmenu" ["-b"]) >>= focusUpTaggedGlobal

bringTaggedMenu :: X ()
bringTaggedMenu = tagComplList >>= (menuArgs "my_dmenu" ["-b"]) >>= (\s -> withTaggedGlobalP s shiftHere)

untagMenuArgs' :: String -> [String] -> X ()
untagMenuArgs' cmd args = tagDelComplList >>= menuArgs cmd args >>= (withFocused . delTag)

tagDelComplList :: X [String]
tagDelComplList = gets windowset >>= maybe (return []) getTags . peek

-- Copied (almost) verbatim from XMonad.Actions.TagWindows...they don't export
-- it for some reason
tagComplList :: X [String]
tagComplList = gets (concat . Prelude.map (integrate' . stack) . W.workspaces . windowset) >>=
    mapM getTags >>=
    return . nub . concat

-- | Update the layout field of a workspace
updateLayoutX :: X ()
updateLayoutX = runOnWorkspaces $ \ww -> return ww

myBroadcastMessage :: (Show a, Message a) => a -> X ()
myBroadcastMessage a = withWindowSet $ \ws -> do
   let c = W.workspace . W.current $ ws
       v = map W.workspace . W.visible $ ws
       h = W.hidden ws
   mapM_ (mySendMessageWithNoRefresh a) (c : v ++ h) 

mySendMessageWithNoRefresh :: (Show a, Message a) => a -> W.Workspace WorkspaceId (Layout Window) Window -> X ()
mySendMessageWithNoRefresh a w = do
   w0 <- handleMessage (W.layout w) (SomeMessage a) `catchX` (return $ Just (W.layout w)) ;
   updateLayout  (W.tag w) w0


main = do
       hSetBinaryMode stdout True ;
       xmonad $ ewmh DeskConf.desktopConfig
            { X.workspaces = myWorkspaces
            , borderWidth = 3
            , focusedBorderColor = "#268bd2"
            , normalBorderColor = "#333333"
            , modMask = myMod -- Map to Windows key
            , manageHook = manage_hook <+> manageHook DeskConf.desktopConfig
            , terminal = myTerm
            , layoutHook = myLayout
            , logHook = myLogHook
            , handleEventHook = handleEventHook DeskConf.desktopConfig <+> fullscreenEventHook
            }
            `additionalKeysP`
            ([("<XF86AudioNext>", spawn "xmms2 next")
            , ("<XF86AudioPrev>", spawn "xmms2 prev")
            , ("<XF86AudioPlay>", spawn "xmms2 toggle")
            , ("M-s", spawn "dxmms2")
            , ("<XF86TouchpadToggle>", spawn "~/bin/mouse_toggle.sh")
            , ("C-S-<Up>", spawn "amixer -q set Master unmute;amixer -q set Master 5%+;xmobar_vol.sh Master")
            , ("C-S-<Down>", spawn "amixer -q set Master unmute;amixer -q set Master 5%-;xmobar_vol.sh Master")
            , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master unmute;amixer -q set Master 5%+;xmobar_vol.sh Master")
            , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master unmute;amixer -q set Master 5%-;xmobar_vol.sh Master")
            , ("<XF86AudioMute>", spawn "audio-toggle.sh; xmobar_vol.sh Master")
            , ("M-m", windows swap_win)
            , ("M-S-m", windows bury_win)
            , ("M-y", do
                sendMessage $ JumpToLayout "Tabbed Simplest" ;
                theme <- readTheme ;
                (sendMessage . SetTheme) $ myDecoTheme (unpack (strip (pack theme))))
            , ("M-p", do
                sendMessage $ NextLayout ;
                theme <- readTheme ;
                (sendMessage . SetTheme) $ myDecoTheme (unpack (strip (pack theme))))
            , ("M-i", shiftTo Prev (WSIs $ return (((get_ws "5") ==) . tag)))
            , ("M-S-.", sendMessage (IncMasterN 1))
            , ("M-S-c", spawn "xmonad --recompile && xmonad --restart")
            , ("M-q", kill1)
            , ("M-c", spawn "dclip copy")
            , ("M-v", spawn "dclip paste")
            , ("M-S-l", spawn "xscreensaver-command --lock")
            , ("C-S-k", spawn "dmenu_man")
            , ("M-S-t", spawn "dict-lookup-selected")
            , ("C-S-r", spawn "wifi-connect.sh")
            ]
            ++
            [("M-" ++ mod ++ [key], screenWorkspace screen >>= flip whenJust(windows . action))
                | (key, screen) <- myScreens, (mod, action) <- zip ["", "S-"] [W.view, liftM2 (.) W.view W.shift]])
            `additionalKeys`
            ([ ((mod1Mask, xK_Print), spawn "scrot \'/home/markw/pictures/scrots/%Y%m%d%H%M%S_scrot-$wx$h.png\' -u")
            , ((mod1Mask .|. controlMask, xK_Print), spawn "scrot \'/home/markw/pictures/scrots/%Y%m%d%H%M%S_scrot-$wx$h.png\' -s")
            , ((0, xK_Print), spawn "scrot \'/home/markw/pictures/scrots/%Y%m%d%H%M%S_scrot-$wx$h.png\'")
            , ((controlMask .|. mod1Mask, xK_1), spawn "setxkbmap us -variant dvorak")
            , ((mod1Mask .|. controlMask, xK_2), spawn "setxkbmap us")
            , ((myMod .|. shiftMask, xK_o), do 
                theme <- readTheme ;
                spawn "toggle-color-scheme.sh" ;
                (myBroadcastMessage . SetTheme) $ myDecoTheme (if (unpack (strip (pack theme))) == "solarized-dark" then "solarized-light" else "solarized-dark")) 
            , ((myMod, xK_d), spawn "todo")
            , ((myMod .|. shiftMask, xK_z), io $ exitWith ExitSuccess)
            , ((myMod, xK_t), spawn myTerm)
            , ((myMod, xK_b), sendMessage $ ToggleStrut D)
            , ((myMod .|. shiftMask, xK_b), sendMessage ToggleStruts)
            , ((myMod .|. shiftMask, xK_slash), withFocused $ windows . W.sink)
            , ((myMod, xK_n), gotoMenuArgs' "my_dmenu" ["-b"])
            , ((myMod .|. shiftMask, xK_n), bringMenuArgs' "my_dmenu" ["-b"])
            , ((myMod .|. shiftMask .|. controlMask, xK_n), copyMenuArgs' "my_dmenu" ["-b"])
            , ((myMod, xK_space), spawn "dmenu_run_plus -i -p Run -fn 'Noto Sans Mono CJK JP Regular':pixelsize=10 -lh 19")
            , ((myMod, xK_Down), BW.focusDown)
            , ((myMod, xK_Up), BW.focusUp)
            , ((mod1Mask , xK_Tab), windows rotateWindows)
            , ((mod1Mask .|. shiftMask, xK_Tab), windows rotateWindowsr)
            , ((myMod .|. shiftMask, xK_Down), windows W.swapDown)
            , ((myMod .|. shiftMask, xK_Up), windows W.swapUp)
            , ((myMod,               xK_Right),  nextWS)
            , ((myMod,               xK_Left),    prevWS)
            , ((myMod .|. shiftMask, xK_Right), shiftToNext >> nextWS)
            , ((myMod .|. shiftMask, xK_Left),   shiftToPrev >> prevWS)
            , ((myMod, xK_x), toggleWS)
            , ((myMod, xK_g), tagMenuArgs' "my_dmenu" ["-b"])
            , ((myMod .|. shiftMask,   xK_g  ), untagMenuArgs' "my_dmenu" ["-b"])
            , ((myMod,                xK_f ), gotoTaggedMenu)
            , ((myMod .|. shiftMask,  xK_f ), bringTaggedMenu)
            ] 
            ++ 
            zip (zip (repeat (myMod)) ([xK_1..xK_9]++[xK_0])) (Prelude.map gotoWorkspace myWorkspaces)
            ++
            zip (zip (repeat (myMod .|. shiftMask)) ([xK_1..xK_9]++[xK_0])) (Prelude.map (\ws -> (addHiddenWorkspace ws) >> (windows $ W.shift ws)) myWorkspaces))
            `removeKeysP`
            (["M-S-q", "M-S-t", "M-Space"] ++ ["M-" ++ mod ++ [key] | key <- "wer", mod <- ["", "S-"]])
