{-# LANGUAGE DeriveDataTypeable #-}
import XMonad hiding ( (|||) )
import qualified XMonad as X
import Data.List (find,nub)
import Data.Map 
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
import XMonad.Layout.Tabbed
import XMonad.Actions.WindowBringer hiding (actionMenu, menuArgs)
import XMonad.Layout.Decoration( DecorationMsg( SetTheme ) )
{-import Main.WindowBringer-}
--import XMonad.Layout.SimpleDecoration

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
myTags = [[a] | a <- ['\9312'..'\9323']]
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
    , className >>= io . appendFile "/home/markw/xmonad_debug" >> idHook
    , className =? "sun-awt-X11-XFramePeer" --> doFloat
    , className =? "Zenity" --> doFloat
    , className =? "arduino" --> doFloat
    , className =? "Xephyr" --> doShift (get_ws "6")
    , className =? "Xephyr" --> doSink
    , transience'
    , manageDocks
    ]
    {-where viewShift = doF . liftM2 (.) W.view W.shift-}

-- | Pops open a prompt with window titles. Choose one, and you will be
-- taken to the corresponding workspace.
{-doPrompt :: WindowPrompt String -> X ()-}
{-doPrompt t c = do-}
  {-a <- case t of-}
         {-Goto  -> fmap gotoAction  windowMap-}
         {-Bring -> fmap bringAction windowMap-}
         {-BringCopy -> fmap bringCopyAction windowMap-}
  {-wm <- windowMap-}
  {-mkXPrompt t c (compList wm) a-}

    {-where-}
      {-winAction a m    = flip whenJust (windows . a) . flip M.lookup m-}
      {-gotoAction       = winAction W.focusWindow-}
      {-bringAction      = winAction bringWindow-}
      {-bringCopyAction  = winAction bringCopyWindow-}

      {-compList m s = return . filter (searchPredicate c s) . map fst . M.toList $ m-}

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
    ,   ppTitle   = xmobarColor "gray50" "" . shorten 55
    ,   ppSep     = xmobarColor "#c0c0c0" "" " :: "++lightOrDark
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

readTheme = do
    handle <- io $ openBinaryFile "/home/markw/.dynamic-colors/colorscheme" ReadMode ;
    s <- io $ hGetLine handle ;
    return s

myLogHook = do 
    s <- readTheme ;
    dynamicLogWithPP (myPP s)
              >> updatePointer (0.5, 0.5) (0, 0)

{--get_alt_win our_stack
        | focused == master = alt_win
        | otherwise         = master
        where focused = elim $ W.peek our_stack-- our_ws
              master  = head $ W.index our_stack-- . our_ws
              alt_win = head $ tail $ W.index our_stack-- . our_ws
              elim (Just a) = a
--}

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
            , fontName            = "xft:Noto Sans Mono CJK JP Regular:pixelsize=10"
            , decoHeight          = 16
            } 
    else
        def { activeColor         = "#fdf6e3"
            , inactiveColor       = "#eee8d5"
            , activeBorderColor   = "#fdf6e3"
            , inactiveBorderColor = "#fdf6e3"
            , activeTextColor     = "#268bd2"
            , inactiveTextColor   = "gray50"
            , fontName            = "xft:Noto Sans Mono CJK JP Regular:pixelsize=10"
            , decoHeight          = 16
            } 

myTab = tabbed shrinkText $ myDecoTheme "solarized-dark"
myLayout = (avoidStruts . smartBorders) $ 
           (tall ||| wide ||| tp ||| myTab)
           where tall = (Tall 1 step ratio)
                 wide = (Mirror (Tall 1 step ratio))
                 tp = (TwoPane step ratio)
                 ratio = (60/100)
                 step  = (10/100)
{-deXer (X a)  = a-}
{----}
{--- | Conditionally run an action, using a @Maybe a@ to decide.-}
{-whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()-}
{-whenJust mg f = maybe (return ()) f mg-}

-- | Conditionally run an action, using a 'X' event to decide
-- | bind the result of dmenu and run the action on the result

{-get_ws_by_dmenu :: [String] -> X ()-}
dmenuDo :: Map String a -> a -> (a -> X()) -> X()
dmenuDo choices d action = (dmenuMap choices) >>=
    \choice -> case choice of
                    Just c -> action c
                    Nothing -> action d

gotoWorkspace w = do s <- gets windowset
                     if tagMember w s
                        then windows $ greedyView w
                        else addWorkspace w
{-viewShift = doF . liftM2 (.) W.view W.shift-}
-- | Calls dmenuMap to grab the appropriate Window, and hands it off to action
--   if found.

{-whenX a f = a >>= \b -> b f-}

myTerm = "my-term"
-- TODO: how to turn this into a string
-- home = getEnv

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
            , ("M-y", sendMessage $ JumpToLayout "Tabbed Simplest")
            , ("M-p", sendMessage $ NextLayout)
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
                (sendMessage . SetTheme) $ myDecoTheme (if (unpack (strip (pack theme))) == "solarized-dark" then "solarized-light" else "solarized-dark"))
            , ((myMod, xK_d), spawn "todo")
            , ((myMod .|. shiftMask, xK_z), io $ exitWith ExitSuccess)
            --, ((myMod .|. controlMask, xK_h), sendMessage $ pullGroup L)
            --, ((myMod .|. controlMask, xK_l), sendMessage $ pullGroup R)
            --, ((myMod .|. controlMask, xK_k), sendMessage $ pullGroup U)
            --, ((myMod .|. controlMask, xK_j), sendMessage $ pullGroup D)
            --, ((myMod .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
            --, ((myMod .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
            --, ((myMod .|. controlMask, xK_period), onGroup W.focusUp')
            --, ((myMod .|. controlMask, xK_comma), onGroup W.focusDown')

            , ((myMod, xK_t), spawn myTerm)
            , ((myMod, xK_b), sendMessage $ ToggleStrut D)
            , ((myMod .|. shiftMask, xK_b), sendMessage ToggleStruts)
            , ((myMod .|. shiftMask, xK_slash), withFocused $ windows . W.sink)
            , ((myMod, xK_n), gotoMenuArgs' "my_dmenu" ["-b"])
            , ((myMod .|. shiftMask, xK_n), bringMenuArgs' "my_dmenu" ["-b"])
            , ((myMod .|. shiftMask .|. controlMask, xK_n), copyMenuArgs' "my_dmenu" ["-b"])
            , ((myMod, xK_space), spawn "dmenu_run_plus -i -p Run -fn 'Noto Sans Mono CJK JP Regular':pixelsize=10 -lh 19")
            --, ((myMod, xK_space), spawn "launcher")
            , ((myMod, xK_Down), BW.focusDown)
            , ((myMod, xK_Up), BW.focusUp)
            , ((mod1Mask , xK_Tab), windows rotateWindows)
            , ((mod1Mask .|. shiftMask, xK_Tab), windows rotateWindowsr)
            , ((myMod .|. shiftMask, xK_Down), windows W.swapDown)
            , ((myMod .|. shiftMask, xK_Up), windows W.swapUp)
            , ((myMod,               xK_Right),  nextWS)
            , ((myMod,               xK_Left),    prevWS)
            --, ((myMod,               xK_h),    prevWS)
            --, ((myMod,               xK_l),  nextWS)
            , ((myMod .|. shiftMask, xK_Right), shiftToNext >> nextWS)
            , ((myMod .|. shiftMask, xK_Left),   shiftToPrev >> prevWS)
            , ((myMod,               xK_x),     toggleWS)
            --, ((myMod,                 xK_f  ), withFocused (addTag "abc"))
            --, ((myMod .|. controlMask, xK_f  ), withFocused (delTag "abc"))
            --, ((myMod .|. shiftMask,   xK_f  ), withTaggedGlobalP "abc" W.sink)
            --, ((myMod,                 xK_d  ), withTaggedP "abc" (W.shiftWin "2"))
            --, ((myMod .|. shiftMask,   xK_d  ), withTaggedGlobalP "abc" shiftHere)
            --, ((myMod .|. controlMask, xK_d  ), focusUpTaggedGlobal "abc")
            , ((myMod,                 xK_g  ), tagMenuArgs' "my_dmenu" ["-b"])
            , ((myMod .|. shiftMask,   xK_g  ), untagMenuArgs' "my_dmenu" ["-b"])
            --, ((myMod, xK_f  ), withFocused (delTag "abc"))
            --, ((myMod,                 xK_f  ), withFocused (addTag "abc"))
            , ((myMod,                xK_f ), gotoTaggedMenu)
            , ((myMod .|. shiftMask,  xK_f ), bringTaggedMenu)
            --, ((myMod .|. controlMask, xK_g  ), tagDelPrompt def)
            --, ((myMod .|. shiftMask,   xK_g  ), tagPrompt def (\s -> withTaggedGlobal s float))
            --, ((modWinMask,                xK_g  ), tagPrompt def (\s -> withTaggedP s (W.shiftWin "2")))
            --, ((modWinMask .|. shiftMask,  xK_g  ), tagPrompt def (\s -> withTaggedGlobalP s shiftHere))
            --, ((modWinMask .|. controlMask, xK_g ), tagPrompt def (\s -> focusUpTaggedGlobal s))
            ] 
            ++ 
            zip (zip (repeat (myMod)) ([xK_1..xK_9]++[xK_0])) (Prelude.map gotoWorkspace myWorkspaces)
            ++
            zip (zip (repeat (myMod .|. shiftMask)) ([xK_1..xK_9]++[xK_0])) (Prelude.map (\ws -> (addHiddenWorkspace ws) >> (windows $ W.shift ws)) myWorkspaces))
            `removeKeysP`
            (["M-S-q", "M-S-t", "M-Space"] ++ ["M-" ++ mod ++ [key] | key <- "wer", mod <- ["", "S-"]])
