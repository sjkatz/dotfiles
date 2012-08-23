import XMonad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig --(additionalKeys)
import System.IO
import XMonad.Config.Gnome

import XMonad.Actions.CycleWS

-- Import for hooks
import XMonad.Operations
import System.Exit
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops

-- Import for layout
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Fullscreen 
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid

import Data.Ratio ((%))

import Data.String.UTF8
 
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Control.Monad (liftM2)

-- Start config

myWorkspaces :: [String]
myWorkspaces = ["1:main", "2:web", "3:vim", "4:chat", "5:music", "6:other", "7:shed", "8:theatre", "9:crap"]

-- Set border colours
myNormalBorderColor  = "#333333"
myFocusedBorderColor = "#DD4814"
myBorderWidth     = 2

-- Hooks
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> viewShift  "1:main"   |   c   <- myDev    ] -- move dev to main
    , [className    =? c            --> viewShift  "2:web"    |   c   <- myWebs   ] -- move webs to main
    , [className    =? c            --> viewShift  "3:vim"    |   c   <- myVim    ] -- move webs to main
    , [className    =? c            --> viewShift	 "4:chat"   |   c   <- myChat   ] -- move chat to chat
    , [className    =? c            --> viewShift  "5:music"  |   c   <- myMusic  ] -- move music to music
    , [className    =? c            --> viewShift  "6:other"  |   c   <- myOther  ] -- move img to div
    , [className    =? c            --> viewShift  "7:shed"   |   c   <- myShed   ] -- move img to div
    , [className    =? c            --> viewShift  "8:theatre"|   c   <- myTheatre] -- move img to div
    , [className    =? c            --> doShift  "9:crap"|   c   <- myCrap] -- move img to div
    , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] -- float my floats
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> myDoFullFloat                           ]
    , [ manageDocks ]
    ]) 
 		where			
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
 
        -- classnames
        myFloats  = ["Smplayer","MPlayer","VirtualBox","Xmessage","XFontSel","Downloads","Nm-connection-editor"]
        myWebs    = ["Firefox","Google-chrome","Chromium", "Chromium-browser"]
        myTheatre = ["Boxee","Trine"]
        myMusic	  = ["Rhythmbox","Spotify"]
        myChat	  = ["Pidgin","Buddy List","Empathy","Empathy-chat"]
        myOther	  = ["Gimp"]
        myShed    = []
        myDev	  = ["Gnome-terminal"]
        myVim	  = ["Gvim"]
        myCrap = ["Update-manager"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]
 
        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]
 
-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

viewShift = doF . liftM2 (.) W.greedyView W.shift

--layoutHook'  =  onWorkspaces ["1:main","5:music"] customLayout $ 
--                onWorkspaces ["6:other"] gimpLayout $ 
--                onWorkspaces ["4:chat"] imLayout $
--                customLayout2

-- Layouts
--customLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| simpleFloat
--  where
--    tiled   = ResizableTall 1 (2/100) (1/2) []
 
--customLayout2 = avoidStruts $ Full ||| tiled ||| Mirror tiled ||| simpleFloat 
--  where
--    tiled   = ResizableTall 1 (2/100) (1/2) []
 
--gimpLayout  = avoidStruts $ withIM (0.11) (Role "gimp-toolbox") $
--              reflectHoriz $
--              withIM (0.15) (Role "gimp-dock") Full
 
--imLayout    = avoidStruts $ withIM (1%5) (And (ClassName "Empathy") (Role "empathy-chat")) Grid 

-- Run xmonad with the specified conifguration
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig 
        {
        manageHook = manageHook' 
--        , layoutHook = layoutHook'
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , workspaces  = myWorkspaces
        , logHook = dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "orange" "" . wrap "[" "]" 
                        , ppTitle = xmobarColor "orange" "" . shorten 50
                       }
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth        = myBorderWidth
        , modMask = mod4Mask -- Bind mod to win key
        } `additionalKeysP` 
        [
        ("M-p", spawn "dmenu_run -b -nb black -nf white -sb black -sf orange")
      -- Logout
    --    , ("M-S-q", spawn "gnome-session-quit") 
      -- moving workspaces
        , ("C-M-h",    prevWS )
        , ("C-M-l",   nextWS )
        , ("C-M-S-h",  shiftToPrev )
        , ("C-M-S-l", shiftToNext )
        ]
        
