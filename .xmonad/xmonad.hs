--------------------------------------------------------------------------------
-- Compiler flags
{-# OPTIONS_GHC -threaded #-}

--------------------------------------------------------------------------------
-- Import stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import System.IO

-- utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Prompt
--import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Layout.IndependentScreens
import XMonad.Util.NamedScratchpad

import XMonad.Actions.CycleWS
import XMonad.Actions.TagWindows

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- layouts
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.Reflect

--------------------------------------------------------------------------------
-- Main
main = do
    h <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ docks def
      { modMask            = myModMask
      , borderWidth        = myBorderWidth
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , terminal           = myTerminal
      , startupHook        = myStartupHook
      , logHook            = myLogHook h
      , manageHook         = manageDocks <+> myManageHook
      , layoutHook         = myLayoutHook
      , handleEventHook    = docksEventHook <+> handleEventHook def
      , keys               = myKeys
      , workspaces         = myWorkspaces
      , focusFollowsMouse  = myFocusFollowsMouse
      }

--------------------------------------------------------------------------------
-- Misc static declarations

-- terminal and browser defs
myTerminal, myBrowser :: String
myTerminal = "urxvt"
myBrowser = "chromium"

-- borders
myBorderWidth :: Dimension
myBorderWidth = 0

myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#400000"

-- mouse focus
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

--------------------------------------------------------------------------------
-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["α","β","γ","δ","ε","ζ","η","θ", "ι"]

--------------------------------------------------------------------------------
-- LayoutHook
myLayoutHook = noBorders
        . smartSpacing 1
        . mkToggle (NOBORDERS ?? NBFULL ?? EOT)
        $ myLayout

myLayout = avoidStruts $ (Mirror tiled ||| Full ||| tiled)
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 3/6
     delta   = 5/100

--------------------------------------------------------------------------------
-- some nice colors for the prompt windows
myXPConfig = def
  { font = "xft:Literation Mono Powerline:pixelsize=19:antialias=true:autohint=true:hinting=true:dpi=120"
  , bgColor = "#1d2021"
  , fgColor = "#ebdbb2"
  , fgHLight = "#F6F1D1"
  , bgHLight = "#0B2027"
  , borderColor = "#0B2027"
  , alwaysHighlight   = True
  , promptBorderWidth = 1
  , position = Bottom
  , height = 19
  , historySize = 50
  }

--------------------------------------------------------------------------------
-- TagHook
addTagHook :: String -> ManageHook
addTagHook tag = do
  w <- ask
  liftX $ addTag tag w
  idHook

--------------------------------------------------------------------------------
-- ManageHook
myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook scratchpads
  <+> composeAll
  [ title =? "xmessage"                     --> doRectFloat centeredRect
  , className =? "Chromium"                 --> doShift "β" <+> addTagHook "d"
  , className =? "Checker Plus for Gmail"   --> doCenterFloat
  , isDialog                                --> doCenterFloat
  , isFullscreen                            --> doCenterFloat
  , pure True                               --> doCenterFloat
  ]

--------------------------------------------------------------------------------
-- scratchpads
scratchpads :: [NamedScratchpad]
scratchpads =
    [ shellScratchpad "htop" ( customFloating $ lowerRightRect )
    , screenScratchpad "screen" ( customFloating $ centeredRect )
    , shellScratchpad "mc" ( customFloating $ upperRightRect )
--    , NS "chromium" "chromium" (className =? "Chromium") ( customFloating $ rightBarRect )
    ]

-- NamedScratchpad
screenScratchpad :: String -> ManageHook -> NamedScratchpad
screenScratchpad session = NS session command (resource =? session)
  where command = "urxvt -name " ++ session ++ " -e screen -R"

shellScratchpad :: String -> ManageHook -> NamedScratchpad
shellScratchpad session = NS session command (resource =? session)
  where command = "urxvt -name " ++ session ++ " -e " ++ session

centeredRect   = W.RationalRect 0.03 0.025 0.95 0.97
upperBarRect   = W.RationalRect 0.0 0.0 1.0 0.4
rightBarRect   = W.RationalRect 0.5 0.0 0.5 1.0
upperRightRect = W.RationalRect 0.5 0.0 0.5 0.5
lowerRightRect = W.RationalRect 0.5 0.5 0.5 0.5

--------------------------------------------------------------------------------
-- StartupHook
myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawn "xmodmap ~/.Xmodmap"
  spawn "xsetroot -cursor_name left_ptr"
  spawn "xset r rate 180 90"
  spawn "xset -b"
  spawn "xrdb -load ~/.Xresources"
  spawn "xsetroot -solid '#1d2021'"
  spawn "feh --bg-scale ~/.xmonad/zentree.png"
  spawn "stty -ixon"

--------------------------------------------------------------------------------
---- logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP {
    ppOutput = hPutStrLn h
}

-- bar
customPP :: PP
customPP = def {
      ppHidden = xmobarColor "#F6F1D1" ""
    , ppCurrent = xmobarColor "#E6B89C" "" . wrap "[" "]"
    , ppUrgent = xmobarColor "#cc241d" "" . wrap "*" "*"
    , ppLayout = \x -> ""
    , ppTitle = xmobarColor "#F6F1D1" "" . shorten 120
    , ppSep = "<fc=#A0CF5D> || </fc>"
}

--------------------------------------------------------------------------------
-- Keys
--------------------------------------------------------------------------------
-- Key defs
type TagKey = (Char, KeySym)
type Tag = Char

--------------------------------------------------------------------------------
keyToCode :: M.Map Char KeySym
keyToCode = M.fromList $ zip (['a' .. 'z'] ++ ['0' .. '9']) ([xK_a .. xK_z] ++ [xK_0 .. xK_9])

--------------------------------------------------------------------------------
myModMask, workspaceMask :: ButtonMask
myModMask           = mod4Mask
myShiftMask         = myModMask .|. shiftMask
myControlMask       = myModMask .|. controlMask
myAltMask           = myModMask .|. mod1Mask
workspaceMask       = myModMask

--------------------------------------------------------------------------------
myMainKeys :: [(( ButtonMask, KeySym ), X () )]
myMainKeys =
  [ ((0, 0x1008ff13), spawn "sh -c 'pactl set-sink-mute 0 false ; pactl set-sink-volume 0 +5%'") -- raise volume
  , ((0, 0x1008ff11), spawn "sh -c 'pactl set-sink-mute 0 false ; pactl -- set-sink-volume 0 -5%'") -- lower volume
  , ((0, 0x1008FF12), spawn "pactl set-sink-mute 0 toggle") -- mute volume
  , ((0, 0x1008FF02), spawn "light -A 10") -- Monitor/panel brightness up
  , ((0, 0x1008FF03), spawn "light -U 10") -- Monitor/panel brightness down
  , ((0, xK_Print), spawn "import ~/.screenshots/$(date '+%Y%m%d-%H%M%S').png")
  , ((myModMask, xK_grave), namedScratchpadAction scratchpads "screen")
  , ((myAltMask, xK_h), namedScratchpadAction scratchpads "htop")
  , ((myAltMask, xK_m), namedScratchpadAction scratchpads "mc")
  ]

myBaseKeys :: XConfig Layout -> [(( ButtonMask, KeySym ), X () )]
myBaseKeys conf = myMainKeys ++
  [ ((mod1Mask, xK_Tab), windows W.focusUp >> windows W.shiftMaster)
  , ((mod1Mask .|. shiftMask, xK_Tab), windows W.focusDown)
  , ((myModMask, xK_Return), windows W.focusMaster  )
  , ((myModMask, xK_F7), spawn "single-lvds")
  , ((myShiftMask, xK_F7), spawn "single-vga")
  , ((myModMask, xK_F1), spawn $ XMonad.terminal conf)
  , ((myModMask, xK_F2), shellPrompt myXPConfig)
--  , ((myModMask, xK_F2), runOrRaisePrompt myXPConfig)
  , ((myModMask, xK_r), toggleWS' ["NSP"])
  , ((myModMask, xK_s), nextScreen)
  , ((myModMask, xK_f), sendMessage $ Toggle NBFULL)
  , ((myModMask, xK_m), sendMessage $ Toggle MIRROR)
  , ((myModMask, xK_k), windows W.focusUp >> windows W.shiftMaster)
  , ((myModMask, xK_j), windows W.focusDown >> windows W.shiftMaster)
  --, ((myModMask, xK_0), windows $ W.greedyView "NSP")
  , ((myShiftMask, xK_Return), windows W.swapMaster)
  , ((myShiftMask, xK_j ), windows W.swapDown )
  , ((myShiftMask, xK_k ), windows W.swapUp )
  , ((myModMask , xK_comma ), sendMessage (IncMasterN 1))
  , ((myModMask , xK_period), sendMessage (IncMasterN (-1)))
  , ((myModMask, xK_h ), sendMessage Shrink)
  , ((myModMask, xK_l ), sendMessage Expand)
  , ((myShiftMask, xK_q ), io (exitWith ExitSuccess))
  , ((myModMask , xK_q ), restart "xmonad" True)
  , ((myModMask, xK_t ), withFocused $ windows . W.sink)
  , ((myModMask, xK_c ), kill)
  , ((myModMask, xK_s), swapNextScreen)
  , ((myShiftMask, xK_s), shiftNextScreen)
--  , ((myModMask, xK_b ), sendMessage ToggleStruts >> refresh)
  , ((myModMask, xK_b ), sendMessage ToggleStruts)
  , ((myModMask, xK_space ), sendMessage NextLayout)
  --, ((myModMask, xK_g), goToSelected defaultGSConfig)
  --

--  , ((modm,                 xK_f  ), withFocused (addTag "abc"))
--  , ((modm .|. controlMask, xK_f  ), withFocused (delTag "abc"))
--  , ((modm .|. shiftMask,   xK_f  ), withTaggedGlobalP "abc" W.sink)
--  , ((modm,                 xK_d  ), withTaggedP "abc" (W.shiftWin "2"))
--  , ((modm .|. shiftMask,   xK_d  ), withTaggedGlobalP "abc" shiftHere)
--  , ((modm .|. controlMask, xK_d  ), focusUpTaggedGlobal "abc")
--  , ((modm,                 xK_g  ), tagPrompt def (\s -> withFocused (addTag s)))
--  , ((modm .|. controlMask, xK_g  ), tagDelPrompt def)
--  , ((modm .|. shiftMask,   xK_g  ), tagPrompt def (\s -> withTaggedGlobal s float))
--  , ((modWinMask,                xK_g  ), tagPrompt def (\s -> withTaggedP s (W.shiftWin "2")))
--  , ((modWinMask .|. shiftMask,  xK_g  ), tagPrompt def (\s -> withTaggedGlobalP s shiftHere))
--  , ((modWinMask .|. controlMask, xK_g ), tagPrompt def (\s -> focusUpTaggedGlobal s))
  ]

myKeys :: XConfig Layout -> M.Map ( ButtonMask, KeySym ) ( X () )
myKeys conf = M.fromList $
  myBaseKeys conf ++
  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9] %! Move client to workspace N
  [((m .|. workspaceMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]] ++
  -- mod-[w,e] %! switch to twinview screen 1/2
  -- mod-shift-[w,e] %! move window to screen 1/2
  [((m .|. workspaceMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]] ++
  buildTagKeys tags focusUpTagged

--------------------------------------------------------------------------------
buildTagKeys :: [Tag] -> ( String -> X () ) -> [(( ButtonMask, KeySym ), X () )]
buildTagKeys tagKeys action =
  [((myShiftMask, keyToCode M.! key), action [key] >> windows W.shiftMaster) | key <- tagKeys ] ++
  [((myModMask, keyToCode M.! key), (withFocused . toggleTag) [key])       | key <- tagKeys ]

--------------------------------------------------------------------------------
toggleTag :: String -> Window -> X ()
toggleTag tag window = do
  tagActive <- hasTag tag window
  if tagActive
  then delTag tag window
  else addTag tag window

--------------------------------------------------------------------------------
xmessage :: String -> X ()
xmessage msg = spawn $ "xmessage " ++ msg

--------------------------------------------------------------------------------
-- explicit list of tags
tags :: [Tag]
tags = [ 'e' -- editor
       , 'd' -- project-related documentation
       , 'o' -- org mode: project-related org or similar
       ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
