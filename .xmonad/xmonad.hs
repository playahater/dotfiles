-- Import stuff
import XMonad
import qualified XMonad.StackSet as W 
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
import System.Exit
import Graphics.X11.Xlib
import System.IO
 
-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Search
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.GridSelect
 
-- utils
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Prompt    as P
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt
 
-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
 
-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid
 
-- Data.Ratio for IM layout
import Data.Ratio ((%))
 
 
-- Main --
main = do
        xmproc <- spawnPipe "xmobar"  -- start xmobar
        xmonad  $ withUrgencyHook NoUrgencyHook $ defaultConfig
                { manageHook = myManageHook
                , layoutHook = myLayoutHook  
                , borderWidth = myBorderWidth
                , normalBorderColor = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor
                , keys = myKeys
                , logHook = myLogHook xmproc
                , modMask = myModMask  
                , startupHook = myStartupHook
                , terminal = myTerminal
                , workspaces = myWorkspaces
                , focusFollowsMouse = False
                }
 
 
 
-- hooks
-- automaticly switching app to workspace 
myManageHook :: ManageHook
myManageHook = scratchpadManageHook (W.RationalRect 0.07 0.09 0.86 0.85) <+> ( composeAll . concat $
                [[isDialog                       --> doFloat
                , className =?  "Xmessage"  --> doFloat 
                , className =? "Save a Bookmark on Delicious"  --> doFloat 
                , className =? "8:gimp"           --> doShift "8:gimp"
                , className =? "uzbl-browser"           --> doShift "4:web"
                , className =? "VirtualBox" --> doShift "5:vbox"
                , className =? "Zathura"    --> doShift "9:pdf"
                ]]
                        )  <+> manageDocks
 


--StartupHook
myStartupHook :: X ()
myStartupHook = do 
                setWMName "LG3D"
                spawn "xmodmap ~/.Xmodmap"
                spawn "xsetroot -cursor_name left_ptr"
                spawn "xset r rate 180 90"
                spawn "xrdb -load ~/.Xdefaults"


--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }
 
 
 
---- Looks --
---- bar
customPP :: PP
customPP = defaultPP { 
                            ppHidden = xmobarColor "green" ""
                          , ppCurrent = xmobarColor "orange" "" . wrap "[" "]"
                          , ppUrgent = xmobarColor "green" "" . wrap "*" "*"
                          , ppLayout = \x -> ""
                          , ppTitle = xmobarColor "green" "" . shorten 120
                          , ppSep = "<fc=#008518> | </fc>"
                     }
 
-- some nice colors for the prompt windows
myXPConfig = defaultXPConfig                                    
    { font = "xft:ProFont:size=11"
    , bgColor = "black"
    , fgColor = "#008518"
    , fgHLight = "#008518"
    , bgHLight = "#000000"
    , borderColor = "black"
    , promptBorderWidth = 1
    , position = Bottom
    , height = 14
    , historySize = 50
    }
 
--- MyTheme For Tabbed layout
myTheme = defaultTheme { decoHeight = 14
    , activeColor = "#000000"
    , activeBorderColor = "#000000"
    , activeTextColor = "#008518"
    , inactiveColor = "#000000"
    , inactiveBorderColor = "#000000"
    , inactiveTextColor = "#EBEBEB"
    , urgentColor = "#000000"
    , urgentTextColor = "#008518"
}
 
--LayoutHook
myLayoutHook  = onWorkspace "1:chat" imLayout $ onWorkspace "2:webdev" webL $ onWorkspace "8:gimp" gimpL $ onWorkspace "7:wine" full $ standardLayouts 
   where

        standardLayouts = avoidStruts $ (tiled ||| tabLayout ||| reflectTiled ||| Mirror tiled |||  Grid ||| Full)
 
        --Layouts
        tiled     = smartBorders (ResizableTall 1 (2/100) (1/2) [])
        reflectTiled = (reflectHoriz tiled)
        tabLayout = (tabbedBottom shrinkText myTheme)
        full    = noBorders Full
 
        --Im Layout
        imLayout = avoidStruts $ smartBorders $ reflectHoriz $ withIM skypeRatio skypeRoster (tiled ||| reflectTiled ||| Grid) where
                chatLayout  = Grid
                skypeRatio  = (1%6)
                skypeRoster = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm")) 

        --Gimp Layout
        gimpL = avoidStruts $ smartBorders $ withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") tabLayout

        webL      = avoidStruts $ full 

-------------------------------------------------------------------------------
---- Terminal --
myTerminal :: String
myTerminal = "urxvt"
 
-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
myModMask :: KeyMask
myModMask = mod4Mask
 
-- borders
myBorderWidth :: Dimension
myBorderWidth = 0
--  
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#400000"
--
 
--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:chat", "2:webdev", "3:code", "4:web", "5:vbox" ,"6:misc", "7:wine", "8:gimp", "9:vlc"]
--
 
-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- killing programs
    [ ((modMask, xK_F1), spawn $ XMonad.terminal conf)
    , ((modMask, xK_c ), kill)
 
    -- opening program launcher / search engine
--    ,((modMask , xK_F2), runOrRaisePrompt myXPConfig)
    ,((modMask , xK_F2), shellPrompt myXPConfig)

    -- GridSelect
    , ((modMask, xK_g), goToSelected defaultGSConfig)
 
    -- layouts
    , ((modMask, xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_b ), sendMessage ToggleStruts)
 
    -- floating layer stuff
    , ((modMask, xK_t ), withFocused $ windows . W.sink)
 
    -- refresh'
    , ((modMask, xK_n ), refresh)
 
    -- focus
    , ((modMask, xK_Tab ), windows W.focusDown)
    , ((modMask, xK_j ), windows W.focusDown)
    , ((modMask, xK_k ), windows W.focusUp)
    , ((modMask, xK_m ), windows W.focusMaster)
 
 
    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j ), windows W.swapDown )
    , ((modMask .|. shiftMask, xK_k ), windows W.swapUp )
 
    -- increase or decrease number of windows in the master area
    , ((modMask , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask , xK_period), sendMessage (IncMasterN (-1)))
 
    -- resizing
    , ((modMask, xK_h ), sendMessage Shrink)
    , ((modMask, xK_l ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l ), sendMessage MirrorExpand)
 
    -- scratchpad
    , ((modMask , xK_grave), scratchpadSpawnAction defaultConfig  {terminal = myTerminal}) 

    -- volume control
    , ((0, 0x1008ff13), spawn "amixer -q set Master 1dB+") -- raise volume
    , ((0, 0x1008ff11), spawn "amixer -q set Master 1dB-") -- lower volume
    , ((modMask, xK_s ),spawn "xset dpms force off")

    -- take screenshot
    , ((0, xK_Print), spawn "screenshot")
 
    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
    , ((modMask , xK_q ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
