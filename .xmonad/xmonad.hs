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
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadSpawnActionCustom, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Paste
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
import XMonad.Layout.IndependentScreens

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
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Grid

-- Data.Ratio for IM layout
import Data.Ratio ((%))

-- Main --
main = do
    xmproc <- spawnPipe "xmobar"  -- start xmobar
    xmonad  $ withUrgencyHook NoUrgencyHook $ defaultConfig {
          manageHook = myManageHook <+> manageDocks
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

-- workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = withScreens 2 ["chat", "web", "dev", "ext", "5" ,"6", "7", "8", "9"]
-- myWorkspaces = ["chat", "web", "dev", "4", "5" ,"6", "7", "8", "9"]

-- LayoutHook
myLayoutHook = onWorkspace "chat" imL $ onWorkspace "web" webL $ standardLayouts where

    -- Layouts
    standardLayouts = avoidStruts $ (tiled ||| tabL ||| reflectTiled ||| Mirror tiled |||  Grid ||| Full)
    tiled = smartBorders (ResizableTall 1 (2/100) (1/2) [])
    reflectTiled = (reflectHoriz tiled)
    tabL = (tabbedBottom shrinkText myTheme)
    full = noBorders Full
    webL = avoidStruts $ full
    imL = avoidStruts $ smartBorders $ reflectHoriz $
               withIM (1%5) (Or (And (ClassName "Pidgin") (Role "buddy_list"))
                            (And (ClassName "Skype")  (And (Role "") (Not (Title "Options"))))) (tabL)
                            -- TODO: add File Transfers

-- ManageHook
myManageHook :: ManageHook
myManageHook = scratchpadManageHook (W.RationalRect 0.03 0.025 0.95 0.97) <+> (
         composeAll . concat $ [[
               isDialog --> doFloat
             , isFullscreen --> doFloat
             , className =? "Xmessage" --> doFloat
             , className =? "Skype"    --> doShift "chat"
             , className =? "Firefox"  --> doShift "web"
             , className =? "Chromium"  --> doShift "web"
             , className =? "PhpStorm"  --> doShift "dev"
         ]]
    )

-- StartupHook
myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D"
    spawn "xmodmap ~/.Xmodmap"
    spawn "xsetroot -cursor_name left_ptr"
    spawn "xset r rate 180 90"
    spawn "xset -b"
    spawn "xrdb -load ~/.Xresources"
    spawn "xinput set-prop 'TPPS/2 IBM TrackPoint' 'Evdev Wheel Emulation' 1"
    spawn "xinput set-prop 'TPPS/2 IBM TrackPoint' 'Evdev Wheel Emulation Button' 2"
    spawn "xinput set-prop 'TPPS/2 IBM TrackPoint' 'Evdev Wheel Emulation Timeout' 200"
    spawn "xinput set-prop 'TPPS/2 IBM TrackPoint' 'Evdev Wheel Emulation Axes' 6 7 4 5"
    --spawn "syndaemon -i 1 -d"
    spawn "xsetroot -solid '#282828'"
    -- spawn "feh --bg-scale media/img/pirate.jpg"

-- logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP {
    ppOutput = hPutStrLn h
}

-- bar
customPP :: PP
customPP = defaultPP {
      ppHidden = xmobarColor "#B8D68C" ""
    , ppCurrent = xmobarColor "#F39D21" "" . wrap "[" "]"
    , ppUrgent = xmobarColor "#E84F4F" "" . wrap "*" "*"
    , ppLayout = \x -> ""
    , ppTitle = xmobarColor "#B8D68C" "" . shorten 120
    , ppSep = "<fc=#A0CF5D> || </fc>"
}

-- terminal
myTerminal :: String
myTerminal = "urxvt"

-- borders
myBorderWidth :: Dimension
myBorderWidth = 0
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#400000"


-- some nice colors for the prompt windows
myXPConfig = defaultXPConfig {
      font = "xft:Literation Mono Powerline:pixelsize=13:antialias=true:autohint=true:hinting=true:dpi=96"
    , bgColor = "#282828"
    , fgColor = "#D7D0C7"
    , fgHLight = "#D7D0C7"
    , bgHLight = "#282828"
    , borderColor = "#282828"
    , promptBorderWidth = 1
    , position = Bottom
    , height = 14
    , historySize = 50
}

-- MyTheme For Tabbed layout
myTheme = defaultTheme {
      decoHeight = 14
    , activeColor = "#282828"
    , activeBorderColor = "#282828"
    , activeTextColor = "#D7D0C7"
    , inactiveColor = "#282828"
    , inactiveBorderColor = "#282828"
    , inactiveTextColor = "#dddddd"
    , urgentColor = "#E84F4F"
    , urgentTextColor = "#D7D0C7"
}

-- modmask
myModMask :: KeyMask
myModMask = mod4Mask

-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [
        -- killing programs
          ((modMask, xK_F1), spawn $ XMonad.terminal conf)
        , ((modMask, xK_c ), kill)

        -- opening program launcher / search engine
        --,((modMask , xK_F2), runOrRaisePrompt myXPConfig)
         ,((modMask , xK_F2), shellPrompt myXPConfig)

        -- GridSelect
        , ((modMask, xK_g), goToSelected defaultGSConfig)

        -- layouts
        , ((modMask, xK_space ), sendMessage NextLayout)
        , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
        , ((modMask, xK_b ), sendMessage ToggleStruts)

        -- floating layer stuff
        , ((modMask, xK_t ), withFocused $ windows . W.sink)

        -- focus
        , ((modMask, xK_Tab ), windows W.focusDown)
        , ((modMask, xK_j ), windows W.focusDown)
        , ((modMask, xK_k ), windows W.focusUp)
        , ((modMask, xK_m ), windows W.focusMaster)

        -- screens
	, ((modMask, xK_o), swapNextScreen)
	, ((modMask .|. shiftMask, xK_o), shiftNextScreen)

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

	-- X-selection-paste buffer
	, ((0, xK_Insert), pasteSelection)

        -- scratchpad
        , ((modMask , xK_grave),  scratchpadSpawnActionCustom "urxvt -name scratchpad -e screen -R")

        -- volume control
        , ((0, 0x1008ff13), spawn "sh -c 'pactl set-sink-mute 0 false ; pactl set-sink-volume 0 +5%'") -- raise volume
        , ((0, 0x1008ff11), spawn "sh -c 'pactl set-sink-mute 0 false ; pactl -- set-sink-volume 0 -5%'") -- lower volume
        , ((0, 0x1008FF12), spawn "pactl set-sink-mute 0 toggle") -- mute volume

        -- brightness control
        , ((0, 0x1008FF02), spawn "xbacklight -inc 10") -- Monitor/panel brightness up
        , ((0, 0x1008FF03), spawn "xbacklight -dec 10") -- Monitor/panel brightness down

        -- take screenshot
        , ((0, xK_Print), spawn "import -window root ~/media/screenshots/$(date '+%Y%m%d-%H%M%S').png")

        -- Handle xrandr
        , ((modMask , xK_F7), spawn "single-lvds")
        , ((modMask .|. shiftMask , xK_F7), spawn "single-vga")

        -- quit, or restart
        , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
        , ((modMask , xK_q ), restart "xmonad" True)
    ]
    ++
    [
        -- workspaces are distinct by screen
     	((m .|. modMask, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]

        -- mod-[1..9] %! Switch to workspace N
        -- mod-shift-[1..9] %! Move client to workspace N
--        ((m .|. modMask, k), windows $ f i)
--        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
    ++
    [
         -- swap screen order
         ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
	   | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2]
	   , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]

        -- mod-[w,e] %! switch to twinview screen 1/2
        -- mod-shift-[w,e] %! move window to screen 1/2
--        ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
--        | (key, sc) <- zip [xK_e, xK_w] [0..]
--        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]

    ]
