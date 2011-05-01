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
--                , startupHook = setWMName "LG3D"
                , startupHook = myStartupHook
                , terminal = myTerminal
                , workspaces = myWorkspaces
                , focusFollowsMouse = False
                }
 
 
 
-- hooks
-- automaticly switching app to workspace 
myManageHook :: ManageHook
myManageHook = scratchpadManageHook (W.RationalRect 0.1 0.1 0.8 0.75) <+> ( composeAll . concat $
                [[isFullscreen                  --> ask >>= \x -> doF (W.float x (W.RationalRect 0 0.02 1 1))
                , isDialog                       --> doCenterFloat
                , className =? "OpenOffice.org 3.1" --> doShift "6:misc" 
                , className =?  "Xmessage"  --> doCenterFloat 
                , className =?  "Zenity"  --> doCenterFloat 
                , className =? "feh"  --> doCenterFloat 
                , className =? "Save a Bookmark on Delicious"  --> doCenterFloat 
                , className =? "8:gimp"           --> doShift "8:gimp"
--                , className =? "Firefox"           --> doShift "2:web"
--                , className =? "Chromium"           --> doShift "2:web"
                , className =? "uzbl"           --> doShift "4:web"
                , className =? "vimprobable"           --> doShift "4:web"
--                , className =? "Pidgin"           --> doShift "1:chat"
                , className =? "Skype"           --> doShift "1:chat"
--                , className =? "MPlayer"  --> doShift "8:vid"
                , className =? "VirtualBox" --> doShift "5:vbox"
                , className =? "Zathura"    --> doShift "9:pdf"
--                , className =? "Evince"   --> doShift "9:pdf"
--                , className =? "Epdfview"   --> doShift "9:pdf"
--                , className =? "Remmina"  --> doShift "6:vbox"]
 
                ]]
                        )  <+> manageDocks
 


--StartupHook
myStartupHook :: X ()
myStartupHook = do 
                setWMName "LG3D"
                spawn "xmodmap ~/.Xmodmap"
                spawn "xsetroot -cursor_name left_ptr"
                spawn "xset -dpms"
                spawn "setterm -blank 0 -powersave off -powerdown 0"
                spawn "xset s off"
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
--                          , ppLayout = xmobarColor "#400000" ""
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
    , promptBorderWidth = 0
    , position = Bottom
    , height = 14
    , historySize = 50
    }
 
--- My Theme For Tabbed layout
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
myLayoutHook  = onWorkspace "1:chat" imLayout $ onWorkspace "2:webdev" webL $ onWorkspace "8:gimp" gimpL $ standardLayouts 
   where
        standardLayouts =   avoidStruts  $ (tiled ||| tabLayout ||| reflectTiled ||| Mirror tiled |||  Grid ||| Full) 
 
        --Layouts
        tiled     = smartBorders (ResizableTall 1 (2/100) (1/2) [])
        reflectTiled = (reflectHoriz tiled)
        tabLayout = (tabbedBottom shrinkText myTheme)
        full    = noBorders Full
 
        --Im Layout
        imLayout = avoidStruts $ smartBorders $ withIM ratio pidginRoster $ reflectHoriz $ withIM skypeRatio skypeRoster (tiled ||| reflectTiled ||| Grid) where
                chatLayout      = Grid
                ratio = (1%9)
                skypeRatio = (1%8)
                pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
                skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm")) 
        --Gimp Layout
        gimpL = avoidStruts $ smartBorders $ withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") tabLayout

        --web Layout
        --webL      = avoidStruts $  tabLayout  ||| tiled ||| reflectHoriz tiled |||  full 
        webL      = avoidStruts $ full 
 
        --VirtualLayout
        fullL = avoidStruts $ full
 
 
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
myWorkspaces = ["1:chat", "2:webdev", "3:code", "4:web", "5:vbox" ,"6:misc", "7:vid", "8:gimp", "9:pdf"]
--
 
-- Switch to the "2:web" workspace
--viewWeb = windows (W.greedyView "2:web")                           -- (0,0a)
--
 
--Search engines to be selected :  [google (g), wikipedia (w) , youtube (y) , maps (m), dictionary (d) , wikipedia (w), bbs (b) ,aur (r), wiki (a) , TPB (t), mininova (n), isohunt (i) ]
--keybinding: hit mod + s + <searchengine>
--searchEngineMap method = M.fromList $
--      [ ((0, xK_g), method S.google )
--       , ((0, xK_y), method S.youtube )
--       , ((0, xK_m), method S.maps )
--       , ((0, xK_d), method S.dictionary )
--       , ((0, xK_w), method S.wikipedia )
--       , ((0, xK_h), method S.hoogle )
--       , ((0, xK_i), method S.isohunt )
--       , ((0, xK_b), method $ S.searchEngine "archbbs" "http://bbs.archlinux.org/search.php?action=search&keywords=")
--       , ((0, xK_r), method $ S.searchEngine "AUR" "http://aur.archlinux.org/packages.php?O=0&L=0&C=0&K=")
--       , ((0, xK_a), method $ S.searchEngine "archwiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
--       ]
 
 
--ssh = "ssh homoludens@tv "
-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- killing programs
    [ ((modMask, xK_F1), spawn $ XMonad.terminal conf)
    , ((modMask, xK_c ), kill)
 
    -- opening program launcher / search engine
--   , ((modMask , xK_s ), SM.submap $ searchEngineMap $ S.promptSearchBrowser myXPConfig "uzbl-browser")
    ,((modMask , xK_F2), runOrRaisePrompt myXPConfig)
--    ,((modMask , xK_F2), shellPrompt myXPConfig)

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
 
--    , ((mod4Mask .|. controlMask, xK_Home), spawn "mocp -G") -- play/pause song
--    , ((mod4Mask .|. controlMask, xK_End), spawn "mocp -s") -- stop playback
--    , ((mod4Mask .|. controlMask, xK_Prior), spawn "mocp -r") -- previous song
--    , ((mod4Mask .|. controlMask, xK_Next), spawn "mocp -f") -- next song
 
    -- scratchpad
    , ((modMask , xK_grave), scratchpadSpawnAction defaultConfig  {terminal = myTerminal}) 
 
    --Programs
--    , ((modMask .|.  shiftMask, xK_f ), spawn "firefox")
--    , ((modMask .|.  shiftMask, xK_p ), spawn "pidgin")
--    , ((modMask .|.  shiftMask, xK_b ), spawn "chromium")

    -- volume control
    , ((0, 0x1008ff13), spawn "amixer -q set Master 1dB+") -- raise volume
    , ((0, 0x1008ff11), spawn "amixer -q set Master 1dB-") -- lower volume

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
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
