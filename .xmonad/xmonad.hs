
-------------------------------------------------------------------------------
-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib
import IO (Handle, hPutStrLn) 

-- utils
import XMonad.Util.Run (spawnPipe)

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile

-------------------------------------------------------------------------------
-- Main --
main = do
       h <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
       xmonad $ defaultConfig 
              { workspaces = workspaces'
              , modMask = modMask'
              , borderWidth = borderWidth'
              , normalBorderColor = normalBorderColor'
              , focusedBorderColor = focusedBorderColor'
              , terminal = terminal'
              , keys = keys'
              , logHook = logHook' h 
              , layoutHook = layoutHook'
              , manageHook = manageHook'
            }

-------------------------------------------------------------------------------
-- Window Rules  --
myManageHook = composeAll . concat $
  [ [ className =? c --> doFloat | c <- myCFloats ]
  , [ title =? t --> doFloat | t <- myTFloats ]
  , [ className =? "Shiretoko" --> doShift "www" ]
  ]
  where
  myCFloats = [ "Nitrogen", "Xmessage" ]
  myTFloats = [ "Downloads", "Save as..." ]

-------------------------------------------------------------------------------
-- Hooks --
manageHook' :: ManageHook
manageHook' = (doF W.swapDown) <+> manageHook defaultConfig <+> manageDocks <+> myManageHook

logHook' :: Handle ->  X ()
logHook' h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }

layoutHook' = customLayout

-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP :: PP
customPP = defaultPP { ppCurrent = xmobarColor "#F92672" "" . wrap "<fc=#888888>[</fc>" "<fc=#888888>]</fc>"
                     , ppTitle = xmobarColor "#FFFFFF" "" . shorten 50
                     , ppSep =  "<fc=#888888> | </fc>"
                     , ppHiddenNoWindows = xmobarColor "#FFFFFF" ""
                     , ppUrgent = xmobarColor "#262626" "" . wrap "[" "]"
                     , ppLayout = xmobarColor "#FFFFFF" ""
                     }

-- borders
borderWidth' :: Dimension
borderWidth' = 1

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#333333"
focusedBorderColor' = "#888888"

-- workspaces
workspaces' :: [WorkspaceId]
workspaces' = ["main", "www", "dev", "gfx", "misc"]

-- layouts
customLayout = avoidStruts $ smartBorders tiled ||| smartBorders (Mirror tiled)  ||| noBorders Full
  where
    tiled = ResizableTall 1 (2/100) (1/2) []

-------------------------------------------------------------------------------
-- Terminal --
terminal' :: String
terminal' = "urxvtc"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' :: KeyMask
modMask' = mod4Mask

-- keys
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf) 
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu -b -i -fn 'terminus' -nb '#3F3F3F' -nf '#FFFFFF' -sb '#93E0E3' -sf '#3F3F3F'` && eval \"exec $exe\"") 
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask .|. shiftMask, xK_m     ), spawn "claws-mail")

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,               xK_b     ), sendMessage ToggleStruts)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- mpd controls
    , ((modMask .|. controlMask,  xK_h     ), spawn "ncmpcpp prev")
    , ((modMask .|. controlMask,  xK_t     ), spawn "ncmpcpp pause")
    , ((modMask .|. controlMask,  xK_n     ), spawn "ncmpcpp play")
    , ((modMask .|. controlMask,  xK_s     ), spawn "ncmpcpp next")
    , ((modMask .|. controlMask,  xK_g     ), spawn "ncmpcpp toggle")
    , ((modMask .|. controlMask,  xK_c     ), spawn "ncmpcpp stop")
    , ((modMask .|. controlMask,  xK_r     ), spawn "ncmpcpp volume +4%")
    , ((modMask .|. controlMask,  xK_l     ), spawn "ncmpcpp volume -4%")

    -- misc
    , ((modMask,              xK_Print  ), spawn "sleep 5; scrot screen_%Y-%m-%d.png")
    , ((0,                    xK_Print  ), spawn "scrot")

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------------------
