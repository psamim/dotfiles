import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Actions.WindowBringer
import XMonad.Hooks.FadeInactive
import XMonad.Util.WorkspaceCompare (getSortByIndex)

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = defaultPP { ppCurrent = xmobarColor "#429942" "" 
                     , ppHidden = xmobarColor "#C98F0A" ""
                     , ppHiddenNoWindows = xmobarColor "#C9A34E" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]" 
                     , ppLayout = xmobarColor "#C9A34E" ""
                     , ppTitle =  xmobarColor "#C9A34E" "" . shorten 80
                     , ppSep = xmobarColor "#429942" "" " | "
                     , ppSort = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
                     }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig = ewmh $ defaultConfig
    { modMask = mod4Mask -- use the Windows button as mod
    , terminal = "alacritty"
    , borderWidth = 0
    , normalBorderColor  = "#333333"
    , focusedBorderColor = "#FF0000"
    , workspaces = myWorkspaces
    , logHook = myLogHook
    , manageHook = manageHook defaultConfig <+> manageDocks <+> myManageHook <+> namedScratchpadManageHook scratchpads
    , layoutHook = smartBorders $ desktopLayoutModifiers $ spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True $
                   layoutHook def
    } `additionalKeys` myKeys `additionalKeysP` [
      -- Media keys
      ("<XF86AudioLowerVolume>", spawn "pactl -- set-sink-volume 8 -10%"),
      ("<XF86AudioRaiseVolume>", spawn "pactl -- set-sink-volume 8 +10%"),
      ("<XF86AudioMute>", spawn "pactl -- set-sink-mute 8 toggle")
    ]

myKeys = [
      ((mod4Mask, xK_d), spawn "rofi -show combi"),
      ((mod4Mask, xK_p), spawn "rofi-pass"),
      (((0, xK_Print)), spawn "flameshot gui"),
      ((mod4Mask, xK_v), namedScratchpadAction scratchpads "term"),
      ((mod4Mask, xK_c), namedScratchpadAction scratchpads "nvim"),
      ((mod4Mask, xK_b), sendMessage ToggleStruts),
      ((mod4Mask, xK_g     ), gotoMenu)
    ]
    ++
    [((m .|. noModMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_F1..xK_F12]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

firefox = "\xf269"
code = "\xf121"
music = "\xf001"
chat = "\xf075"

myWorkspaces = [code, firefox, chat, music]

myManageHook = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myFloats]
    , [ title       =? t --> doFloat           | t <- myOtherFloats]
    , [ className   =? c --> doF (W.shift firefox) | c <- webApps]
    , [ className   =? c --> doF (W.shift chat) | c <- ircApps]
    ]
  where myFloats      = ["MPlayer", "Gimp", "plasma", "yakuake", "Yakuake",
                         "plasma", "Plasma", "plasma-desktop", "Plasma-desktop",
                         "krunner" , "ksplashsimple", "ksplashqml", "plasmashell"]
        myOtherFloats = ["alsamixer"]
        webApps       = ["firefox"]
        ircApps       = ["Ksirc","TelegramDesktop"]              

scratchpads = [
  NS "term" "alacritty --title term" (title =? "term") (customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5)),
  NS "nvim" "alacritty --title nvim" (title =? "nvim") (customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5))
  ] where role = stringProperty "WM_WINDOW_ROLE"

myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.8
