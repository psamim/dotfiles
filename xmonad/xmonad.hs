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
import XMonad.Actions.DynamicProjects
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.CycleWS

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = defaultPP { ppCurrent = xmobarColor "#aa5500" "" 
                     , ppHidden = xmobarColor "#FFFFFF" ""
                     , ppHiddenNoWindows = xmobarColor "#FFFFFF" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]" 
                     , ppLayout = xmobarColor "#FFFFFF" ""
                     , ppTitle =  xmobarColor "#FFFFFF" "" . shorten 80
                     , ppSep = xmobarColor "#FFFFFF" "" " | "
                     , ppSort = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
                     }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig = dynamicProjects projects $ ewmh $ defaultConfig
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
      (((0, xK_Print)), spawn "scrot -e 'kolourpaint $f'"),
      ((mod4Mask, xK_x), namedScratchpadAction scratchpads "term"),
      -- ((mod4Mask, xK_c), namedScratchpadAction scratchpads "nvim"),
      ((mod4Mask, xK_b), sendMessage ToggleStruts),
      ((mod4Mask, xK_g), gotoMenu),
      -- ((mod4Mask, xK_l), nextWS),
      -- ((mod4Mask, xK_h), prevWS),
      ((mod4Mask, xK_v), spawn "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'"),
      -- ((mod4Mask, xK_c), lookupProject editor >>= mapM_ switchProject),
      ((0, xK_F1), lookupProject chrome >>= mapM_ switchProject),
      ((0, xK_F2), lookupProject firefox >>= mapM_ switchProject),
      ((0, xK_F3), lookupProject chat >>= mapM_ switchProject),
      ((0, xK_F4), lookupProject music >>= mapM_ switchProject),
      ((mod4Mask, xK_c), bindOn [(editor, toggleWS), ("", lookupProject editor >>= mapM_ switchProject)])
    ]
    -- ++
    -- [((m .|. noModMask, k), windows $ f i)
    --     | (i, k) <- zip myWorkspaces [xK_F1..xK_F12]
    --     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

firefox = "\xf269"
chrome = "\xf268"
music = "\xf001"
chat = "\xf075"
editor = "\xf121"

myWorkspaces = [chrome, firefox, chat, music, editor]

myManageHook = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myFloats]
    , [ title       =? t --> doFloat           | t <- myOtherFloats]
    , [ title =? "editor" --> doRectFloat (W.RationalRect (1/12) (1/12) (10/12) (10/12))]
    , [ className   =? c --> doF (W.shift firefox) | c <- webApps]
    , [ className   =? c --> doF (W.shift chat) | c <- chatApps]
    , [ className   =? c --> doF (W.shift chrome) | c <- chromeApps]
    ]
  where myFloats      = ["MPlayer", "Gimp", "plasma", "yakuake", "Yakuake",
                         "plasma", "Plasma", "plasma-desktop", "Plasma-desktop",
                         "krunner" , "ksplashsimple", "ksplashqml", "plasmashell"]
        myOtherFloats = ["alsamixer"]
        webApps       = ["firefox"]
        chromeApps = ["Google-chrome"]
        chatApps       = ["TelegramDesktop", "Slack"]              

scratchpads = [
  NS "term" "alacritty --title term" (title =? "term") (customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5))
  ] where role = stringProperty "WM_WINDOW_ROLE"

myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.8

projects :: [Project]
projects =
  [ Project { projectName      = firefox
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "firefox"
            }

  , Project { projectName      = editor
            , projectDirectory = "~/workspace"
            , projectStartHook = Just $ do spawn "alacritty --title editor"
            }

  , Project { projectName      = chrome
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "google-chrome-stable --profile-directory=\"Default\""
            }

  , Project { projectName      = chat
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "telegram-desktop"
                                           spawn "slack"
                                           -- spawn "firefox --new-window https://web.whatsapp.com"
                                           -- spawn "xdotool search --sync --onlyvisible --name \"WhatsApp\" windowactivate key F11"
                                           -- spawn "skypeforlinux"

            }

  , Project { projectName      = music
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "pavucontrol"
                                           spawn "spotify"
            }
  ]
