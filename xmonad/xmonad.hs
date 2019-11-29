import           Data.Char                       (toLower)
import           Data.List                       (isInfixOf)
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.PerWorkspaceKeys
import           XMonad.Actions.WindowBringer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Prompt
import qualified XMonad.StackSet                 as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.WorkspaceCompare    (getSortByIndex)

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = defaultPP { ppCurrent = xmobarColor "#aa5500" "" . myIconMapper
                     , ppHidden = xmobarColor "#FFFFFF" "" . myIconMapper
                     , ppHiddenNoWindows = xmobarColor "#FFFFFF" "" . myIconMapper
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]"
                     , ppLayout = \x -> ""
                     , ppTitle =  xmobarColor "#FFFFFF" "" . shorten 80
                     , ppSep = xmobarColor "#FFFFFF" "" " | "
                     , ppSort = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
                     }

myIconMapper = (\x -> case x of
                         "editor"  ->  editor
                         "chat"    ->  chat
                         "star"    ->  star
                         "firefox" ->  firefox
                         "music"   ->  music
                         "todo"   ->  todo
                         _         -> pad x
                         )

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
    , layoutHook = smartBorders $ spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True $
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
      ((mod4Mask, xK_o), nextWS),
      ((mod4Mask, xK_i), prevWS),
      ((mod4Mask, xK_v), spawn "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'"),
      ((mod4Mask, xK_w), switchProjectPrompt  promptConfig),
      ((mod4Mask, xK_Tab), toggleWS),
      ((0, xK_F1), bindProject "star"),
      ((0, xK_F2), bindProject "firefox"),
      ((0, xK_F3), bindProject "chat"),
      ((0, xK_F4), bindProject "music"),
      ((mod4Mask, xK_c), bindProject "editor"),
      ((mod4Mask, xK_m), bindProject "todo")
    ]
    -- ++
    -- [((m .|. noModMask, k), windows $ f i)
    --     | (i, k) <- zip myWorkspaces [xK_F1..xK_F12]
    --     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    --

bindProject = \x -> bindOn [(x, toggleWS), ("", lookupProject x >>= mapM_ switchProject)]

promptConfig :: XPConfig
promptConfig = def
  { position          = CenteredAt (1/3) (1/2)
  , height            = 50
  , font              = "xft:Fira Code:size=14"
  , bgColor           = "#262e3d"
  , fgColor           = "#eeeeee"
  , fgHLight          = "#ffffff"
  , bgHLight          = "#c50ed2"
  , borderColor       = "#0D1729"
  , promptBorderWidth = 4
  , maxComplRows      = Just 12
  , alwaysHighlight   = False
  , promptKeymap      = emacsLikeXPKeymap
  , searchPredicate   = predicateFunction
  }

-- | A case-insensitive substring predicate function.
predicateFunction :: String -> String -> Bool
predicateFunction x y = lc x `isInfixOf` lc y where lc = map toLower

firefox = "\xf269"
star = "\xf005"
music = "\xf001"
chat = "\xf075"
editor = "\xf121"
todo = "\xf200"

myWorkspaces = ["star", "firefox", "chat", "music", "editor"]

myManageHook = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myFloats]
    , [ title       =? t --> doFloat           | t <- myOtherFloats]
    , [ title =? "editor" --> doRectFloat (W.RationalRect (1/12) (1/12) (10/12) (10/12))]
    , [ title =? "TODOs" --> doRectFloat (W.RationalRect (1/12) (1/12) (10/12) (10/12))]
    , [ className   =? c --> doF (W.shift "firefox") | c <- webApps]
    , [ className   =? c --> doF (W.shift "chat") | c <- chatApps]
    , [ className   =? c --> doF (W.shift "star") | c <- starApps]
    ]
  where myFloats      = ["MPlayer", "Gimp", "plasma", "yakuake", "Yakuake",
                         "plasma", "Plasma", "plasma-desktop", "Plasma-desktop", "File Operation Progress",
                         "krunner" , "ksplashsimple", "ksplashqml", "plasmashell"]
        myOtherFloats = ["alsamixer", "File Operation Progress", "Launch Application"]
        webApps       = ["firefox"]
        starApps = ["Google-chrome"]
        chatApps       = ["TelegramDesktop", "Slack"]

scratchpads = [
  NS "term" "alacritty --title term" (title =? "term") (customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5))
  ] where role = stringProperty "WM_WINDOW_ROLE"

myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.8

projects :: [Project]
projects =
  [ Project { projectName      = "firefox"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "firefox"
            }

  , Project { projectName      = "editor"
            , projectDirectory = "~/workspace"
            , projectStartHook = Just $ do spawn "alacritty --title editor"
            }

  , Project { projectName      = "star"
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            -- , projectStartHook = Just $ do spawn "google-chrome-stable --profile-directory=\"Default\""
            }

  , Project { projectName      = "chat"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "telegram-desktop"
                                           spawn "slack"
                                           -- spawn "firefox --new-window https://web.whatsapp.com"
                                           -- spawn "xdotool search --sync --onlyvisible --name \"WhatsApp\" windowactivate key F11"
                                           -- spawn "skypeforlinux"

            }

  , Project { projectName      = "music"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "pavucontrol"
                                           spawn "spotify"
            }

  , Project { projectName      = "todo"
            , projectDirectory = "~/Notes"
            , projectStartHook = Just $ do spawn "/home/samim/.bin/todo-org"
            }
  ]
