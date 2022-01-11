-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
-- local menubar = require("menubar")
-- local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
-- require("awful.hotkeys_popup.keys")

local sharedtags = require("sharedtags")

local vicious = require("vicious")
local revelation = require("revelation")
local wallpapers = require("wallpaper")

-- package.loaded["naughty.dbus"] = {}
naughty.config.defaults.icon_size = 32
naughty.config.defaults.margin = dpi(10)

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify(
        {
            preset = naughty.config.presets.critical,
            title = "Oops, there were errors during startup!",
            text = awesome.startup_errors
        }
    )
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal(
        "debug::error",
        function(err)
            -- Make sure we don't go into an endless error loop
            if in_error then
                return
            end
            in_error = true

            naughty.notify(
                {
                    preset = naughty.config.presets.critical,
                    title = "Oops, an error happened!",
                    text = tostring(err)
                }
            )
            in_error = false
        end
    )
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
local theme_path = string.format("%s/.config/awesome/themes/%s/theme.lua", os.getenv("HOME"), "default")
beautiful.init(theme_path)

beautiful.taglist_spacing = 0
beautiful.bar_width = 3
-- beautiful.border_focus  = "#ff5500"

-- This is used later as the default terminal and editor to run.
terminal = "alacritty"
editor = os.getenv("EDITOR") or "nano"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    -- awful.layout.suit.floating,
    awful.layout.suit.tile,
    -- awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}
-- }}}

revelation.init()

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
    -- {
    --     "hotkeys",
    --     function()
    --         hotkeys_popup.show_help(nil, awful.screen.focused())
    --     end
    -- },
    {"manual", terminal .. " -e man awesome"},
    {"edit config", editor_cmd .. " " .. awesome.conffile},
    {"restart", awesome.restart},
    {
        "quit",
        function()
            awesome.quit()
        end
    }
}

mymainmenu =
    awful.menu(
    {
        items = {
            {"awesome", myawesomemenu, beautiful.awesome_icon},
            {"open terminal", terminal}
        }
    }
)

mylauncher =
    awful.widget.launcher(
    {
        image = beautiful.awesome_icon,
        menu = mymainmenu
    }
)

-- Menubar configuration
-- menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

orgmode = awful.widget.watch('bash -c "/home/samim/.bin/clocking"', 10)

cpu_widget = require("awesome-wm-widgets.cpu-widget.cpu-widget")
logout_menu_widget = require("awesome-wm-widgets.logout-menu-widget.logout-menu")
net_speed_widget = require("awesome-wm-widgets.net-speed-widget.net-speed")
-- ram_widget = require("awesome-wm-widgets.ram-widget.ram-widget")
-- popup = require("notifs.notif-center.notif_popup")
-- volume_widget = require("awesome-wm-widgets.volume-widget.volume")

memwidget = wibox.widget.textbox()
vicious.cache(vicious.widgets.mem)
vicious.register(memwidget, vicious.widgets.mem, "Mem: $1%", 13)

-- cpuwidget = awful.widget.graph()
-- cpuwidget:set_width(50)
-- cpuwidget:set_background_color "#494B4F44"
-- cpuwidget:set_color {
--   type = "linear",
--   from = {0, 0},
--   to = {100, 100},
--   stops = {
--     {1, "#AECF96"},
--     {0.2, "#88A175"},
--     {0, "#FF5656"}
--   }
-- }
-- vicious.register(cpuwidget, vicious.widgets.cpu, "$1", 3)

wifi = wibox.widget.textbox()
vicious.cache(vicious.widgets.wifiiw)
vicious.register(wifi, vicious.widgets.wifiiw, "${ssid}", 13, "wlp3s0")

notification_widget_box =
    wibox.widget {
    widget = wibox.widget.imagebox,
    image = "/home/samim/.dotfiles/icons/notifications.svg",
    resize = true,
    buttons = gears.table.join(
        awful.button(
            {},
            1,
            function()
                if naughty.is_suspended() then
                    notification_widget_box:set_image("/home/samim/.dotfiles/icons/notifications.svg")
                    naughty.resume()
                else
                    notification_widget_box:set_image("/home/samim/.dotfiles/icons/notifications_off.svg")
                    naughty.suspend()
                end
            end
        )
    )
}

notification_widget =
    wibox.widget {
    widget = wibox.container.margin,
    left = 2,
    right = 2,
    top = 3,
    bottom = 3,
    {
        notification_widget_box,
        layout = wibox.layout.fixed.horizontal
    }
}

mic_widget_box =
    wibox.widget {
    widget = wibox.widget.imagebox,
    image = "/home/samim/.dotfiles/icons/microphone.svg",
    resize = true,
    buttons = gears.table.join(
        awful.button(
            {},
            1,
            function()
                toggleMute()
            end
        )
    )
}

function toggleMute()
    awful.spawn.easy_async(
        "toggle-mute",
        function(stdout)
            if string.find(stdout, "ON") then
                mic_widget_box:set_image("/home/samim/.dotfiles/icons/microphone.svg")
            else
                mic_widget_box:set_image("/home/samim/.dotfiles/icons/microphone-off.svg")
            end
        end
    )
end

mic_widget =
    wibox.widget {
    widget = wibox.container.margin,
    left = 2,
    right = 2,
    top = 3,
    bottom = 3,
    {
        mic_widget_box,
        layout = wibox.layout.fixed.horizontal
    }
}

-- background_widget_box =
--     wibox.widget {
--     widget = wibox.widget.imagebox,
--     image = "/home/samim/.dotfiles/icons/background.svg",
--     resize = true,
--     buttons = gears.table.join(
--         awful.button(
--             {},
--             1,
--             function()
--                 awful.spawn("systemctl --user restart feh-wallpaper  ", false)
--             end
--         )
--     )
-- }

-- background_widget =
--     wibox.widget {
--     widget = wibox.container.margin,
--     left = 2,
--     right = 2,
--     top = 3,
--     bottom = 3,
--     {
--         background_widget_box,
--         layout = wibox.layout.fixed.horizontal
--     }
-- }

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock()

-- Create a wibox for each screen and add it
local taglist_buttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(t)
            t:view_only()
        end
    ),
    awful.button(
        {modkey},
        1,
        function(t)
            if client.focus then
                client.focus:move_to_tag(t)
            end
        end
    ),
    awful.button({}, 3, awful.tag.viewtoggle),
    awful.button(
        {modkey},
        3,
        function(t)
            if client.focus then
                client.focus:toggle_tag(t)
            end
        end
    ),
    awful.button(
        {},
        4,
        function(t)
            awful.tag.viewnext(t.screen)
        end
    ),
    awful.button(
        {},
        5,
        function(t)
            awful.tag.viewprev(t.screen)
        end
    )
)

local tasklist_buttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(c)
            if c == client.focus then
                c.minimized = true
            else
                c:emit_signal("request::activate", "tasklist", {raise = true})
            end
        end
    ),
    awful.button(
        {},
        3,
        function()
            awful.menu.client_list({theme = {width = 250}})
        end
    ),
    awful.button(
        {},
        4,
        function()
            awful.client.focus.byidx(1)
        end
    ),
    awful.button(
        {},
        5,
        function()
            awful.client.focus.byidx(-1)
        end
    )
)

-- local function set_wallpaper(s)
--     -- Wallpaper
--     if beautiful.wallpaper then
--         local wallpaper = beautiful.wallpaper
--         -- If wallpaper is a function, call it with the screen
--         if type(wallpaper) == "function" then
--             wallpaper = wallpaper(s)
--         end
--         gears.wallpaper.maximized(wallpaper, s, true)
--     end
-- end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
-- screen.connect_signal("property::geometry", set_wallpaper)
--

local current_activity = "me"
local tags = {
    me = sharedtags(
        {
            {name = "1", layout = awful.layout.layouts[1], icon = "/home/samim/.dotfiles/icons/home.png"},
            {name = "2", layout = awful.layout.layouts[1], icon = "/home/samim/.dotfiles/icons/connections.png"},
            {name = "3", layout = awful.layout.layouts[1], icon = "/home/samim/.dotfiles/icons/chat.png"},
            {name = "4", layout = awful.layout.layouts[1], icon = "/home/samim/.dotfiles/icons/code.png"}
        }
    ),
    work = sharedtags(
        {
            {name = "1", layout = awful.layout.layouts[1], icon = "/home/samim/.dotfiles/icons/home.png"},
            {name = "2", layout = awful.layout.layouts[1], icon = "/home/samim/.dotfiles/icons/connections.png"},
            {name = "3", layout = awful.layout.layouts[1], icon = "/home/samim/.dotfiles/icons/chat.png"},
            {name = "4", layout = awful.layout.layouts[1], icon = "/home/samim/.dotfiles/icons/code.png"}
        }
    ),
    shared = sharedtags(
        {
            {name = "5", layout = awful.layout.layouts[1], icon = "/home/samim/.dotfiles/icons/cal.png"},
            {name = "6", layout = awful.layout.layouts[1], icon = "/home/samim/.dotfiles/icons/puzzle.png"}
        }
    )
}

activity_widget_box =
    wibox.widget {
    -- widget = wibox.widget.textbox,
    -- markup = tostring(current_activity),
    -- align = "center",
    -- valign = "center",
    widget = wibox.widget.imagebox,
    image = "/home/samim/.dotfiles/icons/man.svg",
    resize = true,
    buttons = gears.table.join(
        awful.button(
            {},
            1,
            function()
                if current_activity == "me" then
                    current_activity = "work"
                    activity_widget_box:set_image("/home/samim/.dotfiles/icons/work.svg")
                else
                    current_activity = "me"
                    activity_widget_box:set_image("/home/samim/.dotfiles/icons/man.svg")
                end
            end
        )
    )
}

activity_widget =
    wibox.widget {
    widget = wibox.container.margin,
    left = 6,
    right = 6,
    top = 6,
    bottom = 6,
    {
        activity_widget_box,
        layout = wibox.layout.fixed.horizontal
    }
}

function getTag(index)
    if index < 5 then
        return tags[current_activity][index]
    end
    return tags["shared"][index - 4]
end

decreased_padding_tag_names = {}

function enable_paddings(m_screen)
    local one_screen = screen.count() == 1
    if m_screen.geometry.width > 1920 or one_screen then
        local current_tag_name = m_screen.selected_tag.name
        m_screen.padding = {right = 260, left = 260}
        decreased_padding_tag_names[current_tag_name] = true
    end
end

function disable_paddings(m_screen)
    local one_screen = screen.count() == 1
    if m_screen.geometry.width > 1920 or one_screen then
        local current_tag_names = m_screen.selected_tag.name
        m_screen.padding = {right = 0, left = 0}
        decreased_padding_tag_names[current_tag_names] = false
    end
end

function dump(o)
    if type(o) == "table" then
        local s = "{ "
        for k, v in pairs(o) do
            if type(k) ~= "number" then
                k = '"' .. k .. '"'
            end
            s = s .. "[" .. k .. "] = " .. dump(v) .. ","
        end
        return s .. "} "
    else
        return tostring(o)
    end
end

function toggle_paddings()
    for s in screen do
        toggle_paddings_for_screen(s)
    end
end

function toggle_paddings_for_screen(s, inverse)
    local selected_tag = s.selected_tag
    inverse = inverse ~= nil
    if selected_tag == nil then
        return
    end
    local current_tag_name = selected_tag.name
    local enabled = decreased_padding_tag_names[tostring(current_tag_name)] or false
    if inverse then
        enabled = not enabled
    end
    if enabled then
        disable_paddings(s)
    else
        enable_paddings(s)
    end
end

awful.screen.connect_for_each_screen(
    function(s)
        -- Wallpaper
        -- set_wallpaper(s)

        -- Each screen has its own tag table.
        -- awful.tag({ "🔵", "🟣", "🟢", "⚫", "🟡", "🔴", "🟤" }, s, awful.layout.layouts[1])
        --

        s:connect_signal(
            "tag::history::update",
            function(s)
                toggle_paddings_for_screen(s, true)
            end
        )

        -- s:connect_signal("removed", awesome.restart)
        -- s:connect_signal("added", awesome.restart)

        -- Create a promptbox for each screen
        -- s.mypromptbox = awful.widget.prompt()
        -- Create an imagebox widget which will contain an icon indicating which layout we're using.
        -- We need one layoutbox per screen.
        s.mylayoutbox = awful.widget.layoutbox(s)
        s.mylayoutbox:buttons(
            gears.table.join(
                awful.button(
                    {},
                    1,
                    function()
                        awful.layout.inc(1)
                    end
                ),
                awful.button(
                    {},
                    3,
                    function()
                        awful.layout.inc(-1)
                    end
                ),
                awful.button(
                    {},
                    4,
                    function()
                        awful.layout.inc(1)
                    end
                ),
                awful.button(
                    {},
                    5,
                    function()
                        awful.layout.inc(-1)
                    end
                )
            )
        )
        -- Create a taglist widget
        s.mytaglist =
            awful.widget.taglist {
            screen = s,
            filter = function(t)
                local is_in_activity = false
                local is_in_shared = false
                local noempty = awful.widget.taglist.filter.noempty(t)

                for _, v in pairs(tags[current_activity]) do
                    if v == t then
                        is_in_activity = true
                        break
                    end
                end

                for _, v in pairs(tags["shared"]) do
                    if v == t then
                        is_in_shared = true
                        break
                    end
                end

                return noempty and (is_in_activity or is_in_shared)
            end,
            buttons = taglist_buttons,
            layout = {
                spacing = 12,
                layout = wibox.layout.fixed.horizontal
            },
            widget_template = {
                {
                    {
                        {
                            {
                                id = "icon_role",
                                widget = wibox.widget.imagebox
                                -- id = "text_role",
                                -- widget = wibox.widget.textbox
                            },
                            margins = 0,
                            widget = wibox.container.margin
                        },
                        layout = wibox.layout.fixed.horizontal
                    },
                    left = 4,
                    right = 4,
                    top = 4,
                    bottom = 4,
                    widget = wibox.container.margin
                },
                id = "background_role",
                widget = wibox.container.background
            }
        }

        -- Create a tasklist widget
        s.mytasklist =
            awful.widget.tasklist {
            screen = s,
            filter = awful.widget.tasklist.filter.currenttags,
            buttons = tasklist_buttons,
            -- style = {
            --     shape_border_width = 1,
            --     shape_border_color = "#777777",
            --     shape = gears.shape.rounded_bar
            -- }
            widget_template = {
                {
                    {
                        -- {
                        -- {
                        --     id     = 'icon_role',
                        --     widget = wibox.widget.imagebox,
                        -- },
                        -- margins = 2,
                        -- widget  = wibox.container.margin,
                        -- },
                        {
                            id = "text_role",
                            widget = wibox.widget.textbox
                        },
                        layout = wibox.layout.fixed.horizontal
                    },
                    left = 10,
                    right = 10,
                    widget = wibox.container.margin
                },
                id = "background_role",
                widget = wibox.container.background
            }
        }

        -- Create the wibox
        width = s.geometry["width"] - 80
        function custom_shape(cr, width, height)
            gears.shape.rounded_rect(cr, width, height, 8)
        end

        s.systray = wibox.widget.systray()
        s.systray.visible = true

        s.mywibox =
            awful.wibar(
            {
                position = "bottom",
                screen = s,
                height = 28,
                width = width,
                shape = custom_shape,
                bg = beautiful.bg_normal .. "CC"
            }
        )

        -- awful.wibar {
        --   position = 'bottom',
        --   height   = 23,
        -- }

        -- Add widgets to the wibox
        s.mywibox:setup {
            layout = wibox.layout.align.horizontal,
            {
                -- Left widgets
                layout = wibox.layout.fixed.horizontal,
                -- mylauncher,
                s.mytaglist
                -- s.mypromptbox,
            },
            {
                wibox.widget {
                    widget = wibox.container.margin,
                    left = 16,
                    right = 16,
                    {
                        orgmode,
                        layout = wibox.layout.fixed.horizontal
                    }
                },
                s.mytasklist,
                layout = wibox.layout.fixed.horizontal
            },
            {
                -- Right widgets
                layout = wibox.layout.fixed.horizontal,
                -- ram_widget({color_used = "#980000"}),
                -- cpuwidget,
                cpu_widget(
                    {
                        width = 70,
                        step_width = 2,
                        step_spacing = 0
                        -- color = '#434c5e'
                    }
                ),
                wibox.widget {
                    widget = wibox.container.margin,
                    left = 12,
                    right = 12,
                    {
                        memwidget,
                        layout = wibox.layout.fixed.horizontal
                    }
                },
                wibox.widget {
                    widget = wibox.container.margin,
                    left = 6,
                    right = 6,
                    {
                        wifi,
                        layout = wibox.layout.fixed.horizontal
                    }
                },
                net_speed_widget(),
                wibox.widget {
                    {
                        {
                            {
                                s.systray,
                                -- volume_widget {
                                --     widget_type = "icon"
                                -- },
                                notification_widget,
                                mic_widget,
                                -- background_widget,
                                layout = wibox.layout.fixed.horizontal
                            },
                            left = 8,
                            right = 6,
                            widget = wibox.container.margin
                        },
                        bg = "#222222",
                        shape = gears.shape.rounded_rect,
                        widget = wibox.container.background
                    },
                    top = 3,
                    bottom = 3,
                    widget = wibox.container.margin
                },
                mykeyboardlayout,
                mytextclock,
                -- s.mylayoutbox,
                activity_widget,
                wibox.widget {
                    widget = wibox.container.margin,
                    left = 6,
                    right = 6,
                    {
                        logout_menu_widget(),
                        layout = wibox.layout.fixed.horizontal
                    }
                }
            }
        }
    end
)
-- }}}

-- {{{ Mouse bindings
root.buttons(
    gears.table.join(
        awful.button(
            {},
            3,
            function()
                mymainmenu:toggle()
            end
        )
        -- awful.button({}, 4, awful.tag.viewnext),
        -- awful.button({}, 5, awful.tag.viewprev)
    )
)
-- }}}

-- {{{ Key bindings
globalkeys =
    gears.table.join(
    awful.key({modkey}, "e", revelation),
    -- personal widget notification center
    -- awful.key(
    --     {modkey},
    --     "i",
    --     function()
    --         popup.visible = not popup.visible
    --     end,
    --     {description = "show notification center", group = "awesome"}
    -- ),
    awful.key({}, "F8", toggleMute),
    awful.key(
        {modkey},
        "-",
        function()
            toggle_paddings()
        end,
        {description = "Toggle paddings", group = "custom"}
    ),
    awful.key(
        {modkey},
        "=",
        function()
            awful.screen.focused().systray.visible = not awful.screen.focused().systray.visible
        end,
        {description = "Toggle systray visibility", group = "custom"}
    ),
    -- awful.key({modkey}, "s", hotkeys_popup.show_help, {description = "show help", group = "awesome"}),
    awful.key(
        {},
        "XF86AudioLowerVolume",
        function()
            awful.spawn("amixer -D pulse sset Master 2%-", false)
        end,
        {description = "decrease volume", group = "sound"}
    ),
    awful.key(
        {},
        "XF86AudioRaiseVolume",
        function()
            awful.spawn("amixer -D pulse sset Master 2%+", false)
        end,
        {description = "raise volume", group = "sound"}
    ),
    awful.key(
        {},
        "XF86AudioMute",
        function()
            awful.util.spawn("amixer -D pulse sset Master toggle", false)
        end,
        {description = "mute", group = "sound"}
    ),
    awful.key(
        {},
        "XF86AudioPlay",
        function()
            awful.util.spawn("playerctl play-pause", false)
        end
    ),
    awful.key(
        {},
        "XF86AudioNext",
        function()
            awful.util.spawn("playerctl next", false)
        end
    ),
    awful.key(
        {},
        "XF86AudioPrev",
        function()
            awful.util.spawn("playerctl previous", false)
        end
    ),
    -- Brightness
    awful.key(
        {},
        "XF86MonBrightnessUp",
        function()
            os.execute("xbacklight -inc 10")
        end,
        {description = "+10%", group = "hotkeys"}
    ),
    awful.key(
        {},
        "XF86MonBrightnessDown",
        function()
            os.execute("xbacklight -dec 10")
        end,
        {description = "-10%", group = "hotkeys"}
    ),
    awful.key({modkey}, "Left", awful.tag.viewprev, {description = "view previous", group = "tag"}),
    awful.key({modkey}, "Right", awful.tag.viewnext, {description = "view next", group = "tag"}),
    awful.key({modkey}, "Escape", awful.tag.history.restore, {description = "go back", group = "tag"}),
    awful.key(
        {modkey},
        "j",
        function()
            awful.client.focus.byidx(1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key(
        {modkey},
        "k",
        function()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key(
        {modkey},
        "w",
        function()
            mymainmenu:show()
        end,
        {description = "show main menu", group = "awesome"}
    ),
    -- Layout manipulation
    awful.key(
        {modkey, "Shift"},
        "j",
        function()
            awful.client.swap.byidx(1)
        end,
        {description = "swap with next client by index", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "k",
        function()
            awful.client.swap.byidx(-1)
        end,
        {description = "swap with previous client by index", group = "client"}
    ),
    awful.key(
        {modkey, "Control"},
        "j",
        function()
            awful.screen.focus_relative(1)
        end,
        {description = "focus the next screen", group = "screen"}
    ),
    awful.key(
        {modkey, "Control"},
        "k",
        function()
            awful.screen.focus_relative(-1)
        end,
        {description = "focus the previous screen", group = "screen"}
    ),
    awful.key({modkey}, "u", awful.client.urgent.jumpto, {description = "jump to urgent client", group = "client"}),
    awful.key(
        {modkey},
        "Tab",
        function()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}
    ),
    -- Standard program
    awful.key(
        {modkey, "Shift"},
        "Return",
        function()
            awful.spawn(terminal)
        end,
        {description = "open a terminal", group = "launcher"}
    ),
    awful.key(
        {modkey},
        "p",
        function()
            awful.spawn("rofi-pass")
        end,
        {description = "open passmenu", group = "launcher"}
    ),
    awful.key(
        {"Mod1"},
        "`",
        function()
            awful.spawn("find-cursor --size 150 --distance 40 --wait 1000  --line-width 5 --color '#E30037'")
        end,
        {description = "find cursor", group = "launcher"}
    ),
    awful.key(
        {},
        "Print",
        function()
            awful.spawn("flameshot gui")
        end,
        {description = "open flameshot", group = "launcher"}
    ),
    awful.key(
        {modkey},
        "v",
        function()
            awful.spawn('rofi -modi "clipboard:greenclip print" -show clipboard -run-command \'{cmd}\'')
        end,
        {description = "open greenclip", group = "launcher"}
    ),
    awful.key(
        {modkey},
        "/",
        function()
            awful.spawn("toggle-layout")
        end,
        {description = "toggle keyboard layout", group = "awesome"}
    ),
    awful.key(
        {modkey},
        "d",
        function()
            awful.spawn("rofi -show combi")
        end,
        {description = "open rofi", group = "launcher"}
    ),
    awful.key({modkey, "Control"}, "r", awesome.restart, {description = "reload awesome", group = "awesome"}),
    awful.key({modkey, "Shift"}, "q", awesome.quit, {description = "quit awesome", group = "awesome"}),
    awful.key(
        {modkey},
        "l",
        function()
            awful.tag.incmwfact(0.05)
        end,
        {description = "increase master width factor", group = "layout"}
    ),
    awful.key(
        {modkey},
        "h",
        function()
            awful.tag.incmwfact(-0.05)
        end,
        {description = "decrease master width factor", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "h",
        function()
            awful.tag.incnmaster(1, nil, true)
        end,
        {description = "increase the number of master clients", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "l",
        function()
            awful.tag.incnmaster(-1, nil, true)
        end,
        {description = "decrease the number of master clients", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
        "h",
        function()
            awful.tag.incncol(1, nil, true)
        end,
        {description = "increase the number of columns", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
        "l",
        function()
            awful.tag.incncol(-1, nil, true)
        end,
        {description = "decrease the number of columns", group = "layout"}
    ),
    awful.key(
        {modkey},
        "space",
        function()
            awful.layout.inc(1)
        end,
        {description = "select next", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "space",
        function()
            awful.layout.inc(-1)
        end,
        {description = "select previous", group = "layout"}
    ),
    -- awful.key({ modkey, "Control" }, "n",
    --           function ()
    --               local c = awful.client.restore()
    --               -- Focus restored client
    --               if c then
    --                 c:emit_signal(
    --                     "request::activate", "key.unminimize", {raise = true}
    --                 )
    --               end
    --           end,
    --           {description = "restore minimized", group = "client"}),

    -- Prompt
    -- awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
    --           {description = "run prompt", group = "launcher"}),

    -- awful.key(
    --     {modkey},
    --     "x",
    --     function()
    --         awful.prompt.run {
    --             prompt = "Run Lua code: ",
    --             textbox = awful.screen.focused().mypromptbox.widget,
    --             exe_callback = awful.util.eval,
    --             history_path = awful.util.get_cache_dir() .. "/history_eval"
    --         }
    --     end,
    --     {description = "lua execute prompt", group = "awesome"}
    -- ),
    -- Menubar
    -- awful.key(
    --     {modkey},
    --     "p",
    --     function()
    --         menubar.show()
    --     end,
    --     {description = "show the menubar", group = "launcher"}
    -- ),
    awful.key(
        {modkey},
        "c",
        function()
            bring_or_swap(4)
        end,
        {description = "view tag terminal", group = "tag"}
    ),
    awful.key(
        {modkey},
        "m",
        function()
            bring_or_swap(5)
        end,
        {description = "view tag Emacs", group = "tag"}
    ),
    awful.key(
        {modkey},
        "n",
        function()
            naughty.destroy_all_notifications(nil, naughty.notificationClosedReason.dismissedByUser)
        end,
        {description = "Destroy notifications", group = "client"}
    )
)

function get_tag_screen(target_tag)
    for s in screen do
        for _, t in pairs(s.tags) do
            if t.name == target_tag.name then
                return s
            end
        end
    end
end

local prev_screen = 1
function set_prev_screen(s)
    prev_screen = s
    -- naughty.notify({preset=naughty.config.presets.normal, title=tostring(s.index), text="THE SAME"})
end

local prev_tag = nil
function set_prev_tag(t)
    if (t ~= prev_tag) then
        prev_tag = t
    end
end

function bring_or_swap(i)
    local target_tag = getTag(i)
    local target_screen = get_tag_screen(target_tag)
    local current_screen = awful.screen.focused()
    local current_tag = current_screen.selected_tag
    local one_screen = screen.count() == 1
    local debug = 0

    if current_tag and target_tag == current_tag then
        if not one_screen and prev_screen and prev_screen.index ~= current_screen.index then -- Just go to other screen
            debug = 1
            awful.screen.focus(prev_screen)
            -- awful.screen.focus_relative(1)
            set_prev_screen(current_screen)
        else -- Bring back
            debug = 2
            sharedtags.viewonly(prev_tag)
            wallpapers.setWallpaperFor(prev_tag, current_screen, current_activity)
            set_prev_tag(current_tag)
        end
    elseif target_tag and target_screen and not one_screen then -- Go to the target screen and tag
        debug = 3
        awful.screen.focus(target_screen)
        set_prev_screen(current_screen)
        set_prev_tag(current_tag)
        -- awful.screen.focus_relative(1)
        sharedtags.viewonly(target_tag)
        wallpapers.setWallpaperFor(target_tag, target_screen, current_activity)
    elseif one_screen then -- Go to target tag
        debug = 4
        awful.screen.focus(target_screen)
        set_prev_tag(current_tag)
        sharedtags.viewonly(target_tag)
        wallpapers.setWallpaperFor(target_tag, target_screen, current_activity)
    end

    local text =
        "target_screen: " ..
        tostring(target_screen.index) ..
            ", target_tag: " ..
                target_tag.name ..
                    ", current_screen: " ..
                        tostring(current_screen.index) ..
                            ", current_tag: " ..
                                (current_tag and current_tag.name or "nil") ..
                                    ", prev_tag: " ..
                                        (prev_tag and prev_tag.name or "nil") .. ", debug: " .. tostring(debug)
    -- naughty.notify({preset = naughty.config.presets.normal, title = "BRING", text = text})
end

clientkeys =
    gears.table.join(
    awful.key(
        {},
        "F11",
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "c",
        function(c)
            c:kill()
        end,
        {description = "close", group = "client"}
    ),
    awful.key({modkey}, "f", awful.client.floating.toggle, {description = "toggle floating", group = "client"}),
    awful.key(
        {modkey},
        "Return",
        function(c)
            c:swap(awful.client.getmaster())
        end,
        {description = "move to master", group = "client"}
    ),
    awful.key(
        {modkey},
        "o",
        function(c)
            local other_screen = awful.screen.focused().index == 1 and 2 or 1
            local target_tag = c.first_tag
            sharedtags.movetag(target_tag, screen[other_screen])
            awful.screen.focus(other_screen)
            sharedtags.viewonly(target_tag)
        end,
        {description = "move to screen", group = "client"}
    ),
    awful.key(
        {modkey},
        "t",
        function(c)
            c.ontop = not c.ontop
        end,
        {description = "toggle keep on top", group = "client"}
    ),
    awful.key(
        {modkey},
        "s",
        function(c)
            c.sticky = not c.sticky
        end,
        {description = "toggle sticky", group = "client"}
    )
    -- awful.key({ modkey,           }, "n",
    --     function (c)
    --         -- The client currently has the input focus, so it cannot be
    --         -- minimized, since minimized clients can't have the focus.
    --         c.minimized = true
    --     end ,
    --     {description = "minimize", group = "client"}),
    -- awful.key({ modkey,           }, "m",
    --     function (c)
    --         c.maximized = not c.maximized
    --         c:raise()
    --     end ,
    --     {description = "(un)maximize", group = "client"}),
    -- awful.key(
    --     {modkey, "Shift"},
    --     "m",
    --     function(c)
    --         c.maximized_horizontal = not c.maximized_horizontal
    --         c:raise()
    --     end,
    --     {description = "(un)maximize horizontally", group = "client"}
    -- ),
    -- awful.key(
    --     {modkey, "Control"},
    --     "m",
    --     function(c)
    --         c.maximized_vertical = not c.maximized_vertical
    --         c:raise()
    --     end,
    --     {description = "(un)maximize vertically", group = "client"}
    -- )
)

for i = 1, 4 do
    globalkeys =
        gears.table.join(
        globalkeys,
        -- View tag only.
        awful.key(
            {},
            "F" .. i,
            function()
                bring_or_swap(i)
            end,
            {description = "view tag #" .. i, group = "tag"}
        )
    )
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys =
        gears.table.join(
        globalkeys,
        -- View tag only.
        awful.key(
            {modkey},
            "#" .. i + 9,
            function()
                -- local screen = awful.screen.focused()
                -- local tag = screen.tags[i]
                -- if tag then
                --     sharedtags.viewonly(tag, screen)
                -- end
                bring_or_swap(i)
            end,
            {description = "view tag #" .. i, group = "tag"}
        ),
        -- Toggle tag display.
        -- awful.key(
        --     {modkey, "Control"},
        --     "#" .. i + 9,
        --     function()
        --         local screen = awful.screen.focused()
        --         local tag = screen.tags[i]
        --         if tag then
        --             sharedtags.viewtoggle(tag, screen)
        --         end
        --     end,
        --     {description = "toggle tag #" .. i, group = "tag"}
        -- ),
        -- Move client to tag.
        awful.key(
            {modkey, "Shift"},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = getTag(i)
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end,
            {description = "move focused client to tag #" .. i, group = "tag"}
        )
        -- Toggle tag on focused client.
        -- awful.key(
        --     {modkey, "Control", "Shift"},
        --     "#" .. i + 9,
        --     function()
        --         if client.focus then
        --             local tag = tags[i]
        --             if tag then
        --                 client.focus:toggle_tag(tag)
        --             end
        --         end
        --     end,
        --     {description = "toggle focused client on tag #" .. i, group = "tag"}
        -- )
    )
end

clientbuttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
        end
    ),
    awful.button(
        {modkey},
        1,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
            awful.mouse.client.move(c)
        end
    ),
    awful.button(
        {modkey},
        3,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
            awful.mouse.client.resize(c)
        end
    )
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    {
        rule = {
            class = "jetbrains-.*",
            instance = "sun-awt-X11-XWindowPeer",
            name = "win.*"
        },
        properties = {
            floating = true,
            focus = true,
            focusable = false,
            ontop = true,
            placement = awful.placement.restore,
            buttons = {}
        }
    },
    {
        rule = {},
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = awful.client.focus.filter,
            raise = true,
            keys = clientkeys,
            buttons = clientbuttons,
            screen = awful.screen.preferred,
            placement = awful.placement.no_overlap + awful.placement.no_offscreen
        }
    },
    {
        rule_any = {class = {"Polybar"}},
        properties = {focusable = false}
    },
    -- Floating clients.
    {
        rule_any = {
            instance = {
                "DTA", -- Firefox addon DownThemAll.
                "copyq", -- Includes session name in class.
                "pinentry",
                "SimpleScreenRecorder"
            },
            class = {
                "zoom",
                "Arandr",
                "Blueman-manager",
                "Gpick",
                "Kruler",
                "MessageWin", -- kalarm.
                "Sxiv",
                "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
                "Wpa_gui",
                "veromix",
                "xtightvncviewer",
                "SimpleScreenRecorder"
            },
            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name = {
                "Event Tester" -- xev.
            },
            role = {
                "AlarmWindow", -- Thunderbird's calendar.
                "ConfigManager", -- Thunderbird's about:config.
                -- "pop-up", -- e.g. Google Chrome's (detached) Developer Tools.
                "GtkFileChooserDialog"
            }
        },
        properties = {floating = true}
    },
    -- Add titlebars to normal clients and dialogs
    {
        rule_any = {
            type = {"normal", "dialog"}
        },
        properties = {titlebars_enabled = true}
    },
    -- Set Firefox to always map on the tag named "2" on screen 1.
    {
        rule_any = {
            class = {"Emacs", "Astroid"}
        },
        properties = {tag = getTag(5)}
    }
    -- {
    --     rule = {class = "Google-chrome", role = "browser"},
    --     properties = {tag = tags[2]}
    -- },
    -- {
    --     rule = {class = "Google-chrome", role = "browser", name = "Work"},
    --     properties = {tag = tags[1]}
    -- },
    -- {
    --     rule = {class = "Google-chrome", role = "browser", name = "work"},
    --     properties = {tag = tags[1]}
    -- },
    -- {
    --     rule = {class = "Alacritty"},
    --     properties = {tag = tags[4]}
    -- },
    -- {
    --     rule_any = {
    --         class = {"TelegramDesktop", "discord", "qtwaw"}
    --     },
    --     properties = {tag = tags[3]}
    -- }
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal(
    "manage",
    function(c)
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- if not awesome.startup then awful.client.setslave(c) end

        c.shape = function(cr, w, h)
            gears.shape.rounded_rect(cr, w, h, 8)
        end

        if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
            -- Prevent clients from being unreachable after screen count changes.
            awful.placement.no_offscreen(c)
        end

        awful.titlebar.hide(c)
        c.maximized = false
    end
)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal(
    "request::titlebars",
    function(c)
        -- buttons for the titlebar
        local buttons =
            gears.table.join(
            awful.button(
                {},
                1,
                function()
                    c:emit_signal("request::activate", "titlebar", {raise = true})
                    awful.mouse.client.move(c)
                end
            ),
            awful.button(
                {},
                3,
                function()
                    c:emit_signal("request::activate", "titlebar", {raise = true})
                    awful.mouse.client.resize(c)
                end
            )
        )

        awful.titlebar(c):setup {
            {
                -- Left
                awful.titlebar.widget.iconwidget(c),
                buttons = buttons,
                layout = wibox.layout.fixed.horizontal
            },
            {
                -- Middle
                {
                    -- Title
                    align = "center",
                    widget = awful.titlebar.widget.titlewidget(c)
                },
                buttons = buttons,
                layout = wibox.layout.flex.horizontal
            },
            {
                -- Right
                awful.titlebar.widget.floatingbutton(c),
                awful.titlebar.widget.maximizedbutton(c),
                awful.titlebar.widget.stickybutton(c),
                awful.titlebar.widget.ontopbutton(c),
                awful.titlebar.widget.closebutton(c),
                layout = wibox.layout.fixed.horizontal()
            },
            layout = wibox.layout.align.horizontal
        }
    end
)

-- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal(
--     "mouse::enter",
--     function(c)
--         c:emit_signal("request::activate", "mouse_enter", {raise = false})
--     end
-- )

function draw_left_bar(c)
    local tag = c.first_tag
    local max = tag and tag.layout.name == "max"
    local only_one = #c.screen.tiled_clients == 1 -- use tiled_clients so that other floating windows don't affect the count

    if (max or only_one) and not c.floating or c.maximized then
        awful.titlebar(
            c,
            {
                position = "left",
                size = 0,
                bg_normal = beautiful.border_normal,
                bg_focus = beautiful.border_focus
            }
        )
    else
        awful.titlebar(
            c,
            {
                position = "left",
                size = beautiful.bar_width,
                bg_normal = beautiful.border_normal,
                bg_focus = beautiful.border_focus
            }
        )
    end
end

client.connect_signal(
    "focus",
    function(c)
        draw_left_bar(c)
        -- c.opacity = 1
    end
)

client.connect_signal(
    "unfocus",
    function(c)
        draw_left_bar(c)
        -- c.opacity = 0.7
    end
)

client.connect_signal(
    "property::size",
    function(c)
        draw_left_bar(c)
    end
)

do
    local autostarts = {
        "cbatticon",
        "pasystray",
        -- "flashfocus",
        "mullvad-vpn",
        "picom",
        "keyboard-configure"
    }

    for _, i in pairs(autostarts) do
        awful.spawn.easy_async_with_shell(
            "ps -C " .. i .. " |wc -l",
            function(stdout, stderr, reason, exit_code)
                if (tonumber(stdout) or 0) < 2 then
                    awful.spawn(i)
                end
            end
        )
    end
end

wallpapers.start(
    {
        path = "~/Pictures/Wallpaper",
        show_notify = false,
        timeout = 0,
        change_on_click = true
    },
    tags
)
