-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, 'luarocks.loader')

-- Standard awesome library
local gears = require 'gears'
local awful = require 'awful'
require 'awful.autofocus'
-- Widget and layout library
local wibox = require 'wibox'
-- Theme handling library
local beautiful = require 'beautiful'
-- Notification library
local naughty = require 'naughty'
local menubar = require 'menubar'
local hotkeys_popup = require 'awful.hotkeys_popup'
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
-- require('awful.hotkeys_popup.keys')
local theme_assets = require 'beautiful.theme_assets'
local xresources = require 'beautiful.xresources'
local dpi = xresources.apply_dpi

-- Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify {
        preset = naughty.config.presets.critical,
        title = 'Oops, there were errors during startup!',
        text = awesome.startup_errors
    }
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal(
        'debug::error',
        function(err)
            -- Make sure we don't go into an endless error loop
            if in_error then
                return
            end
            in_error = true

            naughty.notify(
                {
                    preset = naughty.config.presets.critical,
                    title = 'Oops, an error happened!',
                    text = tostring(err)
                }
            )
            in_error = false
        end
    )
end

-- Options
menubar.cache_entries = true
menubar.show_categories = false

-- Variable definitions

local color_ys_w = 'f9f8f4' -- Floral White, R=249, G=248, B=244
local color_ys_r = 'e32791' -- Deep Cerise, R=227, G=39, B=145
local color_ys_g = '488432' -- La Palma, R=72, G=132, B=50
local color_ys_y = 'a25d0e' -- Golden Brown, R=162, G=93, B=14
local color_ys_b = '2c65b5' -- Cerulean Blue, R=44, G=101, B=181
local color_ys_m = 'b062a7' -- Violet Blue, R=176, G=98, B=167
local color_ys_c = '27bbbe' -- Light Sea Green, R=39, G=187, B=190
local color_ys_k = '999999' -- Grey 60%, R=153, G=153, B=153
local color_qi_w = '1e1e1e' -- Grey 10%, R=30, G=30, B=30
local color_qi_r = 'e32791' -- Deep Cerise, R=227, G=39, B=145
local color_qi_g = '30c798' -- Shamrock, R=48, G=199, B=152
local color_qi_y = 'e3c472' -- Chenin, R=227, G=196, B=114
local color_qi_b = '6796e6' -- Cornflower Blue, R=103, G=150, B=230
local color_qi_m = 'e59fdf' -- Plum, R=229, G=159, B=223
local color_qi_c = '81d8d0' -- Riptide, R=129, G=216, B=208
local color_qi_k = '969696' -- Grey 60%, R=150, G=150, B=150
local color_qi_b_w = '515151' -- Grey 30%, R=81, G=81, B=81
local color_qi_b_r = 'e466ad' -- Hot Pink, R=228, G=102, B=173
local color_qi_b_g = '6cd1b2' -- Medium Aquamarine, R=108, G=209, B=178
local color_qi_b_y = 'e4cf98' -- Double Colonial White, R=228, G=207, B=152
local color_qi_b_b = '91b0e6' -- Jordy Blue, R=145, G=181, B=230
local color_qi_b_m = 'e5b6e1' -- French Lilac, R=229, G=182, B=225
local color_qi_b_c = 'a2dcd7' -- Sinbad, R=162, G=220, B=215
local color_qi_b_k = 'e5e6e6' -- Grey 90%, R=229, G=230, B=230
local color_ys_b_w = 'b8b8b8' -- Grey 70%, R=184, G=184, B=184
local color_ys_b_r = '9f1b66' -- Jazzberry Jam, R=159, G=27, B=102
local color_ys_b_g = '325d23' -- Parsley, R=50, G=93, B=35
local color_ys_b_y = '71410a' -- Raw Umber, R=113, G=65, B=10
local color_ys_b_b = '1f477f' -- Bahama Blue, R=31, G=71, B=127
local color_ys_b_m = '7b4474' -- Eminence, R=123, G=68, B=116
local color_ys_b_c = '1b8486' -- Atoll, R=27, G=132, B=134
local color_ys_b_k = '424242' -- Grey 20%, R=66, G=66, B=66
local color_qi_w_80 = '333333' -- Grey 20%, R=51, G=51, B=51
local color_ys_w_80 = 'edece8' -- Grey 90%, R=237, G=236, B=232
local color_cursor = '20bbfc' -- Deep Sky Blue, R=32, G=187, B=252
local assets = gears.filesystem.get_configuration_dir() .. 'assets/'

-- 0xb2d5751b41ed4edc // airolo
-- 0xe523ec7d7f8f49db // aberystwyth
-- ISO 3166 Country codes, US=840, DE=276
local has_ten_keys = false
local kbd_layout = 276
local has_volume_keys = true
local has_multimedia_keys = true
local is_macintosh = true

-- Theme

local theme = {}

theme.font = 'IBM Plex Sans 10'

theme.bg_normal = '#' .. color_qi_w
theme.bg_focus = '#005577'
theme.bg_urgent = '#' .. color_qi_r
theme.bg_minimize = '#444444'
theme.bg_systray = theme.bg_normal

theme.fg_normal = '#' .. color_ys_b_w
theme.fg_focus = '#' .. color_qi_b_k
theme.fg_urgent = '#' .. color_qi_b_k
theme.fg_minimize = '#ffffff'

theme.useless_gap = dpi(0)
theme.border_width = dpi(2)
theme.border_normal = '#' .. color_ys_b_k
theme.border_focus = '#' .. color_cursor
theme.border_marked = '#' .. color_qi_y

-- There are other variable sets
-- overriding the one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

-- Generate taglist squares:
local taglist_square_size = dpi(4)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(taglist_square_size, theme.fg_normal)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(taglist_square_size, theme.fg_normal)

-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = assets .. 'submenu.png'
theme.menu_height = dpi(15)
theme.menu_width = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = assets .. 'titlebar/close_normal.png'
theme.titlebar_close_button_focus = assets .. 'titlebar/close_focus.png'

theme.titlebar_minimize_button_normal = assets .. 'titlebar/minimize_normal.png'
theme.titlebar_minimize_button_focus = assets .. 'titlebar/minimize_focus.png'

theme.titlebar_ontop_button_normal_inactive = assets .. 'titlebar/ontop_normal_inactive.png'
theme.titlebar_ontop_button_focus_inactive = assets .. 'titlebar/ontop_focus_inactive.png'
theme.titlebar_ontop_button_normal_active = assets .. 'titlebar/ontop_normal_active.png'
theme.titlebar_ontop_button_focus_active = assets .. 'titlebar/ontop_focus_active.png'

theme.titlebar_sticky_button_normal_inactive = assets .. 'titlebar/sticky_normal_inactive.png'
theme.titlebar_sticky_button_focus_inactive = assets .. 'titlebar/sticky_focus_inactive.png'
theme.titlebar_sticky_button_normal_active = assets .. 'titlebar/sticky_normal_active.png'
theme.titlebar_sticky_button_focus_active = assets .. 'titlebar/sticky_focus_active.png'

theme.titlebar_floating_button_normal_inactive = assets .. 'titlebar/floating_normal_inactive.png'
theme.titlebar_floating_button_focus_inactive = assets .. 'titlebar/floating_focus_inactive.png'
theme.titlebar_floating_button_normal_active = assets .. 'titlebar/floating_normal_active.png'
theme.titlebar_floating_button_focus_active = assets .. 'titlebar/floating_focus_active.png'

theme.titlebar_maximized_button_normal_inactive = assets .. 'titlebar/maximized_normal_inactive.png'
theme.titlebar_maximized_button_focus_inactive = assets .. 'titlebar/maximized_focus_inactive.png'
theme.titlebar_maximized_button_normal_active = assets .. 'titlebar/maximized_normal_active.png'
theme.titlebar_maximized_button_focus_active = assets .. 'titlebar/maximized_focus_active.png'

-- XXX waiting for random function in awesome 4.4
theme.wallpaper = gears.filesystem.get_xdg_cache_home() .. 'Tapet/2020-02-14_17-05-44_883_1920x1200.png'

-- You can use your own layout icons like this:
theme.layout_fairh = assets .. 'layouts/fairh.png'
theme.layout_fairv = assets .. 'layouts/fairv.png'
theme.layout_floating = assets .. 'layouts/floating.png'
theme.layout_magnifier = assets .. 'layouts/magnifier.png'
theme.layout_max = assets .. 'layouts/max.png'
theme.layout_fullscreen = assets .. 'layouts/fullscreen.png'
theme.layout_tilebottom = assets .. 'layouts/tilebottom.png'
theme.layout_tileleft = assets .. 'layouts/tileleft.png'
theme.layout_tile = assets .. 'layouts/tile.png'
theme.layout_tiletop = assets .. 'layouts/tiletop.png'
theme.layout_spiral = assets .. 'layouts/spiral.png'
theme.layout_dwindle = assets .. 'layouts/dwindle.png'
theme.layout_cornernw = assets .. 'layouts/cornernw.png'
theme.layout_cornerne = assets .. 'layouts/cornerne.png'
theme.layout_cornersw = assets .. 'layouts/cornersw.png'
theme.layout_cornerse = assets .. 'layouts/cornerse.png'

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(theme.menu_height, theme.bg_focus, theme.fg_focus)

theme.tasklist_icon_size = 12 -- XXX waiting for awesome 4.4
theme.tasklist_disable_icon = true
theme.wibar_height = 20

-- Themes define colours, icons, font and wallpapers.
beautiful.init(theme)

-- This is used later as the default terminal and editor to run.
terminal = os.getenv 'TERMINAL' or 'alacritty'
editor = os.getenv 'EDITOR' or 'vim'
editor_cmd = terminal .. ' -e ' .. editor

local function connect_bluetooth()
    awful.spawn 'bluetoothctl connect 88:C6:26:F4:8A:90'
end

local function disconnect_bluetooth()
    awful.spawn 'bluetoothctl disconnect 88:C6:26:F4:8A:90'
end

local function strawberry_next()
    -- IDEA: check whether mpv or strawberry is running
    awful.spawn 'strawberry -f'
end

local function strawberry_prev()
    awful.spawn 'strawberry --restart-or-previous'
end

local function strawberry_playpause()
    awful.spawn 'strawberry -t'
end

local function strawberry_fwd()
    awful.spawn 'strawberry --seek-by 10'
end

local function strawberry_rew()
    awful.spawn 'strawberry --seek-by -10'
end

modkey = 'Mod4'

local key_above_tab = 'acute'
if kbd_layout == 276 then
    key_above_tab = 'asciicircum'
end

local printkey = 'Print'
if is_macintosh then
    printkey = 'XF86LaunchA'
end

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    -- awful.layout.suit.tile.left,
    -- awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.floating,
    awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}

-- {{{ Menu
-- Create a launcher widget and a main menu
local myawesomemenu = {
    {
        'hotkeys',
        function()
            hotkeys_popup.show_help(nil, awful.screen.focused())
        end
    },
    {'manual', terminal .. ' -e man awesome'},
    {'edit config', editor_cmd .. ' ' .. awesome.conffile}
}

local mymultimediamenu = {
    {'connect bluetooth device (Setúbal)', connect_bluetooth},
    {'disconnect bluetooth device (Setúbal)', disconnect_bluetooth}
}

local myexitmenu = {
    {'restart', awesome.restart},
    {
        'log out',
        function()
            awesome.quit()
        end,
        menubar.utils.lookup_icon('system-log-out')
    },
    {'suspend', 'systemctl suspend', menubar.utils.lookup_icon('system-suspend')},
    {'hibernate', 'systemctl hibernate', menubar.utils.lookup_icon('system-suspend-hibernate')},
    {'reboot', 'systemctl reboot', menubar.utils.lookup_icon('system-reboot')},
    {'shutdown', 'systemctl poweroff', menubar.utils.lookup_icon('system-shutdown')}
}

local mymainmenu =
    awful.menu(
    {
        items = {
            {'awesome', myawesomemenu, beautiful.awesome_icon},
            {'multimedia', mymultimediamenu},
            {'exit', myexitmenu, menubar.utils.lookup_icon('system-shutdown')},
            {'open terminal', terminal}
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
menubar.utils.terminal = terminal -- Set the terminal for applications that require it

-- Keyboard map indicator and switcher
-- mykeyboardlayout = awful.widget.keyboardlayout()

-- Wibar
mytextclock = wibox.widget.textclock('%a %d %b %T %Z', 1)

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
                c:emit_signal('request::activate', 'tasklist', {raise = true})
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

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == 'function' then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, false)
    end
end

local netdevice = ''
local ipwidget =
    awful.widget.watch(
    {'ip', 'route', 'get', '8.8.8.8'},
    7,
    function(widget, stdout, stderr, exitreason, exitcode)
        widget:set_text(stdout:match 'src ([%d.]*)')
        netdevice = stdout:match 'dev ([%S]*)'
    end,
    wibox.widget.textbox()
)

local wifiwidget =
    awful.widget.watch(
    {'iw', 'dev', 'wlp2s0', 'link'},
    11,
    function(widget, stdout, stderr, exitreason, exitcode)
        widget:set_text(
            (stdout:match 'SSID: ([^\n]*)' or 'N/A') ..
                '  ' .. (stdout:match 'Connected to %x%x:%x%x:%x%x:(%x%x:%x%x:%x%x)' or 'N/A')
        )
    end,
    wibox.widget.textbox()
)

local temperaturewidget =
    (function()
    local t = gears.timer {timeout = 17}
    local widget = wibox.widget.textbox()
    t:connect_signal(
        'timeout',
        function()
            t:stop()
            local f = io.open('/sys/class/thermal/thermal_zone1/temp', 'r')
            local temperature = f:read 'n'
            f:close()
            widget:set_text(temperature // 1000 .. ' °C')
            t:again()
        end
    )
    t:start()
    t:emit_signal('timeout')
    return widget
end)()

function fixed(bytes)
    local unit
    local result
    if bytes >= 1000000000 then
        result = bytes / 1000000000
        unit = 'GB'
    elseif bytes >= 1000000 then
        result = bytes / 1000000
        unit = 'MB'
    elseif bytes >= 1000 then
        result = bytes / 1000
        unit = 'kB'
    else
        unit = 'B'
        return string.format('%d %s', bytes, unit)
    end
    return string.format('%.1f %s', result, unit)
end

local netthroughwidget =
    (function()
    local t = gears.timer {timeout = 3}
    local widget = wibox.widget.textbox()
    local old_recv = 0
    local old_send = 0
    local last_time = os.time()
    local send_rate = 0
    local recv_rate = 0
    t:connect_signal(
        'timeout',
        function()
            t:stop()
            local f = io.open('/proc/net/dev', 'r')
            while true do
                local line = f:read 'l'
                if line == nil then
                    break
                end
                local name = string.match(line, '^[%s]?[%s]?[%s]?[%s]?([%w]+):')
                if name == netdevice then
                    local new_recv = tonumber(string.match(line, ':[%s]*([%d]+)'))
                    local new_send = tonumber(string.match(line, '([%d]+)%s+%d+%s+%d+%s+%d+%s+%d+%s+%d+%s+%d+%s+%d$'))
                    local this_time = os.time()
                    local timediff = os.difftime(this_time, last_time)
                    recv_rate = (new_recv - old_recv) // timediff
                    send_rate = (new_send - old_send) // timediff
                    last_time = this_time
                    old_send = new_send
                    old_recv = new_recv
                end
            end
            f:close()
            widget:set_text(fixed(recv_rate) .. '/s  ' .. fixed(send_rate) .. '/s')
            t:again()
        end
    )
    t:start()
    t:emit_signal('timeout')
    return widget
end)()

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal('property::geometry', set_wallpaper)

awful.screen.connect_for_each_screen(
    function(s)
        -- Wallpaper
        set_wallpaper(s)

        -- Each screen has its own tag table.
        awful.tag({'1', '2', '3', '4', '5', '6', '7', '8', '9'}, s, awful.layout.layouts[1])

        -- Create a promptbox for each screen
        s.mypromptbox = awful.widget.prompt()
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
            filter = awful.widget.taglist.filter.noempty,
            buttons = taglist_buttons
        }

        -- Create a tasklist widget
        s.mytasklist =
            awful.widget.tasklist {
            screen = s,
            filter = awful.widget.tasklist.filter.currenttags,
            buttons = tasklist_buttons
        }

        -- Create the wibox
        s.mywibox = awful.wibar({position = 'top', screen = s})

        -- Add widgets to the wibox
        s.mywibox:setup {
            layout = wibox.layout.align.horizontal,
            {
                -- Left widgets
                layout = wibox.layout.fixed.horizontal,
                mylauncher,
                s.mytaglist,
                s.mylayoutbox,
                s.mypromptbox
            },
            s.mytasklist, -- Middle widget
            {
                spacing = 10,
                -- Right widgets
                layout = wibox.layout.fixed.horizontal,
                -- mykeyboardlayout,
                -- wibox.widget.systray(),
                netthroughwidget,
                temperaturewidget,
                wifiwidget,
                ipwidget,
                mytextclock
            }
        }
    end
)

-- Mouse bindings
root.buttons(
    gears.table.join(
        awful.button(
            {},
            3,
            function()
                mymainmenu:toggle()
            end
        )
        -- awful.button({ }, 4, awful.tag.viewnext),
        -- awful.button({ }, 5, awful.tag.viewprev)
    )
)

-- Key bindings
globalkeys =
    gears.table.join(
    awful.key({modkey}, 'F1', hotkeys_popup.show_help, {description = 'show help', group = 'awesome'}),
    awful.key({modkey}, 'Left', awful.tag.viewprev, {description = 'view previous', group = 'tag'}),
    awful.key({modkey}, 'Right', awful.tag.viewnext, {description = 'view next', group = 'tag'}),
    awful.key({modkey}, key_above_tab, awful.tag.history.restore, {description = 'go back', group = 'tag'}),
    awful.key(
        {modkey},
        'j',
        function()
            awful.client.focus.byidx(1)
        end,
        {description = 'focus next by index', group = 'client'}
    ),
    awful.key(
        {modkey},
        'k',
        function()
            awful.client.focus.byidx(-1)
        end,
        {description = 'focus previous by index', group = 'client'}
    ),
    awful.key(
        {modkey},
        'w',
        function()
            mymainmenu:show()
        end,
        {description = 'show main menu', group = 'awesome'}
    ),
    -- Layout manipulation
    awful.key(
        {modkey, 'Shift'},
        'j',
        function()
            awful.client.swap.byidx(1)
        end,
        {description = 'swap with next client by index', group = 'client'}
    ),
    awful.key(
        {modkey, 'Shift'},
        'k',
        function()
            awful.client.swap.byidx(-1)
        end,
        {description = 'swap with previous client by index', group = 'client'}
    ),
    awful.key(
        {modkey, 'Control'},
        'j',
        function()
            awful.screen.focus_relative(1)
        end,
        {description = 'focus the next screen', group = 'screen'}
    ),
    awful.key(
        {modkey, 'Control'},
        'k',
        function()
            awful.screen.focus_relative(-1)
        end,
        {description = 'focus the previous screen', group = 'screen'}
    ),
    awful.key({modkey}, 'u', awful.client.urgent.jumpto, {description = 'jump to urgent client', group = 'client'}),
    awful.key(
        {modkey},
        'Tab',
        function()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = 'go back', group = 'client'}
    ),
    -- Standard program
    awful.key(
        {modkey},
        'Return',
        function()
            awful.spawn(terminal)
        end,
        {description = 'open a terminal', group = 'launcher'}
    ),
    awful.key({modkey, 'Control'}, 'r', awesome.restart, {description = 'reload awesome', group = 'awesome'}),
    awful.key({modkey, 'Shift'}, 'q', awesome.quit, {description = 'quit awesome', group = 'awesome'}),
    awful.key(
        {modkey},
        'l',
        function()
            awful.tag.incmwfact(0.05)
        end,
        {description = 'increase master width factor', group = 'layout'}
    ),
    awful.key(
        {modkey},
        'h',
        function()
            awful.tag.incmwfact(-0.05)
        end,
        {description = 'decrease master width factor', group = 'layout'}
    ),
    awful.key(
        {modkey, 'Shift'},
        'h',
        function()
            awful.tag.incnmaster(1, nil, true)
        end,
        {description = 'increase the number of master clients', group = 'layout'}
    ),
    awful.key(
        {modkey, 'Shift'},
        'l',
        function()
            awful.tag.incnmaster(-1, nil, true)
        end,
        {description = 'decrease the number of master clients', group = 'layout'}
    ),
    awful.key(
        {modkey, 'Control'},
        'h',
        function()
            awful.tag.incncol(1, nil, true)
        end,
        {description = 'increase the number of columns', group = 'layout'}
    ),
    awful.key(
        {modkey, 'Control'},
        'l',
        function()
            awful.tag.incncol(-1, nil, true)
        end,
        {description = 'decrease the number of columns', group = 'layout'}
    ),
    awful.key(
        {modkey},
        'space',
        function()
            awful.layout.inc(1)
        end,
        {description = 'select next', group = 'layout'}
    ),
    awful.key(
        {modkey, 'Shift'},
        'space',
        function()
            awful.layout.inc(-1)
        end,
        {description = 'select previous', group = 'layout'}
    ),
    awful.key(
        {modkey, 'Control'},
        'n',
        function()
            local c = awful.client.restore()
            -- Focus restored client
            if c then
                c:emit_signal('request::activate', 'key.unminimize', {raise = true})
            end
        end,
        {description = 'restore minimized', group = 'client'}
    ),
    -- Prompt
    awful.key(
        {modkey},
        'r',
        function()
            awful.screen.focused().mypromptbox:run()
        end,
        {description = 'run prompt', group = 'launcher'}
    ),
    awful.key(
        {modkey},
        'x',
        function()
            awful.prompt.run {
                prompt = 'Run Lua code: ',
                textbox = awful.screen.focused().mypromptbox.widget,
                exe_callback = awful.util.eval,
                history_path = awful.util.get_cache_dir() .. '/history_eval'
            }
        end,
        {description = 'lua execute prompt', group = 'awesome'}
    ),
    -- Menubar
    awful.key(
        {modkey},
        'p',
        function()
            menubar.show()
        end,
        {description = 'show the menubar', group = 'launcher'}
    ),
    awful.key(
        {},
        printkey,
        function()
            awful.spawn {'flameshot', 'full', '-c', '-p', os.getenv('HOME') .. '/tmp'}
        end,
        {description = 'take a screenshot', group = 'awesome'}
    ),
    awful.key(
        {modkey},
        printkey,
        function()
            awful.spawn {'flameshot', 'gui', '-p', os.getenv('HOME') .. '/tmp'}
        end,
        {description = 'capture a portion of the screen', group = 'awesome'}
    ),
    awful.key(
        {modkey},
        'Escape',
        function()
            awful.spawn {'slock', 'ssh-add', '-D'}
        end,
        {description = 'lock screen and SSH', group = 'awesome'}
    )
)

clientkeys =
    gears.table.join(
    awful.key(
        {modkey},
        'f',
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = 'toggle fullscreen', group = 'client'}
    ),
    awful.key(
        {modkey, 'Shift'},
        'c',
        function(c)
            c:kill()
        end,
        {description = 'close', group = 'client'}
    ),
    awful.key(
        {modkey, 'Control'},
        'space',
        awful.client.floating.toggle,
        {description = 'toggle floating', group = 'client'}
    ),
    awful.key(
        {modkey, 'Control'},
        'Return',
        function(c)
            c:swap(awful.client.getmaster())
        end,
        {description = 'move to master', group = 'client'}
    ),
    awful.key(
        {modkey},
        'o',
        function(c)
            c:move_to_screen()
        end,
        {description = 'move to screen', group = 'client'}
    ),
    awful.key(
        {modkey},
        't',
        function(c)
            c.ontop = not c.ontop
        end,
        {description = 'toggle keep on top', group = 'client'}
    ),
    awful.key(
        {modkey},
        'n',
        function(c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end,
        {description = 'minimize', group = 'client'}
    ),
    awful.key(
        {modkey},
        'm',
        function(c)
            c.maximized = not c.maximized
            c:raise()
        end,
        {description = '(un)maximize', group = 'client'}
    ),
    awful.key(
        {modkey, 'Control'},
        'm',
        function(c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end,
        {description = '(un)maximize vertically', group = 'client'}
    ),
    awful.key(
        {modkey, 'Shift'},
        'm',
        function(c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end,
        {description = '(un)maximize horizontally', group = 'client'}
    )
)

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
            '#' .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    tag:view_only()
                end
            end,
            (function()
                if i == 1 then
                    return {description = '… view tag #1 …', group = 'tag'}
                else
                    return {}
                end
            end)()
        ),
        -- Toggle tag display.
        awful.key(
            {modkey, 'Control'},
            '#' .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
            end,
            (function()
                if i == 1 then
                    return {description = '… toggle tag #1 …', group = 'tag'}
                else
                    return {}
                end
            end)()
        ),
        -- Move client to tag.
        awful.key(
            {modkey, 'Shift'},
            '#' .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end,
            (function()
                if i == 1 then
                    return {description = '… move focused client to #1 …', group = 'tag'}
                else
                    return {}
                end
            end)()
        ),
        -- Toggle tag on focused client.
        awful.key(
            {modkey, 'Control', 'Shift'},
            '#' .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:toggle_tag(tag)
                    end
                end
            end,
            (function()
                if i == 1 then
                    return {description = '… toggle focused client on #1 …', group = 'tag'}
                else
                    return {}
                end
            end)()
        )
    )
end

globalkeys =
    gears.table.join(
    globalkeys,
    awful.key({modkey}, 'F9', connect_bluetooth, {description = 'connect bluetooth device (Setúbal)', group = '🎧'}),
    awful.key(
        {modkey, 'Shift'},
        'F9',
        disconnect_bluetooth,
        {description = 'disconnect bluetooth device (Setúbal)', group = '🎧'}
    )
)

if has_multimedia_keys then
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key({}, 'XF86AudioNext', strawberry_next, {description = '⏭️', group = '🍓'}),
        awful.key({}, 'XF86AudioPrev', strawberry_prev, {description = '⏮', group = '🍓'}),
        awful.key({}, 'XF86AudioPlay', strawberry_playpause, {description = '⏯', group = '🍓'}),
        awful.key({'Shift'}, 'XF86AudioNext', strawberry_fwd, {description = '⏩', group = '🍓'}),
        awful.key({'Shift'}, 'XF86AudioPrev', strawberry_rew, {description = '⏪', group = '🍓'})
    )
end

function increase_volume_curry()
    local latest_notification
    local function my_notify(text)
        if latest_notification then
            naughty.replace_text(latest_notification, text, '')
        else
            latest_notification =
                naughty.notify {
                title = text,
                destroy = function()
                    latest_notification = nil
                end
            }
        end
    end
    return function(percentage)
        if percentage == nil then
            awful.spawn.with_line_callback(
                {'pactl', 'set-sink-mute', '@DEFAULT_SINK@', 'false'},
                {
                    exit = my_notify '🔇 mute'
                }
            )
            return
        end
        local emoji = percentage > 0 and '🔊' or '🔉'
        awful.spawn.easy_async_with_shell(
            "pacmd list-sinks | grep -A 15 '* index' | awk '/volume: front/{gsub(\"%\",\"\",$5); print $5 }'",
            function(stdout, stderr, exitreason, exitcode)
                local volume = stdout
                awful.spawn.with_line_callback(
                    {'pactl', 'set-sink-mute', '@DEFAULT_SINK@', 'false'},
                    {
                        exit = function()
                            local new_volume = math.max(math.min(tonumber(volume) + percentage, 100), 0)
                            awful.spawn.with_line_callback(
                                {'pactl', 'set-sink-volume', '@DEFAULT_SINK@', new_volume .. '%'},
                                {
                                    exit = function()
                                        my_notify(emoji .. new_volume .. ' %')
                                    end
                                }
                            )
                        end
                    }
                )
            end
        )
    end
end

local increase_volume = increase_volume_curry()

if has_volume_keys then
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key(
            {},
            'XF86AudioRaiseVolume',
            function()
                increase_volume(5)
            end,
            {description = '🔊', group = '🎧'}
        ),
        awful.key(
            {},
            'XF86AudioLowerVolume',
            function()
                increase_volume(-5)
            end,
            {description = '🔉', group = '🎧'}
        ),
        awful.key(
            {},
            'XF86AudioMute',
            function()
                increase_volume()
            end,
            {description = '🔇', group = '🎧'}
        )
    )
elseif has_ten_keys then
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key(
            {modkey},
            'KP_Add',
            function()
                increase_volume(5)
            end,
            {description = '🔊', group = '🎧'}
        ),
        awful.key(
            {modkey},
            'KP_Subtract',
            function()
                increase_volume(-5)
            end,
            {description = '🔉', group = '🎧'}
        ),
        awful.key(
            {modkey},
            'KP_Insert',
            function()
                increase_volume()
            end,
            {description = '🔇', group = '🎧'}
        )
    )
end

local function increase_light_curry(brightness_file, max_brightness, emoji)
    local f = io.open(brightness_file, 'r')
    local brightness = f:read 'n'
    f:close()
    local latest_notification
    local function my_notify(text)
        if latest_notification then
            naughty.replace_text(latest_notification, text, '')
        else
            latest_notification =
                naughty.notify {
                title = text,
                destroy = function()
                    latest_notification = nil
                end
            }
        end
    end
    return function(percentage)
        brightness = math.min(math.max(brightness + percentage * max_brightness // 100, 0), max_brightness)
        local f = io.open(brightness_file, 'w+')
        f:write(brightness)
        f:close()
        my_notify(emoji .. ' ' .. brightness * 100 // max_brightness .. '%')
    end
end

if gears.filesystem.file_readable('/sys/class/leds/smc::kbd_backlight/max_brightness') then
    local f = io.open('/sys/class/leds/smc::kbd_backlight/max_brightness', 'r')
    local max_brightness = f:read 'n'
    f:close()
    local increase_keyboard_light =
        increase_light_curry('/sys/class/leds/smc::kbd_backlight/brightness', max_brightness, '⌨️')
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key(
            {},
            'XF86KbdBrightnessDown',
            function()
                increase_keyboard_light(-5)
            end,
            {description = '🔅', group = '⌨️'}
        ),
        awful.key(
            {},
            'XF86KbdBrightnessUp',
            function()
                increase_keyboard_light(5)
            end,
            {description = '🔆', group = '⌨️'}
        )
    )
end

if gears.filesystem.file_readable('/sys/class/backlight/intel_backlight/max_brightness') then
    local f = io.open('/sys/class/backlight/intel_backlight/max_brightness', 'r')
    local max_brightness = f:read 'n'
    f:close()
    local increase_keyboard_light =
        increase_light_curry('/sys/class/backlight/intel_backlight/brightness', max_brightness, '🖥️')
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key(
            {},
            'XF86MonBrightnessDown',
            function()
                increase_keyboard_light(-5)
            end,
            {description = '🔅', group = '🖥️'}
        ),
        awful.key(
            {},
            'XF86MonBrightnessUp',
            function()
                increase_keyboard_light(5)
            end,
            {description = '🔆', group = '🖥️'}
        )
    )
end

clientbuttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(c)
            c:emit_signal('request::activate', 'mouse_click', {raise = true})
        end
    ),
    awful.button(
        {modkey},
        1,
        function(c)
            c:emit_signal('request::activate', 'mouse_click', {raise = true})
            awful.mouse.client.move(c)
        end
    ),
    awful.button(
        {modkey},
        3,
        function(c)
            c:emit_signal('request::activate', 'mouse_click', {raise = true})
            awful.mouse.client.resize(c)
        end
    )
)

-- Set keys
root.keys(globalkeys)

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
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
    -- Floating clients.
    {
        rule_any = {
            instance = {
                'DTA', -- Firefox addon DownThemAll.
                'copyq', -- Includes session name in class.
                'pinentry'
            },
            class = {
                'Arandr',
                'Blueman-manager',
                'Gpick',
                'Kruler',
                'MessageWin', -- kalarm.
                'Sxiv',
                'Tor Browser', -- Needs a fixed window size to avoid fingerprinting by screen size.
                'Wpa_gui',
                'veromix',
                'xtightvncviewer'
            },
            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name = {
                'Event Tester' -- xev.
            },
            role = {
                'AlarmWindow', -- Thunderbird's calendar.
                'ConfigManager', -- Thunderbird's about:config.
                'pop-up' -- e.g. Google Chrome's (detached) Developer Tools.
            }
        },
        properties = {floating = true}
    },
    -- Add titlebars to normal clients and dialogs
    {
        rule_any = {
            type = {'normal', 'dialog'}
        },
        properties = {titlebars_enabled = false}
    },
    {
        rule = {class = 'Journalctl'},
        properties = {screen = 1, tag = '9'}
    },
    {
        rule = {class = 'strawberry'},
        properties = {screen = 1, tag = '4'}
    }
    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}

-- Signals
-- Signal function to execute when a new client appears.
client.connect_signal(
    'manage',
    function(c)
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- if not awesome.startup then awful.client.setslave(c) end

        if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
            -- Prevent clients from being unreachable after screen count changes.
            awful.placement.no_offscreen(c)
        end
    end
)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal(
    'request::titlebars',
    function(c)
        -- buttons for the titlebar
        local buttons =
            gears.table.join(
            awful.button(
                {},
                1,
                function()
                    c:emit_signal('request::activate', 'titlebar', {raise = true})
                    awful.mouse.client.move(c)
                end
            ),
            awful.button(
                {},
                3,
                function()
                    c:emit_signal('request::activate', 'titlebar', {raise = true})
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
                    align = 'center',
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
client.connect_signal(
    'mouse::enter',
    function(c)
        c:emit_signal('request::activate', 'mouse_enter', {raise = false})
    end
)

client.connect_signal(
    'focus',
    function(c)
        c.border_color = beautiful.border_focus
    end
)

client.connect_signal(
    'unfocus',
    function(c)
        c.border_color = beautiful.border_normal
    end
)
