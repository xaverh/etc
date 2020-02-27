local gears = require 'gears'
local awful = require 'awful'
require 'awful.autofocus'
local wibox = require 'wibox'
local beautiful = require 'beautiful'
local naughty = require 'naughty'
local menubar = require 'menubar'
local hotkeys_popup = require 'awful.hotkeys_popup'
local theme_assets = require 'beautiful.theme_assets'
local xresources = require 'beautiful.xresources'
local dpi = xresources.apply_dpi

menubar.cache_entries = true
menubar.show_categories = false

local colors = {
    qi = {
        '1e1e1e', -- Grey 10%
        'e32791', -- Deep Cerise
        '30c798', -- Shamrock
        'e3c472', -- Chenin
        '6796e6', -- Cornflower Blue
        'e59fdf', -- Plum
        '81d8d0', -- Riptide
        '999999', -- Grey 60%
        '515151', -- Grey 30%
        'e466ad', -- Hot Pink
        '6cd1b2', -- Medium Aquamarine
        'e4cf98', -- Double Colonial White
        '91b0e6', -- Jordy Blue
        'e5b6e1', -- French Lilac
        'a2dcd7', -- Sinbad
        'e5e6e6' -- Grey 90%
    },
    ys = {
        'f9f8f4', -- Floral White
        'e32791', -- Deep Cerise
        '488432', -- La Palma
        'a25d0e', -- Golden Brown
        '2c65b5', -- Cerulean Blue
        'b062a7', -- Violet Blue
        '27bbbe', -- Light Sea Green
        '999999', -- Grey 60%
        'b8b8b8', -- Grey 70%
        '9f1b66', -- Jazzberry Jam
        '325d23', -- Parsley
        '71410a', -- Raw Umber
        '1f477f', -- Bahama Blue
        '7b4474', -- Eminence
        '1b8486', -- Atoll
        '424242' -- Grey 20%
    },
    cursor = '20bbfc', -- Deep Sky Blue, R=32, G=187, B=252
    cursor_dark = '0f3a4b' -- Cyprus, R=15
}

local assets = gears.filesystem.get_configuration_dir() .. 'assets/'

local has_volume_keys
local has_multimedia_keys
local is_macintosh
local temperature_filename = '/sys/class/thermal/thermal_zone1/temp'
if os.getenv 'HOSTNAME' == 'aberystwyth' then
    has_volume_keys = true
    has_multimedia_keys = true
    is_macintosh = true
elseif os.getenv 'HOSTNAME' == 'airolo' then
    temperature_filename = '/sys/class/thermal/thermal_zone2/temp'
end

beautiful.init {
    font = '.SF Compact Display 11',
    hotkeys_font = '.SF Compact Display 11',
    hotkeys_description_font = '.SF Compact Display 11',
    bg_normal = '#' .. colors.qi[1],
    bg_focus = '#' .. colors.cursor_dark,
    bg_urgent = '#' .. colors.qi[2],
    bg_minimize = bg_normal,
    fg_normal = '#' .. colors.qi[16],
    fg_focus = fg_normal,
    fg_urgent = fg_normal,
    fg_minimize = '#' .. colors.qi[9],
    tasklist_fg_normal = '#' .. colors.qi[8],
    tasklist_fg_focus = '#' .. colors.qi[16],
    menubar_fg_focus = '#' .. colors.qi[16],
    useless_gap = dpi(0),
    border_width = dpi(2),
    border_normal = '#' .. colors.qi[9],
    border_focus = '#' .. colors.cursor,
    border_marked = '#' .. colors.qi[4],
    menu_submenu_icon = assets .. 'submenu.png',
    menu_height = dpi(20),
    menu_width = dpi(120),
    -- layout_txt_fairh = '[fh]',
    layout_txt_tile = '🐧',
    layout_txt_fairv = '🍱',
    layout_txt_floating = '🎏',
    layout_txt_magnifier = '🔍',
    layout_txt_max = '🌆',
    layout_txt_spiral = '🍤',
    --[[
    layout_txt_fullscreen = '[F]',
    layout_txt_tilebottom = '[b]',
    layout_txt_tileleft = '[l]',
    layout_txt_tiletop = '[tt]',
    layout_txt_dwindle = '[d]',
    layout_txt_cornernw = '[c]',
    layout_txt_cornerne = '[c]',
    layout_txt_cornersw = '[c]',
    layout_txt_cornerse = '[c]',
    --]]
    tasklist_icon_size = 12, -- XXX, doesn't work yet waiting for awesome 4.4
    tasklist_disable_icon = true,
    tasklist_align = 'center',
    wibar_height = dpi(20),
    -- XXX waiting for random function in awesome 4.4
    wallpaper = gears.filesystem.get_xdg_data_home() .. 'Tapet/1280x800/tapet_2020-02-26_20-01-21_973_1280x800.png'
}

beautiful.taglist_squares_sel = theme_assets.taglist_squares_sel(dpi(4), beautiful.fg_normal)
beautiful.taglist_squares_unsel = theme_assets.taglist_squares_unsel(dpi(4), beautiful.fg_normal)
beautiful.awesome_icon = theme_assets.awesome_icon(beautiful.menu_height, beautiful.bg_focus, beautiful.fg_focus)

local function connect_bluetooth()
    awful.spawn 'bluetoothctl connect 88:C6:26:F4:8A:90'
end

local function disconnect_bluetooth()
    awful.spawn 'bluetoothctl disconnect 88:C6:26:F4:8A:90'
end

local function strawberry_next()
    -- TODO: check whether mpv or strawberry is running
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

local modkey = 'Mod4'

local printkey = 'Print'
if is_macintosh then
    printkey = 'XF86LaunchA'
end

awful.layout.layouts = {
    awful.layout.suit.tile,
    -- awful.layout.suit.tile.left,
    -- awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    awful.layout.suit.floating,
    awful.layout.suit.magnifier
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}

local myawesomemenu = {
    {
        'hotkeys',
        function()
            hotkeys_popup.show_help(nil, awful.screen.focused())
        end
    },
    {'manual', os.getenv 'TERMINAL' .. ' -e man awesome'}
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
        menubar.utils.lookup_icon 'system-log-out'
    },
    {'suspend', 'systemctl suspend', menubar.utils.lookup_icon 'system-suspend'},
    {'hibernate', 'systemctl hibernate', menubar.utils.lookup_icon 'system-suspend-hibernate'},
    {'reboot', 'systemctl reboot', menubar.utils.lookup_icon 'system-reboot'},
    {'shutdown', 'systemctl poweroff', menubar.utils.lookup_icon 'system-shutdown'}
}

local mymainmenu =
    awful.menu(
    {
        items = {
            {'awesome', myawesomemenu, beautiful.awesome_icon},
            {'multimedia', mymultimediamenu},
            {'exit', myexitmenu, menubar.utils.lookup_icon 'system-shutdown'},
            {'open terminal', os.getenv 'TERMINAL'}
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
            awful.menu.client_list {theme = {width = 250}}
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
    1.9,
    function(widget, stdout, stderr, exitreason, exitcode)
        local ip = stdout:match 'src ([%d.]*)'
        if ip then
            widget:set_text(ip)
            netdevice = stdout:match 'dev ([%S]*)'
        end
    end,
    wibox.widget.textbox()
)

local wifiwidget
if gears.filesystem.file_executable '/usr/sbin/iw' or gears.filesystem.file_executable '/usr/bin/iw' then
    wifiwidget =
        (function()
        local t = gears.timer {timeout = 2.9}
        local widget = wibox.widget.textbox()
        t:connect_signal(
            'timeout',
            function()
                t:stop()
                awful.spawn.easy_async(
                    {'iw', 'dev', netdevice, 'link'},
                    function(stdout, stderr, exitreason, exitcode)
                        widget:set_text(
                            (stdout:match 'SSID: ([^\n]*)' or 'no') ..
                                ' ' .. (stdout:match 'Connected to %x%x:%x%x:%x%x:(%x%x:%x%x:%x%x)' or 'WiFi')
                        )
                        t:again()
                    end
                )
            end
        )
        t:start()
        t:emit_signal 'timeout'
        return widget
    end)()
end

local temperaturewidget =
    (function()
    local t = gears.timer {timeout = 3.1}
    local widget = wibox.widget.textbox()
    t:connect_signal(
        'timeout',
        function()
            t:stop()
            local f = io.open(temperature_filename, 'r')
            local temperature = f:read 'n'
            f:close()
            widget:set_text(temperature // 1000 .. '\u{2009}°C')
            t:again()
        end
    )
    t:start()
    t:emit_signal 'timeout'
    return widget
end)()

local batterywidget
if gears.filesystem.dir_readable('/sys/class/power_supply/BAT0/') then
    batterywidget =
        (function()
        local t = gears.timer {timeout = 2.3}
        local widget = wibox.widget.textbox()
        t:connect_signal(
            'timeout',
            function()
                t:stop()
                local f = io.open('/sys/class/power_supply/BAT0/status', 'r')
                local battery_status = f:read 'l'
                f:close()
                if battery_status == 'Full' or battery_status == 'Charged' then
                    widget:set_text('100\u{2009}%')
                else
                    local f = io.open('/sys/class/power_supply/BAT0/charge_now', 'r')
                    local charge_now = f:read 'n'
                    f:close()
                    f = io.open('/sys/class/power_supply/BAT0/charge_full', 'r')
                    local charge_full = f:read 'n'
                    f:close()
                    local capacity = charge_now * 100 // charge_full
                    local charging_symbol = battery_status == 'Charging' and '+' or '-'
                    widget:set_text(capacity .. '\u{2009}%\u{2009}' .. charging_symbol)
                end
                t:again()
            end
        )
        t:start()
        t:emit_signal 'timeout'
        return widget
    end)()
end

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

local memorywidget =
    (function()
    local t = gears.timer {timeout = 4.1}
    local widget = wibox.widget.textbox()
    t:connect_signal(
        'timeout',
        function()
            t:stop()
            local mem = {}
            for line in io.lines '/proc/meminfo' do
                for k, v in string.gmatch(line, '([%a]+):[%s]+([%d]+).+') do
                    if k == 'MemTotal' then
                        mem.total = v * 1024
                    elseif k == 'MemFree' then
                        mem.free = v * 1024
                    elseif k == 'Shmem' then
                        mem.shmem = v * 1024
                    elseif k == 'Buffers' then
                        mem.buffers = v * 1024
                    elseif k == 'Cached' then
                        mem.cached = v * 1024
                    elseif k == 'SReclaimable' then
                        mem.sreclaimable = v * 1024
                    end
                end
            end
            -- https://github.com/KittyKatt/screenFetch/issues/386#issuecomment-249312716
            mem.used = mem.total + mem.shmem - mem.free - mem.buffers - mem.cached - mem.sreclaimable
            widget:set_text('  ' .. fixed(mem.used))
            t:again()
        end
    )
    t:start()
    t:emit_signal 'timeout'
    return widget
end)()

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
    t:emit_signal 'timeout'
    return widget
end)()

local function update_txt_layoutbox(s)
    -- Writes a string representation of the current layout in a textbox widget
    local txt_l = beautiful['layout_txt_' .. awful.layout.getname(awful.layout.get(s))] or ''
    s.mylayoutbox:set_text(txt_l)
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal('property::geometry', set_wallpaper)

awful.screen.connect_for_each_screen(
    function(s)
        -- Wallpaper
        set_wallpaper(s)

        if s.index == 1 then
            awful.tag({'🏄🏾‍♀️', '☕', '🏝️', '🍓', '🥑', '🦄', '🎰', '🎱', '🗞️'}, s, awful.layout.layouts[1])
        else
            awful.tag({'🍿', 'ß', '´'}, s, awful.layout.layouts[1])
        end

        -- Create a promptbox for each screen
        s.mypromptbox = awful.widget.prompt()
        -- Textual layoutbox
        s.mylayoutbox = wibox.widget.textbox(beautiful['layout_txt_' .. awful.layout.getname(awful.layout.get(s))])
        awful.tag.attached_connect_signal(
            s,
            'property::selected',
            function()
                update_txt_layoutbox(s)
            end
        )
        awful.tag.attached_connect_signal(
            s,
            'property::layout',
            function()
                update_txt_layoutbox(s)
            end
        )
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
                    2,
                    function()
                        awful.layout.set(awful.layout.layouts[1])
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

        s.mytaglist =
            awful.widget.taglist {
            screen = s,
            filter = s.index == 1 and awful.widget.taglist.filter.noempty or awful.widget.taglist.filter.selected,
            buttons = taglist_buttons
        }

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
                -- Right widgets
                layout = wibox.layout.fixed.horizontal,
                spacing = 10,
                memorywidget,
                netthroughwidget,
                temperaturewidget,
                wifiwidget,
                ipwidget,
                batterywidget,
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

local is_ysgrifennwr = false
local function toggle_theme()
    -- VS Code
    local file = io.open(gears.filesystem.get_xdg_config_home() .. 'Code/User/settings.json', 'r')
    local content = file:read 'a'
    file:close()
    file = io.open(gears.filesystem.get_xdg_config_home() .. 'Code/User/settings.json', 'w')
    if is_ysgrifennwr then
        content = string.gsub(content, 'Ysgrifennwr', 'Qillqaq')
    else
        content = string.gsub(content, 'Qillqaq', 'Ysgrifennwr')
    end
    file:write(content)
    file:close()
    -- Alacritty
    file = io.open(gears.filesystem.get_xdg_config_home() .. 'alacritty/alacritty.yml', 'r')
    content = file:read 'a'
    file:close()
    file = io.open(gears.filesystem.get_xdg_config_home() .. 'alacritty/alacritty.yml', 'w')
    if is_ysgrifennwr then
        for i, v in ipairs(colors.ys) do
            content = string.gsub(content, v, colors.qi[i])
        end
    else
        for i, v in ipairs(colors.qi) do
            content = string.gsub(content, v, colors.ys[i])
        end
    end
    file:write(content)
    file:close()
    -- X11
    awful.spawn {
        'xrdb',
        '-override',
        gears.filesystem.get_xdg_config_home() .. (is_ysgrifennwr and 'Xresources-qillqaq' or 'Xresources-ysgrifennwr')
    }
    is_ysgrifennwr = not is_ysgrifennwr
end

globalkeys =
    gears.table.join(
    awful.key({modkey}, 'F1', hotkeys_popup.show_help, {description = 'show help', group = 'awesome'}),
    awful.key({modkey}, 'Left', awful.tag.viewprev, {description = 'view previous', group = 'tag'}),
    awful.key({modkey}, 'Right', awful.tag.viewnext, {description = 'view next', group = 'tag'}),
    awful.key({modkey}, '#49', awful.tag.history.restore, {description = 'go back', group = 'tag'}),
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
        {description = 'focus the next screen', group = '🖥️ screen'}
    ),
    awful.key(
        {modkey, 'Control'},
        'k',
        function()
            awful.screen.focus_relative(-1)
        end,
        {description = 'focus the previous screen', group = '🖥️ screen'}
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
            awful.spawn(os.getenv 'TERMINAL')
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
        'BackSpace',
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
        'space',
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
            awful.spawn {'ssh-add', '-D'}
            awful.spawn {'slock'}
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
    ),
    awful.key({modkey}, 'F6', toggle_theme, {description = 'change theme', group = 'awesome'}),
    -- XXX next version: https://www.reddit.com/r/awesomewm/comments/bcc73o/how_to_use_clipboard_in_awesome/
    awful.key(
        {modkey},
        '#34',
        function()
            local sel = selection()
            if sel then
                naughty.notify({text = tostring(sel)})
            end
        end,
        {description = 'show content of clipboard', group = '📋 clipboard'}
    ),
    awful.key(
        {modkey},
        'b',
        function()
            beautiful.useless_gap = beautiful.useless_gap == 0 and 10 or 0
            for s in screen do
                awful.layout.arrange(s)
            end
        end,
        {description = 'toggle useless gaps', group = 'layout'}
    )
)

-- Bind all key numbers to tags.
for i = 1, 12 do
    globalkeys =
        gears.table.join(
        globalkeys,
        -- View tag only.
        awful.key(
            {modkey},
            '#' .. i + 9,
            function()
                local tags = i % #screen.primary.tags
                -- index of primary screen is not necessarily #1
                local primary_index = screen.primary.index
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                naughty.notify {title = #screen.tags .. ' ' .. tags .. ' ' .. primary_index}
                if tag then
                    tag:view_only()
                end
            end,
            {description = 'view tag #' .. i, group = 'tag'}
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
            {description = 'toggle tag #' .. i, group = 'tag'}
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
            {description = 'move focused client to tag #' .. i, group = 'tag'}
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
            {description = 'toggle focused client on tag #' .. i, group = 'tag'}
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
else
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key({modkey}, '#63', strawberry_next, {description = '⏭️', group = '🍓'}),
        awful.key({modkey}, '#106', strawberry_prev, {description = '⏮', group = '🍓'}),
        awful.key({modkey}, '#87', strawberry_playpause, {description = '⏯', group = '🍓'}),
        awful.key({modkey}, '#85', strawberry_fwd, {description = '⏩', group = '🍓'}),
        awful.key({modkey}, '#83', strawberry_rew, {description = '⏪', group = '🍓'})
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
            [[pacmd list-sinks | grep -A 15 '* index' | awk '/volume: front/{gsub("%","",$5); print $5 }']],
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
else
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

if gears.filesystem.file_readable '/sys/class/leds/smc::kbd_backlight/max_brightness' then
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

if gears.filesystem.file_readable '/sys/class/backlight/intel_backlight/max_brightness' then
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
            {description = '🔅', group = '🖥️ screen'}
        ),
        awful.key(
            {},
            'XF86MonBrightnessUp',
            function()
                increase_keyboard_light(5)
            end,
            {description = '🔆', group = '🖥️ screen'}
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
    {
        rule = {class = 'Journalctl'},
        properties = {screen = 1, tag = '🗞️'}
    },
    {
        rule = {class = 'strawberry'},
        properties = {screen = 1, tag = '🍓'}
    },
    {
        rule = {class = 'Google-chrome'},
        properties = {screen = 1, tag = '🏄🏾‍♀️'}
    }
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
