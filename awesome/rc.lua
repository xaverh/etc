local gears = require 'gears'
local awful = require 'awful'
require 'awful.autofocus'
local wibox = require 'wibox'
local beautiful = require 'beautiful'
local naughty = require 'naughty'
local hotkeys_popup = require 'awful.hotkeys_popup'
local theme_assets = require 'beautiful.theme_assets'
local xresources = require 'beautiful.xresources'
local dpi = xresources.apply_dpi
local fm0 = require 'fm0'
local icons = os.getenv 'XDG_CONFIG_HOME' .. '/Icons/'

local colors = {
    qi = {
        '#171717', -- Graphite Black
        '#e32791', -- Deep Cerise
        '#30c798', -- Shamrock
        '#e3c472', -- Chenin
        '#6796e6', -- Cornflower Blue
        '#e59fdf', -- Plum
        '#81d8d0', -- Riptide
        '#999999', -- Pearl Light Grey
        '#515151', -- Dark Grey
        '#e466ad', -- Hot Pink
        '#6cd1b2', -- Medium Aquamarine
        '#e4cf98', -- Double Colonial White
        '#91b0e6', -- Jordy Blue
        '#e5b6e1', -- French Lilac
        '#a2dcd7', -- Sinbad
        '#e5e6e6', -- Code Grey
        cursor = '#ff7135', -- Burnt Orange
        selbg = '#0f3a4b', -- Cyprus
        bg_med = '#333333', -- Umbra Grey
        bg_low = '#131313', -- Jet Black
        info = '#1680ac', -- Cerulean
        ui_blue = '#0f3a4b', -- Cyprus
        ui_purple = '#553a63', -- Love Symbol #2,
        ui_orange = '#522900', -- Baker's Chocolate
        error = '#ed2939', -- Alizarin
        warning = '#e9a700' -- Gamboge
    },
    ys = {
        '#f9f8f4', -- Traffic White
        '#e32791', -- Deep Cerise
        '#488432', -- La Palma
        '#a25d0e', -- Golden Brown
        '#2c65b5', -- Cerulean Blue
        '#b062a7', -- Violet Blue
        '#27bbbe', -- Light Sea Green
        '#999999', -- Pearl Light Grey
        '#b8b8b8', -- Fortress Grey
        '#9f1b66', -- Jazzberry Jam
        '#325d23', -- Parsley
        '#71410a', -- Raw Umber
        '#1f477f', -- Bahama Blue
        '#7b4474', -- Eminence
        '#1b8486', -- Atoll
        '#424242', -- Meaning of Everything Grey,
        bg_med = '#dddbd7', -- Light grey
        bg_low = '#edece8', -- Signal White
        selbg = '#baddff', -- Onahau
        cursor = '#20bbfc', -- Deep Sky Blue
        info = '#20bbfc', -- Deep Sky Blue
        ui_blue = '#0f3a4b', -- Cyprus
        ui_purple = '#553a63', -- Love Symbol #2
        ui_orange = '#964f00', -- Saddle Brown
        error = '#ed2939', -- Alizarin
        warning = '#c08a00' -- Dark Goldenrod
    }
}

local has_volume_keys = true
local has_multimedia_keys = true
local is_macintosh = false
local temperature_filename = '/sys/class/thermal/thermal_zone1/temp'

-- TODO:
-- if (os.date('*t').hour + 16) % 24 >= 12 then
-- my_theme = 'qi'
-- else
-- my_theme = 'ys'
-- end
local my_theme = 'qi'

local themes_path = gears.filesystem.get_themes_dir()

beautiful.init {
    layout_fairh = themes_path .. 'default/layouts/fairhw.png',
    layout_fairv = themes_path .. 'default/layouts/fairvw.png',
    layout_floating = themes_path .. 'default/layouts/floatingw.png',
    layout_magnifier = themes_path .. 'default/layouts/magnifierw.png',
    layout_max = themes_path .. 'default/layouts/maxw.png',
    layout_fullscreen = themes_path .. 'default/layouts/fullscreenw.png',
    layout_tilebottom = themes_path .. 'default/layouts/tilebottomw.png',
    layout_tileleft = themes_path .. 'default/layouts/tileleftw.png',
    layout_tile = themes_path .. 'default/layouts/tilew.png',
    layout_tiletop = themes_path .. 'default/layouts/tiletopw.png',
    layout_spiral = themes_path .. 'default/layouts/spiralw.png',
    layout_dwindle = themes_path .. 'default/layouts/dwindlew.png',
    layout_cornernw = themes_path .. 'default/layouts/cornernww.png',
    layout_cornerne = themes_path .. 'default/layouts/cornernew.png',
    layout_cornersw = themes_path .. 'default/layouts/cornersww.png',
    layout_cornerse = themes_path .. 'default/layouts/cornersew.png',
    font = 'sans 9',
    menu_font = 'sans 10',
    hotkeys_font = 'sans 9',
    hotkeys_description_font = 'sans 9',
    tasklist_font_minimized = 'sans Italic 9',
    tasklist_font_focus = 'sans Bold 9',
    bg_normal = colors[my_theme][1],
    bg_focus = colors[my_theme][1],
    bg_urgent = colors[my_theme][2],
    bg_minimize = colors[my_theme][1],
    fg_normal = colors[my_theme][16],
    fg_focus = colors[my_theme][16],
    fg_urgent = colors[my_theme][16],
    fg_minimize = colors[my_theme][8],
    hotkeys_modifiers_fg = colors[my_theme][8],
    border_normal = colors[my_theme][9],
    border_focus = colors[my_theme].cursor,
    border_marked = colors[my_theme][4],
    taglist_bg_focus = colors[my_theme][9],
    maximized_hide_border = true,
    useless_gap = 0,
    border_width = dpi(3),
    menu_submenu_icon = gears.filesystem.get_themes_dir() .. 'default/submenu.png',
    menu_height = dpi(20),
    menu_width = dpi(300),
    menu_bg_focus = colors[my_theme].ui_purple,
    menu_bg_normal = '#edece8',
    menu_fg_normal = colors.ys[16],
    menu_border_width = dpi(1),
    master_width_factor = 0.55,
    -- layout_txt = {
    -- tile = '👈🏻',
    -- fairv = '🤙🏻',
    -- fairh = '🙌🏻',
    -- max = '👊🏻',
    -- magnifier = '🤏🏻',
    -- floating = '🖖🏻'
    -- },
    tasklist_disable_icon = false,
    tasklist_align = 'center',
    wibar_height = dpi(20),
    notification_max_width = dpi(460),
    notification_icon_size = dpi(100),
    notification_border_width = dpi(2),
    notification_border_color = colors[my_theme].cursor,
    -- XXX waiting for random function in awesome 4.4
    -- wallpaper = os.getenv 'HOST' == 'aberystwyth' and gears.filesystem.get_xdg_data_home() .. 'Tapet/1280x800/tapet_2020-02-26_19-57-31_856_1280x800.png' or os.getenv 'HOME' .. '/var/7015773-girl-bus-mood.jpg'
    wallpaper = os.getenv 'HOME' .. '/.local/wallpapers/qillqaq/MKQeftK.jpg'
}

beautiful.taglist_squares_sel = theme_assets.taglist_squares_sel(dpi(5), beautiful.fg_normal)
beautiful.taglist_squares_unsel = theme_assets.taglist_squares_unsel(dpi(5), beautiful.fg_normal)
beautiful.awesome_icon =
    theme_assets.awesome_icon(beautiful.menu_height, beautiful.bg_focus, beautiful.fg_focus)

local function connect_bluetooth(connect, mac)
    awful.spawn {'bluetoothctl', connect and 'connect' or 'disconnect', mac}
end

-- open_terminal passes arguments on as they come: table, string, or nil
local function open_terminal(first_arg, ...)
    if ... ~= nil then
        awful.spawn(table.pack('alacritty', first_arg, ...))
    elseif first_arg then
        awful.spawn('alacritty ' .. first_arg)
    else
        awful.spawn 'alacritty'
    end
end

local printkey = is_macintosh and 'XF86LaunchA' or 'Print'

awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.max,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.fair,
    awful.layout.suit.floating,
    awful.layout.suit.magnifier
}

local myawesomemenu = {
    {
        'hotkeys',
        function()
            hotkeys_popup.show_help(nil, awful.screen.focused())
        end
    }
}

local bluetooth_menu = {
    {
        'connect bluetooth device (Setúbal)',
        function()
            connect_bluetooth(true, '88:C6:26:F4:8A:90')
        end
    },
    {
        'disconnect bluetooth device (Setúbal)',
        function()
            connect_bluetooth(false, '88:C6:26:F4:8A:90')
        end
    },
    {
        'connect bluetooth device (JBL GO)',
        function()
            connect_bluetooth(true, '78:44:05:E4:E0:D4')
        end
    },
    {
        'disconnect bluetooth device (JBL GO)',
        function()
            connect_bluetooth(false, '78:44:05:E4:E0:D4')
        end
    }
}

local myexitmenu = {
    {'restart', awesome.restart, icons .. 'restart.svg'},
    {'quit awesome', awesome.quit},
    {'suspend', 'systemctl suspend', icons .. 'suspend.svg'},
    {'reboot', 'systemctl reboot', icons .. 'reboot.svg'},
    {'shutdown', 'systemctl poweroff', icons .. 'poweroff.svg'}
}

local function change_terminal_font(font, bold_font)
    local command =
        string.format(
        '\27]710;%s,xft:Symbola:style=Regular\27\92\27]711;%s,xft:Symbola:style=Regular\27\92',
        font,
        bold_font
    )
    local terminals = {}
    for i = 0, 99 do
        local terminal = '/dev/pts/' .. i
        if gears.filesystem.file_readable(terminal) then
            table.insert(terminals, terminal)
        end
    end
    for i, terminal in ipairs(terminals) do
        local file = io.open(terminal, 'w')
        file:write(command)
        file:close()
    end
end

local terminal_font_menu = {
    {
        'Consolas 14',
        function()
            change_terminal_font(
                'xft:Consolas:style=Regular:pixelsize=14px',
                'xft:Consolas:style=Bold:pixelsize=14px'
            )
        end
    },
    {
        'Consolas 16',
        function()
            change_terminal_font(
                'xft:Consolas:style=Regular:pixelsize=16px',
                'xft:Consolas:style=Bold:pixelsize=16px'
            )
        end
    },
    {
        'SF Mono 12',
        function()
            change_terminal_font(
                'xft:SF Mono:style=Regular:pixelsize=12px',
                'xft:SF Mono:style=Bold:pixelsize=12px'
            )
        end
    },
    {
        'SF Mono 13',
        function()
            change_terminal_font(
                'xft:SF Mono:style=Regular:pixelsize=13px',
                'xft:SF Mono:style=Bold:pixelsize=13px'
            )
        end
    },
    {
        'SF Mono 14',
        function()
            change_terminal_font(
                'xft:SF Mono:style=Regular:pixelsize=14px',
                'xft:SF Mono:style=Bold:pixelsize=14px'
            )
        end
    },
    {
        'SF Mono 15',
        function()
            change_terminal_font(
                'xft:SF Mono:style=Regular:pixelsize=15px',
                'xft:SF Mono:style=Bold:pixelsize=15px'
            )
        end
    },
    {
        'SF Mono 16',
        function()
            change_terminal_font(
                'xft:SF Mono:style=Regular:pixelsize=16px',
                'xft:SF Mono:style=Bold:pixelsize=16px'
            )
        end
    },
    {
        'IBM Plex Mono 12',
        function()
            change_terminal_font(
                'xft:IBM Plex Mono:style=Regular:pixelsize=12px',
                'xft:IBM Plex Mono:style=Bold:pixelsize=12px'
            )
        end
    },
    {
        'IBM Plex Mono 13',
        function()
            change_terminal_font(
                'xft:IBM Plex Mono:style=Regular:pixelsize=13px',
                'xft:IBM Plex Mono:style=Bold:pixelsize=13px'
            )
        end
    },
    {
        'IBM Plex Mono 14',
        function()
            change_terminal_font(
                'xft:IBM Plex Mono:style=Regular:pixelsize=14px',
                'xft:IBM Plex Mono:style=Bold:pixelsize=14px'
            )
        end
    },
    {
        'IBM Plex Mono 15',
        function()
            change_terminal_font(
                'xft:IBM Plex Mono:style=Regular:pixelsize=15px',
                'xft:IBM Plex Mono:style=Bold:pixelsize=15px'
            )
        end
    },
    {
        'IBM Plex Mono 16',
        function()
            change_terminal_font(
                'xft:IBM Plex Mono:style=Regular:pixelsize=16px',
                'xft:IBM Plex Mono:style=Bold:pixelsize=16px'
            )
        end
    }
}

local mymainmenu =
    awful.menu {
    items = {
        {'awesome', myawesomemenu, beautiful.awesome_icon},
        {'Bluetooth', bluetooth_menu, icons .. 'bluetooth.svg'},
        {'FM0', fm0.get_menu()},
        {'change terminal font', terminal_font_menu},
        {'exit', myexitmenu},
        {
            'open terminal',
            function()
                open_terminal()
            end,
            '/usr/share/doc/nodejs/npm/docs/src/images/terminal-icon.svg'
        }
    }
}

local mylauncher =
    awful.widget.launcher(
    {
        image = beautiful.awesome_icon,
        menu = mymainmenu
    }
)

local mytextclock = wibox.widget.textclock('%a %-d %b %T %Z', 1)

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
        {'Mod4'},
        1,
        function(t)
            if client.focus then
                client.focus:move_to_tag(t)
            end
        end
    ),
    awful.button({}, 3, awful.tag.viewtoggle),
    awful.button(
        {'Mod4'},
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

-- TODO
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

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal('property::geometry', set_wallpaper)

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

local btrfswidget =
    awful.widget.watch(
    {'btrfs', 'filesystem', 'usage', '--si', '/'},
    5.9,
    function(widget, stdout, _, _, _)
        local free, unit = stdout:match 'Free %(estimated%):%s*([%d%.]*)(%a*)'
        if free then
            widget:set_text('  / ' .. free .. '\u{2009}' .. unit)
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
                    function(stdout)
                        widget:set_text(
                            (stdout:match 'SSID: ([^\n]*)' or 'no') ..
                                ' ' ..
                                    (stdout:match 'Connected to %x%x:%x%x:%x%x:(%x%x:%x%x:%x%x)' or
                                        'WiFi')
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
            widget:set_text('θ\u{2009}' .. temperature // 1000 .. '\u{2009}°C')
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
                    local charging_symbol = battery_status == 'Charging' and '↑' or '↓'
                    widget:set_text(capacity .. '\u{2009}%' .. charging_symbol)
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
        return string.format('%d\u{2009}%s', bytes, unit)
    end
    return string.format('%.1f\u{2009}%s', result, unit)
end

local memorywidget =
    (function()
    local t = gears.timer {timeout = 4.1}
    local widget = wibox.widget.textbox()
    t:connect_signal(
        'timeout',
        function()
            t:stop()
            for line in io.lines '/proc/meminfo' do
                for k, v in string.gmatch(line, '([%a]+):[%s]+([%d]+).+') do
                    if k == 'MemAvailable' then
                        mem_available = v * 1024
                        break
                    end
                end
            end
            widget:set_text('m ' .. fixed(mem_available))
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
                    local new_send =
                        tonumber(
                        string.match(line, '([%d]+)%s+%d+%s+%d+%s+%d+%s+%d+%s+%d+%s+%d+%s+%d$')
                    )
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
            widget:set_text('↓' .. fixed(recv_rate) .. '/s  ↑' .. fixed(send_rate) .. '/s')
            t:again()
        end
    )
    t:start()
    t:emit_signal 'timeout'
    return widget
end)()

local default_tags = {'🌐', '☕', '👾', '🧠', '🍓', '🎲', '🎰', '🎱', '🚽', '🏝️', '🏜️', '🦄'} -- 🥑

awful.screen.connect_for_each_screen(
    function(s)
        -- Wallpaper
        set_wallpaper(s)
        if s.index == 1 then
            awful.tag({table.unpack(default_tags, 1, 9)}, s, awful.layout.layouts[1])
        elseif s.index == 2 then
            awful.tag({table.unpack(default_tags, 10, 11)}, s, awful.layout.layouts[1])
        elseif s.index == 3 then
            awful.tag({default_tags[12]}, s, awful.layout.layouts[1])
        end

        -- Textual layoutbox
        -- s.mylayoutbox = wibox.widget.textbox(beautiful.layout_txt[awful.layout.getname(awful.layout.get(s))])
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
            filter = awful.widget.taglist.filter.noempty,
            buttons = taglist_buttons
        }

        s.mytasklist =
            awful.widget.tasklist {
            screen = s,
            filter = awful.widget.tasklist.filter.currenttags,
            buttons = tasklist_buttons
        }

        s.mywibox = awful.wibar {position = 'top', screen = s}

        s.mywibox:setup {
            layout = wibox.layout.align.horizontal,
            {
                mylauncher,
                layout = wibox.layout.fixed.horizontal,
                s.mytaglist,
                s.mylayoutbox
            },
            s.mytasklist,
            {
                layout = wibox.layout.fixed.horizontal,
                spacing = 8,
                btrfswidget,
                memorywidget,
                netthroughwidget,
                temperaturewidget,
                wifiwidget,
                ipwidget,
                batterywidget,
                mytextclock
            }
        }
        screen[s]:connect_signal(
            'arrange',
            function(s)
                for _, c in pairs(s.all_clients) do
                    if
                        c.class == 'Vivaldi-snapshot' and c.instance == 'vivaldi-snapshot' or
                            c.instance == 'crx_cinhimbnkkaeohfgghhklpknlkffjgod'
                     then
                        if
                            s.selected_tag.layout.name == 'max' or
                                s.selected_tag.layout.name ~= 'floating' and #s.clients == 1
                         then
                            if not c.maximized then
                                c.maximized = true
                                awful.layout.arrange(s)
                            end
                        else
                            if c.maximized then
                                c.maximized = false
                                awful.layout.arrange(s)
                            end
                        end
                    end
                end
                for _, c in pairs(s.clients) do
                    if
                        beautiful.useless_gap == 0 and
                            (s.selected_tag.layout.name == 'max' or
                                s.selected_tag.layout.name ~= 'floating' and #s.tiled_clients == 1) and
                            not c.floating or
                            c.maximized
                     then
                        c.border_width = 0
                    else
                        c.border_width = beautiful.border_width
                    end
                end
            end
        )
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

local function toggle_theme()
    my_theme = my_theme == 'qi' and 'ys' or 'qi'
    beautiful.bg_normal = colors[my_theme][1]
    beautiful.bg_focus = colors[my_theme][1]
    beautiful.bg_urgent = colors[my_theme][2]
    beautiful.bg_minimize = colors[my_theme][1]
    beautiful.fg_normal = colors[my_theme][16]
    beautiful.fg_focus = colors[my_theme][16]
    beautiful.fg_urgent = colors[my_theme][16]
    beautiful.fg_minimize = colors[my_theme][8]
    beautiful.hotkeys_modifiers_fg = colors[my_theme][8]
    beautiful.border_normal = colors[my_theme][9]
    beautiful.border_focus = colors[my_theme].cursor
    beautiful.border_marked = colors[my_theme][4]
    beautiful.taglist_bg_focus = colors[my_theme][9]
    beautiful.taglist_squares_sel = theme_assets.taglist_squares_sel(dpi(5), beautiful.fg_normal)
    beautiful.taglist_squares_unsel =
        theme_assets.taglist_squares_unsel(dpi(5), beautiful.fg_normal)
    beautiful.awesome_icon =
        theme_assets.awesome_icon(beautiful.menu_height, beautiful.bg_focus, beautiful.fg_focus)
    mylauncher.image = beautiful.awesome_icon
    beautiful.notification_border_color = colors[my_theme].cursor
    for s in screen do
        s.mywibox.bg = colors[my_theme][1]
        s.mywibox.fg = colors[my_theme][16]
        if client.focus then
            client.focus:emit_signal 'focus'
        end
        for _, c in ipairs(s.clients) do
            if c ~= client.focus then
                c.border_color = beautiful.border_normal
            end
        end
        -- update_txt_layoutbox(s)
    end
    -- VS Code
    local file = io.open(gears.filesystem.get_xdg_config_home() .. 'Code/User/settings.json', 'r')
    local content = file:read 'a'
    file:close()
    file = io.open(gears.filesystem.get_xdg_config_home() .. 'Code/User/settings.json', 'w')
    if my_theme == 'qi' then
        content = string.gsub(content, 'Ysgrifennwr', 'Qillqaq')
    elseif my_theme == 'ys' then
        content = string.gsub(content, 'Qillqaq', 'Ysgrifennwr')
    end
    file:write(content)
    file:close()
    -- Alacritty
    local file = io.open(gears.filesystem.get_xdg_config_home() .. 'alacritty.yml', 'r')
    local content = file:read 'a'
    file:close()
    file = io.open(gears.filesystem.get_xdg_config_home() .. 'alacritty.yml', 'w')
    if my_theme == 'qi' then
        content = string.gsub(content, 'colors: #y', 'yolors:')
        content = string.gsub(content, 'qolors:', 'colors: #q')
    elseif my_theme == 'ys' then
        content = string.gsub(content, 'colors: #q', 'qolors:')
        content = string.gsub(content, 'yolors:', 'colors: #y')
    end
    file:write(content)
    file:close()
end

local function tag_view_nonempty(direction)
    for i = 1, #awful.screen.focused().tags do
        awful.tag.viewidx(i * direction)
        if #awful.screen.focused().clients > 0 then
            return
        else
            awful.tag.history.restore(awful.screen.focused(), 'previous')
        end
    end
end

local function mansplain()
    awful.spawn.easy_async_with_shell(
        'apropos . | dmenu -i -l 10 -p mansplain',
        function(stdout, _, _, exitcode)
            if exitcode == 0 then
                awful.spawn.easy_async_with_shell(
                    [[awk '{gsub(/[()]/,""); print $2" "$1}' <<<']] .. stdout .. "'",
                    function(stdout)
                        open_terminal(
                            string.format(
                                '--title "mansplain: %s" --class mansplain -e man %s',
                                stdout,
                                stdout
                            )
                        )
                    end
                )
            end
        end
    )
end

local function emoji()
    awful.spawn.with_shell [=[cat ~/.config/awesome/emoji.txt | dmenu -i -p '¯\_(ツ)_/¯' | awk '{printf $1}' | xsel -ib]=]
end

local function open_url()
    awful.spawn.with_shell(
        string.format(
            [=[xdg-open $(grep --no-filename --only-matching --perl-regexp "http(s?):\/\/[^ \"\(\)\<\>\]]*" %s/clipmenu.6.%s/*\ * | awk '!x[$0]++' | dmenu -i -p '🌐')]=],
            os.getenv 'CM_DIR',
            os.getenv 'USER'
        )
    )
end

function sharedmovetag(tag, screen)
    screen = screen or awful.screen.focused()
    local oldscreen = tag.screen
    -- If the specified tag is allocated to another screen, we need to move it.
    if oldscreen ~= screen then
        local oldsel = oldscreen.selected_tag
        tag.screen = screen

        if oldsel == tag then
            -- The tag has been moved away. In most cases the tag history
            -- function will find the best match, but if we really want we can
            -- try to find a fallback tag as well.
            if not oldscreen.selected_tag then
                local newtag = awful.tag.find_fallback(oldscreen)
                if newtag then
                    newtag:view_only()
                end
            end
        end
        return true
    end
    return false
end

function sharedviewtoggle(tag, screen)
    local oldscreen = tag.screen

    if sharedmovetag(tag, screen) then
        -- Always mark the tag selected if the screen changed. Just feels a lot
        -- more natural.
        tag.selected = true
        -- Update the history on the old and new screens.
        oldscreen:emit_signal('tag::history::update')
        tag.screen:emit_signal('tag::history::update')
    else
        -- Only toggle the tag unless the screen moved.
        awful.tag.viewtoggle(tag)
    end
end

globalkeys =
    gears.table.join(
    awful.key({'Mod4'}, 'F1', mansplain, {description = 'show help', group = '🚀 launcher'}),
    awful.key({'Mod4'}, 'e', emoji, {description = [[¯\_(ツ)_/¯]], group = '🌐 global'}),
    awful.key({'Mod4'}, 'u', open_url, {description = 'open URL', group = '📋 clipboard'}),
    awful.key(
        {'Mod4'},
        'a',
        function()
            awful.spawn.easy_async_with_shell(
                [[tmux list-session -F \#S | dmenu -i -p tmux]],
                function(stdout)
                    if stdout ~= '' then
                        open_terminal(
                            '-e',
                            'tmux',
                            'new-session',
                            '-A',
                            '-s',
                            string.sub(stdout, 1, -2)
                        )
                    end
                end
            )
        end,
        {description = 'attach to tmux session', group = '🚀 launcher'}
    ),
    awful.key(
        {'Mod4'},
        'o',
        function()
            awful.spawn {'clipmenu', '-i', '-p', '📋'}
        end,
        {description = 'clipmenu', group = '📋 clipboard'}
    ),
    awful.key(
        {'Mod4'},
        'F2',
        hotkeys_popup.show_help,
        {description = 'show help', group = '🌐 global'}
    ),
    awful.key(
        {'Mod4'},
        'Escape',
        function()
            tag_view_nonempty(1)
        end,
        {description = 'view next non-empty tag', group = '🏷️ tag'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'Escape',
        function()
            tag_view_nonempty(-1)
        end,
        {description = 'view previous non-empty tag', group = '🏷️ tag'}
    ),
    awful.key(
        {'Mod4'},
        '#49',
        awful.tag.history.restore,
        {description = 'go back', group = '🏷️ tag'}
    ),
    awful.key(
        {'Mod4'},
        'Left',
        function()
            awful.client.focus.global_bydirection 'left'
        end,
        {description = 'focus client to the left', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'Up',
        function()
            awful.client.focus.global_bydirection 'up'
        end,
        {description = 'focus client above', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'Down',
        function()
            awful.client.focus.global_bydirection 'down'
        end,
        {description = 'focus client below', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'Right',
        function()
            awful.client.focus.global_bydirection 'right'
        end,
        {description = 'focus client to the right', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'Left',
        function()
            awful.client.swap.global_bydirection 'left'
        end,
        {description = 'swap client with client to the left', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'Up',
        function()
            awful.client.swap.global_bydirection 'up'
        end,
        {description = 'swap client with client above', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'Down',
        function()
            awful.client.swap.global_bydirection 'down'
        end,
        {description = 'swap client with client below', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'Right',
        function()
            awful.client.swap.global_bydirection 'right'
        end,
        {description = 'swap client with client to the right', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'j',
        function()
            awful.client.focus.byidx(1)
        end,
        {description = 'focus next by index', group = '🎮 client'}
    ),
    awful.key(
        {'Mod1'},
        'Tab',
        function()
            awful.client.focus.byidx(1)
        end,
        {description = 'focus next by index', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'k',
        function()
            awful.client.focus.byidx(-1)
        end,
        {description = 'focus previous by index', group = '🎮 client'}
    ),
    awful.key(
        {'Mod1', 'Shift'},
        'Tab',
        function()
            awful.client.focus.byidx(-1)
        end,
        {description = 'focus previous by index', group = '🎮 client'}
    ),
    awful.key(
        is_macintosh and {} or {'Mod4'},
        is_macintosh and 'XF86LaunchB' or 'Menu',
        function()
            mymainmenu:show()
        end,
        {description = 'show main menu', group = '🌐 global'}
    ),
    awful.key(
        {'Mod4'},
        '#47',
        function()
            awful.screen.focus_relative(-1)
        end,
        {description = 'focus the previous screen', group = '🖥️ screen'}
    ),
    awful.key(
        {'Mod4'},
        '#48',
        function()
            awful.screen.focus_relative(1)
        end,
        {description = 'focus the next screen', group = '🖥️ screen'}
    ),
    awful.key(
        {'Mod4'},
        'Tab',
        function()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = 'go back', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'Return',
        function()
            open_terminal()
        end,
        {description = 'open a terminal', group = '🚀 launcher'}
    ),
    awful.key(
        {'Mod4'},
        'r',
        function()
            for c in awful.client.iterate(
                function(c)
                    return awful.rules.match(c, {instance = 'FM0', class = 'mpv'})
                end
            ) do
                if client.focus == c then
                    awful.tag.viewtoggle(awful.tag.find_by_name(nil, '📻'))
                    return
                else
                    c:jump_to(true)
                    return
                end
            end
            fm0.start_radio()
        end,
        {description = '📻', group = '🚀 launcher'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'r',
        function()
            if client.focus then
                client.focus:move_to_tag(awful.tag.find_by_name(nil, '📻'))
            end
        end,
        {
            description = 'move client to 📻',
            group = '🏷️ tag'
        }
    ),
    awful.key(
        {'Mod4'},
        'n',
        function()
            local instance = 'nnn' .. awful.screen.focused().index
            local session = instance == 'nnn1' and '🧞‍♂️' or instance == 'nnn2' and '🧞‍♀️' or '🧞'
            for c in awful.client.iterate(
                function(c)
                    return awful.rules.match(
                        c,
                        {
                            instance = instance,
                            class = 'Alacritty'
                        }
                    )
                end
            ) do
                if client.focus == c then
                    awful.tag.viewtoggle(awful.tag.find_by_name(awful.screen.focused(), session))
                    return
                else
                    c:jump_to(true)
                    return
                end
            end
            open_terminal('--class', instance, '--title', 'nnn', '-e', 'nnn')
        end,
        {description = 'nnn', group = '🚀 launcher'}
    ),
    awful.key(
        {'Mod4', 'Control'},
        'n',
        function()
            local instance = 'nnn' .. awful.screen.focused().index
            local session = instance == 'nnn1' and '🧞‍♂️' or instance == 'nnn2' and '🧞‍♀️' or '🧞'
            local t = awful.tag.find_by_name(awful.screen.focused(), session)
            t:view_only()
            -- awful.screen.focus(t.screen)
        end,
        {description = 'view 🧞', group = '🏷️ tag'}
    ),
    awful.key(
        {'Mod4'},
        'v',
        function()
            for c in awful.client.iterate(
                function(c)
                    return awful.rules.match(c, {instance = 'mpvk', class = 'mpv'})
                end
            ) do
                if client.focus == c then
                    awful.tag.viewtoggle(awful.tag.find_by_name(nil, '🍿'))
                    return
                else
                    c:jump_to(true)
                    return
                end
            end
            awful.spawn.easy_async_with_shell(
                string.format(
                    [=[grep --no-filename --only-matching --perl-regexp "http(s?):\/\/[^ \"\(\)\<\>\]]*" %s/clipmenu.6.%s/*\ * | cat <<<"https://www.youtube.com/playlist?list=WL" | awk '!x[$0]++' | dmenu -i -p '🍿']=],
                    os.getenv 'CM_DIR',
                    os.getenv 'USER'
                ),
                function(stdout)
                    if stdout ~= '' then
                        awful.spawn {'mpv', '--really-quiet', string.sub(stdout, 1, -2)}
                    end
                end
            )
        end,
        {description = 'open mpv 🍿', group = '🚀 launcher'}
    ),
    awful.key(
        {'Mod4'},
        'F12',
        function()
            for c in awful.client.iterate(
                function(c)
                    return awful.rules.match(c, {instance = 'Journalctl', class = 'Alacritty'})
                end
            ) do
                if client.focus == c then
                    awful.tag.viewtoggle(awful.tag.find_by_name(nil, '🧻'))
                    return
                else
                    c:jump_to(true)
                    return
                end
            end
            open_terminal(
                '--class',
                'Journalctl',
                '--title',
                'journalctl',
                '-e',
                'journalctl',
                '-b',
                '-f',
                '-n',
                '1000'
            )
        end,
        {description = 'open journalctl 🧻', group = '🚀 launcher'}
    ),
    awful.key(
        {'Mod4', 'Control'},
        'r',
        awesome.restart,
        {description = 'reload awesome', group = '🌐 global'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'q',
        awesome.quit,
        {description = 'quit awesome', group = '🌐 global'}
    ),
    awful.key(
        {'Mod4'},
        'l',
        function()
            awful.tag.incmwfact(0.0125)
        end,
        {description = 'increase master width factor', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4'},
        'h',
        function()
            awful.tag.incmwfact(-0.0125)
        end,
        {description = 'decrease master width factor', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4'},
        'i',
        function()
            awful.tag.incnmaster(1, nil, true)
        end,
        {description = 'increase the number of master clients', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4'},
        'd',
        function()
            awful.tag.incnmaster(-1, nil, true)
        end,
        {description = 'decrease the number of master clients', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Control'},
        'Right',
        function()
            awful.tag.incmwfact(0.0125)
        end,
        {description = 'increase master width factor', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Control'},
        'Left',
        function()
            awful.tag.incmwfact(-0.0125)
        end,
        {description = 'decrease master width factor', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Control'},
        'Down',
        function()
            awful.tag.incnmaster(1, nil, true)
        end,
        {description = 'decrease the number of master clients', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Control'},
        'Up',
        function()
            awful.tag.incnmaster(-1, nil, true)
        end,
        {description = 'increase the number of master clients', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4'},
        'g',
        function()
            awful.layout.set(awful.layout.suit.fair)
        end,
        {description = 'select 🤙', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'g',
        function()
            awful.layout.set(awful.layout.suit.fair.horizontal)
        end,
        {description = 'select 🙌', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4'},
        't',
        function()
            awful.layout.set(awful.layout.suit.tile)
        end,
        {description = 'select 👈', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4'},
        'm',
        function()
            awful.layout.set(awful.layout.suit.max)
        end,
        {description = 'select 👊', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4'},
        'f',
        function()
            awful.layout.set(awful.layout.suit.floating)
        end,
        {description = 'select 🖖', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4'},
        'w',
        function()
            awful.layout.set(awful.layout.suit.magnifier)
        end,
        {description = 'select 🤏', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4'},
        '#61',
        function()
            local c = awful.client.restore()
            -- Focus restored client
            if c then
                c:emit_signal('request::activate', 'key.unminimize', {raise = true})
            end
        end,
        {description = 'restore minimized', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'space',
        function()
            awful.spawn 'dmenu_run'
        end,
        {description = 'run prompt', group = '🚀 launcher'}
    ),
    awful.key(
        {},
        printkey,
        function()
            awful.spawn {'flameshot', 'full', '-c', '-p', os.getenv 'HOME' .. '/tmp'}
        end,
        {description = 'take a screenshot', group = '🌐 global'}
    ),
    awful.key(
        {'Mod4'},
        printkey,
        function()
            awful.spawn {'flameshot', 'gui', '-p', os.getenv 'HOME' .. '/tmp'}
        end,
        {description = 'capture a portion of the screen', group = '🌐 global'}
    ),
    awful.key(
        {'Mod4'},
        is_macintosh and 'XF86Eject' or 'Pause',
        function()
            awful.spawn {'ssh-add', '-D'}
            awful.spawn 'slock'
        end,
        {description = 'lock screen and SSH', group = '🌐 global'}
    ),
    awful.key(
        {'Mod4'},
        'F9',
        function()
            connect_bluetooth(true, '88:C6:26:F4:8A:90')
        end,
        {description = 'connect bluetooth device (Setúbal)', group = '🎧 audio'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'F9',
        function()
            connect_bluetooth(false, '88:C6:26:F4:8A:90')
        end,
        {description = 'disconnect bluetooth device (Setúbal)', group = '🎧 audio'}
    ),
    awful.key({'Mod4'}, 'F6', toggle_theme, {description = 'change theme', group = '🌐 global'})
)

clientkeys =
    gears.table.join(
    awful.key(
        {'Mod4', 'Shift'},
        'w',
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = 'toggle fullscreen', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'Return',
        function(c)
            if c ~= awful.client.getmaster() then
                awful.client.setmaster(c)
            else
                awful.client.setslave(c)
                awful.client.focus.byidx(0, awful.client.getmaster())
            end
        end,
        {description = 'zoom', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'c',
        function(c)
            c:kill()
        end,
        {description = 'close', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'f',
        function(c)
            c.floating = not c.floating
            c:raise()
        end,
        {description = '✈ toggle floating', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        's',
        function(c)
            c.sticky = not c.sticky
            c:raise()
        end,
        {description = '▪ toggle sticky', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'o',
        function(c)
            c:move_to_screen()
        end,
        {description = 'move to screen', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        't',
        function(c)
            c.ontop = not c.ontop
        end,
        {description = '⌃ toggle keep on top', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'period',
        function(c)
            c.minimized = true
        end,
        {description = 'minimize', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'm',
        function(c)
            c.maximized = not c.maximized
            c:raise()
        end,
        {description = '+ (un)maximize', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Control'},
        'm',
        function(c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end,
        {description = '⬍ (un)maximize vertically', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Mod1'},
        'm',
        function(c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end,
        {description = '⬌ (un)maximize horizontally', group = '🎮 client'}
    ),
    -- XXX next version: https://www.reddit.com/r/awesomewm/comments/bcc73o/how_to_use_clipboard_in_awesome/
    awful.key(
        {'Mod4'},
        '#34',
        function()
            local sel = selection()
            if sel then
                local text = tostring(sel)
                if latest_clipboard_notification then
                    naughty.replace_text(latest_clipboard_notification, text, '')
                else
                    latest_clipboard_notification =
                        naughty.notify {
                        title = text,
                        destroy = function()
                            latest_clipboard_notification = nil
                        end
                    }
                end
            end
        end,
        {description = 'show content of clipboard', group = '📋 clipboard'}
    ),
    awful.key(
        {'Mod4'},
        'b',
        function()
            beautiful.useless_gap = beautiful.useless_gap == 0 and dpi(10) or 0
            for s in screen do
                awful.layout.arrange(s)
            end
        end,
        {description = 'toggle useless gaps', group = '💠 layout'}
    )
)

for i, v in ipairs(default_tags) do
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key(
            {'Mod4'},
            '#' .. i + 9,
            function()
                local t = awful.tag.find_by_name(nil, v)
                t:view_only()
                awful.screen.focus(t.screen)
            end,
            {
                description = 'view ' .. v,
                group = '🏷️ tag'
            }
        ),
        awful.key(
            {'Mod4', 'Control'},
            '#' .. i + 9,
            function()
                sharedviewtoggle(awful.tag.find_by_name(nil, v))
            end,
            {
                description = 'toggle ' .. v,
                group = '🏷️ tag'
            }
        ),
        awful.key(
            {'Mod4', 'Shift'},
            '#' .. i + 9,
            function()
                if client.focus then
                    client.focus:move_to_tag(awful.tag.find_by_name(nil, v))
                end
            end,
            {
                description = 'move client to ' .. v,
                group = '🏷️ tag'
            }
        ),
        awful.key(
            {'Mod4', 'Control', 'Shift'},
            '#' .. i + 9,
            function()
                if client.focus then
                    client.focus:toggle_tag(awful.tag.find_by_name(nil, v))
                end
            end,
            {
                description = 'toggle ' .. v .. ' on client',
                group = '🏷️ tag'
            }
        )
    )
end

local function strawberry_next()
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

local function playerctl_next()
    awful.spawn 'playerctl next'
end

local function playerctl_prev()
    awful.spawn 'playerctl previous'
end

local function playerctl_playpause()
    awful.spawn 'playerctl play-pause'
end

local function playerctl_fwd()
    awful.spawn 'playerctl position 10+'
end

local function playerctl_rew()
    awful.spawn 'playerctl position 10-'
end

local function playerctl_stop()
    awful.spawn 'playerctl stop'
end

if has_multimedia_keys then
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key({}, 'XF86AudioNext', playerctl_next, {description = '⏭️', group = '🌐 global'}),
        awful.key({}, 'XF86AudioPrev', playerctl_prev, {description = '⏮', group = '🌐 global'}),
        awful.key(
            {},
            'XF86AudioPlay',
            playerctl_playpause,
            {description = '⏯', group = '🌐 global'}
        ),
        awful.key({}, 'XF86AudioStop', playerctl_stop, {description = '⏹️', group = '🌐 global'}),
        awful.key(
            {'Shift'},
            'XF86AudioNext',
            playerctl_fwd,
            {description = '⏩', group = '🌐 global'}
        ),
        awful.key(
            {'Shift'},
            'XF86AudioPrev',
            playerctl_rew,
            {description = '⏪', group = '🌐 global'}
        )
    )
else
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key({'Mod4'}, '#63', strawberry_next, {description = '⏭️', group = '🍓 strawberry'}),
        awful.key({'Mod4'}, '#106', strawberry_prev, {description = '⏮', group = '🍓 strawberry'}),
        awful.key(
            {'Mod4'},
            '#87',
            strawberry_playpause,
            {description = '⏯', group = '🍓 strawberry'}
        ),
        awful.key({'Mod4'}, '#85', strawberry_fwd, {description = '⏩', group = '🍓 strawberry'}),
        awful.key({'Mod4'}, '#83', strawberry_rew, {description = '⏪', group = '🍓 strawberry'}),
        awful.key({'Mod4'}, 'End', playerctl_next, {description = '⏭️', group = '🌐 global'}),
        awful.key({'Mod4'}, 'Delete', playerctl_prev, {description = '⏮', group = '🌐 global'}),
        awful.key({'Mod4'}, 'Insert', playerctl_playpause, {description = '⏯', group = '🌐 global'}),
        awful.key({'Mod4'}, 'Home', playerctl_stop, {description = '⏹️', group = '🌐 global'})
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
                {'pactl', 'set-sink-mute', '@DEFAULT_SINK@', 'true'},
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
                            local new_volume =
                                math.max(math.min(tonumber(volume) + percentage, 100), 0)
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
            {description = '🔊', group = '🎧 audio'}
        ),
        awful.key(
            {},
            'XF86AudioLowerVolume',
            function()
                increase_volume(-5)
            end,
            {description = '🔉', group = '🎧 audio'}
        ),
        awful.key(
            {},
            'XF86AudioMute',
            function()
                increase_volume()
            end,
            {description = '🔇', group = '🎧 audio'}
        )
    )
else
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key(
            {'Mod4'},
            'KP_Add',
            function()
                increase_volume(5)
            end,
            {description = '🔊', group = '🎧 audio'}
        ),
        awful.key(
            {'Mod4'},
            'KP_Subtract',
            function()
                increase_volume(-5)
            end,
            {description = '🔉', group = '🎧 audio'}
        ),
        awful.key(
            {'Mod4'},
            'KP_Insert',
            function()
                increase_volume()
            end,
            {description = '🔇', group = '🎧 audio'}
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
        brightness =
            math.min(math.max(brightness + percentage * max_brightness // 100, 0), max_brightness)
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
            {description = '🔅', group = '⌨️ keyboard'}
        ),
        awful.key(
            {},
            'XF86KbdBrightnessUp',
            function()
                increase_keyboard_light(5)
            end,
            {description = '🔆', group = '⌨️ keyboard'}
        )
    )
end

if gears.filesystem.file_readable '/sys/class/backlight/intel_backlight/max_brightness' then
    local f = io.open('/sys/class/backlight/intel_backlight/max_brightness', 'r')
    local max_brightness = f:read 'n'
    f:close()
    local increase_keyboard_light =
        increase_light_curry(
        '/sys/class/backlight/intel_backlight/brightness',
        max_brightness,
        '🖥️'
    )
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
        {'Mod4'},
        1,
        function(c)
            c:emit_signal('request::activate', 'mouse_click', {raise = true})
            awful.mouse.client.move(c)
        end
    ),
    awful.button(
        {'Mod4'},
        3,
        function(c)
            c:emit_signal('request::activate', 'mouse_click', {raise = true})
            awful.mouse.client.resize(c)
        end
    )
)

root.keys(globalkeys)

awful.rules.rules = {
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
            placement = awful.placement.no_overlap + awful.placement.no_offscreen,
            size_hints_honor = true,
            tag = function()
                return awful.screen.focused().selected_tag
            end
        }
    },
    {
        rule_any = {
            instance = {
                'DTA', -- Firefox addon DownThemAll.
                'copyq', -- Includes session name in class.
                'pinentry',
                'mansplain'
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
            name = {
                'Event Tester', -- xev.
                'Steam - News (1 of 1)',
                'Steam - News (1 of 2)',
                'Steam - News (1 of 3)',
                'Steam - News (1 of 4)',
                'Steam - News (1 of 5)',
                'Steam - News (1 of 6)',
                'Steam - News (1 of 7)',
                'Steam - News (1 of 8)'
            },
            role = {
                'AlarmWindow', -- Thunderbird's calendar.
                'ConfigManager', -- Thunderbird's about:config.
                'Organizer', -- Firefox Bookmark organizer
                'pop-up', -- e.g. Google Chrome's (detached) Developer Tools.
                -- 'bubble', -- Vivaldi's "cast..." menu
                'page-info' -- Firefox page-info
            }
        },
        properties = {floating = true}
    },
    {
        rule = {instance = 'crx_cinhimbnkkaeohfgghhklpknlkffjgod'}, -- YouTube Music
        properties = {
            floating = false,
            tag = '🍓'
        }
    },
    {
        rule = {instance = 'Journalctl'},
        properties = {
            tag = '🧻',
            maximized = true,
            new_tag = {
                name = '🧻',
                layout = awful.layout.suit.max,
                volatile = true,
                selected = true
            }
        }
    },
    {
        rule = {instance = 'nnn1'},
        properties = {
            tag = '🧞‍♂️',
            new_tag = {
                name = '🧞‍♂️',
                layout = awful.layout.suit.tile,
                volatile = true,
                selected = true
            }
        }
    },
    {
        rule = {instance = 'nnn2'},
        properties = {
            tag = '🧞‍♀️',
            new_tag = {
                name = '🧞‍♀️',
                layout = awful.layout.suit.tile,
                volatile = true,
                selected = true
            }
        }
    },
    {
        rule = {instance = 'nnn3'},
        properties = {
            tag = '🧞',
            new_tag = {
                name = '🧞',
                layout = awful.layout.suit.tile,
                volatile = true,
                selected = true
            }
        }
    },
    {
        rule = {class = 'mpv', instance = 'mpvk'},
        properties = {
            tag = '🍿',
            new_tag = {
                name = '🍿',
                layout = awful.layout.suit.tile,
                volatile = true,
                selected = true
            },
            sticky = false,
            focused = false
        }
    },
    {
        rule = {role = 'PictureInPicture'},
        properties = {
            tag = '🍿',
            new_tag = {
                name = '🍿',
                layout = awful.layout.suit.tile,
                volatile = true,
                selected = true
            },
            sticky = false,
            floating = false,
            focused = false
        }
    },
    {
        rule = {class = 'mpv', instance = 'FM0'},
        properties = {
            new_tag = {
                name = '📻',
                layout = awful.layout.suit.max,
                volatile = true,
                selected = true
            },
            tag = '📻'
        }
    },
    {
        rule = {class = 'Steam', instance = 'Steam'},
        properties = {
            tag = '🎲'
        }
    },
    {
        rule = {class = 'strawberry', instance = 'strawberry'},
        properties = {
            tag = '🍓'
        }
    },
    {
        rule = {class = 'SshAskpass'},
        properties = {
            ontop = true
        }
    },
    {
        rule = {role = 'GtkFileChooserDialog'},
        properties = {
            height = dpi(600),
            width = dpi(860),
            ontop = true,
            x = dpi(210),
            y = dpi(100)
        }
    },
    {
        rule = {class = 'Pavucontrol'},
        properties = {
            height = dpi(600),
            width = dpi(860),
            ontop = true,
            x = dpi(210),
            y = dpi(100),
            floating = true
        }
    },
    {
        rule = {instance = 'mansplain'},
        properties = {
            x = dpi(280),
            y = dpi(100)
        }
    },
    {
        rule = {
            instance = 'code',
            class = 'Code'
        },
        properties = {
            tag = '☕',
            focus = true,
            callback = function(c)
                c:jump_to(false)
            end
        }
    },
    {
        rule = {
            name = 'MuPDF: .*'
        },
        properties = {
            tag = '🧠',
            focus = true,
            sticky = true
            --[[
            callback = function(c)
                c:jump_to(true)
            end
            --]]
        }
    },
    {
        rule = {class = 'Google-chrome'},
        properties = {
            keys = gears.table.join(
                clientkeys,
                awful.key(
                    {'Control'},
                    'q',
                    _,
                    function()
                        awful.key.execute({'Mod1'}, 'F4')
                    end
                )
            )
        }
    },
    {
        rule = {class = 'Spotify'},
        properties = {
            keys = gears.table.join(
                clientkeys,
                awful.key(
                    {'Control'},
                    'q',
                    function(c)
                        c:kill()
                    end
                )
            ),
            tag = '🍓'
        }
    }
}

client.connect_signal(
    'manage',
    function(c)
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        if not awesome.startup then
            awful.client.setslave(c)
        end

        if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
            -- Prevent clients from being unreachable after screen count changes.
            awful.placement.no_offscreen(c)
        end
    end
)

client.connect_signal(
    'mouse::enter',
    function(c)
        c:emit_signal(
            'request::activate',
            'mouse_enter',
            {raise = awful.screen.focused().selected_tag.layout.name == 'floating'}
        )
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
