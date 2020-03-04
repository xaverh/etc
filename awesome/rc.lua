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

local colors = {
    qi = {
        '#1e1e1e', -- Grey 10%
        '#e32791', -- Deep Cerise
        '#30c798', -- Shamrock
        '#e3c472', -- Chenin
        '#6796e6', -- Cornflower Blue
        '#e59fdf', -- Plum
        '#81d8d0', -- Riptide
        '#999999', -- Grey 60%
        '#515151', -- Grey 30%
        '#e466ad', -- Hot Pink
        '#6cd1b2', -- Medium Aquamarine
        '#e4cf98', -- Double Colonial White
        '#91b0e6', -- Jordy Blue
        '#e5b6e1', -- French Lilac
        '#a2dcd7', -- Sinbad
        '#e5e6e6', -- Grey 90%
        -- TODO
        cursor = '#20bbfc', -- Deep Sky Blue
        selbg = '#0f3a4b' -- Cyprus
        -- selbg = '#522900' -- Baker's Chocolate
    },
    ys = {
        '#f9f8f4', -- Floral White
        '#e32791', -- Deep Cerise
        '#488432', -- La Palma
        '#a25d0e', -- Golden Brown
        '#2c65b5', -- Cerulean Blue
        '#b062a7', -- Violet Blue
        '#27bbbe', -- Light Sea Green
        '#999999', -- Grey 60%
        '#b8b8b8', -- Grey 70%
        '#9f1b66', -- Jazzberry Jam
        '#325d23', -- Parsley
        '#71410a', -- Raw Umber
        '#1f477f', -- Bahama Blue
        '#7b4474', -- Eminence
        '#1b8486', -- Atoll
        '#424242', -- Grey 20%
        -- TODO
        selbg = '#add6ff', -- Pale Cornflower Blue
        cursor = '#553a63' -- Honey Flower aka Love Symbol #2
    }
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

local my_theme = 'qi'
-- if (os.date('*t').hour + 16) % 24 >= 12 then
-- my_theme = 'qi'
-- else
-- my_theme = 'ys'
-- end

beautiful.init {
    font = '.SF Compact Display 11',
    hotkeys_font = '.SF Compact Display 11',
    hotkeys_description_font = '.SF Compact Display 11',
    tasklist_font_minimized = '.SF Compact Display Italic 11',
    tasklist_font_focus = '.SF Compact Display Bold 11',
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
    maximized_hide_border = true,
    useless_gap = dpi(0),
    border_width = dpi(2),
    menu_submenu_icon = gears.filesystem.get_themes_dir() .. 'default/submenu.png',
    menu_height = dpi(20),
    menu_width = dpi(150),
    master_width_factor = 0.55,
    layout_txt = {
        tile = '👈🏻',
        tileleft = '👉🏻',
        fairv = '🤙🏻',
        floating = '🖖🏻',
        magnifier = '🤏🏻',
        max = '👊🏻'
    },
    tasklist_disable_icon = true,
    tasklist_align = 'center',
    wibar_height = dpi(20),
    -- XXX waiting for random function in awesome 4.4
    -- wallpaper = os.getenv 'HOSTNAME' == 'aberystwyth' and gears.filesystem.get_xdg_data_home() .. 'Tapet/1280x800/tapet_2020-02-26_19-57-31_856_1280x800.png' or os.getenv 'HOME' .. '/var/7015773-girl-bus-mood.jpg'
    wallpaper = os.getenv 'HOSTNAME' == 'aberystwyth' and os.getenv 'HOME' .. '/var/BingWallpaper-2020-03-02.jpg' or
        os.getenv 'HOME' .. '/var/7015773-girl-bus-mood.jpg'
}

-- TODO: notification borders

beautiful.taglist_squares_sel = theme_assets.taglist_squares_sel(dpi(5), beautiful.fg_normal)
-- beautiful.taglist_squares_unsel = theme_assets.taglist_squares_unsel(dpi(5), beautiful.fg_normal)
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

local printkey = 'Print'
if is_macintosh then
    printkey = 'XF86LaunchA'
end

awful.layout.layouts = {
    awful.layout.suit.tile,
    -- awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    awful.layout.suit.magnifier,
    awful.layout.suit.floating,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
    awful.layout.suit.tile.left
}

local myawesomemenu = {
    {
        'hotkeys',
        function()
            hotkeys_popup.show_help(nil, awful.screen.focused())
        end
    }
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
        end
    },
    {'suspend', 'systemctl suspend'},
    {'hibernate', 'systemctl hibernate'},
    {'reboot', 'systemctl reboot'},
    {'shutdown', 'systemctl poweroff'}
}

local mymainmenu =
    awful.menu(
    {
        items = {
            {'awesome', myawesomemenu, beautiful.awesome_icon},
            {'multimedia', mymultimediamenu},
            {'exit', myexitmenu},
            {'open terminal', 'kitty -1 --listen-on unix:@mykitty'}
        }
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
    local txt_l = beautiful.layout_txt[awful.layout.getname(awful.layout.get(s))] or ''
    s.mylayoutbox:set_text(txt_l)
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal('property::geometry', set_wallpaper)

local default_tags = {'🏄🏽‍♀️', '☕', '🏖️', '🧠', '👾', '🎲', '🎰', '🎱', '🧟‍♂️', '🏝️', '🏜️', '🦄'} --     🧜🏻‍♀️ '💤',🥑'
awful.screen.connect_for_each_screen(
    function(s)
        -- Wallpaper
        set_wallpaper(s)

        if s.index == 1 then
            awful.tag({table.unpack(default_tags, 1, 9)}, s, awful.layout.layouts[1])
        elseif s.index == 2 then
            awful.tag({table.unpack(default_tags, 10, 11)}, s, awful.layout.layouts[1])
        else
            awful.tag({table.unpack(default_tags, 12)}, s, awful.layout.layouts[1])
        end

        -- Create a promptbox for each screen
        s.mypromptbox = awful.widget.prompt()
        -- Textual layoutbox
        s.mylayoutbox = wibox.widget.textbox(beautiful.layout_txt[awful.layout.getname(awful.layout.get(s))])
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
            filter = awful.widget.taglist.filter.selected,
            buttons = taglist_buttons
        }

        s.mytasklist =
            awful.widget.tasklist {
            screen = s,
            filter = awful.widget.tasklist.filter.currenttags,
            buttons = tasklist_buttons
        }

        -- Create the wibox
        s.mywibox = awful.wibar {position = 'top', screen = s}

        s.mywibox:setup {
            layout = wibox.layout.align.horizontal,
            {
                layout = wibox.layout.fixed.horizontal,
                s.mytaglist,
                s.mylayoutbox,
                s.mypromptbox
            },
            s.mytasklist,
            {
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
        screen[s]:connect_signal(
            'arrange',
            function(s)
                for _, c in pairs(s.clients) do
                    if
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
    beautiful.taglist_squares_sel = theme_assets.taglist_squares_sel(dpi(5), beautiful.fg_normal)
    -- beautiful.taglist_squares_unsel = theme_assets.taglist_squares_unsel(dpi(5), beautiful.fg_normal)
    beautiful.awesome_icon = theme_assets.awesome_icon(beautiful.menu_height, beautiful.bg_focus, beautiful.fg_focus)
    if my_theme == 'ys' then
        for k, v in pairs(beautiful.layout_txt) do
            beautiful.layout_txt[k] = string.gsub(v, '\u{1F3FB}', '\u{1F3FF}', 1)
        end
    elseif my_theme == 'qi' then
        for k, v in pairs(beautiful.layout_txt) do
            beautiful.layout_txt[k] = string.gsub(v, '\u{1F3FF}', '\u{1F3FB}', 1)
        end
    end
    for s in screen do
        s.mywibox.bg = colors[my_theme][1]
        s.mywibox.fg = colors[my_theme][16]
        client.focus:emit_signal('focus')
        for _, c in ipairs(s.clients) do
            if c ~= client.focus then
                c.border_color = beautiful.border_normal
            end
        end
        update_txt_layoutbox(s)
    end
    -- Kitty
    awful.spawn {
        'kitty',
        '@',
        '--to',
        'unix:@mykitty',
        'set-colors',
        '-a',
        '-c',
        'foreground=' .. colors[my_theme][16],
        'background=' .. colors[my_theme][1],
        'color0=' .. colors[my_theme][1],
        'color1=' .. colors[my_theme][2],
        'color2=' .. colors[my_theme][3],
        'color3=' .. colors[my_theme][4],
        'color4=' .. colors[my_theme][5],
        'color5=' .. colors[my_theme][6],
        'color6=' .. colors[my_theme][7],
        'color7=' .. colors[my_theme][8],
        'color8=' .. colors[my_theme][9],
        'color9=' .. colors[my_theme][10],
        'color10=' .. colors[my_theme][11],
        'color11=' .. colors[my_theme][12],
        'color12=' .. colors[my_theme][13],
        'color13=' .. colors[my_theme][14],
        'color14=' .. colors[my_theme][15],
        'color15=' .. colors[my_theme][16],
        'cursor=' .. colors[my_theme].cursor,
        'selection_background=' .. colors[my_theme].selbg
    }
    file = io.open(gears.filesystem.get_xdg_config_home() .. 'kitty/kitty.conf', 'r')
    content = file:read 'a'
    file:close()
    file = io.open(gears.filesystem.get_xdg_config_home() .. 'kitty/kitty.conf', 'w')
    if my_theme == 'qi' then
        for i, v in ipairs(colors.ys) do
            content = string.gsub(content, v, colors.qi[i])
        end
    elseif my_theme == 'ys' then
        for i, v in ipairs(colors.qi) do
            content = string.gsub(content, v, colors.ys[i])
        end
    end
    file:write(content)
    file:close()
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
    -- X11
    awful.spawn {
        'xrdb',
        '-override',
        gears.filesystem.get_xdg_config_home() .. (is_ysgrifennwr and 'Xresources-qillqaq' or 'Xresources-ysgrifennwr')
    }
    my_theme = my_theme == 'qi' and 'ys' or 'qi'
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

-- TODO: error handling
local function mansplain()
    awful.spawn.with_shell [[zathura =(man -Tps $(apropos . | rofi -dmenu -i -p mansplain | awk '{gsub(/[()]/,""); print $2" "$1}'))]]
end

local function emoji()
    awful.spawn.with_shell [=[awk 'BEGIN {FS="# "} /;[[:blank:]]fully-qualified/ { sub(" E[[:digit:]]*.[[:digit:]]* ", "\t", $2); print $2 }' /usr/share/unicode/emoji/emoji-test.txt | rofi -dmenu -i -p '¯\_(ツ)_/¯' -no-custom | awk '{printf $1}' | xsel -ib]=]
end

local function open_url()
    awful.spawn.with_shell [=[xdg-open $(\ls -1Qt ${CM_DIR}/clipmenu.5.${USER}/*\ * | xargs awk 1 | grep --only-matching --perl-regexp "http(s?):\/\/[^ \"\(\)\<\>\]]*" | uniq | rofi -dmenu -i -p 'open URL')]=]
end

globalkeys =
    gears.table.join(
    awful.key({'Mod4'}, 'F1', mansplain, {description = 'show help', group = '🚀 launcher'}),
    awful.key({'Mod4'}, 'e', emoji, {description = [[¯\_(ツ)_/¯]], group = '🌐 global'}),
    awful.key({'Mod4'}, 'u', open_url, {description = 'open URL', group = '📋 clipboard'}),
    awful.key(
        {'Mod4'},
        'i',
        function()
            awful.spawn {'clipmenu', '-p', 'clipboard'}
        end,
        {description = 'clipmenu', group = '📋 clipboard'}
    ),
    awful.key({'Mod4'}, 'F2', hotkeys_popup.show_help, {description = 'show help', group = '🌐 global'}),
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
    awful.key({'Mod4'}, 'Prior', awful.tag.viewprev, {description = 'view previous', group = '🏷️ tag'}),
    awful.key({'Mod4'}, 'Next', awful.tag.viewnext, {description = 'view next', group = '🏷️ tag'}),
    awful.key({'Mod4'}, '#49', awful.tag.history.restore, {description = 'go back', group = '🏷️ tag'}),
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
        'h',
        function()
            awful.client.focus.global_bydirection 'left'
        end,
        {description = 'focus client to the left', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'k',
        function()
            awful.client.focus.global_bydirection 'up'
        end,
        {description = 'focus client above', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'j',
        function()
            awful.client.focus.global_bydirection 'down'
        end,
        {description = 'focus client below', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'l',
        function()
            awful.client.focus.global_bydirection 'right'
        end,
        {description = 'focus client to the right', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'h',
        function()
            awful.client.swap.global_bydirection 'left'
        end,
        {description = 'swap client with client to the left', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'k',
        function()
            awful.client.swap.global_bydirection 'up'
        end,
        {description = 'swap client with client above', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'j',
        function()
            awful.client.swap.global_bydirection 'down'
        end,
        {description = 'swap client with client below', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'l',
        function()
            awful.client.swap.global_bydirection 'right'
        end,
        {description = 'swap client with client to the right', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'w',
        function()
            mymainmenu:show()
        end,
        {description = 'show main menu', group = '🌐 global'}
    ),
    awful.key(
        {'Mod4'},
        '#47',
        function()
            awful.screen.focus_relative(1)
        end,
        {description = 'focus the next screen', group = '🖥️ screen'}
    ),
    -- awful.key({'Mod4'}, 'u', awful.client.urgent.jumpto, {description = 'jump to urgent client', group = '🎮 client'}),
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
        {'Mod4'},
        'Return',
        function()
            awful.spawn {'kitty', '-1', '--listen-on', 'unix:@mykitty'}
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
        {description = 'FM0', group = '🚀 launcher'}
    ),
    awful.key(
        {'Mod4'},
        'n',
        function()
            for c in awful.client.iterate(
                function(c)
                    return awful.rules.match(c, {instance = 'Nnn', class = 'kitty'})
                end
            ) do
                if client.focus == c then
                    awful.tag.viewtoggle(awful.tag.find_by_name(nil, '🧞‍♀️'))
                    return
                else
                    c:jump_to(true)
                    return
                end
            end
            awful.spawn {'kitty', '-1', '--listen-on', 'unix:@mykitty', '--title', 'Nnn', '--name', 'Nnn', 'nnn'}
        end,
        {description = 'open nnn', group = '🚀 launcher'}
    ),
    awful.key(
        {'Mod4'},
        'F8',
        function()
            for c in awful.client.iterate(
                function(c)
                    return awful.rules.match(c, {instance = 'strawberry', class = 'strawberry'})
                end
            ) do
                if client.focus == c then
                    awful.tag.viewtoggle(awful.tag.find_by_name(nil, '🍓'))
                    return
                else
                    c:jump_to(true)
                    return
                end
            end
            awful.spawn 'strawberry'
        end,
        {description = '🍓', group = '🚀 launcher'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'F8',
        function()
            awful.tag.find_by_name(nil, '🍓'):view_only()
        end,
        {
            description = 'toggle 🍓',
            group = '🏷️ tag'
        }
    ),
    awful.key(
        {'Mod4'},
        'F12',
        function()
            for c in awful.client.iterate(
                function(c)
                    return awful.rules.match(c, {instance = 'Journalctl', class = 'kitty'})
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
            awful.spawn {
                'kitty',
                '-1',
                '--listen-on',
                'unix:@mykitty',
                '--title',
                'Journalctl',
                '--name',
                'Journalctl',
                'journalctl',
                '-b',
                '-f',
                '-n',
                '1000'
            }
        end,
        {description = 'open nnn', group = '🚀 launcher'}
    ),
    awful.key({'Mod4', 'Control'}, 'r', awesome.restart, {description = 'reload awesome', group = '🌐 global'}),
    awful.key({'Mod4', 'Shift'}, 'q', awesome.quit, {description = 'quit awesome', group = '🌐 global'}),
    awful.key(
        {'Mod4', 'Control'},
        'l',
        function()
            awful.tag.incmwfact(0.0125)
        end,
        {description = 'increase master width factor', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Control'},
        'h',
        function()
            awful.tag.incmwfact(-0.0125)
        end,
        {description = 'decrease master width factor', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Control'},
        'j',
        function()
            awful.tag.incnmaster(1, nil, true)
        end,
        {description = 'increase the number of master clients', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Control'},
        'k',
        function()
            awful.tag.incnmaster(-1, nil, true)
        end,
        {description = 'decrease the number of master clients', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Control', 'Shift'},
        'j',
        function()
            awful.tag.incncol(1, nil, true)
        end,
        {description = 'increase the number of columns', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Control', 'Shift'},
        'k',
        function()
            awful.tag.incncol(-1, nil, true)
        end,
        {description = 'decrease the number of columns', group = '💠 layout'}
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
        {description = 'increase the number of master clients', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Control'},
        'Up',
        function()
            awful.tag.incnmaster(-1, nil, true)
        end,
        {description = 'decrease the number of master clients', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Control', 'Shift'},
        'Down',
        function()
            awful.tag.incncol(1, nil, true)
        end,
        {description = 'increase the number of columns', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Control', 'Shift'},
        'Up',
        function()
            awful.tag.incncol(-1, nil, true)
        end,
        {description = 'decrease the number of columns', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4'},
        'BackSpace',
        function()
            awful.layout.inc(1)
        end,
        {description = 'select next', group = '💠 layout'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'BackSpace',
        function()
            awful.layout.inc(-1)
        end,
        {description = 'select previous', group = '💠 layout'}
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
    --[[awful.key(
        {'Mod4'},
        'space',
        function()
            awful.screen.focused().mypromptbox:run()
        end,
        {description = 'run prompt', group = '🚀 launcher'}
    ),
    --]]
    awful.key(
        {'Mod4'},
        'space',
        function()
            awful.spawn.easy_async {'rofi', '-combi-modi', 'window,drun,run', '-show', 'combi', '-modi', 'combi'}
        end,
        {description = 'run prompt', group = '🚀 launcher'}
    ),
    awful.key(
        {'Mod4'},
        'x',
        function()
            awful.prompt.run {
                prompt = 'Run Lua code: ',
                textbox = awful.screen.focused().mypromptbox.widget,
                exe_callback = awful.util.eval,
                history_path = awful.util.get_cache_dir() .. '/history_eval'
            }
        end,
        {description = 'lua execute prompt', group = '🌐 global'}
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
    )
)

clientkeys =
    gears.table.join(
    awful.key(
        {'Mod4', 'Shift'},
        'f',
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = 'toggle fullscreen', group = '🎮 client'}
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
        {'Mod4'},
        'f',
        function(c)
            c.floating = not c.floating
            c:raise()
        end,
        {description = 'toggle floating', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        's',
        function(c)
            c.sticky = not c.sticky
            c:raise()
        end,
        {description = 'toggle sticky', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'Return',
        function(c)
            awful.client.cycle(true)
        end,
        {description = 'cycle', group = '🎮 client'}
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
        {'Mod4'},
        't',
        function(c)
            c.ontop = not c.ontop
        end,
        {description = 'toggle keep on top', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'period',
        function(c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end,
        {description = 'minimize', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4'},
        'm',
        function(c)
            c.maximized = not c.maximized
            c:raise()
        end,
        {description = '(un)maximize', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Control'},
        'm',
        function(c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end,
        {description = '(un)maximize vertically', group = '🎮 client'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'm',
        function(c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end,
        {description = '(un)maximize horizontally', group = '🎮 client'}
    ),
    awful.key({'Mod4'}, 'F6', toggle_theme, {description = 'change theme', group = '🌐 global'}),
    -- XXX next version: https://www.reddit.com/r/awesomewm/comments/bcc73o/how_to_use_clipboard_in_awesome/
    awful.key(
        {'Mod4'},
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
        {'Mod4'},
        'b',
        function()
            beautiful.useless_gap = beautiful.useless_gap == 0 and 10 or 0
            for s in screen do
                awful.layout.arrange(s)
            end
        end,
        {description = 'toggle useless gaps', group = '💠 layout'}
    )
)

local tags_per_screen = 5
for i, v in ipairs(default_tags) do
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key(
            {'Mod4'},
            '#' .. i + 9,
            function()
                awful.tag.find_by_name(nil, v):view_only()
            end,
            {
                description = 'view ' .. v,
                group = '🏷️ tag'
            }
        ),
        -- Toggle tag display.
        awful.key(
            {'Mod4', 'Control'},
            '#' .. i + 9,
            function()
                awful.tag.viewtoggle(awful.tag.find_by_name(nil, v))
            end,
            {
                description = 'toggle ' .. v,
                group = '🏷️ tag'
            }
        ),
        -- Move client to tag.
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
        -- Toggle tag on focused client.
        awful.key(
            {'Mod4', 'Control', 'Shift'},
            '#' .. i + 9,
            function()
                if client.focus then
                    client.focus:toggle_tag(awful.tag.find_by_name(nil, v))
                end
            end,
            {
                description = 'toggle client on ' .. v,
                group = '🏷️ tag'
            }
        )
    )
end

globalkeys =
    gears.table.join(
    globalkeys,
    awful.key(
        {'Mod4'},
        'F9',
        connect_bluetooth,
        {description = 'connect bluetooth device (Setúbal)', group = '🎧 audio'}
    ),
    awful.key(
        {'Mod4', 'Shift'},
        'F9',
        disconnect_bluetooth,
        {description = 'disconnect bluetooth device (Setúbal)', group = '🎧 audio'}
    )
)

if has_multimedia_keys then
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key({}, 'XF86AudioNext', strawberry_next, {description = '⏭️', group = '🍓 strawberry'}),
        awful.key({}, 'XF86AudioPrev', strawberry_prev, {description = '⏮', group = '🍓 strawberry'}),
        awful.key({}, 'XF86AudioPlay', strawberry_playpause, {description = '⏯', group = '🍓 strawberry'}),
        awful.key({'Shift'}, 'XF86AudioNext', strawberry_fwd, {description = '⏩', group = '🍓 strawberry'}),
        awful.key({'Shift'}, 'XF86AudioPrev', strawberry_rew, {description = '⏪', group = '🍓 strawberry'})
    )
else
    globalkeys =
        gears.table.join(
        globalkeys,
        awful.key({'Mod4'}, '#63', strawberry_next, {description = '⏭️', group = '🍓 strawberry'}),
        awful.key({'Mod4'}, '#106', strawberry_prev, {description = '⏮', group = '🍓 strawberry'}),
        awful.key({'Mod4'}, '#87', strawberry_playpause, {description = '⏯', group = '🍓 strawberry'}),
        awful.key({'Mod4'}, '#85', strawberry_fwd, {description = '⏩', group = '🍓 strawberry'}),
        awful.key({'Mod4'}, '#83', strawberry_rew, {description = '⏪', group = '🍓 strawberry'})
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
            c:emit_signal('request::activate', 'mouse strawberry_click', {raise = true})
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
            placement = awful.placement.no_overlap + awful.placement.no_offscreen,
            size_hints_honor = true
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
        rule = {class = 'kitty', instance = 'Journalctl'},
        properties = {
            new_tag = {
                name = '🧻',
                layout = awful.layout.suit.max,
                volatile = true,
                selected = true
            },
            sticky = false
        }
    },
    {
        rule = {class = 'kitty', instance = 'Nnn'},
        properties = {
            new_tag = {
                name = '🧞‍♀️',
                volatile = true,
                selected = true
            },
            sticky = false
        }
    },
    {
        rule = {class = 'mpv', instance = 'mpvk'},
        properties = {
            new_tag = {
                name = '🍿',
                -- layout = awful.layout.suit.max.fullscreen,
                volatile = true,
                selected = true
            },
            sticky = false
        }
    },
    {
        rule = {class = 'mpv', instance = 'FM0'},
        properties = {
            new_tag = {
                name = '📻',
                layout = awful.layout.suit.magnifier,
                volatile = true,
                selected = true
            },
            sticky = false
        }
    },
    {
        rule = {class = 'strawberry', instance = 'strawberry'},
        properties = {
            new_tag = {
                name = '🍓',
                layout = awful.layout.suit.max,
                volatile = true,
                selected = true
            },
            sticky = false
        }
    }
}

-- Signals
-- Signal function to execute when a new client appears.
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
