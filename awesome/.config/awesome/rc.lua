-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require "gears"
local awful = require "awful"
require "awful.autofocus"
-- Widget and layout library
local wibox = require "wibox"
-- Theme handling library
local beautiful = require "beautiful"
-- Notification library
local naughty = require "naughty"
local menubar = require "menubar"
local hotkeys_popup = require "awful.hotkeys_popup"
-- Vicious
local vicious = require "vicious"
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require "awful.hotkeys_popup.keys"

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

-- Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init "/home/xha/.config/awesome/themes/ysgrifennwr/theme.lua"

-- This is used later as the default terminal and editor to run.
terminal = "urxvt-mlc"
editor = os.getenv "EDITOR" or "vi"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
	-- awful.layout.suit.floating, -- TODO: make available through shortkey only
	awful.layout.suit.tile,
	-- awful.layout.suit.tile.left,
	-- awful.layout.suit.tile.bottom,
	-- awful.layout.suit.tile.top,
	awful.layout.suit.fair,
	-- awful.layout.suit.fair.horizontal,
	-- awful.layout.suit.spiral,
	awful.layout.suit.spiral.dwindle,
	awful.layout.suit.magnifier,
	awful.layout.suit.max,
	-- awful.layout.suit.max.fullscreen, -- TODO: make availabe to shortkey only
	awful.layout.suit.corner.nw
	-- awful.layout.suit.corner.ne,
	-- awful.layout.suit.corner.sw,
	-- awful.layout.suit.corner.se,
}

-- Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
	{
		"hotkeys",
		function()
			hotkeys_popup.show_help(nil, awful.screen.focused())
		end
	},
	{"manual", terminal .. " -e man awesome"},
	{"lock screen", "slock", "/home/xha/.local/share/icons/deadpool.svg"},
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
			{"rxvt-unicode", terminal, "/usr/share/icons/gnome/22x22/apps/gnome-terminal.png"},
			{"Setúbal", "bluetoothctl connect 88:C6:26:F4:8A:90", os.getenv("HOME") .. "/.config/awesome/icons/speakers.svg"},
			{"Mozilla Firefox", "firefox", "/home/xha/.local/share/icons/firefox.svg"},
			{"Visual Studio Code", "code -n", "/usr/share/code/resources/app/resources/linux/code.png"},
			{"PCManFM", "pcmanfm", "/home/xha/.local/share/icons/file-manager.svg"},
			{"Strawberry", "strawberry", "/usr/share/icons/hicolor/scalable/apps/strawberry.svg"},
			{"MµPDF", "mupdf-gl", "/home/xha/.local/share/icons/mupdf.svg"}
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

-- Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock("%a %-d %b %T %Z ", 1)

local function nearest_si_suffix(bytes)
	local bytes = math.tointeger(bytes) or 0
	local suffix = " B"
	if bytes >= 1000000000 then
		bytes = bytes / 1000000000
		suffix = " GB"
	elseif bytes >= 1000000 then
		bytes = bytes / 1000000
		suffix = " MB"
	elseif bytes >= 1000 then
		bytes = bytes / 1000
		suffix = " kB"
	else
		return bytes .. suffix
	end
	return ("%.1f %s"):format(bytes, suffix)
end

ipwidget = wibox.widget.textbox()
vicious.register(ipwidget, vicious.widgets.ipaddr, "$1  ", 7)

temperaturewidget = wibox.widget.textbox()
vicious.register(temperaturewidget, vicious.widgets.hwmontemp, "$1 °C  ", 13, {"coretemp"})

volwidget = wibox.widget.textbox()
vicious.register(volwidget, vicious.widgets.volume, "vol. $1 $2  ", 59, {"Master", "-D", "pulse"})

memwidget = wibox.widget.textbox()
vicious.register(
	memwidget,
	vicious.widgets.mem,
	function(widget, args)
		return nearest_si_suffix(args[2] * 1048576) .. "  "
	end,
	7
)

-- Vicious net widget
netwidget = wibox.widget.textbox()
vicious.register(
	netwidget,
	vicious.widgets.net,
	function(widget, args)
		local carrier = "eno1"
		if args["{wlan0 carrier}"] then
			carrier = "wlan0"
		end
		return nearest_si_suffix(args["{" .. carrier .. " down_b}"]) ..
			"/s  " .. nearest_si_suffix(args["{" .. carrier .. " up_b}"]) .. "/s  "
	end,
	2
)

-- Vicious wifi widget
wifiwidget = wibox.widget.textbox()
vicious.register(
	wifiwidget,
	vicious.widgets.wifiiw,
	function(widget, args)
		return ("%s  %s  "):format(args["{ssid}"], string.sub(args["{bssid}"], -8))
	end,
	11,
	"wlan0"
)

batwidget = wibox.widget.textbox()
vicious.register(batwidget, vicious.widgets.bat, "$1$2%  ", 61, "BAT0")

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

local function set_wallpaper(s)
	-- Wallpaper
	if beautiful.wallpaper then
		local wallpaper = beautiful.wallpaper
		-- If wallpaper is a function, call it with the screen
		if type(wallpaper) == "function" then
			wallpaper = wallpaper(s)
		end
		gears.wallpaper.maximized(wallpaper, s, true)
	end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(
	function(s)
		-- Wallpaper
		set_wallpaper(s)

		-- Each screen has its own tag table.

		-- 🚽 💩 🧻
		awful.tag(
			{"🏄", "☕", "🏝️", "🍓", "🍿", "🦄", "🎰", "🎱", "🗞️"},
			s,
			{
				awful.layout.suit.floating,
				awful.layout.suit.floating,
				awful.layout.suit.floating,
				awful.layout.suit.corner.nw,
				awful.layout.suit.floating,
				awful.layout.suit.max.fullscreen,
				awful.layout.suit.fair,
				awful.layout.suit.spiral.dwindle,
				awful.layout.suit.max
			}
		)

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
			wibox.container.margin(
			awful.widget.tasklist {
				screen = s,
				filter = awful.widget.tasklist.filter.minimizedcurrenttags,
				buttons = tasklist_buttons,
				style = {
					tasklist_disable_icon = true,
					disable_task_name = false,
					-- shape_border_width = 1,
					-- shape_border_color = "#808080",
					-- shape = gears.shape.rectangle,
					spacing = 10
				},
				layout = {
					layout = wibox.layout.fixed.horizontal
				}
			},
			10,
			0,
			0,
			0
		)

		-- Create the horizontal wibox
		s.mywibox = awful.wibar {height = 24, position = "top", screen = s, ontop = true}

		-- Add widgets to the wibox
		s.mywibox:setup {
			layout = wibox.layout.align.horizontal,
			{
				-- Left widgets
				layout = wibox.layout.fixed.horizontal,
				mylauncher,
				s.mytaglist,
				wibox.container.margin(s.mylayoutbox, 2, 2, 2, 3),
				-- wibox.widget.systray(),
				s.mypromptbox
			},
			s.mytasklist, -- Middle widget
			{
				-- Right widgets
				layout = wibox.layout.fixed.horizontal,
				volwidget,
				memwidget,
				netwidget,
				temperaturewidget,
				wifiwidget,
				ipwidget,
				batwidget,
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
		),
		awful.button({}, 4, awful.tag.viewnext),
		awful.button({}, 5, awful.tag.viewprev)
	)
)

-- Key bindings
globalkeys =
	gears.table.join(
	awful.key({modkey}, "F1", hotkeys_popup.show_help, {description = "show help", group = "awesome"}),
	awful.key({modkey}, "w", awful.tag.viewprev, {description = "view previous", group = "tag"}),
	awful.key({modkey}, "s", awful.tag.viewnext, {description = "view next", group = "tag"}),
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
		"Tab",
		function()
			awful.client.focus.byidx(1)
		end,
		{description = "focus next by index", group = "client"}
	),
	awful.key(
		{modkey, "Shift"},
		"Tab",
		function()
			awful.client.focus.byidx(-1)
		end,
		{description = "focus previous by index", group = "client"}
	),
	awful.key(
		{modkey},
		"F9",
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
		"Return",
		function()
			awful.spawn(terminal)
		end,
		{description = "open a terminal", group = "launcher"}
	),
	awful.key(
		{modkey},
		"e",
		function()
			awful.spawn("pcmanfm")
		end,
		{description = "open a file manager", group = "launcher"}
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
		"t",
		function()
			local this_screen = awful.screen.focused()
			local current_layout = awful.layout.get(this_screen)
			if current_layout == awful.layout.suit.floating or current_layout == awful.layout.suit.max.fullscreen then
				awful.layout.set(awful.layout.suit.tile)
			else
				awful.layout.inc(1, this_screen, awful.layout.layouts)
			end
		end,
		{description = "select next", group = "layout"}
	),
	awful.key(
		{modkey, "Shift"},
		"t",
		function()
			local this_screen = awful.screen.focused()
			local current_layout = awful.layout.get(this_screen)
			if current_layout == awful.layout.suit.floating or current_layout == awful.layout.suit.max.fullscreen then
				awful.layout.set(awful.layout.suit.tile)
			else
				awful.layout.inc(-1, this_screen, awful.layout.layouts)
			end
		end,
		{description = "select previous", group = "layout"}
	),
	awful.key(
		{modkey, "Control"},
		"n",
		function()
			local c = awful.client.restore()
			-- Focus restored client
			if c then
				c:emit_signal("request::activate", "key.unminimize", {raise = true})
			end
		end,
		{description = "restore minimized", group = "client"}
	),
	-- Prompt
	awful.key(
		{modkey},
		"space",
		function()
			awful.screen.focused().mypromptbox:run()
		end,
		{description = "run prompt", group = "launcher"}
	),
	awful.key(
		{modkey},
		"x",
		function()
			awful.prompt.run {
				prompt = "Run Lua code: ",
				textbox = awful.screen.focused().mypromptbox.widget,
				exe_callback = awful.util.eval,
				history_path = awful.util.get_cache_dir() .. "/history_eval"
			}
		end,
		{description = "lua execute prompt", group = "awesome"}
	),
	-- Menubar
	awful.key(
		{modkey},
		"p",
		function()
			menubar.show()
		end,
		{description = "show the menubar", group = "launcher"}
	)
)

local last_layout = awful.layout.suit.tile

clientkeys =
	gears.table.join(
	awful.key(
		{modkey},
		"F11",
		function(c)
			c.fullscreen = not c.fullscreen
			c:raise()
		end,
		{description = "toggle fullscreen", group = "client"}
	),
	awful.key(
		{modkey},
		"f",
		function()
			local this_screen = awful.screen.focused()
			local current_layout = awful.layout.get(this_screen)
			if current_layout ~= awful.layout.suit.floating then
				last_layout = awful.layout.get(this_screen)
			end
			awful.layout.inc(1, this_screen, {awful.layout.suit.floating, last_layout})
			awful.layout.arrange(this_screen)
		end,
		{description = "toggle floating layout", group = "layout"}
	),
	awful.key(
		{modkey, "Shift"},
		"f",
		function()
			local this_screen = awful.screen.focused()
			local current_layout = awful.layout.get(this_screen)
			if current_layout ~= awful.layout.suit.max.fullscreen then
				last_layout = current_layout
			end
			awful.layout.inc(1, this_screen, {awful.layout.suit.max.fullscreen, last_layout})
			awful.layout.arrange(this_screen)
		end,
		{description = "toggle fullscreen layout", group = "layout"}
	),
	awful.key(
		{modkey},
		"q",
		function(c)
			c:kill()
		end,
		{description = "close", group = "client"}
	),
	awful.key(
		{modkey, "Control"},
		"space",
		awful.client.floating.toggle,
		{description = "toggle floating", group = "client"}
	),
	awful.key(
		{modkey, "Control"},
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
			c:move_to_screen()
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
		"n",
		function(c)
			-- The client currently has the input focus, so it cannot be
			-- minimized, since minimized clients can't have the focus.
			c.minimized = true
		end,
		{description = "minimize", group = "client"}
	),
	awful.key(
		{modkey},
		"m",
		function(c)
			c.maximized = not c.maximized
			c:raise()
		end,
		{description = "(un)maximize", group = "client"}
	),
	awful.key(
		{modkey, "Control"},
		"m",
		function(c)
			c.maximized_vertical = not c.maximized_vertical
			c:raise()
		end,
		{description = "(un)maximize vertically", group = "client"}
	),
	awful.key(
		{modkey, "Shift"},
		"m",
		function(c)
			c.maximized_horizontal = not c.maximized_horizontal
			c:raise()
		end,
		{description = "(un)maximize horizontally", group = "client"}
	),
	awful.key(
		{},
		"XF86MonBrightnessUp",
		function()
			awful.spawn "xbacklight -inc 10"
		end,
		{description = "increase brightness", group = "awesome"}
	),
	awful.key(
		{},
		"XF86MonBrightnessDown",
		function()
			awful.spawn "xbacklight -dec 10"
		end,
		{description = "decrease brightness", group = "awesome"}
	),
	awful.key(
		{},
		"XF86AudioRaiseVolume",
		function()
			awful.spawn.with_shell "amixer sset Master unmute && amixer sset Master 5%+"
		end,
		{description = "increase volume", group = "awesome"}
	),
	awful.key(
		{},
		"XF86AudioLowerVolume",
		function()
			awful.spawn.with_shell "amixer sset Master unmute && amixer sset Master 5%-"
		end,
		{description = "decrease volume", group = "awesome"}
	),
	awful.key(
		{},
		"XF86AudioMute",
		function()
			awful.spawn "amixer sset Master mute"
		end,
		{description = "mute", group = "awesome"}
	),
	awful.key(
		{},
		"XF86AudioPlay",
		function()
			awful.spawn "strawberry --play-pause"
		end,
		{description = "Toggle play/pause", group = "strawberry"}
	),
	awful.key(
		{},
		"XF86AudioPrev",
		function()
			awful.spawn "strawberry --restart-or-previous"
		end,
		{description = "Restart current song or go to previous song", group = "strawberry"}
	),
	awful.key(
		{},
		"XF86AudioNext",
		function()
			awful.spawn "strawberry --next"
		end,
		{description = "Go to next song", group = "strawberry"}
	),
	awful.key(
		{modkey},
		"Pause",
		function()
			awful.spawn "slock"
		end,
		{description = "Lock screen", group = "awesome"}
	),
	awful.key(
		{"Mod1"},
		"Tab",
		function()
			-- If you want to always position the menu on the same place set coordinates
			awful.menu.menu_keys.down = {"Tab"}
			awful.menu.clients({theme = {width = 250}}, {keygrabber = true, coords = {x = 525, y = 330}})
		end,
		{description = "switch task", group = "awesome"}
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
			"#" .. i + 9,
			function()
				local screen = awful.screen.focused()
				local tag = screen.tags[i]
				if tag then
					tag:view_only()
				end
			end,
			{description = "view tag #" .. i, group = "tag"}
		),
		-- Toggle tag display.
		awful.key(
			{modkey, "Control"},
			"#" .. i + 9,
			function()
				local screen = awful.screen.focused()
				local tag = screen.tags[i]
				if tag then
					awful.tag.viewtoggle(tag)
				end
			end,
			{description = "toggle tag #" .. i, group = "tag"}
		),
		-- Move client to tag.
		awful.key(
			{modkey, "Shift"},
			"#" .. i + 9,
			function()
				if client.focus then
					local tag = client.focus.screen.tags[i]
					if tag then
						client.focus:move_to_tag(tag)
					end
				end
			end,
			{description = "move focused client to tag #" .. i, group = "tag"}
		),
		-- Toggle tag on focused client.
		awful.key(
			{modkey, "Control", "Shift"},
			"#" .. i + 9,
			function()
				if client.focus then
					local tag = client.focus.screen.tags[i]
					if tag then
						client.focus:toggle_tag(tag)
					end
				end
			end,
			{description = "toggle focused client on tag #" .. i, group = "tag"}
		)
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

-- Rules
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
				"DTA", -- Firefox addon DownThemAll.
				"copyq", -- Includes session name in class.
				"pinentry"
			},
			class = {
				"Arandr",
				"Blueman-manager",
				"Gpick",
				"Kruler",
				"MessageWin", -- kalarm.
				"Sxiv",
				"Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
				"Wpa_gui",
				"veromix",
				"xtightvncviewer"
			},
			-- Note that the name property shown in xprop might be set slightly after creation of the client
			-- and the name shown there might not match defined rules here.
			name = {
				"Event Tester" -- xev.
			},
			role = {
				"AlarmWindow", -- Thunderbird's calendar.
				"ConfigManager", -- Thunderbird's about:config.
				"pop-up" -- e.g. Google Chrome's (detached) Developer Tools.
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
	{
		rule = {instance = "strawberry", class = "strawberry"},
		properties = {tag = "🍓"}
	}
}

-- Signals
-- Signal function to execute when a new client appears.
client.connect_signal(
	"manage",
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

		awful.titlebar(c, {size = 20}):setup {
			{
				-- Left
				wibox.container.margin(awful.titlebar.widget.floatingbutton(c), 2, 2, 2, 3),
				wibox.container.margin(awful.titlebar.widget.ontopbutton(c), 2, 2, 2, 3),
				wibox.container.margin(awful.titlebar.widget.stickybutton(c), 0, 0, 0, 4),
				layout = wibox.layout.fixed.horizontal
			},
			{
				-- Middle
				{
					-- Title
					align = "center",
					widget = awful.titlebar.widget.titlewidget(c),
					font = ".Helvetica Neue DeskInterface 9"
				},
				buttons = buttons,
				layout = wibox.layout.flex.horizontal
			},
			{
				-- Right
				awful.titlebar.widget.minimizebutton(c),
				awful.titlebar.widget.maximizedbutton(c),
				awful.titlebar.widget.closebutton(c),
				layout = wibox.layout.fixed.horizontal()
			},
			layout = wibox.layout.align.horizontal
		}

		awful.titlebar(c, {size = 3, position = "bottom"}):setup {
			{
				draw = draw_bottom_left,
				fit = bottom_corner_fit,
				widget = wibox.widget.base.make_widget,
				buttons = awful.util.table.join(
					awful.button(
						{},
						1,
						function(geometry)
							awful.mouse.client.resize(c)
						end
					)
				)
			},
			{
				draw = beautiful.titlebar_bottom_draw or draw_3_dots,
				fit = function(self, w, h)
					return w, h
				end,
				widget = wibox.widget.base.make_widget,
				buttons = awful.util.table.join(
					awful.button(
						{},
						1,
						function(geometry)
							c:raise()
							awful.mouse.client.resize(c)
						end
					)
				)
			},
			{
				draw = draw_bottom_right,
				fit = bottom_corner_fit,
				widget = wibox.widget.base.make_widget,
				buttons = awful.util.table.join(
					awful.button(
						{},
						1,
						function(geometry)
							awful.mouse.client.resize(c)
						end
					)
				)
			},
			id = "main_layout",
			layout = wibox.layout.align.horizontal
		}
	end
)

awful.titlebar.enable_tooltip = false

-- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal("mouse::enter", function(c)
-- c:emit_signal("request::activate", "mouse_enter", {raise = false})
-- end)

client.connect_signal(
	"focus",
	function(c)
		c.border_color = beautiful.border_focus
	end
)
client.connect_signal(
	"unfocus",
	function(c)
		c.border_color = beautiful.border_normal
	end
)
