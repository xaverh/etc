modkey = "Mod4-" -- "-" is required for keybinding strings
shift = "Shift-" 
control = "Control-"
local resizestep = 0.05

local solarized = {
	base03	= "#002b36",
	base02	= "#073642",
	base01	= "#586e75",
	base00	= "#657b83",
	base0	= "#839496",
	base1	= "#93a1a1",
	base2	= "#eee8d5",
	base3	= "#fdf6e3",
	yellow	= "#b58900",
	orange	= "#cb4b16",
	red	= "#dc322f",
	magenta	= "#d33682",
	violet	= "#6c71c4",
	blue	= "#268bd2",
	cyan	= "#2aa198",
	green	= "#859900"
}

keybind = {
[modkey .. shift .. 'q'] = 'quit',
[modkey .. shift .. 'r'] = 'reload',
[modkey .. shift .. 'c'] = 'close',
[modkey .. 'Return'] = 'spawn urxvtcd',
[modkey .. 'F2'] = 'spawn dmenu_run',

-- basic movement
-- focusing clients
[modkey .. 'Left'] =  'focus left',
[modkey .. 'Down'] =  'focus down',
[modkey .. 'Up'] =    'focus up',
[modkey .. 'Right'] = 'focus right',
[modkey .. 'h'] =     'focus left',
[modkey .. 'j'] =     'focus down',
[modkey .. 'k'] =     'focus up',
[modkey .. 'l'] =     'focus right',

-- moving clients
[modkey .. shift .. 'Left'] =  'shift left',
[modkey .. shift .. 'Down'] =  'shift down',
[modkey .. shift .. 'Up'] =    'shift up',
[modkey .. shift .. 'Right'] = 'shift right',
[modkey .. shift .. 'h'] =     'shift left',
[modkey .. shift .. 'j'] =     'shift down',
[modkey .. shift .. 'k'] =     'shift up',
[modkey .. shift .. 'l'] =     'shift right',

-- splitting frames
-- create an empty frame at the specified direction
[modkey .. 'u'] =       'split horizontal 0.5',
[modkey .. 'o'] =       'split vertical 0.5',
-- let the current frame explode into subframes
[modkey .. control .. 'space'] = 'split explode',

-- resizing frames
[modkey .. control .. 'h'] =       'resize left' .. resizestep,
[modkey .. control .. 'j'] =       'resize down' .. resizestep,
[modkey .. control .. 'k'] =       'resize up' .. resizestep,
[modkey .. control .. 'l'] =       'resize right' .. resizestep,
[modkey .. control .. 'Left'] =    'resize left' .. resizestep,
[modkey .. control .. 'Down'] =    'resize down' .. resizestep,
[modkey .. control .. 'Up'] =      'resize up' .. resizestep,
[modkey .. control .. 'Right'] =   'resize right' .. resizestep,

-- cycle through tags
[modkey .. 'period'] = 'use_index +1 --skip-visible',
[modkey .. 'comma'] =  'use_index -1 --skip-visible',

-- layouting
[modkey .. 'r'] = 'remove',
[modkey .. 'space'] = 'cycle_layout 1',
[modkey .. 's'] = 'floating toggle',
[modkey .. 'f'] = 'fullscreen toggle',
[modkey .. 'p'] = 'pseudotile toggle',

-- focus
[modkey .. 'BackSpace'] =   'cycle_monitor',
[modkey .. 'Tab'] =         'cycle_all +1',
[modkey .. shift .. 'Tab'] =   'cycle_all -1',
[modkey .. 'c'] = 'cycle',
[modkey .. 'i'] = 'jumpto urgent',
}

mousebind = {
[modkey .. 'Button1'] = 'move',
[modkey .. 'Button2'] = 'zoom',
[modkey .. 'Button3'] = 'resize',
}

tags = {"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}

-- theme
attr = {
["theme.tiling.reset"] = 1,
["theme.floating.reset"] = 1,
["theme.active.color"] = solarized.violet,
["theme.normal.color"] = solarized.base01,
["theme.urgent.color"] = solarized.magenta,
["theme.inner_width"] = 1,
["theme.inner_color"] = solarized.base03,
["theme.border_width"] = 3,
["theme.floating.border_width"] = 4,
["theme.floating.outer_width"] = 1,
["theme.floating.outer_color"] = solarized.base03,
["theme.active.inner_color"] = solarized.base02,
["theme.active.outer_color"] = solarized.base02,
["theme.background_color"] = solarized.base03,
}

set = {
frame_border_active_color = '#222222',
frame_border_normal_color = '#101010',
frame_bg_normal_color = '#565656',
frame_bg_active_color = solarized.base1,
frame_border_width = 1,
always_show_frame = 1,
frame_bg_transparent = 1,
frame_transparent_width = 5,
frame_gap = 4,
window_gap = 0,
frame_padding = 0,
smart_window_surroundings = 0,
smart_frame_surroundings = 1,
mouse_recenter_gap = 0,
tree_style = "'╾│ ├└╼─┐'",
window_border_active_color = solarized.base1,
}

rule = {
-- ["class=XTerm tag=3 -- move all xterms to tag 3
[""] = "focus=on", -- normally focus new clients
-- ["focus=off -- normally do not focus new clients
-- give focus to most common terminals
-- ["class~'(.*[Rr]xvt.*|.*[Tt]erm|Konsole)'"] focus=on
["windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)'"] = "pseudotile=on",
["windowtype='_NET_WM_WINDOW_TYPE_DIALOG'"] = "focus=on",
["windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)'"] = "manage=off",
}

