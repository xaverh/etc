#!/usr/bin/lua

local query = io.popen("bspc query -N -n .window")
local ids = {}
for line in query:lines("l") do
	local query = io.popen("xdotool getwindowname " .. line)
	local name = query:read("l")
	local instance = name
	local i = 0
	if ids[instance] then
		i = i + 1
		instance = name .. " - " .. i
	end
	ids[instance] = line
	query:close()
end
query:close()

local dmenu_input = "/bin/sh -c 'echo \""
for k, v in pairs(ids) do
	dmenu_input = dmenu_input .. k .. '\n'
end
dmenu_input = dmenu_input .. "\" | dmenu -l 10 -p \"Focus:\"'"
print(dmenu_input)

local dmenu = io.popen(dmenu_input)
local window_title = dmenu:read("l")
dmenu:close()
os.execute("bspc node --focus " .. ids[window_title])