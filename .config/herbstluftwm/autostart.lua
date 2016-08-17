dofile("/home/xha/.config/herbstluftwm/autostartconf.lua")
local panel = "~/.config/herbstluftwm/panel.sh" -- TODO dofile

os.execute("herbstclient emit_hook reload")

os.execute("xsetroot -solid '#002b36'")

-- remove all existing bindings
os.execute("herbstclient keyunbind --all")
os.execute("herbstclient mouseunbind --all")
os.execute("herbstclient unrule -F")

-- read conf file
for section, name in pairs{[attr] = "attr", [set] = "set", [keybind] = "keybind", [mousebind] = "mousebind", [rule] = "rule"} do
	for k, v in pairs(section) do
		os.execute(string.format("herbstclient %s %s %s", name, k, tostring(v)))
	end
end

for k, v in ipairs(tags) do
	os.execute("herbstclient add " .. tostring(v))
	os.execute("herbstclient keybind " .. modkey .. tostring(k) .. " use_index " .. tostring(k)) 
	os.execute("herbstclient keybind " .. modkey .. shift .. tostring(k) .. " move_index " .. tostring(k)) 
end

-- unlock, just to be sure
os.execute("herbstclient unlock")

os.execute("herbstclient set tree_style '╾│ ├└╼─┐'")

-- do multi monitor setup here, e.g.:
-- os.execute("herbstclient set_monitors 1280x1024+0+0 1280x1024+1280+0")
-- or simply:
-- os.execute("herbstclient detect_monitors")

-- find the panel
panel = panel or "/etc/xdg/herbstluftwm/panel.sh"
-- TODO multi monitor setup
-- for monitor in $(herbstclient list_monitors | cut -d: -f1) ; do
    -- start it on each monitor
-- "$panel" $monitor &
os.execute(panel ..
-- monitor
"&")
