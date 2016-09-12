-- Configuration
local tag_here_focused = { before = "#", after = "", color = "#81D8D0", show = true}
local tag_here_notfocused = { before = "", after = "", color = "#608b4e", show = true}
local tag_nothere_focused = { before = "", after = "", color = "#608b4e", show = true}
local tag_nothere_notfocused = { before = "", after = "", color = "#608b4e", show = true}
local tag_empty = { before = "", after = "", color = "#608b4e", show = false }
local tag_not_empty = {before = " ", after = "", color = false, show = true}
local tag_urgent = {before = "", after = "", color = "#f89290", show = true}

-- Items
local workspaces = {}

function split(inputstr, sep)
	local t={}
	for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
		table.insert(t,str)
	end
	return t
end

function combine_markup(tag, tag_settings)
	if not tag_settings.show then
		return ""
	end
	if tag_settings.color then
		return "<span foreground='" ..  tag_settings.color .. "'>" .. tag_settings.before .. tag .. tag_settings.after .. "</span>"
	else
		return tag_settings.before .. tag .. tag_settings.after
	end
end

while true do
	local input = io.stdin:read'*l'
	if string.match(input, "tag_changed") then
		local f = assert(io.popen("herbstclient tag_status", "r"))
		workspaces = split(string.gsub(assert(f:read('*a')), "[\n\r]+", ""), "\t")
		for k, v in pairs(workspaces) do
			if string.match(v, "#") then
				workspaces[k] = combine_markup(string.gsub(v, "#", ''), tag_here_focused)
			end
			if string.match(v, "%.") then
				workspaces[k] = combine_markup(string.gsub(v, "%.", ''), tag_empty)
			end
			if string.match(v, ":") then
				workspaces[k] = combine_markup(string.gsub(v, ":", ""), tag_not_empty)
			end
			if string.match(v, "!") then
				workspaces[k] = combine_markup(string.gsub(v, "!", ""), tag_urgent)
			end
			if string.match(v, "+") then
				workspaces[k] = combine_markup(string.gsub(v, "+", ""), tag_here_notfocused)
			end
			-- TODO: Escaping needed?
			if string.match(v, "%%") then
				workspaces[k] = combine_markup(string.gsub(v, "%%", ""), tag_nothere_focused)
			end
			if string.match(v, "-") then
				workspaces[k] = combine_markup(string.gsub(v, "-", ""), tag_nothere_notfocused)
			end
		end
		-- iterate reverse to not skip after a removal
		for i = #workspaces, 1, -1 do
			if workspaces[i] == "" or workspaces[i] == " " then
				table.remove(workspaces, i)
			end
		end
	end
	print ('<span foreground="#d4d4d4">' .. table.concat(workspaces, " ") .. " </span>")
end

