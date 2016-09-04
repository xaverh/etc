local current_workspace
local workspace_names = {}
local max_length = 0

function split(inputstr, sep)
	local t={}
	for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
		table.insert(t,str)
	end
	return t
end

for i = 1, 2 do
	local input = io.stdin:read'*l'
	if string.match(input, "_NET_CURRENT_DESKTOP") then
		current_workspace = string.match(input, "%d") + 1
	end
	if string.match(input, "_NET_DESKTOP_NAMES") then
		local begin
		_, begin = string.find(input, "=%s")
		workspace_names = split(string.sub(input, begin+1), '", "')
	end
end

for k, v in pairs(workspace_names) do
	if max_length < #workspace_names[k] then
		max_length = #workspace_names[k]
	end
end

for k, v in pairs(workspace_names) do
	if k == current_workspace then
		workspace_names[k] = '<span foreground="#81D8D0">' .. v .. string.rep(" ", max_length - #workspace_names[k]) .. '</span>'
	-- else
		-- workspace_names[k] = ""
		-- workspace_names[k] = '<span foreground="#FF0000">' .. v .. '</span>'
	end
end

-- print(table.concat(workspace_names, " "))
print(workspace_names[current_workspace])