#!/usr/bin/env lua

local filename = os.getenv('HOME') .. '/'
local ls_file
local ls
local docpats = {'%.cbz$', '%.epub$', '%.oxps$', '%.pdf$', '%.xps$'}
local dirpat = '/$'

function filter_for_pattern(str, ...)
	for _, pattern in pairs {...} do
		if string.match(str, pattern) then
			return str
		end
	end
	return nil
end

::recurse::

ls_file = io.popen('ls -1 --group-directories-first -B -p -X ' .. filename)
ls = {'../', './'}

for line in ls_file:lines('l') do
	local match = filter_for_pattern(line, dirpat, table.unpack(docpats))
	if match then
		table.insert(ls, match)
	end
end

local filename_file = io.popen('echo "' .. table.concat(ls, '\n') .. '"' .. ' | rofi -dmenu -i')
filename = filename .. string.sub(filename_file:read('a'), 1, -2)

if filter_for_pattern(filename, dirpat) then
	goto recurse
elseif filter_for_pattern(filename, table.unpack(docpats)) then
	os.execute('mupdf-gl "' .. filename .. '"')
end
