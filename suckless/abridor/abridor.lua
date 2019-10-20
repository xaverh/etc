#!/usr/bin/env lua

local filename = os.getenv('HOME') .. '/'
local ls_file
local ls

function open_folder(filename)
	local application_file =
		io.popen('echo "📂 nnn\n🖼️ sxiv\n♾️ Visual Studio Code\n📼 mpv" | rofi -dmenu -i -p "Open ' .. filename .. ' with…"')
	local application = application_file:read('a')
	if application == '📂 nnn\n' then
		os.execute('kitty -1 nnn "' .. filename .. '"')
	elseif application == '🖼️ sxiv\n' then
		os.execute('sxiv -t "' .. filename .. '"')
	elseif application == '♾️ Visual Studio Code\n' then
		os.execute('code -n "' .. filename .. '"')
	elseif application == '📼 mpv\n' then
		os.execute('mpv "' .. filename .. '"')
	end
end

function filter_for_pattern(str, ...)
	for _, pattern in pairs {...} do
		if string.match(str, pattern) then
			return str
		end
	end
	return nil
end

::recurse::
ls_file = io.popen('ls -1 --group-directories-first -B -p "' .. filename .. '"')
ls = {'../', './'}
for line in ls_file:lines('l') do
	table.insert(ls, line)
end

local filename_file = io.popen('echo "' .. table.concat(ls, '\n') .. '"' .. ' | rofi -dmenu -i -p "' .. filename .. '"')
local filename_new = string.sub(filename_file:read('a'), 1, -2)

if filename_new == '' then
	return
end

filename = filename .. filename_new

-- TODO: pick how to open the folder: sxiv, Visual Studio Code, nnn, mpv
if filter_for_pattern(filename, '/%./$') then
	-- os.execute('sxiv -t ' .. filename)
	open_folder(filename)
elseif filter_for_pattern(filename, '/$') then
	goto recurse
elseif filter_for_pattern(filename, '%.cbz$', '%.epub$', '%.oxps$', '%.pdf$', '%.xps$') then
	os.execute('mupdf-gl "' .. filename .. '"')
elseif
	filter_for_pattern(
		filename,
		'%.mov$',
		'%.mp4$',
		'%.mkv$',
		'%.avi$',
		'%.ogv$',
		'%.webm$',
		'%.ogg$',
		'%.mp3$',
		'%.opus$',
		'%.m4a$',
		'%.flac$',
		'%.MOV$',
		'%.MP4$',
		'%.MKV$',
		'%.AVI$',
		'%.OGV$',
		'%.WEBM$',
		'%.OGG$',
		'%.MP3$',
		'%.OPUS$',
		'%.M4A$',
		'%.FLAC$'
	)
 then
	os.execute('mpv --force-window=yes "' .. filename .. '"')
elseif
	filter_for_pattern(
		filename,
		'%.png$',
		'%.PNG$',
		'%.jpe?g$',
		'%.JPE?G$',
		'%.webp$',
		'%.WEBP$',
		'%.bmp$',
		'%.BMP$',
		'%.gif$',
		'%.GIF$',
		'%.tiff?$',
		'%.TIFF?$'
	)
 then
	os.execute('sxiv "' .. filename .. '"')
else
	os.execute('xdg-open "' .. filename .. '"')
end
