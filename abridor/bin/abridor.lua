#!/usr/bin/env lua

local filename = os.getenv('HOME') .. '/'
local ls_file
local ls

function open_folder(filename)
	local application_file =
		io.popen(
		'echo "📂 nnn\n🖼️ feh\n♾️ Visual Studio Code\n📼 mpv\n🍓 Strawberry" | rofi -dmenu -i -p "Open ' ..
			filename .. ' with…"'
	)
	local application = application_file:read('a')
	if application == '📂 nnn\n' then
		os.execute('kitty -1 nnn "' .. filename .. '"')
	elseif application == '🖼️ feh\n' then
		os.execute('feh -t. "' .. filename .. '"')
	elseif application == '♾️ Visual Studio Code\n' then
		os.execute('code -n "' .. filename .. '"')
	elseif application == '📼 mpv\n' then
		os.execute('mpv "' .. filename .. '"')
	elseif application == '🍓 Strawberry\n' then
		os.execute('strawberry --append "' .. filename .. '"')
		os.execute('strawberry --play')
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
ls_file = io.popen('\\ls -1 --group-directories-first -B -p "' .. filename .. '"')
ls = {'../', './'}
local ARG_MAX = 100000 -- echo crashes if list of args is too long
local arg_length = 7 -- ../\n./\n, so far
for line in ls_file:lines('l') do
	arg_length = arg_length + #line
	if arg_length < ARG_MAX then
		table.insert(ls, line)
	else
		break
	end
end

local filename_file = io.popen('echo "' .. table.concat(ls, '\n') .. '"' .. ' | rofi -dmenu -l 10 -i -p "' .. filename .. '"')
local filename_new = string.sub(filename_file:read('a'), 1, -2)

if filename_new == '' then
	return
end

filename = filename .. filename_new

if filter_for_pattern(filename, '/%./$') then
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
		'%.m3u$',
		'%.m3u8$',
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
		'%.FLAC$',
		'%.m3u$',
		'%.M3U8$'
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
	os.execute('feh -.ke "' .. filename .. '"')
elseif filter_for_pattern(filename, '%.sent$') then
	os.execute('sent "' .. filename .. '"')
else
	os.execute('xdg-open "' .. filename .. '"')
end
