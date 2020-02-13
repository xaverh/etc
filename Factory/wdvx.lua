#!/usr/bin/env lua
local website = "http://www.wdvx.com/live-shows-schedule/the-blue-plate-special/"
local filename = "/tmp/wdvx" .. os.date("%Y%m%d-%H%M%S") .. ".html"
local months = { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" }
local month = tonumber(os.date("%m"))
local year = tonumber(os.date("%Y"))
local mp3_folder = "/home/xha/Musik/WDVX/"
local settings_file = "/home/xha/Musik/WDVX/settings.lua"
dofile(settings_file)

function get_html (website)
	return os.execute("wget " .. website .. " -O " .. filename)
end

function file_to_lines (filename)
	io.input(assert(io.open(filename, "r")))
	local lines = {}
	for line in io.lines() do
		lines[#lines + 1] = line
	end
	io.input():close()
	return lines
end

function get_concerts (strings, month, year)
	local concerts = {}
	for _, v in pairs(strings) do
		local date = string.match(v, [[<li>]] .. months[month] .. "%s*%d+")
		if date then
			local day = tonumber(date:sub(5):match("%d+"))
			local title = v:gsub("<.->", ""):gsub("&amp;", "&"):gsub("&#038;", "&"):gsub("/", ","):gsub("&#8216;","'"):gsub("&#8217;", "'"):gsub("&#8211;", "-"):gsub("&#8221;", '”'):gsub("%s*,%s*", ", "):gsub(date:sub(5) .. "%s*:%s*", ""):gsub("^%s*(.-)%s*$", "%1"):gsub(":", "_ ")
			local d = year * 10000 + month * 100 + day
			concerts[d] = { title = title }
		end
	end
	return concerts
end

function serialize (o)
	if type(o) == "number" then
		io.write(o)
	elseif type(o) == "boolean" then
		io.write(o and "true" or "false")
	elseif type(o) == "string" then
		io.write(string.format("%q", o))
	elseif type(o) == "table" then
		io.write("{\n")
		for k,v in pairs(o) do
			io.write " ["; serialize(k); io.write "] = "
			serialize(v)
			io.write ",\n"
		end
		io.write("}\n")
	else
		error("cannot serialize a " .. type(o))
	end
end

function get_mp3 (date, filename)
	return os.execute("wget http://s3-us-west-2.amazonaws.com/playlistcenter/audio/" .. date .. [[1200-01.mp3 http://s3-us-west-2.amazonaws.com/playlistcenter/audio/]] .. date .. [[1300-01.mp3 -O "]] .. filename .. '"')
end

if get_html(website) then
	local this_month = get_concerts(file_to_lines(filename), month, year)
	local next_month = get_concerts(file_to_lines(filename),
		month+1 ~= 13 and month+1 or 1, year+math.floor(month/12))
	for k, v in pairs(this_month) do
		if concerts[k] and not concerts[k]["downloaded"] then
			concerts[k] = v
		end
	end
	for k, v in pairs(next_month) do
--		if not concerts[k]["downloaded"] then
			concerts[k] = v
--		end
	end
	for date = tonumber(os.date("%Y%m%d")), tonumber(os.date("%Y%m01")), -1 do
		if concerts[date] and not concerts[date]["downloaded"] then
			local downloaded = get_mp3(date, mp3_folder .. date .. " " .. concerts[date]["title"] .. " " .. os.date("%H%M%S") .. ".mp3")
			concerts[date]["downloaded"] = downloaded
		end
	end
	local last_months_year = (month == 1) and (year - 1) or year
	local last_month = (month == 1) and 12 or month - 1
	for date = last_months_year * 10000 + last_month * 100 + 31, last_months_year*10000 + last_month * 100 + 1, -1 do
		if concerts[date] and not concerts[date]["downloaded"] then
			local downloaded = get_mp3(date, mp3_folder .. date .. " " .. concerts[date]["title"] .. " " .. os.date("%H%M%S") .. ".mp3")
			concerts[date]["downloaded"] = downloaded
		end
	end
	io.output(io.open(settings_file, "w"))
	io.write "concerts ="
	serialize(concerts)
	io.output():close()
else
	error "download of schedule failed"
end

