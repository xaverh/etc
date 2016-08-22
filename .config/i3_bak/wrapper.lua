#!/usr/bin/env lua

-- Load the JSON library
jsonpath = arg[0]:gsub('wrapper.lua', 'JSON.lua')
JSON     = loadfile(jsonpath)()

-- respond with i3 protocol format, no "," the first time
first = true

local hashbar = {
    "__________",
    "#_________",
    "##________",
    "###_______",
    "####______",
    "#####_____",
    "######____",
    "#######___",
    "########__",
    "#########_",
    "##########",
}

function mem()
    local mem = {}
    local swap = {}
    for line in io.lines("/proc/meminfo") do
        for k, v in string.gmatch(line, "([%a]+):[%s]+([%d]+).+") do
            if k == "MemTotal" then mem.total = v
            elseif k == "MemFree" then mem.free = v
            elseif k == "Buffers" then mem.buf = v
            elseif k == "Cached" then mem.cached = v
            elseif k == "SwapTotal" then swap.total = v
            elseif k == "SwapFree" then swap.free = v
            end
        end
    end
    local memory = math.floor(0.5+(mem["total"] - mem["free"] - mem["buf"] - mem["cached"])/mem["total"] * 10)
    return {hashbar[memory + 1], memory > 8}
end


function i3respond(list, line)
    if first then
        print(string.sub(line,1,-2) .. "," .. string.sub(JSON:encode(list),2))
        first = false
    else
        print(string.sub(line,1,-2) .. "," .. string.sub(JSON:encode(list),2))
    end
end

-- Print some i3 protocol stuff
print('{"version":1}')
print('[')

-- remove i3 protocol stuff from the output of i3status
io.read()
io.read()

-- While read data from stdin
while true do
    local line = io.read()

    -- Construct the response
    response = {
        {
            name      = "memory",
            color     = mem()[2] and "#f92672",
            full_text = " RAM: " .. mem()[1] .. " "
        },
        {
            name      = "time",
            full_text = " " .. os.date('%e %b %y %I:%M %p') .. " "
        }
    }

    -- Respond!
    i3respond(response, line)
    --print("," .. string.sub(line,2))

    io.flush()
end
