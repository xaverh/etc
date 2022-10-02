local defaults = {
    update_interval = 2897,
}

local wifi_timer = statusd.create_timer()
local settings=table.join(statusd.get_config("wifi"), defaults)

-- Parse the output and then tell statusd when we have all of the value.
function parse_wifi(partial_data)
    -- Keep reading partial_data until it returns nil
    local result = ""
    while partial_data do
        result = result .. partial_data
        -- statusd.popen_bgread() will resume us when it has more data
        partial_data = coroutine.yield()
    end

    -- If we have a new result, tell statusd
    if result and result ~= "" then
        statusd.inform("wifi", (result:match 'SSID: ([^\n]*)' or 'no') .. ' ' .. (result:match 'Connected to %x%x:%x%x:%x%x:(%x%x:%x%x:%x%x)' or
                                        'WiFi'))
    end

    -- Setup the next execution.
    wifi_timer:set(settings.update_interval, update_wifi)
end

local function flush_stderr(partial_data)
    local result = ""
    while partial_data do
        result = result .. partial_data
        partial_data = coroutine.yield()
    end

    if result and result ~= "" then
        print("STDERR:", result, "\n")
    end
end

function update_wifi()
    statusd.popen_bgread('/usr/sbin/iw dev wlan0 link',
                         coroutine.wrap(parse_wifi),
                         coroutine.wrap(flush_stderr))
end

update_wifi()
