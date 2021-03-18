if not statusd_uptime then
	statusd_uptime = {
		interval = 1000,
	}
end

local timer = statusd.create_timer()

local function get_uptime_info()
		local s = os.date '%a %d %b %T %Z'
		timer:set(statusd_uptime.interval, get_uptime_info)
		statusd.inform('uptime', s)
end

get_uptime_info()
