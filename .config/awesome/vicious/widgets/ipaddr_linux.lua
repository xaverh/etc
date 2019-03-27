local helpers = require("vicious.helpers")

-- IPAddr: provides the IP Address using `ip`
-- vicious.widgets.ipaddr
local ipaddr_linux = {}

local function worker(format, warg)
	local test_address
	-- this could be any valid non-private IP address,
	-- there's nothing actually done with them
	if warg == "ipv6" then
		test_address = "2606:4700:4700::1111"
	else
		test_address = "1.1.1.1"
	end

	-- Get data from iw where available
	local f =
		io.popen(
		"export PATH=$PATH:/sbin/:/usr/sbin:/usr/local/sbin;" ..
			"ip route get " .. helpers.shellquote(test_address))
	local ipresult = f:read("*all")
	f:close()

	-- ip wasn't found, isn't executable, or non-wireless interface
	if ipresult == nil then
		return {}
	elseif string.find(ipresult, "Network is unreachable") then
		return {"offline"}
	else
		return string.sub(string.match(ipresult, "src%s[%x%.:]*"), 4)
	end
end

return setmetatable(
	ipaddr_linux,
	{
		__call = function(_, ...)
			return worker(...)
		end
	}
)
