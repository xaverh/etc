---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2019, Xaver Hellauer <software@hellauer.bayern>
---------------------------------------------------

local helpers = require("vicious.helpers")
local spawn = require("vicious.spawn")

-- vicious.widgets.ipaddr
local ipaddr_linux = {}

local ROUTE = "PATH=$PATH:/sbin/:/usr/sbin:/usr/local/sbin ip route get 8.8.8.8"

function ipaddr_linux.async(format, warg, callback)
    local ipinfo = {}

    local function parse_ip(stdout)
        ipinfo[1] = stdout:match " src ([%d.]*)"
    end

    spawn.easy_async_with_shell(
        ROUTE:format(warg),
        function(stdout, stderr, exitreason, exitcode)
            parse_ip(stdout)
            callback(ipinfo)
        end
    )
end

return helpers.setasyncall(ipaddr_linux)
