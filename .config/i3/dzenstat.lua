local ICONPATH="$HOME/.icons/xbm8x8"
local COLOR_ICON="#56c2d6"
local CRIT_COLOR="#f92672"
local DZEN_FG="#a0a0a0"
local DZEN_BG="#1b1d1e"
local HEIGHT=12
local WIDTH=470
local xrandr = io.popen([[xrandr | grep "current" | awk '{print $8}']]) 
local RESOLUTIONW = xrandr:read("*line")
xrandr:close()
-- XXX local RESOLUTIONH=`xrandr | grep "current" | awk '{print $10}' | tr -d ','`
local X = RESOLUTIONW - WIDTH
local Y=0
local BAR_FG="#333333"
local BAR_BG="#808080"
local BAR_H=10
local BAR_W=60
local FONT="Tamsyn:Regular:pixelsize=12"
local SLEEP=1
local VUP="amixer -c0 -q set Master 4dB+"
local VDOWN="amixer -c0 -q set Master 4dB-"
--local EVENT="button3=exit;button4=exec:$VUP;button5=exec:$VDOWN"
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

function round(num, idp)
    return tonumber(string.format("%." .. (idp or 0) .. "f", num))
end

function vol()
    local f = io.popen("amixer get Master")
    local mixer = f:read("*all")
    f:close()
    local volu, mute = string.match(mixer, "([%d]+)%%.*%[([%l]*)")
    if volu == nil then
        f = io.popen('echo "0" | gdbar -fg ' .. BAR_FG ..  ' -bg ' .. BAR_BG .. ' -h ' .. BAR_H .. ' -w ' .. BAR_W .. ' -s o -ss 1 -sw 2 -nonl')
        local gdbar = f:read("*line")
        return "^fg(" ..  CRIT_COLOR .. ")^i(" .. ICONPATH .. "/spkr_01.xbm)^fg() " .. gdbar
    else
        f = io.popen('echo "' .. volu .. '" | gdbar -fg ' .. BAR_FG ..  ' -bg ' .. BAR_BG .. ' -h ' .. BAR_H .. ' -w ' .. BAR_W .. ' -      +++ s o -ss 1 -sw 2 -nonl')
        local gdbar = f:read("*line")
        return "^fg(" .. COLOR_ICON .. ")^i(" .. ICONPATH .. "/spkr_01.xbm)^fg() " .. gdbar
    end
end

function mem()
    local mem = {}
    local swap = {}
    for line in io.lines("/proc/meminfo") do
        for k, v in string.gmatch(line, "([%a]+):[%s]+([%d]+).+") do
            if k == "MemTotal" then mem["total"] = v
            elseif k == "MemFree" then mem["free"] = v
            elseif k == "Buffers" then mem["buf"] = v
            elseif k == "Cached" then mem["cached"] = v
            elseif k == "SwapTotal" then swap["total"] = v
            elseif k == "SwapFree" then swap["free"] = v
            end
        end
    end
    print(math.round(mem["total"] - mem["free"] - mem["buf"] - mem["cached"])/mem["total"] * 100)
    --return "^fg($COLOR_ICON)^i($ICONPATH/mem.xbm)^fg() " .. hashbar[round(mem["free"]/mem["total"])] .. " ^fg($COLOR_ICON)^i($ICONPATH/mem.xbm)^fg() " .. hashbar[round(swap["free"]/swap["total"])] 
end

-- "^fg($CRIT_COLOR)^i($ICONPATH/temp.xbm)^fg($CRIT_COLOR) ${TEMP}°C" $(echo ${TEMP} | gdbar -fg $CRIT_COLOR -bg $BAR_BG -h $BAR_H -s v -sw 5 -ss 0 -sh 1 -nonl)

function date()
    return "^fg(" .. COLOR_ICON .. ")^i(" .. ICONPATH .. "/clock.xbm)^fg(" .. DZEN_FG .. ") " .. os.date("%c")
end

function between()
    return " ^fg(#7298a9)^r(2x2)^fg() "
end

--[=[
Disk ()
{
    SDA2=$(df -h / | awk '/\/$/ {print $5}' | tr -d '%')
    SDA4=$(df -h /home | awk '/home/ {print $5}' | tr -d '%')
    if [[ ${SDA2} -gt 60 ]] ; then
        echo -n "^fg($COLOR_ICON)^i($ICONPATH/fs_01.xbm)^fg() /:${SDA2}% $(echo $SDA2 | gdbar -fg $CRIT_COLOR -bg $BAR_BG -h 7 -w 40 -s o -ss 0 -sw 2 -nonl)"
    else
        echo -n "^fg($COLOR_ICON)^i($ICONPATH/fs_01.xbm)^fg() /:${SDA2}% $(echo $SDA2 | gdbar -fg $BAR_FG -bg $BAR_BG -h 7 -w 40 -s o -ss 0 -sw 2 -nonl)"
        fi
        if [[ ${SDA4} -gt 80 ]] ; then
            echo -n "  ~:${SDA4}% $(echo $SDA4 | gdbar -fg $CRIT_COLOR -bg $BAR_BG -h 7 -w 40 -s o -ss 0 -sw 2 -nonl)"
        else
            echo -n "  ~:${SDA4}% $(echo $SDA4 | gdbar -fg $BAR_FG -bg $BAR_BG -h 7 -w 40 -s o -ss 0 -sw 2 -nonl)"
            fi
            return
        }


        OSLogo ()
        {
            OS=$(uname -a | awk '{print $2}')
            echo -n " ^fg($COLOR_ICON)^i($ICONPATH/${OS}.xbm)^fg()"
            return
        }

--]=]

-- End Of Functions

function print_bar()
    return mem() .. between() .. vol() .. between() .. date()
end

--[[
echo "$(Print)" 
done | $DZEN &
--]]

-- Infinite loop
while true do
    os.execute("sleep " .. tonumber(SLEEP))
    os.execute("echo " .. print_bar() .. " | dzen2 -x " .. X .. " -y " .. Y .. " -w " .. WIDTH .. " -h " .. HEIGHT .. " -fn " .. FONT .. " -ta 'c' -bg " .. DZEN_BG .. " -fg " .. DZEN_FG .. [[" # -e "button3=exit;button4=exec:]] .. VUP .. [[;button5=exec:]] .. VDOWN .. [["]])
end
