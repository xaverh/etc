local style = require 'core.style'
local common = require 'core.common'

math.randomseed(os.time())

local color = {math.random(90, 255), math.random(90, 255), math.random(90, 255)}
local pcolor = {math.random(90, 255), math.random(90, 255), math.random(90, 255)}

local pcolors = {pcolor,
{pcolor[2], pcolor[3], pcolor[1]},
{pcolor[3], pcolor[2], pcolor[1]},
{pcolor[1], pcolor[3], pcolor[2]},
{pcolor[2], pcolor[1], pcolor[3]},
{pcolor[3], pcolor[1], pcolor[2]}}

local function brighter(color, percentage) return math.min(255, color + color * percentage / 100) end

local bright_color = {brighter(color[1], 30), brighter(color[2], 30), brighter(color[3], 25)}

local function color_distance(c1, c2)
	local diff = 0
	for i, v in ipairs(c1) do diff = diff + math.abs(c2[i] - v) end
	return diff
end

style.background = {common.color '#171717'}
style.background2 = {common.color '#212121'}
style.background3 = {common.color '#303030'}
style.text = {common.color '#909090'}
style.caret = color_distance({0xff,0x73,0x15},color)>color_distance({0x5a,0xaa,0xdf},color) and {0xff,0x73,0x15} or {0x5a,0xaa,0xdf}
style.accent = color
style.dim = {common.color '#707070'}
style.divider = {common.color '#070707'}
style.selection = {255 - color[1], 255 - color[2], 255 - color[3], 80}
style.line_number = {common.color '#252525'}
style.line_number2 = {common.color '#444444'}
style.line_highlight = {common.color '#212121'}
style.scrollbar = color
style.scrollbar2 = bright_color

style.syntax = {}
style.syntax['normal'] = {common.color '#e5e6e6'}
style.syntax['symbol'] = {common.color '#e5e6e6'}
style.syntax['comment'] = {common.color '#404040'}
style.syntax['keyword'] = {common.color '#dfdfdf'}
style.syntax['keyword2'] = {common.color '#dfdfdf'}
style.syntax['number'] = bright_color
style.syntax['literal'] = bright_color
style.syntax['string'] = color
style.syntax['operator'] = {common.color '#ffffff'}
style.syntax['function'] = {common.color '#e5e6e6'}

local function color_distance(c1, c2)
	local diff = 0
	for i, v in ipairs(c1) do diff = diff + math.abs(c2[i] - v) end
	return diff
end

table.sort(pcolors, function(a, b)
	return color_distance(color, a) > color_distance(color, b)
end)

style.syntax.paren1 = pcolors[1]
style.syntax.paren2 = pcolors[2]
style.syntax.paren3 = pcolors[3]
style.syntax.paren4 = pcolors[4]
style.syntax.paren5 = pcolors[5]

