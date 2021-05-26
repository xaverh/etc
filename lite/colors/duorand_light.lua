local style = require 'core.style'
local common = require 'core.common'

math.randomseed(os.time())

local color = {math.random(0, 165), math.random(0, 165), math.random(0, 165)}

local function brighter(color, percentage) return math.max(math.min(255, color + color * percentage / 100),0) end

local bright_color = {brighter(color[1], -25), brighter(color[2], -25), brighter(color[3], -25)}

local function color_distance(c1, c2)
	local diff = 0
	for k, v in pairs(c1) do diff = diff + math.abs(c2[k] - v) end
	return diff
end

style.background = {common.color '#f6f5f4'}
style.background2 = {common.color '#f6f5f4'}
style.background3 = {common.color '#f6f5f4'}
style.text = {common.color '#424242'}
style.accent = color
style.dim = {common.color '#b8b8b8'}
style.divider = {common.color '#f6f5f4'}
style.selection = {255 - color[1], 255 - color[2], 255 - color[3], 80}
style.line_number = {common.color '#cccccc'}
style.line_number2 = {common.color '#bbbbbb'}
style.line_highlight = {common.color '#eeedec'}
style.scrollbar = bright_color
style.scrollbar2 = color

style.syntax = {}
style.syntax['normal'] = {common.color '#424242'}
style.syntax['symbol'] = {common.color '#424242'}
style.syntax['comment'] = {common.color '#b8b8b8'}
style.syntax['keyword'] = {common.color '#202020'}
style.syntax['keyword2'] = {common.color '#202020'}
style.syntax['number'] = bright_color
style.syntax['literal'] = bright_color
style.syntax['string'] = color
style.syntax['operator'] = {common.color '#000000'}
style.syntax['function'] = {common.color '#202020'}

local paren_colors = {"#f75c03","#d90368","#820263","#3a579a","#04a777","#5c6f68","#36cdc4"}
table.sort(paren_colors, function(a, b)
	return color_distance(color, {common.color(a)}) > color_distance(color, {common.color(b)})
end)

style.caret = {common.color(paren_colors[1])}
style.syntax.paren1 = {common.color(paren_colors[2])}
style.syntax.paren2 = {common.color(paren_colors[3])}
style.syntax.paren3 = {common.color(paren_colors[4])}
style.syntax.paren4 = {common.color(paren_colors[5])}
style.syntax.paren5 = {common.color(paren_colors[6])}

