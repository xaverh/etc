-- put user settings here
-- this module will be loaded after everything else when the application starts

local core = require 'core'
local keymap = require 'core.keymap'
local config = require 'core.config'
local style = require 'core.style'

-- light theme:
core.reload_module 'colors.duorand'
style.code_font = renderer.font.load(
    "/usr/share/fonts/TTF/Input-Regular_(InputMonoCondensed-Regular).ttf",
    20 * SCALE
)
style.font = renderer.font.load(
    "/usr/share/fonts/OTF/SF-Pro-Display-Regular.otf",
    16 * SCALE
)
-- key binding:
-- keymap.add { ["ctrl+escape"] = "core:quit" }

