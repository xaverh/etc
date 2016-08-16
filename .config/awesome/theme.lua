-- dunzor2, awesome3 theme, by dunz0r

--{{{ Main
local awful = require("awful")
awful.util = require("awful.util")

theme = {}

home          = os.getenv("HOME")
config        = awful.util.getdir("config")
shared        = "/usr/share/awesome"
if not awful.util.file_readable(shared .. "/icons/awesome16.png") then
    shared    = "/usr/share/local/awesome"
end
sharedicons   = shared .. "/icons"
sharedthemes  = shared .. "/themes"
themes        = config .. "/themes"
themename     = "/dunzor2"
if not awful.util.file_readable(themes .. themename .. "/theme.lua") then
       themes = sharedthemes
end
themedir      = themes .. themename

-- if awful.util.file_readable(home .. "/.wallpaper.jpg") then
--     theme.wallpaper      = home .. "/.wallpaper.jpg"
-- else
--     theme.wallpaper = home .. "/Dropbox/Dokumente/Erinnerungen/Desktopmotive/Windows 7/Brasilien/BR-olwp" .. math.random(1, 6) .. ".jpg"
-- end

if awful.util.file_readable(config .. "/vain/init.lua") then
    theme.useless_gap_width  = "3"
end
--}}}

theme.font            = "Tamsyn 8"

theme.bg_normal     = "#161718"
theme.bg_focus      = "#1b1d1e"
theme.bg_urgent     = "#161718"
theme.bg_minimize   = "#161718"

theme.fg_normal     = "#808080"
theme.fg_focus      = "#a0a0a0"
theme.fg_unfocus    = "#808080"
theme.fg_urgent     = "#f92672"
theme.fg_highlight  = "#56c2d6"
theme.fg_minimize   = "#505354"

theme.border_width  = "1"
theme.border_normal = "#161718"
theme.border_focus  = "#82b414"
theme.border_marked = "#f92672"

-- widget colours
theme.wid_fg = "#a0a0a0"
theme.wid_bg = "#1b1d1e"
theme.wid_rl = "#f92672"
theme.wid_rh = "#ff5995"
theme.wid_gl = "#82b414"
theme.wid_gh = "#b6e354"
theme.wid_yl = "#fd971f"
theme.wid_yh = "#feed6c"
theme.wid_bl = "#56c2d6"
theme.wid_bh = "#8cedff"
theme.wid_ml = "#8c54fe"
theme.wid_mh = "#9e6ffe"
theme.wid_cl = "#465457"
theme.wid_ch = "#899ca1"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

theme.taglist_bg_focus = theme.wid_bg
theme.taglist_fg_focus = theme.wid_bl
theme.taglist_bg = theme.wid_bg
theme.taglist_fg = theme.wid_fg
theme.taglist_bg_urgent = theme.wid_bg
theme.taglist_fg_urgent = theme.wid_rl

-- {{{ Widgets
-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
theme.fg_widget        = "#a0a0a0"
theme.fg_center_widget = "#88A175"
theme.fg_end_widget    = "#FF5656"
theme.bg_widget        = "#494B4F"
theme.border_widget    = "#3F3F3F"
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = "#CC9393"
-- mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = "18"
theme.menu_width  = "100"
-- }}}

-- {{{ Icons
-- {{{ Taglist
theme.taglist_squares_sel   = config .. "/icons/squarez.png"
theme.taglist_squares_unsel = config .. "/icons/squareza.png"
--theme.taglist_squares_resize = "false"
-- }}}

-- {{{ Misc
theme.awesome_icon = config .. "/icons/awesome-icon.png"
theme.menu_submenu_icon      = config .. "/icons/submenu.png"
-- }}}


-- {{{ Layout
theme.layout_tile       = config .. "/icons/tilew.png"
theme.layout_tileleft   = config .. "/icons/tileleftw.png"
theme.layout_tilebottom = config .. "/icons/tilebottomw.png"
theme.layout_tiletop    = config .. "/icons/tiletopw.png"
theme.layout_fairv      = config .. "/icons/fairvw.png"
theme.layout_fairh      = config .. "/icons/fairhw.png"
theme.layout_spiral     = config .. "/icons/spiralw.png"
theme.layout_dwindle    = config .. "/icons/dwindlew.png"
theme.layout_max        = config .. "/icons/maxw.png"
theme.layout_fullscreen = config .. "/icons/fullscreenw.png"
theme.layout_magnifier  = config .. "/icons/magnifierw.png"
theme.layout_floating   = config .. "/icons/floatingw.png"
-- }}}


-- {{{ Titlebar
theme.titlebar_close_button_focus  = config .. "/icons/close_focus.png"
theme.titlebar_close_button_normal = config .. "/icons/close_normal.png"

theme.titlebar_ontop_button_focus_active  = config .. "/icons/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active = config .. "/icons/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = config .. "/icons/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = config .. "/icons/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active  = config .. "/icons/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active = config .. "/icons/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = config .. "/icons/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = config .. "/icons/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active  = config .. "/icons/floating_focus_active.png"
theme.titlebar_floating_button_normal_active = config .. "/icons/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = config .. "/icons/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = config .. "/icons/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active  = config .. "/icons/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active = config .. "/icons/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = config .. "/icons/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = config .. "/icons/maximized_normal_inactive.png"

-- }}}
-- }}}
theme.tasklist_floating_icon = config .. "/icons/awesome-icon.png"

return theme
