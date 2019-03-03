-- look_cleanios.lua drawing engine configuration file for Notion.

if not gr.select_engine("de") then return end

de.reset()

local grey10 = "#1E1E1E" -- Grey 10%, R=30, G=30, B=30
local red = "#E32791" -- Deep Cerise, R=227, G=39, B=145
local green = "#30C798" -- Shamrock, R=48, G=199, B=152
local yellow = "#E3C472" -- Chenin, R=227, G=196, B=114
local blue = "#6796E6" -- Cornflower Blue, R=103, G=150, B=230
local magenta = "#E59FDF" -- Plum, R=229, G=159, B=223
local cyan = "#81D8D0" -- Riptide, R=129, G=216, B=208
local grey60 = "#969696" -- Grey 60%, R=150, G=150, B=150
local grey30 = "#515151" -- Grey 30%, R=81, G=81, B=81
local light_red = "#E466AD" -- Hot Pink, R=228, G=102, B=173
local light_green = "#6CD1B2" -- Medium Aquamarine, R=108, G=209, B=178
local light_yellow = "#E4CF98" -- Double Colonial White, R=228, G=207, B=152
local light_blue = "#91B0E6" -- Jordy Blue, R=145, G=181, B=230
local light_magenta = "#E5B6E1" -- French Lilac, R=229, G=182, B=225
local light_cyan = "#A2DCD7" -- Sinbad, R=162, G=220, B=215
local grey90 = "#E5E6E6" -- Grey 90%, R=229, G=230, B=230
local sky_blue = "#20BBFC" -- Deep Sky Blue, R=32, G=187, B=252
local orange = "#AF5E00" -- Golden Brown, R=175, G=94, B=0
local deep_blue = "#005577"

local mainfont = "-sgi-screen-medium-r-normal--11-*-72-72-m-*-*-*"

de.defstyle("*", {
    shadow_colour = grey10,
    highlight_colour = grey10,
    background_colour = grey10,
    foreground_colour = grey90,
    padding_pixels = 3,
    highlight_pixels = 0,
    shadow_pixels = 0,
    border_style = "elevated",
    font = mainfont,
    text_align = "center",
    transparent_background = false,
})

de.defstyle("tab", {
    font = mainfont,
    de.substyle("active-selected", {
        shadow_colour = grey10,
        highlight_colour = grey60,
        background_colour = deep_blue,
        foreground_colour = grey90,
    }),
    de.substyle("active-unselected", {
        shadow_colour = "#ffffff",
        highlight_colour = "#ffffff",
        background_colour = "#d8d8d8",
        foreground_colour = "#000000",
    }),
    de.substyle("inactive-selected", {
        shadow_colour = "#ffffff",
        highlight_colour = "#ffffff",
        background_colour = "#a8a8a8",
        foreground_colour = "#000000",
    }),
    de.substyle("inactive-unselected", {
        shadow_colour = "#ffffff",
        highlight_colour = "#ffffff",
        background_colour = "#d8d8d8",
        foreground_colour = "#000000",
    }),
    text_align = "center",
})

de.defstyle("input-edln", {
    de.substyle("*-cursor", {
        background_colour = "#000000",
        foreground_colour = "#d8d8d8",
    }),
    de.substyle("*-selection", {
        background_colour = "#f0c000",
        foreground_colour = "#000000",
    }),
})

de.defstyle("frame", {
    background_colour = grey90,
    de.substyle("quasiactive", {
        -- Something detached from the frame is active
        padding_colour = grey10,
    }),
    de.substyle("userattr1", {
        -- For user scripts
        padding_colour = "#009010",
    }),
    padding_pixels = 1,
})

de.defstyle("frame-tiled", {
    shadow_pixels = 0,
    highlight_pixels = 0,
    spacing = 8,
})

--de.defstyle("frame-tiled-alt", {
--    bar = "none",
--})

de.defstyle("frame-floating", {
    --bar = "shaped",
    padding_pixels = 0,
})

de.defstyle("frame-transient", {
    --bar = "none",
    padding_pixels = 0,
})


de.defstyle("actnotify", {
    shadow_colour = "#c04040",
    highlight_colour = "#c04040",
    background_colour = "#901010",
    foreground_colour = "#eeeeee",
})

de.defstyle("tab", {
    de.substyle("*-*-*-unselected-activity", {
        shadow_colour = "#c04040",
        highlight_colour = "#c04040",
        background_colour = "#901010",
        foreground_colour = "#eeeeee",
    }),

    de.substyle("*-*-*-selected-activity", {
        shadow_colour = "#c04040",
        highlight_colour = "#c04040",
        background_colour = "#b03030",
        foreground_colour = "#ffffff",
    }),

    de.substyle("*-*-*-tabnumber", {
        background_colour = "black",
        foreground_colour = "green",
    }),
})

de.defstyle("tab-frame", {
    spacing = 1,
})

de.defstyle("tab-frame-floating", {
    spacing = 0,
})

de.defstyle("tab-menuentry", {
    text_align = "left",
})

de.defstyle("tab-menuentry-big", {
    font = "-*-helvetica-medium-r-normal-*-17-*-*-*-*-*-*-*",
    padding_pixels = 7,
})


de.defstyle("stdisp", {
    shadow_pixels = 0,
    highlight_pixels = 0,
    text_align = "left",
    background_colour = "#000000",
    foreground_colour = "grey",
    font="-misc-fixed-medium-r-*-*-13-*-*-*-*-60-*-*",

    de.substyle("important", {
        foreground_colour = "green",
    }),

    de.substyle("critical", {
        foreground_colour = "red",
    }),
})

gr.refresh()

