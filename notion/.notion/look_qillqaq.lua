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
local deep_blue = "#005577" -- Blue Lagoon, R=0, G=85, B=119
local grey20 = "#3a3d41" -- Grey 20%, R=58, G=61, B=65
local grey70 = "#b8b8b8" -- Grey 70%, R=184, G=184, B=184
local cyprus = "#0f3a4b" -- Cyprus, R=15, G=58, B=75

dopath("local_settings")

local mainfont = xha_mainfont or "-sgi-screen-medium-r-normal--11-*-72-72-m-*-*-*"

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
    transparent_background = true,
})

de.defstyle("tab", {
    font = mainfont,
    de.substyle("active-selected", {
        background_colour = deep_blue,
        foreground_colour = grey90,
    }),
    de.substyle("active-unselected", {
        background_colour = cyprus,
        foreground_colour = grey90,
    }),
    de.substyle("inactive-selected", {
        background_colour = grey30,
        foreground_colour = grey90,
    }),
    de.substyle("inactive-unselected", {
        background_colour = grey20,
        foreground_colour = grey90,
    }),
    text_align = "center",
})

de.defstyle("input-edln", {
    transparent_background = false,
    de.substyle("*-cursor", {
        background_colour = grey90,
        foreground_colour = cyprus,
    }),
    de.substyle("*-selection", {
        background_colour = green,
        foreground_colour = grey10,
    }),
})

de.defstyle("frame", {
    background_colour = red,
    de.substyle("quasiactive", {
        -- Something detached from the frame is active
        padding_colour = grey10,
    }),
    de.substyle("userattr1", {
        -- For user scripts
        padding_colour = green,
    }),
    padding_pixels = 0,
})

de.defstyle("frame-tiled", {
    shadow_pixels = 0,
    highlight_pixels = 0,
    spacing = 0,
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
    background_colour = magenta,
    foreground_colour = grey10,
})

de.defstyle("tab", {
    de.substyle("*-*-*-unselected-activity", {
        background_colour = magenta,
        foreground_colour = grey10,
    }),

    de.substyle("*-*-*-selected-activity", {
        background_colour = light_magenta,
        foreground_colour = grey90,
    }),

    de.substyle("*-*-*-tabnumber", {
        background_colour = grey10,
        foreground_colour = green,
    }),
})

de.defstyle("tab-frame", {
    spacing = 1,
})

de.defstyle("tab-frame-floating", {
    spacing = 1,
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
    padding_pixels = 3,
    text_align = "left",
    background_colour = grey10,
    foreground_colour = grey90,
    font=mainfont,
    transparent_background = false,

    de.substyle("important", {
        foreground_colour = green,
    }),

    de.substyle("critical", {
        foreground_colour = red,
    }),
})

gr.refresh()

