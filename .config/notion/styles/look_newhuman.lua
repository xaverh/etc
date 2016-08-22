--
-- look_newhuman for Ion's default drawing engine. 
-- 

if not gr.select_engine("de") then
    return
end

-- Clear existing styles from memory.
de.reset()

-- Base style
de.defstyle("*", {
    transparent_background = true,
    --highlight_colour = "#7b6d67",
    highlight_colour = "#2c2d2e",
    --shadow_colour = "#7b6d67",
	shadow_colour = "#2c2d2e",
    --background_colour = "#3e3734",
    background_colour = "#2c2d2e",
    --foreground_colour = "#ebe0ce",
    foreground_colour = "#e2e2e2",
    
    shadow_pixels = 1,
    highlight_pixels = 1,
    padding_pixels = 1,
    spacing = 0,
    border_style = "elevated",
    border_sides = "tb",
    
--    font = "-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-*-*",
	-- das hier ist ein gescheiterter Versuch
--	font = "-bitstream-bitstream vera sans mono-medium-r-normal-*-10-*-*-*-m-*-*-*",	
	font = "-dejavu-dejavu sans mono-medium-r-*-10-0-*-*-*-m-*-*-*",
    text_align = "center",
})


de.defstyle("frame", {
    --background_colour = "#000000",
    background_colour = "#2c2d2e",
    transparent_background = true,
})


de.defstyle("tab", {
--    font = "-*-helvetica-medium-r-normal-*-10-*-*-*-*-*-*-*",
--	font = "-bitstream-bitstream vera sans mono-medium-r-normal-*-10-*-*-*-m-*-*-*",	
	font = "-dejavu-dejavu sans mono-medium-r-*-10-0-*-*-*-m-*-*-*",
    spacing = 1,
    
    de.substyle("inactive-selected", {
        --highlight_colour = "#D78E49",
        highlight_colour = "#2c2d2e",
        --shadow_colour = "#D78E49",
        shadow_colour = "#2c2d2e",
        --background_colour = "#55422f",
        background_colour = "#2c2d2e",
    }),

    de.substyle("active-selected", {
       -- highlight_colour = "#B98052",
       -- shadow_colour = "#B98052",
        highlight_colour = "#000000",
        shadow_colour = "#000000",
        background_colour = "#000000",
    }),
})


de.defstyle("input", {
    text_align = "left",
    spacing = 1,
    
    de.substyle("*-selection", {
        --background_colour = "#845f3a",
        background_colour = "#000000",
    }),

    de.substyle("*-cursor", {
        --background_colour = "#D78E49",
        background_colour = "#e2e2e2",
        --foreground_colour = "#3e3734",
        foreground_colour = "#2c2d2e",
    }),
})


dopath("lookcommon_clean")

    
-- Refresh objects' brushes.
gr.refresh()
