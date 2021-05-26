if not gr.select_engine 'de' then return end

de.reset()

local fontname <const> = 'M+ 2p'
-- local fontname <const> = 'sans'

de.defstyle('*', {
	shadow_colour = '#404040',
	highlight_colour = '#707070',
	background_colour = '#505050',
	foreground_colour = '#a0a0a0',
	padding_pixels = 1,
	highlight_pixels = 1,
	shadow_pixels = 1,
	border_style = 'elevated',
	font = 'xft:' .. fontname .. ':style=Regular:size=10',
	text_align = 'center'
})

de.defstyle('frame', {
	shadow_colour = '#404040',
	highlight_colour = '#707070',
	padding_colour = '#505050',
	background_colour = '#000000',
	foreground_colour = '#ffffff',
	padding_pixels = 1,
	highlight_pixels = 1,
	shadow_pixels = 1,
	transparent_background = true
})

de.defstyle('tab', {
	font = 'xft:' .. fontname .. ':style=Medium:size=10',
	de.substyle('active-selected', {
		shadow_colour = '#304050',
		highlight_colour = '#708090',
		background_colour = '#506070',
		foreground_colour = '#ffffff',
		font = 'xft:' .. fontname .. ':style=Bold:size=10'
	}),
	de.substyle('active-unselected', {
		shadow_colour = '#203040',
		highlight_colour = '#607080',
		background_colour = '#405060',
		foreground_colour = '#a0a0a0'
	}),
	de.substyle('inactive-selected', {
		shadow_colour = '#404040',
		highlight_colour = '#909090',
		background_colour = '#606060',
		foreground_colour = '#a0a0a0'
	}),
	de.substyle('inactive-unselected', {
		shadow_colour = '#404040',
		highlight_colour = '#707070',
		background_colour = '#505050',
		foreground_colour = '#a0a0a0'
	}),
	text_align = 'center'
})

de.defstyle('input', {
	shadow_colour = '#404040',
	highlight_colour = '#707070',
	background_colour = '#000000',
	foreground_colour = '#ffffff',
	padding_pixels = 1,
	highlight_pixels = 1,
	shadow_pixels = 1,
	border_style = 'elevated',
	de.substyle('*-cursor',
            	{background_colour = '#ffffff', foreground_colour = '#000000'}),
	de.substyle('*-selection',
            	{background_colour = '#505050', foreground_colour = '#ffffff'})
})

de.defstyle('input-menu', {
	de.substyle('active', {
		shadow_colour = '#304050',
		highlight_colour = '#708090',
		background_colour = '#506070',
		foreground_colour = '#ffffff'
	})
})

dopath 'lookcommon_emboss'

gr.refresh()

