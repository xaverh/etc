set background=light
hi clear
if exists("syntax_on")
	syntax reset
endif

let g:colors_name="higgsboson"

hi Cursor       guibg=#404040 guifg=#a0a0a0
hi CursorColumn guibg=#e0e0e0 guifg=#202020
hi CursorLine   guibg=#e0e0e0 guifg=#202020
hi NonText      guibg=#d0d0d0 guifg=#606060 gui=none
hi Visual       guibg=#fffac8 guifg=#404040 gui=none
hi Folded       guibg=#e8e5d0 guifg=#606060 gui=none
hi TabLineFill  guibg=#d0d0d0 guifg=#606060 gui=none
hi SpecialKey   guibg=#e8e8e8 guifg=#a0a0a0 gui=none
hi Search       guibg=#808080 guifg=#ffffff gui=bold
hi ModeMsg      guibg=#fafafa guifg=#304050 gui=bold
hi MoreMsg      guibg=#fafafa guifg=#304050 gui=bold
hi StatusLine   guibg=#808080 guifg=#fafafa gui=bold
hi StatusLineNC guibg=#707070 guifg=#d0d0d0 gui=none
hi VertSplit    guibg=#707070 guifg=#909090 gui=none
hi LineNr       guibg=#dadada guifg=#808080 gui=none
hi MatchParen   guibg=#afffd7 guifg=#404040 gui=bold

hi DiffAdd      guibg=#a67429 guifg=#ffcc7f
hi DiffDelete   guibg=#a62910 guifg=#ff7f50
hi DiffChange   guibg=#425c78 guifg=#7fbdff
hi DiffText     guibg=#4e9a06 guifg=#8ae234
hi SpellBad     gui=undercurl guisp=#f02020


hi Title          guibg=#fafafa guifg=#202020 gui=underline
hi Normal         guibg=#fafafa guifg=#404040 gui=none

hi Comment        guibg=#fafafa guifg=#b2b2b2 gui=none

hi Constant       guibg=#fafafa guifg=#957301 gui=none
hi String         guibg=#fafafa guifg=#a07060 gui=none
hi Character      guibg=#fafafa guifg=#bb6245 gui=none
hi Number         guibg=#fafafa guifg=#ba7222 gui=none
hi Boolean        guibg=#fafafa guifg=#d4552c gui=none
hi Float          guibg=#fafafa guifg=#e87a00 gui=none

hi Identifier     guibg=#fafafa guifg=#259975 gui=none
hi Function       guibg=#fafafa guifg=#37bdaa gui=italic

hi Statement      guibg=#fafafa guifg=#d2346d gui=none
hi Conditional    guibg=#fafafa guifg=#81ac3a gui=bold
hi Repeat         guibg=#fafafa guifg=#aab844 gui=bold
hi Label          guibg=#fafafa guifg=#969664 gui=underline
hi Operator       guibg=#fafafa guifg=#a79a39 gui=none
hi Keyword        guibg=#fafafa guifg=#408077 gui=none
hi Exception      guibg=#fafafa guifg=#a03020 gui=none

hi PreProc        guibg=#fafafa guifg=#7d64af gui=italic
hi Include        guibg=#fafafa guifg=#c5a2d8 gui=none
hi Define         guibg=#fafafa guifg=#87005f gui=none
hi Macro          guibg=#fafafa guifg=#87005f gui=italic
hi PreCondit      guibg=#fafafa guifg=#75698c gui=bold

hi Type           guibg=#fafafa guifg=#7d95ad gui=none
hi StorageClass   guibg=#fafafa guifg=#678b5b gui=none
hi Structure      guibg=#fafafa guifg=#5d6da3 gui=none
hi Typedef        guibg=#fafafa guifg=#5080b0 gui=none

hi Special        guibg=#fafafa guifg=#408077 gui=none
hi SpecialChar    guibg=#fafafa guifg=#603020 gui=none
hi Tag            guibg=#fafafa guifg=#a6a679 gui=underline
hi Delimiter      guibg=#fafafa guifg=#808080 gui=none
hi SpecialComment guibg=#fafafa guifg=#caacac gui=none
hi Debug          guibg=#fafafa guifg=#908080 gui=none

hi Underlined     guibg=#fafafa guifg=#202020 gui=underline

hi Error          guibg=#faf5cd guifg=#c83c28 gui=none

hi ToDo           guibg=#dadada guifg=#808080 gui=bold


