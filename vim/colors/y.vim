vim9script

hi clear
if exists("syntax_on")
  syntax reset
endif

g:colors_name = "y"

class Color
        public var gui: string
        public var cterm: string
endclass

# class Hexagon
#       public var r: Color
#       public var g: Color
#       public var y: Color
#       public var b: Color
#       public var m: Color
#       public var c: Color
# endclass

def Hi(group: string, fg: Color, bg: Color, attr: string)
  if fg.gui != ""
    execute "highlight " .. group .. " guifg=" .. fg.gui .. " ctermfg=" ..  fg.cterm
  endif
  if bg.gui != ""
    execute "highlight " .. group .. " guibg=" .. bg.gui .. " ctermbg=" ..  bg.cterm
  endif
  if attr != ""
    execute "highlight " .. group .. " gui=" .. attr .. " cterm=" .. attr
  endif
enddef

# Miami

var tickleMePink = Color.new("#ff7faa", 'brightred')
var appleGreen = Color.new("#9bb53e", 'brightgreen')
# var peachFuzz = Color.new('#febe98', 'brightyellow') # 9 FIXME derzeit beides, fg nur auf Rhöndorf
# var pictonBlue = Color.new('#00b4ff', 'brightblue') # D? TODO Name
var heliotrope = Color.new('#ee70ff', 'brightmagenta')
# var tiffany = Color.new('#81D8D0', 'brightcyan') # E


# London

# Hintergrundfarben
var londonWhiteBg = Color.new('#FFF1E5', '255') # bg
var londonWhiteBox = Color.new('#F2DFCE', '252')
var londonWhite40 = Color.new('#FFF7EF', '231') # 0
var chicagoGray2 = Color.new('#49483E', '239') # TODO passt von Seiten London, evtl. verschieben in eine Londonfarbe, checken im dunklen Thema; könnte das dunkle 'vanilla' oder 'midori' sein
var peachFuzz = Color.new('#febe98', '224') # 9 FIXME derzeit beides, fg nur auf Rhöndorf
var midori = Color.new('#dbf3cd', '194') # A
var vanilla = Color.new('#f3e5ab', '230') # B
var rhoendorf_light = Color.new('#5b5b66', '236') # FIXME derzeit beides
var tiffany = Color.new('#81D8D0', '87') # E
var rhoendorf = Color.new('#2d3c4b', '236') # F FIXME derzeit beides

# Textfarben
var aaliyah = Color.new('#19191c', '233') # fg
var pictonBlue = Color.new('#00b4ff', '39') # D? TODO Name
var jazzberryJam = Color.new('#9F1B66', '161') # 1
var londonGray3 = Color.new('#807973', '243') # 7
var londonGray6 = Color.new('#CCC1B7', '251') # 8
var londonGreen = Color.new('#458b00', '28') # 2 TODO Name
var lovesymbol2 = Color.new('#553a63', '89') # 5
var puckerUp = Color.new('#CA3368', '198')
var mandarin = Color.new('#ff8833', '209') # 3
var wutang = Color.new('#FFC322', '178') # nur auf Rhöndorf-Hintergrund
var yvesKlein = Color.new('#002fa7', '27') # 4
var atoll = Color.new('#0A6F75', '30') # 6
