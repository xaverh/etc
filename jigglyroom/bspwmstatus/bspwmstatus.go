package bspwmstatus

import (
	"strings"
)

// monitor name only on multi-monitor, das muß irgendwo zwischen applyMarkup und makeItems angewandt werden
// not sure how {} must be processed for lemonbar, % appears to be the only character that needs to be escaped, und zwar %%
const (
	testInput       = `WMHDMI-1:OI:f{}:f3:oDéjà-vu:ffünf:fVI:fdie sieben:fVIII:oШОС:f0:LT:TT:GSPLM`
	backgroundColor = "#1e1e1e"
	foregrundColor  = "#f5f6f6"
	gotoDesktop     = "bspc desktop -f "
	multimonitor    = false
	stickyText      = ""
	markedText      = "*"
	lockedText      = ""
	monocleText     = "[]"
	tilingText      = "##"
)

type button string

const (
	mouseLeft   button = "1"
	mouseMiddle button = "2"
	mouseRight  button = "3"
	mouseUp     button = "4"
	mouseDown   button = "5"
)

type statusItem struct {
	Type string
	Name string
}

/*
var markupMap = map[string]string{
	"f": startLeftClickArea + gotoDesktop + "%q" + clickAreaCmdNameSep + "%[1]q" + endClickArea,
	"f": makeClickable()
	"o": startLeftClickArea + gotoDesktop + "%q" + clickAreaCmdNameSep + "%[1]q" + endClickArea,
	"u": startLeftClickArea + gotoDesktop + "%q" + clickAreaCmdNameSep + "%[1]q" + endClickArea,
	"F": "%q",
	"O": "%q",
	"U": "%q",
	"m": "",
	"M": "",
	"L": "%q",
	// Flag, kann beliebige Kombination von Sticky (S), Private (P), Locked (L) oder Marked (M) sein
	"G": "",
	// State, kann Tiled (T), Pseudotiled (P), Fullscreen (=), Floating (F), Parent Node (@) sein
	"T": "",
}
*/

func markFocusedDesktop(s string) string {
	return s + "*"
}

func markUnfocusedDesktop(s string) string {
	return s
}

func markOccupiedDesktop(s string) string {
	return s + "."
}

func markFreeDesktop(s string) string {
	return s + "0"
}

func markUrgentDesktop(s string) string {
	return s + "!"
}

func desktopButton(text string, desktop string) string {
	return makeClickable(text, "bspc desktop "+desktop+" --focus ", mouseLeft)
}

func makeClickable(txt string, cmd string, b button) string {
	return "%%{A" + string(b) + ":" + cmd + ":}" + txt + "%%{A}"
}

func toggleLayoutWithLeftClick(txt string) string {
	return makeClickable(txt, "bspc desktop focused --layout next", mouseLeft)
}

func circulateWithRightClick(txt string) string {
	return makeClickable(txt, "bspc node @focused:/ --circulate forward", mouseRight)
}

func rotateWithMiddleClick(txt string) string {
	return makeClickable(txt, "bspc node @focused:/ --rotate 90", mouseMiddle)
}

func splitAtColon(str string) []string {
	return strings.Split(str, ":")
}

func makeItems(rawItems []string) []statusItem {
	statusItems := make([]statusItem, len(rawItems))
	for i, v := range rawItems {
		statusItems[i] = statusItem{v[:1], v[1:]}
	}
	return statusItems
}

// Layout can be "tiled" (T) or "monocle" (M)
func formatLayout(format string) string {
	if format == "T" {
		return makeClickable(tilingText, "bspc desktop -l tiled", mouseLeft)
	}
	return makeClickable(monocleText, "bspc desktop -l monocle", mouseLeft)
}

// State can be tiled (T), pseudo-tiled (P), floating (F), fullscreen (=), and parent (@)
func formatState(format string) string {
	switch format {
	case "T":
		return "|"
	case "P":
		return "¦"
	case "F":
		return "><>"
	case "=":
		return "[F]"
	case "@":
		return "@"
	}
	return format
}

// Flag can be marked (M), private (P), sticky (S), and locked (L) in any combination
func formatFlag(s string) string {
	// TODO
	return s
}

func applyMarkup(statusItems []statusItem) []string {
	markupedStrings := make([]string, len(statusItems))
	for i, v := range statusItems {
		switch v.Type {
		case "f":
			markupedStrings[i] = desktopButton(markFreeDesktop(markUnfocusedDesktop(v.Name)), v.Name)
		case "o":
			markupedStrings[i] = desktopButton(markOccupiedDesktop(markUnfocusedDesktop(v.Name)), v.Name)
		case "u":
			markupedStrings[i] = desktopButton(markUrgentDesktop(markUnfocusedDesktop(v.Name)), v.Name)
		case "F":
			markupedStrings[i] = desktopButton(markFreeDesktop(markFocusedDesktop(v.Name)), v.Name)
		case "O":
			markupedStrings[i] = desktopButton(markOccupiedDesktop(markFocusedDesktop(v.Name)), v.Name)
		case "U":
			markupedStrings[i] = desktopButton(markUrgentDesktop(markFocusedDesktop(v.Name)), v.Name)
		case "L":
			markupedStrings[i] = formatLayout(v.Name)
		case "T":
			markupedStrings[i] = formatState(v.Name)
		case "G":
			markupedStrings[i] = formatFlag(v.Name)
		case "M":
			if multimonitor {
				markupedStrings[i] = v.Name
			} else {
				markupedStrings[i] = ""
			}
		case "m":
			markupedStrings[i] = v.Name
		}
	}
	return markupedStrings
}

// FormatBSPWMStatus returns the string given by `bspc subscribe report` in a format digestible by lemonbar
func FormatBSPWMStatus(input string, multimonitor bool) string {
	return strings.Join(applyMarkup(makeItems(splitAtColon(input))), " ")
}
