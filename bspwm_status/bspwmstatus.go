package main

import (
	"fmt"
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

var markupMap = map[string]string{
	"f": startLeftClickArea + gotoDesktop + "%q" + clickAreaCmdNameSep + "%[1]q" + endClickArea,
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

func trimW(str string) string {
	return strings.TrimPrefix(str, "W")
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

func applyMarkup(statusItems []statusItem) []string {
	markupedStrings := make([]string, len(statusItems))
	for i, v := range statusItems {
		markupedStrings[i] = fmt.Sprintf(markupMap[v.Type], v.Name)
	}
	return markupedStrings
}

// Layout can be "tiled" (T) or "monocle" (M)
func formatLayout(format string) string {
	if format == "T" {
		return startLeftClickArea + "bspc desktop -l monocle" + clickAreaCmdNameSep + "##" + endClickArea
	}
	return startLeftClickArea + "bspc desktop focused -l tiled" + clickAreaCmdNameSep + "[]" + endClickArea
}

func main() {
	fmt.Println(applyMarkup(makeItems(splitAtColon(trimW(testInput)))))
}
