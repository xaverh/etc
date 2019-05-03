package lemonbar

// SeparatorModules is the space between the modules
var SeparatorModules = "%{O12}"

// SeparatorWorkspaces is the space within the modules
var SeparatorWorkspaces = "%{O10}"

// AlignRight switches the alignment to the right
var AlignRight = "%{r}"

// Button is a representation of the mouse button usable for lemonbar
type Button string

const (
	MouseLeft   Button = "1"
	MouseMiddle Button = "2"
	MouseRight  Button = "3"
	MouseUp     Button = "4"
	MouseDown   Button = "5"
)

// ApplyColor colors the text with a color, selector can be B (background), F (foreground) and U (underline)
func ApplyColor(s string, color string, selector string) string {
	return "%{" + selector + color + "}" + s + "%{" + selector + "-}"
}

// ReverseColors swaps the foreground and the background color for the given item
func ReverseColors(s string) string {
	return "%{R}" + s + "%{R}"
}

// MakeClickable turns an text into a clickable lemonbar area
func MakeClickable(txt string, cmd string, b Button) string {
	return "%{A" + string(b) + ":" + cmd + ":}" + txt + "%{A}"
}
