package bspwmstatus

import (
	"bufio"
	"fmt"
	"os/exec"
	"strings"

	"../../lemonbar"
)

// monitor name only on multi-monitor, das muß irgendwo zwischen applyMarkup und makeItems angewandt werden
// not sure how {} must be processed for lemonbar, % appears to be the only character that needs to be escaped, und zwar %%
const (
	tagTiled       = "|"
	tagPseudoTiled = "¦"
	tagFloating    = "><>"
	tagFullscreen  = "[F]"
	tagParent      = "@"
	stickyText     = "~"
	markedText     = "*"
	lockedText     = "$"
	privateText    = "p"
	monocleText    = "[]"
	tilingText     = "##"
)

var multimonitor = false

type statusItem struct {
	Type string
	Name string
}

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
	return s
}

func markUrgentDesktop(s string) string {
	return s + "!"
}

func desktopButton(text string, desktop string) string {
	return lemonbar.MakeClickable(text, "bspc desktop "+desktop+" --focus ", lemonbar.MouseLeft)
}

func toggleLayoutWithLeftClick(txt string) string {
	return lemonbar.MakeClickable(txt, "bspc desktop focused --layout next", lemonbar.MouseLeft)
}

func circulateWithRightClick(txt string) string {
	return lemonbar.MakeClickable(txt, "bspc node @focused:/ --circulate forward", lemonbar.MouseRight)
}

func rotateWithMiddleClick(txt string) string {
	return lemonbar.MakeClickable(txt, "bspc node @focused:/ --rotate 90", lemonbar.MouseMiddle)
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
		return lemonbar.MakeClickable(tilingText, "bspc desktop -l tiled", lemonbar.MouseLeft)
	}
	return lemonbar.MakeClickable(monocleText, "bspc desktop -l monocle", lemonbar.MouseLeft)
}

// State can be tiled (T), pseudo-tiled (P), floating (F), fullscreen (=), and parent (@)
func formatState(format string) string {
	switch format {
	case "T":
		return tagTiled
	case "P":
		return tagPseudoTiled
	case "F":
		return tagFloating
	case "=":
		return tagFullscreen
	case "@":
		return tagParent
	}
	return format
}

// Flag can be marked (M), private (P), sticky (S), and locked (L) in any combination
func formatFlag(s string) string {
	return strings.Replace(strings.Replace(strings.Replace(strings.Replace(s, "P", privateText, 1), "S", stickyText, 1), "L", lockedText, 1), "M", markedText, 1)
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

func formatBSPWMStatus(input string, isMultimonitor bool) string {
	multimonitor = isMultimonitor
	return strings.Join(applyMarkup(makeItems(splitAtColon(input))), " ")
}

// BSPWMStatus reports the status of bspwm window manager
type BSPWMStatus struct {
	Output         chan string
	IsMultimonitor bool
}

func (bspwmStatus BSPWMStatus) PrintToChannel() {
	cmd := exec.Command("bspc", "subscribe", "report")
	out, err := cmd.StdoutPipe()

	err = cmd.Start()
	if err != nil {
		bspwmStatus.Output <- fmt.Sprintf("Failed to start err=%v", err)
	}
	scanner := bufio.NewScanner(out)
	scanner.Scan() // discard first empty Scan
	for ok := true; ok; ok = scanner.Scan() {
		bspwmStatus.Output <- formatBSPWMStatus(scanner.Text(), false)
	}
}

func (bspwmStatus BSPWMStatus) GetOutputChannel() chan string {
	return bspwmStatus.Output
}
