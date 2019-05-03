package windowtitle

import (
	"bufio"
	"fmt"
	"os/exec"

	"../../config"
	"../../lemonbar"
)

// WindowTitle prints the title of the current window with help of `xtitle`
type WindowTitle struct {
	Output chan string
	screen string
}

func formatWindowTitle(input string) string {
	return lemonbar.ApplyColor(lemonbar.ApplyColor(input, config.CurrentColorScheme.FocusedHereBg, "B"), config.CurrentColorScheme.FocusedHereFg, "F")
}

func (windowTitle WindowTitle) PrintToChannel() {
	cmd := exec.Command("xtitle", "-s")
	out, err := cmd.StdoutPipe()
	var title string

	title = ""
	err = cmd.Start()
	if err != nil {
		title = fmt.Sprintf("Failed to start err=%v", err)
	}

	scanner := bufio.NewScanner(out)
	for ok := true; ok; ok = scanner.Scan() {
		title = formatWindowTitle(string(scanner.Text()))
		windowTitle.Output <- title
	}
}

func (windowTitle WindowTitle) GetOutputChannel() chan string {
	return windowTitle.Output
}
