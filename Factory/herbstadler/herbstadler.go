package main

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
)

func main() {
	keybinds, _ := exec.Command("/usr/bin/herbstclient", "list_keybinds").Output()
	p := exec.Command("rofi", "-dmenu", "-sort", "-i", "-p", "Keybinds")
	p.Stdin = strings.NewReader(string(keybinds))

	output, err := p.Output()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
	} else {
		herbstclientCommand := exec.Command("herbstclient", strings.Fields(string(output))[1:]...)
		herbstclientCommand.Run()
	}
}
