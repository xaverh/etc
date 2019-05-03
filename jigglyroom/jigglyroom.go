package main

import (
	"fmt"
	"os"
	"reflect"
	"strings"

	"./config"
	"./lemonbar"
	"./modules/alignright"
	"./modules/bspwmstatus"
	"./modules/cputemperature"
	"./modules/dateandtime"
	"./modules/ipaddress"
	"./modules/memory"
	"./modules/menuarea"
	"./modules/networkthroughput"
	"./modules/power"
	"./modules/wifi"
	"./modules/windowtitle"
)

type module interface {
	PrintToChannel()
	GetOutputChannel() chan string
}

var screen = os.Args[1]

func main() {
	modules := make([]module, len(config.Modules))
	for i, v := range config.Modules {
		switch v {
		case "bspwmStatus":
			mod := bspwmstatus.BSPWMStatus{Output: make(chan string)}
			go mod.PrintToChannel()
			modules[i] = &mod
		case "memoryUsage":
			mod := memory.Memory{Output: make(chan string)}
			go mod.PrintToChannel()
			modules[i] = &mod
		case "networkThroughput":
			mod := networkthroughput.NetworkThroughput{Output: make(chan string)}
			go mod.PrintToChannel()
			modules[i] = &mod
		case "cpuTemperature":
			mod := cputemperature.CPUTemperature{Output: make(chan string)}
			go mod.PrintToChannel()
			modules[i] = &mod
		case "ipAddress":
			mod := ipaddress.IPAddress{Output: make(chan string)}
			go mod.PrintToChannel()
			modules[i] = &mod
		case "dateAndTime":
			mod := dateandtime.DateAndTime{Output: make(chan string)}
			go mod.PrintToChannel()
			modules[i] = &mod
		case "windowTitle":
			mod := windowtitle.WindowTitle{Output: make(chan string)}
			go mod.PrintToChannel()
			modules[i] = &mod
		case "power":
			mod := power.Power{Output: make(chan string)}
			go mod.PrintToChannel()
			modules[i] = &mod
		case "wifi":
			mod := wifi.WIFI{Output: make(chan string)}
			go mod.PrintToChannel()
			modules[i] = &mod
		case "alignRight":
			mod := alignright.AlignRight{Output: make(chan string)}
			go mod.PrintToChannel()
			modules[i] = &mod
		case "menuArea":
			mod := menuarea.MenuArea{Output: make(chan string)}
			go mod.PrintToChannel()
			modules[i] = &mod
		}
	}

	// https://stackoverflow.com/questions/19992334/how-to-listen-to-n-channels-dynamic-select-statement#
	cases := make([]reflect.SelectCase, len(modules))
	for i, ch := range modules {
		cases[i] = reflect.SelectCase{Dir: reflect.SelectRecv, Chan: reflect.ValueOf(ch.GetOutputChannel())}
	}

	results := make([]string, len(modules))
	for {
		i, v, ok := reflect.Select(cases)
		if !ok {
			cases = append(cases[:i], cases[i+1:]...)
		} else {
			results[i] = v.String()
			fmt.Println(strings.Join(results, lemonbar.SeparatorModules))
		}
	}
}
