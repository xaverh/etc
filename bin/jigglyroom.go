package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"reflect"
	"regexp"
	"strconv"
	"strings"
	"time"
)

type formatting struct {
	BackgroundOpen      string
	BackgroundClose     string
	BackgroundReset     string
	ForegroundOpen      string
	ForegroundClose     string
	ForegroundReset     string
	LeftClickOpen       string
	LeftClickClose      string
	RightClickOpen      string
	RightClickClose     string
	ClickSeparator      string
	AlignRight          string
	SeparatorWorkspaces string
	SeparatorModules    string
}

type module interface {
	printToChannel()
	getOutputChannel() chan string
}

// Colors for all possible states a workspace can be in. Format: #rrggbb
type Colors struct {
	DefaultFg           string `json:"defaultFg"`
	DefaultBg           string `json:"defaultBg"`
	FreeFg              string `json:"freeFg"`
	FreeBg              string `json:"freeBg"`
	FocusedHereFg       string `json:"focusedHereFg"`
	FocusedHereBg       string `json:"focusedHereBg"`
	NotFocusedHereFg    string `json:"notFocusedHereFg"`
	NotFocusedHereBg    string `json:"notFocusedHereBg"`
	FocusedNotHereFg    string `json:"focusedNotHereFg"`
	FocusedNotHereBg    string `json:"focusedNotHereBg"`
	NotFocusedNotHereFg string `json:"notFocusedNotHereFg"`
	NotFocusedNotHereBg string `json:"notFocusedNotHereBg"`
	UrgentFg            string `json:"urgentFg"`
	UrgentBg            string `json:"urgentBg"`
	WindowTitleBg       string `json:"windowTitleBg"`
	WindowTitleFg       string `json:"windowTitleFg"`
}

type configuration struct {
	Modules             []string `json:"modules"` // values: memoryUsage, networkThroughput, cpuTemperature, ipAddress, dateAndTime, herbstluftwmStatus, power, wifi, alignRight, awesomeDummyOpen, awesomeDummyClose
	NetDevs             []string `json:"networkDevices"`
	Fahrenheit          bool     `json:"Fahrenheit"`
	ThermalZone         int      `json:"thermalZone"`
	DateAndTimeFormat   string   `json:"dateAndTimeFormat"`
	SeparatorModules    string   `json:"separatorModules"`
	SeparatorWorkspaces string   `json:"separatorWorkspaces"`
	WifiDevice          string   `json:"wifiDevice"`
	Colors              Colors   `json:"colors"`
}

func loadConfig(file string) configuration {
	var myConfig configuration
	configFile, err := os.Open(file)
	defer configFile.Close()
	if err != nil {
		fmt.Println(err.Error())
		// TODO: load defaults instead
	}
	jsonParser := json.NewDecoder(configFile)
	jsonParser.Decode(&myConfig)
	return myConfig
}

const (
	unpluggedSign      = "!"
	pluggedSign        = ""
	memIcon            = ""
	ipPattern          = "[0-9.]+"
	srcIPPattern       = "src [0-9.]+"
	bpsSign            = "B/s"
	kbpsSign           = "kB/s"
	mbpsSign           = "MB/s"
	gbpsSign           = "GB/s"
	netReceivedSign    = ""
	netTransmittedSign = ""
)

var (
	screen = os.Args[1]
	// TODO: command line option for config file
	config = loadConfig("/home/xha/.config/jigglyroom/config.json")
	format = formatting{
		BackgroundReset:     "%{B-}",
		BackgroundOpen:      "%{B",
		BackgroundClose:     "}",
		ForegroundReset:     "%{F-}",
		ForegroundOpen:      "%{F",
		ForegroundClose:     "}",
		LeftClickOpen:       "%{A1:",
		LeftClickClose:      "%{A}",
		RightClickOpen:      "%{A3:",
		RightClickClose:     "%{A}",
		ClickSeparator:      ":}",
		AlignRight:          "%{r}",
		SeparatorWorkspaces: config.SeparatorWorkspaces,
		SeparatorModules:    config.SeparatorModules,
	}
	networkThroughput = NetworkThroughput{}
	ssidRegex         = regexp.MustCompile("SSID: (.*?)\n")
	ipRegex           = regexp.MustCompilePOSIX(ipPattern)
	srcIPRegex        = regexp.MustCompilePOSIX(srcIPPattern)
)

// HerbstluftwmStatus gives the combined output of workspace names and window title of the currently opened window
type HerbstluftwmStatus struct {
	Output      chan string
	colors      Colors
	format      formatting
	screen      string
	doesRefresh bool
}

func formatHerbstluftwmStatus(input string, colors Colors, format formatting, screen string) string {
	items := strings.Split(strings.TrimSpace(input), "\t")
	result := ""
	fg := colors.DefaultFg
	bg := colors.DefaultBg
	for _, v := range items {
		switch v[:1] {
		case ".":
			// empty tag
			bg = colors.FreeBg
			fg = colors.FreeFg
		case ":":
			// occupied tag
			bg = colors.DefaultBg
			fg = colors.DefaultFg
		case "+":
			// viewed, here, !focused
			bg = colors.NotFocusedHereBg
			fg = colors.NotFocusedHereFg
		case "-":
			// viewed, !here, !focused
			bg = colors.NotFocusedNotHereBg
			fg = colors.NotFocusedNotHereFg
		case "%":
			// viewed, !here, focused
			bg = colors.FocusedNotHereBg
			fg = colors.FocusedNotHereFg
		case "#":
			// viewed, here, focused
			bg = colors.FocusedHereBg
			fg = colors.FocusedHereFg
		case "!":
			// urgent
			bg = colors.UrgentBg
			fg = colors.UrgentFg
		}
		result = result + format.BackgroundOpen + bg + format.BackgroundClose + format.ForegroundOpen + fg + format.ForegroundClose + format.LeftClickOpen + "herbstclient focus_monitor " + screen + " && herbstclient use " + v[1:] + format.ClickSeparator + format.SeparatorWorkspaces + v[1:] + format.SeparatorWorkspaces + format.LeftClickClose
	}
	return result + format.BackgroundOpen + colors.DefaultBg + format.BackgroundClose + format.ForegroundOpen + colors.DefaultFg + format.ForegroundClose
}

func (herbstluftwmStatus HerbstluftwmStatus) printToChannel() {
	cmd := exec.Command("herbstclient", "--idle")
	out, err := cmd.StdoutPipe()
	var workspaces string
	var windowTitle string

	err = cmd.Start()
	if err != nil {
		workspaces = fmt.Sprintf("Failed to start err=%v", err)
	}
	scanner := bufio.NewScanner(out)
	for ok := true; ok; ok = scanner.Scan() {
		action := strings.Split(scanner.Text(), "\t")
		switch action[0] {
		case "focus_changed":
			fallthrough
		case "window_title_changed":
			if len(action) >= 2 {
				windowTitle = herbstluftwmStatus.format.ForegroundOpen + herbstluftwmStatus.colors.WindowTitleFg + herbstluftwmStatus.format.ForegroundClose +
					herbstluftwmStatus.format.BackgroundOpen + herbstluftwmStatus.colors.WindowTitleBg + herbstluftwmStatus.format.BackgroundClose + herbstluftwmStatus.format.SeparatorModules + action[2] + herbstluftwmStatus.format.SeparatorModules + herbstluftwmStatus.format.BackgroundReset + herbstluftwmStatus.format.ForegroundReset
			} else {
				windowTitle = " "
			}
		default:
			out, err := exec.Command("herbstclient", "tag_status", herbstluftwmStatus.screen).Output()
			if err != nil {
				workspaces = "ERROR: Failed to display tags."
			} else {
				workspaces = formatHerbstluftwmStatus(string(out), herbstluftwmStatus.colors, herbstluftwmStatus.format, herbstluftwmStatus.screen)
			}
		}
		herbstluftwmStatus.Output <- workspaces + windowTitle
	}
	if err := scanner.Err(); err != nil {
		workspaces = fmt.Sprintf("reading standard input: %v", err)
	}
}

func (herbstluftwmStatus HerbstluftwmStatus) getOutputChannel() chan string {
	return herbstluftwmStatus.Output
}

// DateAndTime gives the current time. Format is defined
// through reference time Mon Jan 2 15:04:05 MST 2006 =
// 01/02 03:04:05PM '06 -0700 = Unix time 1136239445
type DateAndTime struct {
	Format      string
	Output      chan string
	doesRefresh bool
}

func (dateAndTime DateAndTime) printToChannel() {
	for {
		dateAndTime.Output <- time.Now().Local().Format(dateAndTime.Format)
		// sleep until beginning of next second
		var now = time.Now()
		time.Sleep(now.Truncate(time.Second).Add(time.Second).Sub(now))
	}
}

func (dateAndTime DateAndTime) getOutputChannel() chan string {
	return dateAndTime.Output
}

// AlignRight is a dummy module, that lets the user define, where the bar changes its alignment to the right
type AlignRight struct {
	Output chan string
}

func (alignRight AlignRight) printToChannel() {
	alignRight.Output <- format.AlignRight
}

func (alignRight AlignRight) getOutputChannel() chan string {
	return alignRight.Output
}

// AwesomeDummyOpen prepares the content for awesome to be consumed
type AwesomeDummyOpen struct {
	Output chan string
}

func (awesomeDummyOpen AwesomeDummyOpen) printToChannel() {
	awesomeDummyOpen.Output <- "jigglyroom.text=\""
}

func (awesomeDummyOpen AwesomeDummyOpen) getOutputChannel() chan string {
	return awesomeDummyOpen.Output
}

// AwesomeDummyClose prepares the content for awesome to be consumed
type AwesomeDummyClose struct {
	Output chan string
}

func (awesomeDummyClose AwesomeDummyClose) printToChannel() {
	awesomeDummyClose.Output <- "\""
}

func (awesomeDummyClose AwesomeDummyClose) getOutputChannel() chan string {
	return awesomeDummyClose.Output
}

// Memory gives the current usage of RAM accoring to /proc/meminfo
type Memory struct {
	Output      chan string
	doesRefresh bool
}

func (memory Memory) printToChannel() {
	for {
		var file, err = os.Open("/proc/meminfo")
		if err != nil {
			memory.Output <- memIcon + "err"
		}

		// done must equal the flag combination (0001 | 0010 | 0100 | 1000) = 15
		var used, done = 0, 0
		for info := bufio.NewScanner(file); done != 15 && info.Scan(); {
			var prop, val = "", 0
			if _, err = fmt.Sscanf(info.Text(), "%s %d", &prop, &val); err != nil {
				memory.Output <- memIcon + "err"
			}
			switch prop {
			case "MemTotal:":
				used += val
				done |= 1
			case "MemFree:":
				used -= val
				done |= 2
			case "Buffers:":
				used -= val
				done |= 4
			case "Cached:":
				used -= val
				done |= 8
			}
		}
		file.Close()
		memory.Output <- fmt.Sprintf("%s%d MB", memIcon, used/1000)
		time.Sleep(time.Duration(2 * time.Second))
	}
}

func (memory Memory) getOutputChannel() chan string {
	return memory.Output
}

// CPUTemperature returns the current temperature of the specified heat sink, can be Fahrenheit or Celsius
type CPUTemperature struct {
	Fahrenheit  bool
	ThermalZone int
	Output      chan string
	doesRefresh bool
}

func (cpuTemperature CPUTemperature) printToChannel() {
	for {
		var temp, err = ioutil.ReadFile("/sys/class/thermal/thermal_zone" + strconv.Itoa(cpuTemperature.ThermalZone) + "/temp")
		if err != nil {
			cpuTemperature.Output <- "temp unknown"
		}
		if cpuTemperature.Fahrenheit {
			celsius, _ := strconv.Atoi(string(temp)[:len(temp)-4])
			fahrenheit := (celsius*18 + 320) / 10
			cpuTemperature.Output <- fmt.Sprintf("%v °F", fahrenheit)
		} else {
			cpuTemperature.Output <- fmt.Sprintf("%s °C", string(temp)[:len(temp)-4])
		}
		time.Sleep(time.Duration(2 * time.Second))
	}
}

func (cpuTemperature CPUTemperature) getOutputChannel() chan string {
	return cpuTemperature.Output
}

// WIFI returns the name of the current wifi (SSID) and the last 6 digits of its router's MAC address (BSSID)
type WIFI struct {
	Output      chan string
	NetDev      string
	doesRefresh bool
}

func (wifi WIFI) printToChannel() {
	sleepTime := 3 * time.Second
	for {
		iwOutput, err := exec.Command("iw", "dev", "wlp2s0", "link").Output()
		if err != nil {
			wifi.Output <- ""
		} else {
			if string(iwOutput) != "Not connected.\n" {
				ssidString := ssidRegex.FindStringSubmatch(string(iwOutput))[0]
				wifi.Output <- ssidString[6:len(ssidString)-1] + " " + string(iwOutput)[22:30]
			} else {
				wifi.Output <- "no WIFI"
			}
		}
		time.Sleep(time.Duration(sleepTime))
	}
}

func (wifi WIFI) getOutputChannel() chan string {
	return wifi.Output
}

// IPAddress returns the current local IPv4 address used for accessing the internet
type IPAddress struct {
	Output      chan string
	doesRefresh bool
}

func (ipAddress IPAddress) printToChannel() {
	for {
		out, err := exec.Command("ip", "route", "get", "8.8.8.8").Output()
		if err != nil {
			ipAddress.Output <- "offline"
		} else {
			ipAddress.Output <- ipRegex.FindString(srcIPRegex.FindString(string(out)))
		}
		time.Sleep(time.Duration(3 * time.Second))
	}
}

func (ipAddress IPAddress) getOutputChannel() chan string {
	return ipAddress.Output
}

// NetworkThroughput gives information about the up- and downstream of given network devices
type NetworkThroughput struct {
	NetDevs     []string
	Output      chan string
	doesRefresh bool
}

func fixed(pre string, rate int) string {
	if rate < 0 {
		return pre + " err"
	}

	var suf = kbpsSign
	if rate > 1000 {
		var result = float64(rate)

		switch {
		case rate >= (1000000000): // GB/s
			result /= 1000000000.0
			suf = gbpsSign
		case rate >= (1000000): // MB/s
			result /= 1000000.0
			suf = mbpsSign
		case rate >= (1000): // kB/s
			result /= 1000.0
			suf = kbpsSign
		default:
			suf = bpsSign
		}

		return fmt.Sprintf("%s%.1f %s", pre, result, suf)

	}
	return fmt.Sprintf("%s%d %s", pre, rate, suf)
}

func (networkThroughput NetworkThroughput) printToChannel() {
	rxOld := 0
	txOld := 0
	for {
		file, err := os.Open("/proc/net/dev")
		if err != nil {
			networkThroughput.Output <- netReceivedSign + " err " + netTransmittedSign + " err"
		}

		var void = 0 // target for unused values
		var dev, rx, tx, rxNow, txNow = "", 0, 0, 0, 0
		var scanner = bufio.NewScanner(file)
		for scanner.Scan() {
			_, err = fmt.Sscanf(scanner.Text(), "%s %d %d %d %d %d %d %d %d %d",
				&dev, &rx, &void, &void, &void, &void, &void, &void, &void, &tx)
			dev = strings.TrimSuffix(dev, ":")
			for _, val := range networkThroughput.NetDevs {
				if val == dev {
					rxNow += rx
					txNow += tx
				}
			}
		}
		file.Close()
		networkThroughput.Output <- fmt.Sprintf("%s %s", fixed(netReceivedSign, rxNow-rxOld), fixed(netTransmittedSign, txNow-txOld))
		rxOld, txOld = rxNow, txNow
		time.Sleep(time.Duration(time.Second))
	}
}

func (networkThroughput NetworkThroughput) getOutputChannel() chan string {
	return networkThroughput.Output
}

// Power returns the current state of the battery, only useful for laptops
type Power struct {
	Output        chan string
	unpluggedSign string
	pluggedSign   string
	doesRefresh   bool
}

func (power Power) printToChannel() {
	const powerSupply = "/sys/class/power_supply/"
	var enFull, enNow, enPerc int = 0, 0, 0
	for {
		var plugged, err = ioutil.ReadFile(powerSupply + "AC/online")
		if err != nil {
			power.Output <- ""
			time.Sleep(time.Duration(1000 * time.Second))
			break
		}
		batts, err := ioutil.ReadDir(powerSupply)
		if err != nil {
			power.Output <- "no battery"
			time.Sleep(time.Duration(1000 * time.Second))
			break
		}

		readval := func(name, field string) int {
			var path = powerSupply + name + "/"
			var file []byte
			if tmp, err := ioutil.ReadFile(path + "energy_" + field); err == nil {
				file = tmp
			} else if tmp, err := ioutil.ReadFile(path + "charge_" + field); err == nil {
				file = tmp
			} else {
				return 0
			}

			if ret, err := strconv.Atoi(strings.TrimSpace(string(file))); err == nil {
				return ret
			}
			return 0
		}

		for _, batt := range batts {
			name := batt.Name()
			if !strings.HasPrefix(name, "BAT") {
				continue
			}

			enFull += readval(name, "full")
			enNow += readval(name, "now")
		}

		if enFull == 0 {
			power.Output <- "Battery found but no readable full file"
		}

		enPerc = enNow * 100 / enFull
		var icon = power.unpluggedSign
		if string(plugged) == "1\n" {
			icon = power.pluggedSign
		}

		power.Output <- fmt.Sprintf("%d%%%s", enPerc, icon)
		time.Sleep(time.Duration(10 * time.Second))
	}
}

func (power Power) getOutputChannel() chan string {
	return power.Output
}

func main() {
	modules := make([]module, len(config.Modules))
	for i, v := range config.Modules {
		switch v {
		case "memoryUsage":
			mod := Memory{Output: make(chan string), doesRefresh: false}
			go mod.printToChannel()
			modules[i] = &mod
		case "networkThroughput":
			mod := NetworkThroughput{Output: make(chan string), NetDevs: config.NetDevs, doesRefresh: false}
			go mod.printToChannel()
			modules[i] = &mod
		case "cpuTemperature":
			mod := CPUTemperature{Output: make(chan string), Fahrenheit: config.Fahrenheit, ThermalZone: config.ThermalZone, doesRefresh: false}
			go mod.printToChannel()
			modules[i] = &mod
		case "ipAddress":
			mod := IPAddress{Output: make(chan string), doesRefresh: false}
			go mod.printToChannel()
			modules[i] = &mod
		case "dateAndTime":
			mod := DateAndTime{Output: make(chan string), Format: config.DateAndTimeFormat, doesRefresh: true}
			go mod.printToChannel()
			modules[i] = &mod
		case "herbstluftwmStatus":
			mod := HerbstluftwmStatus{Output: make(chan string), colors: config.Colors, format: format, screen: screen, doesRefresh: true}
			go mod.printToChannel()
			modules[i] = &mod
		case "power":
			mod := Power{Output: make(chan string), unpluggedSign: unpluggedSign, pluggedSign: pluggedSign, doesRefresh: false}
			go mod.printToChannel()
			modules[i] = &mod
		case "wifi":
			mod := WIFI{Output: make(chan string), NetDev: config.WifiDevice, doesRefresh: false}
			go mod.printToChannel()
			modules[i] = &mod
		case "alignRight":
			mod := AlignRight{Output: make(chan string)}
			go mod.printToChannel()
			modules[i] = &mod
		case "awesomeDummyOpen":
			mod := AwesomeDummyOpen{Output: make(chan string)}
			go mod.printToChannel()
			modules[i] = &mod
		case "awesomeDummyClose":
			mod := AwesomeDummyClose{Output: make(chan string)}
			go mod.printToChannel()
			modules[i] = &mod
		}
	}

	// https://stackoverflow.com/questions/19992334/how-to-listen-to-n-channels-dynamic-select-statement#
	cases := make([]reflect.SelectCase, len(modules))
	for i, ch := range modules {
		cases[i] = reflect.SelectCase{Dir: reflect.SelectRecv, Chan: reflect.ValueOf(ch.getOutputChannel())}
	}

	results := make([]string, len(modules))
	for {
		i, v, ok := reflect.Select(cases)
		if !ok {
			cases = append(cases[:i], cases[i+1:]...)
		} else {
			results[i] = v.String()
			exec.Command("/usr/bin/awesome-client",strings.Join(results, config.SeparatorModules)).Start()
		}
	}
}
