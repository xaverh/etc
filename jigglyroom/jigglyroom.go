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

	"./bspwmstatus"
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
	WorkspacePagerBg    string `json:"workspacePagerBg"`
	WorkspacePagerFg    string `json:"workspacePagerFg"`
}

type configuration struct {
	Modules             []string `json:"modules"` // values: memoryUsage, networkThroughput, cpuTemperature, ipAddress, dateAndTime, herbstluftwmStatus, power, wifi, alignRight, windowTitle, workspacePager, menuArea
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

// WorkspacePager prints the number of workspaces and the currently selected one with help of `xprop`
type WorkspacePager struct {
	Output chan string
	colors Colors
	format formatting
}

func formatWorkspacePager(current string, total int, colors Colors, format formatting) string {
	currentNumber, _ := strconv.Atoi(current)
	pager := ""
	for i := 0; i < total; i++ {
		workspaceString := strconv.Itoa(i)
		workspaceName := strconv.Itoa((i + 1) % 10)
		pager += format.LeftClickOpen + "xdotool set_desktop " + workspaceString + format.ClickSeparator + format.SeparatorWorkspaces + workspaceName
		if i == currentNumber {
			pager += "*"
		} else {
			pager += " "
		}
		pager += format.SeparatorWorkspaces + format.LeftClickClose
	}
	return format.BackgroundOpen + colors.WorkspacePagerBg + format.BackgroundClose + format.ForegroundOpen + colors.WorkspacePagerFg + format.ForegroundClose + pager + format.SeparatorModules + format.BackgroundReset + format.ForegroundReset
}

func (workspacePager WorkspacePager) printToChannel() {
	workspaces := ""
	netNumberOfDesktops, err := exec.Command("xprop", "-root", "_NET_NUMBER_OF_DESKTOPS").Output()
	numberOfWorkspaces, err2 := strconv.Atoi((strings.TrimPrefix(strings.TrimSpace(string(netNumberOfDesktops)), "_NET_NUMBER_OF_DESKTOPS(CARDINAL) = ")))
	if !(err == nil && err2 == nil) {
		workspaces = "ERROR: Failed to display tags."
	}

	cmd := exec.Command("xprop", "-spy", "-root", "_NET_CURRENT_DESKTOP")
	netCurrentDesktop, err := cmd.StdoutPipe()

	err = cmd.Start()
	if err != nil {
		workspaces = fmt.Sprintf("Failed to start err=%v", err)
	}

	desktopScanner := bufio.NewScanner(netCurrentDesktop)
	for ok := true; ok; ok = desktopScanner.Scan() {
		workspaces = formatWorkspacePager(strings.TrimPrefix(strings.TrimSpace(string(desktopScanner.Text())), "_NET_CURRENT_DESKTOP(CARDINAL) = "), numberOfWorkspaces, workspacePager.colors, workspacePager.format)
		workspacePager.Output <- workspaces
	}
}

func (workspacePager WorkspacePager) getOutputChannel() chan string {
	return workspacePager.Output
}

// WindowTitle prints the title of the current window with help of `xtitle`
type WindowTitle struct {
	Output chan string
	colors Colors
	format formatting
	screen string
}

func formatWindowTitle(input string, colors Colors, format formatting, screen string) string {
	return format.BackgroundOpen + colors.WindowTitleBg + format.BackgroundClose + format.ForegroundOpen + colors.WindowTitleFg + format.ForegroundClose + format.SeparatorModules + input + format.SeparatorModules + format.BackgroundReset + format.ForegroundReset
}

func (windowTitle WindowTitle) printToChannel() {
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
		title = formatWindowTitle(string(scanner.Text()), windowTitle.colors, windowTitle.format, windowTitle.screen)
		windowTitle.Output <- title
	}
}

func (windowTitle WindowTitle) getOutputChannel() chan string {
	return windowTitle.Output
}

// HerbstluftwmStatus gives the combined output of workspace names and window title of the currently opened window in herbstluftwm
type HerbstluftwmStatus struct {
	Output chan string
	colors Colors
	format formatting
	screen string
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
	Format string
	Output chan string
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

// MenuArea is a dummy module, that lets the user define, where the menu can be accessed
type MenuArea struct {
	Output chan string
}

func (menuarea MenuArea) printToChannel() {
	menuarea.Output <- format.LeftClickOpen + "9menu_run -font " + "-sgi-screen-medium-r-normal--14-140-72-72-m-70-iso8859-1" + " -fg '#e5e6e6' -bg '#1e1e1e' " + format.ClickSeparator + " " + format.LeftClickClose
}

func (menuarea MenuArea) getOutputChannel() chan string {
	return menuarea.Output
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

// Memory gives the current usage of RAM accoring to /proc/meminfo
type Memory struct {
	Output chan string
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
		time.Sleep(time.Duration(5 * time.Second))
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
		time.Sleep(time.Duration(7 * time.Second))
	}
}

func (cpuTemperature CPUTemperature) getOutputChannel() chan string {
	return cpuTemperature.Output
}

// WIFI returns the name of the current wifi (SSID) and the last 6 digits of its router's MAC address (BSSID)
type WIFI struct {
	Output chan string
	NetDev string
}

func (wifi WIFI) printToChannel() {
	sleepTime := 3 * time.Second
	for {
		iwOutput, err := exec.Command("/usr/sbin/iw", "dev", config.WifiDevice, "link").Output()
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
	Output chan string
}

func (ipAddress IPAddress) printToChannel() {
	time.Sleep(time.Duration(3 * time.Second))
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
	NetDevs []string
	Output  chan string
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
	rate := 2
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
		networkThroughput.Output <- fmt.Sprintf("%s%s%s", fixed(netReceivedSign, (rxNow-rxOld)/rate), format.SeparatorModules, fixed(netTransmittedSign, (txNow-txOld)/rate))
		rxOld, txOld = rxNow, txNow
		time.Sleep(time.Duration(2) * time.Second)
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
}

func (power Power) printToChannel() {
	const powerSupply = "/sys/class/power_supply/"
	var enFull, enNow, enPerc int = 0, 0, 0
	for {
		var plugged, err = ioutil.ReadFile(powerSupply + "AC/online")
		if err != nil {
			power.Output <- ""
			time.Sleep(time.Duration(10007 * time.Second))
			break
		}
		batts, err := ioutil.ReadDir(powerSupply)
		if err != nil {
			power.Output <- "no battery"
			time.Sleep(time.Duration(10007 * time.Second))
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
		time.Sleep(time.Duration(13 * time.Second))
	}
}

func (power Power) getOutputChannel() chan string {
	return power.Output
}

// BSPWMStatus reports the status of bspwm window manager
type BSPWMStatus struct {
	Output         chan string
	IsMultimonitor bool
}

func (bspwmStatus BSPWMStatus) printToChannel() {
	bspwmStatus.Output <- bspwmstatus.FormatBSPWMStatus(`WMHDMI-1:OI:f{}:f3:oDéjà-vu:ffünf:fVI:fdie sieben:fVIII:oШОС:f0:LT:TT:GSPLM`, false)
}

func (bspwmStatus BSPWMStatus) getOutputChannel() chan string {
	return bspwmStatus.Output
}

func main() {
	modules := make([]module, len(config.Modules))
	for i, v := range config.Modules {
		switch v {
		case "bspwmStatus":
			mod := BSPWMStatus{Output: make(chan string)}
			go mod.printToChannel()
			modules[i] = &mod
		case "memoryUsage":
			mod := Memory{Output: make(chan string)}
			go mod.printToChannel()
			modules[i] = &mod
		case "networkThroughput":
			mod := NetworkThroughput{Output: make(chan string), NetDevs: config.NetDevs}
			go mod.printToChannel()
			modules[i] = &mod
		case "cpuTemperature":
			mod := CPUTemperature{Output: make(chan string), Fahrenheit: config.Fahrenheit, ThermalZone: config.ThermalZone}
			go mod.printToChannel()
			modules[i] = &mod
		case "ipAddress":
			mod := IPAddress{Output: make(chan string)}
			go mod.printToChannel()
			modules[i] = &mod
		case "dateAndTime":
			mod := DateAndTime{Output: make(chan string), Format: config.DateAndTimeFormat}
			go mod.printToChannel()
			modules[i] = &mod
		case "herbstluftwmStatus":
			mod := HerbstluftwmStatus{Output: make(chan string), colors: config.Colors, format: format, screen: screen}
			go mod.printToChannel()
			modules[i] = &mod
		case "windowTitle":
			mod := WindowTitle{Output: make(chan string), colors: config.Colors, format: format, screen: screen}
			go mod.printToChannel()
			modules[i] = &mod
		case "workspacePager":
			mod := WorkspacePager{Output: make(chan string), colors: config.Colors, format: format}
			go mod.printToChannel()
			modules[i] = &mod
		case "power":
			mod := Power{Output: make(chan string), unpluggedSign: unpluggedSign, pluggedSign: pluggedSign}
			go mod.printToChannel()
			modules[i] = &mod
		case "wifi":
			mod := WIFI{Output: make(chan string), NetDev: config.WifiDevice}
			go mod.printToChannel()
			modules[i] = &mod
		case "alignRight":
			mod := AlignRight{Output: make(chan string)}
			go mod.printToChannel()
			modules[i] = &mod
		case "menuArea":
			mod := MenuArea{Output: make(chan string)}
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
			fmt.Println(strings.Join(results, config.SeparatorModules))
		}
	}
}
