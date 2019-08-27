package main

import (
	"bufio"
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

var (
	// ThermalZone is '*' in filename /sys/class/thermal/thermal_zone/*/temp
	ThermalZone = "1"

	// Modules should not be strings but rather the modules themselves // XXX
	Modules = []string{
		"menuArea",
		"bspwmStatus",
		"windowTitle",
		"alignRight",
		"memoryUsage",
		"networkThroughput",
		"cpuTemperature",
		"wifi",
		"ipAddress",
		"dateAndTime",
	}

	// NetworkDevices are all network devices whose up- and downstream should be monitored
	NetworkDevices = []string{
		"eno1",
		"wlp2s0",
	}

	// WIFIDevice - the primary WIFI device
	WIFIDevice = "wlp2s0"

	// DateAndTimeFormat is 2007-01-02 15:04:05 +06 in the preferred Format
	DateAndTimeFormat = "Mon 2 Jan 15:04:05 MST"

	// DarkScheme ...
	DarkScheme = ColorScheme{
		DefaultFg:           "#E5E6E6",
		DefaultBg:           "#1E1E1E",
		FreeFg:              "#969696",
		FreeBg:              "#1E1E1E",
		FocusedHereFg:       "#E5E6E6",
		FocusedHereBg:       "#005577",
		NotFocusedHereFg:    "#E5E6E6",
		NotFocusedHereBg:    "#81D8D0",
		FocusedNotHereFg:    "#005577",
		FocusedNotHereBg:    "#1E1E1E",
		NotFocusedNotHereFg: "#81D8D0",
		NotFocusedNotHereBg: "#1E1E1E",
		UrgentFg:            "#E5E6E6",
		UrgentBg:            "#E32791",
		WindowTitleBg:       "#1E1E1E",
		WindowTitleFg:       "#E5E6E6",
		WorkspacePagerBg:    "#1E1E1E",
		WorkspacePagerFg:    "#E5E6E6",
	}

	// LightScheme ...
	LightScheme = ColorScheme{
		DefaultFg:           "#424242",
		DefaultBg:           "#E5E6E8",
		FreeFg:              "#969696",
		FreeBg:              "#1E1E1E",
		FocusedHereFg:       "#E5E6E6",
		FocusedHereBg:       "#005577",
		NotFocusedHereFg:    "#E5E6E6",
		NotFocusedHereBg:    "#81D8D0",
		FocusedNotHereFg:    "#005577",
		FocusedNotHereBg:    "#1E1E1E",
		NotFocusedNotHereFg: "#81D8D0",
		NotFocusedNotHereBg: "#1E1E1E",
		UrgentFg:            "#E5E6E6",
		UrgentBg:            "#E32791",
		WindowTitleBg:       "#1E1E1E",
		WindowTitleFg:       "#E5E6E6",
		WorkspacePagerBg:    "#1E1E1E",
		WorkspacePagerFg:    "#E5E6E6",
	}

	// CurrentColorScheme always points to the currently selected scheme
	CurrentColorScheme = &DarkScheme

	// UnpluggedSign indicates a discharging battery
	UnpluggedSign = "!"
	// PluggedSign indicates a loading/not-discharging battery
	PluggedSign = ""
	// MemIcon is how the used memory in MB should be displayed
	MemIcon = ""
	// BpsSign is the symbol for 10⁰ Bytes/s
	BpsSign = "B/s"
	// KbpsSign is the symbol for 10³ Bytes/s
	KbpsSign = "kB/s"
	// MbpsSign is the symbol for 10⁶ Bytes/s
	MbpsSign = "MB/s"
	// GbpsSign is the symbol for 10⁹ Bytes/s
	GbpsSign = "GB/s"
	// NetReceivedSign indicates downstream
	NetReceivedSign = ""
	// NetTransmittedSign indicates upstream
	NetTransmittedSign = ""
)

// SeparatorModules is the space between the modules
var SeparatorModules = "%{O12}"

// SeparatorWorkspaces is the space within the modules
var SeparatorWorkspaces = "%{O16}"

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

// AlignRight is a dummy module, that lets the user define, where the bar changes its alignment to the right
type AlignRight struct {
	Output chan string
}

func (alignRight AlignRight) PrintToChannel() {
	alignRight.Output <- lemonbar.AlignRight
}

func (alignRight AlignRight) GetOutputChannel() chan string {
	return alignRight.Output
}

// CPUTemperature returns the current temperature of the specified heat sink
type CPUTemperature struct {
	Output chan string
}

func (cpuTemperature CPUTemperature) PrintToChannel() {
	for {
		var temp, err = ioutil.ReadFile("/sys/class/thermal/thermal_zone" + config.ThermalZone + "/temp")
		if err != nil {
			cpuTemperature.Output <- "temp unknown"
		}
		cpuTemperature.Output <- fmt.Sprintf("%s °C", string(temp)[:len(temp)-4])
		time.Sleep(time.Duration(7 * time.Second))
	}
}

func (cpuTemperature CPUTemperature) GetOutputChannel() chan string {
	return cpuTemperature.Output
}

// DateAndTime gives the current time. Format is defined
// through reference time Mon Jan 2 15:04:05 MST 2006 =
// 01/02 03:04:05PM '06 -0700 = Unix time 1136239445
type DateAndTime struct {
	Format string
	Output chan string
}

func (dateAndTime DateAndTime) PrintToChannel() {
	for {
		dateAndTime.Output <- time.Now().Local().Format(config.DateAndTimeFormat)
		// sleep until beginning of next second
		var now = time.Now()
		time.Sleep(now.Truncate(time.Second).Add(time.Second).Sub(now))
	}
}

func (dateAndTime DateAndTime) GetOutputChannel() chan string {
	return dateAndTime.Output
}

var (
	ipRegex    = regexp.MustCompilePOSIX("[0-9.]+")
	srcIPRegex = regexp.MustCompilePOSIX("src [0-9.]+")
)

// IPAddress returns the current local IPv4 address used for accessing the internet
type IPAddress struct {
	Output chan string
}

func (ipAddress IPAddress) PrintToChannel() {
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

func (ipAddress IPAddress) GetOutputChannel() chan string {
	return ipAddress.Output
}

// Memory gives the current usage of RAM accoring to /proc/meminfo
type Memory struct {
	Output chan string
}

func (memory Memory) PrintToChannel() {
	for {
		var file, err = os.Open("/proc/meminfo")
		if err != nil {
			memory.Output <- config.MemIcon + "err"
		}

		// done must equal the flag combination (0001 | 0010 | 0100 | 1000) = 15
		var used, done = 0, 0
		for info := bufio.NewScanner(file); done != 15 && info.Scan(); {
			var prop, val = "", 0
			if _, err = fmt.Sscanf(info.Text(), "%s %d", &prop, &val); err != nil {
				memory.Output <- config.MemIcon + "err"
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
		memory.Output <- fmt.Sprintf("%s%d MB", config.MemIcon, used/1000)
		time.Sleep(time.Duration(5 * time.Second))
	}
}

func (memory Memory) GetOutputChannel() chan string {
	return memory.Output
}

// MenuArea is a dummy module, that lets the user define, where the menu can be accessed
type MenuArea struct {
	Output chan string
}

func (menuarea MenuArea) PrintToChannel() {
	menuarea.Output <- lemonbar.MakeClickable("%%", `9menu_run -font -sgi-screen-medium-r-normal--14-140-72-72-m-70-iso8859-1 -fg '#e5e6e6' -bg '#1e1e1e' `, lemonbar.MouseLeft)
}

func (menuarea MenuArea) GetOutputChannel() chan string {
	return menuarea.Output
}

// NetworkThroughput gives information about the up- and downstream of given network devices
type NetworkThroughput struct {
	Output chan string
}

func fixed(pre string, rate int) string {
	if rate < 0 {
		return pre + " err"
	}

	var suf = config.KbpsSign
	if rate > 1000 {
		var result = float64(rate)

		switch {
		case rate >= (1000000000): // GB/s
			result /= 1000000000.0
			suf = config.GbpsSign
		case rate >= (1000000): // MB/s
			result /= 1000000.0
			suf = config.MbpsSign
		case rate >= (1000): // kB/s
			result /= 1000.0
			suf = config.KbpsSign
		default:
			suf = config.BpsSign
		}

		return fmt.Sprintf("%s%.1f %s", pre, result, suf)

	}
	return fmt.Sprintf("%s%d %s", pre, rate, suf)
}

func (networkThroughput NetworkThroughput) PrintToChannel() {
	rxOld := 0
	txOld := 0
	rate := 2
	for {
		file, err := os.Open("/proc/net/dev")
		if err != nil {
			networkThroughput.Output <- config.NetReceivedSign + " err " + config.NetTransmittedSign + " err"
		}

		var void = 0 // target for unused values
		var dev, rx, tx, rxNow, txNow = "", 0, 0, 0, 0
		var scanner = bufio.NewScanner(file)
		for scanner.Scan() {
			_, err = fmt.Sscanf(scanner.Text(), "%s %d %d %d %d %d %d %d %d %d",
				&dev, &rx, &void, &void, &void, &void, &void, &void, &void, &tx)
			dev = strings.TrimSuffix(dev, ":")
			for _, val := range config.NetworkDevices {
				if val == dev {
					rxNow += rx
					txNow += tx
				}
			}
		}
		file.Close()
		networkThroughput.Output <- fmt.Sprintf("%s%s%s", fixed(config.NetReceivedSign, (rxNow-rxOld)/rate), lemonbar.SeparatorModules, fixed(config.NetTransmittedSign, (txNow-txOld)/rate))
		rxOld, txOld = rxNow, txNow
		time.Sleep(time.Duration(2) * time.Second)
	}
}

func (networkThroughput NetworkThroughput) GetOutputChannel() chan string {
	return networkThroughput.Output
}

// Power returns the current state of the battery, only useful for laptops
type Power struct {
	Output        chan string
	unpluggedSign string
	pluggedSign   string
}

func (power Power) PrintToChannel() {
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

		power.Output <- fmt.Sprintf("%d%%%%%s", enPerc, icon)
		time.Sleep(time.Duration(13 * time.Second))
	}
}

func (power Power) GetOutputChannel() chan string {
	return power.Output
}

var ssidRegex = regexp.MustCompile("SSID: (.*?)\n")

// WIFI returns the name of the current wifi (SSID) and the last 6 digits of its router's MAC address (BSSID)
type WIFI struct {
	Output chan string
	NetDev string
}

func (wifi WIFI) PrintToChannel() {
	sleepTime := 3 * time.Second
	for {
		iwOutput, err := exec.Command("/usr/sbin/iw", "dev", config.WIFIDevice, "link").Output()
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

func (wifi WIFI) GetOutputChannel() chan string {
	return wifi.Output
}

type module interface {
	PrintToChannel()
	GetOutputChannel() chan string
}

var screen = os.Args[1]

func main() {
	modules := make([]module, len(config.Modules))
	for i, v := range config.Modules {
		switch v {
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
