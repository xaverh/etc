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

const (
	// thermalZone is '*' in filename /sys/class/thermal/thermal_zone/*/temp
	thermalZone       = "1"
	wifiDevice        = "wlan0"
	dateAndTimeFormat = "Mon 2 Jan 15:04:05 MST"
	unpluggedSign     = "!"
	pluggedSign       = ""
	separatorModules  = "%{O12}"
	mouseLeft         = "1"
	mouseMiddle       = "2"
	mouseRight        = "3"
	mouseUp           = "4"
	mouseDown         = "5"
)

var (
	// Modules should not be strings but rather the modules themselves // XXX
	modules = []string{
		"alignRight",
		"memoryUsage",
		"networkThroughput",
		"cpuTemperature",
		"wifi",
		"ipAddress",
		"dateAndTime",
	}

	networkDevices = []string{
		"eno1",
		"wlan0",
	}

	ipRegex    = regexp.MustCompilePOSIX("[0-9.]+")
	srcIPRegex = regexp.MustCompilePOSIX("src [0-9.]+")
	ssidRegex  = regexp.MustCompile("SSID: (.*?)\n")
)

func makeClickable(txt string, cmd string, button string) string {
	return "%{A" + button + ":" + cmd + ":}" + txt + "%{A}"
}

func fixed(rate int) string {
	if rate < 0 {
		return "err"
	}

	suf := "kB/s"
	if rate > 1000 {
		var result = float64(rate)

		switch {
		case rate >= (1000000000):
			result /= 1000000000.0
			suf = "GB/s"
		case rate >= (1000000):
			result /= 1000000.0
			suf = "MB/s"
		case rate >= (1000):
			result /= 1000.0
			suf = "kB/s"
		default:
			suf = "B/s"
		}

		return fmt.Sprintf("%.1f %s", result, suf)

	}
	return fmt.Sprintf("%d %s", rate, suf)
}

type alignRight struct {
	Output chan string
}

type cpuTemperature struct {
	Output chan string
}

type dateAndTime struct {
	Output chan string
}

type ipv4Address struct {
	Output chan string
}

type memory struct {
	Output chan string
}

type networkThroughput struct {
	Output chan string
}

type power struct {
	Output chan string
}

type wifi struct {
	Output chan string
}

type barModule interface {
	printToChannel()
	getOutputChannel() chan string
}

func (ar alignRight) printToChannel() {
	ar.Output <- "%{r}"
}

func (ar alignRight) getOutputChannel() chan string {
	return ar.Output
}

func (θ cpuTemperature) printToChannel() {
	for {
		var temp, err = ioutil.ReadFile("/sys/class/thermal/thermal_zone" + thermalZone + "/temp")
		if err != nil {
			θ.Output <- "temp unknown"
		}
		θ.Output <- fmt.Sprintf("%s °C", string(temp)[:len(temp)-4])
		time.Sleep(time.Duration(7 * time.Second))
	}
}

func (θ cpuTemperature) getOutputChannel() chan string {
	return θ.Output
}

func (d dateAndTime) printToChannel() {
	for {
		d.Output <- time.Now().Local().Format(dateAndTimeFormat)
		// sleep until beginning of next second
		var now = time.Now()
		time.Sleep(now.Truncate(time.Second).Add(time.Second).Sub(now))
	}
}

func (d dateAndTime) getOutputChannel() chan string {
	return d.Output
}

func (ip ipv4Address) printToChannel() {
	time.Sleep(time.Duration(3 * time.Second))
	for {
		out, err := exec.Command("ip", "route", "get", "8.8.8.8").Output()
		if err != nil {
			ip.Output <- "offline"
		} else {
			ip.Output <- ipRegex.FindString(srcIPRegex.FindString(string(out)))
		}
		time.Sleep(time.Duration(3 * time.Second))
	}
}

func (ip ipv4Address) getOutputChannel() chan string {
	return ip.Output
}

func (mem memory) printToChannel() {
	for {
		var file, err = os.Open("/proc/meminfo")
		if err != nil {
			mem.Output <- "err"
		}

		// done must equal the flag combination (0001 | 0010 | 0100 | 1000) = 15
		var used, done = 0, 0
		for info := bufio.NewScanner(file); done != 15 && info.Scan(); {
			var prop, val = "", 0
			if _, err = fmt.Sscanf(info.Text(), "%s %d", &prop, &val); err != nil {
				mem.Output <- "err"
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
		mem.Output <- fmt.Sprintf("%d MB", used/1000)
		time.Sleep(time.Duration(5 * time.Second))
	}
}

func (mem memory) getOutputChannel() chan string {
	return mem.Output
}

func (net networkThroughput) printToChannel() {
	rxOld := 0
	txOld := 0
	rate := 2
	for {
		file, err := os.Open("/proc/net/dev")
		if err != nil {
			net.Output <- "err"
		}

		var void = 0 // target for unused values
		var dev, rx, tx, rxNow, txNow = "", 0, 0, 0, 0
		var scanner = bufio.NewScanner(file)
		for scanner.Scan() {
			_, err = fmt.Sscanf(scanner.Text(), "%s %d %d %d %d %d %d %d %d %d",
				&dev, &rx, &void, &void, &void, &void, &void, &void, &void, &tx)
			dev = strings.TrimSuffix(dev, ":")
			for _, val := range networkDevices {
				if val == dev {
					rxNow += rx
					txNow += tx
				}
			}
		}
		file.Close()
		net.Output <- fmt.Sprintf("%s%s%s", fixed((rxNow-rxOld)/rate), separatorModules, fixed((txNow-txOld)/rate))
		rxOld, txOld = rxNow, txNow
		time.Sleep(time.Duration(2) * time.Second)
	}
}

func (net networkThroughput) getOutputChannel() chan string {
	return net.Output
}

func (p power) printToChannel() {
	const powerSupply = "/sys/class/power_supply/"
	var enFull, enNow, enPerc int = 0, 0, 0
	for {
		var plugged, err = ioutil.ReadFile(powerSupply + "AC/online")
		if err != nil {
			p.Output <- ""
			time.Sleep(time.Duration(10007 * time.Second))
			break
		}
		batts, err := ioutil.ReadDir(powerSupply)
		if err != nil {
			p.Output <- "no battery"
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
			p.Output <- "Battery found but no readable full file"
		}

		enPerc = enNow * 100 / enFull
		var icon = unpluggedSign
		if string(plugged) == "1\n" {
			icon = pluggedSign
		}

		p.Output <- fmt.Sprintf("%d%%%%%s", enPerc, icon)
		time.Sleep(time.Duration(13 * time.Second))
	}
}

func (p power) getOutputChannel() chan string {
	return p.Output
}

func (w wifi) printToChannel() {
	sleepTime := 3 * time.Second
	for {
		iwOutput, err := exec.Command("/usr/sbin/iw", "dev", wifiDevice, "link").Output()
		if err != nil {
			w.Output <- ""
		} else {
			if string(iwOutput) != "Not connected.\n" {
				ssidString := ssidRegex.FindStringSubmatch(string(iwOutput))[0]
				w.Output <- ssidString[6:len(ssidString)-1] + " " + string(iwOutput)[22:30]
			} else {
				w.Output <- "no WIFI"
			}
		}
		time.Sleep(time.Duration(sleepTime))
	}
}

func (w wifi) getOutputChannel() chan string {
	return w.Output
}

func main() {
	mods := make([]barModule, len(modules))
	for i, v := range modules {
		switch v {
		case "memoryUsage":
			mod := memory{Output: make(chan string)}
			go mod.printToChannel()
			mods[i] = &mod
		case "networkThroughput":
			mod := networkThroughput{Output: make(chan string)}
			go mod.printToChannel()
			mods[i] = &mod
		case "cpuTemperature":
			mod := cpuTemperature{Output: make(chan string)}
			go mod.printToChannel()
			mods[i] = &mod
		case "ipAddress":
			mod := ipv4Address{Output: make(chan string)}
			go mod.printToChannel()
			mods[i] = &mod
		case "dateAndTime":
			mod := dateAndTime{Output: make(chan string)}
			go mod.printToChannel()
			mods[i] = &mod
		case "power":
			mod := power{Output: make(chan string)}
			go mod.printToChannel()
			mods[i] = &mod
		case "wifi":
			mod := wifi{Output: make(chan string)}
			go mod.printToChannel()
			mods[i] = &mod
		case "alignRight":
			mod := alignRight{Output: make(chan string)}
			go mod.printToChannel()
			mods[i] = &mod
		}
	}

	// https://stackoverflow.com/questions/19992334/how-to-listen-to-n-channels-dynamic-select-statement#
	cases := make([]reflect.SelectCase, len(modules))
	for i, ch := range mods {
		cases[i] = reflect.SelectCase{Dir: reflect.SelectRecv, Chan: reflect.ValueOf(ch.getOutputChannel())}
	}

	results := make([]string, len(modules))
	for {
		i, v, ok := reflect.Select(cases)
		if !ok {
			cases = append(cases[:i], cases[i+1:]...)
		} else {
			results[i] = v.String()
			fmt.Println(strings.Join(results, separatorModules))
		}
	}
}
