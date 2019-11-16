package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"net"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"time"
)

const (
	dateAndTimeFormat = "Mon 2 Jan 15:04:05 MST"
	unpluggedSign     = "!"
	pluggedSign       = ""
	separatorModules  = "  "
	realFg            = "#e5e6e6" // because we 'abuse' lemonbar's F for theming color
)

var (
	networkDevices, _ = net.Interfaces()
	wifiDevice        = ""
	thermalZone       = "1"
	ssidRegex         = regexp.MustCompile("SSID: (.*?)\n")
)

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

func formatHerbstluftwmStatus(input string, screen string) string {
	items := strings.Split(strings.TrimSpace(input), "\t")
	var result string
	for _, v := range items {
		switch v[:1] {
		case ".":
			result += "%{F#515151}%{A:herbstclient chain ⛓️ focus_monitor " + string(screen) + " ⛓️ use '" + v[1:] + "':}%{A3:herbstclient move '" + v[1:] + "':} " + v[1:] + " %{A}%{A}"
		case ":":
			// occupied tag = !viewed, !here, !focused
			result += "%{F#969696}%{A:herbstclient chain ⛓️ focus_monitor " + string(screen) + " ⛓️ use '" + v[1:] + "':}%{A3:herbstclient move '" + v[1:] + "':} " + v[1:] + " %{A}%{A}"
		case "+":
			// viewed, here, !focused
			result += "%{F" + realFg + "}%{U" + realFg + "}%{+u}%{A:herbstclient use '" + v[1:] + "':}%{A3:herbstclient move '" + v[1:] + "':} " + v[1:] + " %{A}%{A}%{-u}%{U-}"
		case "-":
			// viewed, !here, !focused
			result += "%{F" + realFg + "}%{A:herbstclient chain ⛓️ focus_monitor " + string(screen) + " ⛓️ use '" + v[1:] + "':}%{A3:herbstclient move '" + v[1:] + "':} " + v[1:] + " %{A}%{A}"
		case "%":
			// viewed, !here, focused
			result += "%{F-}%{A:herbstclient chain ⛓️ focus_monitor " + string(screen) + " ⛓️ use '" + v[1:] + "':}%{A3:herbstclient move '" + v[1:] + "':} " + v[1:] + " %{A}%{A}"
		case "#":
			// viewed, here, focused
			result += "%{F-}%{+u}%{A:herbstclient use_previous:} " + v[1:] + " %{A}%{-u}"
		case "!":
			// urgent
			result += "%{F#e32791}%{A:herbstclient chain ⛓️ focus_monitor " + string(screen) + " ⛓️ use '" + v[1:] + "':}%{A3:herbstclient move '" + v[1:] + "':} " + v[1:] + " %{A}%{A}"
		}
	}
	return result
}

func getCurFrameWCount() string {
	out, err := exec.Command("herbstclient", "get_attr", "tags.focus.curframe_wcount").Output()
	out2, err2 := exec.Command("herbstclient", "get_attr", "tags.focus.curframe_windex").Output()
	if err != nil || err2 != nil {
		return "%{F#e5e6e6}%{B#005577}%{O1500}%{A}%{r}[?] %{F" + realFg + "}%{B-}"
	}
	clientIndex, err := strconv.Atoi(string(out2)[:len(out2)-1])
	clientNumber := string(out)[:len(out)-1]
	if clientNumber == "0" || clientNumber == "1" {
		return "%{O1500}%{A}%{r}%{F" + realFg + "}%{B-}"
	}
	if err == nil {
		return "%{F#e5e6e6}%{B#005577}%{O1500}%{A}%{r}[" + strconv.Itoa(clientIndex+1) + "/" + clientNumber + "] %{F" + realFg + "}%{B-}"
	}
	return "%{F#e5e6e6}%{B#005577}%{O1500}%{A}%{r}[?] %{F" + realFg + "}%{B-}"
}

func updateHerbstluftStatus(hlwmStatus chan<- string, screen string) {
	cmd := exec.Command("herbstclient", "--idle")
	out, err := cmd.StdoutPipe()
	var workspaces string
	var windowTitle string
	var curFrameWCount string = "0"

	err = cmd.Start()
	if err != nil {
		workspaces = fmt.Sprintf("Failed to start err=%v", err)
	}
	scanner := bufio.NewScanner(out)
	for ok := true; ok; ok = scanner.Scan() {
		action := strings.Split(scanner.Text(), "\t")
		switch action[0] {
		case "focus_changed":
			curFrameWCount = getCurFrameWCount()
			fallthrough
		case "window_title_changed":
			if len(action) >= 2 {
				windowTitle = "%{F#e5e6e6}%{B#005577}  " + action[2]
			} else {
				windowTitle = " "
			}
		default:
			curFrameWCount = getCurFrameWCount()
			out, err := exec.Command("herbstclient", "tag_status", screen).Output()
			if err != nil {
				workspaces = "ERROR: Failed to display tags."
			} else {
				workspaces = formatHerbstluftwmStatus(string(out), screen)
			}
		}
		hlwmStatus <- workspaces + "%{A:herbstclient cycle:}" + windowTitle + curFrameWCount
	}
	if err := scanner.Err(); err != nil {
		workspaces = fmt.Sprintf("reading standard input: %v", err)
	}
}

func updateTemperature(θ chan<- string) {
	for {
		var temp, err = ioutil.ReadFile("/sys/class/thermal/thermal_zone" + thermalZone + "/temp")
		if err != nil {
			θ <- "temp unknown"
		} else {
			θ <- fmt.Sprintf("%s °C", string(temp)[:len(temp)-4])
		}
		time.Sleep(time.Duration(7 * time.Second))
	}
}

func updateTime(d chan<- string) {
	for {
		d <- time.Now().Local().Format(dateAndTimeFormat)
		// sleep until beginning of next second
		var now = time.Now()
		time.Sleep(now.Truncate(time.Second).Add(time.Second).Sub(now))
	}
}

func updateIPAdress(ipv4 chan<- string) {
	for {
		conn, err := net.Dial("udp", "8.8.8.8:80")
		if err != nil {
			ipv4 <- "offline"
		} else {
			localAddr := strings.Split(conn.LocalAddr().(*net.UDPAddr).String(), ":")
			if len(localAddr) > 0 {
				ipv4 <- localAddr[0]
			} else {
				ipv4 <- "%{F#ff00ff} ERROR %{F-}"
				// ipv4 <- conn.LocalAddr().(*net.UDPAddr).String()
			}
		}
		time.Sleep(time.Duration(3 * time.Second))
	}
}

func updateMemUse(mem chan<- string) {
	for {
		var file, err = os.Open("/proc/meminfo")
		if err != nil {
			mem <- "err"
		}

		// done must equal the flag combination (0001 | 0010 | 0100 | 1000) = 15
		var used, done = 0, 0
		for info := bufio.NewScanner(file); done != 15 && info.Scan(); {
			var prop, val = "", 0
			if _, err = fmt.Sscanf(info.Text(), "%s %d", &prop, &val); err != nil {
				mem <- "err"
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
		mem <- fmt.Sprintf("%d MB", used/1000)
		time.Sleep(time.Duration(5 * time.Second))
	}
}

func updateNetUse(net chan<- string) {
	rxOld := 0
	txOld := 0
	rate := 2
	for {
		file, err := os.Open("/proc/net/dev")
		if err != nil {
			net <- "err"
		}

		var void = 0 // target for unused values
		var dev, rx, tx, rxNow, txNow = "", 0, 0, 0, 0
		var scanner = bufio.NewScanner(file)
		for scanner.Scan() {
			_, err = fmt.Sscanf(scanner.Text(), "%s %d %d %d %d %d %d %d %d %d",
				&dev, &rx, &void, &void, &void, &void, &void, &void, &void, &tx)
			dev = strings.TrimSuffix(dev, ":")
			for _, val := range networkDevices {
				if val.Name == dev {
					rxNow += rx
					txNow += tx
				}
			}
		}
		file.Close()
		net <- fmt.Sprintf("%s%s%s", fixed((rxNow-rxOld)/rate), separatorModules, fixed((txNow-txOld)/rate))
		rxOld, txOld = rxNow, txNow
		time.Sleep(time.Duration(2) * time.Second)
	}
}

func updatePower(pow chan<- string) {
	const powerSupply = "/sys/class/power_supply/"
	var enFull, enNow, enPerc int = 0, 0, 0
	for {
		var plugged, err = ioutil.ReadFile(powerSupply + "AC/online")
		if err != nil {
			pow <- ""
			time.Sleep(time.Duration(10007 * time.Second))
			break
		}
		batts, err := ioutil.ReadDir(powerSupply)
		if err != nil {
			pow <- "no battery"
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
			pow <- "Battery found but no readable full file"
		}

		enPerc = enNow * 100 / enFull
		var icon = unpluggedSign
		if string(plugged) == "1\n" {
			icon = pluggedSign
		}

		pow <- fmt.Sprintf("%d%%%s", enPerc, icon)
		time.Sleep(time.Duration(13 * time.Second))
	}
}

func updateWIFI(wifi chan<- string) {
	sleepTime := 4500 * time.Millisecond
	for {
		if wifiDevice != "" {
			iwOutput, _ := exec.Command("/usr/sbin/iw", "dev", wifiDevice, "link").Output()
			if string(iwOutput) != "Not connected.\n" {
				ssidStrings := ssidRegex.FindStringSubmatch(string(iwOutput))
				if len(ssidStrings) > 0 {
					ssidString := ssidStrings[0]
					wifi <- ssidString[6:len(ssidString)-1] + " " + string(iwOutput)[22:30]
				}
			} else {
				wifi <- "%{F#e32791}no WiFi%{F-}"
			}
		} else {
			wifi <- "%{F#969696}no WiFi%{F-}"
		}
		time.Sleep(time.Duration(sleepTime))
	}
}

func main() {
	screen := os.Args[1:]
	if os.Getenv("HOSTNAME") == "airolo" {
		thermalZone = "2"
	}
	for _, v := range networkDevices {
		if v.Name[0] == 'w' {
			wifiDevice = v.Name
			break
		}
	}
	memChan := make(chan string)
	netChan := make(chan string)
	tempChan := make(chan string)
	wifiChan := make(chan string)
	ipChan := make(chan string)
	powChan := make(chan string)
	timeChan := make(chan string)
	hlwmChan := make(chan string)
	go updateMemUse(memChan)
	go updateNetUse(netChan)
	go updateTemperature(tempChan)
	go updateWIFI(wifiChan)
	go updateIPAdress(ipChan)
	go updatePower(powChan)
	go updateTime(timeChan)
	go updateHerbstluftStatus(hlwmChan, screen[0])
	status := make([]string, 8)
	for {
		select {
		case status[0] = <-hlwmChan:
			fmt.Println(strings.Join(status[:], separatorModules))
		case status[7] = <-timeChan:
			fmt.Println(strings.Join(status[:], separatorModules))
		case status[2] = <-memChan:
		case status[3] = <-netChan:
		case status[4] = <-tempChan:
		case status[5] = <-wifiChan:
		case status[6] = <-ipChan:
		case status[1] = <-powChan:
		}
	}
}
