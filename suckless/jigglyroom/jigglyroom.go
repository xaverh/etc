package main

/*
#cgo LDFLAGS: -lX11
#include <X11/Xlib.h>
*/
import "C"

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"time"
)

const (
	// thermalZone is '*' in filename /sys/class/thermal/thermal_zone/*/temp
	wifiDevice        = "wlan0"
	dateAndTimeFormat = "Mon 2 Jan 15:04:05 MST"
	unpluggedSign     = "!"
	pluggedSign       = ""
	separatorModules  = "  "
)

var (
	thermalZone    = "2"
	networkDevices = []string{
		"eno1",
		"wlan0",
	}

	dpy        = C.XOpenDisplay(nil)
	ipRegex    = regexp.MustCompilePOSIX("[0-9.]+")
	srcIPRegex = regexp.MustCompilePOSIX("src [0-9.]+")
	ssidRegex  = regexp.MustCompile("SSID: (.*?)\n")
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

func updateTemperature(θ chan<- string) {
	for {
		var temp, err = ioutil.ReadFile("/sys/class/thermal/thermal_zone" + thermalZone + "/temp")
		if err != nil {
			θ <- "temp unknown"
		}
		θ <- fmt.Sprintf("%s °C", string(temp)[:len(temp)-4])
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
	time.Sleep(time.Duration(3 * time.Second))
	for {
		out, err := exec.Command("ip", "route", "get", "8.8.8.8").Output()
		if err != nil {
			ipv4 <- "offline"
		} else {
			ipv4 <- ipRegex.FindString(srcIPRegex.FindString(string(out)))
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
				if val == dev {
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
	sleepTime := 3 * time.Second
	for {
		iwOutput, err := exec.Command("/usr/sbin/iw", "dev", wifiDevice, "link").Output()
		if err != nil {
			wifi <- ""
		} else {
			if string(iwOutput) != "Not connected.\n" {
				ssidString := ssidRegex.FindStringSubmatch(string(iwOutput))[0]
				wifi <- ssidString[6:len(ssidString)-1] + " " + string(iwOutput)[22:30]
			} else {
				wifi <- "no WIFI"
			}
		}
		time.Sleep(time.Duration(sleepTime))
	}
}

func setStatus(s *C.char) {
	C.XStoreName(dpy, C.XDefaultRootWindow(dpy), s)
	C.XSync(dpy, 1)
}

func main() {
	if os.Getenv("HOSTNAME") == "andermatt" {
		thermalZone = "1"
	}
	memChan := make(chan string)
	netChan := make(chan string)
	tempChan := make(chan string)
	wifiChan := make(chan string)
	ipChan := make(chan string)
	powChan := make(chan string)
	timeChan := make(chan string)
	go updateMemUse(memChan)
	go updateNetUse(netChan)
	go updateTemperature(tempChan)
	go updateWIFI(wifiChan)
	go updateIPAdress(ipChan)
	go updatePower(powChan)
	go updateTime(timeChan)
	status := make([]string, 7)
	for {
		select {
		case status[6] = <-timeChan:
			setStatus(C.CString(strings.Join(status[:], separatorModules)))
		case status[1] = <-memChan:
		case status[2] = <-netChan:
		case status[3] = <-tempChan:
		case status[4] = <-wifiChan:
		case status[5] = <-ipChan:
		case status[0] = <-powChan:
		}
	}
}
