package main

import (
	"bufio"
	"encoding/json"
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

type button int

const (
	buttonLeft button = iota + 1
	buttonMiddle
	buttonRight
	scrollUp
	scrollDown
	scrollLeft
	scrollRight
	buttonBack
	buttonForward
)

type alignment string

const (
	alignLeft   alignment = "left"
	alignRight  alignment = "right"
	alignCenter alignment = "center"
)

type markup string

const (
	none  markup = "none"
	pango markup = "pango"
)

type header struct {
	version     int  `json:"version"`
	stopSignal  int  `json:"stop_signal,omitempty"`
	contSignal  int  `json:"cont_signal,omitempty"`
	clickEvents bool `json:"click_events"`
}

type clickEvent struct {
	button  button `json:"button"`
	x       int    `json:"relative_x,omitempty"`
	y       int    `json:"relative_y,omitempty"`
	width   int    `json:"width,omitempty"`
	height  int    `json:"height,omitempty"`
	screenX int    `json:"x,omitempty"`
	screenY int    `json:"y,omitempty"`
}

/* segment is a single "block" of output that conforms to the i3bar protocol.
See https://i3wm.org/docs/i3bar-protocol.html#_blocks_in_detail for details.
Example:
{
 "full_text": "E: 10.0.0.1 (1000 Mbit/s)",
 "short_text": "10.0.0.1",
 "color": "#00ff00",
 "background": "#1c1c1c",
 "border": "#ee0000",
 "border_top": 1,
 "border_right": 0,
 "border_bottom": 3,
 "border_left": 1,
 "min_width": 300,
 "align": "right",
 "urgent": false,
 "name": "ethernet",
 "instance": "eth0",
 "separator": true,
 "separator_block_width": 9,
 "markup": "none"
} */
type segment struct {
	fullText            string `json:"full_text"`
	shortText           string `json:"short_text,omitempty"`
	color               string `json:"color,omitempty"`
	background          string `json:"background,omitempty"`
	border              int    `json:"border,omitempty"`
	borderTop           int    `json:"border_top,omitempty"`
	borderRight         int    `json:"border_right,omitempty"`
	borderBottom        int    `json:"border_bottom,omitempty"`
	borderLeft          int    `json:"border_left,omitempty"`
	minWidth            int    `json:"min_width,omitempty"`
	urgent              bool   `json:urgent,omitempty"`
	name                string `"json:name,omitempty"`
	instance            string `"json:instance,omitempty"`
	separator           bool   `"json:separator,omitempty"`
	separatorBlockWidth int    `"json:separator_block_width,omitempty"`
	markup              string `"json:markup,omitempty"`
}

const (
	dateAndTimeFormat = "Mon 2 Jan 15:04:05 MST"
	separatorModules  = "  "
	thermalZone       = "1"
)

var (
	networkDevices, _ = net.Interfaces()
	wifiDevice        = ""
	homeDirectory     = os.Getenv("HOME")
	ssidRegex         = regexp.MustCompile("SSID: (.*?)\n")
	btrfsRegex        = regexp.MustCompile(`Free \(estimated\):\s+([\d\.]+)([kMGTPE]?B)`)
)

func siPrefix(n int) string {
	if n < 0 {
		return "err"
	}

	prefix := ""
	if n > 1000 {
		var d = float64(n)

		switch {
		case n >= (1000000000000):
			d /= 1000000000000.0
			prefix = "T"
		case n >= (1000000000):
			d /= 1000000000.0
			prefix = "G"
		case n >= (1000000):
			d /= 1000000.0
			prefix = "M"
		default:
			d /= 1000.0
			prefix = "k"
		}

		return fmt.Sprintf("%.1f\u2009%s", d, prefix)

	}
	return fmt.Sprintf("%d\u2009%s", n, prefix)
}

func updateTemperature(c chan<- string, thermalZone string) {
	for {
		θ, err := ioutil.ReadFile("/sys/class/thermal/thermal_zone" + thermalZone + "/temp")
		if err != nil {
			c <- "temp unknown"
		} else {
			c <- fmt.Sprintf("θ\u2009%s\u2009°C%s", string(θ)[:len(θ)-4], separatorModules)
		}
		time.Sleep(time.Duration(6967 * time.Millisecond))
	}
}

func updateTime(c chan<- string) {
	for {
		c <- time.Now().Local().Format(dateAndTimeFormat)
		// sleep until beginning of next second
		t := time.Now()
		time.Sleep(t.Truncate(time.Second).Add(time.Second).Sub(t))
	}
}

func updateIpv4Address(c chan<- string) {
	for {
		conn, err := net.Dial("udp", "8.8.8.8:80")
		if err != nil {
			c <- "offline"
		} else {
			localAddr := strings.Split(conn.LocalAddr().(*net.UDPAddr).String(), ":")
			if len(localAddr) > 0 {
				c <- localAddr[0] + separatorModules
			} else {
				c <- "ERROR"
				// ipv4 <- conn.LocalAddr().(*net.UDPAddr).String()
			}
		}
		time.Sleep(time.Duration(3037 * time.Millisecond))
	}
}

func updateBtrfsUse(c chan<- string) {
	for {
		out, err := exec.Command("/run/current-system/sw/bin/btrfs", "filesystem", "usage", "--si", "/").Output()
		if err == nil {
			matches := btrfsRegex.FindStringSubmatch(string(out))
			c <- fmt.Sprintf("/ %s\u2009%s%s", matches[1], matches[2], separatorModules)
		} else {
			c <- ""
			time.Sleep(time.Duration(24 * time.Hour))
		}
		time.Sleep(time.Duration(6113 * time.Millisecond))
	}
}

func updateMemUse(c chan<- string) {
	for {
		file, err := os.Open("/proc/meminfo")
		if err != nil {
			c <- "err"
		}

		// done must equal the flag combination (0001 | 0010 | 0100 | 1000) = 15
		used, done := 0, 0
		for info := bufio.NewScanner(file); done != 15 && info.Scan(); {
			prop, val := "", 0
			if _, err = fmt.Sscanf(info.Text(), "%s %d", &prop, &val); err != nil {
				c <- "err"
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
		c <- fmt.Sprintf("m %sB%s", siPrefix(used*1000), separatorModules)
		time.Sleep(time.Duration(5077 * time.Millisecond))
	}
}

func updateNetUse(c chan<- string) {
	rxOld := 0
	txOld := 0
	rate := 2
	for {
		file, err := os.Open("/proc/net/dev")
		if err != nil {
			c <- "err"
		}

		var void int // target for unused values
		dev, rx, tx, rxNow, txNow := "", 0, 0, 0, 0
		scanner := bufio.NewScanner(file)
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
		c <- fmt.Sprintf("↓%sB/s%s↑%sB/s%s", siPrefix((rxNow-rxOld)/rate), separatorModules, siPrefix((txNow-txOld)/rate), separatorModules)
		rxOld, txOld = rxNow, txNow
		time.Sleep(time.Duration(2) * time.Second)
	}
}

func updatePower(c chan<- string) {
	const powerSupply = "/sys/class/power_supply/"
	enFull, enNow, enPerc := 0, 0, 0
	for {
		plugged, err := ioutil.ReadFile(powerSupply + "AC/online")
		if err != nil {
			c <- "error"
			time.Sleep(time.Duration(10007 * time.Second))
			break
		}
		batts, err := ioutil.ReadDir(powerSupply)
		if err != nil {
			c <- ""
			time.Sleep(time.Duration(86371 * time.Second))
			break
		}

		readval := func(name, field string) int {
			path := powerSupply + name + "/"
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
			c <- "Battery found but no readable full file"
		}

		enPerc = enNow * 100 / enFull
		icon := "🔋"
		if string(plugged) == "1\n" {
			icon = "🔌"
		}

		c <- fmt.Sprintf("%d%%%s%s", enPerc, icon, separatorModules)
		time.Sleep(time.Duration(2027 * time.Millisecond))
	}
}

func updateWIFI(c chan<- string) {
	for {
		if wifiDevice != "" {
			iwOutput, _ := exec.Command("/run/current-system/sw/bin/iw", "dev", wifiDevice, "link").Output()
			if string(iwOutput) != "Not connected.\n" {
				ssidStrings := ssidRegex.FindStringSubmatch(string(iwOutput))
				if len(ssidStrings) > 0 {
					ssidString := ssidStrings[0]
					c <- ssidString[6:len(ssidString)-1] + "\u2009" + string(iwOutput)[22:30] + separatorModules
				}
			} else {
				c <- "no WiFi" + separatorModules
			}
		} else {
			c <- "no WiFi" + separatorModules
			for _, v := range networkDevices {
				if v.Name[0] == 'w' {
					wifiDevice = v.Name
				}
			}
		}
		time.Sleep(time.Duration(3559 * time.Millisecond))
	}
}

func main() {
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
	btrfsChan := make(chan string)
	go updateMemUse(memChan)
	go updateNetUse(netChan)
	go updateTemperature(tempChan, thermalZone)
	go updateWIFI(wifiChan)
	go updateIpv4Address(ipChan)
	go updatePower(powChan)
	go updateTime(timeChan)
	go updateBtrfsUse(btrfsChan)
	status := make([]string, 8)
	for {
		select {
		case status[7] = <-timeChan:
			fmt.Println(" " + strings.Join(status[:], ""))
		case status[0] = <-btrfsChan:
		case status[1] = <-memChan:
		case status[2] = <-netChan:
		case status[3] = <-tempChan:
		case status[4] = <-wifiChan:
		case status[5] = <-ipChan:
		case status[6] = <-powChan:
		}
	}
}
