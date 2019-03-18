package main

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
	bpsSign  = "B/s"
	kbpsSign = "kB/s"
	mbpsSign = "MB/s"
	gbpsSign = "GB/s"

	unpluggedSign = "!"
	pluggedSign   = ""

	batteryScale = 100

	memIcon = ""

	netReceivedSign    = ""
	netTransmittedSign = ""

	fieldSeparator = "   "

	ipPattern    = "[0-9.]+"
	srcIPPattern = "src [0-9.]+"

	colorK = "#1E1E1E" /* Grey 10%, R=30, G=30, B=30 */
	colorR = "#E32791" /* Deep Cerise, R=227, G=39, B=145 */
	colorG = "#30C798" /* Shamrock, R=48, G=199, B=152 */
	colorY = "#E3C472" /* Chenin, R=227, G=196, B=114 */
	colorB = "#6796E6" /* Cornflower Blue, R=103, G=150, B=230 */
	colorM = "#E59FDF" /* Plum, R=229, G=159, B=223 */
	colorC = "#81D8D0" /* Riptide, R=129, G=216, B=208 */
	colorW = "#969696" /* Grey 60%, R=150, G=150, B=150 */

	colorLightK = "#515151" /* Grey 30%, R=81, G=81, B=81 */
	colorLightR = "#E466AD" /* Hot Pink, R=228, G=102, B=173 */
	colorLightG = "#6CD1B2" /* Medium Aquamarine, R=108, G=209, B=178 */
	colorLightY = "#E4CF98" /* Double Colonial White, R=228, G=207, B=152 */
	colorLightB = "#91B0E6" /* Jordy Blue, R=145, G=181, B=230 */
	colorLightM = "#E5B6E1" /* French Lilac, R=229, G=182, B=225 */
	colorLightC = "#A2DCD7" /* Sinbad, R=162, G=220, B=215 */
	colorLightW = "#E5E6E6" /* Grey 90%, R=229, G=230, B=230 */

	colorDarkB = "#005577"

	colorDefaultFg           = colorLightW
	colorDefaultBg           = colorK
	colorFreeFg              = colorW
	colorFreeBg              = colorDefaultBg
	colorFocusedHereFg       = colorLightW
	colorFocusedHereBg       = colorDarkB
	colorNotFocusedHereFg    = colorLightW
	colorNotFocusedHereBg    = colorC
	colorFocusedNotHereFg    = colorDarkB
	colorFocusedNotHereBg    = colorDefaultBg
	colorNotFocusedNotHereFg = colorC
	colorNotFocusedNotHereBg = colorDefaultBg
	colorUrgentFg            = colorDefaultFg
	colorUrgentBg            = colorR
)

var (
	netDevs = map[string]struct{}{
		"eno1:":      {},
		// "wlp2s0:":    {},
		"enp0s20u1:": {},
		"ppp0:":      {},
	}
	ipRegex    = regexp.MustCompilePOSIX(ipPattern)
	srcIPRegex = regexp.MustCompilePOSIX(srcIPPattern)
	ssidRegex  = regexp.MustCompile("SSID: (.*?)\n")
	screen     = os.Args[1]
)

func formatHerbstluftwmStatus(input string) string {
	items := strings.Split(strings.TrimSpace(input), "\t")
	result := ""
	fg := colorDefaultFg
	bg := colorDefaultBg
	for _, v := range items {
		switch v[:1] {
		case ".":
			// empty tag
			bg = colorFreeBg
			fg = colorFreeFg
		case ":":
			// occupied tag
			bg = colorDefaultBg
			fg = colorDefaultFg
		case "+":
			// viewed, here, !focused
			bg = colorNotFocusedHereBg
			fg = colorNotFocusedHereFg
		case "-":
			// viewed, !here, !focused
			bg = colorNotFocusedNotHereBg
			fg = colorNotFocusedNotHereFg
		case "%":
			// viewed, !here, focused
			bg = colorFocusedNotHereBg
			fg = colorFocusedNotHereFg
		case "#":
			// viewed, here, focused
			bg = colorFocusedHereBg
			fg = colorFocusedHereFg
		case "!":
			// urgent
			bg = colorUrgentBg
			fg = colorUrgentFg
		}
		result = result + "^bg(" + bg + ")^fg(" + fg + ")^ca(1,herbstclient focus_monitor " + screen + " && herbstclient use " + v[1:] + ")" + fieldSeparator + v[1:] + fieldSeparator + "^ca()"
	}
	return result + "^bg(" + colorDefaultBg + ")^fg(" + colorDefaultFg + ")"
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
		default: // kB/s
			result /= 1000.0
			suf = kbpsSign
		}

		return fmt.Sprintf("%s%.1f %s", pre, result, suf)

	}
	return fmt.Sprintf("%s%d %s", pre, rate, suf)
}

func updateNetUse(netChan chan<- string) {
	rxOld := 0
	txOld := 0
	for {
		file, err := os.Open("/proc/net/dev")
		if err != nil {
			netChan <- netReceivedSign + " err " + netTransmittedSign + " err"
		}

		var void = 0 // target for unused values
		var dev, rx, tx, rxNow, txNow = "", 0, 0, 0, 0
		var scanner = bufio.NewScanner(file)
		for scanner.Scan() {
			_, err = fmt.Sscanf(scanner.Text(), "%s %d %d %d %d %d %d %d %d %d",
				&dev, &rx, &void, &void, &void, &void, &void, &void, &void, &tx)
			if _, ok := netDevs[dev]; ok {
				rxNow += rx
				txNow += tx
			}
		}
		file.Close()
		netChan <- fmt.Sprintf("%s %s", fixed(netReceivedSign, rxNow-rxOld), fixed(netTransmittedSign, txNow-txOld))
		rxOld, txOld = rxNow, txNow
		time.Sleep(time.Duration(time.Second))
	}
}

func updatePower(powChan chan<- string) {
	const powerSupply = "/sys/class/power_supply/"
	var enFull, enNow, enPerc int = 0, 0, 0
	for {
		var plugged, err = ioutil.ReadFile(powerSupply + "AC/online")
		if err != nil {
			powChan <- ""
			time.Sleep(time.Duration(1000 * time.Second))
			break
		}
		batts, err := ioutil.ReadDir(powerSupply)
		if err != nil {
			powChan <- "no battery"
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
			powChan <- "Battery found but no readable full file"
		}

		enPerc = enNow * 10000 / enFull / batteryScale
		var icon = unpluggedSign
		if string(plugged) == "1\n" {
			icon = pluggedSign
		}

		powChan <- fmt.Sprintf("%d%%%s", enPerc, icon)
		time.Sleep(time.Duration(10 * time.Second))
	}
}

func updateIPAdress(ipChan chan<- string) {
	for {
		out, err := exec.Command("ip", "route", "get", "8.8.8.8").Output()
		if err != nil {
			ipChan <- "offline"
		} else {
			ipChan <- ipRegex.FindString(srcIPRegex.FindString(string(out)))
		}
		time.Sleep(time.Duration(3 * time.Second))
	}
}

func updateWIFI(wifiChan chan<- string) {
	sleepTime := 3 * time.Second
	for {
		iwOutput, err := exec.Command("iw", "dev", "wlp2s0", "link").Output()
		if err != nil {
			wifiChan <- ""
		} else {
			if string(iwOutput) != "Not connected.\n" {
				ssidString := ssidRegex.FindStringSubmatch(string(iwOutput))[0]
				wifiChan <- ssidString[6:len(ssidString)-1] + " " + string(iwOutput)[22:30]
			} else {
				wifiChan <- ""
			}
		}
		time.Sleep(time.Duration(sleepTime))
	}
}

func updateTemperature(tempChan chan<- string) {
	for {
		// Airolo:
		// var temp, err = ioutil.ReadFile("/sys/class/thermal/thermal_zone2/temp")
		// Andermatt:
		var temp, err = ioutil.ReadFile("/sys/class/thermal/thermal_zone2/temp")
		if err != nil {
			tempChan <- "temp unknown"
		}
		tempChan <- fmt.Sprintf("%s °C", string(temp)[:len(temp)-4])
		time.Sleep(time.Duration(2 * time.Second))
	}
}

func updateMemUse(memChan chan<- string) {
	for {
		var file, err = os.Open("/proc/meminfo")
		if err != nil {
			memChan <- memIcon + "err"
		}

		// done must equal the flag combination (0001 | 0010 | 0100 | 1000) = 15
		var used, done = 0, 0
		for info := bufio.NewScanner(file); done != 15 && info.Scan(); {
			var prop, val = "", 0
			if _, err = fmt.Sscanf(info.Text(), "%s %d", &prop, &val); err != nil {
				memChan <- memIcon + "err"
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
		memChan <- fmt.Sprintf("%s%d MB", memIcon, used/1000)
		time.Sleep(time.Duration(2 * time.Second))
	}
}

func updateHerbstluftwmState(herbstluftwmChan chan<- string) {
	cmd := exec.Command("herbstclient", "--idle")
	out, err := cmd.StdoutPipe()

	err = cmd.Start()
	if err != nil {
		herbstluftwmChan <- fmt.Sprintf("Failed to start err=%v", err)
	}
	scanner := bufio.NewScanner(out)
	for ok := true; ok; ok = scanner.Scan() {
		action := strings.Split(scanner.Text(), "\t")
		switch action[0] {
		default:
			out, err := exec.Command("herbstclient", "tag_status", screen).Output()
			if err != nil {
				herbstluftwmChan <- "ERROR: Failed to display tags."
			} else {
				herbstluftwmChan <- formatHerbstluftwmStatus(string(out))
			}
		}
	}
	if err := scanner.Err(); err != nil {
		herbstluftwmChan <- fmt.Sprintf("reading standard input: %v", err)
	}
}

func updateTime(timeChan chan<- string) {
	for {
		timeChan <- time.Now().Local().Format("Mon 02 Jan 15:04:05 MST")
		// sleep until beginning of next second
		var now = time.Now()
		time.Sleep(now.Truncate(time.Second).Add(time.Second).Sub(now))
	}
}

func feedDzen2(status [6]string) {
	rightTextOnly := strings.TrimSpace(strings.Join(status[1:], fieldSeparator))
	rightTextOnlyQuoted := strconv.Quote(rightTextOnly)
	out, err := exec.Command("textwidth", os.Args[2], rightTextOnlyQuoted).CombinedOutput()
	if err != nil {
		fmt.Printf("failed to exec textwidth: %v", err)
	}
	textWidth, _ := strconv.Atoi(strings.TrimSuffix(string(out), "\n"))
	panelWidth, _ := strconv.Atoi(os.Args[3])
	// the magic number 5 below moves output slightly to right
	output := status[0] + "^pa(" + strconv.Itoa(panelWidth-textWidth+5) + ")" + rightTextOnly
	fmt.Println(output)
}

func main() {
	memChan := make(chan string)
	netChan := make(chan string)
	tempChan := make(chan string)
	// wifiChan := make(chan string)
	ipChan := make(chan string)
	// powChan := make(chan string)
	herbstluftwmChan := make(chan string)
	timeChan := make(chan string)
	go updateMemUse(memChan)
	go updateNetUse(netChan)
	go updateTemperature(tempChan)
	// go updateWIFI(wifiChan)
	go updateIPAdress(ipChan)
	// go updatePower(powChan)
	go updateHerbstluftwmState(herbstluftwmChan)
	go updateTime(timeChan)
	var status [6]string
	for {
		select {
		case status[0] = <-herbstluftwmChan:
			feedDzen2(status)
		case status[5] = <-timeChan:
			feedDzen2(status)
		case status[1] = <-memChan:
		case status[2] = <-netChan:
		case status[3] = <-tempChan:
		// case status[4] = <-wifiChan:
		case status[4] = <-ipChan:
		// case status[6] = <-powChan:
		}
	}
}
