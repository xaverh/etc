package wifi

import (
	"os/exec"
	"regexp"
	"time"

	"../../config"
)

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
