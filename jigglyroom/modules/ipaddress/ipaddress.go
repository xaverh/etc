package ipaddress

import (
	"os/exec"
	"regexp"
	"time"
)

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
