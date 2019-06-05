package power

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
	"time"
)

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
