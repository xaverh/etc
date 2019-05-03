package memory

import (
	"bufio"
	"fmt"
	"os"
	"time"

	"../../config"
)

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
